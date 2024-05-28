package api

import (
	"bytes"
	"container/heap"
	"encoding/json"
	"fmt"
	"io"
	"log/slog"
	"net/http"
	"net/url"
	"time"
)

type APIError struct {
	Code    int             `json:"code"`
	Data    json.RawMessage `json:"data"`
	Message string          `json:"message"`
}

func (e *APIError) Error() string {
	return fmt.Sprintf("unhandled APIError code %d, message \"%s\", data: %s", e.Code, e.Message, string(e.Data))
}

type APIMessage struct {
	Data  json.RawMessage `json:"data"`
	Error *APIError       `json:"error"`
	//meta
}

type Request struct {
	index    int
	priority int

	method          string
	uri             *url.URL
	payload         any
	responseChannel chan *Response
}

type Response struct {
	Message *APIMessage
	Err     error
}

func (c *Client) Send(method string, uriRef *url.URL, payload any) (*APIMessage, error) {
	responseChannel := make(chan *Response)
	c.requestsChannel <- &Request{
		method:          method,
		payload:         payload,
		priority:        10,
		responseChannel: responseChannel,
		uri:             c.baseURI.ResolveReference(uriRef),
	}
	res := <-responseChannel
	return res.Message, res.Err
}

func queueProcessor(client *Client) {
	var (
		req *Request
		ok  bool
	)
	for {
		// The queue is empty so we do this blocking call
		select {
		case <-client.ctx.Done():
			return
		case req, ok = <-client.requestsChannel:
			if !ok {
				return
			}
			heap.Push(client.pq, req)
		}
		// we enqueue all values read from the channel and process the queue's
		// contents until empty. We keep reading the channel as long as this
		// emptying goes on
		for {
			select {
			case <-client.ctx.Done():
				return
			case req, ok = <-client.requestsChannel:
				if !ok {
					return
				}
				heap.Push(client.pq, req)
			default:
				if client.pq.Len() == 0 {
					break
				}
				// we process one
				if req, ok = heap.Pop(client.pq).(*Request); !ok {
					panic("queueProcessor got something other than a Request on its channel")
				}
				msg, err := client.sendOne(req.method, req.uri, req.payload)
				req.responseChannel <- &Response{
					Message: msg,
					Err:     err,
				}
			}
		}
	}
}

func (c *Client) sendOne(method string, uri *url.URL, payload any) (*APIMessage, error) {
	slog.Debug("request", "method", method, "path", uri.Path, "payload", payload)
	var payloadReader io.Reader
	if payload != nil {
		if body, err := json.Marshal(payload); err != nil {
			return nil, fmt.Errorf("failed to marshal payload: %w", err)
		} else {
			payloadReader = bytes.NewReader(body)
		}
	}

	req, err := http.NewRequestWithContext(c.ctx, method, uri.String(), payloadReader)
	if err != nil {
		return nil, fmt.Errorf("failed to create request: %w", err)
	}
	req.Header = *c.headers

	resp, err := c.httpClient.Do(req)
	if err != nil {
		return nil, fmt.Errorf("failed to do request: %w", err)
	}
	defer resp.Body.Close()
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		return nil, fmt.Errorf("failed to read response body: %w", err)
	}

	var msg APIMessage
	if err = json.Unmarshal(body, &msg); err != nil {
		return nil, fmt.Errorf("failed to unmarshal response body: %w", err)
	}
	slog.Debug("response", "code", resp.StatusCode, "message", msg)
	switch resp.StatusCode {
	case 429:
		e := decodeRateLimitError(msg.Error.Data)
		time.Sleep(e.RetryAfter.Duration() * time.Second)
		return c.sendOne(method, uri, payload)
	}
	return &msg, nil
}
