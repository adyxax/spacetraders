package api

import (
	"bytes"
	"encoding/json"
	"io"
	"log/slog"
	"net/http"
	"time"
)

type Error[T any] struct {
	Code    int    `json:"code"`
	Data    T      `json:"data"`
	Message string `json:"message"`
}

type APIMessage[T any, E any] struct {
	Data  T        `json:"data"`
	Error Error[E] `json:"error"`
	//meta
}

type Response struct {
	Response []byte
	Err      error
}

func Send[T any](c *Client, method, path string, payload any) (message APIMessage[T, any], err error) {
	resp := make(chan *Response)
	c.channel <- &Request{
		method:   method,
		path:     path,
		payload:  payload,
		priority: 10,
		resp:     resp,
	}
	res := <-resp
	if res.Err != nil {
		return message, res.Err
	}
	err = json.Unmarshal(res.Response, &message)
	return message, err
}

func (c *Client) sendOne(method, path string, payload any) (body []byte, err error) {
	slog.Debug("Request", "method", method, "path", path, "payload", payload)
	var req *http.Request
	if payload != nil {
		body, err = json.Marshal(payload)
		if err == nil {
			req, err = http.NewRequest(method, c.baseURL+path, bytes.NewBuffer(body))
		} else {
			return nil, err
		}
	} else {
		req, err = http.NewRequest(method, c.baseURL+path, nil)
	}
	if err != nil {
		return nil, err
	}
	req.Header = *c.headers
	req = req.WithContext(c.ctx)

	resp, err := c.httpClient.Do(req)
	if err != nil {
		slog.Error("sendOne Do", "method", method, "path", path, "error", err)
		return nil, err
	}
	defer func() {
		if e := resp.Body.Close(); err == nil {
			err = e
		}
	}()
	if body, err = io.ReadAll(resp.Body); err != nil {
		slog.Error("sendOne ReadAll", "method", method, "path", path, "error", err)
		return nil, err
	}
	slog.Debug("Response", "body", string(body))
	switch resp.StatusCode {
	case 429:
		e := decode429(body)
		time.Sleep(time.Duration(e.Error.Data.RetryAfter * float64(time.Second)))
		return c.sendOne(method, path, payload)
	}
	return body, nil
}
