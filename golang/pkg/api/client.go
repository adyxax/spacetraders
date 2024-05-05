package api

import (
	"container/heap"
	"context"
	"net/http"
	"time"
)

type Client struct {
	baseURL    string
	channel    chan *Request
	ctx        context.Context
	headers    *http.Header
	httpClient *http.Client
	pq         *PriorityQueue
}

func NewClient(ctx context.Context) *Client {
	pq := make(PriorityQueue, 0)
	heap.Init(&pq)
	client := &Client{
		baseURL: "https://api.spacetraders.io/v2",
		channel: make(chan *Request),
		ctx:     ctx,
		headers: &http.Header{
			"Content-Type": {"application/json"},
		},
		httpClient: &http.Client{
			Timeout: time.Minute,
		},
		pq: &pq,
	}
	go queueProcessor(client)
	return client
}

func (c *Client) Close() {
	close(c.channel)
}

func (c *Client) SetToken(token string) {
	c.headers.Set("Authorization", "Bearer "+token)
}

func queueProcessor(client *Client) {
	var ok bool
	for {
		// The queue is empty so we do this blocking call
		req := <-client.channel
		heap.Push(client.pq, req)
		// we enqueue all values read from the channel and process the queue's
		// contents until empty. We keep reading the channel as long as this
		// emptying goes on
		for {
			select {
			case req = <-client.channel:
				heap.Push(client.pq, req)
			default:
				if client.pq.Len() == 0 {
					break
				}
				// we process one
				if req, ok = heap.Pop(client.pq).(*Request); !ok {
					panic("queueProcessor got something other than a Request on its channel")
				}
				response, err := client.sendOne(req.method, req.path, req.payload)
				req.resp <- &Response{
					Response: response,
					Err:      err,
				}
			}
		}
	}
}
