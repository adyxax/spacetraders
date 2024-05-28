package api

import (
	"container/heap"
	"context"
	"net/http"
	"net/url"
	"time"
)

type Client struct {
	baseURI         *url.URL
	requestsChannel chan *Request
	ctx             context.Context
	headers         *http.Header
	httpClient      *http.Client
	pq              *PriorityQueue
}

func NewClient(ctx context.Context) *Client {
	baseURI, err := url.Parse("https://api.spacetraders.io/v2/")
	if err != nil {
		panic("baseURI failed to parse")
	}
	pq := make(PriorityQueue, 0)
	heap.Init(&pq)
	client := &Client{
		baseURI:         baseURI,
		requestsChannel: make(chan *Request),
		ctx:             ctx,
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
	close(c.requestsChannel)
}

func (c *Client) SetToken(token string) {
	c.headers.Set("Authorization", "Bearer "+token)
}
