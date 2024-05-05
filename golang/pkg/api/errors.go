package api

import (
	"encoding/json"
	"fmt"
	"time"
)

type RateLimitError struct {
	LimitType      string    `json:"type"`
	RetryAfter     float64   `json:"retryAfter"`
	LimitBurst     int       `json:"limitBurst"`
	LimitPerSecond int       `json:"limitPerSecond"`
	Remaining      int       `json:"remaining"`
	Reset          time.Time `json:"reset"`
}

func decode429(msg []byte) (e APIMessage[any, RateLimitError]) {
	if err := json.Unmarshal(msg, &e); err != nil {
		panic(fmt.Sprintf("Failed to decode419: %+v", err))
	}
	return e
}
