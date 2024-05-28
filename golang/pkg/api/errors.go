package api

import (
	"encoding/json"
	"fmt"
	"time"
)

// ----- 429 --------------------------------------------------------------------
type RateLimitError struct {
	LimitType      string    `json:"type"`
	RetryAfter     Duration  `json:"retryAfter"`
	LimitBurst     int       `json:"limitBurst"`
	LimitPerSecond int       `json:"limitPerSecond"`
	Remaining      int       `json:"remaining"`
	Reset          time.Time `json:"reset"`
}

func decodeRateLimitError(msg json.RawMessage) RateLimitError {
	var e RateLimitError
	if err := json.Unmarshal(msg, &e); err != nil {
		panic(fmt.Errorf("Failed to decode iapi error code 429 RateLimitError: %v, %w", msg, err))
	}
	return e
}

// ----- 4214 -------------------------------------------------------------------
type ShipInTransitError struct {
	Arrival           time.Time `json:"arrival"`
	DepartureSymbol   string    `json:"departureSymbol"`
	DepartureTime     time.Time `json:"departureTime"`
	DestinationSymbol string    `json:"destinationSymbol"`
	SecondsToArrival  Duration  `json:"secondsToArrival"`
}

func decodeShipInTransitError(msg json.RawMessage) ShipInTransitError {
	var e ShipInTransitError
	if err := json.Unmarshal(msg, &e); err != nil {
		panic(fmt.Errorf("Failed to decode api error code 4214 ShipInTransitError: %v, %w", msg, err))
	}
	return e
}
