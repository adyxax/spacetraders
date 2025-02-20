package main

import (
	"context"
	"fmt"
	"log/slog"
	"os"
	"os/signal"
	"syscall"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/agent"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/api"
	"git.adyxax.org/adyxax/spacetraders/golang/pkg/database"
)

func main() {
	var opts *slog.HandlerOptions
	if os.Getenv("SPACETRADERS_DEBUG") != "" {
		opts = &slog.HandlerOptions{
			//AddSource: true,
			Level: slog.LevelDebug,
		}
	}
	logger := slog.New(slog.NewJSONHandler(os.Stdout, opts))
	slog.SetDefault(logger)

	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer cancel()

	db, err := database.NewDB(
		ctx,
		"./spacetraders.db?_txlock=immediate",
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "failed to open database: %+v\n", err)
		os.Exit(1)
	}

	apiClient := api.NewClient(ctx, db)
	defer apiClient.Close()
	if err := agent.Run(
		apiClient,
		db,
		os.Getenv,
	); err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err)
		if err := db.Close(); err != nil {
			fmt.Fprintf(os.Stderr, "%s\n", err)
		}
		os.Exit(2)
	}
}
