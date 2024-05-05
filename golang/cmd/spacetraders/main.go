package main

import (
	"context"
	"fmt"
	"log/slog"
	"os"
	"os/signal"

	"git.adyxax.org/adyxax/spacetraders/v2/pkg/api"
	"git.adyxax.org/adyxax/spacetraders/v2/pkg/database"
)

func main() {
	opts := &slog.HandlerOptions{
		//	//AddSource: true,
		Level: slog.LevelDebug,
	}
	//logger := slog.New(slog.NewJSONHandler(os.Stdout, opts))
	logger := slog.New(slog.NewTextHandler(os.Stdout, opts))
	slog.SetDefault(logger)

	ctx, cancel := signal.NotifyContext(context.Background(), os.Interrupt)
	defer cancel()
	db, err := database.DBInit(ctx, "./spacetraders.db")
	if err != nil {
		fmt.Fprintf(os.Stderr, "DbInit error %+v\n", err)
		os.Exit(1)
	}
	client := api.NewClient(ctx)
	defer client.Close()
	err = run( //ctx,
		db,
		client,
		//os.Args,
		//os.Getenv,
		//os.Getwd,
		//os.Stdin,
		//os.Stdout,
		//os.Stderr,
	)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%s\n", err)
		if err = db.Close(); err != nil {
			fmt.Fprintf(os.Stderr, "%s\n", err)
		}
		os.Exit(2)
	}
}

func run( // ctx context.Context,
	db *database.DB,
	client *api.Client,
	//args []string,
	//getenv func(string) string,
	//getwd func() (string, error),
	//stdin io.Reader,
	//stdout, stderr io.Writer,
) (err error) {
	// ----- Get token or register ---------------------------------------------
	token, err := db.GetToken()
	if err != nil || token == "" {
		var r api.APIMessage[api.RegisterMessage, any]
		if r, err = client.Register("COSMIC", "ADYXAX-GO"); err != nil {
			// TODO handle server reset
			fmt.Printf("%+v, %+v\n", r, err)
			return err
		}
		if err = db.AddToken(r.Data.Token); err != nil {
			return err
		}
	}
	client.SetToken(token)
	// ----- Update agent ------------------------------------------------------
	agent, err := client.MyAgent()
	slog.Info("agent", "agent", agent, "err", err)
	return err
}
