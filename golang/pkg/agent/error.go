package agent

import "git.adyxax.org/adyxax/spacetraders/golang/pkg/model"

type shipError struct {
	err  error
	ship *model.Ship
}

func (a *agent) sendShipError(err error, ship *model.Ship) {
	a.channel <- shipError{
		err:  err,
		ship: ship,
	}
}
