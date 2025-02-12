package agent

import (
	"sync"

	"git.adyxax.org/adyxax/spacetraders/golang/pkg/model"
)

type Agent struct {
	data  *model.Agent
	mutex sync.Mutex
}

var agent Agent

func GetAgent() *model.Agent {
	agent.mutex.Lock()
	defer agent.mutex.Unlock()
	return agent.data
}

func SetAgent(data *model.Agent) {
	agent.mutex.Lock()
	defer agent.mutex.Unlock()
	agent.data = data
}
