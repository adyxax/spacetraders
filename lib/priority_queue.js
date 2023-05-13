export class QElement {
	constructor(element, priority) {
		this.element = element;
		this.priority = priority;
	}
}

export class PriorityQueue {
	constructor(elt) {
		this.items = [];
		if (elt !== undefined) {
			this.enqueue(elt, 0);
		}
	}

	enqueue(element, priority) {
		let qElement = new QElement(element, priority);

		for (let i = 0; i < this.items.length; ++i) {
			if (this.items[i].priority > qElement.priority) {
				this.items.splice(i, 0, qElement);
				return;
			}
		}
		this.items.push(qElement);
	}
	dequeue() {
		return this.items.shift(); // we would use pop to get the highest priority, shift() gives us the lowest priority
	}
	front() {
		return this.items[0];
	}
	rear() {
		return this.items[this.items.length - 1];
	}
	isEmpty() {
		return this.items.length === 0;
	}
}
