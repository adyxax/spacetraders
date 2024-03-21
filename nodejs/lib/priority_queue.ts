class QElement {
	element: unknown;
	priority: number;
	constructor(element: unknown, priority: number) {
		this.element = element;
		this.priority = priority;
	}
}

export class PriorityQueue {
	items: Array<QElement>;

	constructor(elt?: unknown) {
		this.items = [];
		if (elt !== undefined) this.enqueue(elt, 0);
	}

	enqueue(element: unknown, priority: number): void {
		let qElement = new QElement(element, priority);

		for (let i = 0; i < this.items.length; ++i) {
			if (this.items[i].priority > qElement.priority) {
				this.items.splice(i, 0, qElement);
				return;
			}
		}
		this.items.push(qElement);
	}
	dequeue(): unknown {
		// we would use pop to get the highest priority, shift() gives us the lowest priority
		return this.items.shift()?.element;
	}
	isEmpty(): boolean {
		return this.items.length === 0;
	}
}
