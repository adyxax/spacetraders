import { getToken } from '../database/config.js';
import { PriorityQueue } from './priority_queue.js';

let busy = false; // lets us know if we are already sending api requests or not.
let headers = undefined; // a file scope variable so that we only evaluate these once.
let queue = new PriorityQueue(); // a priority queue to hold api calls we want to send, allows for throttling.

// chain takes an array of actions as argument. For each one it sets the `next` property.
// example action: {
//    action: function to call,
//    next: optional nested action object, would get overriden by this function, except for the last action,
//    ... other attributes as required by the action function (for example ship or waypoing symbol...)
// }
export function chain(actions) {
	for(let i=actions.length-1;i>0;--i) {
		actions[i-1].next = actions[i];
	}
	actions[0].action(actions[0]);
}

// send takes a data object as argument
// example data: {
//    endpoint: the url endpoint to call,
//    method: HTTP method for `fetch` call, defaults to 'GET',
//    next: optional nested action object, as specified above with the chain function,
//    payload: optional json object that will be send along with the request,
// }
export function send(data) {
	if (!busy) {
		send_this(data);
	} else {
		queue.enqueue(data, data.priority ? data.priority : 10);
	}
}

function send_next() {
	if (queue.isEmpty()) {
		busy = false;
	} else {
		send_this(queue.dequeue().element);
	}
}

function send_this(data) {
	if (headers === undefined) {
		const token = getToken();
		if (token === null) {
			throw 'Could not get token from the database. Did you init or register yet?';
		}
		headers = {
			'Content-Type': 'application/json',
			'Authorization': `Bearer ${token}`
		};
	}
	let options = {
		headers: headers,
	};
	if (data.method !== undefined) {
		options['method'] = data.method;
	}
	if (data.payload !== undefined) {
		options['body'] = JSON.stringify(data.payload);
	}
	busy = true;
	fetch(`https://api.spacetraders.io/v2${data.endpoint}`, options)
		.then(response => response.json())
		.then(response => {
			if (data.next !== undefined) { // if we have a next action, call it now
				data.next.action(data.next, response);
			} else { // otherwise use this debug action
				console.log(JSON.stringify(response, null, 2));
			}
		})
		.catch(err => console.error(err));
	setTimeout(send_next, 500);
}
