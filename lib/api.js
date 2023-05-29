import * as fs from 'fs';

import { getToken } from '../database/config.js';
import { PriorityQueue } from './priority_queue.js';

let busy = false; // lets us know if we are already sending api requests or not.
let headers = undefined; // a file scope variable so that we only evaluate these once.
let queue = new PriorityQueue(); // a priority queue to hold api calls we want to send, allows for throttling.

// send takes a request object as argument and an optional context ctx
// example request: {
//    endpoint: the path part of the url to call,
//    method: HTTP method for `fetch` call, defaults to 'GET',
//    payload: optional json object that will be send along with the request,
//    priority: optional priority value (defaults to 10, lower than 10 means the message will be sent faster)
// }
export function send(request, ctx) {
	return new Promise((resolve, reject) => {
		let data = {
			ctx: ctx,
			reject: reject,
			request: request,
			resolve: resolve,
		};
		if (!busy) {
			busy = true;
			send_this(data);
		} else {
			queue.enqueue(data, request.priority ? request.priority : 10);
		}
	});
}

function send_next() {
	if (queue.isEmpty()) {
		busy = false;
	} else {
		send_this(queue.dequeue().element);
	}
}

// send_this take a data object as argument built in the send function above
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
	if (data.request.method !== undefined) {
		options['method'] = data.request.method;
	}
	if (data.request.payload !== undefined) {
		options['body'] = JSON.stringify(data.request.payload);
	}
	fs.writeFileSync('log', JSON.stringify({event: 'send', date: new Date(), data: data}) + '\n', {flag: 'a+'});
	fetch(`https://api.spacetraders.io/v2${data.request.endpoint}`, options)
		.then(response => response.json())
		.then(response => {
			fs.writeFileSync('log', JSON.stringify({event: 'response', date: new Date(), data: response}) + '\n', {flag: 'a+'});
			return data.resolve(response);})
		.catch(err => {
			fs.writeFileSync('log', JSON.stringify({event: 'error', date: new Date(), data: err}) + '\n', {flag: 'a+'});
			data.reject(err)});
	setTimeout(send_next, 500);
}

export function debugLog(ctx) {
	console.log(`--- ${Date()} -----------------------------------------------------------------------------`);
	console.log(JSON.stringify(ctx, null, 2));
}

export function sleep(delay) {
	return new Promise((resolve) => setTimeout(resolve, delay))
}
