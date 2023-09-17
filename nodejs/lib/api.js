import * as fs from 'fs';
import * as events from 'events';

import { getToken } from '../database/tokens.js';
import { PriorityQueue } from './priority_queue.js';

// queue processor module variables
const bus = new events.EventEmitter(); // a bus to notify the queue processor to start processing messages
let busy = false; // true if we are already sending api requests.
let backoffSeconds = 0;
let running = false;
// other module variables
let headers = undefined; // a file scope variable so that we only evaluate these once.
let queue = new PriorityQueue(); // a priority queue to hold api calls we want to send, allows for throttling.

// a single queue processor should be running at any time, otherwise there will be trouble!
async function queue_processor() {
	if (running) {
		throw 'refusing to start a second queue processor';
	}
	running = true;
	while(true) {
		try {
			if (backoffSeconds > 0) {
				await sleep(backoffSeconds * 1000);
				backoffSeconds = 0;
			}
			if (queue.isEmpty()) {
				busy = false;
				await new Promise(resolve => bus.once('send', resolve));
				busy = true;
			}
			const before = new Date();
			await send_this(queue.dequeue().element);
			const duration = new Date() - before;
			if (duration < 400) { // 333 should work, but 400 should still allow some manual requests to go through during development
				await sleep(400 - duration);
			}
		} catch (e) {
			running = false;
			throw e;
		}
	}
}
queue_processor();

// send takes a request object as argument and an optional context ctx
// example request: {
//    endpoint: the path part of the url to call,
//    method: HTTP method for `fetch` call, defaults to 'GET',
//    page: run a paginated request starting from this page until all the following pages are fetched
//    payload: optional json object that will be send along with the request,
//    priority: optional priority value (defaults to 10, lower than 10 means the message will be sent faster)
// }
export async function send(request, ctx) {
	if (request.page === undefined) {
		return await send_one(request, ctx);
	}
	let ret = [];
	while (true) {
		const response = await send_one(request, ctx);
		if (response.meta === undefined) {
			throw {"message": "paginated request did not return a meta block", "request": request, "response": response};
		}
		ret = ret.concat(response.data);
		if (response.meta.limit * response.meta.page >= response.meta.total) {
			return ret;
		}
		request.page++;
	}
}

function send_one(request, ctx) {
	return new Promise((resolve, reject) => {
		let data = {
			ctx: ctx,
			reject: reject,
			request: request,
			resolve: resolve,
		};
		queue.enqueue(data, request.priority ? request.priority : 10);
		if (!busy) {
			bus.emit('send'); // the queue was previously empty, let's wake up the queue_processor
		}
	});
}

// send_this take a data object as argument built in the send function above
async function send_this(data) {
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
	let pagination = "";
	if (data.request.page !== undefined) {
		pagination=`?limit=20&page=${data.request.page}`;
	}
	fs.writeFileSync('log', JSON.stringify({event: 'send', date: new Date(), data: data}) + '\n', {flag: 'a+'});
	try {
		let response = await fetch(`https://api.spacetraders.io/v2${data.request.endpoint}${pagination}`, options);
		response = await response.json();
		switch(response.error?.code) {
		case 401: // TODO 401 means a server reset happened
			throw response;
			// TODO reject all promises in queue
			// reset database
			// logrotate
			// spawnSync?
			// break;
		case 429:  // 429 means rate limited, let's hold back as instructed
			backoffSeconds = response.error.data.retryAfter;
			queue.enqueue(data, 1);
			break;
		case 503:  // 503 means maintenance mode, let's hold back for 1 minute
			backoffSeconds = 60;
			queue.enqueue(data, 1);
			break;
		default: // no error!
			fs.writeFileSync('log', JSON.stringify({event: 'response', date: new Date(), data: response}) + '\n', {flag: 'a+'});
			return data.resolve(response);
		}
	} catch (err) {
		fs.writeFileSync('log', JSON.stringify({event: 'error', date: new Date(), data: err}) + '\n', {flag: 'a+'});
		switch(err.cause?.code) {
		case 'EAI_AGAIN': // DNS lookup timed out error, let's hold back for 5 seconds
			backoffSeconds = 5;
			queue.enqueue(data, 1);
			break;
		case 'ECONNRESET':
			queue.enqueue(data, 1);
			break;
		case 'UND_ERR_CONNECT_TIMEOUT':
			queue.enqueue(data, 1);
			break;
		default:
			data.reject(err);
		}
	}
}

export function debugLog(ctx) {
	console.log(`--- ${Date()} -----------------------------------------------------------------------------`);
	console.log(JSON.stringify(ctx, null, 2));
}

export function sleep(delay) {
	return new Promise((resolve) => setTimeout(resolve, delay))
}
