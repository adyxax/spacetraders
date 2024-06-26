import fs from 'fs';
import events from 'events';

import { PriorityQueue } from './priority_queue.ts';
import { getToken } from '../database/tokens.ts';
import { RateLimitError } from './errors.ts';

export type APIError = {
	error: string;
	code: number;
	data: unknown;
};

export type Meta = {
	limit: number;
	page: number;
	total: number;
}

export type Request = {
	endpoint: string; // the path part of the url to call
	method?: string; // HTTP method for `fetch` call, defaults to 'GET'
	page?: number; // run a paginated request starting from this page until all the following pages are fetched
	payload?: { [key:string]: any}; // optional json object that will be sent along the request
	priority?: number; // optional priority value, defaults to 10. lower than 10 means the message will be sent faster
};

export type RequestPromise<T> = {
	reject: (reason?: any) => void;
	request: Request;
	resolve: (value: Response<T> | PromiseLike<Response<T>>) => void;
};

export type Response<T> = {
	data: T;
	error?: APIError;
	meta?: Meta;
}

// queue processor module variables
const bus = new events.EventEmitter(); // a bus to notify the queue processor to start processing messages
let busy = false; // true if we are already sending api requests.
let backoffSeconds = 0;
let running = false;
// other module variables
let headers: {[key:string]:string}|null = null; // a file scoped variable so that we only evaluate these once.
let queue = new PriorityQueue(); // a priority queue to hold api calls we want to send, allows for throttling.

async function queue_processor(): Promise<void> {
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
			const before = new Date().getTime();
			const data = queue.dequeue() as RequestPromise<unknown>;
			await send_this(data);
			const duration = new Date().getTime() - before;
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

export async function send<T>(request: Request): Promise<Response<T>> {
	return new Promise((resolve, reject) => {
		const data: RequestPromise<T> = {
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

export async function sendPaginated<T>(request: Request): Promise<Array<T>> {
	if (request.page === undefined) request.page = 1;
	let ret: Array<T> = [];
	while (true) {
		const response = await send<T>(request);
		if (response.meta === undefined) {
			throw {"message": "paginated request did not return a meta block", "request": request, "response": response};
		} else if (response.error) {
			throw {"message": "paginated request returned an error", "request": request, "response": response};
		}
		ret = ret.concat(response.data);
		if (response.meta.limit * response.meta.page >= response.meta.total) {
			return ret;
		}
		request.page++;
	}
}

// send_this take a data object as argument built in the send function above
async function send_this(data: RequestPromise<unknown>): Promise<void> {
	if (headers === null) {
		const token = getToken();
		if (token === null) {
			throw 'Could not get token from the database. Did you init or register yet?';
		}
		headers = {
			'Content-Type': 'application/json',
			'Authorization': `Bearer ${token}`
		};
	}
	let options: {[key:string]:any} = {
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
		const response = await fetch(`https://api.spacetraders.io/v2${data.request.endpoint}${pagination}`, options);
		const json = await response.json() as Response<unknown>;
		switch(json.error?.code) {
			case 401: // TODO 401 means a server reset happened
				throw json;
				// TODO reject all promises in queue
				// reset database
				// logrotate
				// spawnSync?
				// break;
			case 429:  // 429 means rate limited, let's hold back as instructed
				const errorData = json.error.data as RateLimitError;
				backoffSeconds = errorData.retryAfter;
				queue.enqueue(data, 1);
				break;
			case 503:  // 503 means maintenance mode, let's hold back for 1 minute
				backoffSeconds = 60;
				queue.enqueue(data, 1);
				break;
			default: // no error!
				fs.writeFileSync('log', JSON.stringify({event: 'response', date: new Date(), data: json}) + '\n', {flag: 'a+'});
				return data.resolve(json);
		}
	} catch (_err) {
		const err = _err as {cause?: {code: string}};
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

export function debugLog(ctx: any): void {
	console.log(`--- ${Date()} -----------------------------------------------------------------------------`);
	console.log(JSON.stringify(ctx, null, 2));
}

export function sleep(delay: number): Promise<unknown> {
	return new Promise((resolve) => setTimeout(resolve, delay));
}
