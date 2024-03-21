export type APIError = {
	apiError: 'APIError';
	error: string;
	code: number;
	data: any; // TODO
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
