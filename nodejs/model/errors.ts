export type RateLimitError = {
	type: string;
	retryAfter: number;
	limitBurst: number;
	limitPerSecond: number;
	remaining: number;
	reset: Date;
};
