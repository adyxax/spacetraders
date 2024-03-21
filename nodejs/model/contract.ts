export type Contract = {
	id: string;
	factionSymbol: string;
	type: string;
	terms: {
		deadline: Date;
		payment: {
			onAccepted: number;
			onFulfilled: number;
		},
		deliver: Array<{
			tradeSymbol: string;
			destinationSymbol: string;
			unitsRequired: number;
			unitsFulfilled: number;
		}>;
	};
	accepted: boolean;
	fulfilled: boolean;
	expiration: Date;
	deadlineToAccept: Date;
};
