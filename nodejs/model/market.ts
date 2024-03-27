import { CommonThing } from 'common.ts';

export type TradeGood = CommonThing & {
	activity: string;
	purchasePrice: number;
	sellPrice: number;
	supply: string;
	tradeVolume: number;
	type: string;
};

export type Market = {
	symbol: string;
	exchange: Array<CommonThing>;
	exports: Array<CommonThing>;
	imports: Array<CommonThing>;
	//transactions: Array<Transaction>;
	tradeGoods: Array<TradeGood>;
};
