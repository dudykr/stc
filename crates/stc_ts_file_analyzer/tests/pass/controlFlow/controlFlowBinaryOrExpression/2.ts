
export interface I1 {
    length: number;
}

export interface I2 {
    length: number;
}

declare function isNodeList(sourceObj: any): sourceObj is I1;
declare function isHTMLCollection(sourceObj: any): sourceObj is I2;

type EventTargetLike = { a: string } | I2 | I1;

var sourceObj: EventTargetLike = <any>undefined;

if (isHTMLCollection(sourceObj)) {
    sourceObj.length;
}

