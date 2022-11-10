
export interface NodeList {
    length: number;
}

export interface HTMLCollection {
    length: number;
}

declare function isNodeList(sourceObj: any): sourceObj is NodeList;

type EventTargetLike = { a: string } | HTMLCollection | NodeList;

var sourceObj: EventTargetLike = <any>undefined;
if (isNodeList(sourceObj)) {
    sourceObj.length;
}
