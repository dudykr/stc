
export interface NodeList {
    length: number;
}

export interface HTMLCollection {
    length: number;
}

declare function isNodeList(sourceObj: any): sourceObj is NodeList;
declare function isHTMLCollection(sourceObj: any): sourceObj is HTMLCollection;

type EventTargetLike = { a: string } | HTMLCollection | NodeList;

var sourceObj: EventTargetLike = <any>undefined;

if (isNodeList(sourceObj) || isHTMLCollection(sourceObj)) {
    sourceObj.length;
}
