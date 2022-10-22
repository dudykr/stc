type Guid = string & { $Guid };          // Tagged string type
type SerialNo = number & { $SerialNo };  // Tagged number type

function createGuid() {
    return "21EC2020-3AEA-4069-A2DD-08002B30309D" as Guid;
}

function createSerialNo() {
    return 12345 as SerialNo;
}

let map1: { [x: string]: number } = {};
let guid = createGuid();
map1[guid] = 123;  // Can with tagged string

let map2: { [x: number]: string } = {};
let serialNo = createSerialNo();
map2[serialNo] = "hello";  // Can index with tagged number

export const s4 = guid + serialNo;
