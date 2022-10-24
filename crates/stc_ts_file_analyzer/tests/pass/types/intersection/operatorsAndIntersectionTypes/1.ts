type Guid = string & { $Guid };          // Tagged string type

function createGuid() {
    return "21EC2020-3AEA-4069-A2DD-08002B30309D" as Guid;
}

let map1: { [x: string]: number } = {};
let guid = createGuid();
map1[guid] = 123;  // Can with tagged string

export const s3 = guid + guid;

