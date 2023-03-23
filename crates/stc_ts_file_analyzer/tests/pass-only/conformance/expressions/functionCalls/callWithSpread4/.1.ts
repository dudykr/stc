//@strict: true
// @target: esnext
type R = { a: number }
type W = { b: number }
type RW = { a: number, b: number }
declare const pli: {
    (s1: R, s2: RW, s3: RW, s4: RW, s5: W): Promise<void>;
    (streams: ReadonlyArray<R | W | RW>): Promise<void>;
    (s1: R, s2: RW | W, ...streams: Array<RW | W>): Promise<void>;
}

declare var writes: W
declare var reads: R


pli(...[reads, writes, writes] as const)
export { }