//@strict: true

// Repro from #26163

type Mapped<T> = { [K in keyof T]: T[K] };

// Repro from #26163

declare function acceptArray(arr: any[]): void;
declare function mapArray<T extends any[]>(arr: T): Mapped<T>;
export function acceptMappedArray<T extends any[]>(arr: T) {
    acceptArray(mapArray(arr));
}

