

// Repro from #13747

export class Form<T> {
    private values: { [P in keyof T]?: T[P] } = {}
}
