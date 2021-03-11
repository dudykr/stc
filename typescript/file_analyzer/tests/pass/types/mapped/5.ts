

// Repro from #13747

export class Form<T> {
    values: { [P in keyof T]?: T[P] } = {}
}

interface Fields {
    email: string
    password: string
}

export declare var form: Form<Fields>

form.values.email
form.values.password