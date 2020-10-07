
export class Foo {
    field: string = ''


    method() {
        const { field } = this;

        return field;
    }

}

export const res1 = new Foo().method()