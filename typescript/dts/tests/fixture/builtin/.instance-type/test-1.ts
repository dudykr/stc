
export class A {
    b() {
        return 5;
    }
}


export declare const a: InstanceType<typeof A>
export const b = a.b()