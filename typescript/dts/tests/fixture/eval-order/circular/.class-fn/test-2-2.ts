export function trollF() {
    const c = new TrollC();
    c.trollM();
    return c;
}

export class TrollC {
    trollM() {
        return trollF()
    }
}