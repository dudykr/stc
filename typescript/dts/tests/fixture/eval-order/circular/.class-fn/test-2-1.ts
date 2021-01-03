export class TrollC {
    trollM() {
        return trollF()
    }
}

export function trollF() {
    const c = new TrollC();
    c.trollM();
    return c;
}

