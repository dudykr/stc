// @strict: true
// @jsx: preserve
// @skipLibCheck: true
// @libFiles: lib.d.ts,react.d.ts
interface A { isIt: true; text: string; }
interface B { isIt: false; value: number; }
type C = A | B;
const isIt = Math.random() > 0.5;
const c: C = isIt ? { isIt, text: 'hey' } : { isIt, value: 123 };
const cc: C = isIt ? { isIt: isIt, text: 'hey' } : { isIt: isIt, value: 123 };

type ComponentProps =
    | {
        optionalBool: true;
        mandatoryFn: () => void;
    }
    | {
        optionalBool: false;
    };

let Funk = (_props: ComponentProps) => <div>Hello</div>;

let Fail1 = () => <Funk mandatoryFn={() => { }} optionalBool={true} />
let Fail2 = () => <Funk mandatoryFn={() => { }} optionalBool={true as true} />
let True = true as true;
let Fail3 = () => <Funk mandatoryFn={() => { }} optionalBool={True} />
let attrs2 = { optionalBool: true as true, mandatoryFn: () => { } }
let Success = () => <Funk {...attrs2} />