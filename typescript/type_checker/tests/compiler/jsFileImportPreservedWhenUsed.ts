// @target: esnext
// @allowJs: true
// @strict: true
// @outDir: ./out
// @filename: dash.d.ts
type ObjectIterator<TObject, TResult> = (value: TObject[keyof TObject], key: string, collection: TObject) => TResult;

interface LoDashStatic {
    mapValues<T extends object, TResult>(obj: T | null | undefined, callback: ObjectIterator<T, TResult>): { [P in keyof T]: TResult };
}
declare const _: LoDashStatic;
export = _;
// @filename: Consts.ts
export const INDEX_FIELD = '__INDEX';
// @filename: index.js
import * as _ from './dash';
import { INDEX_FIELD } from './Consts';

export class Test {
    /**
     * @param {object} obj
     * @param {object} vm
     */
    test(obj, vm) {
        let index = 0;
        vm.objects = _.mapValues(
            obj,
            object => ({ ...object, [INDEX_FIELD]: index++ }),
        );
    }
}