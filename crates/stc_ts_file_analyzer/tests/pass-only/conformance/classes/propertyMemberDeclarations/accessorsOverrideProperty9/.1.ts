//@strict: true
//@target: es2017

// Mixin utilities
export type Constructor<T = {}> = new (...args: any[]) => T;
export type PropertiesOf<T> = { [K in keyof T]: T[K] };

export interface IApiItemConstructor extends Constructor<ApiItem>, PropertiesOf<typeof ApiItem> { }

// Base class
class ApiItem {
  public get members(): ReadonlyArray<ApiItem> {
    return [];
  }
}
