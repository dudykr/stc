declare module "m" {
    module x {
        interface c {
        }
    }
    export import a = x.c;
    export = x;
}