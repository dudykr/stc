type Tag = { __tag: any };
declare function isNonBlank(value: string): value is string & Tag;
declare function doThis(value: string & Tag): void;
declare function doThat(value: string): void;
let value: string = "asdf";
if (isNonBlank(value)) {
  doThis(value);
} else {
  doThat(value);
}
