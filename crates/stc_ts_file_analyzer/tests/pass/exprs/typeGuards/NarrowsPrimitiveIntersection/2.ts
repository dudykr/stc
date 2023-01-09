declare let value: string;

const enum Tag2 { }
declare function isNonBlank2(value: string): value is string & Tag2;
declare function doThis2(value: string & Tag2): void;
declare function doThat2(value: string): void;
isNonBlank2(value);
if (isNonBlank2(value)) {
  value;
  doThis2(value);
} else {
  value;
  doThat2(value);
}
