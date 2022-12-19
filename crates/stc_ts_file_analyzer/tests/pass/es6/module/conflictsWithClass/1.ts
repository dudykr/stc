class Foo {
  a = 'a'
}

var foo: Foo

module Foo {
  var inside = foo
  var a = inside.a
}
