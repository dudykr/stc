// @target: es2020

const matches = /\w/g[Symbol.matchAll]("matchAll");
const array = [...matches];
const { index, input } = array[0];
