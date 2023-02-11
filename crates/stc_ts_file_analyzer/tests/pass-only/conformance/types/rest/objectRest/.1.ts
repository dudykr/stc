// @target: es2015

var noContextualType = ({ aNumber = 12, ...notEmptyObject }) => aNumber + notEmptyObject.anythingGoes;
