import Child from './test-2';

export default class Parent {
    isChild() {
        return this instanceof Child;
    }
}