interface IProps {
    iconProp?: string;
    nestedProp?: {
        testBool?: boolean;
    }
}

interface INestedProps {
    nestedProps?: IProps;
}

// These are the types of errors we want:
const propB1: IProps | number = { INVALID_PROP_NAME: 'share', iconProp: 'test' };

// Nested typing works here and we also get an expected error:
const propB2: IProps | number = { nestedProp: { asdfasdf: 'test' }, iconProp: 'test' };

// Want an error generated here but there isn't one.
const propA1: INestedProps | number = { nestedProps: { INVALID_PROP_NAME: 'share', iconProp: 'test' } };
