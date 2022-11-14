// Hover over `Errors1` to see what's wrong with this input
type Errors1 = TypeCheck<`

// Define a constant
const a = 5;

// Can it be changed?
a = 3;

`>;

declare const err1: Errors1;

err1

// Hover over `Errors2` to see what's wrong with this input
type Errors2 = TypeCheck<`

// Define a let variable
let b = "hello";

// Can it be changed?
b = "world";

// What about changing its type?
b = true;

`>;

// Hover over `Errors3` to see what's wrong with this input
type Errors3 = TypeCheck<`

// Now with type annotations
let c: number = true;

`>;

// Hover over `Errors4` to see what's wrong with this input
type Errors4 = TypeCheck<`

// Even local references
if (true) {
  let a = 5;

  // This works
  console.log(a);
}

// This doesn't :(
console.log(a);

`>;

/**
 *  _                                     _       _
 * | |                                   (_)     | |
 * | |__  _   _ _ __   ___  ___  ___ _ __ _ _ __ | |_
 * | '_ \| | | | '_ \ / _ \/ __|/ __| '__| | '_ \| __|
 * | | | | |_| | |_) |  __/\__ \ (__| |  | | |_) | |_
 * |_| |_|\__, | .__/ \___||___/\___|_|  |_| .__/ \__|
 *         __/ | |                         | |
 *        |___/|_|                         |_|
 *
 * This is a simplified implementation of TypeScript's type system that's written
 * in TypeScript's type annotations. This means that it uses types only — with
 * no runtime code whatsoever.
 *
 * You pass TypeScript code as a string to the `TypeCheck` generic and get possible
 * type errors back.
 *
 * This project lives at https://github.com/ronami/HypeScript
 *
 * You can install `hypescript` in your own project with `yarn` or `npm`
 * (TypeScript 4.7 or later is required) with `yarn add hypescript`.
 *
 * (Tip: hover over the type aliases below to see type errors in your input)
 *
 */

/**
 * ==============================================================================
 *
 *                        Core library code bellow
 *
 * ==============================================================================
 */

// src/index.ts
export type TypeCheck<Input extends string> =
    // Tokenize input
    Tokenize<Input> extends infer TokenList
    ? TokenList extends Error<any, any>
    ? // If any errors, format them
    Format<[TokenList]>
    : TokenList extends Array<Token<any>>
    ? // If successful, parse into an ast
    Parse<TokenList> extends infer NodeList
    ? NodeList extends Error<any, any>
    ? // If any errors, format them
    Format<[NodeList]>
    : NodeList extends Array<BaseNode<any>>
    ? // If successful, type-check and format errors
    Format<Check<NodeList>>
    : never
    : never
    : never
    : never;

// src/Utils/ArrayUtils.ts
export type Tail<T extends Array<any>> = ((...t: T) => void) extends (
    h: any,
    ...rest: infer R
) => void
    ? R
    : never;

export type Push<T extends Array<any>, E> = [...T, E];

export type Unshift<T extends Array<any>, E> = [E, ...T];

export type Concat<T1 extends Array<any>, T2 extends Array<any>> = [
    ...T1,
    ...T2,
];

export type TailBy<
    T extends Array<any>,
    B extends number,
    A extends Array<any> = [],
> = B extends A['length'] ? T : TailBy<Tail<T>, B, Push<A, 0>>;

// src/Utils/Errors.ts
export type Error<Message extends string, LineNumber extends number> =
    | SyntaxError<Message, LineNumber>
    | ParsingError<Message, LineNumber>
    | TypeError<Message, LineNumber>;

export type SyntaxError<Message extends string, LineNumber extends number> = {
    type: 'SyntaxError';
    message: Message;
    lineNumber: LineNumber;
};

export type ParsingError<Message extends string, LineNumber extends number> = {
    type: 'ParsingError';
    message: Message;
    lineNumber: LineNumber;
};

export type TypeError<Message extends string, LineNumber extends number> = {
    type: 'TypeError';
    message: Message;
    lineNumber: LineNumber;
};

// src/Utils/Format.ts
export type Format<
    Errors extends Array<Error<string, number>>,
    Result extends Array<string> = [],
> = Errors extends []
    ? Result
    : Errors[0] extends Error<infer Message, infer LineNumber>
    ? Format<Tail<Errors>, Push<Result, `${LineNumber}: ${Message}`>>
    : never;

// src/Utils/ObjectUtils.ts
export type ObjectMerge<T1, T2> = Omit<T1, keyof T2> & T2;

// src/Tokenizer/Inputs.ts
export type Keywords =
    | 'const'
    | 'let'
    | 'if'
    | 'return'
    | 'null'
    | 'true'
    | 'false'
    | 'function';

export type Punctuation =
    | ','
    | '('
    | ')'
    | '['
    | ']'
    | '{'
    | '}'
    | '.'
    | ';'
    | ':'
    | '='
    | '*'
    | '+'
    | '-'
    | '/';

export type Numbers = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9';

export type Symbols =
    | 'a'
    | 'b'
    | 'c'
    | 'd'
    | 'e'
    | 'f'
    | 'g'
    | 'h'
    | 'i'
    | 'j'
    | 'k'
    | 'l'
    | 'm'
    | 'n'
    | 'o'
    | 'p'
    | 'q'
    | 'r'
    | 's'
    | 't'
    | 'u'
    | 'v'
    | 'w'
    | 'x'
    | 'y'
    | 'z'
    | 'A'
    | 'B'
    | 'C'
    | 'D'
    | 'E'
    | 'F'
    | 'G'
    | 'H'
    | 'I'
    | 'J'
    | 'K'
    | 'L'
    | 'M'
    | 'N'
    | 'O'
    | 'P'
    | 'Q'
    | 'R'
    | 'S'
    | 'T'
    | 'U'
    | 'V'
    | 'W'
    | 'X'
    | 'Y'
    | 'Z'
    | '0'
    | '1'
    | '2'
    | '3'
    | '4'
    | '5'
    | '6'
    | '7'
    | '8'
    | '9'
    | '_'
    | '$';

// src/Tokenizer/Math.ts
type SuccTable = {
    0: 1;
    1: 2;
    2: 3;
    3: 4;
    4: 5;
    5: 6;
    6: 7;
    7: 8;
    8: 9;
    9: 10;
    10: 11;
    11: 12;
    12: 13;
    13: 14;
    14: 15;
    15: 16;
    16: 17;
    17: 18;
    18: 19;
    19: 20;
    20: 21;
    21: 22;
    22: 23;
    23: 24;
    24: 25;
    25: 26;
    26: 27;
    27: 28;
    28: 29;
    29: 30;
    30: 31;
    31: 32;
    32: 33;
    33: 34;
    34: 35;
    35: 36;
    36: 37;
    37: 38;
    38: 39;
    39: 40;
    40: 41;
    41: 42;
    42: 43;
    43: 44;
    44: 45;
    45: 46;
    46: 47;
    47: 48;
    48: 49;
    49: 50;
    50: 51;
    51: 52;
    52: 53;
    53: 54;
    54: 55;
    55: 56;
    56: 57;
    57: 58;
    58: 59;
    59: 60;
};

export type Succ<T> = T extends keyof SuccTable ? SuccTable[T] : never;

// src/Tokenizer/StringUtils.ts
export type EatFirstChar<T> = T extends `${infer A}${infer B}` ? B : '';

export type GetFirstChar<T> = T extends `${infer A}${infer B}` ? A : '';

export type ConcatStrings<A extends string, B extends string> = `${A}${B}`;

export type StringContains<I extends string, T extends string> = I extends T
    ? true
    : I extends `${T}${infer _}`
    ? true
    : I extends `${infer _0}${T}${infer _1}`
    ? true
    : I extends `${infer _}${T}`
    ? true
    : false;

// src/Tokenizer/Tokenizer.ts
type TokenizeInput<
    Input extends string,
    FirstChar extends string,
    InputTail extends string,
    PrecedingLinebreak extends boolean,
    LineNumber extends number,
    Data extends TokenData<any, any> = TokenData<PrecedingLinebreak, LineNumber>,
> = Input extends `===${infer InputTail}`
    ? [GenericToken<'===', Data>, InputTail]
    : Input extends `==${infer InputTail}`
    ? [GenericToken<'==', Data>, InputTail]
    : FirstChar extends Punctuation
    ? [GenericToken<FirstChar, Data>, InputTail]
    : FirstChar extends Numbers
    ? TokenizeNumber<Input, '', Data, FirstChar>
    : FirstChar extends '"'
    ? TokenizeString<InputTail, '"', Data>
    : FirstChar extends "'"
    ? TokenizeString<InputTail, "'", Data>
    : FirstChar extends Symbols
    ? TokenizeSymbol<Input, '', Data, FirstChar>
    : SyntaxError<`Invalid character.`, LineNumber>;

type TokenizeNumber<
    Input extends string,
    Result extends string,
    PrecedingLinebreak extends TokenData<any, any>,
    FirstChar extends string = GetFirstChar<Input>,
> = FirstChar extends Numbers
    ? TokenizeNumber<
        EatFirstChar<Input>,
        ConcatStrings<Result, FirstChar>,
        PrecedingLinebreak
    >
    : [NumberToken<Result, PrecedingLinebreak>, Input];

type TokenizeString<
    Input extends string,
    QuoteType extends '"' | "'",
    Data extends TokenData<any, any>,
> = Input extends `${infer Before}${QuoteType}${infer After}`
    ? StringContains<Before, '\n'> extends true
    ? SyntaxError<'Unterminated string literal.', Data['lineNumber']>
    : [StringToken<Before, Data>, After]
    : SyntaxError<'Unterminated string literal.', Data['lineNumber']>;

type TokenizeSymbol<
    Input extends string,
    Result extends string,
    PrecedingLinebreak extends TokenData<any, any>,
    FirstChar extends string = GetFirstChar<Input>,
> = FirstChar extends Symbols
    ? TokenizeSymbol<
        EatFirstChar<Input>,
        ConcatStrings<Result, FirstChar>,
        PrecedingLinebreak
    >
    : [
        Result extends Keywords
        ? KeywordToken<Result, PrecedingLinebreak>
        : SymbolToken<Result, PrecedingLinebreak>,
        Input,
    ];

type TokenizeHelper<
    TokenizeResult,
    Result extends Array<any>,
    LineNumber extends number,
> = TokenizeResult extends Array<any>
    ? Tokenize<
        TokenizeResult[1],
        Push<Result, TokenizeResult[0]>,
        LineNumber,
        false
    >
    : TokenizeResult;

export type Tokenize<
    Input extends string,
    Result extends Array<Token<any>> = [],
    LineNumber extends number = 1,
    PrecedingLinebreak extends boolean = false,
    FirstChar extends string = GetFirstChar<Input>,
    InputTail extends string = EatFirstChar<Input>,
> = Input extends ''
    ? Result
    : FirstChar extends ' '
    ? Tokenize<InputTail, Result, LineNumber, PrecedingLinebreak>
    : Input extends `//${infer Commented}\n${infer Rest}`
    ? Tokenize<Rest, Result, Succ<LineNumber>, true>
    : FirstChar extends '\n'
    ? Tokenize<InputTail, Result, Succ<LineNumber>, true>
    : TokenizeInput<
        Input,
        FirstChar,
        InputTail,
        PrecedingLinebreak,
        LineNumber
    > extends infer TokenizeResult
    ? TokenizeHelper<TokenizeResult, Result, LineNumber>
    : never;

// src/Tokenizer/Tokens.ts
export type TokenData<
    PrecedingLinebreak extends boolean,
    LineNumber extends number,
> = {
    precedingLinebreak: PrecedingLinebreak;
    lineNumber: LineNumber;
};

export type GenericToken<
    Value extends string,
    Data extends TokenData<boolean, number>,
> = {
    type: 'generic';
    value: Value;
    data: Data;
};

export type KeywordToken<
    Value extends string,
    Data extends TokenData<boolean, number>,
> = {
    type: 'keyword';
    value: Value;
    data: Data;
};

export type NumberToken<
    Value extends string,
    Data extends TokenData<boolean, number>,
> = {
    type: 'number';
    value: Value;
    data: Data;
};

export type StringToken<
    Value extends string,
    Data extends TokenData<boolean, number>,
> = {
    type: 'string';
    value: Value;
    data: Data;
};

export type SymbolToken<
    Value extends string,
    Data extends TokenData<boolean, number>,
> = {
    type: 'symbol';
    value: Value;
    data: Data;
};

export type Token<
    Data extends TokenData<boolean, number>,
    Value extends string = string,
> =
    | GenericToken<Value, Data>
    | NumberToken<Value, Data>
    | StringToken<Value, Data>
    | SymbolToken<Value, Data>
    | KeywordToken<Value, Data>;

// src/Serializer/Serializer.ts
type SerializeHelper<
    Type extends StaticType,
    ShouldMapLiterals extends boolean,
> = ShouldMapLiterals extends true ? Type : MapLiteralToType<Type>;

export type Serialize<
    Type extends StaticType,
    ShouldMapLiterals extends boolean = false,
> = SerializeHelper<Type, ShouldMapLiterals> extends infer MappedType
    ? MappedType extends StringType
    ? 'string'
    : MappedType extends StringLiteralType<infer Value>
    ? Value
    : MappedType extends BooleanType
    ? 'boolean'
    : MappedType extends BooleanLiteralType<infer Value>
    ? Value
    : MappedType extends NumberType
    ? 'number'
    : MappedType extends NumberLiteralType<infer Value>
    ? Value
    : MappedType extends NullType
    ? 'null'
    : MappedType extends UndefinedType
    ? 'undefined'
    : MappedType extends VoidType
    ? 'void'
    : MappedType extends AnyType
    ? 'any'
    : MappedType extends UnknownType
    ? 'unknown'
    : MappedType extends NeverType
    ? 'never'
    : MappedType extends ArrayType<infer ElementsType>
    ? SerializeArray<ElementsType>
    : MappedType extends UnionType<infer UnionTypes>
    ? SerializeUnion<UnionTypes>
    : MappedType extends ObjectType<infer Properties>
    ? SerializeObject<Properties>
    : MappedType extends FunctionType<infer Params, infer Return>
    ? SerializeFunction<Params, Return>
    : never
    : never;

type SerializeFunction<
    Params extends Array<[string, StaticType]>,
    Return extends StaticType,
> = SerializeFunctionParams<Params> extends infer SerializedParams
    ? SerializedParams extends string
    ? `(${SerializedParams}) => ${Serialize<Return>}`
    : never
    : never;

type SerializeFunctionParams<
    Params extends Array<[string, StaticType]>,
    Result extends string = '',
> = Params extends []
    ? Result
    : Params[0] extends [infer Key, infer Value]
    ? Value extends StaticType
    ? Key extends string
    ? SerializeFunctionParams<
        Tail<Params>,
        `${Result}${Key}: ${Serialize<Value>}` extends infer SerializedSignature
        ? SerializedSignature extends string
        ? Params['length'] extends 1
        ? `${SerializedSignature}`
        : `${SerializedSignature}, `
        : never
        : never
    >
    : never
    : never
    : never;

type ShouldUseParens<Type extends StaticType> = Type extends UnionType<any>
    ? true
    : Type extends FunctionType<any, any>
    ? true
    : false;

type SerializeArray<ElementsType extends StaticType> =
    Serialize<ElementsType> extends infer SerializedElements
    ? SerializedElements extends string
    ? ShouldUseParens<ElementsType> extends true
    ? `(${SerializedElements})[]`
    : `${SerializedElements}[]`
    : never
    : never;

type SerializeUnion<
    UnionTypes extends Array<StaticType>,
    Result extends string = '',
> = UnionTypes extends []
    ? Result
    : UnionTypes[0] extends StaticType
    ? Serialize<UnionTypes[0]> extends infer SerializedType
    ? SerializedType extends string
    ? SerializeUnion<
        Tail<UnionTypes>,
        UnionTypes['length'] extends 1
        ? `${Result}${SerializedType}`
        : `${Result}${SerializedType} | `
    >
    : never
    : never
    : never;

type SerializeObject<
    Properties extends Array<[string, StaticType]>,
    Result extends string = '',
> = Properties extends []
    ? Result extends ''
    ? '{}'
    : `{ ${Result} }`
    : Properties[0] extends [infer Key, infer Value]
    ? Value extends StaticType
    ? Key extends string
    ? `${Key}: ${Serialize<Value>}` extends infer SerializedProperty
    ? SerializedProperty extends string
    ? SerializeObject<
        Tail<Properties>,
        Properties['length'] extends 1
        ? `${Result}${SerializedProperty};`
        : `${Result}${SerializedProperty}; `
    >
    : never
    : never
    : never
    : never
    : never;

// src/Parser/Ast.ts
export type NodeData<StartLine extends number, EndLine extends number> = {
    startLineNumber: StartLine;
    endLineNumber: EndLine;
};

export type NullLiteral<Data extends NodeData<number, number>> = {
    type: 'NullLiteral';
    data: Data;
};

export type NumericLiteral<
    Value extends string,
    Data extends NodeData<number, number>,
> = {
    type: 'NumericLiteral';
    value: Value;
    data: Data;
};

export type BooleanLiteral<
    Value extends boolean,
    Data extends NodeData<number, number>,
> = {
    type: 'BooleanLiteral';
    value: Value;
    data: Data;
};

export type StringLiteral<
    Value extends string,
    Data extends NodeData<number, number>,
> = {
    type: 'StringLiteral';
    value: Value;
    data: Data;
};

export type ArrayExpression<
    Elements extends Array<BaseNode<any>>,
    Data extends NodeData<number, number>,
> = {
    type: 'ArrayExpression';
    elements: Elements;
    data: Data;
};

export type ObjectExpression<
    Properties extends Array<ObjectProperty<any, any, any>>,
    Data extends NodeData<number, number>,
> = {
    type: 'ObjectExpression';
    properties: Properties;
    data: Data;
};

export type ObjectProperty<
    Key extends BaseNode<any>,
    Value extends BaseNode<any>,
    Data extends NodeData<number, number>,
> = {
    type: 'ObjectProperty';
    key: Key;
    value: Value;
    data: Data;
};

export type VariableDeclaration<
    Declarations extends Array<VariableDeclarator<any, any, any>>,
    Kind extends string,
    Data extends NodeData<number, number>,
> = {
    type: 'VariableDeclaration';
    declarations: Declarations;
    kind: Kind;
    data: Data;
};

export type VariableDeclarator<
    Id extends BaseNode<any>,
    Init extends BaseNode<any>,
    Data extends NodeData<number, number>,
> = {
    type: 'VariableDeclarator';
    id: Id;
    init: Init;
    data: Data;
};

export type FunctionDeclaration<
    Id extends BaseNode<any>,
    Params extends Array<BaseNode<any>>,
    Body extends BaseNode<any>,
    Data extends NodeData<number, number>,
> = {
    type: 'FunctionDeclaration';
    id: Id;
    params: Params;
    body: Body;
    data: Data;
};

export type FunctionExpression<
    Id extends BaseNode<any> | null,
    Params extends Array<BaseNode<any>>,
    Body extends BaseNode<any>,
    Data extends NodeData<number, number>,
> = {
    type: 'FunctionExpression';
    id: Id;
    params: Params;
    body: Body;
    data: Data;
};

export type Identifier<
    Name extends string,
    Annotation extends BaseNode<any> | null,
    Data extends NodeData<number, number>,
> = {
    type: 'Identifier';
    name: Name;
    typeAnnotation: Annotation;
    data: Data;
};

export type ExpressionStatement<
    Expression extends BaseNode<any>,
    Data extends NodeData<number, number>,
> = {
    type: 'ExpressionStatement';
    expression: Expression;
    data: Data;
};

export type CallExpression<
    Callee extends BaseNode<any>,
    Arguments extends Array<BaseNode<any>>,
    Data extends NodeData<number, number>,
> = {
    type: 'CallExpression';
    callee: Callee;
    arguments: Arguments;
    data: Data;
};

export type MemberExpression<
    Object extends BaseNode<any>,
    Property extends BaseNode<any>,
    Computed extends boolean,
    Data extends NodeData<number, number>,
> = {
    type: 'MemberExpression';
    object: Object;
    property: Property;
    computed: Computed;
    data: Data;
};

export type IfStatement<
    Test extends BaseNode<any>,
    Consequent extends BaseNode<any>,
    Data extends NodeData<number, number>,
> = {
    type: 'IfStatement';
    test: Test;
    consequent: Consequent;
    data: Data;
    // alternate: A;
};

export type ReturnStatement<
    Argument extends BaseNode<any> | null,
    Data extends NodeData<number, number>,
> = {
    type: 'ReturnStatement';
    argument: Argument;
    data: Data;
};

export type AssignmentExpression<
    Left extends BaseNode<any>,
    Right extends BaseNode<any>,
    Operator extends string,
    Data extends NodeData<number, number>,
> = {
    type: 'AssignmentExpression';
    left: Left;
    right: Right;
    operator: Operator;
    data: Data;
};

export type BinaryExpression<
    Left extends BaseNode<any>,
    Right extends BaseNode<any>,
    Operator extends string,
    Data extends NodeData<number, number>,
> = {
    type: 'BinaryExpression';
    left: Left;
    right: Right;
    operator: Operator;
    data: Data;
};

export type BlockStatement<
    Body extends Array<BaseNode<any>>,
    Data extends NodeData<number, number>,
> = {
    type: 'BlockStatement';
    body: Body;
    data: Data;
};

export type TypeAnnotation<
    Annotation extends BaseNode<any>,
    Data extends NodeData<number, number>,
> = {
    type: 'TypeAnnotation';
    typeAnnotation: Annotation;
    data: Data;
};

export type StringTypeAnnotation<Data extends NodeData<number, number>> = {
    type: 'StringTypeAnnotation';
    data: Data;
};

export type NumberTypeAnnotation<Data extends NodeData<number, number>> = {
    type: 'NumberTypeAnnotation';
    data: Data;
};

export type NullLiteralTypeAnnotation<Data extends NodeData<number, number>> = {
    type: 'NullLiteralTypeAnnotation';
    data: Data;
};

export type BooleanTypeAnnotation<Data extends NodeData<number, number>> = {
    type: 'BooleanTypeAnnotation';
    data: Data;
};

export type GenericTypeAnnotation<I, Data extends NodeData<number, number>> = {
    type: 'GenericTypeAnnotation';
    id: I;
    data: Data;
};

export type AnyTypeAnnotation<Data extends NodeData<number, number>> = {
    type: 'AnyTypeAnnotation';
    data: Data;
};

export type BaseNode<Data extends NodeData<number, number>> =
    | NumericLiteral<any, Data>
    | BooleanLiteral<any, Data>
    | StringLiteral<any, Data>
    | ArrayExpression<any, Data>
    | ObjectExpression<any, Data>
    | ObjectProperty<any, any, Data>
    | VariableDeclaration<any, any, Data>
    | VariableDeclarator<any, any, Data>
    | FunctionDeclaration<any, any, any, Data>
    | FunctionExpression<any, any, any, Data>
    | Identifier<any, any, Data>
    | NullLiteral<Data>
    | ExpressionStatement<any, Data>
    | CallExpression<any, any, Data>
    | MemberExpression<any, any, any, Data>
    | IfStatement<any, any, Data>
    | ReturnStatement<any, Data>
    | AssignmentExpression<any, any, any, Data>
    | BinaryExpression<any, any, any, Data>
    | BlockStatement<any, Data>
    | TypeAnnotation<any, Data>
    | StringTypeAnnotation<Data>
    | NumberTypeAnnotation<Data>
    | NullLiteralTypeAnnotation<Data>
    | BooleanTypeAnnotation<Data>
    | GenericTypeAnnotation<any, Data>
    | AnyTypeAnnotation<Data>;

// src/Parser/Parser.ts
type ParseIdentifier<
    TokenList extends Array<Token<any>>,
    CanBeAnnotated extends boolean,
> = TokenList[0] extends SymbolToken<
    infer Name,
    TokenData<any, infer IdentifierLineNumber>
>
    ? CanBeAnnotated extends true
    ? TokenList[1] extends GenericToken<
        ':',
        TokenData<any, infer ColonLineNumber>
    >
    ? ParseTypeAnnotation<TailBy<TokenList, 2>> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseResult<
        Identifier<
            Name,
            Node,
            NodeData<IdentifierLineNumber, IdentifierLineNumber>
        >,
        TokenList
    >
    : ParseErrorResult<'Type expected.', ColonLineNumber>
    : ParseResult<
        Identifier<
            Name,
            null,
            NodeData<IdentifierLineNumber, IdentifierLineNumber>
        >,
        Tail<TokenList>
    >
    : ParseResult<
        Identifier<
            Name,
            null,
            NodeData<IdentifierLineNumber, IdentifierLineNumber>
        >,
        Tail<TokenList>
    >
    : null;

type ParseTypeAnnotation<TokenList extends Array<Token<any>>> =
    TokenList[0] extends SymbolToken<'string', TokenData<any, infer LineNumber>>
    ? ParseResult<
        TypeAnnotation<
            StringTypeAnnotation<NodeData<LineNumber, LineNumber>>,
            NodeData<LineNumber, LineNumber>
        >,
        Tail<TokenList>
    >
    : TokenList[0] extends SymbolToken<
        'boolean',
        TokenData<any, infer LineNumber>
    >
    ? ParseResult<
        TypeAnnotation<
            BooleanTypeAnnotation<NodeData<LineNumber, LineNumber>>,
            NodeData<LineNumber, LineNumber>
        >,
        Tail<TokenList>
    >
    : TokenList[0] extends KeywordToken<
        'null',
        TokenData<any, infer LineNumber>
    >
    ? ParseResult<
        TypeAnnotation<
            NullLiteralTypeAnnotation<NodeData<LineNumber, LineNumber>>,
            NodeData<LineNumber, LineNumber>
        >,
        Tail<TokenList>
    >
    : TokenList[0] extends SymbolToken<
        'number',
        TokenData<any, infer LineNumber>
    >
    ? ParseResult<
        TypeAnnotation<
            NumberTypeAnnotation<NodeData<LineNumber, LineNumber>>,
            NodeData<LineNumber, LineNumber>
        >,
        Tail<TokenList>
    >
    : TokenList[0] extends SymbolToken<'any', TokenData<any, infer LineNumber>>
    ? ParseResult<
        TypeAnnotation<
            AnyTypeAnnotation<NodeData<LineNumber, LineNumber>>,
            NodeData<LineNumber, LineNumber>
        >,
        Tail<TokenList>
    >
    : TokenList[0] extends SymbolToken<
        infer E,
        TokenData<any, infer LineNumber>
    >
    ? ParseResult<
        TypeAnnotation<
            GenericTypeAnnotation<E, NodeData<LineNumber, LineNumber>>,
            NodeData<LineNumber, LineNumber>
        >,
        Tail<TokenList>
    >
    : null;

type ParseConstVariableDeclaration<
    TokenList extends Array<Token<any>>,
    Scope extends ScopeType,
    Id extends BaseNode<NodeData<number, number>>,
    KindLineNumber extends number,
> = TokenList[0] extends GenericToken<
    '=',
    TokenData<any, infer EqualsLineNumber>
>
    ? ParseExpression<Tail<TokenList>> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : Id extends Identifier<infer Name, any, NodeData<number, number>>
    ? ParseVariableDeclarationHelper<
        TokenList,
        Name,
        Scope,
        Id,
        Node,
        KindLineNumber,
        'const'
    >
    : never
    : ParseErrorResult<'Expression expected.', EqualsLineNumber>
    : ParseError<
        ParsingError<
            "'const' declarations must be initialized.",
            Id['data']['startLineNumber']
        >
    >;

type ParseLetVariableDeclaration<
    TokenList extends Array<Token<any>>,
    Scope extends ScopeType,
    Id extends BaseNode<NodeData<number, number>>,
    KindLineNumber extends number,
> = Id extends Identifier<infer Name, any, NodeData<number, number>>
    ? TokenList[0] extends GenericToken<
        '=',
        TokenData<any, infer EqualsLineNumber>
    >
    ? ParseExpression<Tail<TokenList>> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseVariableDeclarationHelper<
        TokenList,
        Name,
        Scope,
        Id,
        Node,
        KindLineNumber,
        'let'
    >
    : ParseErrorResult<'Expression expected.', EqualsLineNumber>
    : ParseError<
        ParsingError<
            "'let' declarations must be initialized.",
            Id['data']['startLineNumber']
        >
    >
    : never;

type ParseVariableDeclarationHelper<
    TokenList extends Array<Token<any>>,
    Name extends string,
    Scope extends ScopeType,
    Id extends BaseNode<NodeData<number, number>>,
    Init extends BaseNode<NodeData<number, number>>,
    KindLineNumber extends number,
    Kind extends string,
> = Name extends keyof Scope
    ? ParseError<
        ParsingError<
            `Cannot redeclare block-scoped variable '${Name}'.`,
            KindLineNumber
        >
    >
    : ParseResult<
        VariableDeclaration<
            [
                VariableDeclarator<
                    Id,
                    Init,
                    NodeData<
                        Id['data']['startLineNumber'],
                        Init extends BaseNode<any>
                        ? Init['data']['endLineNumber']
                        : Id['data']['endLineNumber']
                    >
                >,
            ],
            Kind,
            NodeData<
                KindLineNumber,
                Init extends BaseNode<any>
                ? Init['data']['endLineNumber']
                : Id['data']['endLineNumber']
            >
        >,
        TokenList,
        null,
        ObjectMerge<Scope, { [a in Name]: true }>
    >;

type ParseVariableDeclaration<
    TokenList extends Array<Token<any>>,
    Scope extends ScopeType,
> = TokenList[0] extends KeywordToken<
    infer Kind,
    TokenData<any, infer KindLineNumber>
>
    ? Kind extends 'const' | 'let'
    ? ParseIdentifier<Tail<TokenList>, true> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : Kind extends 'const'
    ? ParseConstVariableDeclaration<TokenList, Scope, Node, KindLineNumber>
    : ParseLetVariableDeclaration<TokenList, Scope, Node, KindLineNumber>
    : TokenList[1] extends KeywordToken<infer Value, any>
    ? ParseError<
        ParsingError<`Unexpected reserved word '${Value}'.`, KindLineNumber>
    >
    : ParseError<
        ParsingError<
            'Variable declaration list cannot be empty.',
            KindLineNumber
        >
    >
    : null
    : null;

type ParseMemberExpression<
    Node extends BaseNode<NodeData<number, number>>,
    TokenList extends Array<Token<any>>,
> = TokenList[0] extends GenericToken<'.', TokenData<any, infer DotLineNumber>>
    ? TokenList[1] extends SymbolToken<
        infer Name,
        TokenData<any, infer IdentifierLineNumber>
    >
    ? ParseResult<
        MemberExpression<
            Node,
            Identifier<
                Name,
                null,
                NodeData<IdentifierLineNumber, IdentifierLineNumber>
            >,
            false,
            NodeData<Node['data']['startLineNumber'], IdentifierLineNumber>
        >,
        TailBy<TokenList, 2>
    >
    : ParseErrorResult<'Identifier expected.', DotLineNumber>
    : TokenList[0] extends GenericToken<
        '[',
        TokenData<any, infer BracketLineNumber>
    >
    ? ParseExpression<Tail<TokenList>> extends ParseResult<
        infer ExpressionNode,
        infer ExpressionTokenList,
        infer ExpressionError
    >
    ? ExpressionError extends ParsingError<any, any>
    ? ParseError<ExpressionError>
    : ExpressionTokenList[0] extends GenericToken<
        ']',
        TokenData<any, infer ClosingBracketLineNumber>
    >
    ? ParseResult<
        MemberExpression<
            Node,
            ExpressionNode,
            true,
            NodeData<
                ExpressionNode['data']['startLineNumber'],
                ClosingBracketLineNumber
            >
        >,
        Tail<ExpressionTokenList>
    >
    : ParseError<
        ParsingError<
            "']' expected.",
            ExpressionNode['data']['startLineNumber']
        >
    >
    : ParseErrorResult<'Expression expected.', BracketLineNumber>
    : null;

type ParseAssignmentExpression<
    Left extends BaseNode<NodeData<number, number>>,
    TokenList extends Array<Token<any>>,
> = TokenList[0] extends GenericToken<
    '=',
    TokenData<any, infer EqualsLineNumber>
>
    ? ParseExpression<Tail<TokenList>> extends ParseResult<
        infer RightNode,
        infer RightTokenList,
        infer RightError
    >
    ? RightError extends ParsingError<any, any>
    ? ParseError<RightError>
    : Left extends
    | Identifier<any, any, NodeData<infer LeftLineNumber, any>>
    | MemberExpression<any, any, any, NodeData<infer LeftLineNumber, any>>
    ? ParseResult<
        AssignmentExpression<
            Left,
            RightNode,
            '=',
            NodeData<LeftLineNumber, RightNode['data']['startLineNumber']>
        >,
        RightTokenList
    >
    : ParseErrorResult<
        'The left-hand side of an assignment expression must be a variable or a property access.',
        EqualsLineNumber
    >
    : ParseErrorResult<'Expression expected.', EqualsLineNumber>
    : null;

type ParseCallExpression<
    Node extends BaseNode<NodeData<number, number>>,
    TokenList extends Array<Token<any>>,
> = TokenList[0] extends GenericToken<
    '(',
    TokenData<any, infer ParenLineNumber>
>
    ? ParseCallExpressionArguments<
        Tail<TokenList>,
        ParenLineNumber,
        ')'
    > extends ParseArrayResult<infer NodeList, infer TokenList, infer Error>
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseResult<
        CallExpression<
            Node,
            NodeList,
            NodeData<
                Node['data']['startLineNumber'],
                TokenList[0]['data']['lineNumber']
            >
        >,
        Tail<TokenList>
    >
    : null
    : null;

type ParseCallExpressionArguments<
    TokenList extends Array<Token<any>>,
    ParenLineNumber extends number,
    ClosingString extends string,
    NeedComma extends boolean = false,
    Result extends Array<BaseNode<any>> = [],
> = TokenList[0] extends GenericToken<ClosingString, any>
    ? ParseArrayResult<Result, TokenList>
    : TokenList extends []
    ? ParseErrorResult<`'${ClosingString}' expected.`, ParenLineNumber>
    : NeedComma extends true
    ? TokenList[0] extends GenericToken<',', any>
    ? ParseCallExpressionArgumentsHelper<
        Tail<TokenList>,
        ParenLineNumber,
        ClosingString,
        Result
    >
    : TokenList[0] extends Token<TokenData<any, infer LineNumber>>
    ? ParseErrorResult<"',' expected.", LineNumber>
    : never
    : ParseCallExpressionArgumentsHelper<
        TokenList,
        ParenLineNumber,
        ClosingString,
        Result
    >;

type ParseCallExpressionArgumentsHelper<
    TokenList extends Array<Token<any>>,
    ParenLineNumber extends number,
    ClosingString extends string,
    Result extends Array<BaseNode<any>> = [],
> = ParseExpression<TokenList> extends ParseResult<
    infer Node,
    infer TokenList,
    infer Error
>
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : TokenList[0] extends GenericToken<',', any>
    ? ParseCallExpressionArguments<
        Tail<TokenList>,
        ParenLineNumber,
        ClosingString,
        false,
        Push<Result, Node>
    >
    : ParseCallExpressionArguments<
        TokenList,
        ParenLineNumber,
        ClosingString,
        true,
        Push<Result, Node>
    >
    : null;

type ParseBinaryExpression<
    LeftNode extends BaseNode<NodeData<number, number>>,
    TokenList extends Array<Token<TokenData<boolean, number>>>,
> = TokenList[0] extends GenericToken<
    '===' | '==' | '+' | '-' | '*' | '/',
    TokenData<boolean, number>
>
    ? ParseExpression<Tail<TokenList>> extends ParseResult<
        infer RightNode,
        infer RightTokenList,
        infer RightError
    >
    ? RightError extends ParsingError<any, any>
    ? ParseError<RightError>
    : ParseResult<
        BinaryExpression<
            LeftNode,
            RightNode,
            TokenList[0]['value'],
            NodeData<
                LeftNode['data']['startLineNumber'],
                RightNode['data']['endLineNumber']
            >
        >,
        RightTokenList
    >
    : ParseErrorResult<
        'Expression expected.',
        TokenList[0]['data']['lineNumber']
    >
    : null;

type CheckExpression<
    Node extends BaseNode<any>,
    TokenList extends Array<Token<any>>,
> = ParseMemberExpression<Node, TokenList> extends ParseResult<
    infer Node,
    infer TokenList,
    infer Error
>
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : CheckExpression<Node, TokenList>
    : ParseAssignmentExpression<Node, TokenList> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : CheckExpression<Node, TokenList>
    : ParseBinaryExpression<Node, TokenList> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : CheckExpression<Node, TokenList>
    : ParseCallExpression<Node, TokenList> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : CheckExpression<Node, TokenList>
    : ParseResult<Node, TokenList>;

type ParseExpression<TokenList extends Array<Token<any>>> =
    ParseExpressionHelper<TokenList, Tail<TokenList>> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : CheckExpression<Node, TokenList>
    : null;

type TokenToNodeData<InputToken extends Token<any>> = InputToken extends Token<
    TokenData<any, infer LineNumber>
>
    ? NodeData<LineNumber, LineNumber>
    : never;

type ParseExpressionHelper<
    TokenList extends Array<Token<any>>,
    TokenTail extends Array<Token<any>> = Tail<TokenList>,
    Data extends NodeData<any, any> = TokenToNodeData<TokenList[0]>,
> = TokenList[0] extends KeywordToken<'true', any>
    ? ParseResult<BooleanLiteral<true, Data>, TokenTail>
    : TokenList[0] extends KeywordToken<'false', any>
    ? ParseResult<BooleanLiteral<false, Data>, TokenTail>
    : TokenList[0] extends KeywordToken<'null', any>
    ? ParseResult<NullLiteral<Data>, TokenTail>
    : TokenList[0] extends NumberToken<infer Value, any>
    ? ParseResult<NumericLiteral<Value, Data>, TokenTail>
    : TokenList[0] extends StringToken<infer Value, any>
    ? ParseResult<StringLiteral<Value, Data>, TokenTail>
    : ParseFunctionExpression<TokenList> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? ParseResult<Node, TokenList, Error>
    : TokenList[0] extends SymbolToken<infer Value, any>
    ? ParseResult<Identifier<Value, null, Data>, TokenTail>
    : TokenList[0] extends GenericToken<'[', TokenData<any, infer LineNumber>>
    ? ParseArrayExpression<Tail<TokenList>, LineNumber>
    : TokenList[0] extends GenericToken<'{', TokenData<any, infer LineNumber>>
    ? ParseObject<Tail<TokenList>, LineNumber>
    : null;

type ParseObject<
    TokenList extends Array<Token<any>>,
    InitialLineNumber extends number,
    Result extends Array<ObjectProperty<any, any, any>> = [],
    NeedComma extends boolean = false,
> = TokenList[0] extends GenericToken<'}', TokenData<any, infer L>>
    ? ParseResult<
        ObjectExpression<Result, NodeData<InitialLineNumber, L>>,
        Tail<TokenList>
    >
    : TokenList extends []
    ? ParseErrorResult<"'}' expected.", InitialLineNumber>
    : NeedComma extends true
    ? TokenList[0] extends GenericToken<',', any>
    ? ParseObjectItem<Tail<TokenList>, InitialLineNumber, Result>
    : TokenList[0] extends Token<TokenData<any, infer L>>
    ? ParseErrorResult<"',' expected.", L>
    : never
    : ParseObjectItem<TokenList, InitialLineNumber, Result>;

type ParseObjectItem<
    TokenList extends Array<Token<any>>,
    InitialLineNumber extends number,
    Result extends Array<ObjectProperty<any, any, any>> = [],
> = TokenList[0] extends SymbolToken<
    infer Name,
    TokenData<any, infer NameLineNumber>
>
    ? TokenList[1] extends GenericToken<':', any>
    ? ParseExpression<TailBy<TokenList, 2>> extends ParseResult<
        infer ValueNode,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseObjectItemHelper<
        TokenList,
        Identifier<Name, null, NodeData<NameLineNumber, NameLineNumber>>,
        ValueNode,
        InitialLineNumber,
        Result,
        NameLineNumber
    >
    : ParseErrorResult<'Expression expected.', InitialLineNumber>
    : ParseObjectItemHelper<
        Tail<TokenList>,
        Identifier<Name, null, NodeData<NameLineNumber, NameLineNumber>>,
        Identifier<Name, null, NodeData<NameLineNumber, NameLineNumber>>,
        InitialLineNumber,
        Result,
        NameLineNumber
    >
    : ParseErrorResult<"'}' expected.", InitialLineNumber>;

type ParseObjectItemHelper<
    TokenList extends Array<Token<any>>,
    Key extends BaseNode<any>,
    Value extends BaseNode<any>,
    InitialLineNumber extends number,
    Result extends Array<ObjectProperty<any, any, any>>,
    NameLineNumber extends number,
> = TokenList[0] extends GenericToken<',', any>
    ? ParseObject<
        Tail<TokenList>,
        InitialLineNumber,
        Push<
            Result,
            ObjectProperty<
                Key,
                Value,
                NodeData<NameLineNumber, Value['data']['endLineNumber']>
            >
        >,
        false
    >
    : ParseObject<
        TokenList,
        InitialLineNumber,
        Push<
            Result,
            ObjectProperty<
                Key,
                Value,
                NodeData<NameLineNumber, Value['data']['endLineNumber']>
            >
        >,
        true
    >;

type ParseArrayExpression<
    TokenList extends Array<Token<any>>,
    StartLineNumber extends number,
> = ParseCallExpressionArguments<
    TokenList,
    StartLineNumber,
    ']'
> extends ParseArrayResult<infer NodeList, infer TokenList, infer Error>
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseResult<
        ArrayExpression<
            NodeList,
            NodeData<StartLineNumber, TokenList[0]['data']['lineNumber']>
        >,
        Tail<TokenList>
    >
    : null;

type ParseExpressionStatement<TokenList extends Array<Token<any>>> =
    ParseExpression<TokenList> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseResult<ExpressionStatement<Node, Node['data']>, TokenList>
    : null;

type ParseFunctionDeclaration<
    TokenList extends Array<Token<any>>,
    Scope extends ScopeType,
> = TokenList[0] extends KeywordToken<
    'function',
    TokenData<any, infer FunctionLineNumber>
>
    ? TokenList[1] extends SymbolToken<
        infer Name,
        TokenData<any, infer FunctionNameLineNumber>
    >
    ? TokenList[2] extends GenericToken<
        '(',
        TokenData<any, infer ParenLineNumber>
    >
    ? ParseFunctionParams<
        TailBy<TokenList, 3>,
        ParenLineNumber
    > extends ParseArrayResult<infer NodeList, infer TokenList, infer Error>
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseBlockStatement<
        Tail<TokenList>,
        {},
        TokenList[0]['data']['lineNumber'],
        true
    > extends ParseResult<infer Node, infer TokenList, infer Error>
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : Name extends keyof Scope
    ? ParseError<
        ParsingError<
            `Cannot redeclare block-scoped variable '${Name}'.`,
            FunctionLineNumber
        >
    >
    : ParseResult<
        FunctionDeclaration<
            Identifier<
                Name,
                null,
                NodeData<FunctionNameLineNumber, FunctionNameLineNumber>
            >,
            NodeList,
            Node,
            NodeData<FunctionLineNumber, Node['data']['endLineNumber']>
        >,
        TokenList,
        null,
        ObjectMerge<Scope, { [a in Name]: true }>
    >
    : never
    : never
    : ParseErrorResult<"'(' expected.", FunctionNameLineNumber>
    : ParseErrorResult<'Identifier expected.', FunctionLineNumber>
    : null;

type ParseFunctionExpression<TokenList extends Array<Token<any>>> =
    TokenList[0] extends KeywordToken<
        'function',
        TokenData<any, infer FunctionLineNumber>
    >
    ? TokenList[1] extends SymbolToken<
        infer Name,
        TokenData<any, infer FunctionNameLineNumber>
    >
    ? ParseFunctionExpressionHelper<
        TailBy<TokenList, 2>,
        Identifier<
            Name,
            null,
            NodeData<FunctionNameLineNumber, FunctionNameLineNumber>
        >,
        FunctionLineNumber
    >
    : ParseFunctionExpressionHelper<Tail<TokenList>, null, FunctionLineNumber>
    : null;

type ParseFunctionExpressionHelper<
    TokenList extends Array<Token<any>>,
    Id extends BaseNode<any> | null,
    FunctionLineNumber extends number,
> = TokenList[0] extends GenericToken<
    '(',
    TokenData<any, infer ParenLineNumber>
>
    ? ParseFunctionParams<
        Tail<TokenList>,
        ParenLineNumber
    > extends ParseArrayResult<infer NodeList, infer TokenList, infer Error>
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseBlockStatement<
        Tail<TokenList>,
        {},
        FunctionLineNumber,
        true
    > extends ParseResult<infer Node, infer TokenList, infer Error>
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseResult<
        FunctionExpression<
            Id,
            NodeList,
            Node,
            NodeData<FunctionLineNumber, Node['data']['endLineNumber']>
        >,
        TokenList
    >
    : never
    : never
    : ParseErrorResult<"'(' expected.", FunctionLineNumber>;

type ParseFunctionParams<
    TokenList extends Array<Token<any>>,
    InitialLineNumber extends number,
    Result extends Array<BaseNode<NodeData<number, number>>> = [],
    NeedSemicolon extends boolean = false,
> = TokenList[0] extends GenericToken<
    ')',
    TokenData<any, infer ParenLineNumber>
>
    ? TokenList[1] extends GenericToken<'{', any>
    ? ParseArrayResult<Result, Tail<TokenList>>
    : ParseErrorResult<"'{' expected.", ParenLineNumber>
    : TokenList extends []
    ? ParseErrorResult<"')' expected.", InitialLineNumber>
    : NeedSemicolon extends true
    ? TokenList[0] extends GenericToken<',', any>
    ? ParseFunctionParamsHelper<Tail<TokenList>, InitialLineNumber, Result>
    : ParseError<
        ParsingError<"',' expected.", Result[0]['data']['endLineNumber']>
    >
    : ParseFunctionParamsHelper<TokenList, InitialLineNumber, Result>;

type ParseFunctionParamsHelper<
    TokenList extends Array<Token<any>>,
    LineNumber extends number,
    Result extends Array<BaseNode<any>>,
> = ParseIdentifier<TokenList, true> extends ParseResult<
    infer Node,
    infer TokenList,
    infer Error
>
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseFunctionParams<TokenList, LineNumber, Push<Result, Node>, true>
    : ParseErrorResult<'Identifier expected.', LineNumber>;

type ParseBlockStatement<
    TokenList extends Array<Token<any>>,
    Scope extends ScopeType,
    InitialLineNumber extends number,
    InFunctionScope extends boolean,
    Result extends Array<BaseNode<any>> = [],
    NeedSemicolon extends boolean = false,
> = TokenList extends []
    ? Result[0] extends BaseNode<NodeData<any, infer LineNumber>>
    ? ParseErrorResult<"'}' expected.", LineNumber>
    : ParseErrorResult<"'}' expected.", InitialLineNumber>
    : TokenList[0] extends GenericToken<'}', TokenData<any, infer LineNumber>>
    ? ParseResult<
        BlockStatement<Result, NodeData<InitialLineNumber, LineNumber>>,
        Tail<TokenList>
    >
    : TokenList[0] extends GenericToken<';', any>
    ? ParseBlockStatement<
        Tail<TokenList>,
        Scope,
        InitialLineNumber,
        InFunctionScope,
        Result,
        false
    >
    : NeedSemicolon extends false
    ? ParseBlockStatementHelper<
        TokenList,
        InitialLineNumber,
        InFunctionScope,
        Result,
        Scope
    >
    : TokenList[0] extends Token<
        TokenData<infer PrecedingLinebreak, infer LineNumber>
    >
    ? PrecedingLinebreak extends true
    ? ParseBlockStatementHelper<
        TokenList,
        LineNumber,
        InFunctionScope,
        Result,
        Scope
    >
    : ParseErrorResult<"';' expected.", LineNumber>
    : never;

type ParseTopLevel<
    TokenList extends Array<Token<any>>,
    Scope extends ScopeType,
    Result extends Array<BaseNode<any>> = [],
    NeedSemicolon extends boolean = false,
> = TokenList extends []
    ? ParseArrayResult<Result, TokenList>
    : TokenList[0] extends GenericToken<';', any>
    ? ParseTopLevel<Tail<TokenList>, Scope, Result, false>
    : NeedSemicolon extends false
    ? ParseTopLevelHelper<TokenList, Result, Scope>
    : TokenList[0] extends Token<
        TokenData<infer PrecedingLinebreak, infer LineNumber>
    >
    ? PrecedingLinebreak extends true
    ? ParseTopLevelHelper<TokenList, Result, Scope>
    : ParseErrorResult<"';' expected.", LineNumber>
    : never;

type ParseBlockStatementHelper<
    TokenList extends Array<Token<any>>,
    LineNumber extends number,
    InFunctionScope extends boolean,
    Result extends Array<BaseNode<any>>,
    Scope extends ScopeType,
> = ParseStatementHelper<TokenList, InFunctionScope, Scope> extends ParseResult<
    infer Node,
    infer TokenList,
    infer Error,
    infer Scope
>
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseBlockStatement<
        TokenList,
        Scope,
        LineNumber,
        InFunctionScope,
        Push<Result, Node>,
        NodeRequiresSemicolon<Node>
    >
    : never;

type ParseTopLevelHelper<
    TokenList extends Array<Token<any>>,
    Result extends Array<BaseNode<any>>,
    Scope extends ScopeType,
> = ParseStatementHelper<TokenList, false, Scope> extends ParseResult<
    infer Node,
    infer TokenList,
    infer Error,
    infer Scope
>
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseTopLevel<
        TokenList,
        Scope,
        Push<Result, Node>,
        NodeRequiresSemicolon<Node>
    >
    : never;

type ParseIfStatement<
    TokenList extends Array<Token<any>>,
    InFunctionScope extends boolean,
> = TokenList[0] extends KeywordToken<'if', TokenData<any, infer IfLineNumber>>
    ? TokenList[1] extends GenericToken<
        '(',
        TokenData<any, infer ParenLineNumber>
    >
    ? ParseExpression<TailBy<TokenList, 2>> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseIfStatementHelper<
        Node,
        TokenList,
        IfLineNumber,
        InFunctionScope,
        Node['data']['endLineNumber']
    >
    : ParseErrorResult<'Expression expected.', ParenLineNumber>
    : ParseErrorResult<"'(' expected.", IfLineNumber>
    : null;

type ParseReturnStatementHelper<
    TokenList extends Array<Token<any>>,
    StartLineNumber extends number,
> = TokenList[0] extends GenericToken<
    ';',
    TokenData<any, infer SemicolonLineNumber>
>
    ? ParseResult<
        ReturnStatement<null, NodeData<StartLineNumber, SemicolonLineNumber>>,
        Tail<TokenList>
    >
    : ParseExpression<TokenList> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseResult<
        ReturnStatement<
            Node,
            NodeData<StartLineNumber, Node['data']['endLineNumber']>
        >,
        TokenList
    >
    : ParseErrorResult<'Expression expected.', 1>;

type ParseReturnStatement<
    TokenList extends Array<Token<any>>,
    InFunctionScope extends boolean,
> = TokenList[0] extends KeywordToken<
    'return',
    TokenData<any, infer LineNumber>
>
    ? InFunctionScope extends true
    ? TokenList[1] extends Token<TokenData<infer PrecedingLinebreak, any>, any>
    ? PrecedingLinebreak extends false
    ? ParseReturnStatementHelper<Tail<TokenList>, LineNumber>
    : ParseResult<
        ReturnStatement<null, NodeData<LineNumber, LineNumber>>,
        Tail<TokenList>
    >
    : ParseResult<ReturnStatement<null, NodeData<LineNumber, LineNumber>>, []>
    : ParseError<
        ParsingError<
            "A 'return' statement can only be used within a function body.",
            LineNumber
        >
    >
    : null;

type ParseIfStatementHelper<
    Node extends BaseNode<any>,
    TokenList extends Array<Token<any>>,
    StartLineNumber extends number,
    InFunctionScope extends boolean,
    IfExpressionLineNumber extends number,
> = TokenList[0] extends GenericToken<
    ')',
    TokenData<any, infer ClosingParenLineNumber>
>
    ? TokenList[1] extends GenericToken<
        '{',
        TokenData<any, infer CurlyLineNumber>
    >
    ? ParseBlockStatement<
        TailBy<TokenList, 2>,
        {},
        CurlyLineNumber,
        InFunctionScope
    > extends ParseResult<infer BlockNode, infer TokenList, infer Error>
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseResult<
        IfStatement<
            Node,
            BlockNode,
            NodeData<StartLineNumber, BlockNode['data']['endLineNumber']>
        >,
        TokenList
    >
    : never
    : ParseErrorResult<"'{' expected.", ClosingParenLineNumber>
    : ParseErrorResult<"')' expected.", IfExpressionLineNumber>;

type ParseStatementHelper<
    TokenList extends Array<Token<any>>,
    InFunctionScope extends boolean,
    Scope extends ScopeType,
> = ParseFunctionDeclaration<TokenList, Scope> extends ParseResult<
    infer Node,
    infer TokenList,
    infer Error,
    infer Scope
>
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseResult<Node, TokenList, null, Scope>
    : ParseVariableDeclaration<TokenList, Scope> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error,
        infer Scope
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseResult<Node, TokenList, null, Scope>
    : ParseIfStatement<TokenList, InFunctionScope> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseResult<Node, TokenList, null, Scope>
    : ParseReturnStatement<TokenList, InFunctionScope> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseResult<Node, TokenList, null, Scope>
    : ParseExpressionStatement<TokenList> extends ParseResult<
        infer Node,
        infer TokenList,
        infer Error
    >
    ? Error extends ParsingError<any, any>
    ? ParseError<Error>
    : ParseResult<Node, TokenList, null, Scope>
    : ParseErrorResult<'Declaration or statement expected.', 1>;

export type Parse<TokenList extends Array<Token<any>>> = ParseTopLevel<
    TokenList,
    {}
> extends ParseArrayResult<infer NodeList, infer TokenList, infer Error>
    ? Error extends ParsingError<any, any>
    ? Error
    : NodeList
    : never;

// src/Parser/Utils.ts
export type ParseResult<
    Node extends BaseNode<NodeData<number, number>>,
    TokenList extends Array<Token<any>>,
    Error extends ParsingError<any, any> | null = null,
    Scope extends ScopeType = {},
> = {
    type: 'ParseResult';
    node: Node;
    tokenList: TokenList;
    error: Error;
    scope: Scope;
};

export type ParseArrayResult<
    NodeList extends Array<BaseNode<NodeData<number, number>>>,
    TokenList extends Array<Token<TokenData<boolean, number>>>,
    Error extends ParsingError<any, any> | null = null,
    Scope extends ScopeType = {},
> = {
    type: 'ParseResult';
    node: NodeList;
    tokenList: TokenList;
    error: Error;
    scope: Scope;
};

export type ParseError<Error extends ParsingError<any, any>> = ParseResult<
    any,
    any,
    Error
>;

export type ParseErrorResult<
    Message extends string,
    LineNumber extends number,
> = ParseError<ParsingError<Message, LineNumber>>;

export type ScopeType = Record<string, boolean>;

export type NodeRequiresSemicolon<Node extends BaseNode<any>> =
    Node extends IfStatement<any, any, any>
    ? false
    : Node extends FunctionDeclaration<any, any, any, any>
    ? false
    : true;

// src/Checker/Checker.ts
export type Check<NodeList extends Array<BaseNode<any>>> = InferBlockStatement<
    NodeList,
    GlobalTypeMembers
> extends TypeResult<any, any, infer Errors>
    ? Errors
    : never;

type InferBlockStatement<
    NodeList extends Array<BaseNode<any>>,
    State extends StateType,
    Result extends StaticType = NeverType,
    Errors extends Array<TypeError<any, any>> = [],
> = NodeList extends []
    ? MergeTypes<Result, VoidType> extends infer ReturnType
    ? ReturnType extends StaticType
    ? TypeResult<ReturnType, State, Errors>
    : never
    : never
    : NodeList[0] extends ExpressionStatement<infer Expression, any>
    ? InferExpression<Expression, State> extends TypeResult<
        any,
        infer ExpressionState,
        infer ExpressionErrors
    >
    ? InferBlockStatement<
        Tail<NodeList>,
        ExpressionState,
        Result,
        Concat<Errors, ExpressionErrors>
    >
    : never
    : NodeList[0] extends VariableDeclaration<
        [
            VariableDeclarator<
                Identifier<
                    infer Name,
                    infer Annotation,
                    NodeData<infer StartLine, any>
                >,
                infer Init,
                any
            >,
        ],
        infer Kind,
        any
    >
    ? InferVariableDeclaration<
        Name,
        Annotation,
        Init,
        Kind,
        State,
        StartLine
    > extends TypeResult<any, infer DeclarationState, infer DeclarationErrors>
    ? InferBlockStatement<
        Tail<NodeList>,
        DeclarationState,
        Result,
        Concat<Errors, DeclarationErrors>
    >
    : never
    : NodeList[0] extends FunctionDeclaration<
        Identifier<infer Name, any, any>,
        infer Params,
        BlockStatement<infer Body, any>,
        any
    >
    ? InferFunctionDeclaration<Name, Params, Body, State> extends TypeResult<
        any,
        infer DeclarationState,
        infer DeclarationErrors
    >
    ? InferBlockStatement<
        Tail<NodeList>,
        DeclarationState,
        Result,
        Concat<Errors, DeclarationErrors>
    >
    : never
    : NodeList[0] extends ReturnStatement<infer ReturnExpression, any>
    ? InferReturnStatement<ReturnExpression, State> extends TypeResult<
        infer ReturnValue,
        infer ReturnState,
        infer ReturnErrors
    >
    ? MergeTypes<Result, ReturnValue> extends infer ReturnType
    ? ReturnType extends StaticType
    ? TypeResult<ReturnType, ReturnState, Concat<Errors, ReturnErrors>>
    : never
    : never
    : never
    : NodeList[0] extends IfStatement<
        infer Test,
        BlockStatement<infer BlockBody, any>,
        any
    >
    ? InferExpression<Test, State> extends TypeResult<
        infer TestValue,
        infer TestState,
        infer TestErrors
    >
    ? InferBlockStatement<BlockBody, TestState> extends TypeResult<
        infer IfStatementValue,
        any,
        infer IfStatementErrors
    >
    ? InferBlockStatement<
        Tail<NodeList>,
        TestState,
        MergeTypes<Result, IfStatementValue> extends infer ReturnType
        ? ReturnType extends StaticType
        ? IfStatementValue extends VoidType
        ? Result
        : ReturnType
        : never
        : never,
        [...Errors, ...TestErrors, ...IfStatementErrors]
    >
    : never
    : never
    : InferBlockStatement<Tail<NodeList>, State, Result, Errors>;

type InferReturnStatement<
    ReturnExpression extends BaseNode<any> | null,
    State extends StateType,
> = ReturnExpression extends BaseNode<any>
    ? InferExpression<ReturnExpression, State> extends TypeResult<
        infer ExpressionValue,
        infer ExpressionState,
        infer ExpressionErrors
    >
    ? TypeResult<
        MapLiteralToType<ExpressionValue>,
        ExpressionState,
        ExpressionErrors
    >
    : never
    : TypeResult<UndefinedType, State>;

type InferFunctionParams<
    Params extends Array<BaseNode<any>>,
    FunctionParams extends Array<[string, StaticType]> = [],
    ParamsByName extends StateType = {},
    Errors extends Array<TypeError<any, any>> = [],
> = Params extends []
    ? [FunctionParams, ParamsByName, Errors]
    : Params[0] extends Identifier<
        infer Name,
        infer Annotation,
        NodeData<infer LineNumber, any>
    >
    ? Annotation extends TypeAnnotation<infer AnnotationValue, any>
    ? InferFunctionParamsHelper<
        Params,
        FunctionParams,
        ParamsByName,
        MapAnnotationToType<AnnotationValue>,
        Name,
        Errors
    >
    : InferFunctionParamsHelper<
        Params,
        FunctionParams,
        ParamsByName,
        AnyType,
        Name,
        Push<
            Errors,
            TypeError<
                `Parameter '${Name}' implicitly has an 'any' type.`,
                LineNumber
            >
        >
    >
    : never;

type InferFunctionParamsHelper<
    Params extends Array<BaseNode<any>>,
    FunctionParams extends Array<[string, StaticType]>,
    ParamsByName extends StateType,
    Type extends StaticType,
    Name extends string,
    Errors extends Array<TypeError<any, any>>,
> = InferFunctionParams<
    Tail<Params>,
    Push<FunctionParams, [Name, Type]>,
    ObjectMerge<ParamsByName, { [a in Name]: StateVariableType<Type, true> }>,
    Errors
>;

type InferFunctionDeclaration<
    Name extends string,
    Params extends Array<BaseNode<any>>,
    Body extends Array<BaseNode<any>>,
    State extends StateType,
> = InferFunctionParams<Params> extends [
    infer FunctionParams,
    infer ParamsByName,
    infer Errors,
]
    ? FunctionParams extends Array<[string, StaticType]>
    ? ParamsByName extends StateType
    ? Errors extends Array<TypeError<any, any>>
    ? InferBlockStatement<
        Body,
        ObjectMerge<State, ParamsByName>
    > extends TypeResult<
        infer BlockStatementReturnType,
        any,
        infer BlockStatementErrors
    >
    ? TypeResult<
        UndefinedType,
        ObjectMerge<
            State,
            {
                [a in Name]: StateVariableType<
                    FunctionType<FunctionParams, BlockStatementReturnType>,
                    false
                >;
            }
        >,
        Concat<Errors, BlockStatementErrors>
    >
    : never
    : never
    : never
    : never
    : never;

type MatchCallExpressionArguments<
    ParamsType extends Array<[string, StaticType]>,
    ArgumentsType extends Array<StaticType>,
    StartLine extends number,
> = ParamsType extends []
    ? true
    : MatchType<ParamsType[0][1], ArgumentsType[0]> extends true
    ? MatchCallExpressionArguments<
        Tail<ParamsType>,
        Tail<ArgumentsType>,
        StartLine
    >
    : TypeError<
        `Argument of type '${Serialize<
            ArgumentsType[0]
        >}' is not assignable to parameter of type '${Serialize<
            ParamsType[0][1]
        >}'.`,
        StartLine
    >;

type InferVariableDeclaration<
    Name extends string,
    Annotation extends BaseNode<any> | null,
    Init extends BaseNode<any>,
    Kind extends string,
    State extends StateType,
    StartLine extends number,
> = InferExpression<Init, State> extends TypeResult<
    infer InitExpressionValue,
    infer InitExpressionState,
    infer InitExpressionErrors
>
    ? Annotation extends TypeAnnotation<infer AnnotationValue, any>
    ? MapAnnotationToType<AnnotationValue> extends infer ExpectedType
    ? ExpectedType extends StaticType
    ? MatchType<ExpectedType, InitExpressionValue> extends true
    ? TypeResult<
        UndefinedType,
        ObjectMerge<
            InitExpressionState,
            {
                [a in Name]: StateVariableType<
                    ExpectedType,
                    IsKindMutable<Kind>
                >;
            }
        >,
        InitExpressionErrors
    >
    : TypeResult<
        UndefinedType,
        ObjectMerge<
            InitExpressionState,
            {
                [a in Name]: StateVariableType<
                    ExpectedType,
                    IsKindMutable<Kind>
                >;
            }
        >,
        Push<
            InitExpressionErrors,
            TypeError<
                `Type '${Serialize<InitExpressionValue>}' is not assignable to type '${Serialize<ExpectedType>}'.`,
                StartLine
            >
        >
    >
    : never
    : never
    : TypeResult<
        UndefinedType,
        ObjectMerge<
            State,
            Kind extends 'const'
            ? {
                [a in Name]: StateVariableType<InitExpressionValue, false>;
            }
            : {
                [a in Name]: StateVariableType<
                    MapLiteralToType<InitExpressionValue>,
                    true
                >;
            }
        >,
        InitExpressionErrors
    >
    : never;

type InferExpression<
    Node extends BaseNode<any>,
    State extends StateType,
> = Node extends StringLiteral<infer Value, any>
    ? TypeResult<StringLiteralType<Value>, State>
    : Node extends NumericLiteral<infer Value, any>
    ? TypeResult<NumberLiteralType<Value>, State>
    : Node extends NullLiteral<any>
    ? TypeResult<NullType, State>
    : Node extends BooleanLiteral<infer Value, any>
    ? TypeResult<BooleanLiteralType<Value>, State>
    : Node extends Identifier<infer Name, any, NodeData<infer StartLine, any>>
    ? Name extends keyof State
    ? TypeResult<State[Name]['value'], State>
    : TypeResult<
        AnyType,
        State,
        [TypeError<`Cannot find name '${Name}'.`, StartLine>]
    >
    : Node extends FunctionExpression<
        any,
        infer Params,
        BlockStatement<infer Body, any>,
        any
    >
    ? InferFunctionExpression<Params, Body, State>
    : Node extends ObjectExpression<infer Properties, any>
    ? InferObjectProperties<Properties, State>
    : Node extends MemberExpression<
        infer Object,
        infer Property,
        infer Computed,
        any
    >
    ? InferMemberExpression<Object, Property, Computed, State>
    : Node extends ArrayExpression<infer Elements, any>
    ? InferArrayElements<Elements, State>
    : Node extends CallExpression<
        infer Callee,
        infer Arguments,
        NodeData<infer StartLine, any>
    >
    ? InferCallExpression<Callee, Arguments, State, StartLine>
    : Node extends BinaryExpression<
        infer Left,
        infer Right,
        infer Operator,
        NodeData<infer LineNumber, any>
    >
    ? InferBinaryExpression<Left, Right, State, Operator, LineNumber>
    : Node extends AssignmentExpression<
        infer Left,
        infer Right,
        '=',
        NodeData<infer LineNumber, any>
    >
    ? InferAssignmentExpression<Left, Right, State, LineNumber>
    : UnknownType;

type InferFunctionExpression<
    Params extends Array<BaseNode<any>>,
    Body extends Array<BaseNode<any>>,
    State extends StateType,
> = InferFunctionParams<Params> extends [
    infer FunctionParams,
    infer ParamsByName,
    infer Errors,
]
    ? FunctionParams extends Array<[string, StaticType]>
    ? ParamsByName extends StateType
    ? Errors extends Array<TypeError<any, any>>
    ? InferBlockStatement<
        Body,
        ObjectMerge<State, ParamsByName>
    > extends TypeResult<
        infer BlockStatementReturnType,
        any,
        infer BlockStatementErrors
    >
    ? TypeResult<
        FunctionType<FunctionParams, BlockStatementReturnType>,
        State,
        Concat<Errors, BlockStatementErrors>
    >
    : never
    : never
    : never
    : never
    : never;

type InferBinaryExpression<
    Left extends BaseNode<any>,
    Right extends BaseNode<any>,
    State extends StateType,
    Operator extends string,
    LineNumber extends number,
> = InferExpression<Left, State> extends TypeResult<
    infer LeftValue,
    infer LeftState,
    infer LeftErrors
>
    ? InferExpression<Right, LeftState> extends TypeResult<
        infer RightValue,
        infer RightState,
        infer RightErrors
    >
    ? Operator extends '==' | '==='
    ? InferComparisonExpression<
        RightValue,
        LeftValue,
        RightState,
        Concat<LeftErrors, RightErrors>,
        LineNumber
    >
    : Operator extends '+' | '-' | '*' | '/'
    ? InferArithmeticExpression<
        RightValue,
        LeftValue,
        RightState,
        Concat<LeftErrors, RightErrors>,
        LineNumber
    >
    : never
    : never
    : never;

type InferComparisonExpression<
    RightValue extends StaticType,
    LeftValue extends StaticType,
    State extends StateType,
    Errors extends Array<TypeError<any, any>>,
    LineNumber extends number,
> = OverlapType<RightValue, LeftValue> extends false
    ? TypeResult<
        BooleanType,
        State,
        MismatchBinaryErrorHelper<LeftValue, RightValue, LineNumber, Errors>
    >
    : TypeResult<BooleanType, State, Errors>;

type InferArithmeticExpression<
    RightValue extends StaticType,
    LeftValue extends StaticType,
    State extends StateType,
    Errors extends Array<TypeError<any, any>>,
    LineNumber extends number,
> = MatchType<NumberType, RightValue> extends true
    ? MatchType<NumberType, LeftValue> extends true
    ? TypeResult<NumberType, State, Errors>
    : TypeResult<
        NumberType,
        State,
        Push<
            Errors,
            TypeError<
                "The left-hand side of an arithmetic operation must be of type 'any' or 'number'.",
                LineNumber
            >
        >
    >
    : TypeResult<
        NumberType,
        State,
        Push<
            Errors,
            TypeError<
                "The right-hand side of an arithmetic operation must be of type 'any' or 'number'.",
                LineNumber
            >
        >
    >;

type InferAssignmentExpression<
    Left extends BaseNode<any>,
    Right extends BaseNode<any>,
    State extends StateType,
    EqualsLineNumber extends number,
> = InferExpression<Left, State> extends TypeResult<
    infer LeftValue,
    infer LeftState,
    infer LeftErrors
>
    ? InferExpression<Right, LeftState> extends TypeResult<
        infer RightValue,
        infer RightState,
        infer RightErrors
    >
    ? Left extends Identifier<infer Name, any, any>
    ? State[Name]['mutable'] extends false
    ? TypeResult<
        RightValue,
        RightState,
        Push<
            Concat<LeftErrors, RightErrors>,
            TypeError<
                `Cannot assign to '${Name}' because it is a constant.`,
                EqualsLineNumber
            >
        >
    >
    : InferAssignmentExpressionHelper<
        LeftValue,
        RightValue,
        RightState,
        Concat<LeftErrors, RightErrors>,
        EqualsLineNumber
    >
    : InferAssignmentExpressionHelper<
        LeftValue,
        RightValue,
        RightState,
        Concat<LeftErrors, RightErrors>,
        EqualsLineNumber
    >
    : never
    : never;

type InferAssignmentExpressionHelper<
    LeftValue extends StaticType,
    RightValue extends StaticType,
    RightState extends StateType,
    Errors extends Array<TypeError<any, any>>,
    LineNumber extends number,
> = MatchType<LeftValue, RightValue> extends true
    ? TypeResult<RightValue, RightState, Errors>
    : TypeResult<
        RightValue,
        RightState,
        Push<
            Errors,
            TypeError<
                `Type '${Serialize<RightValue>}' is not assignable to type '${Serialize<LeftValue>}'.`,
                LineNumber
            >
        >
    >;

type InferCallExpression<
    Callee extends BaseNode<any>,
    Arguments extends Array<BaseNode<any>>,
    State extends StateType,
    StartLine extends number,
> = InferExpression<Callee, State> extends TypeResult<
    infer CalleeValue,
    infer CalleeState,
    infer CalleeErrors
>
    ? InferExpressionsArray<Arguments, CalleeState> extends TypeArrayResult<
        infer ArgumentsType,
        infer ArgumentsState,
        infer ArgumentsErrors
    >
    ? InferCallExpressionHelper<
        CalleeValue,
        ArgumentsType,
        ArgumentsState,
        Concat<CalleeErrors, ArgumentsErrors>,
        StartLine
    >
    : never
    : never;

type InferCallExpressionHelper<
    CalleeValue extends StaticType,
    ArgumentsType extends Array<StaticType>,
    State extends StateType,
    Errors extends Array<TypeError<any, any>>,
    StartLine extends number,
> = CalleeValue extends FunctionType<infer ParamsType, infer ReturnType>
    ? ParamsType['length'] extends ArgumentsType['length']
    ? MatchCallExpressionArguments<
        ParamsType,
        ArgumentsType,
        StartLine
    > extends TypeError<infer Message, infer StartLine>
    ? TypeResult<
        ReturnType,
        State,
        Push<Errors, TypeError<Message, StartLine>>
    >
    : TypeResult<ReturnType, State, Errors>
    : TypeResult<
        ReturnType,
        State,
        Push<
            Errors,
            TypeError<
                `Expected ${ParamsType['length']} arguments, but got ${ArgumentsType['length']}.`,
                StartLine
            >
        >
    >
    : CalleeValue extends AnyType
    ? TypeResult<AnyType, State, Errors>
    : CalleeValue extends UnionType<infer UnionTypes>
    ? InferCallExpressionUnionHelper<
        CalleeValue,
        UnionTypes,
        ArgumentsType,
        State,
        StartLine,
        Errors
    >
    : TypeResult<
        AnyType,
        State,
        Unshift<
            Errors,
            TypeError<
                `This expression is not callable. Type '${Serialize<CalleeValue>}' has no call signatures.`,
                StartLine
            >
        >
    >;

type InferCallExpressionUnionHelper<
    CalleeValue extends UnionType<any>,
    UnionTypes extends Array<StaticType>,
    ArgumentsType extends Array<StaticType>,
    State extends StateType,
    StartLine extends number,
    Errors extends Array<TypeError<any, any>>,
> = UnionTypes extends Array<FunctionType<any, any>>
    ? InferCallExpressionHelper<
        MergeFunctionTypesArray<Tail<UnionTypes>, UnionTypes[0]>,
        ArgumentsType,
        State,
        Errors,
        StartLine
    >
    : TypeResult<
        AnyType,
        State,
        Push<
            Errors,
            TypeError<
                `This expression is not callable. Not all constituents of type '${Serialize<CalleeValue>}' are callable.`,
                StartLine
            >
        >
    >;

type InferExpressionsArray<
    NodeList extends Array<BaseNode<any>>,
    State extends StateType,
    Result extends Array<StaticType> = [],
    Errors extends Array<TypeError<any, any>> = [],
> = NodeList extends []
    ? TypeArrayResult<Result, State, Errors>
    : InferExpression<NodeList[0], State> extends TypeResult<
        infer ExpressionValue,
        infer ExpressionState,
        infer ExpressionErrors
    >
    ? InferExpressionsArray<
        Tail<NodeList>,
        ObjectMerge<State, ExpressionState>,
        Push<Result, ExpressionValue>,
        Concat<Errors, ExpressionErrors>
    >
    : never;

type InferArrayElements<
    Elements extends Array<BaseNode<any>>,
    State extends StateType,
    First extends boolean = true,
    Result extends StaticType = AnyType,
    Errors extends Array<TypeError<any, any>> = [],
> = Elements extends []
    ? TypeResult<ArrayType<Result>, State, Errors>
    : Elements[0] extends BaseNode<any>
    ? InferExpression<Elements[0], State> extends TypeResult<
        infer ExpressionValue,
        infer ExpressionState,
        infer ExpressionErrors
    >
    ? MapLiteralToType<ExpressionValue> extends infer LiteralType
    ? LiteralType extends StaticType
    ? MergeTypes<Result, LiteralType> extends infer ReturnType
    ? ReturnType extends StaticType
    ? InferArrayElements<
        Tail<Elements>,
        ExpressionState,
        false,
        First extends true ? LiteralType : ReturnType,
        Concat<Errors, ExpressionErrors>
    >
    : never
    : never
    : never
    : never
    : never
    : never;

type InferMemberExpression<
    Object extends BaseNode<any>,
    Property extends BaseNode<any>,
    Computed extends boolean,
    State extends StateType,
> = InferExpression<Object, State> extends TypeResult<
    infer ObjectExpressionValue,
    infer ObjectExpressionState,
    infer ObjectExpressionErrors
>
    ? Computed extends false
    ? Property extends Identifier<
        infer Name,
        any,
        NodeData<infer StartLine, any>
    >
    ? InferMemberExpressionHelper<
        ObjectExpressionValue,
        Name,
        ObjectExpressionState,
        StartLine,
        ObjectExpressionErrors
    >
    : never
    : InferExpression<Property, ObjectExpressionState> extends TypeResult<
        infer PropertyExpressionValue,
        infer PropertyExpressionState,
        infer PropertyExpressionErrors
    >
    ? Property extends BaseNode<NodeData<infer StartLine, any>>
    ? PropertyExpressionValue extends
    | StringLiteralType<infer Value>
    | NumberLiteralType<infer Value>
    ? InferMemberExpressionHelper<
        ObjectExpressionValue,
        Value,
        PropertyExpressionState,
        StartLine,
        Concat<ObjectExpressionErrors, PropertyExpressionErrors>
    >
    : PropertyExpressionValue extends AnyType
    ? TypeResult<
        AnyType,
        PropertyExpressionState,
        Concat<ObjectExpressionErrors, PropertyExpressionErrors>
    >
    : TypeResult<
        AnyType,
        PropertyExpressionState,
        Push<
            Concat<ObjectExpressionErrors, PropertyExpressionErrors>,
            TypeError<
                `Type '${Serialize<PropertyExpressionValue>}' cannot be used as an index type.`,
                StartLine
            >
        >
    >
    : never
    : never
    : never;

type InferMemberExpressionHelper<
    Object extends StaticType,
    Key extends string,
    State extends StateType,
    StartLine extends number,
    Errors extends Array<TypeError<any, any>>,
> = Object extends ObjectType<infer ObjectProperties>
    ? GetObjectValueByKey<
        ObjectProperties,
        Key
    > extends infer MemberExpressionValue
    ? MemberExpressionValue extends StaticType
    ? TypeResult<MemberExpressionValue, State, Errors>
    : PropertyDoesNotExistResult<
        State,
        Errors,
        Key,
        Object,
        StartLine,
        UndefinedType
    >
    : never
    : Object extends ArrayType<infer ElementsType>
    ? IsNumeric<Key> extends true
    ? TypeResult<ElementsType, State, Errors>
    : Key extends keyof ArrayTypeMembers<ElementsType>
    ? TypeResult<ArrayTypeMembers<ElementsType>[Key], State, Errors>
    : PropertyDoesNotExistResult<State, Errors, Key, Object, StartLine>
    : Object extends StringType | StringLiteralType<any>
    ? Key extends keyof StringTypeMembers
    ? TypeResult<StringTypeMembers[Key], State, Errors>
    : PropertyDoesNotExistResult<State, Errors, Key, Object, StartLine>
    : Object extends FunctionType<any, any>
    ? Key extends keyof FunctionTypeMembers
    ? TypeResult<FunctionTypeMembers[Key], State, Errors>
    : PropertyDoesNotExistResult<State, Errors, Key, Object, StartLine>
    : Object extends UnionType<infer UnionTypes>
    ? InferMemberExpressionUnionHelper<UnionTypes, Key, State, StartLine, Errors>
    : Object extends AnyType
    ? TypeResult<AnyType, State, Errors>
    : PropertyDoesNotExistResult<State, Errors, Key, Object, StartLine>;

type InferMemberExpressionUnionHelper<
    UnionTypes extends Array<StaticType>,
    Key extends string,
    State extends StateType,
    StartLine extends number,
    Errors extends Array<TypeError<any, any>>,
    Result extends StaticType = NeverType,
> = UnionTypes extends []
    ? TypeResult<Result, State, Errors>
    : InferMemberExpressionHelper<
        UnionTypes[0],
        Key,
        State,
        StartLine,
        []
    > extends TypeResult<
        infer ExpressionValue,
        infer ExpressionState,
        infer ExpressionErrors
    >
    ? MergeTypes<Result, ExpressionValue> extends infer ReturnType
    ? ReturnType extends StaticType
    ? InferMemberExpressionUnionHelper<
        Tail<UnionTypes>,
        Key,
        ExpressionState,
        StartLine,
        Errors,
        ReturnType
    >
    : never
    : never
    : never;

type InferObjectProperties<
    Properties extends Array<ObjectProperty<any, any, any>>,
    State extends StateType,
    Result extends Array<any> = [],
    Errors extends Array<TypeError<any, any>> = [],
> = Properties extends []
    ? TypeResult<ObjectType<Result>, State, Errors>
    : Properties[0] extends ObjectProperty<
        Identifier<infer Name, any, any>,
        infer Value,
        any
    >
    ? InferExpression<Value, State> extends TypeResult<
        infer ExpressionValue,
        infer ExpressionState,
        infer ExpressionErrors
    >
    ? InferObjectProperties<
        Tail<Properties>,
        ExpressionState,
        Push<Result, [Name, MapLiteralToType<ExpressionValue>]>,
        Concat<Errors, ExpressionErrors>
    >
    : never
    : never;

// src/Checker/MatchType.ts
export type OverlapType<
    TypeA extends StaticType,
    TypeB extends StaticType,
> = TypeA extends ArrayType<infer ArrayTypeA>
    ? TypeB extends ArrayType<infer ArrayTypeB>
    ? OverlapType<ArrayTypeA, ArrayTypeB>
    : false
    : TypeA extends ObjectType<infer PropertiesA>
    ? TypeB extends ObjectType<infer PropertiesB>
    ? OverlapObjectProperties<PropertiesA, PropertiesB>
    : false
    : TypeA extends UnionType<infer UnionTypesA>
    ? TypeB extends UnionType<infer UnionTypesB>
    ? UnionsOverlap<UnionTypesA, UnionTypesB>
    : TypeMatchUnion<UnionTypesA, TypeB>
    : TypeB extends UnionType<infer UnionTypesB>
    ? TypeMatchUnion<UnionTypesB, TypeA>
    : MatchPrimitive<TypeA, TypeB> extends true
    ? true
    : MatchPrimitive<TypeB, TypeA> extends true
    ? true
    : false;

export type MatchType<
    TypeA extends StaticType,
    TypeB extends StaticType,
> = TypeA extends ArrayType<infer ArrayTypeA>
    ? TypeB extends ArrayType<infer ArrayTypeB>
    ? MatchType<ArrayTypeA, ArrayTypeB>
    : false
    : TypeA extends ObjectType<infer PropertiesA>
    ? TypeB extends ObjectType<infer PropertiesB>
    ? MatchObjectProperties<PropertiesA, PropertiesB>
    : false
    : TypeA extends FunctionType<infer ParamsA, infer ReturnA>
    ? TypeB extends FunctionType<infer ParamsB, infer ReturnB>
    ? MatchFunction<ParamsA, ParamsB, ReturnA, ReturnB>
    : false
    : TypeA extends UnionType<infer UnionTypesA>
    ? TypeB extends UnionType<infer UnionTypesB>
    ? UnionMatchUnion<UnionTypesA, UnionTypesB>
    : TypeMatchUnion<UnionTypesA, TypeB>
    : TypeB extends UnionType<infer UnionTypesB>
    ? UnionMatchType<TypeA, UnionTypesB>
    : MatchPrimitive<TypeA, TypeB> extends true
    ? true
    : false;

type MatchFunction<
    ParamsA extends Array<[string, StaticType]>,
    ParamsB extends Array<[string, StaticType]>,
    ReturnA extends StaticType,
    ReturnB extends StaticType,
> = ParamsA extends []
    ? ParamsB extends []
    ? MatchType<ReturnA, ReturnB>
    : false
    : ParamsB extends []
    ? false
    : MatchType<ParamsB[0][1], ParamsA[0][1]> extends true
    ? MatchFunction<Tail<ParamsA>, Tail<ParamsB>, ReturnA, ReturnB>
    : false;

type MatchObjectProperties<
    PropertiesA extends Array<[string, StaticType]>,
    PropertiesB extends Array<[string, StaticType]>,
    Length extends number = PropertiesA['length'],
> = PropertiesA extends []
    ? PropertiesB['length'] extends Length
    ? true
    : false
    : PropertiesB extends []
    ? false
    : MatchType<
        GetObjectValueByKey<PropertiesA, PropertiesA[0][0]>,
        GetObjectValueByKey<PropertiesB, PropertiesA[0][0]>
    > extends true
    ? MatchObjectProperties<Tail<PropertiesA>, PropertiesB, Length>
    : false;

type OverlapObjectProperties<
    PropertiesA extends Array<[string, StaticType]>,
    PropertiesB extends Array<[string, StaticType]>,
    Length extends number = PropertiesA['length'],
> = PropertiesA extends []
    ? PropertiesB['length'] extends Length
    ? true
    : false
    : PropertiesB extends []
    ? false
    : OverlapType<
        GetObjectValueByKey<PropertiesA, PropertiesA[0][0]>,
        GetObjectValueByKey<PropertiesB, PropertiesA[0][0]>
    > extends true
    ? OverlapObjectProperties<Tail<PropertiesA>, PropertiesB, Length>
    : false;

type MatchPrimitive<
    TypeA extends StaticType,
    TypeB extends StaticType,
> = TypeA extends NeverType
    ? false
    : TypeB extends NeverType
    ? false
    : TypeA extends AnyType
    ? true
    : TypeB extends AnyType
    ? true
    : TypeA extends TypeB
    ? TypeB extends TypeA
    ? true
    : false
    : TypeA extends StringType
    ? TypeB extends StringLiteralType<any>
    ? true
    : false
    : TypeA extends BooleanType
    ? TypeB extends BooleanLiteralType<any>
    ? true
    : false
    : TypeA extends NumberType
    ? TypeB extends NumberLiteralType<any>
    ? true
    : false
    : false;

type UnionsOverlap<
    UnionTypesA extends Array<StaticType>,
    UnionTypesB extends Array<StaticType>,
> = UnionTypesB extends []
    ? false
    : TypeMatchUnion<UnionTypesA, UnionTypesB[0]> extends false
    ? UnionMatchUnion<UnionTypesA, Tail<UnionTypesB>>
    : true;

type UnionMatchUnion<
    UnionTypesA extends Array<StaticType>,
    UnionTypesB extends Array<StaticType>,
> = UnionTypesB extends []
    ? true
    : TypeMatchUnion<UnionTypesA, UnionTypesB[0]> extends true
    ? UnionMatchUnion<UnionTypesA, Tail<UnionTypesB>>
    : false;

type TypeMatchUnion<
    UnionTypes extends Array<StaticType>,
    Type extends StaticType,
> = UnionTypes extends []
    ? false
    : MatchType<UnionTypes[0], Type> extends true
    ? true
    : TypeMatchUnion<Tail<UnionTypes>, Type>;

type UnionMatchType<
    Type extends StaticType,
    UnionTypes extends Array<StaticType>,
> = UnionTypes extends []
    ? true
    : MatchType<Type, UnionTypes[0]> extends true
    ? UnionMatchType<Type, Tail<UnionTypes>>
    : false;

// src/Checker/TypeDefinitions.ts
export type GlobalTypeMembers = {
    console: StateVariableType<
        ObjectType<[['log', FunctionType<[['data', AnyType]], UndefinedType>]]>,
        true
    >;
    eval: StateVariableType<FunctionType<[['x', StringType]], AnyType>, false>;
    setTimeout: StateVariableType<
        FunctionType<
            [['handler', FunctionType<[], VoidType>], ['timeout', NumberType]],
            NumberType
        >,
        false
    >;
};

export type StringTypeMembers = {
    length: NumberType;
    includes: FunctionType<[['searchString', StringType]], BooleanType>;
    charAt: FunctionType<[['pos', NumberType]], StringType>;
    indexOf: FunctionType<[['searchString', StringType]], NumberType>;
    startsWith: FunctionType<[['searchString', StringType]], BooleanType>;
    endsWith: FunctionType<[['searchString', StringType]], BooleanType>;
    split: FunctionType<[['separator', StringType]], ArrayType<StringType>>;
    replace: FunctionType<
        [['searchValue', StringType], ['replaceValue', StringType]],
        StringType
    >;
};

export type ArrayTypeMembers<ElementsType extends StaticType> = {
    length: NumberType;
    fill: FunctionType<[['value', ElementsType]], ArrayType<ElementsType>>;
    reverse: FunctionType<[], ArrayType<ElementsType>>;
    includes: FunctionType<[['searchElement', ElementsType]], BooleanType>;
    join: FunctionType<[['separator', StringType]], StringType>;
    indexOf: FunctionType<[['searchElement', ElementsType]], NumberType>;
    push: FunctionType<[['item', ElementsType]], NumberType>;
    pop: MergeTypes<ElementsType, UndefinedType> extends infer ReturnType
    ? ReturnType extends StaticType
    ? FunctionType<[], ReturnType>
    : never
    : never;
    unshift: FunctionType<[['item', ElementsType]], NumberType>;
};

export type FunctionTypeMembers = {
    length: NumberType;
    name: StringType;
    prototype: AnyType;
    toString: FunctionType<[], StringType>;
};

// src/Checker/Types.ts
export type StringType = {
    type: 'StringType';
};

export type StringLiteralType<Value extends string> = {
    type: 'StringLiteralType';
    value: Value;
};

export type NumberType = {
    type: 'NumberType';
};

export type NumberLiteralType<Value extends string> = {
    type: 'NumberLiteralType';
    value: Value;
};

export type BooleanType = {
    type: 'BooleanType';
};

export type BooleanLiteralType<Value extends boolean> = {
    type: 'BooleanLiteralType';
    value: Value;
};

export type NullType = {
    type: 'NullType';
};

export type UndefinedType = {
    type: 'UndefinedType';
};

export type UnknownType = {
    type: 'UnknownType';
};

export type VoidType = {
    type: 'VoidType';
};

export type AnyType = {
    type: 'AnyType';
};

export type NeverType = {
    type: 'NeverType';
};

export type FunctionType<
    Params extends Array<[string, StaticType]>,
    Return extends StaticType,
> = {
    type: 'FunctionType';
    params: Params;
    return: Return;
};

export type ObjectType<Properties extends Array<[string, StaticType]>> = {
    type: 'ObjectType';
    properties: Properties;
};

export type ArrayType<ElementsType extends StaticType> = {
    type: 'ArrayType';
    elements: ElementsType;
};

export type UnionType<Types extends Array<StaticType>> = {
    type: 'UnionType';
    types: Types;
};

export type StaticType =
    | StringType
    | StringLiteralType<any>
    | NumberType
    | NumberLiteralType<any>
    | BooleanType
    | BooleanLiteralType<any>
    | UnknownType
    | VoidType
    | AnyType
    | NullType
    | UndefinedType
    | NeverType
    | FunctionType<any, any>
    | ObjectType<any>
    | ArrayType<any>
    | UnionType<any>;

// src/Checker/Utils.ts
export type StateVariableType<
    Value extends StaticType,
    Mutable extends boolean,
> = {
    type: 'ConstVariableType';
    value: Value;
    mutable: Mutable;
};

export type StateType = Record<string, StateVariableType<StaticType, boolean>>;

export type TypeResult<
    Value extends StaticType,
    State extends StateType,
    Errors extends Array<TypeError<any, any>> = [],
> = {
    type: 'TypeResult';
    value: Value;
    state: State;
    errors: Errors;
};

export type TypeArrayResult<
    TypeList extends Array<StaticType>,
    State extends StateType,
    Errors extends Array<TypeError<any, any>> = [],
> = {
    type: 'TypeResult';
    value: TypeList;
    state: State;
    errors: Errors;
};

export type MapLiteralToType<Type extends StaticType> =
    Type extends NumberLiteralType<any>
    ? NumberType
    : Type extends StringLiteralType<any>
    ? StringType
    : Type extends BooleanLiteralType<any>
    ? BooleanType
    : Type;

export type GetObjectValueByKey<
    ObjectProperties extends Array<[string, StaticType]>,
    Key extends string,
> = ObjectProperties extends []
    ? null
    : ObjectProperties[0] extends [infer PropertyName, infer PropertyValue]
    ? PropertyName extends Key
    ? PropertyValue
    : GetObjectValueByKey<Tail<ObjectProperties>, Key>
    : never;

export type MapAnnotationToType<AnnotationValue extends BaseNode<any>> =
    AnnotationValue extends StringTypeAnnotation<any>
    ? StringType
    : AnnotationValue extends NumberTypeAnnotation<any>
    ? NumberType
    : AnnotationValue extends BooleanTypeAnnotation<any>
    ? BooleanType
    : AnnotationValue extends NullLiteralTypeAnnotation<any>
    ? NullType
    : AnnotationValue extends AnyTypeAnnotation<any>
    ? AnyType
    : UnknownType;

export type MergeTypes<
    TypeA extends StaticType,
    TypeB extends StaticType,
> = TypeA extends NeverType
    ? TypeB
    : TypeB extends NeverType
    ? TypeA
    : TypeA extends AnyType
    ? AnyType
    : TypeB extends AnyType
    ? AnyType
    : MatchType<TypeA, TypeB> extends true
    ? TypeA
    : MatchType<TypeB, TypeA> extends true
    ? TypeB
    : TypeA extends UnionType<infer UnionTypesA>
    ? TypeB extends UnionType<infer UnionTypesB>
    ? UnionType<Concat<UnionTypesA, UnionTypesB>>
    : UnionType<Push<UnionTypesA, TypeB>>
    : TypeB extends UnionType<infer UnionTypesB>
    ? UnionType<Push<UnionTypesB, TypeA>>
    : UnionType<[TypeA, TypeB]>;

export type IsKindMutable<Kind extends string> = Kind extends 'const'
    ? false
    : true;

export type MergeFunctionTypesArray<
    FunctionTypes extends Array<FunctionType<any, any>>,
    ReturnType extends FunctionType<any, any>,
> = FunctionTypes extends []
    ? ReturnType
    : FunctionTypes[0] extends FunctionType<infer Params, infer Return>
    ? MergeFunctionTypesArray<
        Tail<FunctionTypes>,
        MergeFunctionTypes<Params, Return, ReturnType>
    >
    : never;

type MergeFunctionTypes<
    Params extends Array<[string, StaticType]>,
    Return extends StaticType,
    Function extends FunctionType<any, any>,
> = Function extends FunctionType<infer OtherParams, infer OtherReturn>
    ? MergeFunctionParams<Params, OtherParams> extends infer P
    ? P extends Array<[string, StaticType]>
    ? MergeTypes<Return, OtherReturn> extends infer ReturnType
    ? ReturnType extends StaticType
    ? FunctionType<P, ReturnType>
    : never
    : never
    : never
    : never
    : never;

type MergeFunctionParams<
    ParamsA extends Array<[string, StaticType]>,
    ParamsB extends Array<[string, StaticType]>,
    Return extends Array<[string, StaticType]> = [],
> = ParamsA extends []
    ? ParamsB extends []
    ? Return
    : Concat<Return, ParamsB>
    : ParamsB extends []
    ? Concat<Return, ParamsA>
    : MatchType<ParamsA[0][1], ParamsB[0][1]> extends true
    ? MergeFunctionParams<Tail<ParamsA>, Tail<ParamsB>, Push<Return, ParamsB[0]>>
    : MatchType<ParamsB[0][1], ParamsA[0][1]> extends true
    ? MergeFunctionParams<Tail<ParamsA>, Tail<ParamsB>, Push<Return, ParamsA[0]>>
    : MergeFunctionParams<
        Tail<ParamsA>,
        Tail<ParamsB>,
        Push<Return, [ParamsA[0][0], NeverType]>
    >;

export type MismatchBinaryErrorHelper<
    Left extends StaticType,
    Right extends StaticType,
    LineNumber extends number,
    Errors extends Array<TypeError<any, any>>,
    ShouldMapLiterals extends boolean = IsSameLiteralType<Left, Right>,
> = Push<
    Errors,
    TypeError<
        `This condition will always return 'false' since the types '${Serialize<
            Left,
            ShouldMapLiterals
        >}' and '${Serialize<Right, ShouldMapLiterals>}' have no overlap.`,
        LineNumber
    >
>;

type IsSameLiteralType<LeftValue, RightValue> =
    RightValue extends StringLiteralType<any>
    ? LeftValue extends StringLiteralType<any>
    ? true
    : false
    : RightValue extends NumberLiteralType<any>
    ? LeftValue extends NumberLiteralType<any>
    ? true
    : false
    : RightValue extends BooleanLiteralType<any>
    ? LeftValue extends BooleanLiteralType<any>
    ? true
    : false
    : false;

export type PropertyDoesNotExistResult<
    State extends StateType,
    Errors extends Array<TypeError<any, any>>,
    Key extends string,
    Object extends StaticType,
    LineNumber extends number,
    Value extends StaticType = AnyType,
> = TypeResult<
    Value,
    State,
    Push<
        Errors,
        TypeError<
            `Property '${Key}' does not exist on type '${Serialize<Object>}'.`,
            LineNumber
        >
    >
>;

export type IsNumeric<Input extends string> = Input extends ''
    ? true
    : Input extends `${Numbers}${infer Rest}`
    ? IsNumeric<Rest>
    : false;


const a: number = {} as Errors1