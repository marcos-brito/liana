# Notation

The grammar is represented using EBNF with a few minor extensions.

## Name conventions

- `_kind`: A set of possible terminals that distinguish a rule (e.g. `assignment_kind`, `attribute_kind`)
- `_parent`: An item of a greater category (e.g. `loop_expression`, `iterator_loop`)
- `_list`: A sequence of a minor rule (e.g. `parameter_list`, `expression_list`)
- `_clause`: Optional component starting with a keyword (e.g. `where_clause`, `of_clause`)

If none of the above apply a standalone name may be created.

# Lexical elements

## Code representation

```ebnf
unicode_letter = // Letter category from Unicode
unicode_char = // Any Unicode char except newline
unicode_digit = // Number category from Unicode
newline = // Categories Zl and Zp from Unicode
letter = "_" | unicode_letter ;
decimal_digit = "0" .. "9" ;
binary_digit = "0" | "1" ;
octal_digit = "0" | "7" ;
hex_digit = "0" .. "9" | "a" .. "f" ;
```

## Keywords

- `val`
- `var`
- `data`
- `fun`
- `return`
- `signature`
- `instance`
- `of`
- `when`
- `loop`
- `as`
- `if`
- `else`
- `false`
- `true`
- `this`
- `imp`
- `exp`
- `muda`

## Punctuation

| Symbol | Name      | Usage |
| ------ | --------- | ----- |
| @      | At        | todo  |
| !      | Bang      | todo  |
| ?      | Question  | todo  |
| .      | Dot       | todo  |
| ,      | Comma     | todo  |
| :      | Colon     | todo  |
| ;      | SemiColon | todo  |
| +      | Plus      | todo  |
| -      | Minus     | todo  |
| \*     | Star      | todo  |
| /      | Slash     | todo  |
| %      | Percent   | todo  |
| &&     | AndAnd    | todo  |
| \|\|   | OrOr      | todo  |
| >      | Gt        | todo  |
| >=     | Ge        | todo  |
| <      | Lt        | todo  |
| <=     | Le        | todo  |
| =      | Eq        | todo  |
| ==     | EqEq      | todo  |
| !=     | Ne        | todo  |
| ->     | Arrow     | todo  |

## Delimiters

| Symbol | Name         | Usage |
| ------ | ------------ | ----- |
| (      | OpenParen    | todo  |
| )      | CloseParen   | todo  |
| {      | OpenBrace    | todo  |
| }      | CloseBrace   | todo  |
| [      | OpenBracket  | todo  |
| ]      | CloseBracket | todo  |

## Identifier

```ebnf
identifier = letter, { letter | decimal_digit } ;
qualified_identifier = identifier, { ".", identifier } ;
```

```
foo
_bar
foo2._bar_2
```

## Literals

```ebnf
literal
    = integer_literal
    | float_literal
    | boolean_literal
    | char_literal
    | string_literal ;
```

### Integer literals

```ebnf
integer_literal
    = decimal_literal
    | binary_literal
    | octal_literal
    | hex_literal

decimal_literal = decimal_digit , { [ "_" ], decimal_digit } ;
binary_literal = "0", "b", [ "_" ],  binary_digit , { [ "_" ], binary_digit } ;
octal_literal = "0", "o", [ "_" ],  octal_digit , { [ "_" ], octal_digit } ;
hex_literal = "0", "x", [ "_" ],  hex_digit , { [ "_" ], hex_digit } ;
```

```
42
0_42
0b0110
0x_6e_9a_f1
0o3_8_4
```

### Float literals

```ebnf
float_literal
    = fractional_float_literal
    | exponent_float_literal
    | dot_float_literal ;

fractional_float_literal = decimal_literal, "." decimal_literal, [ exponent ] ;
exponent_float_literal = decimal_literal, exponent ;
dot_float_literal = "." decimal_literal, [ exponent ] ;

exponent = "e", [ "+" | "-" ], decimal_literal ;
```

```
31.43
37.12e+32
12.84e-11
11e-32
.45
.45e+83
.72e-83
```

### Boolean literals

```ebnf
boolean_literal = "true" | "false" ;
```

```

#### Char literals

```ebnf
char_literal = "'", unicode_char  "'" ;
```

#### String literals

```ebnf
string_literal = "\"", unicode_char  "\"" ;
```

#### Raw string literals

```ebnf
char_literal = "'", unicode_char  "'" ;
```

## Comments

```ebnf
comment = comment_kind, { unicode_char }, newline ;
comment_kind = "//!" | "///" | "//" ;
```

```
//! This is documentation for the module
/// This is documentation for bar
// This is ignored
fun bar(): i32 -> 4;
```

# Module

```
module = { attribute }, { declaration } ;
```

# Declarations

```
declaration
    = { attribute }
    , [ "exp" ]
    , import_declaration
    | export_declaration
    | data_declaration
    | function_declaration
    | signature_declaration
    | instance_declaration
```

## Functions

```ebnf
function_declaration =
    "fun",
    identifier,
    "(", [ parameter_list ], ")",
    [ ":", type ],
    [ where_clause ]
    ( compound_expression | ";" ) ;

parameter_list = parameter, { ",", parameter } ;
parameter = [ assignment_kind ], identifier, ":", type ;
```

```
fun build(ptr: i32): i32;

fun amount_of_money_in_my_bank_account(): i8 {
    return 98;
}

fun amount_of_money_in_my_bank_account(a: i32, b: i32): i32 -> a + b;
```

## Import

```ebnf
import_declaration = "imp", import, ";" ;
import
    = regular_import
    | renamed_import
    | wildcard_import
    | block_import ;

import_list = import, { ",", import } ;
regular_import = qualified_identifier ;
renamed_import = qualified_identifier, "as", identifier ;
wildcard_import = qualified_identifier, ".*" ;
block_import = qualified_identifier, "{", import_list, "}" ;
```

```
imp io.*;
imp io as Io;
imp io {this, Stdout as out, Reader};
```

## Generics

```
where_clause = "where", "[", { bound_list }, "]" ;
bound_list = bound, { ",", bound } ;
bound = identifier, ":", type, { "+", type } ;
```

```
fun joe(doe: [Doe]): [Doe];

data Foo {
    bar: [Bar]
}
```

## Data

```ebnf
data_declaration
    = "data"
    , identifier
    , [ where_clause ]
    | alias_data
    | rename_data
    | record_data
    | variant_data

alias_data = "=", type, ";" ;
rename_data = of_clause, ";" ;
record_data = "{", field_list, "}" ;
variant_data = "{", variant_list, "}" ;

field_list = field, { ",", field } ;
variant_list = variant, { ",", variant } ;
variant = identifier, [ of_clause | "{", field_list, "}" ] ;
field = identifier, ":", type ;
of_clause = "of", type
```

```
// Alias
data Int = I32;

// Rename
data Foo of I32;

// Record
data Some {
    bar: i32,
}

// Variant
data Leaf {
    Some of Some,
    Foo { bar: String },
    Bar,
}
```

## Signature

```ebnf
signature_declaration = "signature", identifier, [ where_clause ], "{", { function_declaration }, "}" ;
```

```
signature Foo {
    fun bar(_: i32): i32;
}
```

## Instance

```ebnf
instance_declaration =
    "instance",
    type,
    [ of_clause ],
    [ where_clause ],
    "{", { function_declaration }, "}" ;
```

```
instance Foo {
    fun num() -> 5;
}
```

```
instance Foo of Bar {
    fun bar(a: i32): i32 -> a + 5;
}
```

# Statements

```
statement = ( assignment_statement | expression ), ";" ;
```

## Assignment

```ebnf
assignment_statement = assignment_kind, identifier, [ ":", type ], "=", expression ;
assignment_kind = "val" | "var" ;
```

```
var num: i32 = 42;
val c: char = 'h';
```

# Expressions

```ebnf
expression
    = unary_expression
    | binary_expression
    | group_expression
    | call_expression
    | field_expression
    | if_expression
    | when_expression
    | loop_expression
    | closure_expression
    | compound_expression
    | macro_expression
    | return_expression
    | assignment_expression
    | identifier_expression
    | literal_expression ;

literal_expression = literal ;
identifier_expression = qualified_identifier ;
compound_expression = block_expression | arrow_expression ;
block_expression = "{", { expression }, "}" ;
arrow_expression = "->", expression ;

assignment_expression = identifier, "=", expression ;
group_expression = "(", expression, ")" ;
call_expression = expression, "(", [ expression_list ], ")" ;
field_expression = expression, ".", identifier ;
return_expression = "return", expression ;
macro_expression =  "#", qualified_identifier, "(", [ expression_list ], ")" ;

expression_list = raw_expression_list | labeled_expression_list ;
raw_expression_list = expression, { ",", expression } ;
labeled_expression_list = labeled_expression, { ",", expression } ;
labeled_expression = identifier, ":", expression ;
```

## Unary

```ebnf
unary_expression = unary_operator, expression ;
unary_operator = "!" | "-" | "+" ;
```

## Binary

```ebnf
binary_expression = expression, binary_operator, expression ;
binary_operator
    = "+" | "-"
    | "*" | "/"
    | "&&" | "||"
    | ">" | ">="
    | "<" | "<="
    | "==" | "!="
    | "|>" | "%" ;
```

## Closure

```ebnf
closure_expression
    = "fun",
    "(", [ parameter_list ], ")",
    [ ":", type ],
    compound_expression ;
```

```
val fn = fun(a, b) -> c;
val fn = fun() -> c;
val fn = fun(a, b) { c };
```

## If

```ebnf
if_expression = "if", expression, compound_expression, [ else_clause ] ;
else_clause = "else", ( compound_expression | if_expression ) ;
```

```
val a = if cond -> "a" else -> "b";

val a = if cond -> {

}
```

## Loop

```ebnf
loop_expression = "loop", (iterator_loop | predicate_loop | unbounded_loop ) ;
iterator_loop = expression, "as", identifier, compound_expression ;
predicate_loop = expression, compound_expression ;
unbounded_loop = compound_expression ;
```

`loop` is used to specify repeated execution. It

```
loop items as item {}
```

```
loop true {}
```

```
loop {}
loop -> io.print()
```

## When

```ebfn
when_expression = "when", expression, "{", arm_list, "}" ;
arm_list = arm, { ",", arm } ;
arm = pattern_list, arrow_expression ;
```

```
when c {
    'A' -> 'B'
    'A' -> 'B'
    'A' -> 'B'
    _ -> '?'
}
```

### Pattern

```ebnf
pattern
    = literal_pattern
    | underscore_pattern ;

pattern_list = pattern, { "|", pattern } ;

literal_pattern = literal ;
underscore_pattern = "_" ;
```

# Attributes

```ebnf
attribbute = attribute_kind, qualified_identifier, [ "(", expression_list, ")" ] ;
attribute_kind = "@" | "@!" ;
```

```
@deprecated(since: "5.2", note: "Use `bar` instead.")
fun foo(a: i32) -> a + 1;

@attribute
data Foo {
    some: i32,
    other: i32,
}

@attribute
data Bar {
    Some,
    Other,
}

@attribute
fun bar(ast: Ast, count: i32, fob: Foo, bar: Bar) -> Ast;
```

# Types

```
type = ( concrete_type | generic_type ), [ type_kind ] ;
type_kind = "!" | "?" ;
type_list = type, { ",", type } ;
concrete_type = qualified_identifier, [ "[", type_list, "]" ] ;
generic_type = "[", identifier, "]" ;
```

## Numeric

```

```

## Boolean

```
val t: bool = true;
val f: bool = false;
```

## Textual

```
val str: string = "hi";
val c: char = 'h';
```
