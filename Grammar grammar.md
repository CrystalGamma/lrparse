Grammar for the grammar specification files
===========================================

At the highest level of abstraction, a grammar specification for use in lrparse is a token tree of tokens
as used in the Rust language.

There are types of items at the top level of a grammar specification:

Prelude
-------

```
#{ code }
```

The code given in the braces is output at the beginning of the file.
It can be used to, e.g. import symbols from other modules.

The prelude must define or import a type called CodeReference which implements the following methods:

* range(&self, end: &CodeReference) -> CodeReference :
	returns the range between the beginning of self and and the end of end
* start(&self) -> CodeReference : returns a zero-length range which points to the beginning of self

Terminal Symbol Definition
--------------------------

There are two ways to define a terminal symbol:

As a unit symbol:
The symbol contains no meaningful information to the parser
```
#Name
```

As a typed symbol:
```
#Name(type)
```

If a type is used, it must be a single type: if the symbol is represented by a tuple,
the normal parentheses do not implicitly define a tuple, i.e. the following will produce invalid code:
```
#Name(type1, type2)
```
Instead use:
```
#Name((type1, type2))
```

Non-terminal Symbol Definition
------------------------------

As with terminal symbols, non-terminals can be defined with or without a type:
```
Name { rules }
```

```
Name(type) { rules }
```

All remarks about types for terminal symbols apply here, too.

Terminal and non-terminal symbols share a namespace.
Do not use a symbol called Other, it will produce invalid code!

The rules are sequence of symbols, followed by a brace block.
Besides the named symbols, character literals can be used as (terminal) symbols.

The brace block is Rust code which will be executed on reduction by the rule.
It will be executed in a method of the Parser object, which has three parameters:
self, symbols, which is a tuple of the value of all non-unit symbols contained in the rule, in order,
and pos, the code position of the entire symbol to be constructed.
The block must return an Ok(_) with the value of the symbol or an Err(_) with an error code defined in the grammar.

After the brace block, there can optionally be a comma in order to improve readability of the grammar.

Error Code Definition
---------------------

Like symbols, error codes can be defined as unit or with a type:

```
!Name
```

```
!Name(type)
```

Error codes have their own namespace.
As content is never extracted from the error code by the parser, multiple types can be used in an error code.