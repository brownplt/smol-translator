# SMoL Translator

This package defines the SMoL language and provides functions that
translate SMoL programs and outputs to other languages (currently JavaScript and Python).

See [the SMoL vs JavaScript
section](#smol-vs-javascript) for limitations and known
incompatibilities between the languages.

See [the SMoL vs Python
section](#smol-vs-python) for limitations and known incompatibilities
between the languages.

## Installation

```sh
npm install git+https://github.com/brownplt/smol-translator.git
```

## Usage

### Text-to-text Whole-program Translation

Most people translate full programs. If this is your case, you can say

```javascript
import * as SMoL from "../src/SMoL.bs.js";
const program = ... // the program you want to translate
console.log(SMoL.JSTranslator.translateProgram(true, program))
```

Replace `JS` with `PY` if you want Python-like syntax.

### Advanced Usage: Node-to-Node Translation

In some cases, you might want to know what _each part_ of the source program translates to.

This package _used to_ allow users to translate only a part of a program (e.g., a function body). However, for some target languages, it is impossible to provide the correct translation without the whole program. For
example, assume that we want to translate the following function body to Python.

```scheme
(set! n (+ n 1))
n
```

Without knowing where `n` is defined, we can't tell if the correct translation is

```python
nonlocal n
n = n + 1
return n
```

or

```python
global n
n = n + 1
return n
```

I want to provide accurate translation. So translators in this package always take whole program as input. The output can be an enriched AST that include translation of every single AST nodes.

See `test/example.js` for examples.

### Advanced Usage: Update Printing by Replacing Nodes

Sometimes you might already have a program printed, and then decide to change the printed form partially. For example, when I am teaching students how a program runs, I might say the next step of

```lisp
(defvar x 2)
(defvar y (+ x 1))
(* x y)
```

is

```lisp
(defvar x 2)
(defvar y 3)
(* x y)
```

For another example, I might want to say the evaluation context of the function call `(f 5)` in the following program

```lisp
(deffun (f n)
  (+ n 4))
(* (+ (f 5) 2 3) 7)
```

is

```lisp
(deffun (f n)
  (+ n 4))
(* (+ ◌ 2 3) 7)
```

In either example, it is desirable to replace an AST node with a new printed form (i.e., `(+ x 1)` ↦ `3` and `(f 5)` ↦ `◌`). A string search-and-replace does not work for all cases because the same string might appear multiple times in the source program. So, instead, I record the source code location of each AST node and maintain the location in the output.

## Test Suite

This translator has been tested with more than 80% programs from the
SMoL Tutor. 154 were tested. 31 were skipped (not tested) for various
reasons:

1. (8 skipped) Programs from the heap tutorial. This tutorial is all
   about heap structure, so expected answers are NOT program outputs.
2. (20 skipped) Programs from the local tutorial. This tutorial is all
   about local binding forms, which doesn't apply to many languages.
3. (2 skipped) Programs where the expected output involve `@`. These
   programs are, again, testing heap structures.
4. (1 skipped) Programs where the expected output involve `=`. These
   programs output circular data structures. It is difficult to
   translate the outputs.

## SMoL vs JavaScript

Every SMoL program can be translated to a valid JavaScript program.
The translation is straightforward most of the time, except that `let`
expressions (in generally) must be turned into [Immediately Invoked
Function
Expressions](https://developer.mozilla.org/en-US/docs/Glossary/IIFE).

Program outputs might differ slightly after the translation due to the
following language differences:

- In JavaScript, division by zero produces `Infinity` or a number
  rather than an error.
- Variable assignment (e.g., `x = 2`) produces the new value (in this
  case, `2`) rather than a none/void/unit value.
- Indexing an array (known as "vector" in SMoL) outside its index
  range (e.g., `["a", "b", "c"][99]`) produces the `undefined` value
  rather than an error.

All 24 test failure (out of 154 tests) are due to the aforementioned
reasons.

## SMoL vs Python

The translator might produce a Python program that reduces to
different results if the source program

- uses `set!` inside a `lambda` to assign externally defined
  variables, or
- expects a distinction between variable definitions and variable
  assignments, or
- expects variable assignments to produce `#<void>` (known as `None`
  in Python), or
- expects `#<void>` not be printed, or
- expects infix operators to be valid expressions, or
- expects, for example, `(2 + 4) / 2` is `3` rather than `3.0`

All 24 test failure (out of 154 tests) are due to the aforementioned
reasons.

## Key Challenges to the Translation

There are a few key differences between SMoL and the target languages
(currently JavaScript and Python):

- `return` is needed.
- Some SMoL constructs (e.g., `if`) has an expression version and a
  statement version in a target language. Even worse, the less
  flexible target construct (e.g., `if` statements) is more idiomatic
  in the target language. So the translator need to use the statement
  version as often as possible while not doing it blindly.
- Top-level expressions need to be wrapped in a printing construct.
- Python has unusual `nonlocal` and `global` keywords
