# SMoL Translator

This package defines the SMoL language and provides functions that
translate SMoL to other languages. The goal is to support translation
to at least JavaScript and Python.

Translation to JavaScript is ready. See [the SMoL vs JavaScript
section](#smol-vs-javascript) for limitations and known
incompatibilities between the languages.

Translation to Python is WIP. See [the SMoL vs Python
section](#smol-vs-python) for limitations and known incompatibilities
between the languages.

## Usage

Most people translate full programs. If this is your case, you can say

```
import { toJS } from SMoLTranslator;
toJS("program", smolSourceCode)
```

Replace `JS` with `PY` if you want Python-like syntax.

The translator supports more than program-level translation. Shown
below is a description of all supported context:

- `program`: accept any number of terms; every term ends with `;` for
  some languages; top-level expressions are typically wrapped in
  printing constructs (e.g., `console.log(_)` for JavaScript and
  `print(_)` for Python), except for assignment expressions.
- `function-body`: accept one or more terms, last of which must be an
  expression; every term ends with `;` for some languages; the last
  expression is wrapped in `return _`.
- `one-term`: accept exactly one term; no `;` is added.
- `many-terms`: accept any number of terms; no `;` is added.

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
