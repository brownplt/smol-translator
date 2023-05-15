# smol-translator

Translate SMoL to other programming languages. Translation to JavaScript is ready.
See [the SMoL vs JavaScript section](#smol-vs-javascript) for limitations and
known incompatibilities between the languages.
Translation to Python is WIP. See [the SMoL vs JavaScript section](#smol-vs-javascript) for limitations and
known incompatibilities between the languages.

## Usage

Most people translate full programs. If this is your case, you can say

```
import { toJS } from SMoLTranslator;
toJS("program", smolSourceCode)
```

Replace `JS` with `PY` if you want Python-like syntax.

The translator supports more than program-level translation. Shown below is a description of all supported context:

- `program`: accept any number of terms; every term ends with `;` for some languages; expressions are wrapped in `console.log(_)` or `print(_)`, except for assignment expressions.
- `function-body`: accept one or more terms, last of which must be an expression; every term ends with `;` for some languages; the last expression is wrapped in `return _`.
- `one-term`: accept exactly one term; no `;` is added.
- `many-terms`: accept any number of terms; no `;` is added.

## Test Suite

This translator has been tested with more than 80% programs from the SMoL Tutor. 154 were tested. 31 were skipped for various reasons:

1. (8 skipped) Programs from the heap tutorial. This tutorial is all about heap structure, so expected answers are NOT program outputs.
2. (20 skipped) Programs from the local tutorial. This tutorial is all about local binding forms, which doesn't apply to many languages.
3. (2 skipped) Programs where the expected output involve `@`. These programs are, again, testing heap structures.
4. (1 skipped) Programs where the expected output involve `=`. These programs output circular data structures. It is difficult to translate the outputs.

## SMoL vs JavaScript

Every SMoL program can be translated to JavaScript. The translation is mostly straightforward,
except that `let` expressions (in generally) must be turned into [Immediately Invoked Function Expressions](https://developer.mozilla.org/en-US/docs/Glossary/IIFE).

Program outputs might differ slightly after the translation due to the following language differences:

- In JavaScript, division by zero produces `Infinity` or a number rather than an error.
- Variable mutation (e.g., `x = 2`) produces the new value (in this case, `2`) rather than a none/void/unit value.
- Indexing an array (known as "vector" in SMoL) outside its index range (e.g., `[1, 2][99]`) produces the `undefined` value rather than an error.

All 24 failure (out of 154 tests) are due to the aforementioned reasons.

## SMoL vs Python

Many SMoL program can be translated to idiomatic Python. However, we find it difficult to translate the following language constructs:

- multi-term `lambda` expressions: Python `lambda` must contain exactly one term; and the term must be an expression.
- `let` expressions: Python provides nothing similar enough to `let` expressions; we also can't translate `let` to function application
  as [we did for JavaScript](#smol-vs-javascript) because the aforementioned limitation of Python `lambda`.
