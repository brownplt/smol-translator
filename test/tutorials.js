export const tutorials =
{
"def1":
{
  "misconceptions": [
    "IsolatedFun",
    "FlatEnv"
  ],
  "order": [
    "intro_smol",
    "intro_defvar",
    "warmup_defvar",
    "intro_deffun_1",
    "warmup_deffun_1",
    "intro_deffun_2",
    "warmup_deffun_2",
    "intro_block",
    "intro_error",
    "warmup_error",
    "goal_undefined"
  ],
  "questions": {
    "goal_undefined": {
      "emphasize": "It is an error to evaluate an undefined variable.\n"
    },
    "intro_block": {
      "emphasize": "We have two kinds of places where a definition might happen: the top-level **block** and\nfunction bodies (which are also **blocks**).\nA block is a sequence of definitions and expressions.\n\nBlocks form a tree-like structure in a program.\nFor example, we have four blocks in the following program:\n\n```\n(defvar n 42)\n\n(deffun (f x)\n  (defvar y 1)\n  (+ x y))\n\n(deffun (g)\n  (deffun (h m)\n    (* 2 m))\n  (f (h 3)))\n\n(g)\n```\n\nThe blocks are:\n\n- the top-level block, where the definitions of `n`, `f`, and `g` appear\n- the body of `f`, where the definition of `y` appears, which is a sub-block of the top-level block\n- the body of `g`, where the definition of `h` appears, which is also a sub-block of the top-level block, and\n- the body of `h`, where no local definition appears, which is a sub-block of the body of `g`\n"
    },
    "intro_deffun_1": {
      "confirm": "The following program illustrates **function definitions**.\n\n```\n(deffun (f x y)\n  (/ (+ x y) 2))\n\n(f (* 2 3) 4)\n```\n\nThis program produces `5`. It defines a function named `f`,\nwhich has two **formal parameters**, `x` and `y`,\nand **calls** the function with the **actual parameters** `6` and `4`.\n"
    },
    "intro_deffun_2": {
      "confirm": "Function definitions can contain definitions, for example\n\n```\n(deffun (f x y)\n  (defvar n (+ x y))\n  (/ n 2))\n\n(f (* 2 3) 4)\n```\n"
    },
    "intro_defvar": {
      "confirm": "In this tutorial, we will learn about **definitions**.\n\nThe following program illustrates **variable definitions**.\n\n```\n(defvar x 1)\n(defvar y 2)\nx\ny\n(+ x y)\n```\n\nIn this program, the first variable definition **binds** `x` to `1`, and\nthe second variable definition binds `y` to `2`.\n\nThis program produces three values:\nthe value of `x`, the value of `y`, and the value of `(+ x y)`.\nThese values are `1`, `2`, and `3`, respectively.\nIn this Tutor, we will write the result of running this program on a single line as\n\n`1 2 3`\n\nrather than\n\n<code>1\n2\n3\n</code>\n"
    },
    "intro_error": {
      "confirm": "We use the term **values** to refer to the typical result computations.\nThese include numbers, strings, booleans, etc.\nHowever, running a program can also produce an **error**.\n\nFor example, the result of the following program is an `error`\nbecause you can't divide a number by 0.\n\n```\n(defvar x 23)\n(/ x 0)\n```\n\nThe result of the following program is `#t #f error`\nbecause you can't add two boolean values.\n\n```\n#t\n#f\n(+ #t #f)\n```\n"
    },
    "intro_smol": {
      "confirm": "This series of tutorials revolve around a central idea, **SMoL**, the Standard Model of Languages.\nThis is the embodiment of the computational core of many of our widely-used programming languages,\nfrom C# and Java to JavaScript, and Python to Scala and Racket. All these languages (and many others),\nto a large extent, have a common computational core:\n\n- lexical scope\n- nested scope\n- eager evaluation\n- sequential evaluation (per “thread”)\n- mutable first-order variables\n- mutable first-class structures (objects, vectors, etc.)\n- higher-order functions that close over bindings\n- automated memory management (e.g., garbage collection)\n\nWhat goes in SMoL is, of course, a judgment call:\nwhen a feature isn't present across a large number of diverse languages (like static types),\nor shows too much variation between languages that have it (like objects),\nwe do not include that in the *standard* model.\nBut it is not a value judgment:\nthe standard model is about what languages *are*, rather than what languages *should be*.\n"
    },
    "warmup_deffun_1": {
      "answer": "6",
      "feedback": "",
      "program": "(deffun (f x y z)\n  (+ x (+ y z)))\n(f 2 1 3)\n",
      "again": {
        "answer": "6",
        "program": "(deffun (g x y z)\n  (* x (* y z)))\n(g 2 1 3)\n"
      }
    },
    "warmup_deffun_2": {
      "answer": "5",
      "feedback": "",
      "program": "(deffun (f x y z)\n  (defvar p (* y z))\n  (+ x p))\n(f 2 1 3)\n",
      "again": {
        "answer": "9",
        "program": "(deffun (f a b c)\n  (defvar q (+ b c))\n  (* a q))\n(f 3 2 1)\n"
      }
    },
    "warmup_defvar": {
      "answer": "1 3",
      "feedback": "The first definition binds `x` to the value `1`.\nThe second definition binds `y` to the value of `(+ x 2)`, which is `3`.\nSo, the result of this program is `1 3`.\n",
      "program": "(defvar x 1)\n(defvar y (+ x 2))\nx\ny\n",
      "again": {
        "answer": "10 20",
        "program": "(defvar a 10)\n(defvar b (* 2 a))\na\nb\n"
      }
    },
    "warmup_error": {
      "answer": "error",
      "feedback": "The variable `abc` is not defined.",
      "program": "(defvar xyz 42)\nabc\n",
      "again": {
        "answer": "error",
        "program": "(defvar foo 77)\nbar\n"
      }
    }
  }
},
"def2":
{
  "misconceptions": [
    "IsolatedFun",
    "FlatEnv"
  ],
  "order": [
    "intro_more_on_def",
    [
      "new_three_layer",
      "new_ref_global_and_local",
      "what_is_x_2",
      "error_when_refer_to_undefined",
      "shadow_rather_than_overwrite"
    ],
    "reflect_lexical_scope",
    "goal_lexical_scope",
    "goal_lexical_scope_labelling_1",
    "goal_lexical_scope_labelling_2",
    "goal_lexical_scope_labelling_3",
    "intro_scope",
    "scope_terminology_practice",
    "keyframe_lexical_scope_1",
    "keyframe_lexical_scope_2"
  ],
  "questions": {
    "error_when_refer_to_undefined": {
      "answer": "error",
      "feedback": "`(+ (f 2) y)` is evaluated in the top-level block, where `y` is not defined.\nSo, this program errors.\n",
      "program": "(deffun (f x)\n  (defvar y 1)\n  (+ x y))\n\n(+ (f 2) y)\n",
      "again": {
        "answer": "error",
        "program": "(deffun (foo bar)\n  (defvar zzz 8)\n  (* bar zzz))\n\n(* (foo 9) zzz)\n"
      },
      "misconceptions": {
        "4": {
          "feedback": "`(defvar y 1)` binds `y` to `1`.\nYou might think the binding applies everywhere.\nHowever, it only applies to the body of `g`.\n`(+ (f 2) y)` appears in the top-level block, so it tries to refer to\na `y` defined in the top-level block. But `y` is not defined in the top-level block.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "FlatEnv"
        }
      }
    },
    "goal_lexical_scope": {
      "emphasize": "Variable references follow the hierarchical structure of blocks.\n\nIf the variable is defined in the current block, we use that declaration.\n\nOtherwise, we look up the block in which the current block appears, and so on recursively.\n(Specifically, if the current block is a function body, the next block will be the block in which the function definition is;\nif the current block is the top-level block, the next block will be the **primordial block**.)\n\nIf the current block is already the primordial block and we still haven't found a corresponding declaration,\nthe variable reference errors.\n\nThe primordial block is a non-visible block enclosing the top-level block.\nThis block defines values and functions that are provided by the language itself.\n"
    },
    "goal_lexical_scope_labelling_1": {
      "select_and_comment": "Now please scroll back and select 1-3 programs that make the point that\n\n> If the variable is declared in the current block, we use that declaration.\n\nYou don't need to select *all* such programs.\n"
    },
    "goal_lexical_scope_labelling_2": {
      "select_and_comment": "Please select 1-3 programs that make the point that\n\n> Otherwise, we look up the block in which the current block appears, and so on recursively.\n\nYou don't need to select *all* such programs.\n"
    },
    "goal_lexical_scope_labelling_3": {
      "select_and_comment": "Please select 1-3 programs that make the point that\n\n> If the current block is already the primordial block and we still haven't found a corresponding declaration,\n> the variable reference errors.\n\nYou don't need to select *all* such programs.\n"
    },
    "intro_more_on_def": {
      "confirm": "In this tutorial, we will learn *more* about definitions.\n"
    },
    "intro_scope": {
      "emphasize": "The **scope** of a variable is the region of a program where we can refer to the variable.\nTechnically, it includes the block in which the variable is defined (including sub-blocks)\n*except* the sub-blocks where the same name is re-defined.\nWhen the exception happens, that is, when the same name is defined in a sub-block,\nwe say that the variable in the sub-block **shadows** the variable in the outer block.\n\nWe say a variable reference (e.g., \"the `x` in `(+ x 1)`\") is **in the scope of** a declaration (e.g., \"the `x` in `(defvar x 23)`\")\nif and only if the former refers to the latter.\n"
    },
    "keyframe_lexical_scope_1": {
      "prompt": "Here is a program that confused many students\n\n```\n(defvar x 1)\n(deffun (foo)\n  x)\n(deffun (bar)\n  (defvar x 2)\n  (foo))\n\n(bar)\n```\n\nPlease\n\n1. Run this program in the stacker by clicking the green run button above;\n2. The stacker would show how this program produces its result(s);\n3. Keep clicking <button>⏭ Next</button> until you reach a configuration that you find particularly helpful;\n4. Click <button>🔗 Share This Configuration</button> to get a link to your configuration;\n5. Submit your link below;\n"
    },
    "keyframe_lexical_scope_2": {
      "prompt": "Please write a couple of sentences to explain how your configuration explains the result(s) of the program.\n"
    },
    "new_ref_global_and_local": {
      "answer": "3",
      "feedback": "`(f)` returns the value of `(g)`.\nThe value of `(g)` is the value of `(+ x y)`,\nwhich is `(+ 1 2)`, which is `3`.\n",
      "program": "(defvar x 1)\n(deffun (f)\n  (defvar y 2)\n  (deffun (g)\n    (+ x y))\n  (g))\n(f)\n",
      "again": {
        "answer": "18",
        "program": "(defvar a 9)\n(deffun (fun)\n  (defvar b 2)\n  (deffun (prod)\n    (* a b))\n  (prod))\n(fun)\n"
      },
      "misconceptions": {
        "error": {
          "feedback": "You might think `(+ x y)` can't refer to `x` and `y`.\nHowever, `(+ x y)` appears in the body of `g`, which is a sub-block of\nthe body of `f`.\nSo, it can refer to the `x` defined in the top-level block and\nthe `y` defined in `f`.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "IsolatedFun"
        }
      }
    },
    "new_three_layer": {
      "answer": "10",
      "feedback": "This program binds `x` to `1` and `f` to a function and then\nevaluates `(+ (f 3) 4)`.\nThe value of `(+ (f 3) 4)` is the value of `(+ (g) 4)`,\nwhich is the value of `(+ (+ x y z) 4)`,\nwhich is the value of `(+ (+ 1 3 2) 4)`,\nwhich is `10`.\n",
      "program": "(defvar x 1)\n(deffun (f y)\n  (deffun (g)\n    (defvar z 2)\n    (+ x y z))\n  (g))\n(+ (f 3) 4)\n",
      "again": {
        "answer": "24",
        "program": "(defvar aa 3)\n(deffun (abc bb)\n  (deffun (h)\n    (defvar cc 2)\n    (* aa bb cc))\n  (h))\n(* (abc 4) 1)\n"
      },
      "misconceptions": {
        "error": {
          "feedback": "You might think `(+ x y z)` can't refer to `x` and `y`.\nHowever, `(+ x y z)` appears in the body of `g`, which is a sub-block of\nthe body of `f`, which is a sub-block of the top-level block.\nSo, it can refer to the `x` defined in the top-level block\nand the `y` defined in `f`.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "IsolatedFun"
        }
      }
    },
    "reflect_lexical_scope": {
      "prompt": "When we see a variable reference (e.g., the `x` in `(+ x 1)`), how do we find its value, if any?\n"
    },
    "scope_terminology_practice": {
      "checkboxes": [
        [
          false,
          "The scope of the top-level `x` includes the whole program."
        ],
        [
          true,
          "The scope of the top-level `x` includes `(+ x 3)`."
        ],
        [
          false,
          "The top-level `x` shadows the `x` defined in `f`."
        ],
        [
          true,
          "The `x` in `(+ x 1)` is in the scope of the `x` defined in `f`."
        ],
        [
          true,
          "The `x` in `(+ x 2)` is in the scope of the `x` defined in `f`."
        ],
        [
          false,
          "The `x` in `(+ x 3)` is in the scope of the `x` defined in `f`."
        ]
      ],
      "feedback": "The scope of the top-level `x` includes the whole program *except* the body of `f`.\nThe scope of the `x` defined in `f` is the body of `f`.\n\nThe `x` defined in `f` shadows the top-level `x` rather than the other way around.\n",
      "prompt": "Please select all statements that apply to the following program:\n\n```\n(defvar x 45)\n(deffun (f)\n  (deffun (g)\n    (+ x 1))\n  (defvar x 67)\n  (+ x 2))\n(f)\n(+ x 3)\n```\n"
    },
    "shadow_rather_than_overwrite": {
      "answer": "3",
      "feedback": "This program binds `x` to `1` and `f` to a function in the top-level block. It binds `x` to `2` in the body of `f`.\nSo, `(f 0)` evaluates to `(+ 2 0)`, which is `2`, and `(+ (f 0) x)` evaluates to `(+ 2 1)`, which is `3`.\n",
      "program": "(defvar x 1)\n(deffun (f y)\n  (defvar x 2)\n  (+ x y))\n\n(+ (f 0) x)\n",
      "again": {
        "answer": "12",
        "program": "(defvar cat 7)\n(deffun (k dog)\n  (defvar cat 4)\n  (+ cat dog))\n\n(+ (k 1) cat)\n"
      },
      "misconceptions": {
        "4": {
          "feedback": "`(defvar x 2)` binds `x` to `2`.\nYou might think the binding applies everywhere.\nHowever, it only applies to the body of `f`.\nThe `x` in `(+ (f 0) x)` appears in the top-level block, so it refers to the `x` defined in the top-level block.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "FlatEnv"
        }
      }
    },
    "what_is_x_2": {
      "answer": "1",
      "feedback": "`(g)` evaluates to `(f)`, which evaluates to `1` because\n`f` and `x` are both defined in the same block, and\n`f` does *not* get the `x` defined inside `g`.\n",
      "program": "(defvar x 1)\n(deffun (f)\n  x)\n(deffun (g)\n  (defvar x 2)\n  (f))\n\n(g)\n",
      "again": {
        "answer": "21",
        "program": "(defvar s 21)\n(deffun (i) s)\n(deffun (j)\n  (defvar s 76)\n  (i))\n\n(j)\n"
      },
      "misconceptions": {
        "2": {
          "feedback": "`(defvar x 2)` binds `x` to `2`.\nYou might think the binding applies everywhere.\nHowever, it only applies to the body of `g`.\nThe `x` in the body of `f` refers to the `x` defined in the top-level block.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "FlatEnv"
        },
        "error": {
          "feedback": "You might think `f` can't refer to `x`.\nHowever, `f` is defined in the top-level block.\nSo, it can refer to the `x` defined in the top-level block.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "IsolatedFun"
        }
      }
    }
  }
},
"def3":
{
  "misconceptions": [
    "Lazy"
  ],
  "order": [
    "intro_even_more_on_def",
    [
      "defvar_binding_cause_evaluation",
      "funcall_binding_cause_evaluation",
      "bad_order"
    ],
    "reflect_order_of_computation",
    "goal_order_of_computation",
    "goal_order_of_computation_labelling_1",
    "goal_order_of_computation_labelling_2",
    "keyframe_order_of_computation_1",
    "keyframe_order_of_computation_2"
  ],
  "questions": {
    "bad_order": {
      "answer": "error",
      "feedback": "The first definition tries to bind `x` to the value of `(+ y 1)`.\nTo evaluate `(+ y 1)`, we need the value of `y`.\nBut `y` is not bound to a value at that moment.\n",
      "program": "(defvar x (+ y 1))\n(defvar y 2)\n\nx\ny\n",
      "again": {
        "answer": "error",
        "program": "(defvar baz (+ bar 1))\n(defvar bar 2)\n\nbaz\nbar\n"
      },
      "misconceptions": {
        "3 2": {
          "feedback": "You might think `(+ y 1)` is able to refer to `y` and hence evaluate to a value.\nHowever, `y` has not been bound to a value when `(+ y 1)` is evaluated.\nIn SMoL, every block evaluates its definitions and expressions in reading order (i.e., top-to-bottom and left-to-right).\n",
          "misconception": "Lazy"
        }
      }
    },
    "defvar_binding_cause_evaluation": {
      "answer": "error",
      "feedback": "When you define a variable (in this case, `x`), you have to bind it to a value, no\nmatter whether or not you need the value of that variable later in the program.\nThe program errors when it tries to evaluate `(/ 12 0)`.\n",
      "program": "(defvar x (/ 12 0))\n3\n",
      "again": {
        "answer": "error",
        "program": "(defvar b (/ 79 0))\n1\n"
      },
      "misconceptions": {
        "3": {
          "feedback": "You might think `(/ 12 0)` is not evaluated because `x` is not needed.\nHowever, this is not the case.\nIn SMoL, every variable definition evaluates the expression immediately and binds the variable to the value, even if the variable is not used later in the program.\n",
          "misconception": "Lazy"
        }
      }
    },
    "funcall_binding_cause_evaluation": {
      "answer": "error",
      "feedback": "Function calls bind their formal parameters\n(in this case, there is one formal parameter, `x`)\nto the values of actual parameters\n(in this case, there is one actual parameter, `(/ 12 0)`).\nThe program errors when it tries to evaluate `(/ 12 0)`.\n",
      "program": "(deffun (f x)\n  3)\n(f (/ 12 0))\n",
      "again": {
        "answer": "error",
        "program": "(deffun (z y)\n  5)\n(z (/ 78 0))\n"
      },
      "misconceptions": {
        "3": {
          "feedback": "You might think `(/ 12 0)` is not evaluated because `x` is not needed in the function.\nHowever, this is not the case.\nIn SMoL, every function call evaluates the actual parameters immediately and binds the values to formal parameters, even when the formal parameter is not used in the function.\n",
          "misconception": "Lazy"
        }
      }
    },
    "goal_order_of_computation": {
      "emphasize": "- Variables are bound to values. Specifically, every variable definition evaluates the expression immediately and binds the variable to the value, even if the variable is not used later in the program; every function call evaluates the actual parameters immediately and binds the values to formal parameters, even if the formal parameter is not used in the function.\n- Every block evaluates its definitions and expressions in reading order (i.e., top-to-bottom and left-to-right).\n"
    },
    "goal_order_of_computation_labelling_1": {
      "select_and_comment": "Please scroll back and select 1-3 programs that together make these points.\n\n> - Variables are bound to values. Specifically, every variable definition evaluates the expression immediately and binds the variable to the value, even if the variable is not used later in the program; every function call evaluates the actual parameters immediately and binds the values to formal parameters, even if the formal parameter is not used in the function.\n\nYou don't need to select all such programs.\n"
    },
    "goal_order_of_computation_labelling_2": {
      "select_and_comment": "Please scroll back and select 1-3 programs that together make these points.\n\n> - Every block evaluates in top-to-bottom, left-to-right order.\n\nYou don't need to select all such programs.\n"
    },
    "intro_even_more_on_def": {
      "confirm": "In this tutorial, we will learn even more about definitions.\n"
    },
    "keyframe_order_of_computation_1": {
      "prompt": "Here is a program that confused many students\n\n```\n(deffun (addy x)\n  (+ x y))\n(defvar s (addy 1))\n(defvar y 2)\n\ns\n```\n\nPlease\n\n1. Run this program in the stacker by clicking the green run button above;\n2. The stacker would show how this program produces its result(s);\n3. Keep clicking <button>⏭ Next</button> until you reach a configuration that you find particularly helpful;\n4. Click <button>🔗 Share This Configuration</button> to get a link to your configuration;\n5. Submit your link below;\n"
    },
    "keyframe_order_of_computation_2": {
      "prompt": "Please write a couple of sentences to explain how your configuration explains the result(s) of the program.\n"
    },
    "reflect_order_of_computation": {
      "prompt": "In what order are definitions and expressions evaluated?\n"
    }
  }
},
"vectors1":
{
  "misconceptions": [],
  "order": [
    "intro_vectors",
    "warmup_mvec",
    "intro_vecref",
    "warmup_vecref",
    "intro_vecset",
    "warmup_vecset"
  ],
  "questions": {
    "intro_vecref": {
      "confirm": "The following program illustrates how to refer to vector elements.\n\n```\n(defvar v (mvec 84 75))\n(vec-ref v 0)\n(vec-ref v 1)\n(vec-ref v 2)\n```\n\nThis program produces `84 75 error`.\nIt refers to the `0`-th (i.e., first) element, the `1`-th element, and then\ntries to refer to the `2`-th element.\n"
    },
    "intro_vecset": {
      "confirm": "The following program illustrates how to mutate vectors.\n\n```\n(defvar m (mvec 62 77))\n(vec-set! m 0 83)\n(vec-ref m 0)\n```\n\nThis program produces `83`.\nIt **mutates** the vector by **replacing** the `0`-th element with `83`\nand then refers to the `0`-th element.\n\n"
    },
    "intro_vectors": {
      "confirm": "In this tutorial, we will learn about **mutable values**, illustrated with **vectors**.\n\nThe following program illustrates how to create vectors\n\n```\n(mvec (mvec 1 2) (mvec 3) (mvec))\n```\n\nThis program produces `#(#(1 2) #(3) #())`. It creates four vectors:\n\n* a two-**element** vector that refers `1` and `2`\n* a one-element vector that refers `3`\n* an empty vector\n* a three-element vector that refers all three aforementioned vectors\n"
    },
    "warmup_mvec": {
      "answer": "#(44 45)",
      "feedback": "This program binds `n` to `44` and `f` to a function that adds 1 to its parameter.\nAfter that, the program creates a vector that refers the value of `n`,\nwhich is `44`,\nand the value of `(f n)`,\nwhich is `45`.\nThe vector is printed as `#(44 45)`.\n",
      "program": "(defvar n 44)\n(deffun (f x)\n  (+ x 1))\n(mvec n (f n))\n",
      "again": {
        "answer": "#(14 7)",
        "program": "(defvar x 7)\n(deffun (fun n)\n  (* n 2))\n(mvec (fun x) x)\n"
      }
    },
    "warmup_vecref": {
      "answer": "43",
      "feedback": "Recall that the left-most element is the 0-th (rather than 1-th!) element.\n`(vec-ref v 1)` produces the value of `(mvec 43 66)`.\nSo, `(vec-ref vr 0)` produces `43`.\n",
      "program": "(defvar v (mvec (mvec 58 43) (mvec 43 66)))\n(defvar vr (vec-ref v 1))\n(vec-ref vr 0)\n",
      "again": {
        "answer": "88",
        "program": "(defvar v (mvec (mvec 81 82 83) (mvec 84 85 86) (mvec 87 88 89)))\n(vec-ref (vec-ref v 2) 1)\n"
      },
      "misconceptions": {
        "58": {
          "misconception": "Other"
        }
      }
    },
    "warmup_vecset": {
      "answer": "#(67 73)",
      "feedback": "`x` is bound to a vector.\nThe initial content of the vector is `92` and `73`.\nThe mutation replaces the first element with `67`.\nA vector referring `67` and `73` is printed as `#(67 73)`.\n",
      "program": "(defvar x (mvec 92 73))\n(vec-set! x 0 67)\nx\n",
      "again": {
        "answer": "#(66 64)",
        "program": "(defvar v (mvec 83 64))\n(vec-set! v 0 66)\nv\n"
      },
      "misconceptions": {
        "67 73": {
          "feedback": "`x` is bound to a vector.\nA vector referring `67` and `73` is printed as `#(67 73)`.\n",
          "misconception": "Other"
        }
      }
    }
  }
},
"vectors2":
{
  "misconceptions": [
    "DefCopyStruct",
    "CallCopyStruct",
    "StructCopyStruct",
    "NoCircularity"
  ],
  "order": [
    "intro_more_on_vectors",
    [
      "alias_with_defvar",
      "new_alias_with_defvar",
      "alias_with_funcall",
      "new_alias_with_funcall",
      "alias_mvec_in_mvec",
      "alias_mvec_in_mpair",
      "basic_circularity",
      "new_circularity"
    ],
    "reflect_vectors",
    "goal_vectors",
    "goal_vectors_labelling",
    "reflect_heap",
    "goal_heap",
    "comprehension_check1",
    "comprehension_check2",
    "reflect_heap_not_env",
    "goal_heap_not_env",
    "keyframe_vector_aliasing_1",
    "keyframe_vector_aliasing_2"
  ],
  "questions": {
    "alias_mvec_in_mpair": {
      "answer": "#(44 62 73)",
      "feedback": "This program first creates a three-element vector and binds it to `v`.\nAfter that, the program creates a two-element vector. Both elements refer to the first vector.\nAfter that, the `0`-th element of the three-element vector is replaced with `44`.\nFinally, the three-element vector is printed.\n",
      "program": "(defvar v (mvec 51 62 73))\n(defvar vv (mvec v v))\n(vec-set! (vec-ref vv 1) 0 44)\n(vec-ref vv 0)\n",
      "again": {
        "answer": "#(42)",
        "program": "(defvar mv (mvec 63))\n(defvar mv2 (mvec mv mv))\n(vec-set! (vec-ref mv2 0) 0 42)\n(vec-ref mv2 1)\n"
      },
      "misconceptions": {
        "#(51 62 73)": {
          "feedback": "You might think the two elements of `v` refer to different vectors, so changing one doesn't change the other.\nHowever, they refer to the same vector.\nIn SMoL, a vector can be referred to by multiple places, and creating new vectors that refer to existing ones does not create new copies of the existing vectors.\n",
          "misconception": "StructsCopyStructs"
        }
      }
    },
    "alias_mvec_in_mvec": {
      "answer": "#(72 #(72))",
      "feedback": "`x` is bound to a one-element vector.\nThe only element of this vector is `53`.\n`v` is bound to a two-element vector.\nThe elements of this vector are `72` and the one-element vector.\nThe subsequent `(vec-set! x 0 72)` mutates the one-element vector.\nFinally, the value of `v` is printed.\n",
      "program": "(defvar x (mvec 53))\n(defvar v (mvec 72 x))\n(vec-set! x 0 72)\nv\n",
      "again": {
        "answer": "#(52 #(71) 53)",
        "program": "(defvar m (mvec 0))\n(defvar v (mvec 52 m 53))\n(vec-set! m 0 71)\nv\n"
      },
      "misconceptions": {
        "#(72 #(53))": {
          "feedback": "You might think `x` and the 1-th element of `v` refer to different vectors, so changing `x` doesn't change the 1-th element of `v`.\nHowever, they refer to the same vector.\nIn SMoL, a vector can be referred to by multiple places, and creating new vectors that refer to existing ones does not create new copies of the existing vectors.\n",
          "misconception": "StructsCopyStructs"
        }
      }
    },
    "alias_with_defvar": {
      "answer": "#(34)",
      "feedback": "The program creates a one-element vector (the only element being 77)\nand binds it to both `x` and `y`.\nThe `0`-th element of the vector is then replaced with `34`.\nSo, the vector is printed as `#(34)`.\n",
      "program": "(defvar x (mvec 77))\n(defvar y x)\n(vec-set! x 0 34)\ny\n",
      "again": {
        "answer": "#(87 26)",
        "program": "(defvar a (mvec 37 26))\n(defvar b a)\n(vec-set! a 0 87)\nb\n"
      },
      "misconceptions": {
        "#(77)": {
          "feedback": "You might think `x` and `y` refer to different vectors, so changing `x` doesn't change `y`.\nHowever, they refer to the same vector.\nIn SMoL, a vector can be referred to by more than one variable, and binding a vector to a new variable does not create a copy of that vector.\n",
          "misconception": "DefsCopyStructs"
        }
      }
    },
    "alias_with_funcall": {
      "answer": "#(34 86)",
      "feedback": "The program first creates a two-element vector. The elements are `71` and `86`.\nAfter that, the program defines a function `f`.\nThe function call `(f x)` replaces the first vector element with `34`.\nAfter that, the vector is printed. The vector now refers `34` and `86`, so the\nresult is `#(34 86)`.\n",
      "program": "(defvar x (mvec 71 86))\n(deffun (f y)\n  (vec-set! y 0 34))\n(f x)\nx\n",
      "again": {
        "answer": "#(52 17)",
        "program": "(defvar a (mvec 55 17))\n(deffun (foobar b)\n  (vec-set! b 0 52))\n(foobar a)\na\n"
      },
      "misconceptions": {
        "#(71 86)": {
          "feedback": "You might think `x` and `y` refer to different vectors, so changing `y` doesn't change `x`.\nHowever, they refer to the same vector.\nIn SMoL, a vector can be referred to by more than one variable, and vectors that are passed to a function in a function call do not get copied..\n",
          "misconception": "CallsCopyStructs"
        }
      }
    },
    "basic_circularity": {
      "answer": "54",
      "feedback": "`x` is bound to a vector.\n`(vec-set! x 0 x)` makes the vector refer to itself.\nThis is fine because a vector element can be any value, including itself.\nBesides, the vector is not copied, so the mutation finishes immediately.\n",
      "program": "(defvar x (mvec 43 54))\n(vec-set! x 0 x)\n(vec-ref x 1)\n",
      "again": {
        "answer": "84",
        "program": "(defvar x (mvec 84 73 69 52))\n(vec-set! x 1 x)\n(vec-ref x 0)\n"
      },
      "misconceptions": {
        "error": {
          "feedback": "You might think `x` can't refer to itself.\nHowever, this is not the case.\nIn SMoL, vectors can be referred to by arbitrary vectors, including themselves.\n",
          "misconception": "NoCircularity"
        }
      }
    },
    "comprehension_check1": {
      "answer": "@1 = #(1 2 3)",
      "distractors": [
        "@1 = #(1 2 x)",
        "There is nothing in the heap."
      ],
      "feedback": "%2% is wrong because the `(mvec 1 2 x)` creates a vector.\nEvery vector is stored on the heap.\n\n%1% is wrong because vectors refer values, while `x` is not a value.\n",
      "program": "(defvar x 3)\n(defvar v (mvec 1 2 x))\n",
      "prompt": "Which choice best describes the status of the heap at the end of the following program?"
    },
    "comprehension_check2": {
      "answer": "@100 = #(42); @200 = #(@100 @100)",
      "distractors": [
        "@100 = #(3); @200 = #(@100 @100)",
        "@100 = #(42); @200 = #(#(42) #(42))",
        "@100 = #(3); @200 = #(#(42) #(3))",
        "@100 = #(3); @200 = #(@300 @100); @300 = #(42)",
        "There is nothing in the heap."
      ],
      "feedback": "%2% and %3% are wrong.\nVectors refer values.\n`#(42)` and `#(3)` are not values, although some vector values are printed like them.\nSo, `@200 = #(#(42) #(42))` and `@200 = #(#(42) #(3))` can not be valid.\n\n`(mvec 3)` creates a 1-element vector, `@100`.\nThe only element is `3`.\n`(mvec mv mv)` creates a 2-element vector, `@200`.\nBoth elements of `@200` are `@100`.\nNo more vectors are created, which means %4% must be wrong.\nSo, then, the correct answer must be %1% or %0%.\n\nHowever, the subsequent mutation changes `@100` (the first element of `@200`).\nThe 0-th element of `@100` is mutated to `42`.\nSo, %0% is the correct answer.\n",
      "program": "(defvar mv (mvec 3))\n(defvar mv2 (mvec mv mv))\n(vec-set! (vec-ref mv2 0) 0 42)\n",
      "prompt": "Which choice best describes the status of the heap at the end of the following program?"
    },
    "goal_heap": {
      "emphasize": "In SMoL, each vector has its own unique **heap address** (e.g., `@100` and `@200`).\nThe mapping from addresses to vectors is called the **heap**.\n\n(**Note**: we use `@ddd` (e.g., `@123`, `@200`, and `@100`) to represent heap addresses.\nHeap addresses are *random*. The numbers don't mean anything.)\n"
    },
    "goal_heap_not_env": {
      "emphasize": "- Creating a vector does not inherently create a binding.\n- Creating a binding does not necessarily alter the heap.\n"
    },
    "goal_vectors": {
      "emphasize": "A vector can be referred to by more than one variable and even by other vectors (including itself).\nReferring to a vector does not create a copy of the vector; rather, they share the same vector. Specifically\n\n- Binding a vector to a new variable does not create a copy of that vector.\n- Vectors that are passed to a function in a function call do not get copied.\n- Creating new vectors that refer to existing ones does not create new copies of the existing vectors.\n\nThe references share the same vector. That is, vectors can be **aliased**.\n"
    },
    "goal_vectors_labelling": {
      "select_and_comment": "Now please scroll back and select 1-3 programs that make the above points.\n\nYou don't need to select *all* such programs.\n"
    },
    "intro_more_on_vectors": {
      "confirm": "In this tutorial, we will learn *more* about **mutable values**, illustrated with **vectors**.\n"
    },
    "keyframe_vector_aliasing_1": {
      "prompt": "Here is a program that confused many students\n\n```\n(defvar v (mvec 1 2 3 4))\n(defvar vv (mvec v v))\n(vec-set! (vec-ref vv 1) 0 100)\nvv\n```\n\nPlease\n\n1. Run this program in the stacker by clicking the green run button above;\n2. The stacker would show how this program produces its result(s);\n3. Keep clicking <button>⏭ Next</button> until you reach a configuration that you find particularly helpful;\n4. Click <button>🔗 Share This Configuration</button> to get a link to your configuration;\n5. Submit your link below;\n"
    },
    "keyframe_vector_aliasing_2": {
      "prompt": "Please write a couple of sentences to explain how your configuration explains the result(s) of the program.\n"
    },
    "new_alias_with_defvar": {
      "answer": "#(34)",
      "feedback": "`x` is first bound to a vector that refers `62`. `y` is bound to the same vector.\nThe vector mutation replaces `62` with `34`. So, eventually, the vector becomes `#(34)`.\n",
      "program": "(defvar x (mvec 62))\n(defvar y x)\n(vec-set! y 0 34)\nx\n",
      "again": {
        "answer": "#(55 48)",
        "program": "(defvar foo (mvec 65 48))\n(defvar bar foo)\n(vec-set! bar 0 55)\nfoo\n"
      },
      "misconceptions": {
        "#(62)": {
          "feedback": "You might think `x` and `y` refer to different vectors, so changing `y` doesn't change `x`.\nHowever, they refer to the same vector.\nIn SMoL, a vector can be referred to by more than one variable, and binding a vector to a new variable does not create a copy of that vector.\n",
          "misconception": "DefsCopyStructs"
        }
      }
    },
    "new_alias_with_funcall": {
      "answer": "#(34 83)",
      "feedback": "The program creates a vector and binds it to `x`.\nThe function call `(f x)` binds `y` to the same vector.\nThe function replaces the `0`-th element of the vector with `34`\nand then returns the vector.\nSo, the final result is the vector, which is printed as `#(34 83)`.\n",
      "program": "(defvar x (mvec 99 83))\n(deffun (f y)\n  (vec-set! x 0 34)\n  y)\n(f x)\n",
      "again": {
        "answer": "#(42 54)",
        "program": "(defvar a (mvec 66 54))\n(deffun (h b)\n  (vec-set! a 0 42)\n  b)\n(h a)\n"
      },
      "misconceptions": {
        "#(99 83)": {
          "feedback": "You might think `x` and `y` refer to different vectors, so changing `x` doesn't change `y`.\nHowever, they refer to the same vector.\nIn SMoL, a vector can be referred to by more than one variable, and vectors that are passed to a function in a function call do not get copied..\n",
          "misconception": "CallsCopyStructs"
        },
        "error": {
          "feedback": "You might think `(vec-set! x 0 34)` can't refer to `x`.\nHowever, `(vec-set! x 0 34)` appears in the body of `f`, which is a sub-block of the top-level block.\nSo, it can refer to the `x` defined in the top-level block.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "IsolatedFun"
        }
      }
    },
    "new_circularity": {
      "answer": "82",
      "feedback": "The program binds `x` to a two-element vector and `y` to a one-element vector.\nAfter that, it replaces the `0`-th element of the two-element vector with the one-element vector.\nThe other element of the two-element vector is still `82`.\n",
      "program": "(defvar x (mvec 74 82))\n(defvar y (mvec x))\n(vec-set! x 0 y)\n(vec-ref x 1)\n",
      "again": {
        "answer": "41",
        "program": "(defvar a (mvec 41 92))\n(defvar b (mvec a))\n(vec-set! a 1 b)\n(vec-ref a 0)\n"
      },
      "misconceptions": {
        "error": {
          "feedback": "You might think `x` and `y` can't form a circle.\nHowever, this is not the case.\nIn SMoL, vectors can be referred to by arbitrary vectors, including themselves.\n",
          "misconception": "NoCircularity"
        }
      }
    },
    "reflect_heap": {
      "feedback": "We can say, in the first program\n\n- `x` is bound to `@100`\n- `y` is bound to `@100`\n\nwhere\n\n- `@100` is `#(52 96)`\n\nWhile for the other program\n\n- `x` is bound to `@100`\n- `y` is bound to `@200`\n\nwhere\n\n- `@100` is `#(52 96)`\n- `@200` is `#(52 96)`\n",
      "prompt": "Reconsider these two programs that you might have seen. The only difference is in `(defvar y ___)`.\n\n```\n(defvar x (mvec 52 96))\n(defvar y x)\n(vec-set! x 0 34)\ny\n```\n\n```\n(defvar x (mvec 52 96))\n(defvar y (mvec 52 96))\n(vec-set! x 0 34)\ny\n```\n\nIt is tempting to describe the variables as\n\n- `x` is bound to `#(52 96)`\n- `y` is bound to `#(52 96)`\n\nThis description does not help us to understand the program\nbecause it can't explain why `(vec-set! x 0 34)` mutates `y` in one case,\nand does not in the other.\n\nWhat is a better way to describe the bindings that help solve this problem?\n"
    },
    "reflect_heap_not_env": {
      "prompt": "The following program defines two variables but\ncreates nothing on the heap.\n\n```\n(defvar x 2)\n(defvar y 3)\nx\ny\n```\n\nThe following program defines no variables but\ncreates two things on the heap.\n\n```\n(mvec 1 (mvec 2 3))\n```\n\nWhat did you learn from this pair of programs?\n"
    },
    "reflect_vectors": {
      "prompt": "What did you learn about vectors from these programs?"
    }
  }
},
"vectors3":
{
  "misconceptions": [],
  "order": [
    "intro_even_more_on_mvec",
    "warmup_vector",
    [
      "mvecs_are_mvec",
      "vector_not_flatten",
      "alias_mvec_in_mvec",
      "new_alias_mvec_in_mvec",
      "basic_circularity"
    ]
  ],
  "questions": {
    "alias_mvec_in_mvec": {
      "answer": "@100 = #(4); @200 = #(1 2 @100)",
      "distractors": [
        "@100 = #(4); @200 = #(1 2 #(3))",
        "@100 = #(4); @200 = #(1 2 #(4))",
        "@100 = #(4); @200 = #(1 2 3)",
        "@100 = #(4); @200 = #(1 2 4)",
        "@100 = #(3); @200 = #(1 2 @300); @300 = #(4)",
        "@100 = #(3); @200 = #(1 2 @100)"
      ],
      "feedback": "%1,2% are wrong because vectors refer values.\n`#(3)` and `#(4)` are not values, although\nthey can be the printed representation of a vector.\n\n%3,4% are wrong because the 2-th element of the 3-element vector must be a vector.\nThe 3-element vector is created by `(mvec 1 2 x)`.\nThe value of `x` is a vector at that moment.\nThis 3-element vector is never mutated.\n\n%5% is wrong because only two vectors are created.\nThere must be at most two vectors on the heap.\n\n%6% can be the heap after the two vectors are created.\nHowever, the subsequent mutation changes the shorter vector.\nSo, %0% is the correct answer.\n",
      "program": "(defvar x (mvec 3))\n(defvar v (mvec 1 2 x))\n(vec-set! x 0 4)\nv\n",
      "prompt": "Which choice best describes the heap at the end of the following program?\n"
    },
    "basic_circularity": {
      "answer": "@100 = #(1 @100 2)",
      "distractors": [
        "@100 = #(1 0 2)",
        "@100 = #(1 @200 2); @200 = #(1 0 2)",
        "@100 = #(1 #(1 0 2) 2)"
      ],
      "feedback": "%3% is wrong because vectors refer values.\n`#(1 0 2)` is not a value, although\nit can be some vector values printed.\n\n%2% is wrong because only one vector is created.\nThere must not be two vectors on the heap.\n\n%1% can be the heap after the vector is created.\nBut the subsequent mutation replaces the 1-th element of `@100` with `@100`.\nSo, %0% is the correct answer.\n",
      "program": "(defvar x (mvec 1 0 2))\n(vec-set! x 1 x)\n(vlen x)\n",
      "prompt": "Which choice best describes the heap at the end of the following program?\n"
    },
    "intro_even_more_on_mvec": {
      "confirm": "In this tutorial, we will learn even more about **mutable values**, illustrated with **vectors**.\n"
    },
    "mvecs_are_mvec": {
      "answer": "@100 = #(4 5)",
      "distractors": [
        "@100 = #(4 5); @200 = 4",
        "@100 = #(@200 @300); @200 = 4; @300 = 5"
      ],
      "feedback": "For now, the heap maps addresses only to vectors.\nThis rules out %1,2%.\n\nSo, %0% is correct.\n",
      "program": "(defvar mv (mvec 4 5))\n(vec-ref mv 0)\n",
      "prompt": "Which choice best describes the heap at the end of the following program?\n"
    },
    "new_alias_mvec_in_mvec": {
      "answer": "@100 = #(6 2 3); @200 = #(@100 @100)",
      "distractors": [
        "@100 = #(6 2 3); @200 = #(#(1 2 3) #(6 2 3))",
        "@100 = #(1 2 3); @200 = #(#(1 2 3) #(6 2 3))",
        "@100 = #(1 2 3); @200 = #(@100 @300); @300 = #(6 2 3)",
        "@100 = #(1 2 3); @200 = #(1 2 3); @300 = #(6 2 3); @400 = #(@200 @300)",
        "@100 = #(1 2 3); @200 = #(@100 @100)",
        "@100 = #(1 2 3); @200 = #(6 @100)"
      ],
      "feedback": "Vectors refer values (e.g., `1` and `@200`).\nThis rules out %1,2%.\n\nTwo vectors are created.\nSo, there must be two vectors on the heap.\nThis rules out %3,4%.\n\n`vv` is bound to a 2-element vector, and the\n1-th element of the 2-element vector must be the 3-element vector.\nThe mutation replaces the 0-th element in the 3-element vector with `6`.\nSo, %0% is the correct answer, while\n%5% does not reflect the effect of the mutation, and %6% mutates the wrong vector.\n",
      "program": "(defvar v (mvec 1 2 3))\n(defvar vv (mvec v v))\n(vec-set! (vec-ref vv 1) 0 6)\n(vec-ref vv 0)\n",
      "prompt": "Which choice best describes the heap at the end of the following program?\n"
    },
    "vector_not_flatten": {
      "answer": "@100 = #(1 @200); @200 = #(3 4)",
      "distractors": [
        "@100 = #(1 2)",
        "@100 = #(1 2); @200 = #(3 4)",
        "@100 = #(@200 1); @200 = #(3 4)",
        "@100 = #(1 #(3 4)); @200 = #(3 4)",
        "@100 = #(1 #(3 4))"
      ],
      "feedback": "Two vectors are created. So, there must be at least two things on the heap.\nThis rules out %1,5%.\n\n%4% is wrong because vectors refer values (e.g., `1`, `2`, and `@200`).\n`#(3 4)` is not itself a value; it's the *printing* of the value that resides at `@200`.\n%2% can be the heap after the second vector is created.\nHowever, the subsequent mutation changes `@100`.\nSo, %2% is wrong.\n\nThe mutation replaces the 1-th (i.e., second) element rather than the 0-th element,\nso %0% is correct, while %3% is wrong.\n",
      "program": "(defvar m (mvec 1 2))\n(vec-set! m 1 (mvec 3 4))\n",
      "prompt": "Which choice best describes the heap at the end of the following program?\n"
    },
    "warmup_vector": {
      "answer": "@200 = #(33)",
      "distractors": [
        "@200 = #(2)",
        "@200 = 33",
        "@100 = #(2); @200 = #(33)"
      ],
      "feedback": "Exactly one vector was created.\nSo, there must be at most one vector on the heap.\nThis rules out %3%.\n\nFor now, the heap maps addresses only to vectors.\nThis rules out %2%.\n\nThe heap looks like %1% after evaluating `(mvec 2)`.\nHowever, the subsequent mutation changes the vector.\nSo, the correct answer is %0%.\n",
      "program": "(defvar x (mvec 2))\n(vec-set! x 0 33)\nx\n",
      "prompt": "Which choice best describes the heap at the end of the following program?\n\n(**Note**: we use `@ddd` (e.g., `@123`, `@200`, and `@100`) to represent heap addresses.\nHeap addresses are *random*. The numbers don't mean anything.)\n"
    }
  }
},
"mutvars1":
{
  "misconceptions": [
    "DefByRef",
    "CallByRef",
    "StructByRef",
    "IsolatedFun"
  ],
  "order": [
    "intro_mutvars",
    "warmup_set",
    [
      "not_aliased_by_defvar_1",
      "not_aliased_by_defvar_2",
      "not_aliased_by_funarg_2",
      "new_not_aliased_by_funarg",
      "alias_var_in_mvec",
      "new_alias_mvec_in_mvec_trick"
    ],
    "reflect_mutvars_1",
    "goal_mutvars_1",
    "goal_mutvars_1_labelling"
  ],
  "questions": {
    "alias_var_in_mvec": {
      "answer": "#(68 57 59)",
      "feedback": "`v` is bound to the vector created by `(mvec 68 57 x)`.\n`(mvec 68 57 x)` is a function call.\n`x` is evaluated at the point of evaluating `(mvec 68 57 x)`.\nThat is why the vector's content is `68`, `57`, and `59`.\nThe subsequent variable assignment mutates `x`.\nBut this doesn't impact the vector\nbecause the vector refers the value `59` rather than `x`.\n",
      "program": "(defvar x 59)\n(defvar v (mvec 68 57 x))\n(set! x 74)\nv\n",
      "again": {
        "answer": "#(82 66 93)",
        "program": "(defvar x 66)\n(defvar v (mvec 82 x 93))\n(set! x 1)\nv\n"
      },
      "misconceptions": {
        "#(68 57 74)": {
          "feedback": "You might think the `2`-th and last element of the vector refers to `x`.\nHowever, vectors refer to values, so it actually refers to `59`, i.e., *the value of* `x`.\nIn SMoL, variable assignments change *only* the mutated variables.\n",
          "misconception": "StructByRef"
        }
      }
    },
    "goal_mutvars_1": {
      "emphasize": "Variable assignments change *only* the mutated variables. That is, variables are not aliased.\n\n(Note: some programming languages (e.g., C++ and Rust) allow variables to be aliased.\nHowever, even in those languages, variables are not aliased by default.)\n"
    },
    "goal_mutvars_1_labelling": {
      "select_and_comment": "Please scroll back and select 1-3 programs that make the above point.\n\nYou don't need to select *all* such programs.\n"
    },
    "intro_mutvars": {
      "confirm": "In this tutorial, we will learn about **variable assignments** and **mutable variables**.\n\nThe following program illustrates the new concepts.\n\n```\n(defvar x 2)\nx\n(set! x (+ x 1))\nx\n(set! x (* x 2))\nx\n```\n\nThis program produces `2 3 6`.\nIt first defines `x` and binds `x` to `2`.\nThe first variable assignment `(set! x (+ x 1))` **mutates** (the binding of) `x`.\nAfter that, `x` is bound to the value of `(+ x 1)`; this uses the new value of `x`, which is `2`, resulting in `(+ 2 1)`, which is `3`.\nThe next variable assignment again mutates `x` and binds `x` to the value of `(* 3 2)`, which is `6`.\n"
    },
    "new_alias_mvec_in_mvec_trick": {
      "answer": "#(#(55) 55 55)",
      "feedback": "`x` is first bound to a one-element vector.\n`v` is bound to a three-element vector, of which the first element is the one-element vector.\n`(set! x (mvec 66))` binds `x` to a new vector.\nThis doesn't impact `v` because\nthe 0-th element of `v` is still the one-element vector.\n",
      "program": "(defvar x (mvec 55))\n(defvar v (mvec x 55 55))\n(set! x (mvec 66))\nv\n",
      "again": {
        "answer": "#(2 #(0) 3)",
        "program": "(defvar x (mvec 0))\n(defvar v (mvec 2 x 3))\n(set! x (mvec 1))\nv\n"
      },
      "misconceptions": {
        "#(#(66) 55 55)": {
          "feedback": "You might think the `0`-th and first element of `v` refers to `x`.\nHowever, vectors refer to values, so it actually refers to the one-element vector, i.e., *the value of* `x`.\nIn SMoL, variable assignments change *only* the mutated variables.\n",
          "misconception": "StructByRef"
        }
      }
    },
    "new_not_aliased_by_funarg": {
      "answer": "12",
      "feedback": "The function call binds `y` to `12`.\nThe variable assignment mutates the value of `x` to `0`, but `y` is still bound to `12`.\n",
      "program": "(defvar x 12)\n(deffun (f y)\n  (set! x 0)\n  y)\n(f x)\n",
      "again": {
        "answer": "1 2",
        "program": "(defvar a 1)\n(deffun (foobar b)\n  (set! a 2)\n  b)\n(foobar a)\na\n"
      },
      "misconceptions": {
        "0": {
          "feedback": "You might think the function call `(f x)` binds `y` to `x`, so\nchanging `x` will change `y`.\nHowever, we learned in previous tutorials that variables are bound to values, so `y` is bound to `12`, i.e., *the value of* `x`.\nIn SMoL, variable assignments change *only* the mutated variables.\n",
          "misconception": "CallByRef"
        },
        "error": {
          "feedback": "You might think `(set! x 0)` can't refer to `x`.\nHowever, `(set! x 0)` appears in the body of `g`, which is a sub-block of the top-level block,\nso it can mutate the top-level `x`.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "IsolatedFun"
        }
      }
    },
    "not_aliased_by_defvar_1": {
      "answer": "0 12",
      "feedback": "The first definition binds `x` to `12`.\nThe second definition binds `y` to the value of `x`, which is `12`.\nThe variable assignment mutates the binding of `x`, so `x` is now bound to `0`.\nBut `y` is still bound to `12`.\n",
      "program": "(defvar x 12)\n(defvar y x)\n(set! x 0)\nx\ny\n",
      "again": {
        "answer": "2 1",
        "program": "(defvar a 1)\n(defvar b a)\n(set! a 2)\na\nb\n"
      },
      "misconceptions": {
        "0 0": {
          "feedback": "You might think `(defvar y x)` binds `y` to `x`, so changing `x` will change `y`.\nHowever, we learned in previous tutorials that variables are bound to values, so `y` is bound to the `12`, i.e., *the value of* `x`.\nIn SMoL, variable assignments change *only* the mutated variables.\n",
          "misconception": "DefByRef"
        }
      }
    },
    "not_aliased_by_defvar_2": {
      "answer": "40 22",
      "feedback": "`m` and `n` are bound to `40`.\nThe variable assignment mutates the binding of `n`.\nSo, `n` is eventually bound to `22`.\n",
      "program": "(defvar m 40)\n(defvar n m)\n(set! n 22)\nm\nn\n",
      "again": {
        "answer": "1 2",
        "program": "(defvar zzz 1)\n(defvar abc zzz)\n(set! abc 2)\nzzz\nabc\n"
      },
      "misconceptions": {
        "22 22": {
          "feedback": "You might think `(defvar n m)` binds `n` to `m`, so changing `m` will change `n`.\nHowever, we learned in previous tutorials that variables are bound to values, so `n` is bound to the `40`, i.e., *the value of* `m`.\nIn SMoL, variable assignments change *only* the mutated variables.\n",
          "misconception": "DefByRef"
        }
      }
    },
    "not_aliased_by_funarg_2": {
      "answer": "12",
      "feedback": "The function call binds `y` to `12`.\nThe variable assignment mutates the value of `y` to `0`, but `x` is still bound to `12`.\n",
      "program": "(defvar x 12)\n(deffun (f y)\n  (set! y 0)\n  x)\n(f x)\n",
      "again": {
        "answer": "1",
        "program": "(defvar s 1)\n(deffun (foobar t)\n  (set! t 2)\n  s)\n(foobar s)\n"
      },
      "misconceptions": {
        "0": {
          "feedback": "You might think the function call `(f x)` binds `y` to `x`, so\nchanging `y` will change `x`.\nHowever, we learned in previous tutorials that variables are bound to values, so `y` is bound to `12`, i.e., *the value of* `x`.\nIn SMoL, variable assignments change *only* the mutated variables.\n",
          "misconception": "CallByRef"
        },
        "error": {
          "feedback": "You might think `f` can't refer to `x`.\nHowever, `f` is defined in the top-level block,\nso its body can refer to the top-level `x`.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "IsolatedFun"
        }
      }
    },
    "reflect_mutvars_1": {
      "prompt": "What did you learn about variable assignment from these programs?\n"
    },
    "warmup_set": {
      "answer": "20",
      "feedback": "`rent` is bound to `10` initially.\nThe `set!` mutates `rent` to `20`.\nSo, when we print the value of `rent` after the `set!`,\nwe see `20`.\n",
      "program": "(defvar rent 10)\n(set! rent (* 10 2))\nrent\n",
      "again": {
        "answer": "25",
        "program": "(defvar discount 10)\n(set! discount (+ 10 15))\ndiscount\n"
      }
    }
  }
},
"mutvars2":
{
  "misconceptions": [
    "DeepClosure",
    "DefOrSet",
    "IsolatedFun"
  ],
  "order": [
    "intro_more_on_mutvars",
    [
      "update_undefined",
      "new_fun_do_not_rem_val",
      "new_fun_rem_env"
    ],
    "reflect_mutvars_2",
    "goal_mutvars_2",
    "goal_mutvars_2_labelling_1",
    "goal_mutvars_2_labelling_2",
    "reflect_env",
    "goal_env",
    "attention_check1",
    "attention_check2",
    "keyframe_mutable_variables_1",
    "keyframe_mutable_variables_2"
  ],
  "questions": {
    "attention_check1": {
      "checkboxes": [
        [
          true,
          "Definitions"
        ],
        [
          false,
          "Variable mutations (e.g., `(set! x 3)`)"
        ],
        [
          false,
          "Variable references"
        ],
        [
          true,
          "Function calls"
        ]
      ],
      "feedback": "Both definitions and function calls create bindings.\nVariable assignment mutates existing bindings but doesn't create new bindings.\n",
      "prompt": "Which language construct(s) create new bindings?"
    },
    "attention_check2": {
      "checkboxes": [
        [
          false,
          "Definitions"
        ],
        [
          true,
          "Variable mutations (e.g., `(set! x 3)`)"
        ],
        [
          false,
          "Variable references"
        ],
        [
          false,
          "Function calls"
        ]
      ],
      "feedback": "Only variable assignments mutate bindings.\nDefinitions and function calls create new bindings but don't mutate existing bindings.\n",
      "prompt": "Which language construct(s) mutate bindings?"
    },
    "goal_env": {
      "emphasize": "**Environments** (rather than blocks) bind variables to values.\n\nSimilar to vectors, environments are created as programs run.\n\nEnvironments are created from blocks.\nThey form a tree-like structure,\nrespecting the tree-like structure of their corresponding blocks.\nSo, we have a primordial environment, a top-level environment,\nand environments created from function bodies.\n\n*Every function call creates a new environment.* This is very different from\nthe block perspective: every function corresponds to exactly one block, its body.\n"
    },
    "goal_mutvars_2": {
      "emphasize": "- Variable assignments mutate existing bindings and do not create new bindings.\n- Functions refer to the latest values of variables defined outside their definitions. That is, functions do not remember the values of those variables from when the functions were defined.\n"
    },
    "goal_mutvars_2_labelling_1": {
      "select_and_comment": "Please scroll back and select 1-3 programs that make the following point:\n\n> Variable assignments mutate existing bindings and do not create new bindings.\n\nYou don't need to select *all* such programs.\n"
    },
    "goal_mutvars_2_labelling_2": {
      "select_and_comment": "Please scroll back and select 1-3 programs that make the following point:\n\n> Functions refer to the latest values of variables defined outside their definitions. That is, functions do not remember the values of those variables from when the functions were defined.\n\nYou don't need to select *all* such programs.\n"
    },
    "intro_more_on_mutvars": {
      "confirm": "In this tutorial, we will learn more about **variable assignments** and **mutable variables**.\n"
    },
    "keyframe_mutable_variables_1": {
      "prompt": "Here is a program that confused many students\n\n```\n(defvar x 5)\n(deffun (f x y)\n  (set! x y))\n(f x 6)\nx\n```\n\nPlease\n\n1. Run this program in the stacker by clicking the green run button above;\n2. The stacker would show how this program produces its result(s);\n3. Keep clicking <button>⏭ Next</button> until you reach a configuration that you find particularly helpful;\n4. Click <button>🔗 Share This Configuration</button> to get a link to your configuration;\n5. Submit your link below;\n"
    },
    "keyframe_mutable_variables_2": {
      "prompt": "Please write a couple of sentences to explain how your configuration explains the result(s) of the program.\n"
    },
    "new_fun_do_not_rem_val": {
      "answer": "32",
      "feedback": "The program binds `x` to `1` and then defines a function `f`.\n`x` is then bound to `2`. So, `(f 30)` is `(+ x 30)`, which is `32`.s\n",
      "program": "(defvar x 1)\n(deffun (f n)\n  (+ x n))\n(set! x 2)\n(f 30)\n",
      "again": {
        "answer": "14",
        "program": "(defvar o 1)\n(deffun (u t)\n  (+ o t))\n(set! o 9)\n(u 5)\n"
      },
      "misconceptions": {
        "31": {
          "feedback": "You are right that `x` is bound to `1` when `f` is bound to a function.\nYou might think the function remembers the value `1`.\nHowever, `f` does not remember the value of `x`. Rather, it always\nrefers to the latest value of `x`.\nIn SMoL, functions refer to the latest values of variables defined outside their definitions.\n",
          "misconception": "DeepClosure"
        },
        "error": {
          "feedback": "You might think `(+ x n)` can't refer to `x`.\nHowever, `(+ x n)` appears in the body of `f`, which is a sub-block of\nthe top-level block. So, it can refer to the `x` defined\nin the top-level block.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "IsolatedFun"
        }
      }
    },
    "new_fun_rem_env": {
      "answer": "0 1",
      "feedback": "There is exactly one variable `x`.\nThe `x` in `(set! x 0)` refers to that variable.\nCalling `f` evaluates `(set! x 0)`, which mutates `x`.\nWhen the value of `x` is eventually printed, we see the new value.\n",
      "program": "(defvar x 12)\n(deffun (f)\n  x)\n(deffun (g)\n  (set! x 0)\n  (f))\n(g)\n(set! x 1)\n(f)\n",
      "again": {
        "answer": "2 6",
        "program": "(defvar a 4)\n(deffun (h)\n  a)\n(deffun (k)\n  (set! a 2)\n  (h))\n(k)\n(set! a 6)\n(h)\n"
      },
      "misconceptions": {
        "12 1": {
          "feedback": "You might think `(set! x 0)` defines `x`.\nHowever, it will mutate the variable defined outside the functions.\nIn SMoL, variable assignments mutate existing bindings and never create new bindings.\n",
          "misconception": "DefOrSet"
        },
        "12 12": {
          "feedback": "You are right that `x` is bound to `12` when `f` is bound to a function.\nYou might think the function remembers the value `12`.\nHowever, `f` does not remember the value of `x`. Rather, it always\nrefers to the latest value of `x`.\nIn SMoL, functions refer to the latest values of variables defined outside their definitions.\n",
          "misconception": "DeepClosure"
        },
        "error": {
          "feedback": "You might think `f` can't refer to `x`.\nHowever, `f` is defined in the top-level block.\nSo, it can refer to the `x` defined in the top-level block.\n\nYou might think `(set! x 0)` can't refer to `x`.\nHowever, `(set! x 0)` appears in the body of `g`, which is a sub-block of\nthe top-level block. So, it can refer to the `x` defined\nin the top-level block.\n\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "IsolatedFun"
        }
      }
    },
    "reflect_env": {
      "prompt": "Although we keep saying \"this variable is mutated\", the variables themselves are *not* mutated.\nWhat is actually mutated is *the binding between the variables and their values*.\n\nBlocks have been a good way to describe these bindings.\nHowever, they can't explain variable mutations: a block is a piece of source code,\nand we can't change the source code by mutating a variable. Besides,\nblocks can't explain how parameters might be bound differently in different function calls.\nConsider the following program:\n\n```\n(deffun (f n)\n  (+ n 1))\n(f 2)\n(f 3)\n```\n\nIn this program, `n` is bound to `2` in this first function call, and `3` in the second.\nBlocks can't explain how `n` is bound differently because the two calls share the same\nblock: the body of `f`.\n\nWhat is a better way to describe the binding between variables and their values?\n"
    },
    "reflect_mutvars_2": {
      "prompt": "What did you learn about variable assignment from these programs?\n"
    },
    "update_undefined": {
      "answer": "error",
      "feedback": "Mutating an undefined variable (in this case, `foobar`) errors rather than defining the variable.\nSo, this program errors.\n",
      "program": "(set! foobar 2)\nfoobar\n",
      "again": {
        "answer": "error",
        "program": "(set! foo 42)\nfoo\n"
      },
      "misconceptions": {
        "2": {
          "feedback": "You might think `(set! foobar 2)` defines `foobar`.\nHowever, it errors.\nIn SMoL, variable assignments mutate existing bindings and never create new bindings.\n",
          "misconception": "DefOrSet"
        }
      }
    }
  }
},
"lambda1":
{
  "misconceptions": [
    "FunNotVal"
  ],
  "order": [
    "intro_lambda",
    [
      "fun_as_val",
      "fun_as_parameter",
      "new_fun_as_output",
      "fun_in_vectors"
    ],
    "reflect_funval",
    "goal_funval",
    "goal_funval_labelling"
  ],
  "questions": {
    "fun_as_parameter": {
      "answer": "4",
      "feedback": "The value of `(twice double 1)` is the value of `(f (f x))`, where\n`f` is bound to `double` and `x` is bound to `1`.\nSo, the result is the value of `(double (double 1))`, which is `4`.\n",
      "program": "(deffun (twice f x)\n  (f (f x)))\n(deffun (double x)\n  (+ x x))\n(twice double 1)\n",
      "again": {
        "answer": "3",
        "program": "(deffun (ffx f x)\n  (f (f x)))\n(deffun (inc x)\n  (+ x 1))\n(ffx inc 1)\n"
      },
      "misconceptions": {
        "error": {
          "feedback": "You might think that `(twice double 1)` errors because `double` is bound to a function,\nand that it would not error if `double` is bound to a value of other kinds (e.g., numbers and vectors).\nHowever, it does not error.\nIn SMoL, functions are (also) *first-class* citizens of the value world.\n",
          "misconception": "FunNotVal"
        }
      }
    },
    "fun_as_val": {
      "answer": "42",
      "feedback": "This program binds `f` to a function that returns `42`,\nand then binds `g` and `h` to that function.\nFinally, calling that function produces `42`.\n",
      "program": "(deffun (f)\n  42)\n(defvar g f)\n(defvar h g)\n(h)\n",
      "again": {
        "answer": "50",
        "program": "(deffun (f1)\n  5)\n(defvar f2 f1)\n(defvar f3 f2)\n(* (f3) 10)\n"
      },
      "misconceptions": {
        "error": {
          "feedback": "You might think that `(defvar g f)` errors because `f` is bound to a function,\nand that it would not error if `f` is bound to a value of other kinds (e.g., numbers and vectors).\nHowever, it does not error.\nIn SMoL, functions are (also) *first-class* citizens of the value world.\n",
          "misconception": "FunNotVal"
        }
      }
    },
    "fun_in_vectors": {
      "answer": "3",
      "feedback": "`v` is bound to a vector that refers to the function `inc`.\nThe value of `(vec-ref v 0)` is the function `inc`.\nSo, the value of `((vec-ref v 0) 2)` is the value of `(inc 2)`,\nwhich is `3`.\n",
      "program": "(deffun (inc n)\n  (+ n 1))\n(defvar v (mvec inc))\n((vec-ref v 0) 2)\n",
      "again": {
        "answer": "1",
        "program": "(deffun (inc n)\n  (+ n 1))\n(deffun (dec n)\n  (- n 1))\n(defvar v (mvec inc dec))\n((vec-ref v 1) 2)\n"
      },
      "misconceptions": {
        "error": {
          "feedback": "You might think that `(mvec inc)` errors because `inc` is bound to a function,\nand that it would not error if `inc` is bound to a value of other kinds (e.g., numbers and vectors).\nHowever, it does not error.\nIn SMoL, functions are (also) *first-class* citizens of the value world.\n",
          "misconception": "FunNotVal"
        }
      }
    },
    "goal_funval": {
      "emphasize": "Functions are (also) *first-class* citizens of the value world. Specifically,\n\n- Variables (notably parameters) can be bound to functions,\n- Functions can return functions, and\n- Vectors can refer to functions.\n"
    },
    "goal_funval_labelling": {
      "select_and_comment": "Please scroll back and select 1-3 programs that make the point above.\n\nYou don't need to select *all* such programs.\n"
    },
    "intro_lambda": {
      "confirm": "In this tutorial, we will learn about using functions as values.\n\nNote: some programming languages do NOT consider functions or methods a kind of value.\nBut many programming languages, from Python to Rust, do.\n"
    },
    "new_fun_as_output": {
      "answer": "11",
      "feedback": "`inc` is bound to a function that adds 1 to its parameter.\n`g` is bound to a function that returns `inc`.\n`f` is bound to the value of `(g)`, which is `inc`.\nSo, the value of `(f 10)` is `11`.\n",
      "program": "(deffun (inc x)\n  (+ x 1))\n(deffun (g)\n  inc)\n(defvar f (g))\n(f 10)\n",
      "again": {
        "answer": "30",
        "program": "(deffun (fun1)\n  (deffun (average x y)\n    (/ (+ x y) 2))\n  average)\n(defvar x (fun1))\n(x 20 40)\n"
      },
      "misconceptions": {
        "error": {
          "feedback": "You might think that `(g)` errors because it returns a function,\nand that it would not error if it returns a value of other kinds (e.g., numbers and vectors).\nHowever, it does not error.\nIn SMoL, functions are (also) *first-class* citizens of the value world.\n",
          "misconception": "FunNotVal"
        }
      }
    },
    "reflect_funval": {
      "prompt": "What did you learn about functions from these programs?\n"
    }
  }
},
"lambda2":
{
  "misconceptions": [
    "FlatEnv",
    "DeepClosure"
  ],
  "order": [
    "intro_more_funval",
    [
      "fun_ref_env",
      "new_state",
      "counter"
    ],
    "reflect_closure",
    "goal_closure",
    "label_closure"
  ],
  "questions": {
    "counter": {
      "answer": "1 2 1",
      "feedback": "Every time `foo` is called, it creates a *new* environment frame that binds `n`.\nSo, `f` and `g` have different bindings for the `n` variable.\nWhen `f` is called the first time, it mutates its binding for the `n` variable.\nSo, the second call to `f` produces `2` rather than `1`.\n`g` has its own binding for the `n` variable, which still binds `n` to `0`.\nSo, `(g)` produces `1` rather than `3`.\n",
      "program": "(deffun (foo)\n  (defvar n 0)\n  (deffun (bar)\n    (set! n (+ n 1))\n    n)\n  bar)\n(defvar f (foo))\n(defvar g (foo))\n\n(f)\n(f)\n(g)\n",
      "again": {
        "answer": "2 2 4",
        "program": "(deffun (f n)\n  (deffun (dbl)\n    (set! n (* n 2))\n    n)\n  dbl)\n(defvar dbl1 (f 1))\n(defvar dbl2 (f 1))\n\n(dbl1)\n(dbl2)\n(dbl1)\n"
      },
      "misconceptions": {
        "1 1 1": {
          "feedback": "You are right that `n` is bound to `0` when `bar` is bound to a function.\nYou might think the function remembers the value `0`.\nHowever, `bar` does not remember the value of `n`. Rather, it remembers the environment and hence always refers to the latest value of `n`.\n`foo` is called twice, so two environments are created.\n`(f)` mutates the first, while `(g)` mutates the second.\nIn SMoL, functions refer to the latest values of variables defined outside their definitions.\n",
          "misconception": "DeepClosure"
        },
        "1 2 3": {
          "feedback": "`foo` is called twice.\nThe first call binds `n` to `0`.\nThe second call also binds `n` to `0`.\nYou might think the second binding applies everywhere.\nHowever, it only applies to the second call.\n`(f)` mutates the first binding, while `(g)` mutates the second.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "FlatEnv"
        },
        "error": {
          "misconception": "DefOrSet,FunNotVal"
        }
      }
    },
    "fun_ref_env": {
      "answer": "4 6",
      "feedback": "The value of `(bar 2)` is the function `addy` defined in an\nenvironment where `y` is bound to `2`.\nThe value of `(bar 4)` is *another* `addy` defined in an\nenvironment where `y` is bound to `4`.\nThe two `addy` functions are *different* values.\nSo, the value of `(f 2)` is `4`,\nwhile the value of `(g 2)` is `6`.\n",
      "program": "(deffun (bar y)\n  (deffun (addy x)\n    (+ x y))\n  addy)\n(defvar f (bar 2))\n(defvar g (bar 4))\n(f 2)\n(g 2)\n",
      "again": {
        "answer": "40 4",
        "program": "(deffun (f n)\n  (deffun (g m)\n    (* m n))\n  g)\n(defvar fun1 (f 10))\n(defvar fun2 (f 1))\n(fun1 4)\n(fun2 4)\n"
      },
      "misconceptions": {
        "6 6": {
          "feedback": "`bar` is called twice.\nThe first call binds `y` to `2`.\nThe second call binds `y` to `4`.\nYou might think the second binding applies everywhere.\nHowever, it only applies to the second call.\nThe `addy` that `f` refers to is created in the first call, so `(f 2)` adds `2` rather than `4`.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "FlatEnv"
        },
        "error": {
          "feedback": "You might think that `(bar 2)` and `(bar 4)` errors because they return functions,\nand that they would not error if they return values of other kinds (e.g., numbers and vectors).\nHowever, they do not error.\n\nYou might think that the two variable definitions error because the variables are bound to functions,\nand that they would not error if the variables were bound to values of other kinds (e.g., numbers and vectors).\nHowever, they do not error.\n\nIn SMoL, functions are (also) *first-class* citizens of the value world.\n",
          "misconception": "FunNotVal"
        }
      }
    },
    "goal_closure": {
      "emphasize": "Functions remember the environment in which they are defined.\nThat is, function bodies are \"enclosed\" by the environments in which the function values are created.\nSo, function values are called *closures*.\n"
    },
    "intro_more_funval": {
      "confirm": "In this tutorial, we will learn *more* about using functions as values.\n"
    },
    "label_closure": {
      "select_and_comment": "Please scroll back and select 1-3 programs that make the point that\n\n> Functions remember the environment in which they are defined.\n\nYou don't need to select *all* such programs.\n"
    },
    "new_state": {
      "answer": "2",
      "feedback": "`x` is bound to `1`.\n`g` is bound to the function `addx`\n`(set! x 2)` binds `x` to `2`.\nSo, the value of `(g 0)` is the value of `(addx 0)`, which is the value of `(+ 2 0)`, which is `2`.\n",
      "program": "(defvar x 1)\n(deffun (f)\n  (deffun (addx y)\n    (+ x y))\n  addx)\n(defvar g (f))\n(set! x 2)\n(g 0)\n",
      "again": {
        "answer": "2",
        "program": "(defvar a 2)\n(deffun (make)\n  (deffun (f b)\n    (+ a b))\n  f)\n(defvar g (make))\n(set! a 1)\n(g 1)\n"
      },
      "misconceptions": {
        "1": {
          "feedback": "You are right that `x` is bound to `1` when `addx` is created.\nYou might think the function remembers the value `1`.\nHowever, `addx` does not remember the value of `x`.  Rather, it remembers the environment and hence always refers to the latest value of `n`.\nIn SMoL, functions refer to the latest values of variables defined outside their definitions.\n",
          "misconception": "DeepClosure"
        },
        "error": {
          "feedback": "You might think that `(f)` errors because it returns a function,\nand that it would not error if it returns a value of other kinds (e.g., numbers and vectors).\nHowever, it does not error.\n\nYou might think that `(defvar g (f))` errors because `g` is bound to a function,\nand that it would not error if `g` is bound to a value of other kinds (e.g., numbers and vectors).\nHowever, it does not error.\n\nIn SMoL, functions are (also) *first-class* citizens of the value world.\n",
          "misconception": "FunNotVal"
        }
      }
    },
    "reflect_closure": {
      "prompt": "What did you learn about functions from these programs?"
    }
  }
},
"lambda3":
{
  "misconceptions": [
    "FlatEnv",
    "DeepClosure"
  ],
  "order": [
    "intro_lambda",
    "warmup_lambda",
    [
      "lambda_fun_ref_env",
      "lambda_new_state",
      "lambda_counter"
    ],
    "goal_lambda",
    "practice_translate",
    "keyframe_counter_1",
    "keyframe_counter_2"
  ],
  "questions": {
    "goal_lambda": {
      "emphasize": "`(deffun (f x y z) body)` is a shorthand for `(defvar f (lambda (x y z) body))`.\n"
    },
    "intro_lambda": {
      "emphasize": "In this tutorial, we will learn about **lambda expressions**, which are expressions that create functions.\n\nThe following program illustrates how to create a function.\n\n```\n((lambda (n)\n   (+ n 1))\n 2)\n```\n\nThis program program produces `3`. The top-level block contains one expression, a function call. The only (actual) parameter of the function call is `2`. The function of the function call is created by\n\n```\n(lambda (n)\n  (+ n 1))\n```\n\nThis function takes only one parameter `n`, and returns (the value of) `(+ n 1)`.\nSo, the result of the whole program is the value of `(+ 2 1)`, which is `3`.\n"
    },
    "keyframe_counter_1": {
      "prompt": "Here is a program that confused many students\n\n```\n(deffun (foo n)\n  (deffun (bar)\n    (set! n (+ n 1))\n    n)\n  bar)\n(defvar f (foo 0))\n(defvar g (foo 0))\n\n(f)\n(f)\n(g)\n```\n\nPlease\n\n1. Run this program in the stacker by clicking the green run button above;\n2. The stacker would show how this program produces its result(s);\n3. Keep clicking <button>⏭ Next</button> until you reach a configuration that you find particularly helpful;\n4. Click <button>🔗 Share This Configuration</button> to get a link to your configuration;\n5. Submit your link below;\n"
    },
    "keyframe_counter_2": {
      "prompt": "Please write a couple of sentences to explain how your configuration explains the result(s) of the program.\n"
    },
    "lambda_counter": {
      "answer": "1 2 1",
      "feedback": "Every time `foobar` is called, it creates a *new* environment that binds `n`.\nSo, `f` and `g` have different bindings for the `n` variable.\nWhen `f` is called the first time, it mutates its binding for the `n` variable.\nSo, the second call to `f` produces `2` rather than `1`.\n`g` has its own binding for the `n` variable, which still binds `n` to `0`.\nSo, `(g)` produces `1` rather than `3`.\n",
      "program": "(deffun (foobar)\n  (defvar n 0)\n  (lambda ()\n    (set! n (+ n 1))\n    n))\n(defvar f (foobar))\n(defvar g (foobar))\n\n(f)\n(f)\n(g)\n",
      "again": {
        "answer": "2 2 4",
        "program": "(deffun (build-dbl)\n  (defvar n 1)\n  (lambda ()\n    (set! n (* n 2))\n    n))\n(defvar dbl1 (build-dbl))\n(defvar dbl2 (build-dbl))\n\n(dbl1)\n(dbl2)\n(dbl1)\n"
      },
      "misconceptions": {
        "1 1 1": {
          "feedback": "You are right that `n` is bound to `0` when the lambda function is created.\nYou might think the function remembers the value `0`.\nHowever, the lambda function does not remember the value of `n`. Rather, it remembers the environment and hence always refers to the latest value of `n`.\n`foobar` is called twice, so its parameter, `n`, is bound twice.\n`(f)` mutates the first binding, while `(g)` mutates the second.\nIn SMoL, functions refer to the latest values of variables defined outside their definitions.\n",
          "misconception": "DeepClosure"
        },
        "1 2 3": {
          "feedback": "`foobar` is called twice.\nThe first call binds `n` to `0`.\nThe second call also binds `n` to `0`.\nYou might think the second binding applies everywhere.\nHowever, it only applies to the second call.\n`(f)` mutates the first binding, while `(g)` mutates the second.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "FlatEnv"
        },
        "error": {
          "misconception": "DefOrSet,FunNotVal"
        }
      }
    },
    "lambda_fun_ref_env": {
      "answer": "4 6",
      "feedback": "The value of `(bar 2)` is a lambda function defined in an\nenvironment where `y` is bound to `2`.\nThe value of `(bar 4)` is *another* lambda function defined in an\nenvironment where `y` is bound to `4`.\nThe two lambda functions are *different* values.\nSo, the value of `(f 2)` is `12`,\nwhile the value of `(g 2)` is `52`.\n",
      "program": "(deffun (bar y)\n  (lambda (x)\n    (+ x y)))\n(defvar f (bar 2))\n(defvar g (bar 4))\n(f 2)\n(g 2)\n",
      "again": {
        "answer": "40 4",
        "program": "(deffun (f n)\n  (lambda (m)\n    (* m n)))\n(defvar fun1 (f 10))\n(defvar fun2 (f 1))\n(fun1 4)\n(fun2 4)\n"
      },
      "misconceptions": {
        "6 6": {
          "feedback": "`bar` is called twice.\nThe first call binds `y` to `2`.\nThe second call binds `y` to `4`.\nYou might think the second binding applies everywhere.\nHowever, it only applies to the second call.\nThe lambda function that `f` refers to is created in the first call, so `(f 2)` adds `2` rather than `4`.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "FlatEnv"
        },
        "error": {
          "feedback": "You might think that `(bar 2)` and `(bar 4)` errors because they return functions,\nand that they would not error if they return values of other kinds (e.g., numbers and vectors).\nHowever, they do not error.\n\nYou might think that the two variable definitions error because the variables are bound to functions,\nand that they would not error if the variables were bound to values of other kinds (e.g., numbers and vectors).\nHowever, they do not error.\n\nIn SMoL, functions are (also) *first-class* citizens of the value world.\n",
          "misconception": "FunNotVal"
        }
      }
    },
    "lambda_new_state": {
      "answer": "2",
      "feedback": "`x` is bound to `1`.\n`g` is bound to the lambda function.\n`(set! x 2)` binds `x` to `2`.\nSo, the value of `(g 0)` is the value of `(+ 2 0)`, which is `2`.\n",
      "program": "(defvar x 1)\n(deffun (f)\n  (lambda (y)\n    (+ x y)))\n(defvar g (f))\n(set! x 2)\n(g 0)\n",
      "again": {
        "answer": "2",
        "program": "(defvar a 2)\n(deffun (make)\n  (lambda (b)\n    (+ a b)))\n(defvar fun (make))\n(set! a 1)\n(fun 1)\n"
      },
      "misconceptions": {
        "1": {
          "feedback": "You are right that `x` is bound to `1` when the lambda function is created.\nYou might think the function remembers the value `1`.\nHowever, the function does not remember the value of `x`.  Rather, it remembers the environment and hence always refers to the latest value of `x`.\nIn SMoL, functions refer to the latest values of variables defined outside their definitions.\n",
          "misconception": "DeepClosure"
        },
        "error": {
          "feedback": "You might think that `(f)` errors because it returns a function,\nand that it would not error if it returns a value of other kinds (e.g., numbers and vectors).\nHowever, it does not error.\n\nYou might think that `(defvar g (f))` errors because `g` is bound to a function,\nand that it would not error if `g` is bound to a value of other kinds (e.g., numbers and vectors).\nHowever, it does not error.\n\nIn SMoL, functions are (also) *first-class* citizens of the value world.\n",
          "misconception": "FunNotVal"
        }
      }
    },
    "practice_translate": {
      "feedback": "The correct answer is `(defvar f (lambda (x) (+ x 1)))`.\n\nThe following are common wrong answers:\n\n- `(deffun f (lambda (x) (+ x 1)))`, which didn't replace the definition keyword\n- `(defvar f (lambda (f x) (+ x 1)))`, which makes `f` a function that takes two parameters rather than one\n- `(defvar (f x) (+ x 1))`, which didn't switch to `lambda`\n",
      "prompt": "Please rewrite this function definition with as a variable definition that binds lambda function.\n\n```\n(deffun (f x) (+ x 1))\n```\n"
    },
    "warmup_lambda": {
      "answer": "3",
      "feedback": "This program is essentially the same as\n\n```\n(deffun (f x)\n  (deffun (fun y)\n    (+ x y))\n  fun)\n(defvar x 0)\n((f 2) 1)\n```\n",
      "program": "(deffun (f x)\n  (lambda (y) (+ x y)))\n(defvar x 0)\n((f 2) 1)\n",
      "again": {
        "answer": "5",
        "program": "(deffun (g a)\n  (lambda (b) (+ a b)))\n\n((g 3) 2)\n"
      }
    }
  }
},
"post1":
{
  "misconceptions": [],
  "order": [
    "intro_post1",
    [
      "post_new_three_layer",
      "post_error_when_refer_to_undefined",
      "post_bad_order"
    ]
  ],
  "questions": {
    "intro_post1": {
      "confirm": "In this tutorial, we will review previously learned content.\n"
    },
    "post_bad_order": {
      "answer": "error",
      "feedback": "The first definition tries to bind `i` to the value of `(* j 3)`.\nTo evaluate `(* j 3)`, we need the value of `j`.\nBut `j` is not bound to a value at that moment.\n",
      "program": "(defvar i (* j 3))\n(defvar j 2)\n\ni\nj\n",
      "again": {
        "answer": "error",
        "program": "(defvar baz (+ bar 1))\n(defvar bar 2)\n\nbaz\nbar\n"
      },
      "misconceptions": {
        "6 2": {
          "feedback": "You might think `(* j 3)` is able to refer to `j` and hence evaluate to a value.\nHowever, `j` has not been bound to a value when `(* j 3)` is evaluated.\nIn SMoL, every block evaluates its definitions and expressions in reading order (i.e., top-to-bottom and left-to-right).\n",
          "misconception": "Lazy"
        }
      }
    },
    "post_error_when_refer_to_undefined": {
      "answer": "error",
      "feedback": "`(+ (k 3) b)` is evaluated in the top-level block, where `b` is not defined.\nSo, this program errors.\n",
      "program": "(deffun (k a)\n  (defvar b 1)\n  (+ a b))\n\n(+ (k 3) b)\n",
      "again": {
        "answer": "error",
        "program": "(deffun (foo bar)\n  (defvar zzz 8)\n  (* bar zzz))\n\n(* (foo 9) zzz)\n"
      },
      "misconceptions": {
        "5": {
          "feedback": "`(defvar b 1)` binds `b` to `1`.\nYou might think the binding applies everywhere.\nHowever, it only applies to the body of `k`.\n`(+ (k 3) b)` appears in the top-level block, so it tries to refer to\na `b` defined in the top-level block. But `b` is not defined in the top-level block.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "FlatEnv"
        }
      }
    },
    "post_new_three_layer": {
      "answer": "15",
      "feedback": "This program binds `a` to `3` and `foo` to a function, and then\nevaluates `(+ (foo 4) 2)`.\nThe value of `(+ (foo 4) 2)` is the value of `(+ (bar) 2)`,\nwhich is the value of `(+ (+ a b z) 2)`,\nwhich is the value of `(+ (+ 3 4 6) 2)`,\nwhich is `10`.\n",
      "program": "(defvar a 3)\n(deffun (foo b)\n  (deffun (bar)\n    (defvar c 6)\n    (+ a b c))\n  (bar))\n(+ (foo 4) 2)\n",
      "again": {
        "answer": "24",
        "program": "(defvar aa 3)\n(deffun (abc bb)\n  (deffun (h)\n    (defvar cc 2)\n    (* aa bb cc))\n  (h))\n(* (abc 4) 1)\n"
      },
      "misconceptions": {
        "error": {
          "feedback": "You might think `(+ a b c)` can't refer to `a` and `b`.\nHowever, `(+ a b c)` appears in the body of `bar`, which is a sub-block of\nthe body of `foo`, which is a sub-block of the top-level block.\nSo, it can refer to the `a` defined in the top-level block\nand the `b` defined in `foo`.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "IsolatedFun"
        }
      }
    }
  }
},
"post2":
{
  "misconceptions": [],
  "order": [
    "intro_post2",
    [
      "post_new_three_layer",
      "post_error_when_refer_to_undefined",
      "post_bad_order",
      "post_alias_with_defvar",
      "post_alias_with_funcall",
      "post_alias_mvec_in_mvec",
      "post_basic_circularity"
    ]
  ],
  "questions": {
    "intro_post2": {
      "confirm": "In this tutorial, we will review previously learned content.\n"
    },
    "post_alias_mvec_in_mvec": {
      "answer": "#(72 #(72))",
      "feedback": "`v1` is bound to a one-element vector.\nThe only element of this vector is `53`.\n`v2` is bound to a two-element vector.\nThe elements of this vector are `72` and the one-element vector.\nThe subsequent `(vec-set! v1 0 72)` mutates the one-element vector.\nFinally, the value of `v2` is printed.\n",
      "program": "(defvar v1 (mvec 53))\n(defvar v2 (mvec 72 v1))\n(vec-set! v1 0 72)\nv2\n",
      "again": {
        "answer": "#(52 #(71) 53)",
        "program": "(defvar m (mvec 0))\n(defvar v (mvec 52 m 53))\n(vec-set! m 0 71)\nv\n"
      },
      "misconceptions": {
        "#(72 #(53))": {
          "feedback": "You might think `v1` and the 1-th element of `v2` refer to different vectors, so changing `v1` doesn't change the 1-th element of `v2`.\nHowever, they refer to the same vector.\nIn SMoL, a vector can be referred to by multiple places, and creating new vectors that refer to existing ones does not create new copies of the existing vectors.\n",
          "misconception": "StructsCopyStructs"
        }
      }
    },
    "post_alias_with_defvar": {
      "answer": "#(45)",
      "feedback": "The program creates a one-element vector (the only element being 23)\nand binds it to both `v1` and `v2`.\nThe `0`-th element of the vector is then replaced with `45`.\nSo, when the vector is printed as `#(45)`.\n",
      "program": "(defvar v1 (mvec 23))\n(defvar v2 v1)\n(vec-set! v1 0 45)\nv2\n",
      "again": {
        "answer": "#(87 26)",
        "program": "(defvar a (mvec 37 26))\n(defvar b a)\n(vec-set! a 0 87)\nb\n"
      },
      "misconceptions": {
        "#(23)": {
          "feedback": "You might think `v1` and `v2` refer to different vectors, so changing `v1` doesn't change `v2`.\nHowever, they refer to the same vector.\nIn SMoL, a vector can be referred to by more than one variable, and binding a vector to a new variable does not create a copy of that vector.\n",
          "misconception": "DefsCopyStructs"
        }
      }
    },
    "post_alias_with_funcall": {
      "answer": "#(43 77)",
      "feedback": "The program first creates a two-element vector. The elements are both `77`.\nAfter that, the program defines a function `f`.\nThe function call `(f m1)` replaces the first vector element with `43`.\nAfter that, the vector is printed. The vector now refers `43` and `77`, so the\nresult is `#(43 77)`.\n",
      "program": "(defvar m1 (mvec 77 77))\n(deffun (f m2)\n  (vec-set! m2 0 43))\n(f m1)\nm1\n",
      "again": {
        "answer": "#(52 17)",
        "program": "(defvar a (mvec 55 17))\n(deffun (foobar b)\n  (vec-set! b 0 52))\n(foobar a)\na\n"
      },
      "misconceptions": {
        "#(77 77)": {
          "feedback": "You might think `m1` and `m2` refer to different vectors, so changing `m2` doesn't change `m1`.\nHowever, they refer to the same vector.\nIn SMoL, a vector can be referred to by more than one variable, and vectors that are passed to a function in a function call do not get copied..\n",
          "misconception": "CallsCopyStructs"
        }
      }
    },
    "post_bad_order": {
      "answer": "error",
      "feedback": "The first definition tries to bind `n` to the value of `(* m 4)`.\nTo evaluate `(* m 4)`, we need the value of `m`.\nBut `m` is not bound to a value at that moment.\n",
      "program": "(defvar n (* m 4))\n(defvar m 3)\n\nn\nm\n",
      "again": {
        "answer": "error",
        "program": "(defvar baz (+ bar 1))\n(defvar bar 2)\n\nbaz\nbar\n"
      },
      "misconceptions": {
        "12 3": {
          "feedback": "You might think `(* m 4)` is able to refer to `m` and hence evaluate to a value.\nHowever, `m` has not been bound to a value when `(* m 4)` is evaluated.\nIn SMoL, every block evaluates its definitions and expressions in reading order (i.e., top-to-bottom and left-to-right).\n",
          "misconception": "Lazy"
        }
      }
    },
    "post_basic_circularity": {
      "answer": "76",
      "feedback": "`m` is bound to a vector.\n`(vec-set! m 0 m)` replaces the 0-th element of the vector with the vector itself.\nThis is fine because a vector element can be any value, including itself.\nBesides, the vector is not copied, so the mutation finishes immediately.\n",
      "program": "(defvar m (mvec 82 76))\n(vec-set! m 0 m)\n(vec-ref m 1)\n",
      "again": {
        "answer": "84",
        "program": "(defvar abc (mvec 84 82 85 86))\n(vec-set! abc 1 abc)\n(vec-ref abc 0)\n"
      },
      "misconceptions": {
        "error": {
          "feedback": "You might think `m` can't refer to itself.\nHowever, this is not the case.\nIn SMoL, vectors can be referred to by arbitrary vectors, including themselves.\n",
          "misconception": "NoCircularity"
        }
      }
    },
    "post_error_when_refer_to_undefined": {
      "answer": "error",
      "feedback": "`(+ (foobar 2) m)` is evaluated in the top-level block, where `m` is not defined.\nSo, this program errors.\n",
      "program": "(deffun (foobar n)\n  (defvar m 4)\n  (+ n m))\n\n(+ (foobar 2) m)\n",
      "again": {
        "answer": "error",
        "program": "(deffun (foo bar)\n  (defvar zzz 8)\n  (* bar zzz))\n\n(* (foo 9) zzz)\n"
      },
      "misconceptions": {
        "10": {
          "feedback": "`(defvar m 4)` binds `m` to `4`.\nYou might think the binding applies everywhere.\nHowever, it only applies to the body of `foobar`.\n`(+ (foobar 2) m)` appears in the top-level block, so it tries to refer to\nan `m` defined in the top-level block. But `m` is not defined in the top-level block.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "FlatEnv"
        }
      }
    },
    "post_new_three_layer": {
      "answer": "13",
      "feedback": "This program binds `n` to `5` and `f1` to a function, and then\nevaluates `(+ (f1 1) 3)`.\nThe value of `(+ (f1 1) 3)` is the value of `(+ (f2) 3)`,\nwhich is the value of `(+ (+ n m l) 3)`,\nwhich is the value of `(+ (+ 5 1 4) 3)`,\nwhich is `13`.\n",
      "program": "(defvar n 5)\n(deffun (f1 m)\n  (deffun (f2)\n    (defvar l 4)\n    (+ n m l))\n  (f2))\n(+ (f1 1) 3)\n",
      "again": {
        "answer": "24",
        "program": "(defvar aa 3)\n(deffun (abc bb)\n  (deffun (h)\n    (defvar cc 2)\n    (* aa bb cc))\n  (h))\n(* (abc 4) 1)\n"
      },
      "misconceptions": {
        "error": {
          "feedback": "You might think `(+ n m l)` can't refer to `n` and `m`.\nHowever, `(+ n m l)` appears in the body of `f2`, which is a sub-block of\nthe body of `f1`, which is a sub-block of the top-level block.\nSo, it can refer to the `n` defined in the top-level block\nand the `m` defined in `f1`.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "IsolatedFun"
        }
      }
    }
  }
},
"post3":
{
  "misconceptions": [
    "FunNotVal"
  ],
  "order": [
    "intro_post3",
    [
      "post_alias_with_defvar",
      "post_alias_with_funcall",
      "post_alias_mvec_in_mvec",
      "post_basic_circularity",
      "post_new_fun_rem_env",
      "post_not_aliased_by_defvar_1",
      "post_not_aliased_by_funarg_2",
      "post_new_alias_mvec_in_mvec_trick"
    ]
  ],
  "questions": {
    "intro_post3": {
      "confirm": "In this tutorial, we will review previously learned content.\n"
    },
    "post_alias_mvec_in_mvec": {
      "answer": "#(77 #(77))",
      "feedback": "`nn` is bound to a one-element vector.\nThe only element of this vector is `66`.\n`mm` is bound to a two-element vector.\nThe elements of this vector are `77` and the one-element vector.\nThe subsequent `(vec-set! nn 0 77)` mutates the one-element vector.\nFinally, the value of `mm` is printed.\n",
      "program": "(defvar nn (mvec 66))\n(defvar mm (mvec 77 nn))\n(vec-set! nn 0 77)\nmm\n",
      "again": {
        "answer": "#(52 #(71) 53)",
        "program": "(defvar m (mvec 0))\n(defvar v (mvec 52 m 53))\n(vec-set! m 0 71)\nv\n"
      },
      "misconceptions": {
        "#(77 #(66))": {
          "feedback": "You might think `nn` and the 1-th element of `mm` refer to different vectors, so changing `nn` doesn't change the 1-th element of `mm`.\nHowever, they refer to the same vector.\nIn SMoL, a vector can be referred to by multiple places, and creating new vectors that refer to existing ones does not create new copies of the existing vectors.\n",
          "misconception": "StructsCopyStructs"
        }
      }
    },
    "post_alias_with_defvar": {
      "answer": "#(94)",
      "feedback": "The program creates a one-element vector (the only element being 72)\nand binds it to both `p` and `q`.\nThe `0`-th element of the vector is then replaced with `94`.\nSo, when the vector is printed as `#(94)`.\n",
      "program": "(defvar p (mvec 72))\n(defvar q p)\n(vec-set! p 0 94)\nq\n",
      "again": {
        "answer": "#(87 26)",
        "program": "(defvar a (mvec 37 26))\n(defvar b a)\n(vec-set! a 0 87)\nb\n"
      },
      "misconceptions": {
        "#(72)": {
          "feedback": "You might think `p` and `q` refer to different vectors, so changing `p` doesn't change `q`.\nHowever, they refer to the same vector.\nIn SMoL, a vector can be referred to by more than one variable, and binding a vector to a new variable does not create a copy of that vector.\n",
          "misconception": "DefsCopyStructs"
        }
      }
    },
    "post_alias_with_funcall": {
      "answer": "#(97 88)",
      "feedback": "The program first creates a two-element vector. The elements are both `88`.\nAfter that, the program defines a function `f`.\nThe function call `(f zz)` replaces the first vector element with `97`.\nAfter that, the vector is printed. The vector now refers `97` and `88`, so the\nresult is `#(97 88)`.\n",
      "program": "(defvar zz (mvec 88 88))\n(deffun (f aa)\n  (vec-set! aa 0 97))\n(f zz)\nzz\n",
      "again": {
        "answer": "#(52 17)",
        "program": "(defvar a (mvec 55 17))\n(deffun (foobar b)\n  (vec-set! b 0 52))\n(foobar a)\na\n"
      },
      "misconceptions": {
        "#(88 88)": {
          "feedback": "You might think `zz` and `aa` refer to different vectors, so changing `aa` doesn't change `zz`.\nHowever, they refer to the same vector.\nIn SMoL, a vector can be referred to by more than one variable, and vectors that are passed to a function in a function call do not get copied..\n",
          "misconception": "CallsCopyStructs"
        }
      }
    },
    "post_basic_circularity": {
      "answer": "85",
      "feedback": "`ns` is bound to a vector.\n`(vec-set! ns 0 ns)` replaces the 0-th element of the vector with the vector itself.\nThis is fine because a vector element can be any value, including itself.\nBesides, the vector is not copied, so the mutation finishes immediately.\n",
      "program": "(defvar ns (mvec 74 85))\n(vec-set! ns 0 ns)\n(vec-ref ns 1)\n",
      "again": {
        "answer": "84",
        "program": "(defvar abc (mvec 84 82 85 86))\n(vec-set! abc 1 abc)\n(vec-ref abc 0)\n"
      },
      "misconceptions": {
        "error": {
          "feedback": "You might think `ns` can't refer to itself.\nHowever, this is not the case.\nIn SMoL, vectors can be referred to by arbitrary vectors, including themselves.\n",
          "misconception": "NoCircularity"
        }
      }
    },
    "post_new_alias_mvec_in_mvec_trick": {
      "answer": "#(#(66) 66 66)",
      "feedback": "`m` is first bound to a one-element vector.\n`z` is bound to a three-element vector, of which the first element is the one-element vector.\n`(set! m (mvec 43))` binds `m` to a new vector.\nThis doesn't impact `z` because\nthe 0-th element of `z` is still the one-element vector.\n",
      "program": "(defvar m (mvec 66))\n(defvar z (mvec m 66 66))\n(set! m (mvec 43))\nz\n",
      "again": {
        "answer": "#(2 #(0) 3)",
        "program": "(defvar x (mvec 0))\n(defvar v (mvec 2 x 3))\n(set! x (mvec 1))\nv\n"
      },
      "misconceptions": {
        "#(#(43) 66 66)": {
          "feedback": "You might think the `0`-th and first element of `z` refers to `m`.\nHowever, vectors refer to values, so it actually refers to the one-element vector, i.e., *the value of* `m`.\nIn SMoL, variable assignments change *only* the mutated variables.\n",
          "misconception": "StructByRef"
        }
      }
    },
    "post_new_fun_rem_env": {
      "answer": "3 5",
      "feedback": "There is exactly one variable `n`.\nThe `n` in `(set! n 3)` refers to that variable.\nCalling `bar` evaluates `(set! n 3)`, which mutates `n`.\nWhen the value of `n` is eventually printed, we see the new value.\n",
      "program": "(defvar n 7)\n(deffun (foo)\n  n)\n(deffun (bar)\n  (set! n 3)\n  (foo))\n(bar)\n(set! n 5)\n(foo)\n",
      "again": {
        "answer": "2 6",
        "program": "(defvar a 4)\n(deffun (h)\n  a)\n(deffun (k)\n  (set! a 2)\n  (h))\n(k)\n(set! a 6)\n(h)\n"
      },
      "misconceptions": {
        "7 5": {
          "feedback": "You might think `(set! n 3)` defines `n`.\nHowever, it will mutate the top-level `n` defined outside the functions.\nIn SMoL, variable assignments mutate existing bindings and never create new bindings.\n",
          "misconception": "DefOrSet"
        },
        "7 7": {
          "feedback": "You are right that `n` is bound to `7` when `f` is bound to a function.\nYou might think the function remembers the value `7`.\nHowever, `f` does not remember the value of `n`. Rather, it always\nrefers to the latest value of `n`.\nIn SMoL, functions refer to the latest values of variables defined outside their definitions.\n",
          "misconception": "DeepClosure"
        },
        "error": {
          "feedback": "You might think `foo` can't refer to `n`.\nHowever, `foo` is defined in the top-level block, so its body is allowed to refer to the top-level `n`.\n\nYou might think `(set! n 3)` can't mutate `n`.\nHowever, `(set! n 3)` appears in the body of `bar`, which is a sub-block of the top-level block,\nso it is allowed to mutate the top-level `n`.\n\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "IsolatedFun"
        }
      }
    },
    "post_not_aliased_by_defvar_1": {
      "answer": "5 6",
      "feedback": "The first definition binds `a` to `6`.\nThe second definition binds `b` to the value of `a`, which is `6`.\nThe `set!` mutates the binding of `a`, so `a` is now bound to `5`.\nBut `b` is still bound to `6`.\n",
      "program": "(defvar a 6)\n(defvar b a)\n(set! a 5)\na\nb\n",
      "again": {
        "answer": "2 1",
        "program": "(defvar a 1)\n(defvar b a)\n(set! a 2)\na\nb\n"
      },
      "misconceptions": {
        "5 5": {
          "feedback": "You might think `(defvar b a)` binds `b` to `a`, so changing `a` will change `b`.\nHowever, we learned in previous tutorials that variables are bound to values, so `b` is bound to the `5`, i.e., *the value of* `a`.\nIn SMoL, variable assignments change *only* the mutated variables.\n",
          "misconception": "DefByRef"
        }
      }
    },
    "post_not_aliased_by_funarg_2": {
      "answer": "5",
      "feedback": "The function call binds `b` to `5`.\nThe `set!` mutates the value of `b` to `3`, but `a` is still bound to `5`.\n",
      "program": "(defvar a 5)\n(deffun (k b)\n  (set! b 3)\n  a)\n(k a)\n",
      "again": {
        "answer": "1",
        "program": "(defvar s 1)\n(deffun (foobar t)\n  (set! t 2)\n  s)\n(foobar s)\n"
      },
      "misconceptions": {
        "3": {
          "feedback": "You might think the function call `(k a)` binds `b` to `a`, so\nchanging `b` will change `a`.\nHowever, we learned in previous tutorials that variables are bound to values, so `b` is bound to `5`, i.e., *the value of* `a`.\nIn SMoL, variable assignments change *only* the mutated variables.\n",
          "misconception": "CallByRef"
        },
        "error": {
          "feedback": "You might think `k` can't refer to `a`.\nHowever, `k` is defined in the top-level block,\nso its body can refer to the top-level `a`.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "IsolatedFun"
        }
      }
    }
  }
},
"post4":
{
  "misconceptions": [
    "Other"
  ],
  "order": [
    "intro_post4",
    [
      "post_new_fun_rem_env",
      "post_not_aliased_by_defvar_1",
      "post_not_aliased_by_funarg_2",
      "post_new_alias_mvec_in_mvec_trick",
      "post_lambda_fun_ref_env",
      "post_lambda_new_state"
    ]
  ],
  "questions": {
    "intro_post4": {
      "confirm": "In this tutorial, we will review previously learned content.\n"
    },
    "post_lambda_fun_ref_env": {
      "answer": "6 5",
      "feedback": "The value of `(k 3)` is a lambda function defined in an\nenvironment where `b` is bound to `3`.\nThe value of `(k 2)` is *another* lambda function defined in an\nenvironment where `b` is bound to `2`.\nThe two lambda functions are *different* values.\nSo, the value of `(foo 3)` is `6`,\nwhile the value of `(bar 3)` is `5`.\n",
      "program": "(deffun (k b)\n  (lambda (a)\n    (+ a b)))\n(defvar foo (k 3))\n(defvar bar (k 2))\n(foo 3)\n(bar 3)\n",
      "again": {
        "answer": "40 4",
        "program": "(deffun (f n)\n  (lambda (m)\n    (* m n)))\n(defvar fun1 (f 10))\n(defvar fun2 (f 1))\n(fun1 4)\n(fun2 4)\n"
      },
      "misconceptions": {
        "5 5": {
          "feedback": "`k` is called twice.\nThe first call binds `a` to `3`.\nThe second call binds `a` to `2`.\nYou might think the second binding applies everywhere.\nHowever, it only applies to the second call.\nThe lambda function that `foo` refers to is created in the first call, so `(foo 3)` adds `3` rather than `2`.\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "FlatEnv"
        },
        "error": {
          "feedback": "You might think that `(k 3)` and `(k 2)` errors because they return functions,\nand that they would not error if they return values of other kinds (e.g., numbers and vectors).\nHowever, they do not error.\n\nYou might think that the two variable definitions error because the variables are bound to functions,\nand that they would not error if the variables were bound to values of other kinds (e.g., numbers and vectors).\nHowever, they do not error.\n\nIn SMoL, functions are (also) *first-class* citizens of the value world.\n",
          "misconception": "FunNotVal"
        }
      }
    },
    "post_lambda_new_state": {
      "answer": "3",
      "feedback": "`a` is bound to `1`.\n`bar` is bound to the lambda function.\n`(set! a 3)` binds `a` to `3`.\nSo, the value of `(bar 0)` is the value of `(+ 3 0)`, which is `3`.\n",
      "program": "(defvar a 1)\n(deffun (foo)\n  (lambda (b)\n    (+ a b)))\n(defvar bar (foo))\n(set! a 3)\n(bar 0)\n",
      "again": {
        "answer": "2",
        "program": "(defvar a 2)\n(deffun (make)\n  (lambda (b)\n    (+ a b)))\n(defvar fun (make))\n(set! a 1)\n(fun 1)\n"
      },
      "misconceptions": {
        "1": {
          "feedback": "You are right that `a` is bound to `1` when the lambda function is created.\nYou might think the function remembers the value `1`.\nHowever, the function does not remember the value of `a`.  Rather, it remembers the environment and hence always refers to the latest value of `a`.\nIn SMoL, functions refer to the latest values of variables defined outside their definitions.\n",
          "misconception": "DeepClosure"
        },
        "error": {
          "feedback": "You might think that `(foo)` errors because it returns a function,\nand that it would not error if it returns a value of other kinds (e.g., numbers and vectors).\nHowever, it does not error.\n\nYou might think that `(defvar bar (foo))` errors because `g` is bound to a function,\nand that it would not error if `g` is bound to a value of other kinds (e.g., numbers and vectors).\nHowever, it does not error.\n\nIn SMoL, functions are (also) *first-class* citizens of the value world.\n",
          "misconception": "FunNotVal"
        }
      }
    },
    "post_new_alias_mvec_in_mvec_trick": {
      "answer": "#(#(88) 88 88)",
      "feedback": "`a` is first bound to a one-element vector.\n`c` is bound to a three-element vector, of which the first element is the one-element vector.\n`(set! a (mvec 76))` binds `a` to a new vector.\nThis doesn't impact `c` because\nthe 0-th element of `c` is still the one-element vector.\n",
      "program": "(defvar a (mvec 88))\n(defvar c (mvec a 88 88))\n(set! a (mvec 76))\nc\n",
      "again": {
        "answer": "#(2 #(0) 3)",
        "program": "(defvar x (mvec 0))\n(defvar v (mvec 2 x 3))\n(set! x (mvec 1))\nv\n"
      },
      "misconceptions": {
        "#(#(76) 88 88)": {
          "feedback": "You might think the `0`-th and first element of `c` refers to `a`.\nHowever, vectors refer to values, so it actually refers to the one-element vector, i.e., *the value of* `a`.\nIn SMoL, variable assignments change *only* the mutated variables.\n",
          "misconception": "StructByRef"
        }
      }
    },
    "post_new_fun_rem_env": {
      "answer": "4 2",
      "feedback": "There is exactly one variable `t`.\nThe `t` in `(set! t 4)` refers to that variable.\nCalling `f2` evaluates `(set! t 4)`, which mutates `t`.\nWhen the value of `t` is eventually printed, we see the new value.\n",
      "program": "(defvar t 6)\n(deffun (f1)\n  t)\n(deffun (f2)\n  (set! t 4)\n  (f1))\n(f2)\n(set! t 2)\n(f1)\n",
      "again": {
        "answer": "2 6",
        "program": "(defvar a 4)\n(deffun (h)\n  a)\n(deffun (k)\n  (set! a 2)\n  (h))\n(k)\n(set! a 6)\n(h)\n"
      },
      "misconceptions": {
        "6 2": {
          "feedback": "You might think `(set! t 4)` defines `t`.\nHowever, it will mutate the top-level `t` defined outside the functions.\nIn SMoL, variable assignments mutate existing bindings and never create new bindings.\n",
          "misconception": "DefOrSet"
        },
        "6 6": {
          "feedback": "You are right that `t` is bound to `6` when `f` is bound to a function.\nYou might think the function remembers the value `6`.\nHowever, `f` does not remember the value of `t`. Rather, it always\nrefers to the latest value of `t`.\nIn SMoL, functions refer to the latest values of variables defined outside their definitions.\n",
          "misconception": "DeepClosure"
        },
        "error": {
          "feedback": "You might think `f1` can't refer to `t`.\nHowever, `f1` is defined in the top-level block, so its body is allowed to refer to the top-level `t`.\n\nYou might think `(set! t 4)` can't mutate `t`.\nHowever, `(set! t 4)` appears in the body of `f2`, which is a sub-block of the top-level block,\nso it is allowed to mutate the top-level `t`.\n\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "IsolatedFun"
        }
      }
    },
    "post_not_aliased_by_defvar_1": {
      "answer": "6 3",
      "feedback": "The first definition binds `n` to `3`.\nThe second definition binds `m` to the value of `n`, which is `3`.\nThe `set!` mutates the binding of `n`, so `n` is now bound to `6`.\nBut `m` is still bound to `3`.\n",
      "program": "(defvar n 3)\n(defvar m n)\n(set! n 6)\nn\nm\n",
      "again": {
        "answer": "2 1",
        "program": "(defvar a 1)\n(defvar b a)\n(set! a 2)\na\nb\n"
      },
      "misconceptions": {
        "6 6": {
          "feedback": "You might think `(defvar m n)` binds `m` to `n`, so changing `n` will change `m`.\nHowever, we learned in previous tutorials that variables are bound to values, so `m` is bound to the `6`, i.e., *the value of* `n`.\nIn SMoL, variable assignments change *only* the mutated variables.\n",
          "misconception": "DefByRef"
        }
      }
    },
    "post_not_aliased_by_funarg_2": {
      "answer": "2",
      "feedback": "The function call binds `m` to `2`.\nThe `set!` mutates the value of `m` to `7`, but `n` is still bound to `2`.\n",
      "program": "(defvar n 2)\n(deffun (h m)\n  (set! m 7)\n  n)\n(h n)\n",
      "again": {
        "answer": "1",
        "program": "(defvar s 1)\n(deffun (foobar t)\n  (set! t 2)\n  s)\n(foobar s)\n"
      },
      "misconceptions": {
        "7": {
          "feedback": "You might think the function call `(h n)` binds `m` to `n`, so\nchanging `m` will change `n`.\nHowever, we learned in previous tutorials that variables are bound to values, so `m` is bound to `2`, i.e., *the value of* `n`.\nIn SMoL, variable assignments change *only* the mutated variables.\n",
          "misconception": "CallByRef"
        },
        "error": {
          "feedback": "You might think `h` can't refer to `n`.\nHowever, `h` is defined in the top-level block,\nso its body can refer to the top-level `n`.\n\nIn SMoL, variable references follow the hierarchical structure of blocks.\n",
          "misconception": "IsolatedFun"
        }
      }
    }
  }
},
"begin":
{
  "misconceptions": [
    "Other"
  ],
  "order": [
    "intro_begin",
    [
      "warmup_begin_1",
      "warmup_begin_2",
      "begin_setbang",
      "begin_can_take_three",
      "intro_if",
      "if_begin_1",
      "if_begin_3",
      "if_begin_4"
    ],
    "reflect_begin",
    "goal_begin"
  ],
  "questions": {
    "begin_can_take_three": {
      "answer": "1 4 1",
      "feedback": "At first, `x` is bound to `0`, and `y` is bound to `0`.\nThe `begin` mutates `x` to `1` and `y` to `4`,\nand returns the value of `x`, which is now `1`.\n",
      "program": "(defvar x 0)\n(defvar y x)\n(defvar z\n  (begin\n    (set! x (+ x 1))\n    (set! y 4)\n    x))\nx\ny\nz\n",
      "again": {
        "answer": "6 32 6",
        "program": "(defvar a 2)\n(defvar b a)\n(defvar c\n  (begin\n    (set! a (* a 3))\n    (set! b 32)\n    a))\na\nb\nc\n"
      }
    },
    "begin_setbang": {
      "answer": "1 1",
      "feedback": "The `begin` first increases `x` by one, then evaluates `x`. So, the value of `y` is `1`.",
      "program": "(defvar x 0)\n(defvar y\n  (begin\n    (set! x (+ x 1))\n    x))\nx\ny\n",
      "again": {
        "answer": "60 60",
        "program": "(defvar a 30)\n(defvar b\n  (begin\n    (set! a (* a 2))\n    a))\na\nb\n"
      },
      "misconceptions": {
        "0 0": {
          "misconception": "Other"
        },
        "0 1": {
          "misconception": "Other"
        },
        "1 0": {
          "misconception": "Other"
        }
      }
    },
    "goal_begin": {
      "emphasize": "`begin` computes its sub-expressions in top-to-bottom, left-to-right order, and gives the value of the last expression.\n"
    },
    "if_begin_1": {
      "answer": "error",
      "feedback": "This is a syntax error!\nThere are 4 sub-expressions in the `if` expression: `(equal? n 0)` and `1` and `(set! ...)` and `(* ...)`.\nAs a result, the language doesn't know which is the condition, which is the then branch, and which is the else branch.\n",
      "program": "(defvar counter 0)\n(deffun (factorial n)\n  (if (equal? n 0)\n      1\n      (set! counter (+ counter 1))\n      (* (factorial (- n 1)) n)))\n(factorial 2)\n",
      "again": {
        "answer": "error",
        "program": "(defvar x 7)\n(deffun (f n)\n  (if (> n 88)\n      1\n      (set! x (+ x 1))\n      (* (f (+ n 1)) n)))\n(f 45)\n"
      }
    },
    "if_begin_3": {
      "answer": "2",
      "feedback": "Finally, the program is correct!\n",
      "program": "(defvar counter 0)\n(deffun (factorial n)\n  (if (equal? n 0)\n      1\n      (begin\n        (set! counter (+ counter 1))\n        (* (factorial (- n 1)) n))))\n(factorial 2)\n",
      "again": {
        "answer": "2",
        "program": "(defvar x 0)\n(deffun (fib n)\n  (if (<= n 1)\n      1\n      (begin\n        (set! x (+ x 1))\n        (+ (fib (- n 1))\n           (fib (- n 2))))))\n(fib 2)\n"
      }
    },
    "if_begin_4": {
      "answer": "2",
      "feedback": "This program is still correct! The `then` branch (`1`) is on the same line as the condition (`(equal? n 0)`).\n",
      "program": "(defvar counter 0)\n(deffun (factorial n)\n  (if (equal? n 0) 1\n      (begin\n        (set! counter (+ counter 1))\n        (* (factorial (- n 1)) n))))\n(factorial 2)\n",
      "again": {
        "answer": "2",
        "program": "(defvar x 0)\n(deffun (fib n)\n  (if (<= n 1) 1\n      (begin\n        (set! x (+ x 1))\n        (+ (fib (- n 1))\n           (fib (- n 2))))))\n(fib 2)\n"
      }
    },
    "intro_begin": {
      "confirm": "In this tutorial, we will learn about `begin`.\n\n`begin` lets you evaluate a sequence of expressions and returns the value of the last expression.\n"
    },
    "intro_if": {
      "confirm": "The next few questions are going to combine `begin` with `if`.\n\n`(if cnd thn els)` evaluates `cnd`.\nIf the evaluation produces a true value, `thn` is evaluated in place of the whole `if` expression.\nOtherwise, `els` is evaluated.\n\nGiving `if` too few or too many subexpressions is a syntactic error.\nFor example, all of the following programs produce errors.\n\n```\n(if (equal? 2 3)\n    \"Do something\")\n```\n\n```\n(if (equal? \"apple\" \"orange\")\n    \"What?!\"\n    \"Okay\"\n    \"...\")\n```\n\nNow let's move on to other questions.\n"
    },
    "reflect_begin": {
      "prompt": "What did you learn about `begin` from these programs?"
    },
    "warmup_begin_1": {
      "answer": "4",
      "feedback": "The definition binds `x`  to the value of the `begin` expression.\nA `begin` expression evaluates its sub-expressions from left to right\nand produces the value of the right-most expression.\nSo, `x` is bound to `4`.\n",
      "program": "(defvar x (begin 2 3 4))\nx\n",
      "again": {
        "answer": "5",
        "program": "(defvar bval (begin 7 6 5))\nbval\n"
      },
      "misconceptions": {
        "2 3 4": {
          "misconception": "Other"
        }
      }
    },
    "warmup_begin_2": {
      "answer": "67",
      "feedback": "The value of `(begin (begin 6 45) (begin 67))` is the value of\n`(begin 67)`, which is `67`.\n",
      "program": "(defvar z\n  (begin\n    (begin\n      6\n      45)\n    (begin\n      67)))\nz\n",
      "again": {
        "answer": "45",
        "program": "(defvar res\n  (begin\n    (begin\n      67)\n    (begin\n      (begin 5)\n      45)))\nres\n"
      }
    }
  }
},
"local":
{
  "misconceptions": [
    "Other"
  ],
  "order": [
    "intro_local",
    "warmup_nested",
    "warmup_local",
    "shadow",
    "goal_bind",
    "syntax_error",
    "all_let",
    "classifier_let_1",
    "classifier_let_2",
    "classifier_let_3",
    "reflect_let",
    "goal_let",
    "intro_letrec",
    "goal_letrec",
    "intro_letstar",
    "goal_letstar",
    "swap_let",
    "swap_letrec",
    "swap_letstar"
  ],
  "questions": {
    "all_let": {
      "confirm": "What we have seen so far also applies to `letrec` and `let*`.\n"
    },
    "classifier_let_1": {
      "answer": "error",
      "feedback": "The program errors because `(+ x 1)` is not in the scope of `x`.\n",
      "program": "(let ([x 2]\n      [y (+ x 1)])\n   (+ x y))\n",
      "again": {
        "answer": "error",
        "program": "(let ([a 2]\n      [b (* a 3)])\n   (+ a b))\n"
      },
      "misconceptions": {
        "5": {
          "misconception": "Other"
        }
      }
    },
    "classifier_let_2": {
      "answer": "error",
      "feedback": "The scope of `x` only includes the `let` body (i.e., `(getx)`)\nand does NOT include any of the right-hand-side expressions (i.e., `42` and the `lambda` expression).\nSo, the `x` in the  `lambda` expression is undefined.\n",
      "program": "(let ([x 42]\n      [getx (lambda () x)])\n  (getx))\n",
      "again": {
        "answer": "error",
        "program": "(let ([foo 5]\n      [getter (lambda () foo)])\n  (getter))\n"
      },
      "misconceptions": {
        "42": {
          "misconception": "Other"
        }
      }
    },
    "classifier_let_3": {
      "answer": "error",
      "feedback": "The program has a scope error.\nThe `lambda` expression is not in the scope of `f`.\nSo, it can not refer to itself.\n",
      "program": "(let ([f (lambda (n)\n           (or (equal? n 0)\n               (f (- n 1))))])\n  (f 3))\n",
      "again": {
        "answer": "error",
        "program": "(let ([h (lambda (x)\n           (or (equal? x 64)\n               (h (* x 2))))])\n  (h 2))\n"
      },
      "misconceptions": {
        "#t": {
          "misconception": "Other"
        }
      }
    },
    "goal_bind": {
      "emphasize": "A `let` expression binds its left-hand-side variables such that\nits body is in the scope of all the variables.\n\n(Recall that the scope of a variable is the region of a program where the\nvariable can be referred to.)\n"
    },
    "goal_let": {
      "emphasize": "In a `let`, the scope of a left-hand-side variable includes *none* of the right-hand-side expressions.\n\nIf we put it visually,\n\n<image src=\"data:image/png;base64,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\"/>\n"
    },
    "goal_letrec": {
      "emphasize": "In a `letrec`, the scope of a left-hand-side variable includes\n*all* of the right-hand-side expressions.\n\nIf we put it visually,\n\n<image src=\"data:image/png;base64,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\" />\n"
    },
    "goal_letstar": {
      "emphasize": "In a `let*`, the scope of a left-hand-side variable includes *all of the subsequent* right-hand-side expressions.\n\nIf we put it visually,\n\n<image src=\"data:image/png;base64,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\" />\n"
    },
    "intro_letrec": {
      "confirm": "`letrec` and `let*` are similar to `let`: all of them declare their left-hand-side variables such that\ntheir bodies are in the scope of all the left-hand side variables.\n\nThe following program tries to use `let` to define the factorial function.\n\n```\n(let ([f (lambda (n)\n           (if (equal? n 0)\n               1\n               (* (f (- n 1)) n)))])\n  (f 5))\n```\n\n\nThis program will NOT produce a value.\nRecall that in a `let`, the scope of a left-hand-side variable includes\nnone of the right-hand-side expressions.\nSo, the `lambda` expression can NOT refer to `f`.\n\nIf we use `letrec` rather than `let`, this program will work.\n\nYou can even use `letrec` to define mutually recursive functions. For example,\n\n```\n(letrec ([od? (lambda (n)\n                (if (equal? n 0)\n                    #f\n                    (ev? (- n 1))))]\n         [ev? (lambda (n)\n                (if (equal? n 0)\n                    #t\n                    (od? (- n 1))))])\n  (od? 2))\n```\n\nLet's summarize the scoping rules of `letrec`.\n"
    },
    "intro_letstar": {
      "confirm": "The following program tries to use `let` to process `x` through a series of operations.\n\n```\n(deffun (double n)\n  (* n 2))\n(deffun (add1 n)\n  (+ n 1))\n\n(let ([x 0]\n      [x (add1 x)]\n      [x (double x)])\n  (+ 3 x))\n```\n\n\nThis program will error because none of the right-hand-side expressions can refer to `x`.\nReplacing `let` with `letrec` will NOT fix the error.\nRecall that in a `letrec`, the scope of a left-hand-side variable includes\n*all* of the right-hand-side expressions.\nTo achieve this while avoiding confusion,\n`letrec` insists that all left-hand-side variables must be distinct.\n(`let` also has this restriction, but for a different reason.)\n\nIf we replace `let` with `let*`, this program will produce `5`, the value of `(+ 3 (double (add1 0)))`.\n`let*` allows you to do a series of bindings sequentially.\n\nLet's summarize the scoping rules of `let*`.\nAfter that, we will do a handful of exercises to test your understanding of `let`, `letrec`, and `let*`.\n"
    },
    "intro_local": {
      "confirm": "In this tutorial, we will learn about *local binding forms*: `let` and `letrec` and `let*`.\n\nWe will first focus on `let`. The following program *binds* `x` to `2` and `y` to `3`, and evaluates\n`(+ x y)`. The result of running this program is `5`.\n\n```\n(let ([x 2]\n      [y (+ 2 1)])\n  (+ x y))\n```\n\nIn SMoL, `let` binds the *left-hand-side variables*\n(in this case, `x` and `y`) to\nthe values of the *right-hand-side expressions* (in this case, `2` and `(+ 2 1)`)\nsuch that the *body* (in this case, `(+ x y)`)\ncan refer to all those left-hand-side variables.\n\nFurthermore, the bindings are *local*,\nwhich means we can't refer to the `let`-bound variables outside the `let`.\n\nFor example, the following program produces `4 10`.\n\n```\n(defvar x 10)\n(let ([x 4])\n  x)\nx\n```\n"
    },
    "reflect_let": {
      "prompt": "What is the scope of a left-hand-side variable of `let`?"
    },
    "shadow": {
      "answer": "15",
      "feedback": "The top-level `x` is `3`.\nThe top-level `y` is bound to the value of `x` in the `let`, which is `5`.\nSo, the product is `15`.\n",
      "program": "(defvar x 3)\n(defvar y (let ([x 5]) x))\n(* x y)\n",
      "again": {
        "answer": "60",
        "program": "(defvar a 30)\n(defvar b (let ([a 2]) a))\n(* a b)\n"
      },
      "misconceptions": {
        "25": {
          "feedback": "The `let` binds `x` to `5` in its body.\nBut in the top-level environment, `x` is still bound to `3`.\n",
          "misconception": "Other"
        },
        "9": {
          "misconception": "Other"
        }
      }
    },
    "swap_let": {
      "answer": "#(3 2)",
      "feedback": "The top-level environment binds `x` to `2` and `y` to `3`.\n\nNeither `(+ y 1)` nor `(+ x 1)` refers to the `x` and `y` bound by the `let`. So, they both refer to the top-level definitions.\n",
      "program": "(defvar x 1)\n(defvar y 2)\n\n(let ([x (+ y 1)]\n      [y (+ x 1)])\n  (mvec x y))\n",
      "again": {
        "answer": "#(20 10)",
        "program": "(defvar a 1)\n(defvar b 2)\n\n(let ([a (* b 10)]\n      [b (* a 10)])\n  (mvec a b))\n"
      },
      "misconceptions": {
        "#(3 4)": {
          "misconception": "Other"
        },
        "error": {
          "misconception": "Other"
        }
      }
    },
    "swap_letrec": {
      "answer": "error",
      "feedback": "`(+ y 1)` refers to the local `y`, which is not bound to a value (but a `💣`) when `(+ y 1)` is evaluated.\n",
      "program": "(defvar x 1)\n(defvar y 2)\n\n(letrec ([x (+ y 1)]\n         [y (+ x 1)])\n  (mvec x y))\n",
      "again": {
        "answer": "error",
        "program": "(defvar a 1)\n(defvar b 2)\n\n(letrec ([a (* b 10)]\n         [b (* a 10)])\n  (mvec a b))\n"
      },
      "misconceptions": {
        "#(3 2)": {
          "misconception": "Other"
        },
        "#(3 4)": {
          "misconception": "Other"
        }
      }
    },
    "swap_letstar": {
      "answer": "#(3 4)",
      "feedback": "The top-level environment binds `x` to `2` and `y` to `3`.\n\n`(+ y 1)` refers to the top-level `y` while `(+ x 1)` refers to the local `x`.\n",
      "program": "(defvar x 1)\n(defvar y 2)\n\n(let* ([x (+ y 1)]\n       [y (+ x 1)])\n  (mvec x y))\n",
      "again": {
        "answer": "#(15 25)",
        "program": "(defvar a 7)\n(defvar b 5)\n\n(let* ([a (+ b 10)]\n       [b (+ a 10)])\n  (mvec a b))\n"
      },
      "misconceptions": {
        "#(3 2)": {
          "misconception": "Other"
        },
        "error": {
          "misconception": "Other"
        }
      }
    },
    "syntax_error": {
      "answer": "error",
      "feedback": "This program has a syntax error!\nIt should say `([foo 41])` rather than `(foo 41)`.\n",
      "program": "(let (foo 41)\n  (+ foo 1))\n",
      "again": {
        "answer": "error",
        "program": "(let (x 2)\n  (+ x 1))\n"
      },
      "misconceptions": {
        "42": {
          "misconception": "Other"
        }
      }
    },
    "warmup_local": {
      "answer": "6 5",
      "feedback": "This program includes two blocks: the top-level block and the body of the `let` expression.\nThe first addition (i.e., `(+ x 1)`) refers to the local `x`, the `x` defined in the body of the `let` expression.\nSo, it produces the value of `(+ 5 1)`, which is `6`.\nIn contrast, the second addition (i.e., `(+ x 2)`) refers to the top-level `x`.\nSo, it produces the value of `(+ 3 2)`, which is `5`.\n",
      "program": "(defvar x 3)\n(let ([x 5])\n  (+ x 1))\n(+ x 2)\n",
      "again": {
        "answer": "5 12",
        "program": "(defvar vr 7)\n(let ([vr 1])\n  (+ vr 4))\n(+ vr 5)\n"
      },
      "misconceptions": {
        "4 5": {
          "misconception": "Other"
        },
        "4 7": {
          "misconception": "Other"
        },
        "6 7": {
          "misconception": "Other"
        }
      }
    },
    "warmup_nested": {
      "answer": "32",
      "feedback": "",
      "program": "(let ([x 2])\n  (let ([y 30])\n    (+ x y)))\n",
      "again": {
        "answer": "9",
        "program": "(let ([a 10])\n  (let ([b 1])\n    (- a b)))\n"
      }
    }
  }
}}
