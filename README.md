# cmput_325_assignment_2

## Overview

In this assignment, you implement a restricted form of the classic problem of simplifying arithmetic expressions. You first implement two simplification operations, then a general algorithm to transform such expressions into a
normal form.

The main restriction is that we only deal with the operations `+`, `-`,
and `*`, and that the arguments are only integers and a single variable
symbol `x`. Therefore, all expressions eventually simplify to a polynomial
in `x`, with integer coefficients.

### Types of Expressions in this Assignment

We have two main types of expressions in this assignment:

  * Assignment 2 expressions or A2Expr are built as explained above
  * polynomials in `x` or `PExpr` are represented in a specific short form.

#### A2Expr - a more formal definition

  * An integer is an `A2Expr`
  * `x` is an `A2Expr`
  * If `E1` and `E2` are `A2Expr`, then ``(+ E1 E2)``, ``(- E1 E2)``
  and ``(* E1 E2)`` are `A2Expr`
  Nothing else is an `A2Expr`
