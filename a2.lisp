; Copyright 2019 Nathan Klapstien

; A2Expr - a more formal definition
;
;     * An integer is an A2Expr
;     * x is an A2Expr
;     * If E1 and E2 are A2Expr, then (+ E1 E2), (- E1 E2) and (* E1 E2) are
;       A2Expr
;     * Nothing else is an A2Expr

; check if the A2Expr list element is a valid operator `+`, `-`, or `*`
(defun valid-A2Expr-op (Op)
  (or
    (eq '+ Op)
    (eq '- Op)
    (eq '* Op)))

; check if the A2Expr element is x
(defun valid-A2Expr-x (E)
  (eq 'x E))

(defun valid-A2Expr-int (E)
  "Check if the A2Expr element is a integerp
E: the A2Expr element to check"
  (integerp E))

(defun valid-A2Expr-var (E)
  "Check if the A2Expr element is valid
E: the A2Expr element to check"
  (or
    (valid-A2Expr-int E)
    (valid-A2Expr-x E)
    (valid-A2Expr-list-format E)))

; check if the A2Expr element is a valid list element
; e.g. `(+ E1 E2)`
(defun valid-A2Expr-list-format (L)
  (and
    (listp L)
    (= 3 (LIST-LENGTH L))
    (valid-A2Expr-op (nth 0 L))
    (valid-A2Expr-var (nth 1 L))
    (valid-A2Expr-var (nth 2 L)))))

(defun A2Expr-zero-subtract-simplify (L)
  (if (valid-A2Expr-list-format L)
    (if (eq '- (nth 0 L))
      (if (eq 0 (nth 2 L))
        (nth 1 L)
        L)
      L)
    L))

(defun A2Expr-zero-plus-simplify (L)
  (if (valid-A2Expr-list-format L)
    (if (eq '+ (nth 0 L))
      (if (eq 0 (nth 1 L))
        (nth 2 L)
        (if (eq 0 (nth 2 L))
          (nth 1 L)
          L))
      L)
    L))

(defun A2Expr-one-multiply-simplify (L)
  (if (valid-A2Expr-list-format L)
    (if (eq '* (nth 0 L))
      (if (eq 1 (nth 1 L))
        (nth 2 L)
        (if (eq 1 (nth 2 L))
          (nth 1 L)
          L))
      L)
    L))

(defun A2Expr-simplify-list (L)
  (if (valid-A2Expr-var L)
    (if (valid-A2Expr-list-format L)
      (A2Expr-one-multiply-simplify (A2Expr-zero-subtract-simplify (A2Expr-zero-plus-simplify L)))
      L)
    NIL))

(defun A2Expr-simplify (L)
  (if (valid-A2Expr-var L)
    (if (valid-A2Expr-list-format L)
      (A2Expr-simplify-list (list (nth 0 L) (A2Expr-simplify (nth 1 L)) (A2Expr-simplify (nth 2 L))))
      L)
    NIL))


;#1 (2 marks)
;
;Write a Lisp function:
;
; (remove-identities E)
;
; The input E can be any valid A2Expr. This function should replace any term of
; the form (+ 0 Exp), (+ Exp 0), (* 1 Exp), (* Exp 1) by Exp. It should apply
; this simplification recursively to all nested expressions. It should not
; change E in any other way, and return the simplified A2Expr.
;
; See remove-identities Examples in public tests.
(defun remove-identities (E)
  ())


; #2 (2 marks)
;
; Write a Lisp function:
;
; (simplify-zeroes E)
;
; The input E can be any valid A2Expr. This function should replace any term of
; the form (* 0 Exp), (* Exp 0) or (- Exp Exp) by 0. It should apply this
; simplification recursively into all nested expressions. It should not change E
; in any other way, and return the simplified A2Expr.
;
; See simplify-zeroes Examples in public tests.
(defun simplify-zeroes (E) ())


; #3 (1 mark + 1 possible bonus mark)
; #3.1 (1 mark)
;
; Write a Lisp function:
;
; (simplify E)
;
; This function should repeatedly call
; 1. remove-identities, followed by
; 2. simplify-zeroes,
; until no more simplification is possible in either step 1 or 2.
;
; See simplify Examples in public tests.
(defun simplify (E) ())


; #3 bonus question (1 bonus mark)
;
; Does the order of calls to remove-identities and simplify-zeroes in simplify
; matter? Could we get a different final result in simplify if we called these
; two functions in a different order?
;
;     If your answer is yes, give an example with two different call sequences
;     with different results.
;     If your answer is no, give a good logical argument for why.


;;;;;; Discussion: How to get More Simplifications?
;
; The simplify function above is still very limited in what it can do. For
; example, the A2Expr
; (- (* (+ x 5) (- x 5) ) (* x x))
; is mathematically equivalent to -25. However, our simplify cannot see that.
; We could add more and more simplification rules similar to the cases above,
; and hope to catch more and more cases that way. Fortunately, we do not have
; to do that. There is a nice general way to simplify all A2Expr.
; PExpr as a Compact Representation of Polynomials
;
; A polynomial in variable x is a function of x. In mathematics, it is usually
; written in the form
;
; anxn+an-1xn-1+... +a2x2+a1x+a0
;
; We call the ak the coefficients and the powers of x the exponents.
;
; In principle, we can write any polynomial as an A2Expr. However, it gets messy
; and hard to read pretty quickly. For example, an A2Expr corresponding to
; 5x2+3x+7 would be (+ (* 5 (* x x)) (+ (* 3 x) 7)).
;
; PExpr are a compact way of representing polynomials in Lisp as a list of dotted
; pairs of coefficients and exponents, (ak . k).
;
; For example, 5x2+3x+7 written as a PExpr is ((5 . 2) (3 . 1) (7 . 0)).
;
; A PExpr P is said to be in normal form if:
;
;     The dotted pairs in P are in strictly decreasing order of exponent
;     (this implies there are no duplicate exponents in the list).
;     There are no zero coefficients
;     An empty list represents the constant polynomial of value 0.
;
; For example, ((5 . 10) (3 . 4) (7 . 0)) is in normal form, but the following are not:
;
; ((5 . 2) (7 . 0) (3 . 1)) - exponents not in decreasing order
; ((5 . 3) (0 . 2) (3 . 1) (7 . 0)) - zero coefficient in (0 . 2)
; ((0 . 0)) - zero coefficient


; #4 (2 marks)
;
; Write a Lisp function:
;
; (normalize P)
;
; The input P is an arbitrary PExpr. The output is the normal form of P.
; See the normalize examples in public tests.
(defun normalize (P) ())


; #5 (5 marks)
;
; Write a Lisp function:
;
; (polynomial E)
;
; The input E is an arbitrary A2Expr. The output is the equivalent
; PExpr of E in normal form. First implement the three helper functions
; in 5.1 and 5.2.
(defun polynomial (E) ())


; #5.1 (1 mark)
;
; Write two Lisp functions:
;
; (poly-add P1 P2)
;
; (poly-subtract P1 P2)
;
; For these two functions, the inputs P1 and P2 are PExpr in normal form.
; The output should be the sum (for poly-add) or difference (for poly-subtract)
; of the two PExpr, also in normal form. See poly-add, poly-subtract examples
; in public tests.
;
; Hints:
; normalize is your friend...
; If n is an integer, then (- n) computes its negative value. Note the space.
(defun poly-add (P1 P2) ())


(defun poly-subtract (P1 P2) ())


; #5.2 (2 marks)
;
; Write a Lisp function:
;
; (poly-multiply P1 P2)
;
; Again, the inputs P1 and P2 as well as the output are PExpr in normal form.
; See poly-multiply examples in public tests.
;
; Hints: To compute the product of two PExpr, "multiply them out" and normalize
; the result.
; Example for illustration only in math-like syntax:
; (x+1)*(x-1) = x^2 - x + x - 1 = x^2 + (-1 + 1)x -1 = x^2 - 1
; Real examples in PExpr for the assignment: see public tests.
;
; The function cartesian in sample code list-functions.lisp is an example with
; similar structure, which may help you with the recursion over both P1 and P2.
(defun poly-multiply (P1 P2) ())


; #5.3 (2 marks)
;
; Implement polynomial, using the three helper functions.
;
; Hint: use recursion, with the base cases:
; 1. integer n - represent by (n . 0)
; 2. atom x - represent by (1 . 1)
;
; See polynomial examples in public tests.


; #6 (2 marks) Printing a PExpr in normal form
;
; Write a Lisp function:
;
; (print-pexpr P)
;
; The input P is a PExpr in normal form. Output a string representing P in the
; following "common sense" format:
;
;     Print terms in the form cx^n, where c and n are integer
;     Print " + " (space plus space) or " - " (space minus space) between the
;     terms, depending on whether the next term has a positive or negative
;     coefficient
;     Print terms in sorted order from highest to lowest exponent
;     Do not print the 1 if the coefficient is 1, except for the constant term
;     Print -, not -1 if the coefficient is -1, except for the constant term
;     Do not print *x^0 for a constant term
;     Print x instead of x^1
;     Print 0 if the PExpr is nil.
;
; See print-pexpr Examples in public tests, and string functions.
(defun print-pexpr (P) ())
