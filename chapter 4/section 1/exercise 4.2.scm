The problem: Louis wants to place procedure application before assignment and definition
in the order of the conditional block that constitutes eval.

a - What is wrong with this plan?
The predicate application? will return true if the expression it is passed is a pair.
As given in the hint, (define x 3) will clearly return #t from pair?. As we can see from
eval as written, this will attempt to apply the evaluation of the symbol define within
the given environment. As eval is recursively called, it will recognize the symbol
define and attempt to evaluate a definition with the ONLY information being define,
which means an attempt to extract a definition-value from the exp will fail, as all
eval-definition receives is a list like ('define).

b - Change the syntax within eval to require procedure application to begin with
a symbol 'call, i.e. instead of (factorial 3), we (call factorial 3), or (+ 1 2)
becomes (call + 1 2).

See exercise 4.2 code.scm for this implementation.
