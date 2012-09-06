
To The Heart of Symbolic Algebra, With ViewPatterns and QuasiQuoters
====================================================================

**(Very much rough and in progress! Not all the code works. Paper based on simplifying colah/HaskSymb.)**


One of the things that makes Haskell code so elegant is the ability to pattern match. In some lesser languages, one has to spend time breaking input into cases and extracting values, convoluting the code. But Haskell cuts right through this! A cannonical example of this is `fib`:

```haskell
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
```

There is nothing here besides our intentions, and there in lies the elegance.

Into the Breach
----------------

To prove our point, we will consider a tougher problem: symbolic algebra. We will need to create a data structure to represent expressions, naturally.

```haskell
data Expr = Const Int
          | Var   String
          | Add   Expr   Expr
          | Mult  Expr   Expr
```

(We restrict our selves to a relatively to the Polynomial Ring with Integer Coefficients, a relatively simple structure, primarily for brevity. One could easily generalize this.)

For example, `a + 2*b` would be `Add (Var "a") (Mult (Const 2) (Var "b"))`.

... And that's rather ugly...

We can make things significantly prettier if we use shorter names and operator constructors -- Haskell allows for operators beginning with a colon to be constructors.

```haskell
data Expr = C Int
          | V String
          | Expr :+: Expr
          | Expr :*: Expr
infixl 6 :+:
infixl 7 :*:
```

Now `a + 2*b` can be represented as `V "a" :+: C 2 :*: V "b"`, which is somewhat better.

Again, the real power is to pattern match:

```haskell
aboutExpr (a :+: b :+: c :+: d) = "4+ sum at top"
aboutExpr (a :+: b :+: c      ) = "3  sum at top"
aboutExpr (a :+: b            ) = "2  sum at top"
aboutExpr (a :*: b            ) = "mult at top"
aboutExpr a = "something else"
```

Yay! Let's test it.

```haskell
$ let a = (V "a" :+: V "b")
$ aboutExpr (a :+: a)
"3  sum at top"
```

... What?!?! 

Well, `a :+: b :+: c :+: d` is specifically matching `((a :+: b) :+: c) :+: d`, where as `a :+: a` is actually `(V "a" :+: V "b") :+: (V "a" :+: V "b")` -- just because we know they're the same thing, doesn't me Haskell does!

But take heart, friends, and have faith in Haskell. For GHC is our sheppard...

One Step Forward, Two Steps Back
--------------------------------

Well, we could normalize these things before hand. Define a function like this:

```haskell
normalize (a :+: (b :+: c)) = 
	case normalize (b :+: c) of
		(b' :+: c') -> normalize (a :+: b') :+: c'
normalize a = a
```

And then things like our `aboutExpr` could be written as:

```haskell
aboutExpr x = case normalize x of
	(a :+: b :+: c :+: d) -> "4+ sum at top"
	(a :+: b :+: c      ) -> "3  sum at top"
	...
```

This is still ugly, though. Thankfully, the GHC extension `ViewPatterns` can save us! It allows us to run functions on things before pattern matching (syntax: `func -> pattern`).

```haskell
{-# LANGUAGE ViewPatterns #-}

aboutExpr (normalize -> a :+: b :+: c :+: d) = "4+ sum at top"
aboutExpr (normalize -> a :+: b :+: c      ) = "3  sum at top"
...
```

It's a very useful design pattern. In fact, normalize can be written better with it, to.

```haskell
normalize (a :+: (normalize -> b :+: c)) = normalize (a :+: b) :+: c
normalize a = a
```

Generally speaking, if you see `f x y ... = case foo x y ... of`, it's worth considering if you can use a ViewPattern instead.


So, what comes after that? Let's try to write a function that is actually usefull.

```haskell
expand (normalize -> a :*: (b :+: c) ) = expand (a :*: b) + expand (a :*: c)
expand (normalize -> a :+: b) = expand a :+: expand b
exapnd a = a
```

But this doesn't expand `(2+x)*y`! Normalize can't really help us here -- we don't want complicated conventions about where products go --, we need another case: `(b :+: c) :*: a`. But that's ugly!

And it could be much worse in other cases! Recall that a list of length `n` with `x₁` duplicates of one element, `x₂` duplicates of another, etc, has `n!/(x₁!*x₂!*...)` permutations. So even something like `a*(b+c)*(d+e+f)` would require six cases to fully catch! Madness!

Still, we shall persevere.

A Cunning Plan
--------------

What if, instead of normalize, we preprocessed our arguments with a function that was aware of the pattern we sought to match. We would then write something like:

```haskell
expand (match "a*(b+c)" -> (a,b,c) ) = expand (a :*: b) + expand (a :*: c)
expand (match "a+b" -> (a,b)) = expand a :+: expand b
exapnd a = a
```

The exact type we desire for `match` requires a bit of consideration. Clearly, the first argument is a `String`, the next a `Expr`, but what is the result? We can't use tuples, as we do above, because the number of variables matched may vary. So a list of `Expr`s. But we need to be able to represent failure, so `Maybe [Expr]`. And, actually, there can be multiple answers (consider matching `2+1` to `a+b`: `[[2,1],[1,2]]`), so it should be a list of solutions instead of a `Maybe`.

```haskell
match :: String -> Expr -> [[Expr]]
```

Allowing for things like:

```haskell
expand (match "a*(b+c)" -> [a,b,c]:_ ) = expand (a :*: b) + expand (a :*: c)
```

Conveniences & Luxuries
-----------------------

Now, at this point, our original `Expr` definition is rather silly. It was motivated by the syntax of its pattern matches being close to our normal mathematical syntax, but that's no longer relevant. Instead, let us build a data type that more closely matches the data's nature.

```haskell
data Expr = Const Int
          | Var   String
          | Sum   [Expr]
          | Prod  [Expr]
          deriving Show
```

(We derive an instance of `Show` because writing a fancy instance is trivial and not terribly interesting. That said, I will be using a nicer one to actually display `Expr`s in this paper. Link to it is available at the end.)

To make constructing them nicer, let us make `Expr` a `Num` instance.

```haskell
instance Num Expr where
	fromInteger n = Const (fromInteger n)
	(Sum as) + (Sum bs) = Sum (as ++ bs)
	(Sum as) + b        = Sum (as ++[b])
	a        + (Sum bs) = Sum (a  :  bs)
	a        + b        = Sum [a,b]
	(Prod as) * (Prod bs) = Prod (as ++ bs)
	(Prod as) * b         = Prod (as ++[b])
	a         * (Prod bs) = Prod (a  :  bs)
	a         * b         = Prod [a,b]
	a - b = a + (-1*b)
	abs a = error "abs not implemented -- sorry, Num is silly"
	signum a = error "abs not implemented -- sorry, Num is silly"
```

While we're at it, let's also use `OverloadedStrings` to make variable construction easy. :)

```haskell
{-# LANGUAGE OverloadedStrings #-}

import GHC.Exts (IsString, fromString)

instance IsString Expr where
	fromString s = Var s
```

Now we can write something like `x*(y+1)` as `"x"*("y"+1)`. Yay! Isn't that nice?

Back to our evil plot, though. We need to implement `match`.

Implementing `match`
--------------------

Fundamentally, we have two tasks in implementing `match`. The first is to parse the expression. The second is to construct the matcher. Both will be done in one step.

We will be using two libraries to make this easier.

```haskell
import Text.Parsec
import 
```

The first is the well known *Parsec*, a combinatoric parsing library. It seems redundant to remind the reader of its API.

The second is *PatternPower*. It was written to solve more general pattern matching problems. The following are patterns in it:

```haskell
Wild
Free "a"
ListPat [Commutative] [Free "a", Const 1, Free "b", Wild]
```

We will implement the pattern matcher as a function on integers, with each integer representing a level of fixity.

The bottom, zero, level just strips whitespace.

```haskell
mathPat n@0 = do
	many space
	pat <- mathPat (n+1)
	many space
	return pat
```

The next level parses sums. We generate a pattern to commutatively match them, expanding or contracting the sum as necessary for the pattern.

```haskell
mathPat n@1 =
	( try $ do
		pats <- sepBy2 (mathPat (n+1)) (pad $ char '+')
		return $ PreProcess sumD $ 
			ListPat [Commutative, CompressExtra Sum, FillMissing 0] pats
	<|> (mathPat (n+1))
```

For the next level, we do essentially the same thing as before, for products.

```haskell
mathPat n@2 =
	( try $ do
		pats <- sepBy2 (mathPat (n+1)) (pad $ char '*')
		return $ PreProcess prodD $ 
			ListPat [Commutative, CompressExtra Prod, FillMissing 1] pats
	<|> (mathPat (n+1))
```

Finally, we parse variables, constants, and bracketed expressions (which we drop back down).

```haskell
mathPat n@3 = 
	(try $ fmap (Const'.Const.read) $ many1 digit)
	(try $ fmap  Free $ many1 letter)
	) <|> (try $ do
		char '('
		a <- mathPat 0
		char ')'
		return a
	)
```

To QuasiQuotation and Beyond!
-----------------------------

Now we are able to write silly things like this:

```haskell
expand (match "a*(b+c)" -> [a,b,c]:_ ) = expand (a :*: b) + expand (a :*: c)
```

And you know what? It's still silly. There's still duplicated work. Why are we naming variables in `"a*(b+c)"` and again in `[a,b,c]`? Because of silliness. Because we are working around a limitation of the language, instead of in an ideal syntax.

Thankfully, GHC is coming to our rescue again. This time, with the `TemplateHaskell` and `QuasiQuotes` extensions.

What is a QuasiQuoter, you ask? Well, it's a way to extend Haskell's syntax. You wrap your new language in `[quasiQuoterName|` and `|]` and transform it into a Haskell expression, pattern, type, or declaration.

In our case, we'll be declaring a `m` QuasiQuoter such that `[m| a*(b+c) |]` is equivalent to the pattern `match "a*(b+c)" -> [a,b,c]:_`.

Let's declare the QuasiQuoter. First we need some libraries.

```haskell
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
```

This gives us the `QuasiQuoter` constructor. It takes four arguments, each one describing how to work in a different case.

```haskell
m  =  QuasiQuoter 
	(error "m doesn't support generating expressions.")
	mPat
	(error "m doesn't support generating types.")
	(error "m doesn't support generating declarations.")
```

And now we just need to declare `mPat :: String -> TH.PatQ` (turns strings into Template Haskell patterns).

Now, do you remember our friend the pattern power library? It provides `finishPat :: SymbPat a -> PatQ` which can do the heavy lifting for us.

```haskell
mPat = finishPat . matchPat
```

Seeing Clearly
---------------

At the beginning, we considered the elegance of the canonical Haskell implementation of `fib`. It was elegant, because nothing was there except our intent. I leave you with these, which need no explanation, and are of the same nature.

```haskell
expand [m|  a+b  |] = expand a + expand b
expand [m|a*(b+c)|] = expand (a*b) + expand (a*c)
expand       a      = a

collectTerms [m| aC*x + bC*x + c |] = collectTerms $ (aC+bC)*x + c
collectTerms [m|    x + bC*x + c |] = collectTerms $ (1+bC) *x + c
collectTerms [m|    x +    x + c |] = collectTerms $  2     *x + c
collectTerms          a             =  a

diff var b = collectTerms . expand . diff' $ b
	where
		diff' [m| a + b |] = diff' a + diff' b
		diff' [m| a * b |] = a* (diff' b) + b* (diff' a)
		diff' [m|   aC  |] = 0
		diff'      expr 
		     | var == expr = 1
		diff'       _      = 0

```








