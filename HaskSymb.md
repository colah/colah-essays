
To The Heart of Symbolic Algebra, With ViewPatterns and QuasiQuoters
====================================================================


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

This is still ugly, though. Thankfully, the GHC extention **ViewPatterns** can save us! It allows us to run functions on things before pattern matching (syntax: `func -> pattern`).

```haskell
{-# LANGUAGE ViewPatterns #-}

aboutExpr (normalize -> a :+: b :+: c :+: d) = "4+ sum at top"
aboutExpr (normalize -> a :+: b :+: c      ) = "3  sum at top"
...
```

It's a very usefull design pattern. In fact, normalize can be writen better with it, to.

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

The exact type we desire for `match` requires a bit of consideration. Clearly, the first argument is a `String`, the next a `Expr`, but what is the result? We can't use tuples, as we do above, because the number of variables matched may vary. So a list of `Expr`s. But we need to be able to represent failure, so `Maybe [Expr]`.

```haskell
match :: String -> Expr -> Maybe [Expr]
```

Allowing for things like:

```haskell
expand (match "a*(b+c)" -> [a,b,c]:_ ) = expand (a :*: b) + expand (a :*: c)
```

Now, at this point, our original `Expr` definition is rather silly. It was motivated by the syntax of its pattern matches being close to our normal mathematical syntax, but that's no longer relevant. Instead, let us build a data type that more closesly matches the data's nature.

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

While we're at it, let's also use **OverloadedStrings** to make variable construction easy. :)

```haskell
{-# LANGUAGE OverloadedStrings #-}

fromString s = Var s
```

Now we can write something like `x*(y+1)` as `"x"*("y"+1)`. Yay! Isn't that nice?

Back to our evil plot, though. We need to implement `match`.

Implementing `match`
--------------------

Fundamentally, we have two tasks in implementing `match`. The first is to parse the expression. The second is to construct the matcher.

We will be using two libraries to make this easier.

The first is the well known *Parsec*, a combinatoric parsing library. It seems redundant to remind the reader of its API.

The second is *PatternPower*. It was written to solve more general pattern matching problems. The following are patterns in it:

```haskell
Wild
Free "a"
ListPat [Commutative] [Free "a", Const 1, Free "b", Wild]
```





