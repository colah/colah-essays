Monads For The Terrified
========================

What Is This Craziness?!? (The Section You Can Ignore)
-------------------------------------------------------

People learning Haskell generally seem to get tripped up about monads. There are good reasons for this. 

 1. Monads are, really, needed to write a serious Haskell program. As a consequence, people generally try to learn them fairly shortly after beginning to use Haskell.
 2. Monads are defined in Haskell and have a very short definition. As such, it is very tempting to not just teach someone how to use monads, but to also try to teach them the underlying definition. The definition involves a bunch of rather advanced Haskell. In combination with (1), this is a big problem.
 3. Monads are a very abstract idea. I'm not really sure that the motivation can be properly grasped by anything other than using them.
 4. Monads are generally interweaved with the idea of an IO object.

This introduction attempts to avoid some of these problems. In particular, it is only going to teach you *how to use monads* and not *what they are*. This avoids (2) and, to a lesser extent, (3). I've tested this approach on a number of people in the last few months, and it seems to be quite effective. That said, I understand that some people may prefer an introduction focusing on the *what* and *why*, in which case you may wish to look Chris Smith's [Why Do Monad's Matter?](http://cdsmith.wordpress.com/2012/04/18/why-do-monads-matter/).

Some day, you will need to learn more. But, if you find you understand the contents of this tutorial, I'd encourage you to wait and play around with monads for a while. Then, when the time comes to learn the in full, they'll be natural and obvious ideas. We can talk more about that later. For now: onwards!

Outing List
-----------

If you're like most programmers I know, lists are very close friends of yours. In fact, I think we all love lists. I don't know how I'd get by without them.

However, lists haven't told you something. They know that most people are really scared of monads and don't want you to be scared of them. But lists are monads. In fact, if you've ever written code like `[a*b | a <- [1,2,3,4], b <- [1,2]]`, you've used them as monads!

(It took several hours of persuasion to make lists feel comfortable telling you this. Please remind them you still love them!)

In Haskell, lists aren't just one type. We can have a list of strings (`[String]`), a list of integers (`[Int]`), a list of tuples of integers and strings (`[ (Int, String) ]`), or... Well, you get the idea. All monads do this: they wrap around a type and make a new type. 

But the main part of what a monad is, is generalizing things like `[a*b | a <- [1,2,3,4], b <- [1,2]]`. That syntax is specific to lists, the more general syntax is a **do block**. If we rewrite that expression as a do block, it is:

```haskell
do
	a <- [1,2,3,4]
	b <- [1,2]
	return (a*b)

-- Output: [1,2,2,4,3,6,4,8]
```

Now, in some cases, we may not want to use one of the variables at the end...

```haskell
do
	a <- [1,2,3,4]
	b <- [1,2]
	return b

-- Output: [1,2,1,2,1,2,1,2]
```

In which case, we don't even have to make the variable in the first place:

```haskell
do
	[1,2,3,4]
	b <- [1,2]
	return b

-- Output: [1,2,1,2,1,2,1,2]
```

Notice that one can also nest do blocks:

```haskell

do
	a <- ['a','b']
	b <- do
		a2 <- [1,2]
		b2 <- [0,1]
		return (a2+b2)
	return (a,b)

-- Output: [('a',1),('a',2),('a',2),('a',3),('b',1),('b',2),('b',2),('b',3)]

```

because that is the same as

```haskell

foo = do
	a2 <- [1,2]
	b2 <- [0,1]
	return (a2+b2)

-- foo = [1,2,2,3]

do
	a <- ['a','b']
	b <- foo -- that is: b <- [1,2,2,3]
	return (a,b)

-- Output: [('a',1),('a',2),('a',2),('a',3),('b',1),('b',2),('b',2),('b',3)]

```


OK, So What About Monads?
---------------------------

Well, that's a do block. A monad is something that wraps around a type and can be used to build do blocks.

Not a very satisfying answer? After all, you ask, *what does a do block actually mean*? What does it mean that we can make a do block?

... Ah, that we can make a do block? You see, being a monad is really just that we've told Haskell what do blocks mean for our monad. **We could have defined the behaviour however we wanted, as long as it created coherent do blocks!**

A do block can be thought of as stringing objects together and allowing the 'content' of earlier ones in deciding what later ones are. But what it means to "string objects together" or what the "content" is entirely decided at the declaration of the monad. (For example, someone decided what a do block for a list should mean... And now that's what it means.)

**Monads abstract a structure, not an idea.**


Maybe = Baby List
-----------------

OK. Let's introduce a new monad! You may not have met it before, but Maybe is the baby cousin of List. You'll become friends quick!

Imagine you want to write a lookup function for a hash. Hopefully, we'll find the value for the key we give and the function can return that value, but sometimes we won't be able to. Now, you think, my good friend list will help me! If I succeed, I'll return a singleton list. If I fail, I'll return an empty list.

```haskell
-- What you are likely thinking!!

-- we make a dictionary
dict = fromList [('a',1), ('b',2)]

-- we can try and look things up

-- A singleton if we succeed
lookup dict 'a' -- [1]
-- An empty if we fail
lookup dict 'c' -- []
```

Haskell programmers like to have a specific idea corresponding to their types. At this point, we're thinking of lists in a very strange way. And the idea we're thinking of is something we'll often want to do.

So, we define Maybe. Like a list, Maybe's wrap around a type -- for example, if we want to represent a type that may be an integer, we use `Maybe Int`.

A Maybe value can be a `Nothing`, corresponding to an empty list `[]`, or a `Just a`, corresponding to a singleton list `[a]`.

We could even define a function transforming a maybe value into a list:

```haksell
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just a) = [a]
```

In do blocks, `Maybe`s behave just like their corresponding lists.

```haskell
do
	a <- Just 1 -- Think [1]
	b <- Just 2 -- Think [2]
	return (a+b)
-- Just 3 -- Think [3]

do
	a <- Just 1  -- Think [1]
	b <- Nothing -- Think []
	return (a+b)
-- Nothing -- Think []
```


IO Objects
-----------

When I explain to people that Haskell is a purely functional language, that functions have no side effects, there's a minute before the penny drops. Then they start wondering: how do you do IO?

Then answer they get is usually one word: monads. I think this is a large part of the reason that people find monads so intimidating -- "What Dark Magic they must be, to circumvent purity like that!". As an answer, it's also a little bit economical with the truth. You don't *need* monads to do IO in Haskell. You need IO objects. Monads are just really useful for building IO objects!

In a lot of languages, like C, the program beings execution at a `main` function and calls other things from there. Haskell programs also declare a `main` object, but it's not a function! It's an IO object.

It's also not really accurate to think of `main` as where the execution 'starts'. `main` is your program. IO objects represent something that can interact with the outside world: give output, take input, react to input with output based on it... And `main` is the IO object that is your program. Everything else you write is there to build it.

For example:

```haskell

-- putStrLn :: String -> IO()

sayHi :: IO ()
sayHi = putStrLn "hi"

main = sayHi

```

Now, you ask, why is it `IO ()` and not just `IO`? The answer is that an IO object can be something that, if it is ever run, will give us something. For example, `getLine` is an `IO String` -- if it is ever run, it gives back a string! But what about things like `putStrLn` that don't give anything back? We say they give back the empty tuple `()` or, in other words, nothing.

So, at this point it makes sense that we *can* do IO. The question becomes, how do we do *complicated* IO? Obviously, we need a way to build more complicated IO objects! As you've probably guessed, IO objects are monads and we're going to use do blocks to build complicated ones!

```haskell
do                                             -- An object that, if ever run, will:
	putStrLn "enter your name:"                -- Print some text
	name <- getLine                            -- get a line of input
	putStrLn "Thanks for entering your name!"  -- print some more text
	putStrLn ("Your name is " ++ name)         -- print more based on the input
	return ()                                  -- Let's not returning anything
```



Final Questions
-----------------


**When should I seak out more knowledge about monads?**

When you feel the urge to start making your own monads. Or after a few months of using them.

When you do, I strongly recommend Chris Smith's [Why Do Monad's Matter?](http://cdsmith.wordpress.com/2012/04/18/why-do-monads-matter/). It really helps one understand the role of monads in computer science and why we care about them.

**How do I learn the Widom of the Monad?**

Work with monads. With time, they will become a part of the way you think. 

The Parsec library uses monads in a particularily brilliant way, and is also one of the most amazing pieces of code I've ever used. I strongly recommend playing with it. An example, from Real World Haskell's [chapter on Parsec](http://book.realworldhaskell.org/read/using-parsec.html), of a slightly simplified CSV file parser to give you a taste:

```haskell
csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n")
eol = char '\n'
```

**I found a do block without a return at the end!**

When there isn't a return, the content of the last object in the block is returned. 

For example:

```haskell
do
	a
	b
```

Is the same thing as:

```haskell
do
	a
	bval <- b
	return bval
```

**What does (>>=) mean? What about (>>)?**

If possible, ignore them for now.

If not, think of them as short hands for things you often want to do in do blocks.

`a >> b` is the same thing as

```haskell
do
	a
	b
```

And `a >>= b` is the same as

```haskell
do
	aval <- a
	b aval
```

**Anything else?**

Yes. Familiarize yourself with the Monad library, `Control.Monad`. It contains all sorts of goodies! I'll be going through some personal favourites, but you should look through the documentation!

`sequence [a, b, c, d...]` is the same thing as

```haskell
do
	a' <- a
	b' <- b
	c' <- c
	d' <- d
	...
	return [a', b', c', d'...]
```

Also useful is `sequence_`, for example `sequence_ [a, b, c, d...]` is equivalent to:

```haskell
do
	a' <- a
	b' <- b
	c' <- c
	d' <- d
	...
	return ()
```

A classic example of using this would be something like `sequence_ $ map putStrLn $ ["abc", "def", "boo!"]` which would result, if run, in the output:

```
abc
def
boo!
```

Another favourite is `forM_`. It is best demonstrated by an example:

```haskell
forM_ [1,2.. 30] $ \n -> do
	putStrLn $ "n = " ++ show n
	when (n < 10) $
		putStrLn "Also, n is lower than 10!!"
```

Which does exactly what you think!

I'll leave the rest for you to discover by yourself.
