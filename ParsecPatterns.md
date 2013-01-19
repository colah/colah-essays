Parsec Patterns
===============

At the last meeting of the Toronto Haskell User Group (Jan 9, 2013), after going over the basics of combinatoric parsing library Parsec, I talked about some clever tricks that made my work with it much cleaner.

The Branch and Help Combinators
-------------------------------

In parsec, sequential parsing is done using monadic blocks. For example:

```hs
do
   string "abc"
   string "def"
```

would parse "abcdef".

A parsing branch, where we can parse one thing or another, is represented with the `<|>` operator. For example, to parse "abc" or "def", we could write:

```hs
string "abc" <|> string "def"
```

Now, the default behavior of Parsec is to not backtrack in parsing for efficiency reasons. For example, `string "abc" <|> string "adef"` could never successfuly parse "adef" because it wouldn't backtrack from successfully parsing the leading 'a' with `string "abc"`.

To circumvent this behavior, we have the `try` function which takes a parser and backtracks if it fails. For example, we could get the desired behavior from the previors example by writing:

```hs
try (string "abc") <|> string "adef"
```

This try-branch behavior is extremely, overwhelmingly common, at least in the parsers I write. But it gets ugly really fast. This is especially true when you mix monadic code in because, for reasons unknown to me, Haskell does not allow functions to take a do block directly as an argument.

```hs

-- Not allowed!
a = try do
	string "abc"
	string "def"

-- Allowed!
a = try (do
	string "abc"
	string "def" )

-- Allowed!
a = try $ do
	string "abc"
	string "def"

```

That last one, you say, doesn't look to bad. And indeed it doesn't. The problem is when you throw in a branch. Of course, the following is not valid:

```hs
-- Not allowed!
a = 
	try do
		string "abc"
		...
	<|> do
		string "adef"
		...
```

You need to do something like:

```hs
a = 
	(try (do
		string "abc"
		...
	)) <|> do
		string "adef"
		...
```

Or, a little bit cleaner.

Eww.

As evryone knows, brackets are icky and have cooties, especially when they surround code blocks. And especially when they're nested.

We can make it a bit cleaner with a `$` operator.

```hs
a = 
	(try $ do
		string "abc"
		...
	) <|> do
		string "adef"
		...
```

But it still rapidly becomes gross as the code grows.

... But wait, there's more!

One of the neatest features of Parsec is its capacity to give intelligent error messages. This is largely dependant on people giving human readable names to different things that can be parsed with the `<?>` operator. For example, `many1 digit <?> "integer"`.

But how does that mix into our new code? Well...

```hs
a = 
	((try $ do
		string "abc"
		...
	<?> "fooA"
	) <|> do
		string "adef"
		...
	<?> "fooB"
	) <?> "foo"
```

Super pretty code, right? Yeah, that's what I thought.

Well, since we generally want branches to put a try around the left hand branch, let's make an operator, `*<|>`, that does that.

```hs
infixr 1 *<|>
a *<|> b = try a <|> b
```

For example, 

```hs
a = 
	(try $ do
		string "abc"
		...
	) <|> do
		string "adef"
		...
```

Can now be written as:

```hs
a = 
	do
		string "abc"
		...
	*<|> do
		string "adef"
		...
```

Pretty!

It gets better, though. We create a `?:` for providing help info.

```hs
infixr 2 ?:
l ?: p = p <?> l
```

We can use it to describe what blocks of code are before we write them.

```hs
a = 
	"abc thingmagigy" ?: do
		string "abc"
		...
	*<|> "adef mbober" ?: do
		string "adef"
		...
```

To see how this can improve code, have a look at this [before](https://github.com/colah/ImplicitCAD/blob/2183ff43a1cf0209b3ac79a253de20f5f5de873d/Graphics/Implicit/ExtOpenScad/Parser/Expr.hs) and [after](https://github.com/colah/ImplicitCAD/blob/master/Graphics/Implicit/ExtOpenScad/Parser/Expr.hs) from [ImplicitCAD](http://implicitcad.org/)'s extopenscad expression parser.

Smart Whitespace
----------------

Extopenscad for statments are things like `for (a = [1,2,3]) ...`. Here's a slightly simplified version of the original parser for them:

```hs
forStatement = do
	string "for"
	genSpace
	string "("
	genSpace
	pattern <- patternMatcher
	genSpace
	string "="
	genSpace
	vexpr <- expression
	genSpace
	string ")"
	genSpace
	loopContent <- suite
	return $ For pattern vexpr loopContent
```

(`genSpace` is generalized space -- possibly multiple spaces, tabs, newlines, comments...)

You may notice that we need to specify whitespace everywhere. We can massively clean this up with the following functions, `stringGS`, which matches string literals normally except for matching spaces as generalized whitespace.

```hs
stringGS (' ':xs) = do
	x' <- genSpace
	xs' <- stringGS xs
	return (x' ++ xs')
stringGS (x:xs) = do
	x' <- char x
	xs' <- stringGS xs
	return (x' : xs')
stringGS "" = return ""
```

We can now rewrite our for statement code as:

```hs
forStatement = do
	stringGS " for ( "
	pattern <- patternMatcher
	stringGS " = "
	vexpr <- expression
	string " ) "
	loopContent <- suite
	return $ For pattern vexpr loopContent
```

Much nicer! I'm sure you can imagine how much that cleaned up ImplicitCAD's statement code!



