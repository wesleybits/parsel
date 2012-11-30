# Parsel!

Parsel is my attempt at a functional text parsing library that aught to be 
easy to use and resistant to failure.  It is also a small project for myself
to explore monads.  As it stands now, string parsing is tested and works, 
somewhat, but file parsing is implemented and remains untested.

At the heart of Parsel is the Parsel object which implements simple pattern-
matching methods along with `unit` and `run`.  The Parsel class handles
reader state, implements the `or` and `and` logic operators, the `then` 
operator, and honors Scala's for-comprehension features.

## Using Parsel

In the `parser.scala` source file you'll find the `test` object, which is
an example of how Parsel is intended to be used.  Let's look into it a
little more deeply.

At the top, `test` defines a series of case classes that inherit from the same
empty base class, then makes a type alias.  This pattern is beyond important
when using Parsel for type-safety reasons.  Let's look at the type alias for
`test`:

        type Token = Parsel[Expr]

Token defines a Parsel type with an Expr inside, where Expr is the shared 
parent to `test`'s case classes.  The type parameter for Parsel is defined as
prefectly invariant, so the types within our Parsels must be perfectly the 
same when we start applying logical operators to them.  I did this so not too 
much type information is lost when parsing large and complex documents.
Doing something like this will save you from rediculous amounts of frustration
when trying to deal with Parsel.

Next are a series of `match<something>` methods defined like so:

        def matchFloat:Token =
          for {
            i  <- atLeastOneOf( digits )
            d  <- oneOf( "." )
            r  <- manyOf( digits )
          } yield ExprFloat( (i + d + r).toFloat )

Here we see one special constant and three of the pattern matching functions
defined by Parsel.  The above expression is equivalent to "[\d]+\.[\d]*" in
regexp, but this definition method allows for recursive patterns (we'll see 
those later).

The constants are:

* `digits`: a string of all the digits from 0 to 9
* `lowerCase`: all the lower case letters in the English alphabet
* `upperCase`: all the upper case letters in the English alphabet
* `whitespace`: all the white space characters I can think of

The pattern matching methods all exhibit the same behaviors, besides EOF which
accepts no arguments.  They accept a string containing the characters you're 
matching against and returns a Parsel that contains either the largest string 
that it can match or an error, but don't worry about handling errors on your 
own, Parsel has special behaviors regerding them.

The pattern-matching methods are:

* `EOF`: matches only the end of file, returns a string with a single null 
character.
* `oneOf`: matches only one character found in its argument
* `oneNotOf`: matches only one character not in its argument
* `manyOf`: matches zero or more characters in its argument
* `noneOf`: same as `manyOf`, but only characters not in its argument
* `atLeastOneOf`: matches one or more characters in its argument
* `atLeastOneNotOf`: the `noneOf` to `atLeastOneOf`
* `exactly`: exactly matches its argument

Later in `test` we find an example of a recursive pattern:

        def matchList:Token =
          for {
            expr <- matchExpr
            rest <- expr match {
                      case ExprClose() => unit(ExprList(Nil))
                      case _           => matchList
                    }
          } yield expr match {
            case ExprClose() => rest
            case _           => rest match {
              case ExprList(l) => ExprList( expr :: l )
              case _           => expr
            }
          }

Here we see `unit` in use.  `unit` is a monadic operator that will allow you to
use it's argument in a for-comprehension in a neuteral way.  Parsel's `unit` 
will wrap it's argument in a Parsel instance, which, when addressed, will not 
alter the reader's state.

The next precedure down we see the parser's top level:

        def matchExpr:Token = 
          for {
            expr <- manyOf( whitespace ) then (
                      matchSymbol or
                      matchFloat or
                      matchInt or
                      matchDelimeter )
            res  <- expr match {
                      case ExprOpen() => matchList
                      case _          => unit(expr)
                    }
          } yield res

`unit` is used again, but we know what it does already.  A couple of the 
logic operators are used here, too.  Parsel deals with them like this:

* `A or B`: A and B must wrap the same type.  If A should match, skip B, 
otherwise rewind the reader's state to before A's attempt and try B.
* `A and B`: A and B must wrap the same type.  If A should match, try B
with the new reader state after matching A, throwing away A's result, 
otherwise skip B and fail outright.
* `A then B`: A and B can wrap completely different types.  Whether or not
A matches is completely immaterial, just try A and then send the altered
reader state to B for its try, throwing away A's result, if any.

After designing a beautiful set of patterns to match, you can get things 
rolling and change that nasty text file into a nice and lovely data structure
with:

        val res = 
          run ("some raw text") {
            matchExpr
          } match {
            case Right(v) => { println ("HORAY!"); v }
            case Left(v)  => { println ("ONOES an error: " + v); 
                               /* do something about it */ 
                             }
          }

Or:

        val file = new File("someFile.txt")
        val res = run (file) {
          matchExpr
        } ...

## How does it work?

If you don't know what a monad is, here's some reading:

* [Monads Are Elephants](http://james-iry.blogspot.com/2007/09/monads-are-elephants-part-1.html)
* [You Could Have Invented Monads!](http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html)

If monads are still hazey, then I find a stiff dose of Haskell helps, which is
how I sussed them out.  Knowing monads is important here, because Parsel is 
a monad.  When you're defining patterns to match, you're actually composing
functions, especially when you're using the logic operators on
your patterns or using for-comprehensions.  Its reader state is actually a
sequence of text, being either a singleton list for raw text, or a lazy list
of text lines read in from a BufferedReader.  This allows Parsel to remember
when in the parsing it failed and, if an alternative pattern exists, go back
in time to an earlier state to try a different one.

If you like this, then also check out [Parsec](www.haskell.org/haskellwiki/Parsec), which is pretty much the same thing for Haskell (seeing that Parsel is a
Parsec clone).  As with any new software, use with caution and post 
problems/bugs/feature-requests here on this git.

## Misc

To build you'll need SBT, so get it and fire it up in this directory.  Later
I might push a pre-built jar onto Maven if there are enough users.