---
title: "Learning Haskell by Making Tetris"
date: 2023-07-18
layout: post
categories: 
tags: 
---


# Table of Contents

1.  [Beginning at the End](#org363df3e)
2.  [What This Is](#orgb71b324)
3.  [What This Isn&rsquo;t](#org59f8b97)
4.  [Prelude](#org131a065)
5.  [Strategy](#orgc0ec4e1)
6.  [Imports and Dependencies](#org83dae88)
7.  [Establishing the Grid](#org139ee46)
8.  [Making Some Tetrominos](#org89330e4)
9.  [Representing the Game State](#org508bf49)
TODO: grep for TODO and resolve


<a id="org363df3e"></a>

# Beginning at the End

![img](/img/tetriskell.gif)  
*We&rsquo;ll make this over the course of the tutorial*


<a id="orgb71b324"></a>

# What This Is

This post is a hands-on introduction to Haskell via the implementation of a little-known game involving falling blocks, because that&rsquo;s how I first learnt the basics. I&rsquo;ll try explain Haskell-specific concepts in detail, such that an audience of competent programmers with no Haskell or even functional programming familiarity could follow it and end up with a passing understanding of how to build a simple Haskell application.

I&rsquo;ll explicitly try to overexplain everything, either in prose or in comments.

We&rsquo;ll end up with a minimal terminal implementation of Tetris, and a simple agent playing using [beam search](https://en.wikipedia.org/wiki/Beam_search).


<a id="org59f8b97"></a>

# What This Isn&rsquo;t

I won&rsquo;t touch on package management or project structure - in fact, this post is a literate<sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup> Haskell file, and the concatenated code blocks can be run via `runhaskell tetris.hs`. There are plenty of tutorials on package managers like Stack and Cabal, and on general project management out there - for now, all you need is whatever Haskell distribution your machine uses. [GHCup](https://www.haskell.org/ghcup/) is as good a place to start as any. You might already even have `runhaskell` on your machine.

We&rsquo;ll try to use as few external dependencies as possible, and won&rsquo;t use any language extensions.

There are a lot of ways one could write this code more cleanly and performantly - avoiding passing around explicit state using monad transformers like `StateT`, being more careful around the use of strictness versus laziness, and so on - I&rsquo;m considering this out of scope and will try keep it as simple as I can.


<a id="org131a065"></a>

# Prelude

I watched the [Tetris](https://en.wikipedia.org/wiki/Tetris_(film)) movie this week. There&rsquo;s this almost certainly apocryphal scene where Alexey Patjinov is demoing his creation to a publisher, who has a [&ldquo;drop the &lsquo;the&rsquo;&rdquo;](https://www.youtube.com/watch?v=PEgk2v6KntY) moment and suggests all completed rows should vanish at once, rather than one at a time, enabling the achievement of the four-lines-at-once namesake move. They swiftly hack the feature together on a tiny monochrome display, and I was reminded how lucky I am to live in an era of rich tooling, expressive languages, and 4K monitors.

When I was first learning Haskell, though, it felt like punching holes in cards. I couldn&rsquo;t get my head around the interplay between the purity of the language and the need to interact with the real world. A long while before, I&rsquo;d grokked Gary Bernhardt&rsquo;s [Functional Core, Imperative Shell](https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell) message, but how does this apply in a world where, supposedly, **everything** is functional? As we&rsquo;ll see, the Haskell equivalent is something like &ldquo;functional core, `IO` shell&rdquo; - but we&rsquo;re getting ahead of ourselves. I wrote [my own toy implementation](https://github.com/harryaskham/tetriskell) as a way of getting to grips with the language, and thought I&rsquo;d revisit it, rewriting it piece-by-piece in notebook style.


<a id="orgc0ec4e1"></a>

# Strategy

-   We&rsquo;ll represent the play area as a [`Map`](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html) (think: a tree-backed dictionary) whose keys are coordinates, and whose values are the contents of a grid cell, where the y-coordinate grows from top to bottom.
-   We&rsquo;ll make this 10x24, to allow for a 4-row buffer at the top in which to place new pieces.
-   Pieces themselves will begin life in a 4x4 grid, and remain that way until they get fixed to the board.
-   This lets us implement both collision detection and bounds checks on falling pieces by stepping forward (either by gravity or user input), looking for overlap, and simply rejecting the new game state.
-   Eventually, we&rsquo;ll have three threads running:
    -   One to progress the game state (lower the blocks, delete full rows, update score, etc)
    -   One to draw the game to the screen
    -   One to accept user input and act on it
-   We&rsquo;ll also implement a simple bot that simulates a few blocks ahead and optimises for keeping the grid as low as possible.


<a id="org83dae88"></a>

# Imports and Dependencies

We&rsquo;ll start with the imports we need. Haskell is &ldquo;batteries included&rdquo; in so far as there are a tonne of widely used, canonical core libraries - but you need to make them available on your system. For example, we&rsquo;ll be using [`Map`](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html) a lot, which is part of the `containers` package. This needs to be available on your system - there are myriad ways of doing this, but simplest is just running `$ cabal install containers`. You might be using Stack, or Nix, or something else. The full list of packages we need here are:

-   `base`
-   `containers`
-   `random`
-   `random-shuffle`

If you&rsquo;re following along, you&rsquo;ll want to install them all:

`cabal install --lib base containers random random-shuffle`

Versioning is a whole other topic. We aren&rsquo;t using any unstable features of these packages, so I&rsquo;ve not suggested pinning any particular versions, but just know it&rsquo;s often useful to do so do avoid dependency hell. A good package manager will help you here in a real project (I use Nix for everything).

Alright, so say we&rsquo;ve got our `tetris.hs` blank slate. This is going to be a single-file program, so we&rsquo;ll put everything into a monolithic `Main` module.

{% highlight haskell %}
-- Every Haskell source file begins with a module definition like this.
module Main where

-- There are lots of Map-related methods; a qualified import avoids naming
-- clashes, and means we can look things up using `M.lookup` rather than
-- simply `lookup`.
-- Ignore the 'Strict' for now - laziness/strictness is out of scope here.
import Data.Map.Strict qualified as M

-- By also importing the Map type directly, we don't need to constantly
-- specify `M.Map` and can just use `Map` directly in our type signatures.
import Data.Map.Strict (Map)

-- Other things we'll need throughout
import Data.List (intercalate, foldl')
import Data.Function ((&))
import System.Random (RandomGen, split, newStdGen)
import System.Random.Shuffle (shuffle')
import Control.Monad (forM_)
{% endhighlight %}


<a id="org139ee46"></a>

# Establishing the Grid

Now let&rsquo;s think about how we&rsquo;ll represent the game state, the entities within it, and the actions we can take.

We&rsquo;ll need a 2D grid of cells, each of which can be empty or filled with a block, and that block . Whenever you have state in this &ldquo;one-of-many&rdquo; form, where you might reach for an enum, in Haskell you can define a sum type:

{% highlight haskell %}
:{
-- This is a sum type; we can now use these colour values directly in our code.
-- Yes, we'll be using British English.
data Colour = Blue | Orange | Yellow | Green | Purple | Red | Cyan

-- Another sum type; we either have a block of a certain colour, or empty space.
data Cell = Block Colour | Empty
:}
{% endhighlight %}

Now we&rsquo;re ready to set up our grid:

{% highlight haskell %}
:{
-- This is a type alias - now any time we want a 2-dimensional coordinate,
-- we can use V2 rather than continually specify that we're representing
-- x and y as a tuple of Ints.

-- You get this and more for free in the `linear` package as `Linear.V2`
--- but I want to avoid as many dependencies as possible.
type V2 = (Int, Int)

-- Rather than use a 2D array-of-arrays, we'll just use
-- a map keyed by our ~V2~ coordinate type, whose values are of our `Cell` type.
-- We use a new datatype here rather than an alias, as this will later allow us to
-- attach new behaviour to the Grid in the form of typeclass instances.
-- This gives us a constructor function `Grid`, which accepts a width, height, and
-- `Map` as its arguments and gives us back a value of type `Grid`. That the
-- constructor has the same name as the type is just convention.
data Grid = Grid Int Int (Map V2 Cell)
:}
{% endhighlight %}

And our first function, a simple constructor:

{% highlight haskell %}
:{
-- Right, our first function - this will construct us an empty grid.
-- It's a fairly common pattern to prefix constructors like this with 'mk'.

-- You can think of a `Map` as a list of key-value pairs where it's efficient
-- to pick out any one pair by its key; it's easy to switch back and forth
-- between these `Map` and list-of-pairs representations, and it's an easy
-- way to construct them.

-- The type signature follows the :: and here simply says we take no arguments,
-- and return an instance of the Grid type. Typically, for top-level functions
-- like this, you include a type signature before the implementation, even
-- though the compiler can often figure it out itself.

-- We use a list comprehension to create the `(V2, Cell)` pairs of the grid, and
-- pass this to M.fromList to get our `Map V2 Cell`, i.e. our `Grid`.
-- Note that Haskell range sugar is inclusive, so [1 .. 3] is [1, 2, 3].

-- The $ operator applies the function on the left of it (in this case `Grid`) to
-- everything on the right; it's a common way of avoiding Lisp-style parenthesis
-- overload.
mkEmptyGrid :: Int -> Int -> Grid
mkEmptyGrid width height =
  Grid width height
    $ M.fromList [((x, y), Empty) | x <- [0 .. width - 1] , y <- [0 .. height - 1]]
:}
{% endhighlight %}

Let&rsquo;s get some output going. We&rsquo;re going to want to be able to pretty-print a bunch of our entities (our grids, our scoreboard) - when we want to implement the same broad concept across multiple disparate types, we draw for a typeclass (similar to a trait in Rust, or maybe an interface in Go). We&rsquo;ll define a `Pretty` typeclass - any type that implements this will be convertable to a nicely formatted `String`<sup><a id="fnr.2" class="footref" href="#fn.2" role="doc-backlink">2</a></sup> which we can later print to the screen<sup><a id="fnr.3" class="footref" href="#fn.3" role="doc-backlink">3</a></sup>.

Here `a` is a placeholder for the type that will implement the `Pretty` class. We&rsquo;re simply saying that anything prettifiable must define a `pretty` function that spits out a nice `String` representation. Very hand-wavily, Haskell&rsquo;s type signatures are written this way as all functions can be partially applied and are curried by default; for now, a function with a signature of `foo :: a -> b -> c -> d` can be thought of as a three argument function taking an `a`, a `b`, a `c` and returning a `d`.

{% highlight haskell %}
:{
class Pretty a where
  pretty :: a -> String
:}
{% endhighlight %}

We can make `Cell` an instance of this typeclass simply by associating each cell with a character. We can use Haskell&rsquo;s pattern-matching to have `pretty` behave differently depending on whether it&rsquo;s given an `Empty` cell or a `Block` cell. We can also cheat a little, and make the `Pretty` representation of a `Colour` be a terminal escape code we can use to give colour to the blocks by using it as a prefix.

{% highlight haskell %}
:{
instance Pretty Colour where
  pretty Red = "\x1b[31m"
  pretty Blue = "\x1b[34m"
  pretty Cyan = "\x1b[36m"
  pretty Yellow = "\x1b[33m"
  pretty Green = "\x1b[32m"
  pretty Purple = "\x1b[35m"
  pretty Orange = "\x1b[37m"
:}
{% endhighlight %}

TODO: Why are colours being deleted on export?

{% highlight haskell %}
:{
instance Pretty Cell where
  pretty Empty = "."
  pretty (Block colour) = pretty colour <> "█"
:}
{% endhighlight %}

The `<>` is shorthand for `mconcat` - a member of the `Monoid` typeclass, which roughly represents things that can be empty, and can be joined together. `String` is a `Monoid` so `<>` just concatenates them.

We&rsquo;re ready to prettify our `Grid`. Since we&rsquo;re operating over collections of things, we can start using higher-order functions; in Haskell, `fmap` from the `Functor` typeclass lets you apply a function to the inhabitants of any instance of a given `Functor`. A list is an instance of `Functor`, and so for some list `xs`, `fmap f xs` just operates like the `map(f, xs)` function you find over lists in most other languages.

Helper functions and intermediate values defined in `where` blocks are available in the above scope. Type signatures are optional, but I&rsquo;ve included them for clarity - they can also help the compiler tell you when you&rsquo;ve gone off track. I&rsquo;ve included some alternative equivalent implementations of `prettyRow` here; I won&rsquo;t keep doing this, but it gives you a sense of the different ways one can construct functions.

We use `M.!` to look up keys in our grid; this is unsafe, and can throw an error. A nicer way would be to use `M.lookup`, which returns a `Maybe Cell` here, meaning we&rsquo;d have to handle the `Nothing` case (i.e. out of bounds) and the `Just cell` case separately. We know we&rsquo;re within bounds here, so we&rsquo;ll keep it simple, but it&rsquo;s worth knowing.

{% highlight haskell %}
:{
instance Pretty Grid where
  pretty (Grid width height grid) = intercalate "\n" (prettyRow <$> rows) -- <$> is just an inline fmap
    where
      rows :: [[Cell]]
      rows = [[grid M.! (x, y) | x <- [0 .. width - 1]] | y <- [0 .. height - 1]]
      prettyRow :: [Cell] -> String
      prettyRow row = concatMap pretty row
      -- Alternative implementations:
      -- With eta-reduction:
      -- prettyRow = concatMap pretty
      -- With explicit fmap:
      -- prettyRow row = concat (fmap pretty row)
      -- Using the fmap "spaceship" operator:
      -- prettyRow row = concat (pretty <$> row)
      -- Using the Monad instance of List (don't worry, it just aliases concatMap):
      -- prettyRow row = pretty =<< row
:}
{% endhighlight %}

Here we&rsquo;ve converted back from our `Map` representation of the `Grid` to a `List`-based one, in order to more easily convert it to a list of `String` that we can join (`intercalate` in Haskell) together with newlines inbetween.

We can finally print our grid! It&rsquo;s nothing special, but here we go:

{% highlight haskell %}
:{
putStrLn $ pretty (mkEmptyGrid 10 24)
:}
{% endhighlight %}

    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........
    ..........

Alright!


<a id="org89330e4"></a>

# Making Some Tetrominos

Let&rsquo;s make the pieces. We&rsquo;ll represent them as another product type with a colour and coordinates, and take advantage of Haskell&rsquo;s laziness to construct an infinite stream of pieces, in chunks of seven, where each of the seven chunks is a shuffled collection containing every piece (per the **official rules**). This&rsquo;ll let us easily draw the next piece, as well as enabling a simple lookahead for a next-piece preview.

We&rsquo;ll encode the actual shapes by the coordinates of their full blocks, letting us specify their colour as well. We&rsquo;ll use some helpers to let us quickly set coloured blocks on an empty grid. Eventually we&rsquo;ll have a function that transforms a `Grid` into a copy of itself containing one new coloured block - we can then `fold` this function, using an empty 4x4 grid as the initial state, over the coordinates of the piece, which will add the blocks one by one, giving us the finished piece.

{% highlight haskell %}
:{
data Piece = Piece Colour [V2]

pieceL :: Piece
pieceL = Piece Orange [(1, 3), (1, 2), (1, 1), (2, 3)]

pieceR :: Piece
pieceR = Piece Blue [(1, 3), (1, 2), (1, 1), (2, 1)]

pieceSquare :: Piece
pieceSquare = Piece Yellow [(1, 3), (1, 2), (2, 3), (2, 2)]

pieceS :: Piece
pieceS = Piece Green [(0, 3), (1, 3), (1, 2), (2, 2)]

pieceZ :: Piece
pieceZ = Piece Red [(0, 2), (1, 2), (1, 3), (2, 3)]

pieceT :: Piece
pieceT = Piece Purple [(0, 3), (1, 3), (2, 3), (1, 2)]

pieceLine :: Piece
pieceLine = Piece Cyan [(1, 3), (1, 2), (1, 1), (1, 0)]

allPieces :: [Piece]
allPieces = [pieceL, pieceR, pieceSquare, pieceS, pieceZ, pieceT, pieceLine]

-- Here we have a lazy infinite list of pieces.
-- To avoid requiring side-effects here, we take a random state as an argument.
-- Later, when we're inside the IO monad, we can hook into this source of randomness
-- and pass it in; by avoiding this here, we can keep this function pure.
-- The shuffle API is a little odd, so we need to handle splitting the random state
-- ourselves otherwise every chunk of seven pieces will be the same.
pieceStream :: RandomGen g => g -> [Piece]
pieceStream g =
  let (_, g') = split g -- obtain a new random generator for the recursive call
   in shuffle' allPieces (length allPieces) g <> pieceStream g'
:}
{% endhighlight %}

Now we need some functions for composing a `Piece` and a `Grid`, both for inspection and later, for placing tetrominos on the playing field.

{% highlight haskell %}
:{
-- By only passing the first argument here, we get back a partially applied
-- function; this is a new function of type `Grid -> V2 -> Grid` which is
-- exactly what we need for our fold. It's a bit of an awkward argument
-- ordering for anything other than a fold.
withBlock :: Colour -> Grid -> V2 -> Grid
withBlock colour (Grid width height grid) xy =
  Grid width height (M.insert xy (Block colour) grid)

-- Adds a whole piece to the grid one block at a time
withPiece :: Piece -> Grid -> Grid
withPiece (Piece colour coordinates) grid = foldl' (withBlock colour) grid coordinates

-- Here the (&) operator is just the reverse of ($) - everything to the
-- right is applied to the left. Useful for builder functions like these.
mkPieceGrid :: Piece -> Grid
mkPieceGrid piece = mkEmptyGrid 4 4 & (withPiece piece)
:}
{% endhighlight %}

Whew, okay. Let&rsquo;s give ourselves a nice way of inspecting these pieces - we&rsquo;ll use this for things like next piece preview. We can just pretty-print the containing grid; here we use point-free style to omit the argument. The `(.)` operator composes functions right-to-left, so since we want to first convert to a grid, and then pretty-print, we can write:

{% highlight haskell %}
:{
instance Pretty Piece where
  pretty = pretty . mkPieceGrid
:}
{% endhighlight %}

Notice how we take our grid as an argument, and return ostensibly a new one; in some languages this would be expensive, but Haskell&rsquo;s functional data structures make this a cheap operation, and let us pass around and create updated versions of state without needing to worry about mutation. We can just think in terms of pure transformations of our entities.<sup><a id="fnr.4" class="footref" href="#fn.4" role="doc-backlink">4</a></sup>

Let&rsquo;s see if we got that right by pretty-printing these pieces:

{% highlight haskell %}
:{
-- `mapM_` just lets us run a function with side effects and with no return value
-- like `putStrLn` over a collection of things.
-- Because we're in IO, we can make use of the system's source of randomness via newStdGen
 do
   g <- newStdGen
   let pieces = pieceStream g
       batch1 = take 7 pieces
       batch2 = take 7 (drop 7 pieces)
       prettyBatch i batch = "Batch " <> show i <> ":\n" <> intercalate "\n\n" [pretty piece | piece <- batch]
   forM_
     (zip [1..] [batch1, batch2])
     (putStrLn . uncurry prettyBatch)
:}
{% endhighlight %}

    Batch 1:
    ....
    ....
    .██.
    ██..
    
    ....
    ....
    ██..
    .██.
    
    ....
    ....
    .█..
    ███.
    
    .█..
    .█..
    .█..
    .█..
    
    ....
    .█..
    .█..
    .██.
    
    ....
    .██.
    .█..
    .█..
    
    ....
    ....
    .██.
    .██.
    Batch 2:
    ....
    ....
    .██.
    ██..
    
    ....
    .█..
    .█..
    .██.
    
    .█..
    .█..
    .█..
    .█..
    
    ....
    ....
    .█..
    ███.
    
    ....
    ....
    ██..
    .██.
    
    ....
    .██.
    .█..
    .█..
    
    ....
    ....
    .██.
    .██.

Looks good to me - each batch of seven represents all pieces, and each is separately shuffled.

We introduced a number of new concepts here; we secretly entered a monad (`IO`, specifically), enabling the `do`-notation you see above, and giving us the ability to enact the useful side effect of being able to print to the screen. In fact, we&rsquo;ve been doing this all along with every call to `putStrLn`. We&rsquo;ll get into `IO` more later when we start dealing with user input and multiprocessing.

We also introduced `uncurry` - we wanted to pass the tuples of form `(1, batch1)` we&rsquo;d created via `zip` into a function that wanted arguments `1 batch1` - `uncurry` will convert a function that wants two arguments into a function that wants a tuple of those two arguments<sup><a id="fnr.5" class="footref" href="#fn.5" role="doc-backlink">5</a></sup>.

While we&rsquo;re here, let&rsquo;s implement piece rotation.

TODO


<a id="org508bf49"></a>

# Representing the Game State

Now we&rsquo;ll create the type we&rsquo;ll be using to store all state about the ongoing game. This is a product type; rather than &ldquo;one-of-many&rdquo;, this represents a collection of many simultaneous values. You can think of it like a struct.

{% highlight haskell %}
:{
data Game = Game {
  grid :: Grid,
  score :: Int
}
-- TODO more
:}
{% endhighlight %}

TODO

# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> Okay, not quite. I&rsquo;m writing this in Emacs, where `org-babel` will run each block in GHCi, a Haskell interpreter, with `set +m` enabled to allow multiline blocks. The whole thing gets compiled to Markdown via `ox-jekyll`. The end result is the same, more or less, as writing actual literate code.

<sup><a id="fn.2" href="#fnr.2">2</a></sup> You&rsquo;ll typically be recommended to eschew `String` (which is a linked list of characters) for the more efficient `Text` type; we don&rsquo;t need to worry about this for a toy application.

<sup><a id="fn.3" href="#fnr.3">3</a></sup> There&rsquo;s already the `Show` typeclass that does exactly this, and which can be automatically derived for many types, but I tend to think of it as for debugging and inspection purposes - I prefer a separate typeclass for representations intended to be user-facing.

<sup><a id="fn.4" href="#fnr.4">4</a></sup> The use of `foldl'` here does two things: we fold from the left (irrelevant in this case, but important sometimes), and we fold strictly - that is, we don&rsquo;t accumulate a load of unevaluated thunks and overflow the stack. Again, never going to happen in our toy example, but worth knowing.

<sup><a id="fn.5" href="#fnr.5">5</a></sup> It gets more complex when you&rsquo;re dealing with more arguments (`uncurry3` and so on exist).
