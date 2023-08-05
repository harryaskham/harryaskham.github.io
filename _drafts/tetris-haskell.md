---
title: "Learning Haskell by Building Tetris"
date: 2023-07-18
layout: post
categories: 
tags: 
---


# Table of Contents

1.  [Beginning at the End](#orgcd51ea8)
2.  [What This Is](#orgbbcc5ba)
3.  [What This Isn&rsquo;t](#orgfd214ec)
4.  [Prelude](#org9ffd53a)
5.  [Strategy](#org0515c96)
6.  [Imports and Dependencies](#orga21918c)
7.  [Establishing the Grid](#org4a373f2)
8.  [Making Some Tetrominos](#org8c353b7)
9.  [Rotations](#org7f3a22e)
10. [Placing Pieces on the Grid](#org3034eef)
11. [Representing the Game State](#orgc31503d)
12. [The Introduction of Time and Logic](#orgecf47ff)


<a id="orgcd51ea8"></a>

# Beginning at the End

![img](/img/tetriskell.gif)  

This is what we&rsquo;ll build over the course of this post<sup><a id="fnr.1" class="footref" href="#fn.1" role="doc-backlink">1</a></sup>.


<a id="orgbbcc5ba"></a>

# What This Is

This post is a hands-on introduction to Haskell via the implementation of a little-known game involving falling blocks, because that&rsquo;s how I first learnt the basics. I&rsquo;ll try explain Haskell-specific concepts in detail, such that an audience of competent programmers with no Haskell or even functional programming familiarity could follow it and end up with a passing understanding of how to build a simple Haskell application.

I&rsquo;ll explicitly try to overexplain everything, either in prose or in comments. I&rsquo;m also going to purposefully try to use a variety of different styles of Haskell programming.

We&rsquo;ll end up with a minimal terminal implementation of Tetris, and a simple agent playing using [beam search](https://en.wikipedia.org/wiki/Beam_search).


<a id="orgfd214ec"></a>

# What This Isn&rsquo;t

I won&rsquo;t touch on package management or project structure - in fact, this post is a literate<sup><a id="fnr.2" class="footref" href="#fn.2" role="doc-backlink">2</a></sup> Haskell file, and the concatenated code blocks (if written to `tetris.hs`) can be run via `runhaskell tetris.hs`. There are plenty of tutorials on package managers like Stack and Cabal, and on general project management out there - for now, all you need is whatever Haskell distribution your machine uses. [GHCup](https://www.haskell.org/ghcup/) is as good a place to start as any. You might already even have `runhaskell` on your machine.

We&rsquo;ll try to use as few external dependencies as possible, and won&rsquo;t use any language extensions.

There are a lot of ways one could write this code more cleanly and performantly - avoiding passing around explicit state using monad transformers like `StateT`, being more careful around the use of strictness versus laziness, and so on - I&rsquo;m considering this out of scope and will try keep it as simple as I can. There will be no catamorphisms, hylomorphisms, or other such morphisms here.


<a id="org9ffd53a"></a>

# Prelude

I watched the [Tetris](https://en.wikipedia.org/wiki/Tetris_(film)) movie this week. There&rsquo;s this almost certainly apocryphal scene where Alexey Patjinov is demoing his creation to a publisher, who has a [&ldquo;drop the &lsquo;the&rsquo;&rdquo;](https://www.youtube.com/watch?v=PEgk2v6KntY) moment and suggests all completed rows should vanish at once, rather than one at a time, enabling the achievement of the four-lines-at-once namesake move. They swiftly hack the feature together on a tiny monochrome display, and I was reminded how lucky I am to live in an era of rich tooling, expressive languages, and 4K monitors.

When I was first learning Haskell, though, it felt like punching holes in cards. I couldn&rsquo;t get my head around the interplay between the purity of the language and the need to interact with the real world. A long while before, I&rsquo;d grokked Gary Bernhardt&rsquo;s [Functional Core, Imperative Shell](https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell) message, but how does this apply in a world where, supposedly, **everything** is functional? As we&rsquo;ll see, the Haskell equivalent is something like &ldquo;functional core, `IO` shell&rdquo; - but we&rsquo;re getting ahead of ourselves. I wrote [my own toy implementation](https://github.com/harryaskham/tetriskell) as a way of getting to grips with the language, and thought I&rsquo;d revisit it, rewriting it piece-by-piece in notebook style.

**Please note** that I myself am a kind of &ldquo;expert beginner&rdquo; - I love the language but I&rsquo;m sure (in fact I know) there&rsquo;s a lot here that could be improved upon, even with the constraints of targetting a beginner audience. My email is in the footer and I welcome errata.


<a id="org0515c96"></a>

# Strategy

-   We&rsquo;ll build up from a play area, to the tetrominos, to the game logic, to user input, and finally to a self-playing bot.
-   We&rsquo;ll represent the play area as a [`Map`](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html) (think: a tree-backed dictionary) whose keys are coordinates, and whose values are the contents of a grid cell, where the y-coordinate grows from top to bottom.
-   We&rsquo;ll make this 10x24, to allow for a 4-row buffer at the top in which to place new pieces.
-   Pieces themselves will begin life in a 4x4 grid, and remain that way until they get fixed to the board.
    -   This lets us implement rotation, collision detection and bounds checks on falling pieces by stepping forward (either by rotating, by translation or by gravity), looking for overlap, and simply rejecting the new game state if we have overlapping blocks.
-   We&rsquo;ll build logic to move the game forward one &ldquo;step&rdquo; (apply gravity, fix blocks when they hit bottom, delete full rows, update the score, etc.)
-   Eventually, we&rsquo;ll have three threads running:
    -   One to progress the game state
    -   One to draw the game to the screen
    -   One to accept user input and act on it
-   We&rsquo;ll finally implement a simple bot that looks a few blocks ahead and optimises for keeping the grid as low as possible.


<a id="orga21918c"></a>

# Imports and Dependencies

We&rsquo;ll start with the imports we need. Haskell is &ldquo;batteries included&rdquo; in so far as there is a rich collection of widely used, canonical core libraries on [Hackage](https://hackage.haskell.org/) - but they don&rsquo;t come with the compiler. You need to make them available on your system. For example, we&rsquo;ll be using [`Map`](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html) a lot, which is part of the `containers` package. The glorious [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) needs you to install these libraries. There are myriad ways of doing this, but simplest might just be running `cabal install --lib <libname>`.

The full list of packages we need here are:

-   `base`
-   `containers`
-   `random`
-   `random-shuffle`
-   `monad-extras`

If you&rsquo;re following along, you&rsquo;ll want to install them all:

`cabal install --lib base containers random random-shuffle monad-extras`<sup><a id="fnr.3" class="footref" href="#fn.3" role="doc-backlink">3</a></sup>

Versioning is a whole other topic. We aren&rsquo;t using any unstable features of these packages, so I&rsquo;ve not suggested pinning any particular versions, but just know it&rsquo;s often useful to do so do avoid dependency hell in a real project. A good package manager<sup><a id="fnr.4" class="footref" href="#fn.4" role="doc-backlink">4</a></sup> (Cabal, Stack, Nix, others) will help you here.

Alright, so say we&rsquo;ve got our `tetris.hs` blank slate. This is going to be a single-file program, so we&rsquo;ll put everything into a monolithic `Main` module. This isn&rsquo;t great practice for serious projects, but for our purposes we can keep everything in `Main`.

{% highlight haskell %}
:{
-- Every Haskell source file begins with a module definition like this.
-- In your own project, you might have submodules like `module Server.API.Payments where`
-- to reflect the boring pragmatism of real-world engineering.
-- This would typically live at the path `lib/Server/API/Payments.hs`
-- In a Cabal project, this monolithic file would live in `app/Main.hs`.
module Main where
:}
{% endhighlight %}

I&rsquo;ll spell out each import we&rsquo;re using explicitly<sup><a id="fnr.5" class="footref" href="#fn.5" role="doc-backlink">5</a></sup>:

{% highlight haskell %}
:{
-- There are lots of Map-related methods; a qualified import avoids naming
-- clashes, and means we can look things up using `M.lookup` rather than
-- simply `lookup`.
-- Ignore the 'Strict' for now - laziness/strictness is a large and separate topic.
import qualified Data.Map.Strict as M
:}
{% endhighlight %}

{% highlight haskell %}
:{
-- By also importing the Map type directly, we don't need to constantly
-- specify `M.Map` and can just use `Map` directly in our type signatures.
import Data.Map.Strict (Map)
:}
{% endhighlight %}

{% highlight haskell %}
:{
-- `intercalate` is similar to Python's `x.join()`
-- `foldl'` is similar to Python's `reduce(f, xs)`
import Data.List (intercalate, foldl', intersect)
:}
{% endhighlight %}

{% highlight haskell %}
:{
-- This will let us easily modify 2-tuples (i.e. our coordinates)
import Data.Bifunctor (bimap)
:}
{% endhighlight %}

{% highlight haskell %}
:{
-- Reverse function application; allows e.g. `thing & withProperty a` pipelining.
import Data.Function ((&))
:}
{% endhighlight %}

{% highlight haskell %}
:{
-- Provides access to system pseudorandomness and control over setting random seeds.
import System.Random (RandomGen, split, newStdGen, getStdGen, mkStdGen)
:}
{% endhighlight %}

{% highlight haskell %}
:{
-- Utilities for shuffling collections (e.g. of tetrominos)
import System.Random.Shuffle (shuffle')
:}
{% endhighlight %}

{% highlight haskell %}
:{
-- We'll be making use of this module for control flow when we get to our
--imperative-looking (but still functional!) shell.
import Control.Monad (forM_)
:}
{% endhighlight %}

{% highlight haskell %}
:{
-- This will let us step our game state forward in time in a way that lets us
-- continually check for validity.
import Control.Monad.Extra (iterateM)
:}
{% endhighlight %}

{% highlight haskell %}
:{
-- We'll use these to make modifications to coordinates as we stick different
-- UI elements together.
import Control.Arrow (first, second)
:}
{% endhighlight %}


<a id="org4a373f2"></a>

# Establishing the Grid

Now let&rsquo;s think about how we&rsquo;ll represent the game state, the entities within it, and the actions we can take.

We&rsquo;ll need a 2D grid of cells, each of which can be empty or filled with a block, and that block . Whenever you have state in this &ldquo;one-of-many&rdquo; form, where you might reach for an enum, in Haskell you can define a sum type:

{% highlight haskell %}
:{
-- This is a sum type; we can now use these colour values directly in our code.
-- Yes, we'll be using British English.
data Colour = Blue
            | Orange
            | Yellow
            | Green
            | Purple
            | Red
            | Cyan
            | Black
            | White
            deriving (Eq)

-- Another sum type; we either have a block of a certain colour, or empty space.
-- We also insert the ability to display a char here because later, we'll use this
-- to compose some basic UI elements.
-- We derive Eq both times here so that we can later check for full cells by
-- inequality with Empty
data Cell = Block Colour | BlockChar Colour Char | Empty deriving (Eq)
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

-- This is just a helper we'll use later to pull out the underlying Map as needed.
unGrid :: Grid -> Map V2 Cell
unGrid (Grid _ _ grid) = grid
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

Let&rsquo;s get some output going. We&rsquo;re going to want to be able to pretty-print a bunch of our entities (our grids, our scoreboard) - when we want to implement the same broad concept across multiple disparate types, we draw for a typeclass (similar to a trait in Rust, or maybe an interface in Go). We&rsquo;ll define a `Pretty` typeclass - any type that implements this will be convertable to a nicely formatted `String`<sup><a id="fnr.6" class="footref" href="#fn.6" role="doc-backlink">6</a></sup> which we can later print to the screen<sup><a id="fnr.7" class="footref" href="#fn.7" role="doc-backlink">7</a></sup>.

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
  pretty Black = "\x1b[30m"
  pretty White = "\x1b[97m"
:}
{% endhighlight %}

{% highlight haskell %}
:{
ansiColorEnd :: String
ansiColorEnd = "\x1b[0m"

instance Pretty Cell where
  pretty Empty = " "
  pretty (Block colour) = pretty colour <> "█" <> ansiColorEnd
  pretty (BlockChar colour c) = pretty colour <> [c] <> ansiColorEnd
:}
{% endhighlight %}

The `<>` is shorthand for `mconcat` - a member of the `Monoid` typeclass, which roughly represents things that can be empty, and can be joined together. `String` is a `Monoid` so `<>` just concatenates them.

Since an empty grid is going to be quite boring to print, let us make a way of adding a border to a grid. We can use `BlockChar` with Unicode line and corner chars to surround a grid. Let&rsquo;s make this a typeclass too! That way, we can add borders to regular grid, but also to UI elements.

{% highlight haskell %}
:{

class Borderable a where
  withBorder :: a -> a

instance Borderable Grid where
  withBorder (Grid width height grid) =
    -- Create a new Grid with enough room for the UI elements
    Grid (width + 2) (height + 2)
      (grid
      & M.mapKeys (first (+1) . second (+1))  -- Shift every coordinate by (+1, +1)
      -- Then we insert the elements using the helpers below
      & withLeftBorder
      & withRightBorder
      & withTopBorder
      & withBottomBorder
      & withCorners)
    where
      -- First a helper to insert black characters at the given coordinates
      insertBlackChars char coordinates =
        M.union (M.fromList (zip coordinates (repeat (BlockChar Black char))))
      -- And now we use this over the four sides of the grid and the corner pieces.
      withLeftBorder = insertBlackChars '│' [(0, y) | y <- [0 .. height + 1]]
      withRightBorder = insertBlackChars '│' [(width + 1, y) | y <- [0 .. height + 1]]
      withTopBorder = insertBlackChars '─' [(x, 0) | x <- [0 .. width + 1]]
      withBottomBorder = insertBlackChars '─' [(x, height + 1) | x <- [0 .. width + 1]]
      withCorners = M.insert (0, 0) (BlockChar Black '┌')
                  . M.insert (width + 1, 0) (BlockChar Black '┐')
                  . M.insert (0, height + 1) (BlockChar Black '└')
                  . M.insert (width + 1, height + 1) (BlockChar Black '┘')
:}
{% endhighlight %}

We&rsquo;re ready to prettify our `Grid`. Since we&rsquo;re operating over collections of things, we can start using higher-order functions; in Haskell, `fmap` from the `Functor` typeclass lets you apply a function to the inhabitants of any instance of a given `Functor`. A list is an instance of `Functor`, and so for some list `xs`, `fmap f xs` just operates like the `map(f, xs)` function you find over lists in most other languages.

Helper functions and intermediate values defined in `where` blocks are available in the above scope. Type signatures are optional, but I&rsquo;ve included them for clarity - they can also help the compiler tell you when you&rsquo;ve gone off track. I&rsquo;ve included some alternative equivalent implementations of `prettyRow` here; I won&rsquo;t keep doing this, but it gives you a sense of the different ways one can construct functions.

We use `M.!` to look up keys in our grid; this is unsafe, and can throw an error. A nicer way would be to use `M.lookup`, which returns a `Maybe Cell` here, meaning we&rsquo;d have to handle the `Nothing` case (i.e. out of bounds) and the `Just cell` case separately. We know we&rsquo;re within bounds here, so we&rsquo;ll keep it simple, but it&rsquo;s worth knowing.

{% highlight haskell %}
:{
instance Pretty Grid where
  pretty (Grid width height grid) = intercalate "\n" (prettyRow <$> rows)
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
putStrLn $ pretty (withBorder $ mkEmptyGrid 10 24)
:}
{% endhighlight %}

Alright!

We&rsquo;ll hide the top four rows later on. For now it&rsquo;s useful to print the whole grid, as we&rsquo;ll use this to display our tetrominos too.


<a id="org8c353b7"></a>

# Making Some Tetrominos

Let&rsquo;s make the pieces. We&rsquo;ll represent them as a product type with a colour and coordinates, and take advantage of Haskell&rsquo;s laziness to construct an infinite stream of pieces, in chunks of seven, where each of the seven chunks is a shuffled collection containing every piece (per the **official rules**). This&rsquo;ll let us easily draw the next piece, as well as enabling a simple lookahead for a next-piece preview.

We&rsquo;ll encode the actual shapes by the coordinates of their full blocks, letting us specify their colour as well. We&rsquo;ll use some helpers to let us quickly set coloured blocks on an empty grid. Eventually we&rsquo;ll have a function that transforms a `Grid` into a copy of itself containing one new coloured block - we can then `fold` this function, using an empty 4x4 grid as the initial state, over the coordinates of the piece, which will add the blocks one by one, giving us the finished piece.

{% highlight haskell %}
:{
data Piece = PieceL
           | PieceR
           | PieceSquare
           | PieceS
           | PieceZ
           | PieceT
           | PieceLine
           deriving (Enum, Bounded)

-- We get this nicely for free from the Enum and Bounded instances.
allPieces :: [Piece]
allPieces = [minBound .. maxBound]
:}
{% endhighlight %}

Now we can specify piece properties using simple pattern-matched functions:

{% highlight haskell %}
:{
pieceColour :: Piece -> Colour
pieceColour PieceL = Orange
pieceColour PieceR = Blue
pieceColour PieceSquare = Yellow
pieceColour PieceS = Green
pieceColour PieceZ = Red
pieceColour PieceT = Purple
pieceColour PieceLine = Cyan

pieceCoords :: Piece -> [V2]
pieceCoords PieceL = [(1, 3), (1, 2), (1, 1), (2, 3)]
pieceCoords PieceR = [(1, 3), (1, 2), (1, 1), (2, 1)]
pieceCoords PieceSquare = [(1, 2), (1, 1), (2, 2), (2, 1)]
pieceCoords PieceS = [(0, 2), (1, 2), (1, 1), (2, 1)]
pieceCoords PieceZ = [(0, 1), (1, 1), (1, 2), (2, 2)]
pieceCoords PieceT = [(0, 2), (1, 2), (2, 2), (1, 1)]
pieceCoords PieceLine = [(0, 3), (1, 3), (2, 3), (3, 3)]
:}
{% endhighlight %}

And now we can generate our infinite stream of pieces lazily:

{% highlight haskell %}
:{
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

We will also need some notion of a falling piece; something combining colour and location:

{% highlight haskell %}
:{
-- We need a type to represent the actively falling piece that combines colour and coordinates.
data ActivePiece = ActivePiece Colour [V2]

-- We also want some way of converting a piece into an active piece, which can be
-- placed on a grid.
initPiece :: Piece -> ActivePiece
initPiece piece = ActivePiece (pieceColour piece) (pieceCoords piece)
:}
{% endhighlight %}

Now we need some functions for composing an `ActivePiece` and a `Grid`, both for inspection and later, for placing tetrominos on the playing field.

{% highlight haskell %}
:{
-- By only passing the first argument here, we get back a partially applied
-- function; this is a new function of type `Grid -> V2 -> Grid` which is
-- exactly what we need for our fold. It's a bit of an awkward argument
-- ordering for anything other than a fold.

-- Note that if the block is outside of bounds, we just don't render it.
-- This will make hiding the top rows easier later on.
withBlock :: Colour -> Grid -> V2 -> Grid
withBlock colour original@(Grid width height grid) (x, y)
  | x < 0 || x >= width || y < 0 || y >= height = original
  | otherwise = Grid width height $ M.insert (x, y) (Block colour) grid

-- Adds a whole piece to the grid one block at a time
withPiece :: ActivePiece -> Grid -> Grid
withPiece (ActivePiece colour coords) grid = foldl' (withBlock colour) grid coords

-- Here the (&) operator is just the reverse of ($) - everything to the
-- right is applied to the left. Useful for builder functions like these.
mkPieceGrid :: Piece -> Grid
mkPieceGrid piece = mkEmptyGrid 4 4 & (withPiece $ initPiece piece)
:}
{% endhighlight %}

Whew, okay. Let&rsquo;s give ourselves a nice way of inspecting these pieces - we&rsquo;ll use this for things like next piece preview. We can just pretty-print the containing grid; here we use point-free style to omit the argument. The `(.)` operator composes functions right-to-left, so since we want to first convert to a grid, and then pretty-print, we can write:

{% highlight haskell %}
:{
instance Pretty Piece where
  pretty = pretty . mkPieceGrid
:}
{% endhighlight %}

Notice how we take our grid as an argument, and return ostensibly a new one; in some languages this would be expensive, but Haskell&rsquo;s functional data structures make this a cheap operation, and let us pass around and create updated versions of state without needing to worry about mutation. We can just think in terms of pure transformations of our entities.<sup><a id="fnr.8" class="footref" href="#fn.8" role="doc-backlink">8</a></sup>

Let&rsquo;s see if we got that right by pretty-printing these pieces.

For fun, we&rsquo;ll implement `Monoid` for `Grid`; this just means defining what it means for a `Grid` to be empty, and how to stitch two grids together. However, just like `Int` (which can be combined multiple ways - summing, multiplying), there&rsquo;s no unique way to combine two grids - so let&rsquo;s implement both horizontal and vertical stitching. This will require some `newtype` wrappers - for example, we can&rsquo;t just do `2 <> 3 == ???` in Haskell, as it doesn&rsquo;t know which `Monoid` to use for the concatenation; instead we either:

-   `Sum 2 <> Sum 3 == Sum 5`
-   `Product 2 <> Product 3 == Product 6`

There&rsquo;s a practical use here; we&rsquo;ll use these `Monoid` instances to compose UI elements like the grid, the next piece preview, and the display of the held piece. When we concatenate two grids along an edge, we&rsquo;ll grow the shorter grid to match it. This is a design choice; if we didn&rsquo;t do this, we&rsquo;d still have a [lawful `Monoid`](https://en.wikibooks.org/wiki/Haskell/Monoids#Monoid_laws)<sup><a id="fnr.9" class="footref" href="#fn.9" role="doc-backlink">9</a></sup>, but it wouldn&rsquo;t be as useful for us.

A detail; a `Semigroup` is something that can be associatively combined - that&rsquo;s where the `<>` comes from (shorthand for `mconcat`). A `Monoid` is a `Semigroup` with an identity element (e.g. the empty grid - something you can combine either on the left or right, and get the same thing back). So to make something a `Monoid`, we first make it a `Semigroup`, then simply define what an empty one looks like. It goes like this:

{% highlight haskell %}
:{
-- This wrapper will represent the stitching of grids along the horizontal side.
-- That is, grid B is placed underneath grid A
newtype HGrid = HGrid { unHGrid :: Grid }

instance Semigroup HGrid where
  -- First we make a new empty grid with the correct dimensions
  -- Then we stitch the first grid with the second shifted down by the height of the first
  (HGrid (Grid widthA heightA gridA)) <> (HGrid (Grid widthB heightB gridB)) =
    let (Grid width height grid) = mkEmptyGrid (max widthA widthB) (heightA + heightB)
        combinedGrid = grid
          & M.union gridA
          & M.union ((second (+ heightA) `M.mapKeys` gridB))
     in HGrid $ Grid width height combinedGrid

instance Monoid HGrid where
  mempty = HGrid $ mkEmptyGrid 0 0

-- Let's make sure we can add borders to our composable UI elements:
-- Note that we could do this using Monofunctor and omap, but we'll be explicit.
instance Borderable HGrid where
    withBorder (HGrid grid) = HGrid $ withBorder grid

-- Let's also just make it easy to pretty-print our UI elements:
instance Pretty HGrid where
    pretty (HGrid grid) = pretty grid
:}
{% endhighlight %}

There&rsquo;s quite a bit going on here; essentially, we construct a new empty grid of combined height, and wide enough to accomodate both grids. The `unHGrid` named member just lets us easily unwrap this type later on.

Then we `M.unionWith` the original grid, copying over its elements.

Finally, we copy over the second grid - but this time, we increase all y-coordinates by the height of the first grid by first creating a partial function that increments the second member of a tuple (`second (+heightA))`) and using an `M.mapKeys` to bump all y-coordinates of the second grid to the correct locations.

Note that we use backticks to inline the function, since it&rsquo;s kind of standing in place of the `fmap` operator `(<$>)`<sup><a id="fnr.10" class="footref" href="#fn.10" role="doc-backlink">10</a></sup>.

Let&rsquo;s just test this quickly:

{% highlight haskell %}
:{
putStrLn . pretty . mconcat
  $ HGrid . withBorder . mkPieceGrid <$> [PieceL, PieceR, PieceS]
:}
{% endhighlight %}

    ┌────┐
    │    │
    │ █  │
    │ █  │
    │ ██ │
    └────┘
    ┌────┐
    │    │
    │ ██ │
    │ █  │
    │ █  │
    └────┘
    ┌────┐
    │    │
    │ ██ │
    │██  │
    │    │
    └────┘

Now the same for the `VGrid`:

{% highlight haskell %}
:{
newtype VGrid = VGrid { unVGrid :: Grid }

instance Semigroup VGrid where
  (VGrid (Grid widthA heightA gridA)) <> (VGrid (Grid widthB heightB gridB)) =
    let (Grid width height grid) = mkEmptyGrid (widthA + widthB) (max heightA heightB)
        combinedGrid = grid
          & M.union gridA
          & M.union ((first (+ widthA) `M.mapKeys` gridB))
     in VGrid $ Grid width height combinedGrid

instance Monoid VGrid where
  mempty = VGrid $ mkEmptyGrid 0 0

instance Borderable VGrid where
    withBorder (VGrid grid) = VGrid $ withBorder grid

instance Pretty VGrid where
    pretty (VGrid grid) = pretty grid
:}
{% endhighlight %}

Again, always worth testing:

{% highlight haskell %}
:{
putStrLn . pretty . mconcat
  $ VGrid . withBorder . mkPieceGrid <$> [PieceL, PieceR, PieceS]
:}
{% endhighlight %}

Now we can generate some batches of seven pieces, and stitch them together like so:

{% highlight haskell %}
:{
do
  -- Get the system source of randomness
  g <- newStdGen
  -- Create a stream of pieces wrapped in our VGrid Monoid.
  let vStream = VGrid . withBorder . mkPieceGrid <$> pieceStream g
  -- We create an infinite stream of batches, each stitched together with a border.
  let rows pieces = (mconcat $ take 7 pieces) : rows (drop 7 pieces)
  -- Now we can take 5 of these rows, unwrap them, rewrap as VGrid, and stitch them again.
  let grid = unHGrid $ mconcat (HGrid . withBorder . unVGrid <$> take 5 (rows vStream))
  -- Finally we can print the underlying, unwrapped grid.
  putStrLn (pretty grid)
:}
{% endhighlight %}

    ┌──────────────────────────────────────────┐
    │┌────┐┌────┐┌────┐┌────┐┌────┐┌────┐┌────┐│
    ││    ││    ││    ││    ││    ││    ││    ││
    ││ ██ ││ ██ ││ █  ││    ││██  ││ ██ ││ █  ││
    ││██  ││ ██ ││ █  ││    ││ ██ ││ █  ││███ ││
    ││    ││    ││ ██ ││████││    ││ █  ││    ││
    │└────┘└────┘└────┘└────┘└────┘└────┘└────┘│
    └──────────────────────────────────────────┘
    ┌──────────────────────────────────────────┐
    │┌────┐┌────┐┌────┐┌────┐┌────┐┌────┐┌────┐│
    ││    ││    ││    ││    ││    ││    ││    ││
    ││ ██ ││██  ││ ██ ││ █  ││ ██ ││    ││ █  ││
    ││██  ││ ██ ││ █  ││ █  ││ ██ ││    ││███ ││
    ││    ││    ││ █  ││ ██ ││    ││████││    ││
    │└────┘└────┘└────┘└────┘└────┘└────┘└────┘│
    └──────────────────────────────────────────┘
    ┌──────────────────────────────────────────┐
    │┌────┐┌────┐┌────┐┌────┐┌────┐┌────┐┌────┐│
    ││    ││    ││    ││    ││    ││    ││    ││
    ││ ██ ││    ││ ██ ││ █  ││██  ││ ██ ││ █  ││
    ││██  ││    ││ █  ││███ ││ ██ ││ ██ ││ █  ││
    ││    ││████││ █  ││    ││    ││    ││ ██ ││
    │└────┘└────┘└────┘└────┘└────┘└────┘└────┘│
    └──────────────────────────────────────────┘
    ┌──────────────────────────────────────────┐
    │┌────┐┌────┐┌────┐┌────┐┌────┐┌────┐┌────┐│
    ││    ││    ││    ││    ││    ││    ││    ││
    ││ ██ ││ █  ││ ██ ││    ││██  ││ █  ││ ██ ││
    ││██  ││███ ││ █  ││    ││ ██ ││ █  ││ ██ ││
    ││    ││    ││ █  ││████││    ││ ██ ││    ││
    │└────┘└────┘└────┘└────┘└────┘└────┘└────┘│
    └──────────────────────────────────────────┘
    ┌──────────────────────────────────────────┐
    │┌────┐┌────┐┌────┐┌────┐┌────┐┌────┐┌────┐│
    ││    ││    ││    ││    ││    ││    ││    ││
    ││██  ││ ██ ││ █  ││ █  ││ ██ ││    ││ ██ ││
    ││ ██ ││██  ││███ ││ █  ││ ██ ││    ││ █  ││
    ││    ││    ││    ││ ██ ││    ││████││ █  ││
    │└────┘└────┘└────┘└────┘└────┘└────┘└────┘│
    └──────────────────────────────────────────┘

Looks good to me - each batch of seven represents all pieces, and each is separately shuffled. But where&rsquo;s our colour?! In a terminal, those ANSI control codes would show up just fine.

We introduced a number of new concepts here; we secretly entered a monad (`IO`, specifically), enabling the `do`-notation you see above, and giving us the ability to enact the useful side effect of being able to print to the screen. In fact, we&rsquo;ve been doing this all along with every call to `putStrLn`. We&rsquo;ll get into `IO` more later when we start dealing with user input and multiprocessing.

We also introduced `uncurry` - we wanted to pass the tuples of form `f (1, batch1)` we&rsquo;d created via `zip` into a function that wanted arguments `f 1 batch1` - `uncurry` will convert a function that wants two arguments into a function that wants a tuple of those two arguments<sup><a id="fnr.11" class="footref" href="#fn.11" role="doc-backlink">11</a></sup>.


<a id="org7f3a22e"></a>

# Rotations

While we&rsquo;re here, let&rsquo;s implement piece rotation. We&rsquo;d like to handle a single coordinate at a time, which means we&rsquo;ll also need to pass in information about the bounding box within which we&rsquo;re rotating.

{% highlight haskell %}
:{
data Rotation = CW | CCW

-- Here we apply e.g. a (-y, x) rotation but offset back
-- Here bounds will be supplied based on the frame of reference of the rotation.
-- This will usually be the piece's bounding box.
rotate :: Rotation -> V2 -> V2 -> V2 -> V2
rotate CW (minX, minY) (maxX, maxY) (x, y) = (-y + (maxX - minX), x)
rotate CCW (minX, minY) (maxX, maxY) (x, y) = (y, -x + (maxY - minY))

-- This will let us rotate an entire grid by supplying the
-- appropriate rotation function. We convert the grid to a list briefly,
-- then convert it back.
-- This is inefficient in that it scans for the minX and minY each time.
rotateGrid :: Rotation -> Grid -> Grid
rotateGrid rotation (Grid width height grid) =
  let minX = minimum $ fst <$> M.keys grid
      minY = minimum $ snd <$> M.keys grid
      maxX = maximum $ fst <$> M.keys grid
      maxY = maximum $ snd <$> M.keys grid
      rotateFn = rotate rotation (minX, minY) (maxX, maxY)
   in Grid width height (M.mapKeys rotateFn grid)
:}

{% endhighlight %}

Now we can rotate coordinates, but we want to rotate pieces themselves.

Let&rsquo;s take a look at these rotations with a helper:<sup><a id="fnr.12" class="footref" href="#fn.12" role="doc-backlink">12</a></sup>

{% highlight haskell %}
:{
showRotations rotation =
    forM_ allPieces
    $ (\piece ->
          piece
            & mkPieceGrid
            & iterate (rotateGrid rotation)
            & take 4
            & fmap (VGrid . withBorder)
            & mconcat
            & unVGrid
            & pretty
            & putStrLn)
:}
{% endhighlight %}

First clockwise:

{% highlight haskell %}
:{
showRotations CW
:}
{% endhighlight %}

    ┌────┐┌────┐┌────┐┌────┐
    │    ││    ││ ██ ││    │
    │ █  ││███ ││  █ ││   █│
    │ █  ││█   ││  █ ││ ███│
    │ ██ ││    ││    ││    │
    └────┘└────┘└────┘└────┘
    ┌────┐┌────┐┌────┐┌────┐
    │    ││    ││  █ ││    │
    │ ██ ││███ ││  █ ││ █  │
    │ █  ││  █ ││ ██ ││ ███│
    │ █  ││    ││    ││    │
    └────┘└────┘└────┘└────┘
    ┌────┐┌────┐┌────┐┌────┐
    │    ││    ││    ││    │
    │ ██ ││ ██ ││ ██ ││ ██ │
    │ ██ ││ ██ ││ ██ ││ ██ │
    │    ││    ││    ││    │
    └────┘└────┘└────┘└────┘
    ┌────┐┌────┐┌────┐┌────┐
    │    ││ █  ││    ││    │
    │ ██ ││ ██ ││  ██││ █  │
    │██  ││  █ ││ ██ ││ ██ │
    │    ││    ││    ││  █ │
    └────┘└────┘└────┘└────┘
    ┌────┐┌────┐┌────┐┌────┐
    │    ││  █ ││    ││    │
    │██  ││ ██ ││ ██ ││  █ │
    │ ██ ││ █  ││  ██││ ██ │
    │    ││    ││    ││ █  │
    └────┘└────┘└────┘└────┘
    ┌────┐┌────┐┌────┐┌────┐
    │    ││ █  ││    ││    │
    │ █  ││ ██ ││ ███││  █ │
    │███ ││ █  ││  █ ││ ██ │
    │    ││    ││    ││  █ │
    └────┘└────┘└────┘└────┘
    ┌────┐┌────┐┌────┐┌────┐
    │    ││█   ││████││   █│
    │    ││█   ││    ││   █│
    │    ││█   ││    ││   █│
    │████││█   ││    ││   █│
    └────┘└────┘└────┘└────┘

And counterclockwise:

{% highlight haskell %}
:{
showRotations CCW
:}
{% endhighlight %}

    ┌────┐┌────┐┌────┐┌────┐
    │    ││    ││ ██ ││    │
    │ █  ││   █││  █ ││███ │
    │ █  ││ ███││  █ ││█   │
    │ ██ ││    ││    ││    │
    └────┘└────┘└────┘└────┘
    ┌────┐┌────┐┌────┐┌────┐
    │    ││    ││  █ ││    │
    │ ██ ││ █  ││  █ ││███ │
    │ █  ││ ███││ ██ ││  █ │
    │ █  ││    ││    ││    │
    └────┘└────┘└────┘└────┘
    ┌────┐┌────┐┌────┐┌────┐
    │    ││    ││    ││    │
    │ ██ ││ ██ ││ ██ ││ ██ │
    │ ██ ││ ██ ││ ██ ││ ██ │
    │    ││    ││    ││    │
    └────┘└────┘└────┘└────┘
    ┌────┐┌────┐┌────┐┌────┐
    │    ││    ││    ││ █  │
    │ ██ ││ █  ││  ██││ ██ │
    │██  ││ ██ ││ ██ ││  █ │
    │    ││  █ ││    ││    │
    └────┘└────┘└────┘└────┘
    ┌────┐┌────┐┌────┐┌────┐
    │    ││    ││    ││  █ │
    │██  ││  █ ││ ██ ││ ██ │
    │ ██ ││ ██ ││  ██││ █  │
    │    ││ █  ││    ││    │
    └────┘└────┘└────┘└────┘
    ┌────┐┌────┐┌────┐┌────┐
    │    ││    ││    ││ █  │
    │ █  ││  █ ││ ███││ ██ │
    │███ ││ ██ ││  █ ││ █  │
    │    ││  █ ││    ││    │
    └────┘└────┘└────┘└────┘
    ┌────┐┌────┐┌────┐┌────┐
    │    ││   █││████││█   │
    │    ││   █││    ││█   │
    │    ││   █││    ││█   │
    │████││   █││    ││█   │
    └────┘└────┘└────┘└────┘

I&rsquo;m almost sure it&rsquo;s not **Regulation Tetris Rotation Rules**, but it&rsquo;ll do.


<a id="org3034eef"></a>

# Placing Pieces on the Grid

Let&rsquo;s start by placing a piece in that buffer zone at the top of the grid (which we&rsquo;ll eventually hide).

We want it to be anchored to the bottom, so that it immediately starts to become visible as it falls, so we&rsquo;ll translate it based on its lowest y-coordinate.

{% highlight haskell %}
:{
-- Ensure the piece is centred and anchored to the top of the viewport.
initPiece :: Piece -> ActivePiece
initPiece piece = ActivePiece (pieceColour piece) coordinates
  where
    -- We need to ensure the largest y-coordinate is 3
    yOffset = 3 - maximum (snd <$> pieceCoords piece)
    -- And we'd like to roughly centre the piece, so we'll offset it by 3
    coordinates = (\(x, y) -> (x + 3, y + yOffset)) <$> pieceCoords piece
:}
{% endhighlight %}

And let&rsquo;s test this, as ever:

{% highlight haskell %}
:{
putStrLn . pretty . withBorder $ mkEmptyGrid 10 24 & withPiece (initPiece PieceS)
:}
{% endhighlight %}

    ┌──────────┐
    │          │
    │          │
    │    ██    │
    │   ██     │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    │          │
    └──────────┘
    ghc

Looks solid - one step of gravity after this, and the piece will become visible.


<a id="orgc31503d"></a>

# Representing the Game State

Now we&rsquo;ll create the type we&rsquo;ll be using to store all state about the ongoing game. Note that we still keep this outside of `IO`, requiring that a source of randomness is piped in to create this state.

We&rsquo;re going to implement piece holding - since there might not be a held piece, we&rsquo;ll represent this using `Maybe`. This is a Haskell staple, defined as `data Maybe a = Just a | Nothing`. It&rsquo;s like Rust&rsquo;s `Option<a>` and there are analogues in most languages. It forces you to consider both cases when you may or may not have a value.

{% highlight haskell %}
:{
data Game = Game {
  grid :: Grid,
  currentPiece :: ActivePiece,
  heldPiece :: Maybe Piece,
  pieces :: [Piece],
  score :: Int
}

mkGame :: RandomGen g => g -> Game
mkGame g =
  let (firstPiece:rest) = pieceStream g
   in Game {
        grid = mkEmptyGrid 10 24,
        currentPiece = initPiece firstPiece,
        pieces = rest,
        score = 0,
        heldPiece = Nothing
      }
:}
{% endhighlight %}

As we pull pieces from the infinite lazy list `pieces`, we&rsquo;ll create new `Game` objects that contain the remainder of the lazy list.

Note each field of this record type (essentially a Haskell product type with named members) creates a function of the same name, which you can call on inhabitants of this datatype to retrieve the field value. So `score game` will return the score of a game, and so on. This can cause all kinds of namespace clashes and there are a lot of ways around it, but for now we&rsquo;re just going to use these default record accessors.

Alright - now we&rsquo;re in a position to render our rudimentary UI by stitching these things together. On the left we&rsquo;ll have our grid, and on the right we&rsquo;ll have our next piece on the top, and our held piece on the bottom:

We&rsquo;ll need a way of adding string labels to our UI:

{% highlight haskell %}
:{
-- Turn a string into a grid for composability
-- Only supports single lines, but will be fine for our simple UI.
sToG :: String -> Grid
sToG s =
  Grid (length s) 1
    $ M.fromList [((x, 0), BlockChar White c) | (x, c) <- zip [0..] s]
:}
{% endhighlight %}

And a way of hiding the buffer zone:

{% highlight haskell %}
:{
hideBuffer :: Grid -> Grid
hideBuffer (Grid width height grid) = Grid width (height - 4) grid'
  where
    grid' =
      grid
        & M.mapKeys (second (subtract 4))
        & M.filterWithKey (\(_, y) _ -> y >= 0)
:}
{% endhighlight %}

Now finally we can put it all together:

{% highlight haskell %}
:{
-- Here we'll stitch it all together, dropping the four lines, and popping the
-- score at the top with the held piece and next piece on the right.
gameGrid :: Game -> Grid
gameGrid game =
  let -- Let's add a label at the top to display the score.
      scoreGrid = withBorder . HGrid . sToG $ "Score: " <> show (score game)
      -- Now the left hand side; the grid with the current piece,
      -- but the top four lines hidden.
      lhs = withBorder . VGrid . hideBuffer $ grid game & withPiece (currentPiece game)
      -- Create a preview with a label above it showing the next piece
      nextPiece = HGrid (sToG "Next:") <> HGrid (mkPieceGrid (head (pieces game)))
      -- Now we show the held piece; it might not exist, so we need to handle that case.
      held = HGrid (sToG "Held:") <>
             (HGrid $ case heldPiece game of
                        Nothing -> mkEmptyGrid 4 4
                        Just piece -> mkPieceGrid piece)
      -- To construct the RHS we can just add borders and mconcat them with <>
      rhs = withBorder nextPiece <> withBorder held
      -- It's a little clumsy to stitch an HGrid and VGrid, but it works.
      playArea = HGrid . unVGrid $ lhs <> VGrid (unHGrid rhs)
      -- Finally, we can stitch it all together
      gameInterface = scoreGrid <> playArea
   in unHGrid gameInterface

-- Finally we just pretty-print the game grid itself
instance Pretty Game where
  pretty = pretty . gameGrid
:}
{% endhighlight %}

We can preview this as so:

{% highlight haskell %}
:{
do
  -- g <- newStdGen -- This would be system-random; for now we'll set a seed
  let g = mkStdGen 42 -- This sets our random seed.
  -- Create a new Game with one of its records set so we have a held piece to show
  let game = (mkGame g) {heldPiece = Just PieceS}
  putStrLn (pretty game)
:}
{% endhighlight %}

    ┌────────┐         
    │Score: 0│         
    └────────┘         
    ┌──────────┐┌─────┐
    │          ││Next:│
    │          ││     │
    │          ││ ██  │
    │          ││ █   │
    │          ││ █   │
    │          │└─────┘
    │          │┌─────┐
    │          ││Held:│
    │          ││     │
    │          ││ ██  │
    │          ││██   │
    │          ││     │
    │          │└─────┘
    │          │       
    │          │       
    │          │       
    │          │       
    │          │       
    │          │       
    │          │       
    └──────────┘       
    g

This is looking a bit like Tetris! We can no longer see the buffer zone at the top with the falling piece, but we can see the next piece displayed on the right hand side, and below that we&rsquo;ve artificially inserted a held square piece, and as we can see it&rsquo;s all composing nicely.


<a id="orgecf47ff"></a>

# The Introduction of Time and Logic

Let&rsquo;s ignore user input for now and focus solely on advancing time.

To make this work, we&rsquo;ll need a way to:

-   Advance the current piece downwards
-   Fix pieces in place when they hit the bottom
-   Pulls a new piece from the infinite stream and places it at the top

We&rsquo;ll build a `step` function that does all of this at once, but first let&rsquo;s implement gravity. To do this correctly, we also need a way of checking if a game is in a valid state, to stop pieces from falling through the floor.

A valid `Game` is one where there are no out of bound blocks, and the current `ActivePiece` is not overlapping with any of the existing blocks. By induction, if we start with a valid `Game`, and only place pieces in valid places, we only need to check the currently active piece:

{% highlight haskell %}
:{
isValid :: Game -> Bool
isValid game =
  let -- We unwrap here to get to activeCoords; libraries like lens make this easier.
      (ActivePiece _ activeCoords) = currentPiece game
      -- We use a comprehension to find any non-empty blocks
      fullCoords = [c | (c, block) <- M.toList (unGrid $ grid game), block /= Empty]
      -- We'll let ourselves use magic numbers in our bounds checker.
      outOfBounds (x, y) = x < 0 || x > 9 || y < 0 || y > 23
      -- Finally, we ensure there is no overlap and no OOB block.
      -- We could use Data.Set to make this significantly more efficient.
   in (null (intersect activeCoords fullCoords)) && (not (any outOfBounds activeCoords))
:}
{% endhighlight %}

{% highlight haskell %}
:{
-- We need a way to translate a piece
-- bimap comes because a tuple is a Bifunctor,
-- letting us map over both elements at once.
movePiece :: V2 -> ActivePiece -> ActivePiece
movePiece (x, y) (ActivePiece colour coords) =
  ActivePiece colour (bimap (+x) (+y) <$> coords)

-- Here we use record update syntax to edit just one field.
-- If applying gravity results in an invalid game, we can represent this by Nothing.
-- Here we use guard syntax to handle multiple boolean cases.
applyGravity :: Game -> Maybe Game
applyGravity game
  | isValid game' = Just game'
  | otherwise = Nothing
  where
    game' = game { currentPiece = movePiece (0, 1) (currentPiece game) }
:}
{% endhighlight %}

So let&rsquo;s test this out a few times - for now we&rsquo;ll represent the passage of time horizontally, so we&rsquo;ll make a few game states, pull out the grids, and stitch them side by side. We&rsquo;d like to keep applying `applyGravity` over and over - but each time we take a `Game` to a `Maybe Game`. We want some way of chaining these iterations together - and that&rsquo;s where the fact that `Maybe` belongs to the `Monad` typeclass comes in.

This is **not** a `Monad` tutorial but it&rsquo;s useful to know that this is what&rsquo;s powering the composition<sup><a id="fnr.13" class="footref" href="#fn.13" role="doc-backlink">13</a></sup> of instances of this `applyGravity` function together in a type-consistent way.

{% highlight haskell %}
:{
-- Now that we're dealing with Maybe, let's implement a hacky way
-- to debug print both cases.
instance Pretty a => Pretty (Maybe a) where
  pretty Nothing = "Nothing Pretty"
  pretty (Just a) = pretty a

-- This takes some thinking and is left as an exercise for the reader.
-- We need to map some functions two Functors deep - the outer layer is the List
-- and the inner layer is the Maybe.
-- Having an operator for this is useful.
infixl 4 <$$>
(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

-- There are monadic library functions that'll do this generically, but let's manually
-- implement the composition of our Maybes. This will iterate until it hits a Nothing,
-- and then stop.
iterateMaybes :: (a -> Maybe a) -> a -> [Maybe a]
iterateMaybes f a =
  case f a of
    Just x -> Just x : iterateMaybes f x
    Nothing -> [Nothing]

debugGravity :: Maybe String
debugGravity =
  let games = iterateMaybes applyGravity (mkGame (mkStdGen 42))
   in fmap (pretty . unVGrid) . mconcat $ (withBorder . VGrid . gameGrid <$$> games)
:}
{% endhighlight %}

Here we unsafely unwrap the `Maybe String` since we know it&rsquo;s going to be a `Just`, but bear in mind that&rsquo;s not great practice in production:

{% highlight haskell %}
:{
let (Just s) = debugGravity in putStrLn s
:}
{% endhighlight %}

    ┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐
    │┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         │
    ││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │
    │└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         │
    │┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐│
    ││    ██    ││Next:││││    █     ││Next:││││    █     ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││
    ││          ││     ││││    ██    ││     ││││    █     ││     ││││    █     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││
    ││          ││ ██  ││││          ││ ██  ││││    ██    ││ ██  ││││    █     ││ ██  ││││    █     ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││
    ││          ││ █   ││││          ││ █   ││││          ││ █   ││││    ██    ││ █   ││││    █     ││ █   ││││    █     ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││
    ││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││    ██    ││ █   ││││    █     ││ █   ││││    █     ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││
    ││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││    ██    │└─────┘│││    █     │└─────┘│││    █     │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│
    ││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││    ██    │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│
    ││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││    ██    ││Held:││││    █     ││Held:││││    █     ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││
    ││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││    █     ││     ││││    █     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││
    ││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││    █     ││     ││││    █     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││
    ││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││    █     ││     ││││    █     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││
    ││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││    █     ││     ││││    █     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││
    ││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││    ██    │└─────┘│││    █     │└─────┘│││    █     │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│
    ││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │││    █     │       │││    █     │       │││          │       │││          │       │││          │       │││          │       │
    ││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │││    █     │       │││    █     │       │││          │       │││          │       │││          │       │
    ││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │││    █     │       │││    █     │       │││          │       │││          │       │
    ││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │││    █     │       │││    █     │       │││          │       │
    ││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │││    █     │       │││    █     │       │
    ││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │││    █     │       │
    ││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │
    │└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       │
    └───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘

Sick, we hit the bottom and then we stop.

Let&rsquo;s create a way to fix our active pieces to the grid - simple, because we can just take the union of the coordinates. We&rsquo;ll simultaneously draw a new piece from the stream, too.

{% highlight haskell %}
:{
fixPiece :: Game -> Game
fixPiece game =
  game { grid = gridWithPiece
       , currentPiece = initPiece $ head (pieces game)
       , pieces = tail (pieces game)
       }
  where
    (ActivePiece colour coords) = currentPiece game
    (Grid width height g) = grid game
    gridWithPiece =
      Grid width height
        $ foldl' (\g coord -> M.insert coord (Block colour) g) g coords
:}
{% endhighlight %}

Now we can continually apply gravity, and when we reach an invalid state, we can fix the piece instead. The call to `applyGravity` lets us look one step ahead and respond accordingly. However, if after fixing a piece, we&rsquo;re still invalid (i.e. we&rsquo;ve reached the top of the grid), we can return `Nothing` again.

{% highlight haskell %}
:{
loseTheGame :: Game -> Maybe Game
loseTheGame game
  | isValid game =
      case applyGravity game of
        Just game' -> Just game'
        Nothing -> Just (fixPiece game)
  | otherwise = Nothing

debugLoseTheGame :: Maybe String
debugLoseTheGame  =
  let games = iterateMaybes loseTheGame (mkGame (mkStdGen 42))
   in fmap (pretty . unVGrid) . mconcat $ (withBorder . VGrid . gameGrid <$$> games)
:}
{% endhighlight %}

And so now when we go to print this:

{% highlight haskell %}
:{
let (Just s) = debugLoseTheGame in putStrLn s
:}
{% endhighlight %}

    ┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐┌───────────────────┐
    │┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         ││┌────────┐         │
    ││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │││Score: 0│         │
    │└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         ││└────────┘         │
    │┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐││┌──────────┐┌─────┐│
    ││    ██    ││Next:││││    █     ││Next:││││    █     ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││    █     ││Next:││││    █     ││Next:││││    ██    ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││    ██    ││Next:││││   ██     ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││   ██     ││Next:││││    ██    ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││    ██    ││Next:││││    ██    ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││   ███    ││Next:││││    █     ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││   ████   ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││   ██     ││Next:││││    ██    ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││          ││Next:││││    █     ││Next:││││    █     ││Next:││││    ██    ││Next:││││    ██    ││Next:││││    ██    ││Next:││
    ││          ││     ││││    ██    ││     ││││    █     ││     ││││    █     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    █     ││     ││││    █     ││     ││││    ██    ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││   ██     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││   ██     ││     ││││    ██    ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││    ██    ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││   ███    ││     ││││    █     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││   ████   ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││   ██     ││     ││││    ██    ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    █     ││     ││││    █     ││     ││││    █     ││     ││││    █     ││     ││
    ││          ││ ██  ││││          ││ ██  ││││    ██    ││ ██  ││││    █     ││ ██  ││││    █     ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││██   ││││          ││██   ││││          ││██   ││││    █     ││██   ││││    █     ││██   ││││    ██    ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││    ██    ││ ██  ││││   ██     ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││   ██     ││ ██  ││││    ██    ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││    ██    ││ █   ││││    ██    ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││     ││││          ││     ││││          ││     ││││   ███    ││     ││││    █     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││   ████   ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││   ██     ││ ██  ││││    ██    ││ ██  ││││          ││ ██  ││││          ││██   ││││          ││██   ││││          ││██   ││││    █     ││██   ││││    █     ││ █   ││││    █     ││     ││
    ││          ││ █   ││││          ││ █   ││││          ││ █   ││││    ██    ││ █   ││││    █     ││ █   ││││    █     ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││    █     ││ ██  ││││    █     ││ ██  ││││    ██    ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││    ██    ││██   ││││   ██     ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││   ██     ││ ██  ││││    ██    ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││ ██  ││││          ││███  ││││          ││███  ││││          ││███  ││││          ││███  ││││    ██    ││███  ││││    ██    ││███  ││││          ││███  ││││          ││███  ││││          ││███  ││││          ││███  ││││          ││███  ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││   ███    ││     ││││    █     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││██   ││││          ││██   ││││          ││██   ││││          ││██   ││││   ████   ││██   ││││          ││██   ││││          ││██   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││   ██     ││ █   ││││    ██    ││ █   ││││    ██    ││ ██  ││││    ██    ││ ██  ││││    ██    ││ ██  ││││    ██    ││ ██  ││││    ██    ││███  ││││    ██    ││     ││
    ││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││    ██    ││ █   ││││    █     ││ █   ││││    █     ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    █     ││     ││││    █     ││     ││││    ██    ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││   ██     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││   ██     ││     ││││    ██    ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││    ██    ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││████ ││││          ││████ ││││          ││████ ││││          ││████ ││││          ││████ ││││   ███    ││████ ││││    █     ││████ ││││          ││████ ││││          ││████ ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││   ████   ││     ││││          ││     ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││          ││ █   ││││   ██     ││ █   ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││████ ││
    ││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││    ██    │└─────┘│││    █     │└─────┘│││    █     │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││    █     │└─────┘│││    █     │└─────┘│││    ██    │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││    ██    │└─────┘│││   ██     │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││   ██     │└─────┘│││    ██    │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││    ██    │└─────┘│││    ██    │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││   ███    │└─────┘│││    █     │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││   ████   │└─────┘│││   ████   │└─────┘│││   ████   │└─────┘│││   ████   │└─────┘│││   ████   │└─────┘│││   ████   │└─────┘│││   ████   │└─────┘│││   ████   │└─────┘│││   ████   │└─────┘│││   ████   │└─────┘│││   ████   │└─────┘│││   ████   │└─────┘│││   ████   │└─────┘│
    ││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││    ██    │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    ██    │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││    ██    │┌─────┐│││   ██     │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││   ██     │┌─────┐│││    ██    │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││    ██    │┌─────┐│││    ██    │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││          │┌─────┐│││   ███    │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│││    █     │┌─────┐│
    ││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││    ██    ││Held:││││    █     ││Held:││││    █     ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││    █     ││Held:││││    █     ││Held:││││    ██    ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││    ██    ││Held:││││   ██     ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││   ██     ││Held:││││    ██    ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││    ██    ││Held:││││    ██    ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││          ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││││   ███    ││Held:││
    ││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││    █     ││     ││││    █     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    █     ││     ││││    █     ││     ││││    ██    ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││   ██     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││   ██     ││     ││││    ██    ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││
    ││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││    █     ││     ││││    █     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    █     ││     ││││    █     ││     ││││    ██    ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││   ██     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││   ██     ││     ││││    ██    ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││
    ││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││    █     ││     ││││    █     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    █     ││     ││││    █     ││     ││││    ██    ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││   ██     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││   ██     ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││││    ██    ││     ││
    ││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││    █     ││     ││││    █     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    █     ││     ││││    █     ││     ││││    ██    ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││    ██    ││     ││││   ██     ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││          ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││││   ██     ││     ││
    ││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││    ██    │└─────┘│││    █     │└─────┘│││    █     │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││    █     │└─────┘│││    █     │└─────┘│││    ██    │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││          │└─────┘│││    ██    │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│││   ██     │└─────┘│
    ││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │││    █     │       │││    █     │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    █     │       │││    █     │       │││    ██    │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │
    ││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │││    █     │       │││    █     │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    █     │       │││    █     │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │
    ││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │││    █     │       │││    █     │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │
    ││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │││    █     │       │││    █     │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │
    ││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │
    ││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │││    █     │       │
    ││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││          │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │││    ██    │       │
    │└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       ││└──────────┘       │
    └───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘└───────────────────┘

# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> Okay, for now this is actually a version I build ages ago. I&rsquo;m rewriting this from scratch for this post, so ours will look a little different, and hopefully better!

<sup><a id="fn.2" href="#fnr.2">2</a></sup> Okay, not quite. I&rsquo;m writing this in Emacs, where `org-babel` will run each block in GHCi, a Haskell interpreter, with `set +m` enabled to allow multiline blocks. The whole thing gets compiled to Markdown via `org-jekyll`. The end result is the same, more or less, as writing actual literate code, with some of the advantages of a Jupyter-style workflow.

<sup><a id="fn.3" href="#fnr.3">3</a></sup> Note that in general this is a terrible idea and gave me all kinds of headaches writing this post. Using Cabal in a global manner like this is inviting trouble. Pick and learn a package manager (could still be Cabal, but in the context of a project, not a blog post)

<sup><a id="fn.4" href="#fnr.4">4</a></sup> I use Cabal&rsquo;s Nix integration for anything serious.

<sup><a id="fn.5" href="#fnr.5">5</a></sup> Also because for whatever reason, I can&rsquo;t get `org-babel` to accept more than one import per code block and I really want to be able to run this entire post as a single notebook-style program.

<sup><a id="fn.6" href="#fnr.6">6</a></sup> You&rsquo;ll typically be recommended to eschew `String` (which is a linked list of characters) for the more efficient `Text` type; we don&rsquo;t need to worry about this for a toy application.

<sup><a id="fn.7" href="#fnr.7">7</a></sup> There&rsquo;s already the `Show` typeclass that does exactly this, and which can be automatically derived for many types, but I tend to think of it as for debugging and inspection purposes - I prefer a separate typeclass for representations intended to be user-facing.

<sup><a id="fn.8" href="#fnr.8">8</a></sup> The use of `foldl'` here does two things: we fold from the left (irrelevant in this case, but important sometimes), and we fold strictly - that is, we don&rsquo;t accumulate a load of unevaluated thunks and overflow the stack. Again, never going to happen in our toy example, but worth knowing.

<sup><a id="fn.9" href="#fnr.9">9</a></sup> That is, associative, and with a left and right identity (the empty grid in both cases).

<sup><a id="fn.10" href="#fnr.10">10</a></sup> Note that when referring to operators both in code and prose, it&rsquo;s typical to refer to them in parentheses. `(+) 1 2` is the same as `1 + 2`.

<sup><a id="fn.11" href="#fnr.11">11</a></sup> It gets more complex when you&rsquo;re dealing with more arguments - `uncurry3 f (a, b c) = f a b c` and so on exist but there&rsquo;s no way to write generic `uncurryN` without resorting to `TemplateHaskell` to the best of my knowledge. Tweet at me if I&rsquo;m wrong please.

<sup><a id="fn.12" href="#fnr.12">12</a></sup> The lambda syntax used here twice nested makes e.g. `(\a b -> a + b)` equivalent to `(+)`.

<sup><a id="fn.13" href="#fnr.13">13</a></sup> In this case, Kleisli composition; the `(>=>)` operator composes `a -> m b` and `b -> m c` into `a -> m c`.
