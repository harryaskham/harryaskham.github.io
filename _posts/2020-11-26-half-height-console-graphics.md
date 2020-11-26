---
layout: post
title: Half-Height Console Graphics with Haskell and ncurses
---

# Motivation

Most terminals print characters twice as long as they are wide. This means that any character-based graphics that naively treat a character as a pixel are going to end up distorted. There are [plenty of ways](https://en.wikipedia.org/wiki/ANSI_art) around this, but I wanted to write up the ncurses-based solution I came up with when writing a terminal-only roguelike engine in Haskell recently with support for simulating square pixels in an otherwise rectangular terminal.

<video muted controls="controls">
  <source type="video/mp4" src="/video/rouge-example.mp4"></source>
</video>

_A debug run showing the end result: full-height text at the bottom of the terminal, and dense half-height ncurses graphics at the top._

# Problem and Solution

The best examples of character per pixel ncurses rendering are those using the [libtcod](https://libtcod.readthedocs.io/en/latest/) engine:

![](/img/libtcod.png)

This looks good, but note that each cell is the same height as the text around it (in the above screenshot, this effect is minimised by using a shorter monospaced font). We'd like two modes: one for denser cell-based graphics and another for full-sized text, both in the same terminal window.

Our solution will be to represent the canvas as a (W, (H/2)) dimension 2D array of [Upper Half Block (▀)](https://www.compart.com/en/unicode/U+2580) characters, where (for example) if we take the top-left cell in (x, y) coordinates as (0, 0), we set the foreground colour to our canvas' (0, 0) colour and the background colour to our canvas' (0, 1) colour, simulating two rows of half-height pixels using only one row of characters.

I'll omit most of the setup - here we have a simple 2D array graphics buffer (each cell is one of 15 colours), and an environment that contains a camera position and viewport width and height. This function draws the buffer to the screen as half-height block characters. We pair up rows in chunks of two, and render within the `Update` monad of `UI.NCurses`.

```haskell
drawBuffer :: (MonadReader Env m, MonadIO m) => GfxBuffer -> m (Update ())
drawBuffer gfxBuffer = do
  env <- ask
  camera <- liftIO $ readMVar $ env^.camera
  let (V2 cx cy') = camera^.topLeft
      (V2 cw ch') = camera^.dims
      -- Adjust vertical values to account for the block-rendering
      cy = cy' `div` 2
      ch = ch' `div` 2
      -- Pair up rows to allow for block rendering.
      pairedBuffer = V.fromList $ combineRows <$> VS.chunksOf 2 gfxBuffer
      mkSetter x y = do
        let (ColorPair fg bg) = pairedBuffer V.! y V.! x
            colorID = getCol (env^.colors) fg bg
        moveCursor (fromIntegral y) (fromIntegral x)
        setColor colorID
        drawGlyph $ Glyph '▀' []
  return $ sequence_ [mkSetter x y | x <- [0..cw-1], y <- [0..ch-1]]
```

We lose two things by taking this approach:

- Cells can only be block colour and can't contain text (they already contain the half-block character).
- The colour pallette must be limited, as we'll now see.

# Colour Representation in ncurses

There's a problem for colour-hungry use-cases. ncurses allows you to use the predefined terminal colour library, and also allows you to overwrite these built-in colours and extend to a table of 255 custom colours (colour 0 cannot be overridden) - but the snag is that a "custom colour" is really a combination of background and foreground colour.

With our condensed representation, if we have a pallette of say 4 custom colours, we would need 16 custom fg/bg pairs to represent each possibility that might arise. With our available 255 slots, then, we can only represent 15 unique colours.

Using the `Color` type from `UI.NCurses`, we first define a pair type for our block cells and implement `Ord` so we can look up the 1-255 colour key for a given pair:

```haskell
-- A pair of colors s.t. we can display half-characters using unicode blocks.
data ColorPair = ColorPair Color Color deriving (Show, Eq)

instance Ord ColorPair where
  -- Arbitrary comparison so that we end up with distinct map keys.
  a <= b = show a <= show b
  
-- A map to store the registered color pairings.
type ColorMap = M.Map ColorPair ColorID
```

Colour definition itself is stateful: we need to register our 255-colour pallette inside the `Curses` monad. The `ColorMap` itself is just a convenience lookup keeping track of which logical colour-pairs map on to which terminal colour ID. The library requires colours in RGB-1000 format - we include a quick-and-dirty method for constructing these from hex strings.

```haskell
-- Takes a hex code and converts to a tuple of RGB 1000 values.
-- These are required by the curses register.
parseColor :: String -> (Integer, Integer, Integer)
parseColor cs = toTuple3 scaled
  where
    hexes = fst <$> (head . readHex <$> LS.chunksOf 2 cs)
    scaled = (\x -> (x * 1000) `div` 255) <$> hexes
    
-- Takes a list of hex strings and for each index, sets the corresponding terminal colour
-- to that represented by the hex.
defineCustomColors :: [String] -> Curses ()
defineCustomColors hexes = sequence_ . getZipList $ ZipList mkColors <*> ZipList colorIDs
  where
    colorIDs = [1..] :: [Int16]
    mkColors = [ \i -> defineColor (Color i) r g b
               | (r, g, b) <- parseColor <$> hexes
               ]
```

And finally, in order to both set the colours and track the pairwise mapping, we call the following init function to get a `Curses ColorMap`:

```haskell
-- Takes all custom colors and defines + persists them as an overlap map.
initColors :: [String] -> Curses ColorMap
initColors hexes = do
  -- Set the terminal colours appropriately
  defineCustomColors hexes
  -- Create and return the ColorMap tracking the assignments made
  sequenceA $ M.fromList (zip colorKeys newColors)
  where
    colors = Color <$> [1..15]
    colorIDs = [1..length colors ^ 2]
    colorCombos = (,) <$> colors <*> colors
    colorKeys = uncurry ColorPair <$> colorCombos
    colorCreators = ZipList $ uncurry newColorID <$> colorCombos
    newColors = getZipList $ colorCreators <*> ZipList (fromIntegral <$> colorIDs)
```

With a fully constructed map (whose keys are foreground/background pairs), we can get the block representation for a two-row cell by simple lookup:

```haskell
getCol :: ColorMap -> Color -> Color -> Maybe ColorID
getCol colors fg bg = colors^.at (ColorPair fg bg)
```
