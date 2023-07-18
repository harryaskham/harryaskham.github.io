---
title: "Falling Blocks in Beginner Haskell"
date: 2023-07-18
layout: post
categories: 
tags: 
---

![img](/img/tetriskell.gif)  
*We&rsquo;ll make this over the course of the tutorial*


# What This Is

This post is a hands-on intro to Haskell via the implementation of a little-known game involving falling blocks, because that&rsquo;s how I first learnt the basics. I&rsquo;ll try explain almost everything, such that a competent programmer with no Haskell or even functional programming familiarity can follow it and end up with a passing understanding of building a simple Haskell application.

We end up with a minimal terminal implementation of Tetris, and a simple agent playing using [beam search](https://en.wikipedia.org/wiki/Beam_search).


# What This Isn&rsquo;t

I won&rsquo;t touch on package management or project structure - in fact, this post is literate Haskell, and the concatenated code blocks can be run via `runhaskell tetris.hs` from the source TODO. There are plenty of tutorials on Stack, Cabal and general project management out there - for now, all you need is whatever Haskell distribution your machine uses. [GHCup](https://www.haskell.org/ghcup/) is as good a place to start as any. You might already even have `runhaskell` on your machine. We&rsquo;ll try to use as few external dependencies as possible.


# Prelude

I watched the [Tetris](https://en.wikipedia.org/wiki/Tetris_(film)) movie this week. There&rsquo;s this almost certainly apocryphal scene where Alexey Patjinov is demoing his creation to a publisher, who has a [&ldquo;drop the &lsquo;the&rsquo;&rdquo;](https://www.youtube.com/watch?v=PEgk2v6KntY) moment and suggests all completed rows should vanish at once, rather than one at a time, enabling the achievement of the four-lines-at-once namesake move. They swiftly hack the feature together on a tiny monochrome display, and I was reminded how lucky I am to live in an era of rich tooling, expressive languages, and 4K monitors.

When I was first learning Haskell, though, it felt like punching holes in cards. I couldn&rsquo;t get my head around the interplay between the purity of the language and the need to interact with the real world. A long while before, I&rsquo;d grokked Gary Bernhardt&rsquo;s [Functional Core, Imperative Shell](https://www.destroyallsoftware.com/screencasts/catalog/functional-core-imperative-shell) message, but how does this apply in a world where, supposedly, **everything** is functional? As we&rsquo;ll see, the Haskell equivalent is something like &ldquo;functional core, `IO` shell&rdquo; - but we&rsquo;re getting ahead of ourselves. I wrote [my own toy implementation](https://github.com/harryaskham/tetriskell) as a way of getting to grips with the language, and thought I&rsquo;d revisit it, rewriting it piece-by-piece to reflect my current workflow.


# Let&rsquo;s Get Started

Alright, so we&rsquo;ve got our `tetris.hs` blank slate. This is going to be a single-file program. We open with:

{% highlight haskell %}
module Main where

foo :: String
foo = "woo"
{% endhighlight %}

We can now run this as:

{% highlight haskell %}
main :: IO ()
main = putStrLn $ foo <> foo
{% endhighlight %}

This gives:

    woowoo
    g
