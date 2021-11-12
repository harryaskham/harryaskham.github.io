---
layout: post
title: Pretty Inline Symbolic Mathematics in Org-Mode
---

{:center: style="text-align: center;"}

I recently started playing around with [SymPy](https://www.sympy.org) as a kind of local alternative to the increasingly frustrating Wolfram Alpha, and found an ergonomic way to interact with it inside org-mode using Babel to get pretty live-previews of the equations being produced.

This is a great environment for interactive symbolic calculation, and could even be used to keep derivations and mathematical computations inline inside the papers they're included in. Looks like there's a decent amount of [prior art](https://dynamics-and-control.readthedocs.io/en/latest/0_Getting_Started/Notebook%20introduction.html) for doing this in Jupyter, but I tend to prefer Org and Babel where possible.

![](/img/sympy-emacs.png)
{: center}
*The output of SymPy in Babel rendered as inline LaTeX*
{: center}

## Requirements

One needs Python and SymPy installed, and LaTeX set up well enough that previewing LaTeX blocks in org-mode via `org-latex-preview` is working. I'm also using [org-fragtog](https://github.com/io12/org-fragtog) for the automated display of inline LaTeX, but that's not a requirement.

## Inline LaTeX Rendering for Babel Output

By default, org-mode will render any LaTeX inside e.g. `$inline$` blocks, `\[multi-line blocks\]`, etc if they appear inline alongside the rest of the text in an org file.

What we'd like is to have it render the output of a Babel block - it doesn't render this by default. Babel even has a `latex` output option, which wraps the output of a block inside `#+BEGIN_EXPORT latex`, but this still does not render inline - only in, for example, subsequently produced PDF files.

The trick is to set the Babel block's result to `raw`, which will insert the output string into the org file wholesale - we make sure the output string is valid inline LaTeX by manually wrapping it inside `\[\]`.

*Note: using `raw`, multiple runs of the same cell turn out to continuously append output to the `#+RESULTS` block; `replace` is ignored. Thankfully `drawer` also works, and is compatible with `replace`.*

We need to include a preamble Babel block somewhere in the org file, defining a helper that will wrap raw LaTeX strings for inline display:

```python
#+BEGIN_SRC python :session
from sympy import *

def inline(s):
    return "\[" + s + "\]\n"
#+END_SRC
```

Then, wherever we want to use SymPy and render its output, we include a Babel block with `raw` output (here we compute and display a few common integrals - although admittedly some are in quite a non-standard form), making use of SymPy's `latex` formatter to generate LaTeX strings we can stitch together. The `#+RESULTS` block ends up containing valid LaTeX which renders automatically (if using org-fragtog) or which can be toggled using `org-latex-preview`.

```python
#+BEGIN_SRC python :session :results replace raw
x = symbols("x")
functions = [
    x*2,
    ln(x),
    csc(x),
    atan(x),
    exp(x**2)
]
display = lambda f: inline(latex(Integral(f, x)) + " = " + latex(integrate(f)) + " + c")
"\n".join(map(display, functions))
#+END_SRC
```

Which spits out something like the image at the top of this page. Make a change, hit `C-c C-c`, and get updated equations from SymPy near-instantly.
