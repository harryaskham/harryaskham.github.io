#!/usr/bin/env python3
"""Draw original tiny pixel birthday GIFs; Pillow is only needed to regenerate."""
from pathlib import Path
from PIL import Image, ImageDraw

OUT = Path(__file__).resolve().parents[1] / "static" / "alex" / "images"
OUT.mkdir(exist_ok=True)
BG = (7, 4, 37)
PINK, CYAN, GOLD, WHITE = "#ff5cce", "#62f5ff", "#fff568", "#ffffff"


def star(draw, x, y, color, size=2):
    draw.line((x-size, y, x+size, y), fill=color)
    draw.line((x, y-size, x, y+size), fill=color)


def cake(frame):
    im = Image.new("RGB", (64, 64), BG)
    d = ImageDraw.Draw(im)
    d.rectangle((8, 53, 56, 56), fill=CYAN)
    d.rectangle((12, 34, 52, 52), fill=PINK, outline=WHITE, width=2)
    d.rectangle((16, 24, 48, 34), fill="#c86aff", outline=WHITE, width=2)
    for x in range(15, 52, 7):
        d.rectangle((x, 35, x+3, 39+(x % 3)), fill=WHITE)
    for x in [22, 32, 42]:
        d.rectangle((x-1, 15, x+1, 24), fill=CYAN)
        y = 9 + (frame + x) % 2
        d.polygon([(x, y), (x-2, y+4), (x, y+6), (x+2, y+4)], fill=GOLD)
    for x, y, c in [(8, 16, CYAN), (55, 27, GOLD), (5, 43, PINK), (51, 8, PINK)]:
        star(d, x, y, c, 1 + frame % 2)
    return im


def balloons(frame):
    im = Image.new("RGB", (64, 64), BG)
    d = ImageDraw.Draw(im)
    for x, y, color in [(14, 13, PINK), (45, 15, CYAN), (30, 8, GOLD)]:
        y += [0, 1, 2, 1][frame % 4]
        d.line([(x, y+19), (x-2, y+25), (x+2, y+32), (x, 57)], fill=WHITE)
        d.ellipse((x-8, y, x+8, y+19), fill=color)
        d.rectangle((x-4, y+4, x-2, y+8), fill=WHITE)
        d.polygon([(x, y+19), (x-2, y+22), (x+2, y+22)], fill=color)
    star(d, 5, 5, CYAN)
    star(d, 57, 44, GOLD, 1+frame%2)
    return im


for name, draw in [("cake", cake), ("balloons", balloons)]:
    frames = [draw(i).resize((128, 128), Image.Resampling.NEAREST) for i in range(4)]
    frames[0].save(OUT / f"{name}.png")
    frames[0].save(OUT / f"{name}.gif", save_all=True, append_images=frames[1:], duration=350, loop=0, optimize=True)

im = Image.new("RGB", (160, 160), BG)
d = ImageDraw.Draw(im)
for x, y, c, size in [(14, 24, "#61568d", 1), (97, 20, "#9582ad", 1), (42, 97, "#8067a0", 2), (136, 123, "#776197", 1), (115, 72, "#534775", 1)]:
    star(d, x, y, c, size)
im.save(OUT / "stars.png", optimize=True)
