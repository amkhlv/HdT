HdT
===

A handy PdF viewer with the possibility to scribble over pages.
Uses `SVG` overlays editable with [Inkscape](https://inkscape.org/)

Installation
------------

The command line tools [pdftocairo](https://manpages.debian.org/bookworm/poppler-utils/pdftocairo.1.en.html) and
[xmlstarlet](https://en.wikipedia.org/wiki/XMLStarlet) should be installed.

And [Inkscape](https://inkscape.org/).

To build and install `hdt`, run `cabal install` in this directory.

Configuration
-------------

The sample configuration file `hdt.dhall` should be copied to `~/.config/hdt.dhall`

Use
---

### Keyboard shortcuts


    Action                                    Keyboard Shortcut   Button
    Reload file                               Ctrl-l              ↺
    Next Page                                 n                   ⇨
    Previous Page                             p                   ⇦
    Scroll Up Slow                            k
    Scroll Up Fast                            Shift-k
    Scroll Down Slow                          j
    Scroll Down Fast                          Shift-j
    Scroll Left Slow                          h
    Scroll Left Fast                          Shift-h
    Scroll Right Slow                         l
    Scroll Right Fast                         Shift-l
    Zoom In                                   .                   ⊕
    Zoom Out                                  ,                   ⊖
    Go back after link jump                   b                   ↩
    Enter search term                         /
    Look for matches on next pages            Ctrl-n
    Look for matches on previous pages        Ctrl-p
    Toggle highlight of search matches        F1
    Go to page                                g                   go
    Extract text on this page                 t                   txt
    Copy path to filename.pdf to clipboard    c
    Copy path to filename.d to clipboard      d
    -- see [Scribble over with Inkscape] ↓↓↓

Working with bookmarks:

    Open bookmarks                            Ctrl-b
    Add bookmark                              a                   a

Bookmarks are labelled by letters ("charhints", like in [Tridactyl](https://addons.mozilla.org/en-US/firefox/addon/tridactyl-vim/)).
To navigate to the bookmark, press the corresponding key. To delete the bookmark, press that key with `Shift` (like upcase).

### Scribble over with Inkscape

Let us view a file called `paper.pdf` :

    hdt paper.pdf &

To create an SVG overlay over page 3, do this:

    mkdir paper.d
    cd paper.d
    hdt -e 3
    inkscape p3.svg &

While drawing in Inkscape, make sure that you draw on the layer with the id: `layer1`. This should be automatic, just double-check.
It is the `layer1` that becomes the overlay, when viewing the `paper.pdf`.
The overlays are all in `paper.d/`, named `p<n>.svg`, where `n` is the page number.

To see the effect in the viewed file, reload it with `Ctrl-l`.
