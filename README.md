# samegame
An Elm implementation of the color matching game SameGame.

## The game
The game looks like this:
![samegame](https://user-images.githubusercontent.com/284189/132958297-1ac3f653-3d7f-4d61-a5d9-5a7f84764651.png)

When you click a cell, it and all adjacent cells of the same color are removed, as long as there's at least one such neighbor.

After they've been removed, cells that were resting on top of the removed cells will fall downwards. If a column becomes completely empty, columns to the right of it shift towards the left.

The goal of the game is to get as high a score as possible. The score increases every time a group of cells is removed. Removing 2 cells scores 1 point, 3 cells scores 4 points, 4 scores 9, 5 scores 16, and so on.

The game ends when there are no more cells with neighbors of the same color.

## How to build
First you need to install Elm, which you can do from https://guide.elm-lang.org/install/

To run in a local webserver, simply run `elm reactor` at the root of the project. Open localhost:8000 in a browser, and navigate to src/Main.elm.

To build a self-contained version, run `elm make src/Main.elm`. This will create an index.html file with everything included.
