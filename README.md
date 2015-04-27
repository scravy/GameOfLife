# Game of Life

[![Build Status](https://travis-ci.org/scravy/GameOfLife.svg)](https://travis-ci.org/scravy/GameOfLife)

An implementation of [Conway's Game of Life](http://en.wikipedia.org/wiki/Conway's_Game_of_Life) written in [Haskell](http://www.haskell.org/) using the [Gloss library](https://hackage.haskell.org/package/gloss).

## Running

Install the [Haskell Platform](http://www.haskell.org/platform) first.

Then do:

    git clone https://github.com/scravy/GameOfLife.git
    cabal run

## Install

    cabal install

## Use

`Space` pauses/unpauses the simulation (so you can start painting an initial worl or altering the current setup).

`Tab` will save the current world to `world.txt`.

You can load a predefined/previously saved scenario like so:

    cabal run spaceships.txt

Chekcout the example files `spaceships.txt` and `glidergun.txt`. You can alter basically anything in there
(world size for example).

