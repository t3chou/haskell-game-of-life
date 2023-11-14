# haskell-game-of-life

## Introduction

Welcome to the Conway's Game of Life repository! This project is an implementation of the famous cellular automaton devised by the British mathematician John Horton Conway in 1970. Conway's Game of Life is not a game in the conventional sense but a simulation that follows a set of simple rules to evolve cell structures on a grid through generations.

## Game Description

The Game of Life is a zero-player game, meaning that its evolution is determined by its initial state, requiring no further input from human players. One interacts with the Game of Life by creating an initial configuration and observing how it evolves.

## Rules

The game is played on an infinite, two-dimensional orthogonal grid of square cells, each of which is in one of two possible states, alive or dead. Every cell interacts with its eight neighbors, which are the cells that are horizontally, vertically, or diagonally adjacent. At each step in time, the following transitions occur:

1. **Birth**: A dead cell with exactly three live neighbors becomes a live cell.
2. **Survival**: A live cell with two or three live neighbors stays alive.
3. **Death**:
   - Overpopulation: A live cell with more than three live neighbors dies.
   - Loneliness: A live cell with fewer than two live neighbors dies.

## Goals and Educational Value

The primary goal of Conway's Game of Life is to observe the evolving patterns based on the simple rules. It's fascinating to see how intricate structures can emerge and evolve from simple beginnings. The Game of Life is not only a fun simulation but also an excellent tool for teaching concepts in mathematics, computer science, and theoretical biology, particularly:

- Cellular automata theory
- Emergent behaviors
- Pattern formation
- Fractals and complexity

Enjoy exploring the vast universe of Conway's Game of Life, where simplicity leads to complexity!
