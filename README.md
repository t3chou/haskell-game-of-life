# Conway's Game in Haskell

## Members of the Group
A59023999, A59023768, A59025027, A59012427

## Overview
This project implements Conway's Game of Life by using Haskell, which is great for modeling complex mathematical concepts with the design of Haskell. The Game of Life, a zero-player game, evolves from an initial state, showing emergent behavior based on some rules and status (Birth, Survival, and Death). Haskell's strong typing and pure functionality align perfectly with this game.

## Game Description

The Game of Life occurs on an infinite grid of cells. The status of each cell will be changed based on neighboring cells following those rules:
- **Birth**: A dead cell with three live neighbors becomes alive.
- **Survival**: A live cell with two or three live neighbors stays alive.
- **Death**:
  - **Overpopulation**: A live cell with over three live neighbors dies.
  - **Loneliness**: A live cell with under two live neighbors dies.

## implementation

Haskell's fits for expressing the game's rules. Key aspects of our implementation include:

1. **Functional Grid Representation**: Using list comprehensions and higher-order functions for the grid.
2. **State Transformation**: Pure functions for state transitions between generations.
4. **Interaction Functionality**: Implementing pause/continue in the game, boosting the interaction between users and the game with the help of the brick library.
5. **Unit Testing**: Using QuickCheck to test each functionality, and game logic.
6. **Managing Dependencies**: Making use of tools like Cabal or Stack to manage dependencies.

## Project Goals

1. **Mathematical Fidelity**: Ensure full implementation of the game following Conway’s rules.
3. **User Interactions**: Interacting with a user-friendly interface by key presses for pausing/continuing the game or quitting using the brick library.
4. **Using existing libraries of the hackage ecosystem**: Utilizing existing libraries like containers and vector.

This project aims to create a Conway's Game of Life simulation in Haskell, as well as representing a string connection between a mathematical model and functional programming.
