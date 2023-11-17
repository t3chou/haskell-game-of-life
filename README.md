# Conway's Game in Haskell

## Members of the Group
A59023999, A59023768, A59025027, A59012427

## Overview
This project implements Conway's Game of Life by using Haskell, which is great for modeling complex mathematical concepts with the design of Haskell. The Game of Life, a zero-player game, evolves from an initial state, showing emergent behavior from some simple rules(Birth, Survival, and Death). Haskell's strong typing and pure functional nature align perfectly with this simulation.

## Game Description
The Game of Life occurs on an infinite grid of cells, each alive or dead, and these cells will change states based on neighboring cells. And the main rules are:
- **Birth**: A dead cell with three live neighbors becomes alive.
- **Survival**: A live cell with two or three live neighbors stays alive.
- **Death**:
  - **Overpopulation**: A live cell with over three live neighbors dies.
  - **Loneliness**: A live cell with under two live neighbors dies.

## Haskell and Game Implementation
Haskell's syntax is fit for expressing the game's rules. Key aspects of our implementation will include:

1. **Functional Grid Representation**: Using list comprehensions and higher-order functions for the grid.
2. **State Transformation**: Pure functions for predictable state transitions between generations.
3. **Interactive Interface**: Developing a user-friendly interface with the brick library.
4. **Interaction Functionality**: Adding pause/continue functionality into this game, boosting the interaction between users and the game.
5. **Unit Testing**: Using QuickCheck to test edge cases, game logic, and added functions.
6. **Managing Dependencies**: Making use of tools like Cabal or Stack to manage dependencies.

## Project Goals
1. **Mathematical Fidelity**: Ensure full and accurate implementation of the game that follows Conway’s rules.
2. **Interactive Interface**: Create a high-level interface for user interaction with the game by brick library.
3. **User Interactions**: Managing user behavior like key presses for pausing/continuing the game or quitting.
4. **Use existing libraries of the hackage ecosystem**: Utilizing existing libraries like containers and vector.

This project aims to create a robust Conway's Game of Life simulation in Haskell, showcasing the synergy between a mathematical model and functional programming.
