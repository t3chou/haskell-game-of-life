# Conway's Game in Haskell

## Members of the Group
A59023999, 

## Overview
This project implements Conway's Game of Life using Haskell, a language ideal for modeling complex mathematical concepts with clarity. The Game of Life, a zero-player game, evolves from an initial state, demonstrating emergent behavior from simple rules. Haskell's strong typing, pure functional nature, and lazy evaluation align perfectly with this simulation.

## Game Description
The Game of Life occurs on an infinite grid of cells, each alive or dead, changing states based on neighboring cells. The rules are:
- **Birth**: A dead cell with three live neighbors becomes alive.
- **Survival**: A live cell with two or three live neighbors stays alive.
- **Death**:
  - **Overpopulation**: A live cell with over three live neighbors dies.
  - **Loneliness**: A live cell with under two live neighbors dies.

## Haskell and Game Implementation
Haskell's syntax is ideal for expressing the game's rules. Key aspects of our implementation will include:

1. **Functional Grid Representation**: Using list comprehensions and higher-order functions for the grid.
2. **State Transformation**: Pure functions for predictable state transitions between generations.
3. **Lazy Evaluation**: Managing potentially infinite grids efficiently.
4. **Performance Optimization**: Utilizing Haskell’s type system and optimization techniques for handling large grids.
5. **Interactive Interface**: Developing a user-friendly interface with the brick library.

## Project Goals
1. **Mathematical Fidelity**: Ensure an accurate implementation of Conway’s rules.
2. **Performance Optimization**: Efficient management of large grids and numerous generations.
3. **Interactive Interface**: Create an accessible interface for user interaction with the game.

This project aims to create a robust Conway's Game of Life simulation in Haskell, showcasing the synergy between a mathematical model and functional programming.
