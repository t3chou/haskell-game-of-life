# Conway's Game in Haskell

## Updated per Milestone 2 requirements

- What is the architecture of your application (the key components)?

Currently, we finished the basic functionality of the program to run in the command line but not using brick. As you can observe in the source code directory, there are three files under the src directory, and the `evolution` function in `Life.hs` is the key part for the transition to happen. In the next step, we plan to make a GUI interface using brick and connect the basic logic with the foreground’s fancy appearance. 

- What challenges (if any) did you have so far and how did you solve them?

I believe using Haskell to program is the biggest challenge right now. It is the first time for everyone to use Haskell, we are not familiar with a lot of details. However, once we get over the initial learning curve and get familiar with those libraries, things should become easier. To answer how to solve them, we need to program more using Rust to solve it.  

- Do you expect to meet your goals until the deadline?

We should be able to do that. 

- If not, how will you modify your goals?

We do not plan to modify our goals, even though the integration functionality might be simpler than the original proposed one. 

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
