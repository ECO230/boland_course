# Game Simulator -- Requirements & Implementation Notes

## Overview

This document describes the Game Simulator (Chess Partial Observability
App), including:

1.  Functional requirements
2.  UI structure
3.  State management
4.  Move engine logic
5.  Logging logic per condition
6.  Extension / modification guidance

------------------------------------------------------------------------

# 1. Core Requirements

## High-Level Goal

Build a Shiny application that simulates famous chess games under
varying levels of observability.

Users manually advance the simulation using a **Next Move** button.

There is: - A game selector (Game 1--5) - A Reset button (restart
current game) - A Next Move button (advance one move) - Move progression
is strictly manual (no auto-play)

------------------------------------------------------------------------

# 2. Game Architecture

## Game Data

Five predefined famous games are stored as:

``` r
GAMES <- list(
  list(name = "...", moves = c("e2e4", "e7e5", ...)),
  ...
)
```

Moves are encoded in UCI format: - "e2e4" - "g1f3" - "e7e8q"
(promotion) - Castling handled via king two-square move

------------------------------------------------------------------------

# 3. State Management (Shiny)

The application uses a single shared `reactiveValues()` object:

``` r
state <- reactiveValues(
  board,
  game_id,
  moves,
  idx,
  target,
  log1,
  log2,
  log3,
  log4,
  prev_target_piece
)
```

## Key Variables

  Variable              Purpose
  --------------------- -----------------------------------------------------
  `board`               Current 8x8 chess board matrix
  `moves`               UCI move vector for selected game
  `idx`                 Current move index
  `target`              Random middle square used for partial observability
  `log1–4`              Logs for each condition
  `prev_target_piece`   Used to detect square changes in Condition 3

------------------------------------------------------------------------

# 4. Move Execution Engine

Each click of **Next Move** runs:

``` r
advance_one()
```

### Execution Steps

1.  Read next UCI move
2.  Parse from/to squares
3.  Handle castling
4.  Handle en passant
5.  Handle promotion
6.  Update board matrix
7.  Increment move index
8.  Update logs per condition

------------------------------------------------------------------------

# 5. Conditions (UI Modes)

## Condition 1 -- Dot Only

-   Only one square visible
-   Piece displayed as white or black dot
-   Log records: "white dot", "black dot", or "empty"
-   No coordinates revealed

## Condition 2 -- Piece Shape

-   One square visible
-   Unicode chess symbols displayed
-   Log records: "white knight", "black pawn", etc.
-   No coordinates revealed

## Condition 3 -- Local Perimeter

-   3x3 grid centered on target square
-   Target highlighted
-   Log records when square changes
-   Still no coordinates revealed

## Condition 4 -- Full Board

-   Entire 8x8 board visible
-   Target square highlighted
-   Reveals game name and square
-   Log records full move-style text when move touches target

------------------------------------------------------------------------

# 6. Rendering Strategy

Rendering handled via:

``` r
render_board_any()
```

Supports: - show = "one" - show = "three" - show = "full"

Board built with CSS grid and Unicode chess symbols.

------------------------------------------------------------------------

# 7. Reset Behavior

Reset: - Reloads selected game - Clears logs - Resets board - Resets
move index - Generates new random middle square

------------------------------------------------------------------------

# 8. Design Philosophy

The simulator demonstrates: - Partial observability - Information
asymmetry - How inference changes with context - How seeing local vs
global structure alters predictive ability

------------------------------------------------------------------------

# 9. Modification Guidelines

To add new games: Append to `GAMES` list using UCI move format.

To change observability levels: Modify `render_board_any()` display
logic.

To modify logging: Update `advance_one()` function.

------------------------------------------------------------------------

# 10. Known Constraints

-   No chess engine validation
-   No check/checkmate detection
-   No PGN parsing
-   No undo feature
-   No persistent state across sessions

------------------------------------------------------------------------

# End of Document
