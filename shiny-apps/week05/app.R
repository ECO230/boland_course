# MUST be at the very top of app.R, before any library(...)
if (requireNamespace("renv", quietly = TRUE)) {
  renv::load("/data/junior/boland_course")
}

library(shiny)

# ----------------------------
# Chess utilities
# ----------------------------
`%||%` <- function(a, b) if (!is.null(a)) a else b

square_to_rc <- function(sq) {
  file <- substr(sq, 1, 1)
  rank <- as.integer(substr(sq, 2, 2))
  col <- match(file, letters[1:8])
  row <- 9 - rank
  c(row = row, col = col)
}

rc_to_square <- function(row, col) paste0(letters[col], as.character(9 - row))

empty_board <- function() {
  matrix("", nrow = 8, ncol = 8, dimnames = list(as.character(8:1), letters[1:8]))
}

initial_board <- function() {
  b <- empty_board()
  b[1, ] <- c("bR","bN","bB","bQ","bK","bB","bN","bR")
  b[2, ] <- rep("bP", 8)
  b[7, ] <- rep("wP", 8)
  b[8, ] <- c("wR","wN","wB","wQ","wK","wB","wN","wR")
  b
}

piece_color <- function(p) if (p == "") "" else substr(p, 1, 1)
piece_type  <- function(p) if (p == "") "" else substr(p, 2, 2)

piece_name <- function(pt) {
  switch(pt, P="pawn", N="knight", B="bishop", R="rook", Q="queen", K="king", "")
}

piece_unicode <- function(p) {
  if (p == "") return("")
  mapping <- list(
    wK="\u265A", wQ="\u265B", wR="\u265C", wB="\u265D", wN="\u265E", wP="\u265F",
    bK="\u265A", bQ="\u265B", bR="\u265C", bB="\u265D", bN="\u265E", bP="\u265F"
  )
  mapping[[p]] %||% ""
}

apply_uci_move <- function(board, uci) {
  from <- substr(uci, 1, 2)
  to   <- substr(uci, 3, 4)
  promo <- if (nchar(uci) >= 5) substr(uci, 5, 5) else ""
  
  frc <- square_to_rc(from)
  trc <- square_to_rc(to)
  
  moving <- board[frc["row"], frc["col"]]
  captured <- board[trc["row"], trc["col"]]
  
  info <- list(from=from, to=to, moving=moving, captured=captured, promo=promo, castle=FALSE, en_passant=FALSE)
  
  board[frc["row"], frc["col"]] <- ""
  
  # Castling
  if (piece_type(moving) == "K" && abs(trc["col"] - frc["col"]) == 2) {
    if (trc["col"] > frc["col"]) {
      rook_from <- rc_to_square(frc["row"], 8)
      rook_to   <- rc_to_square(frc["row"], 6)
    } else {
      rook_from <- rc_to_square(frc["row"], 1)
      rook_to   <- rc_to_square(frc["row"], 4)
    }
    rfrc <- square_to_rc(rook_from)
    rtrc <- square_to_rc(rook_to)
    rook_piece <- board[rfrc["row"], rfrc["col"]]
    board[rfrc["row"], rfrc["col"]] <- ""
    board[rtrc["row"], rtrc["col"]] <- rook_piece
    info$castle <- TRUE
    info$rook_from <- rook_from
    info$rook_to <- rook_to
  }
  
  # En passant
  if (piece_type(moving) == "P" && captured == "" && frc["col"] != trc["col"]) {
    cap_row <- if (piece_color(moving) == "w") trc["row"] + 1 else trc["row"] - 1
    if (cap_row >= 1 && cap_row <= 8) {
      ep_captured <- board[cap_row, trc["col"]]
      if (piece_type(ep_captured) == "P" && piece_color(ep_captured) != piece_color(moving)) {
        board[cap_row, trc["col"]] <- ""
        info$en_passant <- TRUE
        info$captured <- ep_captured
        info$captured_square <- rc_to_square(cap_row, trc["col"])
      }
    }
  }
  
  placed <- moving
  if (promo != "" && piece_type(moving) == "P") {
    placed <- paste0(piece_color(moving), toupper(promo))
    info$promo_piece <- placed
  }
  
  board[trc["row"], trc["col"]] <- placed
  
  list(board=board, info=info)
}

MIDDLE_SQUARES <- as.vector(outer(letters[2:7], 2:7, paste0))
random_middle_square <- function() sample(MIDDLE_SQUARES, 1)
piece_content <- function(p) {
  tags$span(class=paste("piece", if (piece_color(p) == "w") "white" else "black"),
            piece_unicode(p))
}

# ----------------------------
# Rendering (centered 1x1, centered 3x3, full 8x8)
# ----------------------------
render_board_any <- function(board, mode = c("dot","piece"),
                             show = c("full","one","three"),
                             target_sq,
                             highlight_sq = NULL, id_prefix = "full-") {
  mode <- match.arg(mode)
  show <- match.arg(show)
  
  if (show == "full") {
    grid_dim <- 8
    squares <- matrix("", nrow=8, ncol=8)
    for (r in 1:8) for (c in 1:8) squares[r,c] <- rc_to_square(r,c)
  } else if (show == "one") {
    grid_dim <- 1
    squares <- matrix(target_sq, nrow=1, ncol=1)
  } else {
    grid_dim <- 3
    trc <- square_to_rc(target_sq)
    rows <- (trc["row"]-1):(trc["row"]+1)
    cols <- (trc["col"]-1):(trc["col"]+1)
    squares <- matrix(NA_character_, nrow=3, ncol=3)
    for (i in 1:3) for (j in 1:3) {
      r <- rows[i]; c <- cols[j]
      if (r>=1 && r<=8 && c>=1 && c<=8) squares[i,j] <- rc_to_square(r,c)
    }
  }
  
  cell_divs <- list()
  for (r in 1:grid_dim) for (c in 1:grid_dim) {
    sq <- squares[r,c]
    if (is.na(sq) || sq=="") {
      cell_divs[[length(cell_divs)+1]] <- tags$div(class="sq hidden", "")
      next
    }
    rc <- square_to_rc(sq)
    p <- board[rc["row"], rc["col"]]
    
    is_dark <- ((rc["row"] + rc["col"]) %% 2 == 0)
    base_class <- if (is_dark) "sq dark" else "sq light"
    
    content <- ""
    if (mode == "dot") {
      if (p != "") {
        content <- if (piece_color(p) == "w") tags$span(class="dot white", "\u25CF")
        else tags$span(class="dot black", "\u25CF")
      }
    } else {
      content <- piece_content(p)
    }
    
    extra <- if (!is.null(highlight_sq) && sq == highlight_sq) " highlight" else ""
    cell_divs[[length(cell_divs)+1]] <- tags$div(class=paste0(base_class, extra), id=if (show == "full") paste0(id_prefix, sq) else NULL, content)
  }
  
  tags$div(
    class="board-wrap",
    tags$div(
      class="board",
      style=paste0(
        "grid-template-columns: repeat(", grid_dim, ", 78px); ",
        "grid-template-rows: repeat(", grid_dim, ", 78px);"
      ),
      cell_divs
    )
  )
}

# ----------------------------
# Five famous games (UCI)
# ----------------------------
GAMES <- list(
  list(
    name = "The Immortal Game (Anderssen–Kieseritzky, 1851)",
    moves = strsplit(
      paste(
        "e2e4 e7e5 f2f4 e5f4 f1c4 d8h4 g1f3 h4h6 d2d4 h6h5",
        "c1f4 g7g5 h2h4 h5g6 h4h5 g6g2 h1f1 g2e4 f3e5 d7d5 c4d5",
        "f4c7 e8d8 d5e4 g8f6 c7e5 f6e4 f1f4 e4d6 e5d6 c7d6 f4f7 d8e8 d6e7"
      ),
      " "
    )[[1]]
  ),
  list(
    name = "The Opera Game (Morphy–Duke/Count, 1858)",
    moves = strsplit(
      paste(
        "e2e4 e7e5 g1f3 d7d6 d2d4 c8g4 d4e5 g4f3 d1f3 d6e5 f1c4 g8f6",
        "f3b3 d8e7 b1c3 c7c6 c1g5 b7b5 c3b5 c6b5 c4b5 b8d7 e1c1 a8d8 d1d7 d8d7 b3b8 d7d8 b8d8"
      ),
      " "
    )[[1]]
  ),
  list(
    name = "Game of the Century (Byrne–Fischer, 1956)",
    moves = strsplit(
      paste(
        "g1f3 g8f6 c2c4 g7g6 b1c3 f8g7 d2d4 e8g8 c1f4 d7d5 d1b3 d5c4 b3c4 c7c6",
        "e2e4 b7b5 c4c5 a7a6 a2a4 b5b4 c3e2 f6e4 f4e5 g7e5 c5e5 d8a5 e5f4 e4c3 b2c3 a5c3 e1e2 c3b2 e2e3 b2c3 f1d3 c3c1 a1c1"
      ),
      " "
    )[[1]]
  ),
  list(
    name = "Kasparov–Topalov (Wijk aan Zee, 1999)",
    moves = strsplit(
      paste(
        "e2e4 d7d6 d2d4 g8f6 b1c3 g7g6 c1e3 f8g7 d1d2 c7c6 f2f3 b7b5 g1e2 b8d7",
        "e3h6 g7h6 d2h6 a7a5 a2a3 a5a4 e2f4 e7e5 f4h5 e5d4 c3d5 c6d5 e4d5 f6h5 h6h5 g6h5 f1b5 e8f8 e1c1 d7f6 b5c4 f8g7"
      ),
      " "
    )[[1]]
  ),
  list(
    name = "Deep Blue–Kasparov (1997, Game 6)",
    moves = strsplit(
      paste(
        "e2e4 c7c6 d2d4 d7d5 b1c3 d5e4 c3e4 b8d7 f1d3 g8f6 d1e2 e7e6 g1f3 f8e7",
        "e1g1 e8g8 c1g5 f6e4 g5e7 d8e7 e2e4 e7e4 d3e4 f7f5 e4c4 g8h8 a1e1 d7f6 c4e6"
      ),
      " "
    )[[1]]
  )
)

GAME_CHOICES <- setNames(as.character(seq_along(GAMES)), paste0("Game ", seq_along(GAMES)))

# ----------------------------
# UI
# ----------------------------
ui <- navbarPage(
  title = "Game Simulator",
  header = tags$head(
    tags$script(HTML("
      Shiny.addCustomMessageHandler('board-patch', function(message) {
        message.cells.forEach(function(cell) {
          ['full-', 'combined-'].forEach(function(prefix) {
            var square = document.getElementById(prefix + cell.square);
            if (square) square.innerHTML = cell.html;
          });
        });
        document.querySelectorAll('#board4 .sq, #board5full .sq').forEach(function(square) {
          square.classList.toggle('highlight', square.id === 'full-' + message.target || square.id === 'combined-' + message.target);
        });
      });
    ")),
    tags$style(HTML("
      .app-wrap { max-width: 1200px; margin: 0 auto; }
      .board-wrap { display:flex; justify-content:center; align-items:center; min-height: 640px; }
      .board {
        display:grid;
        gap: 8px;
        padding: 18px;
        border-radius: 18px;
        background: #0e0e0e;
        box-shadow: 0 12px 34px rgba(0,0,0,0.30);
      }
      .sq {
        width: 78px; height: 78px;
        display:flex; align-items:center; justify-content:center;
        border-radius: 16px;
        user-select:none;
        font-size: 46px;
        line-height:1;
      }
      .light { background:#f2e6d8; }
      .dark  { background:#b88964; }
      .hidden { background:#111 !important; }
      .highlight {
        outline: 6px solid rgba(255,215,0,0.95);
        outline-offset: -6px;
        box-shadow: inset 0 0 0 2px rgba(0,0,0,0.25);
      }
      .piece.white { color:#fff; -webkit-text-stroke:1px #222; paint-order:stroke fill; }
      .piece.black { color:#111; }
      .piece { font-family: 'DejaVu Sans', 'Segoe UI Symbol', serif; filter: drop-shadow(0 2px 2px rgba(0,0,0,0.25)); }
      .dot { font-size: 36px; }
      .dot.white { color:#f7f7f7; text-shadow:0 2px 2px rgba(0,0,0,0.35); }
      .dot.black { color:#111; text-shadow:0 2px 2px rgba(255,255,255,0.25); }

      .panel {
        padding: 14px;
        border-radius: 16px;
        background: rgba(255,255,255,0.78);
        border: 1px solid rgba(0,0,0,0.06);
      }
      .tally { max-height: 520px; overflow-y:auto; padding-right:8px; }
      .small-muted { color: rgba(0,0,0,0.6); font-size:12px; }
      .status-pill {
        display:inline-block; padding: 6px 10px; border-radius: 999px;
        background: rgba(0,0,0,0.06);
        margin-left: 8px; font-size: 12px;
      }
      .combined-views { display:grid; grid-template-columns:repeat(3, minmax(0, 1fr)); gap:16px; }
      .combined-view { min-width:0; text-align:center; }
      .combined-views .board-wrap { min-height:280px; }
      .combined-full { grid-column:1 / -1; }
      .combined-views .board { gap:4px; padding:10px; max-width:100%; }
      .combined-full .board { grid-template-columns:repeat(8, minmax(0, 1fr)) !important; grid-template-rows:repeat(8, auto) !important; width:min(100%, 620px); }
      .combined-full .sq { width:100%; height:auto; aspect-ratio:1; border-radius:8px; font-size:clamp(22px, 4vw, 46px); }
      @media (max-width:900px) {
        .combined-views { grid-template-columns:1fr; }
        .combined-views .board-wrap { min-height:120px; }
      }
      .controls-row { display:flex; gap: 10px; flex-wrap: wrap; }
    "))
  ),
  
  tabPanel(
    "Condition 1 — Dot only",
    div(class="app-wrap",
        sidebarLayout(
          sidebarPanel(
            class="panel",
            div(class="controls-row",
                selectInput("game1", "Select game", choices = GAME_CHOICES, selected = "1"),
                selectInput("square1", "Square of interest", choices=MIDDLE_SQUARES, selected="e4"),
                actionButton("random1", "Random square"),
                actionButton("reset1", "Reset game"),
                actionButton("previous1", "Previous move"),
                actionButton("next1", "Next move")
            ),
            hr(),
            uiOutput("meta1"),
            hr(),
            div(class="tally", tableOutput("tally1"))
          ),
          mainPanel(class="panel", uiOutput("board1"))
        )
    )
  ),
  
  tabPanel(
    "Condition 2 — Piece shapes",
    div(class="app-wrap",
        sidebarLayout(
          sidebarPanel(
            class="panel",
            div(class="controls-row",
                selectInput("game2", "Select game", choices = GAME_CHOICES, selected = "1"),
                selectInput("square2", "Square of interest", choices=MIDDLE_SQUARES, selected="e4"),
                actionButton("random2", "Random square"),
                actionButton("reset2", "Reset game"),
                actionButton("previous2", "Previous move"),
                actionButton("next2", "Next move")
            ),
            hr(),
            uiOutput("meta2"),
            hr(),
            div(class="tally", tableOutput("tally2"))
          ),
          mainPanel(class="panel", uiOutput("board2"))
        )
    )
  ),
  
  tabPanel(
    "Condition 3 — Local perimeter",
    div(class="app-wrap",
        sidebarLayout(
          sidebarPanel(
            class="panel",
            div(class="controls-row",
                selectInput("game3", "Select game", choices = GAME_CHOICES, selected = "1"),
                selectInput("square3", "Square of interest", choices=MIDDLE_SQUARES, selected="e4"),
                actionButton("random3", "Random square"),
                actionButton("reset3", "Reset game"),
                actionButton("previous3", "Previous move"),
                actionButton("next3", "Next move")
            ),
            hr(),
            uiOutput("meta3"),
            hr(),
            div(class="tally", tableOutput("tally3"))
          ),
          mainPanel(class="panel", uiOutput("board3"))
        )
    )
  ),
  
  tabPanel(
    "Condition 4 — Full board",
    div(class="app-wrap",
        sidebarLayout(
          sidebarPanel(
            class="panel",
            div(class="controls-row",
                selectInput("game4", "Select game", choices = GAME_CHOICES, selected = "1"),
                selectInput("square4", "Square of interest", choices=MIDDLE_SQUARES, selected="e4"),
                actionButton("random4", "Random square"),
                actionButton("reset4", "Reset game"),
                actionButton("previous4", "Previous move"),
                actionButton("next4", "Next move")
            ),
            hr(),
            uiOutput("meta4"),
            hr(),
            div(class="tally", tableOutput("tally4"))
          ),
          mainPanel(class="panel", uiOutput("board4"))
        )
    )
  ),
  tabPanel(
    "Condition 5 - All views",
    div(class="app-wrap",
      p("Each view shows the same game at the same step. Use the full board to explain what appears in the simpler views, then predict the next dot before advancing."),
      sidebarLayout(
        sidebarPanel(class="panel",
          div(class="controls-row",
            selectInput("game5", "Select game", choices=GAME_CHOICES, selected="1"),
            selectInput("square5", "Square of interest", choices=MIDDLE_SQUARES, selected="e4"),
            actionButton("random5", "Random square"),
            actionButton("reset5", "Reset game"),
            actionButton("previous5", "Previous move"),
            actionButton("next5", "Next move")
          ),
          hr(), uiOutput("meta5"), hr(), uiOutput("connection5")
        ),
        mainPanel(class="panel",
          div(class="combined-views",
            div(class="combined-view", h4("Condition 1: Dot only"), uiOutput("board5dot")),
            div(class="combined-view", h4("Condition 2: Piece shapes"), uiOutput("board5piece")),
            div(class="combined-view", h4("Condition 3: Local perimeter"), uiOutput("board5local")),
            div(class="combined-view combined-full", h4("Condition 4: Full board"), uiOutput("board5full"))
          )
        )
      )
    )
  )
)
# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  
  state <- reactiveValues(
    board = initial_board(),
    game_id = 1L,
    moves = GAMES[[1]]$moves,
    idx = 0L,
    # hidden until condition 4 UI displays it
    target = "e4",
    # logs
    log1 = data.frame(step=integer(0), seen=character(0), stringsAsFactors = FALSE),
    log2 = data.frame(step=integer(0), seen=character(0), stringsAsFactors = FALSE),
    log3 = data.frame(step=integer(0), event=character(0), stringsAsFactors = FALSE),
    log4 = data.frame(step=integer(0), note=character(0), stringsAsFactors = FALSE),
    prev_target_piece = ""
  )
  
  history <- list()
  snapshot <- function() {
    list(board=state$board, idx=state$idx, log1=state$log1, log2=state$log2,
         log3=state$log3, log4=state$log4, prev_target_piece=state$prev_target_piece)
  }
  previous_one <- function() {
    if (state$idx == 0L) return(invisible(FALSE))
    saved <- history[[state$idx]]
    for (name in names(saved)) state[[name]] <- saved[[name]]
    invisible(TRUE)
  }
  reset_game <- function(game_id) {
    history <<- list()
    state$game_id <- as.integer(game_id)
    state$moves <- GAMES[[state$game_id]]$moves
    state$board <- initial_board()
    state$idx <- 0L
    
    # Reset preserves the selected square.
    
    state$log1 <- data.frame(step=integer(0), seen=character(0), stringsAsFactors = FALSE)
    state$log2 <- data.frame(step=integer(0), seen=character(0), stringsAsFactors = FALSE)
    state$log3 <- data.frame(step=integer(0), event=character(0), stringsAsFactors = FALSE)
    state$log4 <- data.frame(step=integer(0), note=character(0), stringsAsFactors = FALSE)
    
    rc <- square_to_rc(state$target)
    state$prev_target_piece <- state$board[rc["row"], rc["col"]]
  }
  
  # Shared "advance one move" (board updates + logs update immediately)
  advance_one <- function() {
    n <- length(state$moves)
    if (state$idx >= n) return(invisible(FALSE))
    
    history[[state$idx + 1L]] <<- snapshot()
    uci <- state$moves[state$idx + 1]
    
    # before-state
    from <- substr(uci, 1, 2)
    to   <- substr(uci, 3, 4)
    frc <- square_to_rc(from)
    trc <- square_to_rc(to)
    moving_before <- state$board[frc["row"], frc["col"]]
    captured_before <- state$board[trc["row"], trc["col"]]
    
    res <- apply_uci_move(state$board, uci)
    state$board <- res$board
    info <- res$info
    
    state$idx <- state$idx + 1L
    step <- state$idx
    
    # after-state at target
    tgt_rc <- square_to_rc(state$target)
    tgt_piece <- state$board[tgt_rc["row"], tgt_rc["col"]]
    
    # Condition 1 log
    seen1 <- if (tgt_piece == "") "empty" else if (piece_color(tgt_piece) == "w") "white dot" else "black dot"
    state$log1 <- rbind(state$log1, data.frame(step=step, seen=seen1, stringsAsFactors = FALSE))
    
    # Condition 2 log
    seen2 <- if (tgt_piece == "") {
      "empty"
    } else {
      col <- if (piece_color(tgt_piece) == "w") "white" else "black"
      paste(col, piece_name(piece_type(tgt_piece)))
    }
    state$log2 <- rbind(state$log2, data.frame(step=step, seen=seen2, stringsAsFactors = FALSE))
    
    # Condition 3 log: only when target changes (no coordinates)
    prev <- state$prev_target_piece
    if (tgt_piece != prev) {
      col <- if (tgt_piece == "") "" else if (piece_color(tgt_piece) == "w") "White" else "Black"
      nm  <- if (tgt_piece == "") "empty" else paste(col, tools::toTitleCase(piece_name(piece_type(tgt_piece))))
      state$log3 <- rbind(state$log3, data.frame(step=step, event=paste0("Square changed: now ", nm), stringsAsFactors = FALSE))
      state$prev_target_piece <- tgt_piece
    }
    
    # Condition 4 log: moves that touch target (with coords + move-like text)
    touches <- (from == state$target) || (to == state$target) ||
      (isTRUE(info$en_passant) && identical(info$captured_square, state$target))
    
    if (touches) {
      mover_col <- if (piece_color(moving_before) == "w") "White" else "Black"
      mover_pt  <- tools::toTitleCase(piece_name(piece_type(moving_before)))
      note <- paste0(mover_col, " ", mover_pt, " ", from, " \u2192 ", to)
      if (captured_before != "" || isTRUE(info$en_passant)) note <- paste0(note, " (capture)")
      if (isTRUE(info$castle)) note <- paste0(note, " (castling)")
      if (!is.null(info$promo_piece)) {
        note <- paste0(note, " (promotion to ", tools::toTitleCase(piece_name(piece_type(info$promo_piece))), ")")
      }
      state$log4 <- rbind(state$log4, data.frame(step=step, note=note, stringsAsFactors = FALSE))
    }
    
    invisible(TRUE)
  }
  
  # All conditions share the same game, square, board, and history.
  select_square <- function(square) {
    if (is.null(square) || !square %in% MIDDLE_SQUARES || square == state$target) return()
    state$target <- square
    reset_game(state$game_id)
  }
  for (tab in 1:5) local({
    i <- tab
    observeEvent(input[[paste0("reset", i)]], reset_game(state$game_id))
    observeEvent(input[[paste0("game", i)]], {
      game <- as.integer(input[[paste0("game", i)]])
      if (game != state$game_id) reset_game(game)
    }, ignoreInit=TRUE)
    observeEvent(input[[paste0("square", i)]],
                 select_square(input[[paste0("square", i)]]), ignoreInit=TRUE)
    observeEvent(input[[paste0("random", i)]],
                 select_square(sample(setdiff(MIDDLE_SQUARES, state$target), 1)))
    observeEvent(input[[paste0("next", i)]], advance_one())
    observeEvent(input[[paste0("previous", i)]], previous_one())
  })
  observe({
    for (i in 1:5) {
      updateSelectInput(session, paste0("game", i), selected=as.character(state$game_id))
      updateSelectInput(session, paste0("square", i), selected=state$target)
    }
  })
  # Meta UIs:
  meta_generic <- function() {
    tags$div(
      tags$div(tags$strong("Simulator")),
      tags$div(tags$strong("Step: "), paste0(state$idx, " / ", length(state$moves)))
    )
  }
  meta_full <- function() {
    tags$div(
      tags$div(tags$strong("Simulator")),
      tags$div(tags$strong("Game: "), GAMES[[state$game_id]]$name),
      tags$div(tags$strong("Square of interest: "), state$target),
      tags$div(tags$strong("Step: "), paste0(state$idx, " / ", length(state$moves)))
    )
  }
  
  output$meta1 <- renderUI(meta_generic())
  output$meta2 <- renderUI(meta_generic())
  output$meta3 <- renderUI(meta_generic())
  output$meta4 <- renderUI(meta_full())
  
  output$meta5 <- renderUI(meta_full())
  output$connection5 <- renderUI({
    rc <- square_to_rc(state$target)
    p <- state$board[rc["row"], rc["col"]]
    description <- if (p == "") "empty" else
      paste(if (piece_color(p) == "w") "a white" else "a black", piece_name(piece_type(p)))
    dot <- if (p == "") "an empty square" else
      paste("a", if (piece_color(p) == "w") "white" else "black", "dot")
    tags$div(
      tags$p(paste("On the full board,", state$target, "is", description,
                   "- so Condition 1 shows", paste0(dot, "."))),
      tags$p("Condition 2 adds the piece's identity. Condition 3 adds its neighbors. The full board provides the system that explains their changes."),
      tags$p("Predict: will the next move enter or leave the highlighted square, or leave it unchanged?")
    )
  })
  output$board5dot <- renderUI(render_board_any(state$board, "dot", "one", state$target, state$target))
  output$board5piece <- renderUI(render_board_any(state$board, "piece", "one", state$target, state$target))
  output$board5local <- renderUI(render_board_any(state$board, "piece", "three", state$target, state$target))
  output$board5full <- renderUI({
    isolate(render_board_any(state$board, "piece", "full", state$target, state$target,
                             id_prefix="combined-"))
  })

  # Boards
  output$board1 <- renderUI({
    render_board_any(state$board, mode="dot", show="one", target_sq=state$target, highlight_sq=state$target)
  })
  output$board2 <- renderUI({
    render_board_any(state$board, mode="piece", show="one", target_sq=state$target, highlight_sq=state$target)
  })
  output$board3 <- renderUI({
    render_board_any(state$board, mode="piece", show="three", target_sq=state$target, highlight_sq=state$target)
  })
  output$board4 <- renderUI({
    isolate(render_board_any(state$board, mode="piece", show="full", target_sq=state$target, highlight_sq=state$target))
  })
  
  # Keep full-board cells mounted; send only pieces that changed.
  last_board <- NULL
  observe({
    board <- state$board
    target <- state$target
    changed <- if (is.null(last_board)) which(matrix(TRUE, 8, 8), arr.ind=TRUE) else
      which(board != last_board, arr.ind=TRUE)
    cells <- lapply(seq_len(nrow(changed)), function(i) {
      r <- changed[i, 1]; c <- changed[i, 2]
      list(square=rc_to_square(r, c), html=as.character(piece_content(board[r, c])))
    })
    session$sendCustomMessage("board-patch", list(cells=unname(cells), target=target))
    last_board <<- board
  })

  # Tallies
  output$tally1 <- renderTable({ if (nrow(state$log1) == 0) data.frame() else state$log1 },
                               striped=TRUE, hover=TRUE, spacing="s")
  output$tally2 <- renderTable({ if (nrow(state$log2) == 0) data.frame() else state$log2 },
                               striped=TRUE, hover=TRUE, spacing="s")
  output$tally3 <- renderTable({ if (nrow(state$log3) == 0) data.frame() else state$log3 },
                               striped=TRUE, hover=TRUE, spacing="s")
  output$tally4 <- renderTable({ if (nrow(state$log4) == 0) data.frame() else state$log4 },
                               striped=TRUE, hover=TRUE, spacing="s")
}

shinyApp(ui, server)