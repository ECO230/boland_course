# MUST be at the very top of app.R, before any library(...)
if (requireNamespace("renv", quietly = TRUE)) {
  renv::load("/data/junior/boland_course")
}

library(shiny)

# ----------------------------
# Helpers
# ----------------------------

# Real coin flips (fair)
flip_real <- function(n) {
  sample(c("H", "T"), size = n, replace = TRUE)
}

# Human pretending to flip a coin:
# Goals:
# - baseline looks like alternating (HTHT...), but not perfectly
# - runs of 2 are common
# - runs of 3 can happen
# - runs of 4 are very unlikely
# - if a run of 4 ever happens once, they refuse to do it again (cap at 3 thereafter)
flip_human <- function(n) {
  out <- character(n)
  out[1] <- sample(c("H", "T"), 1)
  
  run_len <- 1L
  did_run4 <- FALSE
  
  for (i in 2:n) {
    prev <- out[i - 1]
    
    # After they've ever had a run of 4, they refuse to do it again:
    # force switch at run_len >= 3 (so they can still make triples, but never reach 4 again)
    if (isTRUE(did_run4) && run_len >= 3L) {
      out[i] <- if (prev == "H") "T" else "H"
      run_len <- 1L
      next
    }
    
    # Base behavior:
    # run_len = 1: prefers to alternate (switch) but not always -> doubles common
    # run_len = 2: more likely to switch -> triples less common
    # run_len = 3: strongly likely to switch -> 4 is very rare
    p_switch <- switch(
      as.character(run_len),
      "1" = 0.65,
      "2" = 0.78,
      "3" = 0.97,
      0.78
    )
    
    # If already at a run of 4 (should be rare), force switch
    if (run_len >= 4L) {
      p_switch <- 1.0
    }
    
    do_switch <- (runif(1) < p_switch)
    out[i] <- if (do_switch) (if (prev == "H") "T" else "H") else prev
    
    if (out[i] == prev) {
      run_len <- run_len + 1L
    } else {
      run_len <- 1L
    }
    
    # Track if they ever hit a run of 4 anywhere
    if (!did_run4 && run_len >= 4L) did_run4 <- TRUE
  }
  
  out
}

# Build 5 sequences: 4 real + 1 human, random column placement
make_sequences <- function(n) {
  human_col <- sample(1:5, 1)
  seqs <- vector("list", 5)
  
  for (k in 1:5) {
    seqs[[k]] <- if (k == human_col) flip_human(n) else flip_real(n)
  }
  
  list(seqs = seqs, human_col = human_col)
}

# Run length for each position (length of the contiguous run segment that position belongs to)
# Example: H H H T T -> runlen = 3 3 3 2 2
run_lengths <- function(x) {
  n <- length(x)
  if (n == 0) return(integer(0))
  r <- rle(x)
  rep(r$lengths, r$lengths)
}

run_class <- function(runlen) {
  if (is.na(runlen) || runlen < 3) return("")
  if (runlen == 3) return("run3")
  if (runlen == 4) return("run4")
  if (runlen == 5) return("run5")
  return("run6") # 6+
}

# ----------------------------
# UI
# ----------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .panel {
        padding: 14px;
        border-radius: 14px;
        background: rgba(255,255,255,0.88);
        border: 1px solid rgba(0,0,0,0.06);
        margin-bottom: 14px;
      }
      .flip-col-title {
        font-weight: 700;
        text-align: center;
        margin-bottom: 6px;
      }
      .flip-box {
        border-radius: 10px;
        border: 1px solid rgba(0,0,0,0.06);
        overflow: hidden;
      }
      .flip-row {
        text-align: center;
        font-weight: 700;
        font-size: 16px;
        padding: 2px 0;
        line-height: 1.35;
      }

      /* Run highlights (length >= 3). Longer run -> stronger highlight */
      .flip-row.run3 { box-shadow: inset 0 0 0 9999px rgba(111,168,220,0.14); }
      .flip-row.run4 { box-shadow: inset 0 0 0 9999px rgba(111,168,220,0.22); }
      .flip-row.run5 { box-shadow: inset 0 0 0 9999px rgba(111,168,220,0.32); }
      .flip-row.run6 { box-shadow: inset 0 0 0 9999px rgba(111,168,220,0.44); }

      .hint { color: rgba(0,0,0,0.65); font-size: 12px; }

      .reveal-real { color: rgba(0,0,0,0.80); }
      .reveal-human { color: #8E3F3F; }

      /* Badges with more contrast */
      .badge {
        display: inline-block;
        padding: 2px 8px;
        border-radius: 999px;
        font-size: 12px;
        margin-left: 6px;
        border: 1px solid rgba(0,0,0,0.18);
        background: rgba(0,0,0,0.10);
        color: rgba(0,0,0,0.88);
      }
      .badge.human {
        border-color: rgba(179,90,90,0.40);
        background: rgba(179,90,90,0.14);
        color: #7A2F2F;
      }
    "))
  ),
  
  div(
    class = "panel",
    h3("Coin Flip Simulator"),
    div(
      class = "hint",
      "Simulate 5 sequences. Four are real flips; one is a human pretending. ",
      "Use Reveal when you're ready."
    ),
    br(),
    fluidRow(
      column(
        3,
        selectInput("n", "Number of flips (n)", choices = c(5, 10, 30, 50, 100), selected = 50)
      ),
      column(
        3,
        checkboxInput("show_highlights", "Highlight runs (>=3)", value = FALSE)
      ),
      column(
        6,
        div(
          style = "margin-top: 24px;",
          actionButton("simulate", "Simulate"),
          actionButton("reveal", "Reveal which is human", style = "margin-left: 8px;")
        )
      )
    )
  ),
  
  uiOutput("grid")
)

# ----------------------------
# Server
# ----------------------------
server <- function(input, output, session) {
  
  rv <- reactiveValues(
    seqs = NULL,
    human_col = NULL,
    revealed = FALSE
  )
  
  observeEvent(input$simulate, {
    n <- as.integer(input$n)
    res <- make_sequences(n)
    rv$seqs <- res$seqs
    rv$human_col <- res$human_col
    rv$revealed <- FALSE
  })
  
  observeEvent(input$reveal, {
    if (is.null(rv$seqs)) return()
    rv$revealed <- TRUE
  })
  
  output$grid <- renderUI({
    if (is.null(rv$seqs)) {
      return(div(class = "panel", div(class = "hint", "Click Simulate to generate sequences.")))
    }
    
    n <- length(rv$seqs[[1]])
    do_hl <- isTRUE(input$show_highlights)
    
    cols <- lapply(1:5, function(k) {
      flips <- rv$seqs[[k]]
      rlen <- run_lengths(flips)
      
      if (!isTRUE(rv$revealed)) {
        title <- "Coin Flip"
        badge <- NULL
        title_class <- "flip-col-title"
      } else {
        if (k == rv$human_col) {
          title <- "Coin Flip"
          badge <- span(class = "badge human", "Human (pretending)")
          title_class <- "flip-col-title reveal-human"
        } else {
          title <- "Coin Flip"
          badge <- span(class = "badge", "Real flips")
          title_class <- "flip-col-title reveal-real"
        }
      }
      
      rows <- lapply(seq_len(n), function(i) {
        cls <- "flip-row"
        if (do_hl) {
          rc <- run_class(rlen[i])
          if (nzchar(rc)) cls <- paste(cls, rc)
        }
        div(class = cls, flips[i])
      })
      
      column(
        width = 2,
        div(
          class = "panel",
          div(class = title_class, title, badge),
          div(class = "flip-box", rows)
        )
      )
    })
    
    fluidRow(
      column(1),
      cols[[1]], cols[[2]], cols[[3]], cols[[4]], cols[[5]],
      column(1)
    )
  })
}

shinyApp(ui, server)