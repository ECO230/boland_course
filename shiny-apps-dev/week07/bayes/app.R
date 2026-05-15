column(12,
       div(class="bfInterpretWrap",
           div(class="bfInterpretTitle", "Bayes Factor (bf10) - Interpretation"),
           div(class="bfInterpretSvg",
               tags$img(src = "bayes_factor.svg", alt = "Bayes factor interpretation diagram")
           )
       )
)# app.R
# Bayes / Prevalence / Test Accuracy / Posterior (Shiny)
# Slide-like build:
# 1) Priors: prevalence + complement (and fixed H_A / H_0)
# 2) Add test results: TP/TN/FP/FN and rates
# 3) Add posterior: emphasize TP & FP -> P(Left | Positive)
# 4) Bayes Factor (Odds update): Prior odds * BF+ = Posterior odds
# 5) Same BF, different prevalence: BF impact is multiplicative on odds across priors

library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

clamp01 <- function(x) pmin(pmax(x, 0), 1)

fmt_pct <- function(x, digits = 0) {
  ifelse(is.na(x), "—", percent(x, accuracy = 10^(-digits)))
}
fmt_pct1 <- function(x) {
  ifelse(is.na(x), "—", percent(x, accuracy = 0.1))
}
fmt_num <- function(x, digits = 0) {
  ifelse(is.na(x), "—", format(round(x, digits), big.mark = ",", nsmall = digits))
}
fmt_odds <- function(o, digits = 2) {
  ifelse(is.na(o), "—", format(round(o, digits), nsmall = digits, scientific = FALSE))
}

# ---- Fixed-count simulation with random assignment (counts pinned to slider values) ----
simulate_population_fixed <- function(n, prevalence, sens, spec, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  prevalence <- clamp01(prevalence)
  sens <- clamp01(sens)
  spec <- clamp01(spec)
  
  ids <- seq_len(n)
  
  # Pin prevalence
  n_left <- round(n * prevalence)
  n_left <- max(0, min(n, n_left))
  n_right <- n - n_left
  
  left_ids <- if (n_left > 0) sample(ids, n_left, replace = FALSE) else integer(0)
  left_group <- ids %in% left_ids
  
  # Pin sensitivity among left
  tp <- round(n_left * sens)
  tp <- max(0, min(n_left, tp))
  fn <- n_left - tp
  tp_ids <- if (tp > 0) sample(left_ids, tp, replace = FALSE) else integer(0)
  
  # Pin specificity among right
  right_ids <- setdiff(ids, left_ids)
  tn <- round(n_right * spec)
  tn <- max(0, min(n_right, tn))
  fp <- n_right - tn
  
  tn_ids <- if (tn > 0) sample(right_ids, tn, replace = FALSE) else integer(0)
  fp_ids <- setdiff(right_ids, tn_ids)
  
  positive <- logical(n)
  positive[tp_ids] <- TRUE
  positive[fp_ids] <- TRUE
  
  outcome <- dplyr::case_when(
    left_group & positive  ~ "TP",
    left_group & !positive ~ "FN",
    !left_group & positive ~ "FP",
    TRUE                   ~ "TN"
  )
  
  tibble(
    id = ids,
    left_group = left_group,
    positive = positive,
    outcome = factor(outcome, levels = c("TP","FN","FP","TN"))
  )
}

metrics_from_df <- function(df) {
  tp <- sum(df$outcome == "TP")
  fn <- sum(df$outcome == "FN")
  fp <- sum(df$outcome == "FP")
  tn <- sum(df$outcome == "TN")
  
  n <- nrow(df)
  n_left <- tp + fn
  n_right <- fp + tn
  n_pos <- tp + fp
  
  ppv <- ifelse(n_pos > 0, tp / n_pos, NA_real_)
  
  list(
    n = n,
    tp = tp, fn = fn, fp = fp, tn = tn,
    n_left = n_left, n_right = n_right,
    n_pos = n_pos,
    ppv = ppv
  )
}

# ---- Grid helpers ----
assign_grid_left_top_left <- function(df) {
  n <- nrow(df)
  rows <- ceiling(sqrt(n))
  cols <- ceiling(n / rows)
  
  df2 <- df %>%
    arrange(desc(left_group), id) %>%
    mutate(idx = row_number()) %>%
    mutate(
      x = ((idx - 1) %% cols) + 1,
      y = rows - ((idx - 1) %/% cols)
    )
  attr(df2, "bounds") <- list(rows = rows, cols = cols)
  df2
}

assign_grid_tp_top_left <- function(df_pos) {
  n <- nrow(df_pos)
  if (n == 0) return(df_pos)
  
  rows <- ceiling(sqrt(n))
  cols <- ceiling(n / rows)
  
  df2 <- df_pos %>%
    arrange(desc(outcome == "TP"), id) %>%
    mutate(idx = row_number()) %>%
    mutate(
      x = ((idx - 1) %% cols) + 1,
      y = rows - ((idx - 1) %/% cols)
    )
  attr(df2, "bounds") <- list(rows = rows, cols = cols)
  df2
}

# ---- Bayes helpers ----
bf_positive <- function(sens, spec) {
  sens <- clamp01(sens)
  spec <- clamp01(spec)
  denom <- (1 - spec)
  if (denom <= 0) return(Inf)
  sens / denom
}

odds_from_prob <- function(p) {
  p <- clamp01(p)
  ifelse(p %in% c(0, 1), NA_real_, p / (1 - p))
}
prob_from_odds <- function(o) {
  ifelse(is.na(o), NA_real_, o / (1 + o))
}

# ---------------------------
# UI
# ---------------------------
ui <- fluidPage(
  tags$head(
    tags$style(HTML(" 
      .callout { padding: 10px 12px; border-radius: 10px; background: #f7f7f7; border: 1px solid #e6e6e6; }
      .smallNote { color: #666; font-size: 13px; }
      .metricCard { padding: 12px 14px; border-radius: 12px; background: white; border: 1px solid #e9e9e9; }
      .metricTitle { font-size: 22px; font-weight: 900; margin-bottom: 6px; }
      .metricLine { font-size: 16px; line-height: 1.7; }
      .muted { color: #666; }
      .faded { opacity: 0.28; }
      .tightHr { margin: 10px 0; }
      .hypBadge { font-size: 18px; font-weight: 900; color: #111; opacity: 0.95; margin-bottom: 8px; }
      .whoTitle { font-size: 44px; font-weight: 900; color: #1b2a57; margin-top: 10px; }
      .bigEq { font-size: 34px; font-weight: 900; }
      .bigEqSmall { font-size: 22px; font-weight: 800; }
      .eqBlock { padding: 12px 14px; border-radius: 14px; background: white; border: 1px solid #e9e9e9; }
      .eqRow { display: flex; align-items: baseline; gap: 12px; flex-wrap: wrap; }

      .badge { display: inline-block; padding: 2px 8px; border-radius: 999px; font-weight: 800; font-size: 12px; color: white; }
      .bTP { background: #00C853; }
      .bFN { background: #CFE8D6; color: #3f6f47; }
      .bFP { background: #FF1744; }
      .bTN { background: #F2D0D0; color: #9a5a5a; }

      .cTP    { color: #00C853; font-weight: 900; }
      .cFP    { color: #FF1744; font-weight: 900; }
      .cBF    { color: #1E5AA8; font-weight: 900; }
      .cOdds  { color: #333; font-weight: 900; }
      .cCalc  { color: #2D5DA8; font-weight: 900; }
      .cCalcTab4 { color: #2D5DA8; font-weight: 400; }
      .storyTop { min-height: 7.5rem; }
      .storyMid { min-height: 14rem; }
      .storyBot { min-height: 9rem; }
      .storyLine { display:flex; justify-content:space-between; align-items:baseline; font-size:18px; line-height:1.7; }
      .storyLine .lbl { color:#111; }
      .storyLine .val { color:#111; font-weight:800; }
      .storyLine.right { justify-content:flex-end; gap:10px; }
      .storyLine.right .lbl { text-align:right; }
      .storyLine.right .val { min-width:3.8rem; text-align:right; }
      .storyLine.left { gap:10px; }
      .storyLine.left .val { min-width:3.8rem; text-align:right; }
      .storyLine.faded { opacity:0.28; }
      .storyGap { height: 1.2rem; }
      .storyGapSmall { height: 0.55rem; }
      .storyFrac { text-align:center; font-size:18px; line-height:1.6; color:#666; }
      .storyH { font-size:18px; font-weight:900; color:#111; margin-bottom:0.55rem; }
      .cLeft { color:#5E9E66; font-weight:900; }
      .cRight { color:#D46A6A; font-weight:900; }

      .metricRow { display: flex; justify-content: space-between; align-items: baseline; font-size: 18px; line-height: 1.7; }
      .metricRow .label { color: #111; }
      .metricRow .val { font-weight: 800; color: #111; }
      .metricRow.faded { opacity: 0.28; }
      .metricRow.muted .label, .metricRow.muted .val { color: #666; }
      .fracLine { text-align: center; font-size: 18px; line-height: 1.6; margin-top: 2px; }
      .fracLine.faded { opacity: 0.28; }
      .fracLine.muted { color: #666; }

      /* Tab 2 card tuning */
      .protoName {
        text-align: center;
        font-size: 24px;
        line-height: 1.1;
        font-weight: 900;
        margin: 0.15rem 0 0.95rem 0;
      }
      .protoCountLine {
        text-align: center;
        font-size: 20px;
        line-height: 1.15;
        font-weight: 500;
        margin: 0 0 1.55rem 0;
      }
      .protoCountLine .num {
        font-size: 22px;
        font-weight: 900;
      }
      .protoTopMeasure {
        display:flex;
        justify-content:space-between;
        align-items:baseline;
        gap: 12px;
        margin: 0 0 1.8rem 0;
      }
      .protoTopMeasure .lbl {
        font-size: 19px;
        line-height: 1.15;
        font-weight: 900;
        color:#333;
      }
      .protoTopMeasure .val {
        font-size: 27px;
        line-height: 1.05;
        font-weight: 900;
      }
      .protoPair {
        display:flex;
        justify-content:space-between;
        align-items:baseline;
        gap: 12px;
        font-size: 18px;
        line-height: 1.25;
        margin: 0;
      }
      .protoPair .lbl {
        color:#111;
        font-weight: 400;
      }
      .protoPair .val {
        min-width: 4rem;
        text-align: right;
        font-weight: 500;
      }
      .protoSectionLine {
        border-top: 1px solid #a9a9a9;
        margin: 0.75rem 0 0.95rem 0;
      }
      .protoPairName {
        margin-bottom: 2.05rem;
      }
      .protoRate {
        display:flex;
        justify-content:space-between;
        align-items:baseline;
        gap: 12px;
        margin: 0 0 2.0rem 0;
      }
      .protoRate .lbl {
        font-size: 19px;
        line-height: 1.15;
        font-weight: 900;
        color:#333;
      }
      .protoRate .val {
        font-size: 19px;
        line-height: 1.15;
        font-weight: 900;
        color:#333;
        text-align:right;
      }
      .protoMinor {
        text-align:center;
        color:#8a8a8a;
        margin-top: 0;
      }
      .protoMinor .lbl {
        display:block;
        font-size: 18px;
        line-height: 1.15;
        font-weight: 400;
        color:#8a8a8a;
        margin-bottom: 0.7rem;
      }
      .protoMinor .val {
        display:block;
        font-size: 18px;
        line-height: 1.15;
        font-weight: 400;
        color:#7b7b7b;
      }

      .protoStackSection {
        margin-bottom: 2.1rem;
      }
      .protoStackSection:last-child {
        margin-bottom: 0;
      }
      .protoMutedSection {
        opacity: 0.32;
      }
      .protoMutedSection .protoName,
      .protoMutedSection .protoCountLine,
      .protoMutedSection .protoTopMeasure .lbl,
      .protoMutedSection .protoTopMeasure .val,
      .protoMutedSection .protoPair .lbl,
      .protoMutedSection .protoPair .val,
      .protoMutedSection .protoRate .lbl,
      .protoMutedSection .protoRate .val {
        color: #7a7a7a !important;
      }
      .protoMinor.protoMutedSection .lbl,
      .protoMinor.protoMutedSection .val {
        color: #9a9a9a !important;
      }
      .posteriorHeadline {
        text-align: center;
        margin-top: 0.6rem;
        margin-bottom: 0.25rem;
        color:#111;
        font-size: 34px;
        line-height: 1.15;
        font-weight: 700;
      }
      .posteriorHeadline .title,
      .posteriorHeadline .val {
        display:inline;
        color:#111;
        font-size: inherit;
        line-height: inherit;
        font-weight: inherit;
      }
      .bottomHeadline {
        text-align: center;
        margin-top: 0.6rem;
        margin-bottom: 0.25rem;
        color:#111;
        font-size: 34px;
        line-height: 1.15;
        font-weight: 700;
      }
      .bfTopGrid {
        display:grid;
        grid-template-columns: 1fr auto 1fr auto 1fr;
        align-items:end;
        column-gap: 10px;
      }
      .bfCol {
        text-align:center;
        padding: 0 6px;
      }
      .bfSep {
        font-size: 40px;
        font-weight: 700;
        color:#444;
        line-height: 1;
        padding-bottom: 44px;
      }
      .bfFrac {
        display:inline-block;
        text-align:center;
        font-size: 30px;
        font-weight: 600;
        color:#333;
        line-height: 1.15;
      }
      .bfFrac .num {
        display:block;
        border-bottom: 4px solid #444;
        padding: 0 8px 3px 8px;
      }
      .bfFrac .den {
        display:block;
        padding-top: 4px;
      }
      .bfLabel { font-size:20px; 
        margin-top: 8px;
        font-size: 17px;
        font-weight: 700;
        color:#444;
      }
      .bfPctGrid {
        display:grid;
        grid-template-columns: 1fr auto 1fr auto 1fr;
        align-items:center;
        column-gap: 10px;
        margin-top: 6px;
      }
      .bfPct {
        text-align:center;
        font-size: 20px;
        color:#333;
      }
      .bfOddsGrid {
        display:grid;
        grid-template-columns: 1fr 1fr 1fr;
        align-items:start;
        column-gap: 10px;
        margin-top: 10px;
      }
      .bfOddsCell {
        text-align:center;
      }
      .bfOddsMain {
        font-size: 22px;
        font-weight: 700;
        color:#333;
      }
      .bfOddsSub {
        font-size: 14px;
        color:#666;
        margin-top: 3px;
      }
      .bfBridgeTop {
        display:grid;
        grid-template-columns: 1.4fr 0.9fr;
        column-gap: 28px;
        align-items:center;
        margin-bottom: 1.4rem;
      }
      .bfBridgeConcept {
        text-align:left;
        font-size: 24px;
        font-weight: 800;
        line-height: 1.2;
      }
      .bfBridgeConcept .prior { color:#C28A00; }
      .bfBridgeConcept .bf { color:#4F7F39; }
      .bfBridgeConcept .post { color:#3558A0; }
      .bfBridgeOddsLine {
        text-align:center;
        font-size: 22px;
        font-weight: 800;
        line-height: 1.2;
        color:#222;
      }
      .bfBridgeOddsLine .bf { color:#4F7F39; }
      .bfBridgeGrid {
        display:grid;
        grid-template-columns: 1fr 1fr 1.15fr;
        column-gap: 34px;
        align-items:start;
        margin-top: 0.8rem;
      }
      .bfBridgeHead {
        font-size: 22px;
        font-weight: 800;
        color:#333;
        margin-bottom: 1.0rem;
        text-align:center;
      }
      .bfBridgeTable {
        display:grid;
        grid-template-columns: 1fr 1fr;
        row-gap: 1.2rem;
        column-gap: 24px;
        align-items:center;
      }
      .bfBridgeTable .th {
        font-size: 21px;
        font-weight: 800;
        color:#333;
        text-align:center;
        margin-bottom: 0.4rem;
      }
      .bfBridgeTable .cell {
        font-size: 20px;
        font-weight: 800;
        text-align:center;
      }
      .bfBridgeTable .priorPct,
      .bfBridgeTable .priorOdds {
        color:#C28A00;
      }
      .bfBridgeTable .postPct,
      .bfBridgeTable .postOdds {
        color:#3558A0;
      }
      .bfBridgeCallout {
        display:flex;
        align-items:center;
        justify-content:center;
        text-align:center;
        min-height: 240px;
        font-size: 34px;
        font-weight: 700;
        line-height: 1.35;
        color:#222;
      }
      .bfBridgeCallout em {
        font-style: italic;
      }
      .bfBridgeStack {
        display:flex;
        flex-direction:column;
        gap: 1.15rem;
        align-items:center;
      }
      .bfBridgeBox {
        width: 86%;
        text-align:center;
      }
      .bfBridgeConcept2 {
        font-size: 24px;
        font-weight: 800;
        line-height: 1.2;
      }
      .bfBridgeConcept2 .prior { color:#5E9E66; }
      .bfBridgeConcept2 .bf { color:#2D5DA8; }
      .bfBridgeConcept2 .post { color:#C28A00; }
      .bfBridgeSubTable {
        display:grid;
        grid-template-columns: 1fr 1fr;
        width: 62%;
        margin: 1rem auto 0 auto;
        row-gap: 1rem;
        column-gap: 26px;
        align-items:center;
      }
      .bfBridgeSubTable .th {
        font-size: 21px;
        font-weight: 800;
        color:#333;
        text-align:center;
      }
      .bfBridgeSubTable .priorPct,
      .bfBridgeSubTable .priorOdds {
        color:#5E9E66;
        font-size: 20px;
        font-weight: 800;
        text-align:center;
      }
      .bfBridgeSubTable .postPct,
      .bfBridgeSubTable .postOdds {
        color:#C28A00;
        font-size: 20px;
        font-weight: 800;
        text-align:center;
      }
      .bfBridgeOddsHeadline {
        font-size: 24px;
        font-weight: 800;
        line-height: 1.2;
      }
      .bfBridgeOddsHeadline .prior { color:#5E9E66; }
      .bfBridgeOddsHeadline .bf { color:#2D5DA8; }
      .bfBridgeOddsHeadline .post { color:#C28A00; }
      .bfBridgeInnerBox {
        width: 100%;
        border: 1px solid #e6e6e6;
        border-radius: 14px;
        padding: 18px 18px 20px 18px;
        background: #fff;
      }
      .bfBridgeBottom {
        text-align:center;
        margin-top: 0.7rem;
        margin-bottom: 0.25rem;
        color:#1b2a57;
        font-size: 34px;
        line-height: 1.15;
        font-weight: 700;
      }
      .bfInterpretWrap {
        text-align:center;
        margin-top: 0.6rem;
        margin-bottom: 0.25rem;
      }
      .bfInterpretTitle {
        color:#1b2a57;
        font-size: 34px;
        line-height: 1.15;
        font-weight: 700;
        margin-bottom: 0.5rem;
      }
      .bfInterpretSvg {
        display:block;
        max-width: 760px;
        width: 100%;
        margin: 0 auto;
      }
      .bfInterpretSvg img {
        width: 100%;
        height: auto;
        display:block;
      }
      .posteriorFormulaLine {
        text-align: center;
        margin-top: 0.15rem;
        margin-bottom: 1.05rem;
        color:#333;
        font-size: 22px;
        line-height: 1.2;
        font-weight: 700;
      }
      .posteriorFormulaLine .tpv {
        color:#00C853;
        font-weight:900;
      }
      .posteriorFormulaLine .fpv {
        color:#FF1744;
        font-weight:900;
      }
    "))
  ),
  
  titlePanel("Who Practiced? (Prevalence → Test Accuracy → Posterior Belief)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tags$details(class="controlsDetails", open = NA,
                   tags$summary("Controls"),
                   div(class="controlsBody",
                       textInput("label_left",  "Left group label",  value = "Practice"),
                       textInput("label_right", "Right group label", value = "No Practice"),
                       hr(),
                       sliderInput("n", "Sample size (N)", min = 100, max = 5000, value = 1000, step = 50),
                       sliderInput("prev", "Prevalence: P(Left group)", min = 0.001, max = 0.50, value = 0.01, step = 0.001),
                       sliderInput("sens", "Sensitivity: P(Positive | Left)", min = 0.50, max = 0.99, value = 0.90, step = 0.01),
                       sliderInput("spec", "Specificity: P(Negative | Right)", min = 0.50, max = 0.99, value = 0.91, step = 0.01),
                       actionButton("resim", "Resimulate", class = "btn-primary"),
                       br(), br(),
                       checkboxInput("show_labels", "Show numeric callouts", value = TRUE),
                       hr(),
                       div(class="smallNote",
                           "Counts are pinned to slider values (rounded). ",
                           "Which observations land in TP/FN/FP/TN is random each resimulate."
                       )
                   )
      ),
      br(),
      div(class="callout",
          div(style="font-size:22px;font-weight:800;margin-bottom:10px;","Legend"),
          div(class="metricLine", span(class="badge bTP","TP"), " true positive"),
          div(class="metricLine", span(class="badge bFN","FN"), " false negative"),
          div(class="metricLine", span(class="badge bFP","FP"), " false positive"),
          div(class="metricLine", span(class="badge bTN","TN"), " true negative")
      )
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("1) Population (Prevalence)", uiOutput("panel_pop")),
        tabPanel("2) Apply Test (TP/FN/FP/TN)", uiOutput("panel_test")),
        tabPanel("3) Only Positives (Posterior)", uiOutput("panel_pos")),
        tabPanel("4) Same BF, Different Prevalence", uiOutput("panel_bf2")),
        tabPanel("5) Bayes Factor (Odds Update)", uiOutput("panel_bf"))
      )
    )
  )
)

# ---------------------------
# Server
# ---------------------------
server <- function(input, output, session) {
  
  seed_val <- reactiveVal(1234)
  observeEvent(input$resim, { seed_val(sample.int(1e9, 1)) })
  
  col_left_soft  <- "#CFE8D6"
  col_right_soft <- "#F2D0D0"
  
  col_tp_bright <- "#00C853"
  col_fp_bright <- "#FF1744"
  
  col_fn_soft <- "#CFE9D4"
  col_tn_soft <- "#F7D3D3"
  
  base_plot_theme <- theme_void(base_size = 14) +
    theme(
      plot.margin = margin(20, 10, 10, 10),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA)
    )
  
  output$left_hyp <- renderUI({
    div(class="hypBadge", HTML("H<sub>A</sub>"))
  })
  output$right_hyp <- renderUI({
    div(class="hypBadge", HTML("H<sub>0</sub>"))
  })
  
  sim_raw <- reactive({
    simulate_population_fixed(
      n = input$n,
      prevalence = input$prev,
      sens = input$sens,
      spec = input$spec,
      seed = seed_val()
    )
  })
  
  sim_grid <- reactive({
    assign_grid_left_top_left(sim_raw())
  })
  
  met <- reactive({ metrics_from_df(sim_raw()) })
  
  grid_bounds <- reactive({
    df <- sim_grid()
    attr(df, "bounds")
  })
  
  bf_plus <- reactive({ bf_positive(input$sens, input$spec) })
  prior_odds <- reactive({ odds_from_prob(input$prev) })
  post_odds <- reactive({ prior_odds() * bf_plus() })
  post_prob <- reactive({ prob_from_odds(post_odds()) })
  
  # -------------------------
  # TAB 1 (priors only)
  # -------------------------
  output$panel_pop <- renderUI({
    fluidRow(
      column(3,
             div(class="metricCard",
                 uiOutput("left_hyp"),
                 uiOutput("left_metrics_pop")
             )
      ),
      column(6, plotOutput("plot_pop", height = "720px")),
      column(3,
             div(class="metricCard",
                 uiOutput("right_hyp"),
                 uiOutput("right_metrics_pop")
             )
      ),
      column(12, div(class="bottomHeadline", "Who is in each group?"))
    )
  })
  
  output$left_metrics_pop <- renderUI({
    m <- met()
    if (!isTRUE(input$show_labels)) return(NULL)
    
    n_left_plain <- format(m$n_left, scientific = FALSE, trim = TRUE, big.mark = "")
    n_total_plain <- format(m$n, scientific = FALSE, trim = TRUE, big.mark = "")
    
    tagList(
      div(class="protoStackSection",
          div(class="protoName cLeft", input$label_left),
          div(class="protoCountLine",
              HTML(paste0('<span class="num cLeft">', n_left_plain, '</span>/', n_total_plain))
          ),
          div(class="protoTopMeasure",
              span(class="lbl", "Prevalence"),
              span(class="val cLeft", fmt_pct(input$prev, 0))
          )
      )
    )
  })
  
  output$right_metrics_pop <- renderUI({
    m <- met()
    if (!isTRUE(input$show_labels)) return(NULL)
    
    n_right_plain <- format(m$n_right, scientific = FALSE, trim = TRUE, big.mark = "")
    n_total_plain <- format(m$n, scientific = FALSE, trim = TRUE, big.mark = "")
    
    tagList(
      div(class="protoStackSection",
          div(class="protoName cRight", input$label_right),
          div(class="protoCountLine",
              HTML(paste0('<span class="num cRight">', n_right_plain, '</span>/', n_total_plain))
          ),
          div(class="protoTopMeasure",
              span(class="lbl", "Baseline"),
              span(class="val cRight", fmt_pct(1 - input$prev, 0))
          )
      )
    )
  })
  
  output$plot_pop <- renderPlot({
    df <- sim_grid()
    b  <- grid_bounds()
    
    xlim <- c(1, b$cols)
    ylim <- c(-round(b$rows * 0.10), b$rows + round(b$rows * 0.10))
    
    df <- df %>% mutate(group = ifelse(left_group, "Left", "Right"))
    
    ggplot(df, aes(x, y)) +
      geom_tile(aes(fill = group), width = 0.90, height = 0.90) +
      scale_fill_manual(values = c("Left" = col_left_soft, "Right" = col_right_soft)) +
      coord_fixed(xlim = xlim, ylim = ylim, clip = "off", expand = FALSE) +
      base_plot_theme +
      theme(legend.position = "none")
  })
  
  # -------------------------
  # TAB 2 (add test results)
  # -------------------------
  output$panel_test <- renderUI({
    fluidRow(
      column(3,
             div(class="metricCard",
                 uiOutput("left_hyp"),
                 uiOutput("left_metrics_test")
             )
      ),
      column(6, plotOutput("plot_test", height = "720px")),
      column(3,
             div(class="metricCard",
                 uiOutput("right_hyp"),
                 uiOutput("right_metrics_test")
             )
      ),
      column(12, div(class="bottomHeadline", "Who tests positive?"))
    )
  })
  
  output$left_metrics_test <- renderUI({
    m <- met()
    if (!isTRUE(input$show_labels)) return(NULL)
    
    n_left_plain <- format(m$n_left, scientific = FALSE, trim = TRUE, big.mark = "")
    n_total_plain <- format(m$n, scientific = FALSE, trim = TRUE, big.mark = "")
    fn_plain <- format(m$fn, scientific = FALSE, trim = TRUE, big.mark = "")
    tp_plain <- format(m$tp, scientific = FALSE, trim = TRUE, big.mark = "")
    
    tagList(
      div(class="protoStackSection",
          div(class="protoName cLeft", input$label_left),
          div(class="protoCountLine",
              HTML(paste0('<span class="num cLeft">', n_left_plain, '</span>/', n_total_plain))
          ),
          div(class="protoTopMeasure",
              span(class="lbl", "Prevalence"),
              span(class="val cLeft", fmt_pct(input$prev, 0))
          )
      ),
      div(class="protoStackSection",
          div(class="protoPair",
              span(class="lbl", "True Positive"),
              HTML(paste0('<span class="val cTP" style="font-weight:700;">', tp_plain, '</span>'))
          ),
          div(class="protoSectionLine"),
          div(class="protoPair protoPairName",
              span(class="lbl", input$label_left),
              HTML(paste0('<span class="val cLeft">', n_left_plain, '</span>'))
          ),
          div(class="protoRate",
              span(class="lbl", "Sensitivity"),
              span(class="val", fmt_pct(input$sens, 0))
          )
      ),
      div(class="protoMinor protoStackSection",
          span(class="lbl", "False Negative Rate"),
          span(class="val", paste0(fn_plain, "/", n_left_plain, " = ", fmt_pct(1 - input$sens, 0)))
      )
    )
  })
  
  output$right_metrics_test <- renderUI({
    m <- met()
    if (!isTRUE(input$show_labels)) return(NULL)
    
    n_right_plain <- format(m$n_right, scientific = FALSE, trim = TRUE, big.mark = "")
    n_total_plain <- format(m$n, scientific = FALSE, trim = TRUE, big.mark = "")
    tn_plain <- format(m$tn, scientific = FALSE, trim = TRUE, big.mark = "")
    fp_plain <- format(m$fp, scientific = FALSE, trim = TRUE, big.mark = "")
    
    tagList(
      div(class="protoStackSection",
          div(class="protoName cRight", input$label_right),
          div(class="protoCountLine",
              HTML(paste0('<span class="num cRight">', n_right_plain, '</span>/', n_total_plain))
          ),
          div(class="protoTopMeasure",
              span(class="lbl", "Baseline"),
              span(class="val cRight", fmt_pct(1 - input$prev, 0))
          )
      ),
      div(class="protoStackSection",
          div(class="protoPair",
              span(class="lbl", "True Negative"),
              HTML(paste0('<span class="val">', tn_plain, '</span>'))
          ),
          div(class="protoSectionLine"),
          div(class="protoPair protoPairName",
              span(class="lbl", input$label_right),
              HTML(paste0('<span class="val cRight">', n_right_plain, '</span>'))
          ),
          div(class="protoRate",
              span(class="lbl", "Specificity"),
              span(class="val", fmt_pct(input$spec, 0))
          )
      ),
      div(class="protoMinor protoStackSection",
          span(class="lbl", "False Positive Rate"),
          HTML(paste0('<span class="val"><span class="cFP" style="font-weight:700;">', fp_plain, '</span>/', n_right_plain, ' = ', fmt_pct(1 - input$spec, 0), '</span>'))
      )
    )
  })
  
  output$plot_test <- renderPlot({
    df <- sim_grid()
    b  <- grid_bounds()
    
    xlim <- c(1, b$cols)
    ylim <- c(-round(b$rows * 0.10), b$rows + round(b$rows * 0.10))
    
    ggplot(df, aes(x, y)) +
      geom_tile(aes(fill = outcome), width = 0.90, height = 0.90) +
      scale_fill_manual(values = c(
        "TP" = col_tp_bright,
        "FP" = col_fp_bright,
        "FN" = col_fn_soft,
        "TN" = col_tn_soft
      )) +
      coord_fixed(xlim = xlim, ylim = ylim, clip = "off", expand = FALSE) +
      base_plot_theme +
      theme(legend.position = "none")
  })
  
  # -------------------------
  # TAB 3 (posterior focus)
  # -------------------------
  output$panel_pos <- renderUI({
    fluidRow(
      column(3,
             div(class="metricCard",
                 uiOutput("left_hyp"),
                 uiOutput("left_metrics_pos")
             )
      ),
      column(6, plotOutput("plot_pos", height = "720px")),
      column(3,
             div(class="metricCard",
                 uiOutput("right_hyp"),
                 uiOutput("right_metrics_pos")
             )
      ),
      column(12,
             uiOutput("posterior_formula_ui"),
             div(class="posteriorHeadline",
                 span(class="title", HTML(paste0("Probability ", input$label_left, " Given Positive: "))),
                 span(class="val", textOutput("posterior_prob_text", inline = TRUE))
             )
      )
    )
  })
  
  output$left_metrics_pos <- renderUI({
    m <- met()
    if (!isTRUE(input$show_labels)) return(NULL)
    
    n_left_plain <- format(m$n_left, scientific = FALSE, trim = TRUE, big.mark = "")
    n_total_plain <- format(m$n, scientific = FALSE, trim = TRUE, big.mark = "")
    fn_plain <- format(m$fn, scientific = FALSE, trim = TRUE, big.mark = "")
    tp_plain <- format(m$tp, scientific = FALSE, trim = TRUE, big.mark = "")
    
    tagList(
      div(class="protoStackSection protoMutedSection",
          div(class="protoName cLeft", input$label_left),
          div(class="protoCountLine",
              HTML(paste0('<span class="num cLeft">', n_left_plain, '</span>/', n_total_plain))
          ),
          div(class="protoTopMeasure",
              span(class="lbl", "Prevalence"),
              span(class="val cLeft", fmt_pct(input$prev, 0))
          )
      ),
      div(class="protoStackSection",
          div(class="protoPair",
              span(class="lbl", "True Positive"),
              HTML(paste0('<span class="val cTP" style="font-weight:900; font-size:18px;">', tp_plain, '</span>'))
          ),
          div(class="protoSectionLine"),
          div(class="protoPair protoPairName protoMutedSection",
              span(class="lbl", input$label_left),
              HTML(paste0('<span class="val cLeft">', n_left_plain, '</span>'))
          ),
          div(class="protoRate protoMutedSection",
              span(class="lbl", "Sensitivity"),
              span(class="val", fmt_pct(input$sens, 0))
          )
      ),
      div(class="protoMinor protoStackSection protoMutedSection",
          span(class="lbl", "False Negative Rate"),
          span(class="val", paste0(fn_plain, "/", n_left_plain, " = ", fmt_pct(1 - input$sens, 0)))
      )
    )
  })
  
  output$right_metrics_pos <- renderUI({
    m <- met()
    if (!isTRUE(input$show_labels)) return(NULL)
    
    n_right_plain <- format(m$n_right, scientific = FALSE, trim = TRUE, big.mark = "")
    n_total_plain <- format(m$n, scientific = FALSE, trim = TRUE, big.mark = "")
    tn_plain <- format(m$tn, scientific = FALSE, trim = TRUE, big.mark = "")
    fp_plain <- format(m$fp, scientific = FALSE, trim = TRUE, big.mark = "")
    
    tagList(
      div(class="protoStackSection protoMutedSection",
          div(class="protoName cRight", input$label_right),
          div(class="protoCountLine",
              HTML(paste0('<span class="num cRight">', n_right_plain, '</span>/', n_total_plain))
          ),
          div(class="protoTopMeasure",
              span(class="lbl", "Baseline"),
              span(class="val cRight", fmt_pct(1 - input$prev, 0))
          )
      ),
      div(class="protoStackSection",
          div(class="protoPair protoMutedSection",
              span(class="lbl", "True Negative"),
              HTML(paste0('<span class="val">', tn_plain, '</span>'))
          ),
          div(class="protoSectionLine"),
          div(class="protoPair protoPairName protoMutedSection",
              span(class="lbl", input$label_right),
              HTML(paste0('<span class="val cRight">', n_right_plain, '</span>'))
          ),
          div(class="protoRate protoMutedSection",
              span(class="lbl", "Specificity"),
              span(class="val", fmt_pct(input$spec, 0))
          )
      ),
      div(class="protoMinor protoStackSection",
          span(class="lbl", "False Positive Rate"),
          HTML(paste0('<span class="val"><span class="cFP" style="font-weight:900; font-size:18px;">', fp_plain, '</span>/', n_right_plain, ' = ', fmt_pct(1 - input$spec, 0), '</span>'))
      )
    )
  })
  
  output$posterior_formula_ui <- renderUI({
    m <- met()
    div(class="posteriorFormulaLine",
        HTML(paste0(
          "P(", input$label_left, " | Positive) = ",
          "<span class='tpv'>", fmt_num(m$tp, 0), "</span>",
          "/(",
          "<span class='tpv'>", fmt_num(m$tp, 0), "</span>",
          " + ",
          "<span class='fpv'>", fmt_num(m$fp, 0), "</span>",
          ")"
        ))
    )
  })
  
  output$posterior_prob_text <- renderText({ fmt_pct(met()$ppv, 0) })
  
  output$plot_pos <- renderPlot({
    df <- sim_grid()
    
    if (sum(df$positive) == 0) {
      ggplot() +
        theme_void(base_size = 18) +
        annotate("text", x = 0, y = 0,
                 label = "No positives in this configuration.\n(Counts are pinned.)",
                 size = 7, fontface = "bold") +
        xlim(-1, 1) + ylim(-1, 1)
    } else {
      b <- grid_bounds()
      
      xlim <- c(1, b$cols)
      ylim <- c(-round(b$rows * 0.10), b$rows + round(b$rows * 0.10))
      
      ggplot(df, aes(x, y)) +
        geom_tile(aes(fill = outcome), width = 0.90, height = 0.90) +
        scale_fill_manual(values = c(
          "TP" = col_tp_bright,
          "FP" = col_fp_bright,
          "FN" = "#FBFBFB",
          "TN" = "#F8F8F8"
        )) +
        coord_fixed(xlim = xlim, ylim = ylim, clip = "off", expand = FALSE) +
        base_plot_theme +
        theme(legend.position = "none")
    }
  })
  
  # -------------------------
  # TAB 4 (Bayes factor odds update)
  # -------------------------
  
  output$bf_interpret_ui <- renderUI({
    if (file.exists("bayes_factor.svg")) {
      div(class = "bfInterpretSvg",
          HTML(paste(readLines("bayes_factor.svg", warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
      )
    } else {
      div(class = "smallNote", "Place bayes_factor.svg in the app directory.")
    }
  })
  
  output$panel_bf <- renderUI({
    m <- met()
    bf <- bf_plus()
    po <- prior_odds()
    psto <- post_odds()
    pstp <- post_prob()
    
    prior_left <- round(input$prev * 100)
    prior_right <- round((1 - input$prev) * 100)
    post_left <- round(prior_left * bf)
    post_right <- prior_right
    
    n_left_plain <- format(m$n_left, scientific = FALSE, trim = TRUE, big.mark = "")
    n_right_plain <- format(m$n_right, scientific = FALSE, trim = TRUE, big.mark = "")
    n_total_plain <- format(m$n, scientific = FALSE, trim = TRUE, big.mark = "")
    fn_plain <- format(m$fn, scientific = FALSE, trim = TRUE, big.mark = "")
    tn_plain <- format(m$tn, scientific = FALSE, trim = TRUE, big.mark = "")
    fp_plain <- format(m$fp, scientific = FALSE, trim = TRUE, big.mark = "")
    
    fluidRow(
      column(3,
             div(class="metricCard",
                 uiOutput("left_hyp"),
                 if (isTRUE(input$show_labels)) {
                   tagList(
                     div(class="protoStackSection",
                         div(class="protoName protoMutedSection cLeft", input$label_left),
                         div(class="protoCountLine protoMutedSection",
                             HTML(paste0('<span class="num cLeft">', n_left_plain, '</span>/', n_total_plain))
                         ),
                         div(class="protoTopMeasure",
                             span(class="lbl", "Prevalence"),
                             span(class="val cLeft", fmt_pct(input$prev, 0))
                         )
                     ),
                     div(style="height:28px;"),
                     div(class="protoStackSection",
                         div(class="protoPair protoMutedSection",
                             span(class="lbl", "True Positive"),
                             HTML(paste0('<span class="val cTP" style="font-weight:900; font-size:18px;">', format(m$tp, scientific = FALSE, trim = TRUE, big.mark = ""), '</span>'))
                         ),
                         div(class="protoSectionLine"),
                         div(class="protoPair protoPairName protoMutedSection",
                             span(class="lbl", input$label_left),
                             HTML(paste0('<span class="val cLeft">', n_left_plain, '</span>'))
                         ),
                         div(class="protoRate",
                             span(class="lbl", HTML("<span class='cCalc'>Sensitivity</span>")),
                             HTML(paste0('<span class="val cCalc" style="font-weight:900; color:#2D5DA8;">', fmt_pct(input$sens, 0), '</span>'))
                         )
                     ),
                     div(class="protoMinor protoStackSection protoMutedSection",
                         span(class="lbl", "False Negative Rate"),
                         span(class="val", paste0(fn_plain, "/", n_left_plain, " = ", fmt_pct(1 - input$sens, 0)))
                     )
                   )
                 }
             )
      ),
      column(6,
             div(class="eqBlock",
                 div(class="bfTopGrid",
                     div(class="bfCol",
                         div(class="bfFrac",
                             span(class="num", HTML("p(H<sub>A</sub>)")),
                             span(class="den", HTML("p(H<sub>0</sub>)"))
                         ),
                         div(class="bfLabel", "Prior Odds")
                     ),
                     div(class="bfSep", style="padding-bottom:34px; font-size:30px;", "×"),
                     div(class="bfCol",
                         div(class="bfFrac",
                             span(class="num", HTML("p(data | H<sub>A</sub>)")),
                             span(class="den", HTML("p(data | H<sub>0</sub>)"))
                         ),
                         div(class="bfLabel", "Bayes Factor")
                     ),
                     div(class="bfSep", style="padding-bottom:34px; font-size:30px;", "="),
                     div(class="bfCol",
                         div(class="bfFrac",
                             span(class="num", HTML("p(H<sub>A</sub> | data)")),
                             span(class="den", HTML("p(H<sub>0</sub> | data)"))
                         ),
                         div(class="bfLabel", "Posterior Odds")
                     )
                 ),
                 tags$hr(class="tightHr"),
                 div(class="bfPctGrid",
                     div(class="bfPct",
                         HTML(paste0("(<span class='cLeft'>", fmt_pct1(input$prev), "</span> / <span class='cRight'>", fmt_pct1(1-input$prev), "</span>)"))
                     ),
                     div(class="bfSep", style="font-size:26px; padding-bottom:0;", "×"),
                     div(class="bfPct",
                         HTML(paste0("(<span class='cCalc'>", fmt_pct1(input$sens), "</span> / <span class='cCalc'>", fmt_pct1(1-input$spec), "</span>)"))
                     ),
                     div(class="bfSep", style="font-size:26px; padding-bottom:0;", "="),
                     div(class="bfPct",
                         HTML(paste0("(<span class='cLeft'>", fmt_pct(pstp,0), "</span> / <span class='cRight'>", fmt_pct(1-pstp,0), "</span>)"))
                     )
                 ),
                 div(class="bfOddsGrid",
                     div(class="bfOddsCell",
                         div(class="bfOddsMain", HTML(paste0("<span class='cOdds'>", prior_left, ":", prior_right, "</span>"))),
                         div(class="bfOddsSub", HTML(paste0("decimal odds = <span class='cOdds'>", fmt_odds(po, 3), "</span>")))
                     ),
                     div(class="bfOddsCell",
                         div(class="bfOddsMain", HTML(paste0("<span class='cBF'>", format(round(bf, 0), nsmall = 0), "</span>"))),
                         div(class="bfOddsSub", HTML(paste0("decimal BF = <span class='cBF'>", fmt_odds(bf, 3), "</span>")))
                     ),
                     div(class="bfOddsCell",
                         div(class="bfOddsMain", HTML(paste0("<span class='cOdds'>", post_left, ":", post_right, "</span>"))),
                         div(class="bfOddsSub", HTML(paste0("decimal odds = <span class='cOdds'>", fmt_odds(psto, 3), "</span>")))
                     )
                 )
             )
      ),
      column(3,
             div(class="metricCard",
                 uiOutput("right_hyp"),
                 if (isTRUE(input$show_labels)) {
                   tagList(
                     div(class="protoStackSection",
                         div(class="protoName protoMutedSection cRight", input$label_right),
                         div(class="protoCountLine protoMutedSection",
                             HTML(paste0('<span class="num cRight">', n_right_plain, '</span>/', n_total_plain))
                         ),
                         div(class="protoTopMeasure",
                             span(class="lbl", "Baseline"),
                             span(class="val cRight", fmt_pct(1 - input$prev, 0))
                         )
                     ),
                     div(style="height:28px;"),
                     div(class="protoStackSection protoMutedSection",
                         div(class="protoPair protoMutedSection",
                             span(class="lbl", "True Negative"),
                             HTML(paste0('<span class="val">', tn_plain, '</span>'))
                         ),
                         div(class="protoSectionLine"),
                         div(class="protoPair protoPairName protoMutedSection",
                             span(class="lbl", input$label_right),
                             HTML(paste0('<span class="val cRight">', n_right_plain, '</span>'))
                         ),
                         div(class="protoRate protoMutedSection",
                             span(class="lbl", "Specificity"),
                             span(class="val", fmt_pct(input$spec, 0))
                         )
                     ),
                     div(class="protoMinor protoStackSection",
                         span(class="lbl", HTML("<span class='cCalc'>False Positive Rate</span>")),
                         HTML(paste0('<span class="val"><span class="cCalcTab4" style="font-size:18px;">', fp_plain, '</span>/', n_right_plain, ' = <span class="cCalc" style="font-weight:900; color:#2D5DA8;">', fmt_pct(1 - input$spec, 0), '</span></span>'))
                     )
                   )
                 }
             )
      ),
      column(12,
             div(class="bfInterpretWrap",
                 div(class="bfInterpretTitle", "Bayes Factor (bf10) - Interpretation"),
                 uiOutput("bf_interpret_ui")
             )
      )
    )
  })
  
  # -------------------------
  # TAB 5 (Same BF, different prevalence)
  # -------------------------
  output$panel_bf2 <- renderUI({
    bf <- bf_plus()
    p_mid <- input$prev
    p_vals <- c(p_mid / 10, p_mid, p_mid * 10)
    p_vals <- pmin(pmax(p_vals, 0.0001), 0.9999)
    
    post_vals <- sapply(p_vals, function(p) prob_from_odds(odds_from_prob(p) * bf))
    
    prior_denoms <- sapply(p_vals, function(p) max(1, round((1 - p) / p)))
    bf_mult <- if (is.finite(bf)) round(bf) else NA_integer_
    post_nums <- rep(bf_mult, length(prior_denoms))
    
    fluidRow(
      column(12,
             div(class="eqBlock",
                 div(class="bfBridgeStack",
                     div(class="bfBridgeBox",
                         div(class="bfBridgeInnerBox",
                             div(class="bfBridgeConcept2",
                                 HTML(paste0(
                                   "<span class='prior'>Prior Belief ", fmt_pct(p_mid, 0), "</span> * ",
                                   "<span class='bf'>Bayes Factor</span> = ",
                                   "<span class='post'>New Belief ", fmt_pct(prob_from_odds(odds_from_prob(p_mid) * bf), 0), "</span>"
                                 ))
                             ),
                             div(class="bfBridgeSubTable",
                                 div(class="th", "Prevalence"),
                                 div(class="th", HTML(paste0("P(", input$label_left, " | Positive)"))),
                                 div(class="priorPct", fmt_pct(p_vals[1], 1)),
                                 div(class="postPct", fmt_pct(post_vals[1], 0)),
                                 div(class="priorPct", fmt_pct(p_vals[2], 0)),
                                 div(class="postPct", fmt_pct(post_vals[2], 0)),
                                 div(class="priorPct", fmt_pct(p_vals[3], 0)),
                                 div(class="postPct", fmt_pct(post_vals[3], 0))
                             )
                         )
                     ),
                     div(class="bfBridgeBox",
                         div(class="bfBridgeInnerBox",
                             div(class="bfBridgeOddsHeadline",
                                 HTML(paste0(
                                   "<span class='prior'>Prior Odds</span> 1:", prior_denoms[2], " * ",
                                   "<span class='bf'>Bayes Factor</span> = ",
                                   "<span class='post'>New Odds</span> ", post_nums[2], ":", prior_denoms[2]
                                 ))
                             ),
                             div(class="bfBridgeSubTable",
                                 div(class="th", "Prior Odds"),
                                 div(class="th", HTML("New Odds")),
                                 div(class="priorOdds", paste0("1:", prior_denoms[1])),
                                 div(class="postOdds", paste0(post_nums[1], ":", prior_denoms[1])),
                                 div(class="priorOdds", paste0("1:", prior_denoms[2])),
                                 div(class="postOdds", paste0(post_nums[2], ":", prior_denoms[2])),
                                 div(class="priorOdds", paste0("1:", prior_denoms[3])),
                                 div(class="postOdds", paste0(post_nums[3], ":", prior_denoms[3]))
                             )
                         )
                     )
                 )
             )
      ),
      column(12,
             div(class="bfBridgeBottom",
                 HTML(paste0("Bayes Factor <span class='cBF'>", round(bf), "</span> measures how odds are updated based on the data"))
             )
      )
    )
  })
  
  output$bf_table <- renderTable({
    NULL
  })
  
}

shinyApp(ui, server)
