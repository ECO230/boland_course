# ============================================================
# Accident dataset: 4 views for Distance & Temperature
#   1) Base (hist + μ/σ)
#   2) Percentiles (P10/P25/P75/P90)
#   3) IQR shading (P25–P75)
#   4) Box + whiskers + tail dots (Shiny-style manual drawing)
# ============================================================

library(tidyverse)
library(ggplot2)
library(patchwork)

# ---- Load + trim outliers (your rule) ----
COURSE_ROOT <- "/data/junior/boland_course"
acc_path <- file.path(COURSE_ROOT, "shared", "data", "accident_wi.csv")

acc <- read_csv(acc_path, show_col_types = FALSE) %>%
  filter(`Distance(mi)` <= 4)

# ---- Parameters ----
bins    <- 12                 # consistent binning across histogram views
pct_vec <- c(10, 25, 75, 90)  # percentiles to display (Pxx)

# Title/label spacing (more room for Pxx labels)
TITLE_GAP_B <- 18             # whitespace between title and panel
LABEL_VJUST <- -0.7           # label height above plot (more negative = higher)

# Boxplot controls
SHOW_INLIER_DOTS <- FALSE     # FALSE matches your screenshot (only tails/outliers)
MAX_INLIERS      <- 3500      # subsample cap if inlier dots are on

# Colors
COL_DISTANCE <- "#59A14F"
COL_TEMP     <- "#4C78A8"
COL_LINE     <- "#D62728"
COL_IQR_FILL <- "#E15759"
ALPHA_IQR    <- 0.22

# ------------------------------------------------------------
# Shared theme fragment (title spacing applied everywhere)
# ------------------------------------------------------------
theme_title_spaced <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(
        face = "bold",
        hjust = 0.5,
        margin = margin(b = TITLE_GAP_B)
      ),
      panel.grid.minor = element_blank(),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# ------------------------------------------------------------
# Helper 1: Base panel = histogram + stats underneath
# ------------------------------------------------------------
make_hist_with_stats <- function(data, var, title,
                                 bins = 12,
                                 fill = "#4C78A8",
                                 mean_digits = 3,
                                 sd_digits = 2) {
  
  x <- suppressWarnings(as.numeric(data[[var]]))
  x <- x[is.finite(x)]
  
  mu  <- mean(x)
  sig <- sd(x)
  
  p_hist <- ggplot(tibble(x = x), aes(x)) +
    geom_histogram(bins = bins, fill = fill, color = "white") +
    labs(title = title, x = var, y = "Count") +
    theme_title_spaced(base_size = 13) +
    theme(plot.margin = margin(10, 10, 0, 10))  # tighter to stats block
  
  p_stats <- ggplot() +
    annotate(
      "text", x = 0, y = 0,
      label = paste0(
        "μ = ", format(round(mu, mean_digits), nsmall = mean_digits), "\n",
        "σ = ", format(round(sig, sd_digits),   nsmall = sd_digits)
      ),
      size = 7
    ) +
    xlim(-1, 1) + ylim(-1, 1) +
    theme_void() +
    theme(plot.margin = margin(0, 10, 10, 10))
  
  p_hist / p_stats + plot_layout(heights = c(3, 1))
}

# ------------------------------------------------------------
# Helper 2: Percentile panel = histogram + vlines + labels (Pxx)
# ------------------------------------------------------------
make_hist_percentiles <- function(data, var, title,
                                  bins = 12,
                                  fill = "#4C78A8",
                                  pcts = c(10, 25, 75, 90),
                                  line_col = "#D62728") {
  
  x <- suppressWarnings(as.numeric(data[[var]]))
  x <- x[is.finite(x)]
  
  qs <- as.numeric(quantile(x, probs = pcts/100, na.rm = TRUE, type = 7))
  df_q <- tibble(pct = pcts, q = qs)
  
  ggplot(tibble(x = x), aes(x)) +
    geom_histogram(bins = bins, fill = fill, color = "white") +
    geom_vline(data = df_q, aes(xintercept = q),
               color = line_col, linewidth = 1.0, alpha = 0.9) +
    geom_text(
      data = df_q,
      aes(x = q, y = Inf, label = paste0("P", pct)),
      vjust = LABEL_VJUST, size = 4, color = "black"
    ) +
    labs(title = title, x = var, y = "Count") +
    theme_title_spaced(base_size = 13) +
    coord_cartesian(clip = "off")
}

# ------------------------------------------------------------
# Helper 3: IQR panel = shaded P25–P75 + vlines + labels
# ------------------------------------------------------------
make_hist_iqr <- function(data, var, title,
                          bins = 12,
                          fill = "#4C78A8",
                          shade_fill = "#E15759",
                          shade_alpha = 0.22,
                          line_col = "#D62728") {
  
  x <- suppressWarnings(as.numeric(data[[var]]))
  x <- x[is.finite(x)]
  
  q <- as.numeric(quantile(x, probs = c(.25, .75), na.rm = TRUE, type = 7))
  q1 <- q[1]; q3 <- q[2]
  
  ggplot(tibble(x = x), aes(x)) +
    annotate("rect",
             xmin = q1, xmax = q3,
             ymin = -Inf, ymax = Inf,
             fill = shade_fill, alpha = shade_alpha) +
    geom_histogram(bins = bins, fill = fill, color = "white") +
    geom_vline(xintercept = c(q1, q3),
               color = line_col, linewidth = 1.1, alpha = 0.95) +
    geom_text(
      data = tibble(q = c(q1, q3), lab = c("P25", "P75")),
      aes(x = q, y = Inf, label = lab),
      vjust = LABEL_VJUST, size = 4, color = "black"
    ) +
    labs(title = title, x = var, y = "Count") +
    theme_title_spaced(base_size = 13) +
    coord_cartesian(clip = "off")
}

# ------------------------------------------------------------
# Helper 4: Box + whiskers + tail dots (Shiny-style manual drawing)
# ------------------------------------------------------------
make_boxplot_shiny_style <- function(data, var, title,
                                     color = "#4C78A8",
                                     show_inlier_dots = FALSE,
                                     max_inliers = 3500,
                                     inlier_alpha = 0.25,
                                     inlier_size  = 1.05,
                                     outlier_alpha = 0.90,
                                     outlier_size  = 1.35,
                                     box_w = 0.10,
                                     cap_w_mult = 0.80) {
  
  x <- suppressWarnings(as.numeric(data[[var]]))
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    return(ggplot() + theme_void() + ggtitle(paste0(title, " (no data)")))
  }
  if (length(x) < 2) {
    return(
      ggplot(data.frame(x = x, y = 1), aes(x, y)) +
        geom_point(color = color, size = 2) +
        scale_y_continuous(NULL, breaks = NULL) +
        labs(title = title, x = var, y = NULL) +
        theme_title_spaced(base_size = 13)
    )
  }
  
  # Tukey boxplot stats (1.5*IQR whiskers) and outliers
  bp <- boxplot.stats(x)
  s5 <- bp$stats
  lowW <- s5[1]; q1 <- s5[2]; med <- s5[3]; q3 <- s5[4]; hiW <- s5[5]
  
  is_out <- (x < lowW) | (x > hiW)
  x_out <- x[is_out]
  x_in  <- x[!is_out]
  
  # Optional subsample inliers
  set.seed(230)
  if (length(x_in) > max_inliers) x_in <- sample(x_in, max_inliers)
  
  y0 <- 1
  df_in  <- data.frame(x = x_in,  y = rep(y0, length(x_in)))
  df_out <- data.frame(x = x_out, y = rep(y0, length(x_out)))
  
  cap_w <- box_w * cap_w_mult
  n_in <- nrow(df_in)
  jitter_h <- min(0.22, 0.05 + 0.04 * log10(n_in + 1))
  
  p <- ggplot() +
    # box
    geom_rect(aes(xmin = q1, xmax = q3, ymin = y0 - box_w/2, ymax = y0 + box_w/2),
              fill = "grey88", color = "grey35") +
    # median
    geom_segment(aes(x = med, xend = med, y = y0 - box_w/2, yend = y0 + box_w/2),
                 color = "grey35", linewidth = 1.1) +
    # whiskers
    geom_segment(aes(x = lowW, xend = q1, y = y0, yend = y0), color = "grey35") +
    geom_segment(aes(x = q3, xend = hiW, y = y0, yend = y0), color = "grey35") +
    # caps
    geom_segment(aes(x = lowW, xend = lowW, y = y0 - cap_w/2, yend = y0 + cap_w/2), color = "grey35") +
    geom_segment(aes(x = hiW,  xend = hiW,  y = y0 - cap_w/2, yend = y0 + cap_w/2), color = "grey35")
  
  # optional inlier dots
  if (isTRUE(show_inlier_dots) && nrow(df_in) > 0) {
    p <- p +
      geom_jitter(data = df_in, aes(x = x, y = y),
                  height = jitter_h, width = 0,
                  alpha = inlier_alpha, size = inlier_size, color = color)
  }
  
  # outliers always visible
  if (nrow(df_out) > 0) {
    p <- p +
      geom_point(data = df_out, aes(x = x, y = y),
                 alpha = outlier_alpha, size = outlier_size, color = color)
  }
  
  p +
    scale_y_continuous(NULL, breaks = NULL, limits = c(y0 - 0.28, y0 + 0.28)) +
    labs(title = title, x = var, y = NULL) +
    theme_title_spaced(base_size = 13) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank())
}

# ============================================================
# Build the 4 views (each is two-panel: Distance | Temperature)
# ============================================================

# --- 1) Base (μ/σ under each) ---
p_distance_base <- make_hist_with_stats(
  acc, "Distance(mi)", "Distance (mi)",
  bins = bins, fill = COL_DISTANCE,
  mean_digits = 3, sd_digits = 2
)

p_temp_base <- make_hist_with_stats(
  acc, "Temperature(F)", "Temperature (F)",
  bins = bins, fill = COL_TEMP,
  mean_digits = 2, sd_digits = 2
)

plot_base <- p_distance_base | p_temp_base

# --- 2) Percentiles ---
p_distance_pct <- make_hist_percentiles(
  acc, "Distance(mi)", "Distance Percentiles",
  bins = bins, fill = COL_DISTANCE,
  pcts = pct_vec, line_col = COL_LINE
)

p_temp_pct <- make_hist_percentiles(
  acc, "Temperature(F)", "Temperature Percentiles",
  bins = bins, fill = COL_TEMP,
  pcts = pct_vec, line_col = COL_LINE
)

plot_percentiles <- p_distance_pct | p_temp_pct

# --- 3) IQR shading ---
p_distance_iqr <- make_hist_iqr(
  acc, "Distance(mi)", "Distance IQR",
  bins = bins, fill = COL_DISTANCE,
  shade_fill = COL_IQR_FILL, shade_alpha = ALPHA_IQR,
  line_col = COL_LINE
)

p_temp_iqr <- make_hist_iqr(
  acc, "Temperature(F)", "Temperature IQR",
  bins = bins, fill = COL_TEMP,
  shade_fill = COL_IQR_FILL, shade_alpha = ALPHA_IQR,
  line_col = COL_LINE
)

plot_iqr <- p_distance_iqr | p_temp_iqr

# --- 4) Box + tails (Shiny-style) ---
p_distance_box <- make_boxplot_shiny_style(
  acc, "Distance(mi)", "Distance — Boxplot + tails",
  color = COL_DISTANCE,
  show_inlier_dots = SHOW_INLIER_DOTS,
  max_inliers = MAX_INLIERS
)

p_temp_box <- make_boxplot_shiny_style(
  acc, "Temperature(F)", "Temperature — Boxplot + tails",
  color = COL_TEMP,
  show_inlier_dots = SHOW_INLIER_DOTS,
  max_inliers = MAX_INLIERS
)

plot_box <- p_distance_box | p_temp_box

# ============================================================
# Display whichever view you want:
# ============================================================
plot_base
plot_percentiles
plot_iqr
plot_box

# Optional saves:
# ggsave("acc_base.png", plot_base, width = 12, height = 6, dpi = 200)
# ggsave("acc_percentiles.png", plot_percentiles, width = 12, height = 5.5, dpi = 200)
# ggsave("acc_iqr.png", plot_iqr, width = 12, height = 5.5, dpi = 200)
# ggsave("acc_box.png", plot_box, width = 12, height = 4.2, dpi = 200)

# ============================================================
# Quick knobs:
# - More room for percentile labels: increase TITLE_GAP_B or
#   set LABEL_VJUST more negative (e.g., -0.9)
# - Show inlier dots on boxplots: set SHOW_INLIER_DOTS <- TRUE
# - Thinner box: set box_w in make_boxplot_shiny_style to 0.08
# ============================================================