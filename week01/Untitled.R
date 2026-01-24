
# install.packages(c("ggplot2", "dplyr"))  # if needed

library(ggplot2)
library(dplyr)

set.seed(42)

# Helper: light moving-average to get gentle wiggles (base R, no extra packages)
ma <- function(x, k = 7) {
  stats::filter(x, rep(1/k, k), sides = 2) |> as.numeric()
}

# X axis
n  <- 600
x  <- seq(0, 100, length.out = n)  # "Time"

# --- Curves (all scaled to [0, 1]) ----------------------------------------

# 1) Orange: fast start, then plateaus
#    Use a Gompertz-like rise to a plateau; add tiny noise + smooth
a_or  <- 0.015         # rate
y_or0 <- exp(-exp(-a_or*(x - 25)))  # Gompertz shape
y_or  <- (y_or0 - min(y_or0)) / (max(y_or0) - min(y_or0))
y_or  <- pmin(1, pmax(0, y_or + rnorm(n, 0, 0.004)))
y_or  <- ma(y_or, 9)
y_or  <- pmin(1, pmax(0, y_or))

# 2) Blue: centered S-curve (logistic), steeper in the middle
k_bl  <- 0.22   # steepness
x0_bl <- 55     # inflection near the middle
y_bl0 <- 1 / (1 + exp(-k_bl*(x - x0_bl)))
y_bl  <- (y_bl0 - min(y_bl0)) / (max(y_bl0) - min(y_bl0))
y_bl  <- pmin(1, pmax(0, y_bl + rnorm(n, 0, 0.004)))
y_bl  <- ma(y_bl, 7)
y_bl  <- pmin(1, pmax(0, y_bl))

# 3) Purple: slow, steady climb across most of the range
#    Use a power curve + linear blend to stay gradual for longer
y_pr0 <- (x/100)^4.35 * 0.92 + (x/100) * 0.08
y_pr  <- pmin(1, pmax(0, y_pr0 + rnorm(n, 0, 0.004)))
y_pr  <- ma(y_pr, 9)
y_pr  <- pmin(1, pmax(0, y_pr))

# Bind to tidy frame
df <- bind_rows(
  data.frame(x = x, y = y_bl, curve = "Blue – S curve"),
  data.frame(x = x, y = y_or, curve = "Orange – fast then plateau"),
  data.frame(x = x, y = y_pr, curve = "Purple – steady long climb")
)

# Colors chosen to resemble your figure
cols <- c(
  "Blue – S curve"               = "#2B5C88",
  "Orange – fast then plateau"   = "#E5871B",
  "Purple – steady long climb"   = "#6B5CA5"
)

p <- ggplot(df, aes(x, y, color = curve)) +
  geom_line(linewidth = 1.0, lineend = "round") +
  scale_color_manual(values = cols, guide = "none") +
  coord_cartesian(ylim = c(0, 1), xlim = c(0, 100), expand = FALSE) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  labs(
    title = "Learning Curve",
    x = "Time",
    y = "Skill"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 26, hjust = 0),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.line = element_line(color = "#333333", linewidth = 0.7),
    axis.ticks = element_line(color = "#333333", linewidth = 0.5)
  )

# Optional: label line ends like in many learning-curve graphics
endpoints <- df %>% group_by(curve) %>% slice_tail(n = 1)
p +
  geom_text(
    data = endpoints,
    aes(label = sub(".*–\\s", "", curve)),
    hjust = -0.05, vjust = 0.5, size = 5, fontface = "plain",
    show.legend = FALSE
  ) +
  coord_cartesian(xlim = c(0, 102))  # add room for end labels

# ggsave("learning_curves.png", width = 12, height = 6, dpi = 300)
