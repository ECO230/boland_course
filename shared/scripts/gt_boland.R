
# shared/R/gt_boland.R
# Global GT theme for Boland minimalist Reveal decks

gt_boland <- function(tbl, base_size = 11, tight = TRUE) {
  tbl |>
    gt::opt_table_font(
      font = list(
        gt::google_font("Inter"),
        "Segoe UI",
        "Open Sans",
        "Arial"
      )
    ) |>
    gt::tab_options(
      table.width = gt::pct(100),
      table.font.size = gt::px(base_size),
      data_row.padding = gt::px(if (tight) 2 else 4),
      column_labels.padding = gt::px(if (tight) 2 else 4),
      table.border.top.width = gt::px(0),
      table.border.bottom.width = gt::px(0),
      column_labels.border.top.width = gt::px(0),
      column_labels.border.bottom.width = gt::px(0),
      table_body.hlines.width = gt::px(0)
    ) |>
    gt::opt_vertical_padding(scale = if (tight) 0.7 else 1) |>
    gt::opt_horizontal_padding(scale = if (tight) 0.9 else 1)
}
