# Shared sampling/comparison rules. No data source or UI dependencies.
comparison_levels <- function(x) {
  x <- as.character(x)
  x[is.na(x) | !nzchar(trimws(x))] <- "(No response)"
  x
}

sample_by_group <- function(df, n, group_var, random = TRUE) {
  stopifnot(group_var %in% names(df))
  n <- max(0L, min(as.integer(n), nrow(df)))
  groups <- split(seq_len(nrow(df)), comparison_levels(df[[group_var]]))
  groups <- groups[lengths(groups) > 0]
  if (!length(groups) || n == 0L) return(df[integer(0), , drop = FALSE])
  exact <- lengths(groups) / nrow(df) * n
  take <- floor(exact)
  remaining <- n - sum(take)
  if (remaining > 0L) {
    priority <- order(exact - take, decreasing = TRUE)
    take[priority[seq_len(remaining)]] <- take[priority[seq_len(remaining)]] + 1L
  }
  ids <- unlist(lapply(seq_along(groups), function(i) {
    group <- groups[[i]]
    if (take[i] == 0L) return(integer(0))
    # sample.int avoids R's sample(single_integer) shortcut.
    if (random) group[sample.int(length(group), take[i])] else head(group, take[i])
  }), use.names = FALSE)
  df[ids, , drop = FALSE]
}

composition_comparison <- function(pool, random_sample, nonrandom_sample, var) {
  levels <- sort(unique(comparison_levels(pool[[var]])))
  counts <- function(df) as.integer(table(factor(comparison_levels(df[[var]]), levels = levels)))
  pool_n <- counts(pool)
  random_n <- counts(random_sample)
  nonrandom_n <- counts(nonrandom_sample)
  data.frame(
    level = levels, pool_n = pool_n, random_n = random_n, nonrandom_n = nonrandom_n,
    pool_pct = pool_n / nrow(pool),
    random_pct = random_n / nrow(random_sample),
    nonrandom_pct = nonrandom_n / nrow(nonrandom_sample),
    stringsAsFactors = FALSE
  )
}
