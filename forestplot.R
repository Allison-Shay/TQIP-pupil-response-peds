#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(cowplot)
  library(scales)
})

input_file  <- "analysis/final_analysis.csv"
out_risk <- "analysis/forest_plot_risk.pdf"
out_logistic <- "analysis/forest_plot_logistic.pdf"
out_linear   <- "analysis/forest_plot_linear.pdf"

# Remove 0's before decimal point for p-values
fmt_p_no0 <- function(p) {
  if (is.na(p)) return(NA_character_)

  p <- as.character(p)

  # handle strings like "<0.001"
  if (grepl("^<\\s*0\\.", p)) {
    return(sub("^<\\s*0\\.", "<.", p))
  }

  # handle numeric p-values
  p_num <- suppressWarnings(as.numeric(p))
  if (!is.na(p_num)) {
    return(sub("^0\\.", ".", sprintf("%.3f", p_num)))
  }

  p
}


parse_ci <- function(ci_vec) {
  parts <- str_split(as.character(ci_vec), "\\s+to\\s+", simplify = TRUE)
  tibble(
    lo = suppressWarnings(as.numeric(parts[, 1])),
    hi = suppressWarnings(as.numeric(parts[, 2]))
  )
}

prep_df <- function(df) {
  ci <- parse_ci(df$ci_95)
  df %>%
    mutate(
      est = suppressWarnings(as.numeric(estimate)),
      lo  = ci$lo,
      hi  = ci$hi,
      p_txt = stringr::str_replace_all(as.character(p_value), "0\\.", "."),
      est_ci_txt = sprintf("%.2f (%.2f–%.2f)", est, lo, hi)
    ) %>%
    filter(is.finite(est), is.finite(lo), is.finite(hi))
}

make_combo_plot <- function(df_sub, out_pdf, kind = c("logistic", "linear", "risk")) {
  kind <- match.arg(kind)
  y_expand <- expansion(mult = c(0.14, 0.22))

  df_sub <- df_sub %>%
    mutate(outcome = as.character(outcome)) %>%
    mutate(outcome_f = factor(outcome, levels = rev(outcome)))

  if (nrow(df_sub) == 0) stop("No rows to plot.", call. = FALSE)

  if (kind == "logistic") {
    df_sub <- df_sub %>% filter(est > 0, lo > 0, hi > 0)
    if (nrow(df_sub) == 0) stop("No valid logistic rows (need positive OR/CI).", call. = FALSE)

    x_null <- 1
    xlab <- "Odds ratio (95% CI)"
    decreased_label <- "Decreased odds"
    increased_label <- "Increased odds"

    # symmetric limits in log space around log(1)=0
    log_lo <- log(df_sub$lo)
    log_hi <- log(df_sub$hi)
    max_abs_log <- max(abs(c(log_lo, log_hi)), na.rm = TRUE)
    x_limits <- exp(c(-max_abs_log, max_abs_log))

    # ticks (keep within limits)
    tick_candidates <- c(0.25, 0.33, 0.5, 0.6, 0.8, 1, 1.2, 1.4, 2, 3, 4)
    x_breaks <- tick_candidates[tick_candidates >= x_limits[1] & tick_candidates <= x_limits[2]]
    if (length(x_breaks) < 3) x_breaks <- exp(pretty(log(x_limits), n = 7))

    x_scale <- scale_x_log10(
      limits = x_limits,
      breaks = x_breaks,
      labels = format(x_breaks, trim = TRUE)
    )

    # header x positions (in data coordinates)
    hdr_left_x  <- exp(log(x_limits[1]) + 0.25 * (log(x_limits[2]) - log(x_limits[1])))
    hdr_right_x <- exp(log(x_limits[1]) + 0.75 * (log(x_limits[2]) - log(x_limits[1])))

  } else if (kind == "risk") {
    df_sub <- df_sub %>% filter(est > 0, lo > 0, hi > 0)
    if (nrow(df_sub) == 0) stop("No valid risk rows (need positive OR/CI).", call. = FALSE)

    x_null <- 1
    xlab <- "Risk ratio (95% CI)"
    decreased_label <- "Decreased risk"
    increased_label <- "Increased risk"

    # symmetric limits in log space around log(1)=0
    log_lo <- log(df_sub$lo)
    log_hi <- log(df_sub$hi)
    max_abs_log <- max(abs(c(log_lo, log_hi)), na.rm = TRUE)
    x_limits <- exp(c(-max_abs_log, max_abs_log))

    # ticks (keep within limits)
    tick_candidates <- c(0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3)
    x_breaks <- tick_candidates[tick_candidates >= x_limits[1] & tick_candidates <= x_limits[2]]
    if (length(x_breaks) < 3) x_breaks <- exp(pretty(log(x_limits), n = 7))

    x_scale <- scale_x_log10(
      limits = x_limits,
      breaks = x_breaks,
      labels = format(x_breaks, trim = TRUE)
    )

    # header x positions (in data coordinates)
    hdr_left_x  <- exp(log(x_limits[1]) + 0.25 * (log(x_limits[2]) - log(x_limits[1])))
    hdr_right_x <- exp(log(x_limits[1]) + 0.75 * (log(x_limits[2]) - log(x_limits[1])))

  } else {
    x_null <- 0
    xlab <- "Difference in days (95% CI)"
    decreased_label <- "Decreased days"
    increased_label <- "Increased days"

    # symmetric limits around 0 in linear space
    max_abs <- max(abs(c(df_sub$lo, df_sub$hi)), na.rm = TRUE)
    if (!is.finite(max_abs) || max_abs == 0) max_abs <- 1
    x_limits <- c(-max_abs, max_abs)

    x_breaks <- pretty(x_limits, n = 7)

    x_scale <- scale_x_continuous(
      limits = x_limits,
      breaks = x_breaks
    )

    hdr_left_x  <- x_limits[1] + 0.25 * (x_limits[2] - x_limits[1])
    hdr_right_x <- x_limits[1] + 0.75 * (x_limits[2] - x_limits[1])
  }

  n_rows <- nlevels(df_sub$outcome_f)
  y_expand <- expansion(mult = c(0.14, 0.22))

  p_left <- ggplot(df_sub, aes(y = outcome_f)) +
    geom_text(aes(x = 0.92, label = outcome), hjust = 0, size = 3.6) +
    geom_text(aes(x = 1.45, label = est_ci_txt), hjust = 0, size = 3.6) +
    scale_x_continuous(limits = c(0, 1.75), breaks = NULL) +
    scale_y_discrete(expand = y_expand) +
    coord_cartesian(clip = "off") +
    theme_void(base_size = 11) +
    theme(plot.margin = margin(14, 0, 6, 8)) +
    annotate("text", x = 0.92, y = n_rows + 0.9, label = "Outcome",
             fontface = "bold", hjust = 0, size = 3.9) +
    annotate("text", x = 1.45, y = n_rows + 0.9, label = "Estimate (95% CI)",
             fontface = "bold", hjust = 0, size = 3.9)

  p_mid <- ggplot(df_sub, aes(x = est, y = outcome_f)) +
    geom_vline(xintercept = x_null, linetype = "dashed", linewidth = 0.8) +
    geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.18, linewidth = 0.7) +
    geom_point(shape = 15, size = 2.7) +
    x_scale +
    scale_y_discrete(expand = y_expand) +
    coord_cartesian(clip = "off") +
    labs(x = xlab, y = NULL) +
    theme_classic(base_size = 11) +
    theme(
      axis.title.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y  = element_blank(),
      plot.margin  = margin(14, 0, 6, 0)
    ) +
    annotate("text", x = hdr_left_x,  y = n_rows + 0.9, label = decreased_label,
             fontface = "bold", size = 3.8) +
    annotate("text", x = hdr_right_x, y = n_rows + 0.9, label = increased_label,
             fontface = "bold", size = 3.8)

  p_right <- ggplot(df_sub, aes(y = outcome_f)) +
    geom_text(aes(x = 0, label = p_txt), hjust = 1, size = 3.6) +
    scale_x_continuous(limits = c(-0.05, 0), breaks = NULL) +
    scale_y_discrete(expand = y_expand) +
    coord_cartesian(clip = "off") +
    theme_void(base_size = 11) +
    theme(plot.margin = margin(14, 40, 6, 0)) +
    annotate("text", x = 0, y = n_rows + 0.9, label = "P value",
             fontface = "bold", hjust = 1, size = 3.9)

  combined <- cowplot::plot_grid(
    p_left, p_mid, p_right,
    nrow = 1,
    rel_widths = c(1.55, 1.05, 0.28),
    align = "h",
    axis = "tb"
  )

  # your preferred compact height
  attr(combined, "plot_height") <- 1.0 + 0.24 * n_rows
  combined
}

df <- read_csv(input_file, show_col_types = FALSE) %>% prep_df()

df_risk  <- df %>% filter(str_detect(model, "Risk"))
df_logit  <- df %>% filter(str_detect(model, "Logistic"))
df_linear <- df %>% filter(str_detect(model, "Linear"))

plot_risk <- make_combo_plot(df_risk,  kind = "risk")
plot_logistic <- make_combo_plot(df_logit,  kind = "logistic")
plot_linear   <- make_combo_plot(df_linear, kind = "linear")

h_risk <- attr(plot_risk, "plot_height")
h_logit  <- attr(plot_logistic, "plot_height")
h_linear <- attr(plot_linear,   "plot_height")

# Make outputs now:

combined_vertical <- cowplot::plot_grid(
  plot_logistic,
  plot_linear,
  ncol = 1,
  rel_heights = c(h_logit, h_linear),
  align = "v"
)

ggsave(
  "analysis/forest_plot_combined.pdf",
  combined_vertical,
  width = 12.5,
  height = h_logit + h_linear,
  units = "in"
)

cat("Wrote:\n")
cat("  analysis/forest_plot_combined.pdf\n")



combined_vertical <- cowplot::plot_grid(
  plot_risk,
  plot_linear,
  ncol = 1,
  rel_heights = c(h_risk, h_linear),
  align = "v"
)

ggsave(
  "analysis/forest_plot_combined_risk.pdf",
  combined_vertical,
  width = 12.5,
  height = h_risk + h_linear,
  units = "in"
)

cat("Wrote:\n")
cat("  analysis/forest_plot_combined_risk.pdf\n")

