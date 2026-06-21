library(rms)

input_path <- "/Users/hunter/Downloads/cleaned.csv"

d <- read.csv(input_path, na.strings = c("NA", ""))

# Outcome: 6=Discharged to home or self-care (routine discharge)
d$home_discharge <- ifelse(d$hospdischargedisposition == 6, 1, 0)

# remove unilateral pupil response because it's still in the cleaned csv
d <- d[is.na(d$tbipupillaryresponse) | d$tbipupillaryresponse %in% c(1, 3), ]
d$tbipupillaryresponse <- factor(
  d$tbipupillaryresponse,
  levels = c(1, 3),
  labels = c("Bilateral present", "Bilateral absent")
)
d$ich_category <- factor(d$ich_category)
d$ich_category <- factor(
  d$ich_category,
  levels = c(
    ">=2 concomitant ICHs",
    "isolated EDH",
    "isolated IPH",
    "isolated SAH",
    "isolated SDH",
    "other/unspecified ICH"
  ),
  labels = c(
    "Multi",
    "EDH",
    "IPH",
    "SAH",
    "SDH",
    "Other"
  )
)
d$gcs_group <- cut(
  d$totalgcs,
  breaks = c(2, 5, 8, 12, 15),
  labels = c("GCS 3-5", "GCS 6-8", "GCS 9-12", "GCS 13-15"),
  include.lowest = TRUE,
  right = TRUE
)
d$gcs_group <- factor(
  d$gcs_group,
  levels = c("GCS 3-5", "GCS 6-8", "GCS 9-12", "GCS 13-15")
)

label(d$iss) <- "ISS"
label(d$gcs_group) <- "Total GCS"
label(d$tbipupillaryresponse) <- "Pupil response"
label(d$ich_category) <- "ICH category"

model_vars <- c(
  "home_discharge",
  "year",
  "iss",
  "gcs_group",
  "tbipupillaryresponse",
  "ich_category"
)

analytic <- d[complete.cases(d[, model_vars]), model_vars]
train <- analytic[analytic$year <= 2022, ]
valid <- analytic[analytic$year == 2023, ]

#if you wanted to do 80-20 random:
#set.seed(2026)
#

#train_index <- sample(
#  seq_len(nrow(analytic)),
#  size = floor(0.8 * nrow(analytic))
#)

#train <- analytic[train_index, ]
#valid <- analytic[-train_index, ]

dd <- datadist(train)
options(datadist = "dd")

fit <- lrm(
  home_discharge ~ rcs(iss, 4) + tbipupillaryresponse +
    gcs_group + ich_category,
  data = train,
  x = TRUE,
  y = TRUE
)

auc_rank <- function(y, p) {
  ok <- !is.na(y) & !is.na(p)
  y <- y[ok]
  p <- p[ok]
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  ranks <- rank(p, ties.method = "average")
  (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

roc_points <- function(y, p) {
  ok <- !is.na(y) & !is.na(p)
  y <- y[ok]
  p <- p[ok]
  thresholds <- c(Inf, sort(unique(p), decreasing = TRUE), -Inf)

  out <- data.frame(
    threshold = thresholds,
    sensitivity = NA_real_,
    specificity = NA_real_
  )

  for (i in seq_along(thresholds)) {
    pred <- ifelse(p >= thresholds[i], 1, 0)

    tp <- sum(pred == 1 & y == 1)
    fp <- sum(pred == 1 & y == 0)
    tn <- sum(pred == 0 & y == 0)
    fn <- sum(pred == 0 & y == 1)

    out$sensitivity[i] <- tp / (tp + fn)
    out$specificity[i] <- tn / (tn + fp)
  }

  out
}

pred_train <- predict(fit, newdata = train, type = "fitted")
pred_valid <- predict(fit, newdata = valid, type = "fitted")

train_auc <- auc_rank(train$home_discharge, pred_train)
valid_auc <- auc_rank(valid$home_discharge, pred_valid)
train_brier <- mean((train$home_discharge - pred_train)^2)
valid_brier <- mean((valid$home_discharge - pred_valid)^2)

cat("\nNomogram\n")
cat("=================================\n")
cat("Outcome: routine discharge home/self-care, hospdischargedisposition == 6\n")
cat("Model: home_discharge ~ rcs(iss, 4) + tbipupillaryresponse + gcs_group + ich_category\n")
cat("Training set: 2018-2022\n") #if switched change to cat("Training set: random 80% of 2018-2023 cohort\n")
cat("Validation set: 2023\n\n") #if switched change to cat("Validation set: random held-out 20% of 2018-2023 cohort\n\n")
cat("Analytic rows:", nrow(analytic), "\n")
cat("Training rows:", nrow(train), "\n")
cat("Validation rows:", nrow(valid), "\n")
cat("Training home-discharge rate:", round(mean(train$home_discharge), 4), "\n")
cat("Validation home-discharge rate:", round(mean(valid$home_discharge), 4), "\n")
cat("Training AUC:", round(train_auc, 4), "\n")
cat("Validation AUC:", round(valid_auc, 4), "\n")
cat("Training Brier score:", round(train_brier, 4), "\n")
cat("Validation Brier score:", round(valid_brier, 4), "\n\n")

print(anova(fit))

nom <- nomogram(
  fit,
  fun = plogis,
  fun.at = c(0.05, 0.1, 0.2, 0.3, 0.5, 0.7, 0.9),
  funlabel = "Probability of routine discharge home",
  lp = FALSE
)

draw_nomogram <- function() {
  old_par <- par(
    lwd = 1.6,
    col.axis = "black",
    col.lab = "black",
    fg = "black",
    mar = c(4.6, 4.1, 4.1, 2.1)
  )
  on.exit(par(old_par), add = TRUE)
  plot(
    nom,
    xfrac = 0.16,
    cex.axis = 0.68,
    cex.var = 0.9,
    force.label = TRUE,
    tck = -0.025,
    tcl = -0.35,
    lmgp = 0.35,
    points.label = "Points",
    total.points.label = "Total Points"
  )
  title("Nomogram", line = 1, cex.main = 1.15)
}

pdf("nomogram4_ich_gcs_bins.pdf", width = 14.5, height = 8.5, pointsize = 12)
draw_nomogram()
dev.off()

quartz(width = 14.5, height = 8.5)
draw_nomogram()

roc_valid <- roc_points(valid$home_discharge, pred_valid)

pdf("nomogram4_validation_roc.pdf", width = 7, height = 7)
plot(
  1 - roc_valid$specificity,
  roc_valid$sensitivity,
  type = "l",
  lwd = 3,
  col = "#0B6E69",
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "1 - Specificity",
  ylab = "Sensitivity",
  main = paste0("2023 validation ROC, AUC = ", round(valid_auc, 3))
)
abline(0, 1, lty = 2, col = "gray50", lwd = 2)
dev.off()

quartz(width = 7, height = 7)
plot(
  1 - roc_valid$specificity,
  roc_valid$sensitivity,
  type = "l",
  lwd = 3,
  col = "#0B6E69",
  xlim = c(0, 1),
  ylim = c(0, 1),
  xlab = "1 - Specificity",
  ylab = "Sensitivity",
  main = paste0("2023 validation ROC, AUC = ", round(valid_auc, 3))
)
abline(0, 1, lty = 2, col = "gray50", lwd = 2)

