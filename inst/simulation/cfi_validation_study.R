# =============================================================================
# CFI Validation Study — v4 (Adjustment-Based)
# causalfrag: A Cross-Framework Causal Fragility Index
# Author: Subir Hait, Michigan State University
#
# Key innovation over v1-v3:
#   Ground-truth fragility is assigned AFTER simulation using a fully adjusted model
#   that includes the true hidden confounder U. This eliminates the conceptual
#   mismatch between simulation labels and observed inference fragility.
#
# Adjustment-based ground truth:
#   For each dataset, fit both naive (without U) and fully adjusted (with U) models.
#   Classify fragility based on what happens to the estimate when U is revealed:
#
#   robust:  naive significant, fully adjusted model significant, bias_reduction < 25%
#   mixed:   naive significant, fully adjusted model significant, bias_reduction 25-60%
#   fragile: naive significant, fully adjusted model non-significant OR bias_reduction > 60%
#
#   Then test: does CFI predict adjustment-based fragility?
#
# Validation metrics:
#   (1) Mean CFI by reference fragility classification label
#   (2) AUC: does CFI predict fragile (fully adjusted) vs non-fragile?
#   (3) Correlation: CFI ~ bias_reduction
#   (4) Sensitivity/specificity at CFI threshold 50
#
# Safe paper framing:
#   "CFI is evaluated against adjustment-based fragility, defined as the degree
#    to which the naive estimate is attenuated when the true unmeasured
#    confounder is included in the model."
# =============================================================================

library(causalfrag)
suppressPackageStartupMessages({
  library(sensemakr)
  library(EValue)
  library(konfound)
})

set.seed(42)
N_SIM  <- 1000   # total datasets to generate
N_OBS  <- 300    # per dataset

# ---- simulation engine -------------------------------------------------------

simulate_adjusted_dataset <- function(n, beta_t, gamma_u, delta_u) {

  X  <- rnorm(n)
  U  <- rnorm(n)                              # true hidden confounder
  T_prob <- plogis(0.3 * X + delta_u * U)
  Tr <- rbinom(n, 1, T_prob)
  Y  <- beta_t * Tr + 0.3 * X + gamma_u * U + rnorm(n)

  # naive model (no U — as researcher would fit)
  fit_naive  <- lm(Y ~ Tr + X)

  # fully adjusted model (includes true U — never available in practice)
  fit_adjusted <- lm(Y ~ Tr + X + U)

  coef_naive  <- coef(fit_naive)["Tr"]
  coef_adjusted <- coef(fit_adjusted)["Tr"]
  p_naive     <- summary(fit_naive)$coefficients["Tr", "Pr(>|t|)"]
  p_adjusted    <- summary(fit_adjusted)$coefficients["Tr", "Pr(>|t|)"]

  # bias reduction: how much does estimate shrink when U is added?
  # positive = estimate attenuated; negative = estimate amplified
  bias_reduction <- if (abs(coef_naive) > 1e-6)
    1 - abs(coef_adjusted / coef_naive) else NA_real_

  list(
    fit_naive      = fit_naive,
    data           = data.frame(Y = Y, Tr = Tr, X = X),
    coef_naive     = coef_naive,
    coef_adjusted    = coef_adjusted,
    p_naive        = p_naive,
    p_adjusted       = p_adjusted,
    bias_reduction = bias_reduction
  )
}

assign_reference_truth <- function(p_naive, p_adjusted, bias_reduction) {
  if (is.na(bias_reduction)) return(NA_character_)
  if (p_naive >= 0.05) return("not_significant")

  # fragile: inference collapses (non-significantly adjusted) OR estimate nearly gone
  if (p_adjusted >= 0.05 || bias_reduction > 0.70)
    return("fragile")

  # mixed: estimate substantially attenuated but survives
  if (bias_reduction >= 0.35)
    return("mixed")

  # robust: estimate largely unchanged after adding U
  return("robust")
}

run_cfi_on_naive <- function(fit_naive, df) {
  tryCatch({
    res <- run_sensitivity(fit_naive, treatment = "Tr",
                           data = df, verbose = FALSE)
    res <- flag_fragility(res)
    comp <- res$cfi$components
    list(
      cfi       = res$cfi$score,
      label     = res$cfi$label,
      rv_q      = res$results$sensemakr$rv_q,
      rv_qa     = res$results$sensemakr$rv_qa,
      evalue    = res$results$evalue$evalue,
      pct_bias  = res$results$itcv$pct_bias,
      score_rv  = as.numeric(comp["rv"]       %||% NA),
      score_rva = as.numeric(comp["rv_alpha"] %||% NA),
      score_ev  = as.numeric(comp["evalue"]   %||% NA),
      score_pct = as.numeric(comp["pct_bias"] %||% NA)
    )
  }, error = function(e) NULL)
}

# ---- varied DGP parameters ---------------------------------------------------
# Mix across a grid of beta and gamma/delta values to get diverse fragility

dgp_grid <- expand.grid(
  beta_t  = c(0.00, 0.10, 0.25, 0.45),
  gamma_u = c(0.15, 0.35, 0.55, 0.75),
  delta_u = c(0.15, 0.35, 0.55, 0.75)
)
# sample N_SIM rows from the grid (with replacement)
set.seed(42)
dgp_sample <- dgp_grid[sample(nrow(dgp_grid), N_SIM, replace = TRUE), ]

# ---- run simulation ----------------------------------------------------------

cat("Running CFI validation simulation (v4 — adjustment-based)...\n")
cat(sprintf("  %d datasets, n=%d each, varied DGP parameters\n\n", N_SIM, N_OBS))

all_results <- vector("list", N_SIM)
n_kept <- 0L

pb_marks <- floor(seq(1, N_SIM, length.out = 21))
cat("  Progress: [")

for (i in seq_len(N_SIM)) {
  if (i %in% pb_marks) cat(".")

  row <- dgp_sample[i, ]

  sim <- tryCatch(
    simulate_adjusted_dataset(N_OBS, row$beta_t, row$gamma_u, row$delta_u),
    error = function(e) NULL
  )
  if (is.null(sim)) next

  truth <- assign_reference_truth(sim$p_naive, sim$p_adjusted, sim$bias_reduction)
  if (is.na(truth) || truth == "not_significant") next

  cfi_res <- run_cfi_on_naive(sim$fit_naive, sim$data)
  if (is.null(cfi_res)) next

  n_kept <- n_kept + 1L
  all_results[[n_kept]] <- data.frame(
    truth          = truth,
    beta_t         = row$beta_t,
    gamma_u        = row$gamma_u,
    delta_u        = row$delta_u,
    coef_naive     = sim$coef_naive,
    coef_adjusted    = sim$coef_adjusted,
    bias_reduction = sim$bias_reduction,
    p_naive        = sim$p_naive,
    p_adjusted       = sim$p_adjusted,
    cfi            = cfi_res$cfi,
    label          = cfi_res$label,
    rv_q           = cfi_res$rv_q,
    rv_qa          = cfi_res$rv_qa,
    evalue         = cfi_res$evalue,
    pct_bias       = cfi_res$pct_bias,
    score_rv       = cfi_res$score_rv,
    score_rva      = cfi_res$score_rva,
    score_ev       = cfi_res$score_ev,
    score_pct      = cfi_res$score_pct,
    stringsAsFactors = FALSE
  )
}

cat(sprintf("] %d datasets used\n\n", n_kept))
sim_data <- do.call(rbind, all_results[seq_len(n_kept)])
rownames(sim_data) <- NULL

# ---- summary -----------------------------------------------------------------

cat("=== Reference fragility distribution ===\n")
print(table(sim_data$truth))

cat("\n=== Mean CFI by reference fragility classification ===\n\n")
for (tr in c("robust", "mixed", "fragile")) {
  sub <- sim_data$cfi[sim_data$truth == tr]
  if (length(sub) == 0) next
  cat(sprintf("  %-10s  n=%4d  Mean=%5.1f  SD=%4.1f  Median=%5.1f\n",
              tr, length(sub), mean(sub), sd(sub), median(sub)))
}

cat("\n=== Component score means by reference fragility classification ===\n\n")
cat(sprintf("  %-10s  %7s  %7s  %7s  %7s  %7s\n",
            "Truth", "CFI", "RV(pt)", "RV(sg)", "E-val", "PctBias"))
cat(paste(rep("-", 55), collapse=""), "\n")
for (tr in c("robust", "mixed", "fragile")) {
  sub <- sim_data[sim_data$truth == tr, ]
  if (nrow(sub) == 0) next
  cat(sprintf("  %-10s  %7.1f  %7.1f  %7.1f  %7.1f  %7.1f\n",
              tr,
              mean(sub$cfi,       na.rm=TRUE),
              mean(sub$score_rv,  na.rm=TRUE),
              mean(sub$score_rva, na.rm=TRUE),
              mean(sub$score_ev,  na.rm=TRUE),
              mean(sub$score_pct, na.rm=TRUE)))
}

cat("\n=== Raw sensitivity metric means by reference fragility classification ===\n\n")
cat(sprintf("  %-10s  %7s  %7s  %7s  %7s\n",
            "Truth", "RV_q", "RV_qa", "E-val", "PctBias"))
cat(paste(rep("-", 45), collapse=""), "\n")
for (tr in c("robust", "mixed", "fragile")) {
  sub <- sim_data[sim_data$truth == tr, ]
  if (nrow(sub) == 0) next
  cat(sprintf("  %-10s  %7.4f  %7.4f  %7.3f  %7.1f%%\n",
              tr,
              mean(sub$rv_q,    na.rm=TRUE),
              mean(sub$rv_qa,   na.rm=TRUE),
              mean(sub$evalue,  na.rm=TRUE),
              mean(sub$pct_bias, na.rm=TRUE)))
}

cat("\n=== Correlation: CFI ~ bias_reduction ===\n")
cr <- cor(sim_data$cfi, sim_data$bias_reduction, use = "complete.obs",
          method = "spearman")
cat(sprintf("  Spearman r = %.3f\n", cr))
cat("  (negative expected: higher bias_reduction = more fragile = lower CFI)\n")

cat("\n=== AUC: does CFI predict fragile (fully adjusted) vs non-fragile? ===\n")
sim_data$is_fragile <- as.integer(sim_data$truth == "fragile")
wt  <- wilcox.test(cfi ~ is_fragile, data = sim_data)
auc <- wt$statistic / (sum(sim_data$is_fragile == 0) *
                        sum(sim_data$is_fragile == 1))
cat(sprintf("  AUC = %.3f  (0.5 = chance, 1.0 = perfect)\n", auc))

cat("\n=== Kruskal-Wallis (CFI ~ reference fragility classification) ===\n")
kt <- kruskal.test(cfi ~ truth, data = sim_data)
cat(sprintf("  chi-squared = %.2f, df = %d, p = %.2e\n",
            kt$statistic, kt$parameter, kt$p.value))

rob <- sim_data$cfi[sim_data$truth == "robust"]
fra <- sim_data$cfi[sim_data$truth == "fragile"]
if (length(fra) > 0 && length(rob) > 0) {
  wt2 <- wilcox.test(rob, fra)
  r_e <- 1 - (2 * wt2$statistic) / (length(rob) * length(fra))
  cat(sprintf("\n  Effect size (r, robust vs fragile): %.3f\n", r_e))
}

cat("\n=== CFI threshold analysis (threshold = 50) ===\n")
sim_data$pred_fragile <- as.integer(sim_data$cfi < 50)
TP <- sum(sim_data$is_fragile == 1 & sim_data$pred_fragile == 1)
FP <- sum(sim_data$is_fragile == 0 & sim_data$pred_fragile == 1)
TN <- sum(sim_data$is_fragile == 0 & sim_data$pred_fragile == 0)
FN <- sum(sim_data$is_fragile == 1 & sim_data$pred_fragile == 0)
sensitivity <- TP / (TP + FN)
specificity <- TN / (TN + FP)
cat(sprintf("  Sensitivity (fragile correctly flagged): %.1f%%\n",
            sensitivity * 100))
cat(sprintf("  Specificity (non-fragile correctly passed): %.1f%%\n",
            specificity * 100))

# Save results locally for use with cfi_validation_plots.R
# Note: results are not included in the package distribution
out_file <- file.path("inst", "simulation", "cfi_validation_results.rds")
saveRDS(sim_data, out_file)
cat(sprintf("\nResults saved locally to: %s\n", out_file))
cat("Run cfi_validation_plots.R to generate figures.\n")
