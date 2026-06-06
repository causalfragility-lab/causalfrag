# =============================================================================
# CFI Validation Figures
# causalfrag: A Cross-Framework Causal Fragility Index
# Run after cfi_validation_study.R
# Produces 3 publication-ready figures (PDF + PNG)
# =============================================================================

if (!requireNamespace("ggplot2", quietly = TRUE))
  stop("Install ggplot2: install.packages('ggplot2')")

library(ggplot2)

sim_data <- readRDS("inst/simulation/cfi_validation_results.rds")
sim_data$is_fragile <- as.integer(sim_data$truth == "fragile")

# factor with clean labels and correct order
sim_data$truth_label <- factor(sim_data$truth,
  levels = c("robust", "mixed", "fragile"),
  labels = c("Adjustment-stable\n(robust)",
             "Intermediate\n(mixed)",
             "Adjustment-sensitive\n(fragile)"))

pal <- c(
  "Adjustment-stable\n(robust)"    = "#3B6D11",
  "Intermediate\n(mixed)"          = "#BA7517",
  "Adjustment-sensitive\n(fragile)" = "#A32D2D"
)

out_dir <- file.path(tempdir(), "causalfrag-validation-plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# Figure 1: CFI distribution by reference fragility condition
# =============================================================================

p1 <- ggplot(sim_data, aes(x = truth_label, y = cfi, fill = truth_label)) +
  geom_boxplot(alpha = 0.75, outlier.shape = NA, width = 0.45,
               linewidth = 0.4) +
  geom_jitter(aes(color = truth_label), width = 0.12,
              alpha = 0.12, size = 0.7) +
  geom_hline(yintercept = c(50, 70, 85), linetype = "dashed",
             color = "gray55", linewidth = 0.35) +
  annotate("text", x = 0.52, y = 25,  label = "Fragile",
           size = 2.8, color = "gray45", hjust = 0) +
  annotate("text", x = 0.52, y = 59,  label = "Caution",
           size = 2.8, color = "gray45", hjust = 0) +
  annotate("text", x = 0.52, y = 77,  label = "Moderately robust",
           size = 2.8, color = "gray45", hjust = 0) +
  annotate("text", x = 0.52, y = 91,  label = "Robust",
           size = 2.8, color = "gray45", hjust = 0) +
  scale_fill_manual(values = pal) +
  scale_color_manual(values = pal) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     name = "Causal Fragility Index (CFI)") +
  scale_x_discrete(name = NULL) +
  labs(
    title   = "Figure 1. CFI Distribution by Reference Fragility Condition",
    subtitle = sprintf(
      "Adjustment-based simulation validation: %d datasets, n = 300 each",
      nrow(sim_data)),
    caption = paste0(
      "Dashed lines indicate CFI classification boundaries (50, 70, 85).\n",
      "Reference fragility assigned by comparing naive vs. fully adjusted ",
      "regression estimates.\n",
      "AUC = 0.813; Kruskal-Wallis chi-square(2) = 237.94, p < .001.")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(size = 11, face = "bold"),
    plot.subtitle    = element_text(size = 9,  color = "gray35"),
    plot.caption     = element_text(size = 8,  color = "gray45",
                                    hjust = 0, lineheight = 1.3),
    panel.grid.major.x = element_blank(),
    axis.text.x      = element_text(size = 9)
  )

ggsave(file.path(out_dir, "fig1_cfi_by_condition.pdf"), p1,
       width = 6.5, height = 5)
ggsave(file.path(out_dir, "fig1_cfi_by_condition.png"), p1,
       width = 6.5, height = 5, dpi = 300)
cat("Figure 1 saved\n")


# =============================================================================
# Figure 2: ROC-style threshold plot (sensitivity vs specificity)
# =============================================================================

thresholds <- seq(0, 100, by = 1)
roc_data <- do.call(rbind, lapply(thresholds, function(thr) {
  pred <- as.integer(sim_data$cfi < thr)
  TP   <- sum(sim_data$is_fragile == 1 & pred == 1)
  FP   <- sum(sim_data$is_fragile == 0 & pred == 1)
  TN   <- sum(sim_data$is_fragile == 0 & pred == 0)
  FN   <- sum(sim_data$is_fragile == 1 & pred == 0)
  data.frame(
    threshold   = thr,
    sensitivity = TP / max(TP + FN, 1),
    specificity = TN / max(TN + FP, 1),
    fpr         = 1 - TN / max(TN + FP, 1)
  )
}))

# key operating points
key_pts <- roc_data[roc_data$threshold %in% c(50, 60, 70, 80), ]

p2 <- ggplot(roc_data, aes(x = fpr, y = sensitivity)) +
  geom_line(color = "#185FA5", linewidth = 0.9) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray60", linewidth = 0.4) +
  geom_point(data = key_pts, aes(x = fpr, y = sensitivity),
             color = "#A32D2D", size = 3) +
  geom_text(data = key_pts,
            aes(x = fpr + 0.02, y = sensitivity - 0.03,
                label = paste0("CFI<", threshold)),
            size = 3, hjust = 0, color = "#A32D2D") +
  annotate("text", x = 0.65, y = 0.15,
           label = "AUC = 0.813", size = 4,
           color = "#185FA5", fontface = "bold") +
  scale_x_continuous(name = "False positive rate (1 - Specificity)",
                     limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_y_continuous(name = "Sensitivity (true positive rate)",
                     limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  labs(
    title   = "Figure 2. ROC Curve for CFI as a Fragility Diagnostic",
    subtitle = "Predicting adjustment-sensitive inferences across CFI thresholds",
    caption  = paste0(
      "Red points indicate key operating thresholds (CFI < 50, 60, 70, 80).\n",
      "CFI < 70 achieves best balanced accuracy (72.8%): ",
      "sensitivity = 80.1%, specificity = 65.5%.")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(size = 11, face = "bold"),
    plot.subtitle = element_text(size = 9,  color = "gray35"),
    plot.caption  = element_text(size = 8,  color = "gray45",
                                  hjust = 0, lineheight = 1.3),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(out_dir, "fig2_roc_curve.pdf"), p2,
       width = 5.5, height = 5)
ggsave(file.path(out_dir, "fig2_roc_curve.png"), p2,
       width = 5.5, height = 5, dpi = 300)
cat("Figure 2 saved\n")


# =============================================================================
# Figure 3: Component score heatmap by condition
# =============================================================================

comp_summary <- do.call(rbind, lapply(c("robust","mixed","fragile"), function(tr) {
  sub <- sim_data[sim_data$truth == tr, ]
  data.frame(
    condition  = tr,
    component  = c("RV (point est.)", "RV (significance)",
                   "E-value", "Pct bias (ITCV)"),
    mean_score = c(mean(sub$score_rv,  na.rm=TRUE),
                   mean(sub$score_rva, na.rm=TRUE),
                   mean(sub$score_ev,  na.rm=TRUE),
                   mean(sub$score_pct, na.rm=TRUE)),
    stringsAsFactors = FALSE
  )
}))

comp_summary$condition <- factor(comp_summary$condition,
  levels = c("fragile", "mixed", "robust"),
  labels = c("Adjustment-sensitive", "Intermediate", "Adjustment-stable"))
comp_summary$component <- factor(comp_summary$component,
  levels = c("RV (point est.)", "RV (significance)",
             "E-value", "Pct bias (ITCV)"))

p3 <- ggplot(comp_summary,
             aes(x = component, y = condition, fill = mean_score)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.1f", mean_score)),
            size = 3.5, color = "white", fontface = "bold") +
  scale_fill_gradient2(
    low      = "#A32D2D",
    mid      = "#BA7517",
    high     = "#3B6D11",
    midpoint = 60,
    limits   = c(0, 100),
    name     = "Mean\ncomponent\nscore"
  ) +
  scale_x_discrete(name = NULL) +
  scale_y_discrete(name = NULL) +
  labs(
    title   = "Figure 3. Mean CFI Component Scores by Reference Fragility Condition",
    subtitle = "Each cell shows the mean scaled score (0-100) for that framework and condition",
    caption  = paste0(
      "Pct bias (ITCV) saturates at 100 across conditions, indicating limited ",
      "discrimination by this component.\n",
      "RV (significance) shows the strongest separation between fragile and ",
      "non-fragile conditions.")
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(size = 11, face = "bold"),
    plot.subtitle    = element_text(size = 9,  color = "gray35"),
    plot.caption     = element_text(size = 8,  color = "gray45",
                                     hjust = 0, lineheight = 1.3),
    axis.text.x      = element_text(size = 9, angle = 15, hjust = 1),
    panel.grid        = element_blank(),
    legend.key.height = unit(1.2, "cm")
  )

ggsave(file.path(out_dir, "fig3_component_heatmap.pdf"), p3,
       width = 7, height = 4)
ggsave(file.path(out_dir, "fig3_component_heatmap.png"), p3,
       width = 7, height = 4, dpi = 300)
cat("Figure 3 saved\n")

cat(sprintf("\nAll figures saved to: %s\n", out_dir))
cat("  fig1_cfi_by_condition.pdf / .png\n")
cat("  fig2_roc_curve.pdf / .png\n")
cat("  fig3_component_heatmap.pdf / .png\n")
