# Run All Analyses - Redundancy Masking Project
# Execute this script from RStudio with the project root as working directory

cat("========================================\n")
cat("Redundancy Masking Analysis Pipeline\n")
cat("========================================\n\n")

# Experiment 1 Analysis (13 scripts)
cat("========== EXPERIMENT 1 ==========\n\n")

cat("[1/13] Preprocessing exp1 data...\n")
source("analysis/exp1/01_preprocessing.R")

cat("[2/13] Creating number deviation plot...\n")
source("analysis/exp1/02_number_deviation_plot.R")

cat("[3/13] Running spacing deviation model...\n")
source("analysis/exp1/03_spacing_deviation_model.R")

cat("[4/13] Creating spacing deviation plot...\n")
source("analysis/exp1/04_spacing_deviation_plot.R")

cat("[5/13] Running width deviation model...\n")
source("analysis/exp1/05_width_deviation_model.R")

cat("[6/13] Creating width deviation plot...\n")
source("analysis/exp1/06_width_deviation_plot.R")

cat("[7/13] Running Bayesian analysis...\n")
source("analysis/exp1/07_bayesian_analysis.R")

cat("[8/13] Creating Bayesian plot...\n")
source("analysis/exp1/08_bayesian_plot.R")

cat("[9/13] Running density deviation analysis...\n")
source("analysis/exp1/09_density_deviation_analysis.R")

cat("[10/13] Creating density deviation plot...\n")
source("analysis/exp1/10_density_deviation_plot.R")

cat("[11/13] Creating density deviation ridge plot...\n")
source("analysis/exp1/11_density_deviation_ridgeplot.R")

cat("[12/13] Running density by condition analysis...\n")
source("analysis/exp1/12_density_by_condition_analysis.R")

cat("[13/13] Creating density by condition plot...\n")
source("analysis/exp1/13_density_by_condition_plot.R")

cat("\nExperiment 1 complete!\n\n")

# Experiment 2 Analysis (6 scripts)
cat("========== EXPERIMENT 2 ==========\n\n")

cat("[1/6] Preprocessing exp2 data...\n")
source("analysis/exp2/01_preprocessing.R")

cat("[2/6] Creating number deviation plot...\n")
source("analysis/exp2/02_number_deviation_plot.R")

cat("[3/6] Running width deviation model...\n")
source("analysis/exp2/03_width_deviation_model.R")

cat("[4/6] Creating width deviation plot...\n")
source("analysis/exp2/04_width_deviation_plot.R")

cat("[5/6] Running Bayesian analysis...\n")
source("analysis/exp2/05_bayesian_analysis.R")

cat("[6/6] Creating Bayesian plot...\n")
source("analysis/exp2/06_bayesian_plot.R")

cat("\nExperiment 2 complete!\n\n")

# Combined Analysis
cat("========== COMBINED ANALYSIS ==========\n\n")

cat("[1/1] Creating combined bar plot...\n")
source("analysis/combined_width_deviation_barplot.R")

cat("\n========================================\n")
cat("All analyses complete!\n")
cat("========================================\n\n")
cat("Output locations:\n")
cat("  - Exp1 figures: outputs/exp1/figures/\n")
cat("  - Exp1 tables:  outputs/exp1/tables/\n")
cat("  - Exp2 figures: outputs/exp2/figures/\n")
cat("  - Exp2 tables:  outputs/exp2/tables/\n")
cat("  - Combined:     outputs/\n")
