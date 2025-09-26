# Repository Guidelines

## Project Structure & Module Organization
The repository centers on a scripted R pipeline. Core steps live in `analysis/01_preprocessing.R` through `analysis/04_visualization.R`; keep new stages numbered with two-digit prefixes to preserve execution order. Shared helpers belong in `analysis/functions/`, split by concern (analytics, plotting, themes). Participant files sit in `data/raw/`, while canonical derivatives overwrite `data/processed.csv` or add new artifacts under `data/`. Script outputs populate `outputs/` (tables, figures, interactive drafts); avoid manual edits so diffs reflect script changes. Leave `.legacy/` as read-only historical context unless migrating code forward.

## Build, Test, and Development Commands
Run scripts from the repository root to reproduce the workflow:
- `Rscript analysis/01_preprocessing.R` rebuilds the cleaned dataset and exclusion plot.
- `Rscript analysis/02_descriptive_stats.R` refreshes summary tables in `outputs/tables/`.
- `Rscript analysis/03_statistical_models.R` fits mixed models and writes contrasts to `outputs/tables/`.
- `Rscript analysis/04_visualization.R` regenerates publication figures in `outputs/figures/`.
Open `RM_loss_n_gain.Rproj` in RStudio for an interactive environment that respects project settings.

## Coding Style & Naming Conventions
Use UTF-8 encoding, two-space indentation (per `.Rproj`), and tidyverse pipelines. Name scripts `NN_topic.R`, functions and objects in `snake_case`, and keep helper files focused with short, documented functions. Reuse the section banner comment style for readability and favor vectorised tidyverse verbs over base loops. Run `styler::style_file()` or `lintr::lint_dir('analysis')` before committing when available.

## Testing Guidelines
There is no dedicated `testthat` suite yet; validate changes by rerunning the full script chain in order and verifying that key artifacts (e.g., `data/processed.csv`, `outputs/figures/data_exclusions_exp1.png`) update as expected. For new helpers, add inline guards (`stopifnot`) and, where feasible, create `analysis/tests/test_<topic>.R` with reproducible edge cases. Note any shifts in participant counts, exclusion rates, or model fit metrics in your PR.

## Commit & Pull Request Guidelines
Write commit summaries in the imperative mood under 72 characters (`Add spacing diagnostics table`), and separate data refreshes from code refactors. Before opening a pull request, rerun the scripts you touched, stage only intentional artifacts, and ensure ignored files (`.Rhistory`, `.RData`) stay untracked. PR descriptions should explain the motivation, list regenerated outputs, highlight data touchpoints, attach key before/after visuals, and reference issues with `Fixes #ID` when applicable.

## Data Handling & Security Tips
Raw exports in `data/raw/` may contain sensitive timing or identifiers—scrub and document any additions. Prefer sharing derived summaries from `data/` or `outputs/`, and avoid committing deployment credentials or unpublished participant data. Coordinate before removing files from `.legacy/` to preserve provenance.
