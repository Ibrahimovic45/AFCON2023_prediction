# AFCON2023_prediction

Predict AFCON 2023 matches with a small ML pipeline (Gradient Boosting) and an interactive Shiny dashboard.

This repository contains the Python simulation & model code and an R Shiny frontend that uses reticulate to call the Python simulation and plotting functions.

---

## Summary
- Purpose: run tournament simulations for the 2023 Africa Cup of Nations (AFCON), inspect per-match probabilities and visualize knockout brackets.
- Primary languages: R (Shiny app), Python (model + simulation + plotting).
- Entry point: `app24.R` (R/Shiny). Note: `app_24.R` is a near-duplicate backup and can be ignored or removed to avoid ambiguity.

### Stack / Notable libraries
- R: shiny, shinydashboard, reticulate, leaflet
- Python: pandas, scikit-learn (GradientBoostingClassifier), networkx, matplotlib, seaborn

## Recommended quickstart
1. Clone the repo

   git clone https://github.com/Ibrahimovic45/AFCON2023_prediction.git
   cd AFCON2023_prediction

2. Create the Python environment (use the provided `environment.yml`):

   conda env create -f environment.yml
   conda activate env-reticulate

   (If you prefer not to use conda, you can create a virtualenv and `pip install` the packages listed in `environment.yml`.)

   Note: if you prefer a different environment name, set RETICULATE_PYTHON to point to the Python interpreter you want reticulate to use, or change the call to `use_condaenv()` inside `app24.R`.

3. Install the R dependencies (run inside R / RStudio):

   install.packages(c("shiny","shinydashboard","reticulate","leaflet","tidyverse","viridis","shinydashboardPlus","plotly","rintrojs","shinycssloaders","shinyWidgets"))

4. Ensure app assets exist

   The Shiny app references images in a `www/` directory (mascot.jpg, civ.jpg, mrc.jpeg, nga.jpg, sen.jpg, egy.jpg). Create `www/` and add those images or remove/adjust the references in `app24.R` if you don't need them.

5. Run the app

   Recommended: open `app24.R` in RStudio and click "Run App" (reticulate integrates best from an interactive R session).

   Alternatively, from a shell:

   Rscript app24.R

## Key files
- app24.R              — main Shiny app (UI + server). Uses `source_python("afcon_simulation_f4.py")` and `source_python("plot.py")`
- afcon_simulation_f4.py — simulation and feature pipeline; defines `afcon_sim()` which returns simulated playoffs + text log
- plot.py              — draws knockout bracket using networkx and matplotlib
- model_gb.py          — trains / returns a GradientBoostingClassifier on `database.csv`
- model_evaluation.py  — notebook-style model evaluation (AUC / confusion matrix)
- database.csv         — training dataset (used by model_gb.py)
- team_stats_raw.csv   — raw team statistics used in building features
- intro.txt            — text shown in the app intro page
- Ivory_Coast.txt, Morocco.txt, Nigeria.txt, Senegal.txt, Egypt.txt — small team description files used by the app

## Notes / maintenance suggestions
- Duplicate file: `app_24.R` is a near-identical copy of `app24.R`. Consider removing it from the repository to avoid confusion during deployment.
- Model persistence: currently `afcon_simulation_f4.py` retrains or uses training CSV each run — consider training once and saving a serialized model (joblib/pickle) so the Shiny app runs faster.
- Assets: the app expects a `www/` folder with images (civ.jpg, mrc.jpeg, etc.). Keep these assets under `www/` so Shiny serves them correctly.
- Environment reproducibility: `environment.yml` is provided for conda users to create a reproducible Python environment.

## Contributing
Contributing: no CONTRIBUTING.md is present; open an issue or PR to discuss changes.

---
