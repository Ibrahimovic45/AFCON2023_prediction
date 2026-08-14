# AFCON2023_prediction

Predict AFCON 2023 matches with a small ML pipeline (Gradient Boosting) and an interactive Shiny dashboard.

This repository contains the Python simulation & model code and an R Shiny frontend that uses reticulate to call the Python simulation and plotting functions.

---

## Summary
- Purpose: run tournament simulations for the 2023 Africa Cup of Nations (AFCON), inspect per-match probabilities and visualize knockout brackets.
- Primary languages: R (Shiny app), Python (model + simulation + plotting).
- Entry point: `app24.R` (R/Shiny). There is also a near-duplicate `app_24.R` — see "Notes" below.

### Stack / Notable libraries
- R: shiny, shinydashboard, reticulate, leaflet
- Python: pandas, scikit-learn (GradientBoostingClassifier), networkx, matplotlib, seaborn

## Recommended quickstart
1. Clone the repo

   git clone https://github.com/Ibrahimovic45/AFCON2023_prediction.git
   cd AFCON2023_prediction

2. Create the Python environment (the Shiny app expects a conda env named `env-reticulate` by default):

   conda create -n env-reticulate python=3.10 -y
   conda activate env-reticulate
   pip install pandas scikit-learn networkx matplotlib seaborn pygraphviz graphviz

   Note: if you prefer a different environment name, set RETICULATE_PYTHON or change the call to `use_condaenv()` inside `app24.R`.

3. Install the R dependencies (run inside R / RStudio):

   install.packages(c("shiny","shinydashboard","reticulate","leaflet","tidyverse","viridis","shinydashboardPlus","plotly","rintrojs","shinycssloaders","shinyWidgets"))

4. Run the app from RStudio by opening `app24.R` and clicking "Run App", or from the command line:

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
- Duplicate file: `app_24.R` appears to be a near-identical copy of `app24.R`. Consider removing one to avoid confusion.
- Model persistence: currently `afcon_simulation_f4.py` retrains or uses training CSV each run — consider training once and saving a serialized model (joblib/pickle) so the Shiny app runs faster.
- Assets: the app expects a `www/` folder with images (civ.jpg, mrc.jpeg, etc.). Keep these assets under `www/` so Shiny serves them correctly.
- Environment reproducibility: consider adding an environment.yml (conda) and a DESCRIPTION or Dockerfile for reproducible runs.

## Contributing
See CONTRIBUTING.md for development, tests and PR guidelines.

---

If you want, I can:
- Remove the duplicate `app_24.R` and keep a single app file, or rename it to `app24.R.bak`.
- Add an environment.yml or Dockerfile for reproducible installs.
- Persist the trained model (joblib) and update the simulation to load it rather than retraining every run.
