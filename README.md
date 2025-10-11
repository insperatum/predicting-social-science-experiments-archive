# Replication Archive for _Predicting Results of Social Science Experiments Using Large Language Models_

## Data

All data used in our analysis is stored in the `data` directory:

### Survey of Social Scientists
- Data is located in `survey_data/`

### Archive 1 (TESS; Coppock et al)
- `rct_responses.RDS` - All original human responses
- `LLM_responses.RDS` - Prompts and responses for all LLM calls
- `forecasting_responses.RDS` - Data from layperson forecasters
- Research Assistant coding files:
  - `RA_study_features.csv` - Study-level features, publication dates, etc.
  - `RA_outcome_features.csv` - Features of the dependent variables in each study
  - `RA_hypotheses.RDS` - Contrasts that were hypothesized by the original authors
  - `gpt_author_recognition.csv` - Results from robustness check on whether GPT-4 can guess paper authors.

### Archive 2 (Megastudies)
- `megastudies.rds` - All effects and GPT-4 predictions (calculated by the same method as Archive 1)
  - These are preprocessed rather than raw data, as raw data isn't available for many studies
  - Some studies are unpublished, and so condition names have been changed to avoid pre-empting authors' original publications.
- `individual_expert_predictions.rds` - Expert forecasts (used in the paper section: _Identifying Effective Interventions_).

## Code

### Setup and Execution
1. Install all requirements using `00_requirements.R`
2. If required, modify `config.R` (see below)
3. Generate all paper figures by running `00_run_all_analyses.R`
   - Results are saved in `output/plots/`

### Performance Notes
- Intermediate results are saved in `output/processed_data/` and loaded by default, so subsequent runs will be faster
- Analysis is computationally intensive due to running many replications and averaging results
  - Takes approximately 2 hours on a 2024 MacBook Pro
  - For faster execution: modify `config.R` and decrease `n_runs = 32` to e.g. `n_runs = 2`. Results will be similar but noisier
  - By default, `config.R` uses `future::plan("multisession", workers = n_workers)` to parallelize over cores. If you experience issues with this setup, we suggest changing this to use the future.callr package (`future::plan(future.callr::callr, workers = n_workers)`) or otherwise disabling parallelization altogether (`future::plan("sequential")`)