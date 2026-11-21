Milestone 2 submission

Milestone 2.pdf is the write-up. The exact revised LLM prompt is in
Milestone2/onet_labeling_prompt_v2.txt.

Reproduce the results

Run these commands from the folder containing this README. The figures
and prediction script requires the R packages ggplot2 and glmnet.

Rscript Milestone2/06_compare_onet_pilot.R
Rscript Milestone2/06_compare_onet_pilot.R holdout
Rscript Milestone2/09_results_and_prediction.R

The first two commands print the pilot and holdout audits. The last
command prints the ACS and model results and writes both figures to
Milestone2/figures.

Data

Milestone1/tidy_data.csv contains 2023 ACS estimates from IPUMS USA.
Each row represents one year, subgroup, and educational attainment
category. sample_n counts respondents, weighted_population sums person
weights, and share_pct is the weighted share within a subgroup.

onet_data/cleaned/onet_llm_input.csv contains one row per O*NET 28.1
occupation. llm_text combines the occupation description and available
task text selected for labeling. onet_soc_code identifies occupations,
and soc2018_code identifies the broader six-digit SOC group.

The pilot and holdout folders contain the human-reviewed and
independently hand-coded scores, respectively, plus the corresponding
LLM scores. A score of 1 indicates evidence of the category, 0 indicates
no qualifying duty in sufficiently detailed text, and a blank
indicates insufficient evidence. Match score files by onet_soc_code.

Data attribution

The O*NET® 28.1 Database is provided by the U.S. Department of Labor,
Employment and Training Administration (USDOL/ETA), under CC BY 4.0
(https://creativecommons.org/licenses/by/4.0/).
O*NET® is a trademark of USDOL/ETA.
Source: https://www.onetcenter.org/db_releases.html

The project selected, normalized, and combined descriptions and task
statements and added human and LLM labels. USDOL/ETA has not approved,
endorsed, or tested these adaptations.
