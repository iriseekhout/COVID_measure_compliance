# Save output ------------------------------------------------------------------

# * Summary by group -----------------------------------------------------------
sink(file ="experiment2/output/txt/exp2_summary_by_group.txt")

cat(
"
--------------------------------------------------------------------------------

EXPERIMENT 2

--------------------------------------------------------------------------------
"
)

cat(
"
--------------------------------------------------------------------------------

CONTINUOUS VARIABLES

--------------------------------------------------------------------------------
"
)
T1_continuous
cat("\n\n")
T1_ttest
cat("\n\n")

cat(
"
--------------------------------------------------------------------------------

CATEGORICAL VARIABLES

--------------------------------------------------------------------------------
"
)
T1_categorical
cat("\n\n")
T1_chisq
cat("\n\n")
T1_wilcox



sink()



save(
  list = c(
    "T1_continuous", "T1_ttest",
    "T1_categorical", "T1_chisq", "T1_wilcox"
  ),
  file = here("experiment2", "output", "rdata", "exp2_summary_by_group.rdata")
)




# * Internal consistency constructs --------------------------------------------
sink(file = here("experiment2", "output", "txt", "exp2_internal_consistency_constructs.txt"))

cat(
  "
--------------------------------------------------------------------------------

EXPERIMENT 2

--------------------------------------------------------------------------------
"
)

cat("Self-efficacy T0 \n")
alpha_SE_0

cat("\n Intention T0 \n")
alpha_INT_0

cat("\n perceived risk (self) T0 \n")
spearman_PR_0

cat("\n perceived vulnerability (others) T0 \n")
alpha_PV_0

cat("\n response efficacy T0 \n")
spearman_RE_0

cat("\n behaviour index T1 \n")
alpha_behaviour_1

cat("\n Self-efficacy T1 \n")
alpha_SE_1

sink()



save(
  list = c(
    "alpha_SE_0",
    "alpha_INT_0",
    "spearman_PR_0",
    "alpha_PV_0",
    "spearman_RE_0",
    "alpha_behaviour_1",
    "alpha_SE_1"
  ),
  file = here("experiment2", "output", "rdata", "exp2_internal_consistency_constructs.rdata")
)



# * Descriptive statistics constructs ------------------------------------------
sink(file = here("experiment2", "output", "txt", "exp2_descriptive_statistics_constructs.txt"))

cat(
"
--------------------------------------------------------------------------------

EXPERIMENT 2

--------------------------------------------------------------------------------
"
)

descriptive_statistics

sink()



save(
  list = "descriptive_statistics",
  file = here("experiment2", "output", "rdata", "exp2_descriptive_statistics_constructs.rdata")
)



# * Regression models ----------------------------------------------------------
sink(file = here("experiment2", "output", "txt", "exp2_regression_models.txt"))

cat(
"
--------------------------------------------------------------------------------

EXPERIMENT 2

--------------------------------------------------------------------------------
"
)

cat("Regression coefficients T0 \n")
regression_output

cat("\n R^2 and Cohen's f^2 \n")
regression_output_f2

cat("\n")
cat("f^2 = (R^2ab - R^2a)/(1-R^2ab)")



sink()



save(
  list = c("regression_output", "regression_output_f2"),
  file = here("experiment2", "output", "rdata", "exp2_regression_models.rdata")
)



# * Frequencies preventive behaviour -------------------------------------------
sink(file = here("experiment2", "output", "txt", "exp2_frequencies_behaviour.txt"))

cat(
"
--------------------------------------------------------------------------------

EXPERIMENT 2

--------------------------------------------------------------------------------
"
)

cat(
"
1. Wash my hands regularly (20 sec.) with soap and water \n
2. Always sneezing or coughing in my elbow \n
3. Always keep 1.5 meters away from other people \n
4. Avoid people who are vulnerable \n
5. Work from home as much as possible \n
6. Avoid crowds \n
7. Use paper tissues \n
"
)
behaviour_freq

cat("\n\n Item summary \n")
item_summary_behaviour
cat("\n")

sink()



save(
  list = c("behaviour_freq", "item_summary_behaviour"),
  file = here("experiment2", "output", "rdata", "exp2_frequencies_behaviour.rdata")
)



# * Wilcoxon preventive Behaviour ----------------------------------------------
sink(file = here("experiment2", "output", "txt", "exp2_wilcox_behaviour.txt"))

cat(
"
--------------------------------------------------------------------------------

EXPERIMENT 2

--------------------------------------------------------------------------------
"
)

cat(
"
1. Wash my hands regularly (20 sec.) with soap and water \n
2. Always sneezing or coughing in my elbow \n
3. Always keep 1.5 meters away from other people \n
4. Avoid people who are vulnerable \n
5. Work from home as much as possible \n
6. Avoid crowds \n
7. Use paper tissues \n
"
)
wilcox_output

sink()



save(
  list = c("wilcox_output"),
  file = here("experiment2", "output", "rdata", "exp2_wilcox_behaviour.rdata")
)



# * Frequencies BHJ ------------------------------------------------------------
sink(file = here("experiment2", "output", "txt", "exp2_frequencies_BHJ.txt"))

cat(
"
--------------------------------------------------------------------------------

EXPERIMENT 2

--------------------------------------------------------------------------------
"
)

cat(
"
number of videos  \n
1. Maarten \n
2. Nils & Luka \n
3. Rosanna \n
4. Fleur \n
5. I did not watch any videos \n
relevance \n
recognisable \n
helpful \n
"
)
BHJ_categorical


sink()



save(
  list = c("BHJ_categorical"),
  file = here("experiment2", "output", "rdata", "exp2_frequencies_BHJ.rdata")
)

