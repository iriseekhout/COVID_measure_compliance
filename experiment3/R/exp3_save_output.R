# Save output ------------------------------------------------------------------

# * Summary by group -----------------------------------------------------------
sink(file = here("experiment3","output","txt","exp3_summary_by_group.txt"))

cat(
"
--------------------------------------------------------------------------------

EXPERIMENT 3

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
  file = here("experiment3", "output", "rdata", "exp3_summary_by_group.rdata")
)



# * Internal consistency constructs --------------------------------------------
sink(file = here("experiment3", "output", "txt", "exp3_internal_consistency_constructs.txt"))

cat(
"
--------------------------------------------------------------------------------

EXPERIMENT 3

--------------------------------------------------------------------------------
"
)

cat("self-efficacy T0 \n")
alpha_SE_0

cat("\n intention T0 \n")
alpha_INT_0

cat("\n perceived risk (self) T0 \n")
spearman_PR_0

cat("\n perceived vulnerability (others) T0 \n")
alpha_PV_0

cat("\n response efficacy T0 \n")
spearman_RE_0

cat("\n feelings T0 \n")
alpha_feelings_0

cat("\n statements T0 \n")
alpha_statements_0

cat("\n statements T0 (2) \n")
alpha_statements_0_2

cat("\n behaviour index T1 \n")
alpha_behaviour_1

cat("\n self-efficacy T1 \n")
alpha_SE_1

sink()



save(
  list = c(
    "alpha_SE_0",
    "alpha_INT_0",
    "spearman_PR_0",
    "alpha_PV_0",
    "spearman_RE_0",
    "alpha_feelings_0",
    "alpha_statements_0",
    "alpha_statements_0_2",
    "alpha_behaviour_1",
    "alpha_SE_1"
  ),
  file = here("experiment3", "output", "rdata", "exp3_internal_consistency_constructs.rdata")
)



# * Descriptive statistics constructs ------------------------------------------
sink(file = here("experiment3", "output", "txt", "exp3_descriptive_statistics_constructs.txt"))

cat(
"
--------------------------------------------------------------------------------

EXPERIMENT 3

--------------------------------------------------------------------------------
"
)

descriptive_statistics

sink()



save(
  list = "descriptive_statistics",
  file = here("experiment3", "output", "rdata", "exp3_descriptive_statistics_constructs.rdata")
)



# * Regression models ----------------------------------------------------------
sink(file = here("experiment3", "output", "txt", "exp3_regression_models.txt"))

cat(
"
--------------------------------------------------------------------------------

EXPERIMENT 3

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
  file = here("experiment3", "output", "rdata", "exp3_regression_models.rdata")
)



# * Frequencies preventive behaviour -------------------------------------------
sink(file = here("experiment3", "output", "txt", "exp3_frequencies_behaviour.txt"))

cat(
"
--------------------------------------------------------------------------------

EXPERIMENT 3

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

cat("\n\n Item summary")
item_summary_behaviour
cat("\n")

sink()



save(
  list = c("behaviour_freq", "item_summary_behaviour"),
  file = here("experiment3", "output", "rdata", "exp3_frequencies_behaviour.rdata")
)



# * Wilcoxon preventive Behaviour ----------------------------------------------
sink(file = here("experiment3", "output", "txt", "exp3_wilcox_behaviour.txt"))

cat(
"
--------------------------------------------------------------------------------

EXPERIMENT 3

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
  file = here("experiment3", "output", "rdata", "exp3_wilcox_behaviour.rdata")
)



# * Frequencies SI ------------------------------------------------------------

sink(file = here("experiment3", "output", "txt", "exp3_frequencies_SI.txt"))

cat(
"
--------------------------------------------------------------------------------

EXPERIMENT 3

--------------------------------------------------------------------------------
"
)


SI_categorical
cat("\n\n")
descriptive_statistics_SI

sink()



save(
  list = c("SI_categorical", "descriptive_statistics_SI"),
  file = here("experiment3", "output", "rdata", "exp3_frequencies_SI.rdata")
)

