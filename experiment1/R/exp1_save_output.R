# Save output ------------------------------------------------------------------

# * Summary by group -----------------------------------------------------------
sink(file = here("experiment1","output","txt","exp1_summary_by_group.txt"))

cat(
"
--------------------------------------------------------------------------------
EXPERIMENT 1
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
  file = "experiment1/output/rdata/exp1_summary_by_group.rdata"
)



# * Internal consistency constructs --------------------------------------------
sink(file = "experiment1/output/txt/exp1_internal_consistency_constructs.txt")

cat(
"
--------------------------------------------------------------------------------
EXPERIMENT 1
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
  file = "experiment1/output/rdata/exp1_internal_consistency_constructs.rdata"
)



# * Descriptive statistics constructs ------------------------------------------
sink(file = "experiment1/output/txt/exp1_descriptive_statistics_constructs.txt")

cat(
"
--------------------------------------------------------------------------------
EXPERIMENT 1
--------------------------------------------------------------------------------
"
)

descriptive_statistics

sink()



save(
  list = "descriptive_statistics",
  file = "experiment1/output/rdata/exp1_descriptive_statistics_constructs.rdata"
)



# * Regression models ----------------------------------------------------------
sink(file = "experiment1/output/txt/exp1_regression_models.txt")

cat(
"
--------------------------------------------------------------------------------
EXPERIMENT 1
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
  file = "experiment1/output/rdata/exp1_regression_models.rdata"
)



# * Frequencies preventive behaviour -------------------------------------------
sink(file = "experiment1/output/txt/exp1_frequencies_behaviour.txt")
cat(
  "
--------------------------------------------------------------------------------
EXPERIMENT 1
--------------------------------------------------------------------------------
"
)

cat(
"
1. Wash my hands regularly (20 sec.) with soap and water \n
2. Always sneezing or coughing in my elbow \n
3. Always keep 1.5 meters away from other people \n
4. Avoid people who are vulnerable \n
5. Stay at home as much as possible \n
6. Receive as little visitors as possible \n
7. Use paper tissues \n
8. Working from home as much as possible \n
9. Avoid crowds \n
"
)
behaviour_freq

cat("\n\n Item summary")
item_summary_behaviour
cat("\n")

sink()



save(
  list = c("behaviour_freq", "item_summary_behaviour"),
  file = "experiment1/output/rdata/exp1_frequencies_behaviour.rdata"
)



# * Wilcoxon preventive Behaviour ----------------------------------------------
sink(file = "experiment1/output/txt/exp1_wilcox_behaviour.txt")

cat(
"
--------------------------------------------------------------------------------
EXPERIMENT 1
--------------------------------------------------------------------------------
"
)

cat("
1. Wash my hands regularly (20 sec.) with soap and water \n
2. Always sneezing or coughing in my elbow \n
3. Always keep 1.5 meters away from other people \n
4. Avoid people who are vulnerable \n
5. Stay at home as much as possible \n
6. Receive as little visitors as possible \n
7. Use paper tissues \n
8. working from home as much as possible \n
9. Avoid crowds \n
"
)
wilcox_output

sink()

save(
  list = c("wilcox_output"),
  file = "experiment1/output/rdata/exp1_wilcox_behaviour.rdata"
)



# * Frequencies VHS ------------------------------------------------------------

sink(file = "experiment1/output/txt/exp1_frequencies_VHS.txt")

cat(
  "
--------------------------------------------------------------------------------
EXPERIMENT 1
--------------------------------------------------------------------------------
"
)

cat(
"
1. When I am in the supermarket and I have less than 1.5 meters to pass someone \n
2. When there is less than 1.5 meters of space when walking \n
3. When someone gets too close \n
4. When I see / notice that someone needs a hug \n
5. When I miss going out for days \n
6. When I have a special occasion, such as a birthday or holiday \n
7. When I feel alone or lonely \n
8. When I need a hug \n
9. When someone is suddenly at the door \n
10. When I feel lonely and need to see friends or family \n
11. When I don't have soap in the house \n
12. When I forget to wash my hands \n
13. When family members (grandma, grandpa, mother, father) feel lonely and ask if I come by \n
"
)

VHS_output

sink()



save(
  list = c("VHS_output"),
  file = "experiment1/output/rdata/exp1_frequencies_VHS.rdata"
)

