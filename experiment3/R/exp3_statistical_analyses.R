# Statistical analyses ---------------------------------------------------------

# 1. Summary by group ----------------------------------------------------------

# n
T1_n <- dataset %>%
  mutate(
    sex = Geslacht_c,
    educational_level = Opleiding_recoded,
    country_of_birth = Geboorteland_recoded,
    kids = Kinderen,
    infected = Besmet,
    contact = Contact,
    risk = Risico,
    works_in_care = Werk_zorg,
    serious = Serieus
  ) %>%
  select(
    group,
    sex, educational_level, country_of_birth, kids,
    infected, contact, risk, works_in_care,
    serious
  ) %>%
  pivot_longer(
    .,
    cols = c(everything(), - group),
    names_to = "variable"
  ) %>%
  group_by(group, variable, value) %>%
  summarise(
    across(
      everything()
    ),
    n = n(),
    .groups = "drop"
  ) %>%
  print()



# pct
T1_pct <- T1_n %>%
  group_by(group, variable) %>%
  filter(!is.na(value)) %>%
  mutate(
    pct = n / sum(n) * 100,
    pct = round(pct, 2)
  ) %>%
  select(group, variable, value, n, pct) %>%
  pivot_wider(
    .,
    names_from = group,
    values_from = c(n, pct)
  ) %>%
  select(
    variable, value, pct_1, pct_2
  ) %>%
  as.data.frame() %>%
  print()



# T1 categorical
T1_categorical <- full_join(
  T1_n %>%
    pivot_wider(
      .,
      names_from = group,
      names_prefix = "n_",
      values_from = c(n)
    ),
  T1_pct,
  by = c("variable", "value")
) %>%
  arrange(
    match(
      variable,
      c(
        "sex", "educational_level", "country_of_birth", "kids",
        "infected", "contact", "risk", "works_in_care",
        "serious"
      )
    ), value
  ) %>%
  as.data.frame() %>%
  select(
    variable, value,
    n_1, pct_1, n_2, pct_2
  ) %>%
  print(); rm(T1_n, T1_pct)



T1_continuous <- dataset %>%
  select(group, Leeftijd) %>%
  mutate(age = Leeftijd) %>%
  select(group, age) %>%
  group_by(group) %>%
  summarise(
    across(
      everything(),
      list(
        n = ~ sum(!is.na(.)),
        m = ~ round(mean(., na.rm = TRUE), 2),
        sd = ~ round(sd(., na.rm = TRUE), 2),
        mdn = ~ median(., na.rm = TRUE),
        iqr25 = ~ quantile(., na.rm = TRUE, names = FALSE, probs = 0.25),
        iqr75 = ~ quantile(., na.rm = TRUE, names = FALSE, probs = 0.75)
      )
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(everything(), -group)
  ) %>%
  separate(
    col = name,
    into = c("variable", "statistic")
  ) %>%
  pivot_wider(
    names_from = "statistic"
  ) %>%
  as.data.frame() %>%
  print()



# independent t-test age
T1_ttest <- dataset %>%
  select(group, Leeftijd) %>%
  mutate(age = Leeftijd) %>%
  t.test(
    formula = age ~ group,
    paired = FALSE,
    var.equal = TRUE,
    data = .
  ) %>%
  tidy(.) %>%
  as.data.frame() %>%
  print()



# Pearson's chi-squared test sex
chisq_sex <- T1_categorical %>%
  filter(variable == "sex") %>%
  filter(value == 1 | value == 2) %>%
  select(value, n_1, n_2) %>%
  column_to_rownames("value") %>%
  chisq.test(., correct = TRUE) %>%
  print()



# Pearson's chi-squared test country of birth
chisq_country_of_birth <- T1_categorical %>%
  filter(variable == "country_of_birth") %>%
  select(value, n_1, n_2) %>%
  column_to_rownames("value") %>%
  chisq.test(., correct = TRUE) %>%
  print()



# Pearson's chi-squared test kids
chisq_kids <- T1_categorical %>%
  filter(variable == "kids", !is.na(value)) %>%
  select(value, n_1, n_2) %>%
  column_to_rownames("value") %>%
  chisq.test(., correct = TRUE) %>%
  print()


# Pearson's chi-squared test infected # DISCUSS
chisq_infected <- T1_categorical %>%
  filter(variable == "infected") %>%
  select(value, n_1, n_2) %>%
  column_to_rownames("value") %>%
  chisq.test(., simulate.p.value = TRUE) %>%
  print()



# Pearson's chi-squared test contact
chisq_contact <- T1_categorical %>%
  filter(variable == "contact") %>%
  select(value, n_1, n_2) %>%
  column_to_rownames("value") %>%
  chisq.test(., correct = TRUE) %>%
  print()



# Pearson's chi-squared test risk
chisq_risk <- T1_categorical %>%
  filter(variable == "risk") %>%
  select(value, n_1, n_2) %>%
  column_to_rownames("value") %>%
  chisq.test(., correct = TRUE) %>%
  print()



# Pearson's chi-squared test works in care
chisq_works_in_care <- T1_categorical %>%
  filter(variable == "works_in_care") %>%
  select(value, n_1, n_2) %>%
  column_to_rownames("value") %>%
  chisq.test(., correct = TRUE) %>%
  print()



T1_chisq_list <- list(
  sex = chisq_sex,
  country_of_birth = chisq_country_of_birth,
  kids = chisq_kids,
  infected = chisq_infected,
  contact = chisq_contact,
  risk = chisq_risk,
  works_in_care = chisq_works_in_care
); rm(
  chisq_sex, chisq_country_of_birth, chisq_kids, chisq_infected, chisq_contact,
  chisq_risk, chisq_works_in_care
)



T1_chisq <- T1_chisq_list %>%
  purrr::map_df(
    .,
    broom::tidy,
    .id = "variable"
  ) %>%
  mutate(
    statistic = round(statistic, 3),
    p.value = round(p.value, 3)
  ) %>%
  as.data.frame() %>%
  print()



# linear-by-linear association test educational level
lbl_educational_level <- dataset %>%
  mutate(educational_level = Opleiding_recoded) %>%
  select(educational_level, group) %>%
  table() %>%
  coin::lbl_test(.) %>%
  print()



# linear-by-linear association test seriousness
lbl_serious <- dataset %>%
  mutate(serious = Serieus) %>%
  select(serious, group) %>%
  table() %>%
  coin::lbl_test(.) %>%
  print()



# Wilcoxon rank sum test level of education
wilcox_education <- wilcox.test(
    formula = Opleiding_recoded ~ group,
    data = dataset
  )



# Wilcoxon rank sum test serious
wilcox_serious <- wilcox.test(
  formula = Serieus ~ group,
  data = dataset
)



# Tidy Wilcox output
T1_wilcox_list <- list(
  education = wilcox_education,
  serious = wilcox_serious
)



T1_wilcox <- T1_wilcox_list %>%
  purrr::map_df(
    .,
    broom::tidy,
    .id = "variable"
  ) %>%
  mutate(
    statistic = round(statistic, 3),
    p.value = round(p.value, 3)
  ) %>%
  as.data.frame() %>%
  print()



# 2. Internal consistency constructs -------------------------------------------

# self-efficacy T0
alpha_SE_0 <- dataset %>%
  select(SE_1_0, SE_2_0, SE_3_0, SE_4_0, SE_5_0, SE_6_0, SE_7_0) %>%
  psych::alpha()



# intention T0
alpha_INT_0 <- dataset %>%
  select(INT_1_0, INT_2_0, INT_3_0, INT_4_0, INT_5_0, INT_6_0, INT_7_0) %>%
  psych::alpha()



# perceived risk (self) T0
spearman_PR_0 <- with(
  data = dataset,
  expr = spearmanCI::spearmanCI(
    x = Kans_ziek,
    y = Kans_besmet,
    method = "empirical"
    )
  )



# perceived vulnerability (others) T0
alpha_PV_0 <- dataset %>%
  select(kwets_groepen_1, kwets_groepen_2, kwets_groepen_3, kwets_groepen_4, kwets_groepen_5) %>%
  psych::alpha()



# response efficacy (self) T0
spearman_RE_0 <- with(
  data = dataset,
  expr = spearmanCI::spearmanCI(
    x = RespEff_1,
    y = RespEff_2,
    method = "empirical"
    )
  )



# feelings T0
alpha_feelings_0 <- dataset %>%
  select(Gev_sympathie, Gev_medeleven, Gev_betrokkenheid, Gev_begrip, Gev_warmte) %>%
  psych::alpha()



# statements T0
alpha_statements_0 <- dataset %>%
  select(eigen_schuld_R, kwets_beschermen, belang_voorkomen, bereid_beschermen) %>%
  psych::alpha()


# statements T0 (2)
alpha_statements_0_2 <- dataset %>%
  select(kwets_beschermen, belang_voorkomen, bereid_beschermen) %>%
  psych::alpha()



# behaviour index T1
alpha_behaviour_1 <- dataset %>%
  select(Gedrag_2_1, Gedrag_2_2, Gedrag_2_3, Gedrag_2_4, Gedrag_2_5, Gedrag_2_6, Gedrag_2_7) %>%
  psych::alpha()



# self efficacy T1
alpha_SE_1 <- dataset %>%
  select(SE_1_1, SE_2_1, SE_3_1, SE_4_1, SE_5_1, SE_6_1, SE_7_1) %>%
  psych::alpha()



# 3. Descriptive statistics constructs -----------------------------------------
descriptive_statistics <- dataset %>%
  mutate(
    PS_0 = Ernst_besmet,
    PC_0 = gepercipieerde_compliance
  ) %>%
  select(
    group,
    SE_0,
    INT_0,
    KANS_0,
    PS_0,
    VUL_GRP_0,
    PC_0,
    RE_0,
    BEHAVIOUR_1,
    SE_1
  ) %>%
  rename_all(. %>% gsub("_", ".", .)) %>%
  group_by(group) %>%
  summarise(
    across(
      everything(),
      list(
        n = ~ round(sum(!is.na(.)), 2),
        mean = ~ round(mean(., na.rm = TRUE), 2),
        sd = ~ round(sd(., na.rm = TRUE), 2),
        median = ~ round(median(., na.rm = TRUE), 2),
        iqr25 = ~ round(quantile(x = ., names = FALSE, na.rm = TRUE, probs = 0.25), 2),
        iqr75 = ~ round(quantile(x = ., names = FALSE, na.rm = TRUE, probs = 0.75), 2)
      )
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(everything(), -group)
  ) %>%
  separate(
    col = name,
    into = c("variable", "statistic"),
    sep = "_"
  ) %>%
  pivot_wider(
    names_from = c("statistic", "group")
  ) %>%
  as.data.frame() %>%
  print()



# 4. Boxplots of outcomes ------------------------------------------------------
boxplots <- dataset %>%
  select(group, SE_0, SE_1) %>%
  pivot_longer(
    .,
    cols = c(SE_0, SE_1)
    )

bp <- ggplot2::ggplot(
  boxplots,
  aes(
    x = name,
    y = value,
    fill = group
    )
  ) + geom_boxplot()



# 5. Regression models ---------------------------------------------------------

# Function for calculating Cohen's F^2
# From Statistical power analysis for behavioural sciences by Jacob Cohen, 3rd
# edition, chapter 9. p.410
CohenF2 <- function(R2ab, R2a) {
  f2 <- (R2ab - R2a)/(1 - R2ab)
  return(f2)
}

CohenF2(
  R2ab = .45,
  R2a = .30
) # .2727

reg_dataset <-
  dataset %>%
  mutate(lft_gr = ifelse(Leeftijd <= 38, 0, 1))

# self efficacy T0
m1_SE_0 <- lm(
  formula = SE_0 ~ as.factor(group) + as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

m2_SE_0 <- lm(
  formula = SE_0 ~ as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

qqnorm(m1_SE_0$residuals)
qqline(m1_SE_0$residuals)



# intention T0
m1_INT_0 <- lm(
  formula = INT_0 ~ as.factor(group) + as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

m2_INT_0 <- lm(
  formula = INT_0 ~ as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

qqnorm(m1_INT_0$residuals)
qqline(m1_INT_0$residuals)



# perceived risk (self) T0
m1_KANS_0 <- lm(
  formula = KANS_0 ~ as.factor(group) + as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

m2_KANS_0 <- lm(
  formula = KANS_0 ~ as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

qqnorm(m1_KANS_0$residuals)
qqline(m1_KANS_0$residuals)



# perceived severity T0
m1_PS_0 <- lm(
  formula = Ernst_besmet ~ as.factor(group) + as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

m2_PS_0 <- lm(
  formula = Ernst_besmet ~ as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

qqnorm(m1_PS_0$residuals)
qqline(m1_PS_0$residuals)



# perceived vulnerability (others) T0
m1_VUL_GRP_0 <- lm(
  formula = VUL_GRP_0 ~ as.factor(group) + as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

m2_VUL_GRP_0 <- lm(
  formula = VUL_GRP_0 ~ as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

qqnorm(m1_VUL_GRP_0$residuals)
qqline(m1_VUL_GRP_0$residuals)



# perceived compliance T0
m1_PC_0 <- lm(
  formula = gepercipieerde_compliance ~ as.factor(group) + as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

m2_PC_0 <- lm(
  formula = gepercipieerde_compliance ~ as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

qqnorm(m1_PC_0$residuals)
qqline(m1_PC_0$residuals)



# response efficacy T0
m1_RE_0 <- lm(
  formula = RE_0 ~ as.factor(group) + as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

m2_RE_0 <- lm(
  formula = RE_0 ~ as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

qqnorm(m1_RE_0$residuals)
qqline(m1_RE_0$residuals)



# behaviour index T1
m1_BEHAVIOUR_1 <- lm(
  formula = BEHAVIOUR_1 ~ as.factor(group) + as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

m2_BEHAVIOUR_1 <- lm(
  formula = BEHAVIOUR_1 ~ as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

qqnorm(m1_BEHAVIOUR_1$residuals)
qqline(m1_BEHAVIOUR_1$residuals)



# self efficacy T1
m1_SE_1 <- lm(
  formula = SE_1 ~ as.factor(group) + as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

m2_SE_1 <- lm(
  formula = SE_1 ~ as.factor(Geslacht_recoded) + Leeftijd + as.factor(Opleiding_recoded),
  data = dataset
)

qqnorm(m1_SE_1$residuals)
qqline(m1_SE_1$residuals)


## additional interactie leeftijd
m1_INT_0_lftint <- lm(
  formula = INT_0 ~ as.factor(group)*lft_gr + as.factor(Geslacht_recoded) + as.factor(Opleiding_recoded),
  data = reg_dataset
)

m1_BEHAVIOUR_1_lftint <- lm(
  formula = BEHAVIOUR_1 ~ as.factor(group)*lft_gr + as.factor(Geslacht_recoded) + as.factor(Opleiding_recoded),
  data = reg_dataset
)

m1_SE_1_lftint <- lm(
  formula = SE_1 ~ as.factor(group)*lft_gr + as.factor(Geslacht_recoded) + as.factor(Opleiding_recoded),
  data = reg_dataset
)

# tidy regression output
regression_models <- list(
  SE_0_AB = m1_SE_0, SE_0_A = m2_SE_0,
  INT_0_AB = m1_INT_0, INT_0_A = m2_INT_0,
  KANS_0_AB = m1_KANS_0, KANS_0_A = m2_KANS_0,
  PS_0_AB = m1_PS_0, PS_0_A = m2_PS_0,
  PV_0_AB = m1_VUL_GRP_0, PV_0_A = m2_VUL_GRP_0,
  PC_0_AB = m1_PC_0, PC_0_A = m2_PC_0,
  RE_0_AB = m1_RE_0, RE_0_A = m2_RE_0,
  BEHAVIOUR_1_AB = m1_BEHAVIOUR_1, BEHAVIOUR_1_A = m2_BEHAVIOUR_1,
  SE_1_AB = m1_SE_1, SE_1_A = m2_SE_1
); rm(
  m1_SE_0, m2_SE_0,  m1_INT_0, m2_INT_0, m1_KANS_0, m2_KANS_0, m1_PS_0, m2_PS_0,
  m1_VUL_GRP_0, m2_VUL_GRP_0, m1_PC_0, m2_PC_0, m1_RE_0, m2_RE_0,
  m1_BEHAVIOUR_1, m2_BEHAVIOUR_1, m1_SE_1, m2_SE_1
)


regr_models_lft <-
  list(intentie_T0 = m1_INT_0_lftint,
       behavior_T1 = m1_BEHAVIOUR_1_lftint,
       selfeffi_T1 = m1_SE_1_lftint
  )

regression_output <- regression_models %>%
  purrr::map_df(
    .,
    broom::tidy,
    .id = "model"
  ) %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2),
    p.value = round(p.value, 2)
  ) %>%
  separate(
    col = "model",
    into = c("model", "R^2"),
    sep = "_(?=[^_]+$)"
  ) %>%
  filter(`R^2` == "AB") %>%
  select(-"R^2") %>%
  as.data.frame() %>%
  print()



regression_output_f2 <- regression_models %>%
  purrr::map_df(
    .,
    broom::glance,
    .id = "model"
  ) %>%
  separate(
    col = model,
    into = c("model", "R^2"),
    sep = "_(?=[^_]+$)"
  ) %>%
  group_by(model) %>%
  arrange("model", "R^2") %>%
  mutate(`f^2` = (r.squared - lead(r.squared))/(1- r.squared)) %>%
  select(model, "R^2", r.squared, "f^2") %>%
  mutate(
    r.squared = round(r.squared, 3),
    `f^2` = round(`f^2`, 3)
  ) %>%
  as.data.frame() %>%
  print()

regression_lftint <- regr_models_lft %>%
  purrr::map_df(
    .,
    broom::tidy,
    .id = "model"
  ) %>%
  mutate(
    estimate = round(estimate, 2),
    std.error = round(std.error, 2),
    statistic = round(statistic, 2),
    p.value = round(p.value, 2)
  )


# 6. Frequencies preventive behaviour ------------------------------------------

# n
behaviour_n <- dataset %>%
  select(
    group,
    Gedrag, Gedrag_2_1, Gedrag_2_2, Gedrag_2_3, Gedrag_2_4, Gedrag_2_5,
    Gedrag_2_6, Gedrag_2_7
  ) %>%
  pivot_longer(
    cols = c(everything(), - group),
    names_to = "variable"
  )%>%
  group_by(group, variable, value) %>%
  summarise(
    n = n(),
    .groups = "drop"
  )



# pct
behaviour_pct <- behaviour_n %>%
  filter(!is.na(value)) %>%
  group_by(group, variable) %>%
  mutate(
    pct = n / sum(n) * 100,
    pct = round(pct, 2)
  ) %>%
  select(group, variable, value, n, pct) %>%
  pivot_wider(
    .,
    names_from = group,
    values_from = c(n, pct)
  ) %>%
  select(
    variable, value, pct_1, pct_2
  ) %>%
  print()



behaviour_freq <- full_join(
  behaviour_n %>%
    pivot_wider(
      .,
      names_from = group,
      names_prefix = "n_",
      values_from = c(n)
    ),
  behaviour_pct,
  by = c("variable", "value")
) %>%
  select(variable, value, n_1, pct_1, n_2, pct_2) %>%
  as.data.frame() %>%
  print()



item_summary_behaviour <- dataset %>%
  select(
    group,
    Gedrag_2_1, Gedrag_2_2, Gedrag_2_3, Gedrag_2_4, Gedrag_2_5, Gedrag_2_6,
    Gedrag_2_7
  ) %>%
  rename_all(. %>% gsub("_", ".", .)) %>%
  group_by(group) %>%
  summarise(
    across(
      everything(),
      list(
        mean = ~ round(mean(., na.rm = TRUE), 2),
        sd = ~ round(sd(., na.rm = TRUE), 2),
        mdn = ~ round(median(., na.rm = TRUE), 2),
        iqr25 = ~ round(quantile(., na.rm = TRUE, probs = 0.25), 2),
        iqr75 = ~ round(quantile(., na.rm = TRUE, probs = 0.75), 2)
      )
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    .,
    cols = c(everything(), - group)
  ) %>%
  as.data.frame() %>%
  separate(
    col = name,
    into = c("variable", "statistic"),
    sep = "_"
  ) %>%
  pivot_wider(
    names_from = c("statistic", "group"),
    values_from = "value"
  ) %>%
  as.data.frame() %>%
  print()



# 7. Wilcoxon preventive Behaviour ---------------------------------------------

# 1. Wash my hands regularly (20 sec.) with soap and water
wilcox_behaviour_1 <- wilcox.test(
  formula = Gedrag_2_1 ~ group,
  data = dataset,
  estimate = TRUE,
  conf.int = TRUE
) %>% print()



# 2. Always sneezing or coughing in my elbow
wilcox_behaviour_2 <- wilcox.test(
  formula = Gedrag_2_2 ~ group,
  data = dataset,
  estimate = TRUE,
  conf.int = TRUE
) %>% print()



# 3. Always keep 1.5 meters away from other people
wilcox_behaviour_3 <- wilcox.test(
  formula = Gedrag_2_3 ~ group,
  data = dataset,
  estimate = TRUE,
  conf.int = TRUE
) %>% print()



# 4. Avoid people who are vulnerable
wilcox_behaviour_4 <- wilcox.test(
  formula = Gedrag_2_4 ~ group,
  data = dataset,
  estimate = TRUE,
  conf.int = TRUE
) %>% print()



# 5. Work from home as much as possible
wilcox_behaviour_5 <- wilcox.test(
  formula = Gedrag_2_5 ~ group,
  data = dataset,
  estimate = TRUE,
  conf.int = TRUE
) %>% print()



# 6. Avoid crowds
wilcox_behaviour_6 <- wilcox.test(
  formula = Gedrag_2_6 ~ group,
  data = dataset,
  estimate = TRUE,
  conf.int = TRUE
) %>% print()



# 7. Use paper tissues
wilcox_behaviour_7 <- wilcox.test(
  formula = Gedrag_2_7 ~ group,
  data = dataset,
  estimate = TRUE,
  conf.int = TRUE
) %>% print()



# tidy wilcox output
wilcox_behaviour <- list(
  behaviour_1 = wilcox_behaviour_1, behaviour_2 = wilcox_behaviour_2,
  behaviour_3 = wilcox_behaviour_3, behaviour_4 = wilcox_behaviour_4,
  behaviour_5 = wilcox_behaviour_5, behaviour_6 = wilcox_behaviour_6,
  behaviour_7 = wilcox_behaviour_7
); rm(
  wilcox_behaviour_1, wilcox_behaviour_2, wilcox_behaviour_3,
  wilcox_behaviour_4, wilcox_behaviour_5, wilcox_behaviour_6,
  wilcox_behaviour_7
)



wilcox_output <-  wilcox_behaviour %>%
  purrr::map_df(
    .,
    broom::tidy,
    .id = "variable"
  ) %>%
  mutate(
    p.value = round(p.value, 2)
  ) %>%
  mutate(
    statistic = round(statistic, 4),
    p.value = round(p.value, 4)
  ) %>%
  as.data.frame() %>%
  print()



# 8. Frequencies SI -----------------------------------------------------------

# n
SI_n <- dataset %>%
  mutate(
    bracelet = kleinigheid,
    bracelet_received = kleinigheid_ontvangen,
    bracelet_worn = kleinigheid_gedragen,
    feelings_sympathy = Gev_sympathie,
    feelings_compassion = Gev_sympathie,
    feelings_involvement = Gev_betrokkenheid,
    feelings_understanding = Gev_begrip,
    feelings_warmth = Gev_warmte,
    statement_fault = eigen_schuld_R,
    statement_protect_vulnerable = kwets_beschermen,
    statement_prevention_important = belang_voorkomen,
    statement_willingness_protect = bereid_beschermen
    ) %>%
  select(
    bracelet, bracelet_received, bracelet_worn,
    feelings_sympathy, feelings_compassion, feelings_involvement,
    feelings_understanding, feelings_warmth,
    statement_fault, statement_protect_vulnerable,
    statement_prevention_important, statement_willingness_protect
  ) %>%
  pivot_longer(
    .,
    cols = everything(),
    names_to = "variable"
  ) %>%
  group_by(variable, value) %>%
  summarise(
    across(
      everything()
    ),
    n = n(),
    .groups = "drop"
  )



SI_pct = SI_n %>%
  filter(!is.na(value)) %>%
  group_by(variable) %>%
  mutate(
    pct = n / sum(n) * 100,
    pct = round(pct, 3)
  )



SI_categorical <- full_join(
  SI_n,
  SI_pct,
  by = c("variable", "value", "n")
) %>%
  as.data.frame() %>%
  print()



# Feelings & statements
descriptive_statistics_SI <- dataset %>%
  select(FEELINGS_0, STATEMENTS_0, STATEMENTS_0_2) %>%
  rename_all(~ gsub("_", " ", .)) %>%
  summarise(
    across(
      everything(),
      list(
        n = ~ sum(!is.na(.)),
        m = ~ round(mean(., na.rm = TRUE)),
        sd = ~ round(sd(., na.rm = TRUE)),
        mdn = ~ round(median(., na.rm = TRUE), 3),
        irq25 = ~ quantile(., na.rm = TRUE, names = FALSE, probs = 0.25),
        iqr75 = ~ quantile(., na.rm = TRUE, names = FALSE, probs = 0.75)
      )
    ),
    .groups = "drop"
  ) %>%
  pivot_longer(
    .,
    cols = everything()
  ) %>%
  separate(
    col = name,
    into = c("variable", "statistic"),
    sep = "_"
  ) %>%
  pivot_wider(
    names_from = statistic,
    values_from = value
  ) %>%
  as.data.frame() %>%
  print()
