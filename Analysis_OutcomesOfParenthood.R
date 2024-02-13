# Project Name: Outcomes of Parenthood
# Author: Bruno Alves de Carvalho
# Status: ongoing
# Note: split data processing and data analysis into 2 separate scripts


# Set up ------------------------------------------------------------------

# Set the directory
setwd("/Users/brunoalvesdecarvalho/Desktop/DataWarehouse_20231015_ve01")

# Loading packages for data processing
library(tidyverse)
library(memoise)
library(haven)

# Loading packages for data analysis
library(tidyverse)
library(tidymodels)
library(rsample)
library(workflowsets)

# Load functions
source("R_Scripts/FunctionRepository_20231016_ve01.R")

# Load color palette
source("R_Scripts/ColorPalette_20240128_ve01.R")

# Load data
aggregated_data_shp <- 
  readRDS("SHP/Data_Aggregated_1999_2022/cached_data_shp.rds")



# Collect data ------------------------------------------------------------

# Select variables necessary for analysis
selected_vars_shp <- 
  rep(
    list(
      c(
        # id variables
        "idpers",
        "idhous$$",
        # time variable
        "year",
        # social variables
        "age$$",
        "generation",
        "sex$$",
        "civsta$$", 
        "educat$$", # highest level of education achieved, 11 grid
        "edugr$$", # highest level of education achieved, 19 grid
        "edyear$$", # number of years in education
        "p$$e15", # current education, 19 grid
        # economic variables
        "iptotni", # total income
        "iwyni", # working income
        # geographical variables
        "canton$$",
        "com2_$$",
        # political variables
        "p$$p10", # political position, scale 1 (left) to 10 (right)
        "p$$p11", # member of political party
        "p$$p19", # party of choice if elections tomorrow
        "p$$p46", # political position of father
        "p$$p47", # political position of mother
        "p$$p67", # party identification
        # other
        "p$$c06", # sleeping problems, last 12 months, binary yes / no
        "p$$c06a", # sleeping problems, last 4 weeks, scale 1 to 3
        "p$$f04", # satisfaction with way housework is shared, scale 0 to 10 (completely satisfied)
        "p$$n04", # practical support from partner, scale 0 to 10 (a great deal)
        "p$$n14", # practical support from relatives
        "p$$n21", # practical support from neighbors
        "p$$n28", # practical support from close friends
        "p$$n32", # practical support from colleagues
        "h$$f53", # child care in case of illness (10 categories)
        "p$$f50", # interference of work in family obligations, scale 0 to 10 (very strongly)
        
        # independent variable
        "ownkid$$", # number of children born
        "nbb_$$", # new baby, binary yes / no
        "nbkid$$", # number of children in household (0 to 17 years old)
        "nbpers$$", # number of persons in household
        
        # dependent variable
        "p$$f54", # Happy with the partner, scale 0 to 10 (very happy)
        "p$$c44", # satisfaction with life in general, scale 0 to 10 (completely satisfied)
        "p$$c47", # frequency of moments of joy, scale 0 to 10 (always)
        "p$$c48", # frequency of moments of anger
        "p$$c49", # frequency of moments of sadness
        "p$$c50", # frequency of moments of worry
        "p$$c18", # frequency of energy and optimism
        "h$$h01"# improvement or deterioration of standard of living, scale 0 to 10 (greatly improved)
      )
    ),
    length(1999:2022)
  )

# Merge data into one single data-set
merged_data_shp <-
  shp_merge_data(aggregated_data_shp, selected_vars_shp)


# Transform data ----------------------------------------------------------

# Remove symbols from field names
colnames(merged_data_shp)[str_detect(colnames(merged_data_shp), "\\$$")] <-
  str_replace_all(colnames(merged_data_shp)[str_detect(colnames(merged_data_shp), "\\$$")], "\\$\\$", "")

# Create factors and other categorical data
merged_data_shp$sex_fct <- 
  factor(merged_data_shp$sex, levels = c(1,2,3), labels = c("man", "woman", "other"))

merged_data_shp$polideology <- 
  ifelse(
    merged_data_shp$`p$$p10` > 5, "right_wing", 
    ifelse(
      merged_data_shp$`p$$p10` < 5 & merged_data_shp$`p$$p10` >= 0, "left_wing", 
      ifelse(
        merged_data_shp$`p$$p10` == 5, "Neutral", NA)
    )
  )

merged_data_shp$canton <- 
  factor(merged_data_shp$canton, 
         levels = c(1:26), 
         labels = c("Argovia", "Appenzell Inner-Rhodes", "Appenzell Outer-Rhodes", 
                    "Berne", "Basle-Town", "Basle-Country", "Fribourg", "Geneva", 
                    "Glarus", "Grisons", "Jura", "Lucerne", "Neuchatel", "Nidwalden", 
                    "Obwalden", "St. Gall", "Schaffhausen", "Solothurn", "Schwyz", 
                    "Thurgovia", "Ticino", "Uri", "Vaud", "Valais", "Zug", "Zurich"))

merged_data_shp$langregion <- 
  fct_collapse(merged_data_shp$canton, 
               "German-Speaking Cantons" = c("Argovia", "Appenzell Inner-Rhodes", "Appenzell Outer-Rhodes", 
                                             "Basle-Town", "Basle-Country", "Berne", "Glarus", "Lucerne", 
                                             "Nidwalden", "Obwalden", "St. Gall", "Schaffhausen", "Schwyz", 
                                             "Thurgovia", "Uri", "Zug", "Zurich", "Solothurn"), 
               "French-Speaking Cantons" = c("Vaud", "Geneva", "Jura", "Neuchatel"), 
               "Italian-Speaking Cantons" = c("Ticino"), 
               "Bilingual Cantons" = c("Fribourg", "Valais", "Grisons"))

transform_scales <- function(field) {
  ifelse(field < 0, NA, field)
}

tab_family <-
  merged_data_shp %>% 
  mutate(
    sleep_problems = 
      ifelse(
        `p$$c06` == 1 & year %in% c(1999:2004), 1, 
        ifelse(
          `p$$c06` == 2 & year %in% c(1999:2004), 0, 
          ifelse(
            `p$$c06` < 1 & year %in% c(1999:2004), NA, 
            ifelse(
              `p$$c06a` > 1, 1, 
              ifelse(
                `p$$c06a` == 1, 0, NA)
              )
            )
          )
        ) %>% 
      factor(levels = c(0,1), labels = c("no", "yes")),
    marriage =
      ifelse(
        civsta == 2, 1, 
        ifelse(
          civsta < 1, NA, 0
          )
        ) %>% 
      factor(levels = c(0,1), labels = c("not_married", "married")),
    separation = 
      ifelse(
        civsta %in% c(3,4,7), 1, 
        ifelse(
          civsta < 1, NA, 0
          )
        ) %>% 
      factor(levels = c(0,1), labels = c("not_separated", "separated")),
    income_level = 
      ifelse(
        iptotni > median(iptotni, na.rm = TRUE), "above", "below"
      ) %>% 
      as.factor(),
    partner_support = 
      ifelse(`p$$n04` < 0, NA, `p$$n04`) %>% 
      as_factor() %>% 
      fct_collapse(
        low = c(0,1,2,3,4,5,6), 
        moderate = c(7,8), 
        high = c(9,10)),
    relatives_support = 
      ifelse(`p$$n14` < 0, NA, `p$$n14`) %>% 
      as_factor() %>% 
      fct_collapse(
        low = c(0,1,2,3,4), 
        moderate = c(5,6,7), 
        high = c(8,9,10)),
    housework_satisfaction =  
      ifelse(`p$$f04` < 0, NA, `p$$f04`) %>% 
      as_factor() %>% 
      fct_collapse(
        low = c(0,1,2,3,4,5,6), 
        moderate = c(7,8), 
        high = c(9,10)), 
    across(
      c(
        "p$$f54",
        "p$$c44",
        "edyear"
        ),
      transform_scales
      )
    ) %>% 
  rename(
    nbb = nbb_,
    commune_type = com2_, 
    hpyprtnr_depvar = `p$$f54`, 
    lifesat_depvar = `p$$c44`
    ) %>% 
  select(
    -matches("\\$"), 
    -starts_with("edu"), 
    -ends_with("i"), 
    -all_of(c("generation", "canton", "civsta", "sex"))
    )

# Correct data errors
tab_family[which(tab_family$idpers == 63186101 & tab_family$sex_fct == "woman"),"sex_fct"] <- 
  "man"
tab_family[which(tab_family$idpers == 63186102 & tab_family$age == 24), c("ownkid")] <- 
  1

# Modify table and create variables to enable analysis
tab_family <- 
  tab_family %>% 
  # Filter for individuals with at least 3 yrs of observation
  left_join(
    x = tab_family %>% group_by(idpers) %>% summarise(n = n()) %>% filter(n >= 3), 
    by = "idpers", relationship = "one-to-many") %>% 
  mutate(ownkid_new = ifelse(ownkid < 0, NA, ownkid)) %>% 
  # Group by individual ids
  group_by(idpers) %>%
  # Push the last number of kids forward if value is missing
  fill(ownkid_new, .direction = "down") %>% 
  # Filter for the remaining missing values in number of children
  filter(!is.na(ownkid_new)) %>% 
  # Flag moment when the individual has their first child
  mutate(
    row_id = row_number(),
    cumsum = cumsum(ownkid_new), 
    first_child = ifelse(cumsum == 1 & nbb == 1, 1, 0),
    # Flag period when the individual is a parent
    parenthood = ifelse(cumsum >= 1, 1, 0) %>% factor(levels = c(0,1), labels = c("no_children", "has_children")),
    # Identify year of first child
    year_of_first_child = ifelse(first_child == 1, year, NA)
  ) %>% 
  fill(year_of_first_child, .direction = "updown") %>% 
  # Identify number of years since and before first child
  mutate(years_since_first_child = year - year_of_first_child) %>% 
  ungroup()
tab_family <-
  tab_family %>% 
  # Identify if the individual is a parent or non-parent
  left_join(
    tab_family %>% group_by(idpers) %>% summarise(sum_nbkids = sum(ifelse(ownkid_new < 0, 0, ownkid), na.rm = T)), relationship = "many-to-one") %>% 
  mutate(parent = ifelse(sum_nbkids > 0, "parent", "non_parent")) %>% 
  select(
    -starts_with("nb"),
    -cumsum,
    -sum_nbkids,
    -ownkid,
    -row_id,
    -year_of_first_child,
    -parent,
    -n
    ) %>% 
  select(
    idpers, idhous, 
    year, age, 
    sex_fct, edyear, income_level, polideology,
    langregion, commune_type, 
    marriage, separation, 
    parenthood, first_child, years_since_first_child, ownkid_new,
    ends_with("support"), sleep_problems, housework_satisfaction,
    ends_with("depvar")
    )


# Preliminary Exploratory Data Analysis -----------------------------------

# plot age distribution of parents and non-parents, by sex
tab_family %>% 
  filter(sex_fct != "other") %>% 
  ggplot() + 
  geom_histogram(aes(x = age)) + 
  facet_wrap(~ parenthood + sex_fct)

# plot age distribution of mother and father when they have their first child
tab_family %>% 
  filter(first_child == 1) %>% 
  ggplot() + 
  geom_histogram(aes(x = age)) +
  facet_wrap(~ sex_fct)

# summary statistics of independent variables
arsenal::tableby(
  ~ partner_support + relatives_support + housework_satisfaction, 
  data = tab_family, 
  control = 
    arsenal::tableby.control(
      total = T, 
      numeric.stats = c("Nmiss", "meansd", "range", "medianq1q3"))) %>% 
  summary()


# Happiness with Partner: Exploratory Data Analysis -----------------------

# plot distribution of happiness with partner
tab_family %>% 
  group_by(idpers) %>%
  summarise(mean = mean(hpyprtnr_depvar, na.rm = T)) %>% 
  ggplot() + 
  geom_histogram(aes(x = mean))

# plot of happiness with partner over age, by parenthood
tab_family %>% 
  mutate(hpyprtnr_depvar = ifelse(hpyprtnr_depvar < 0, NA, hpyprtnr_depvar)) %>% 
  group_by(parenthood, age) %>% 
  summarise(
    n = n(), 
    mean_hpyprtnr = mean(hpyprtnr_depvar, na.rm = T), 
    n_non_missing_hpyprtnr = sum(!is.na(hpyprtnr_depvar))
  ) %>% 
  ggplot(aes(x = age, y = mean_hpyprtnr, color = parenthood)) + 
  geom_point() + 
  geom_smooth(span = 0.75) +
  scale_y_continuous(limits = c(7.5, 10)) +
  scale_x_continuous(limits = c(25, 85))

# plot of happiness with partner over age, by parenthood and sleep problems
tab_family %>% 
  mutate(hpyprtnr_depvar = ifelse(hpyprtnr_depvar < 0, NA, hpyprtnr_depvar)) %>% 
  group_by(parenthood, sleep_problems, age) %>% 
  summarise(
    n = n(), 
    mean_hpyprtnr = mean(hpyprtnr_depvar, na.rm = T), 
    n_non_missing_hpyprtnr = sum(!is.na(hpyprtnr_depvar))
  ) %>% filter(!is.na(sleep_problems)) %>% 
  ggplot(aes(x = age, y = mean_hpyprtnr, color = parenthood)) + 
  geom_point() + 
  geom_smooth(span = 0.75) + facet_wrap(~ sleep_problems) +
  scale_y_continuous(limits = c(7.5, 10)) +
  scale_x_continuous(limits = c(25, 85))

# plot of happiness with partner over age, by parenthood and housework
tab_family %>% 
  group_by(parenthood, housework_satisfaction, age) %>% 
  summarise(
    n = n(), 
    mean_hpyprtnr = mean(hpyprtnr_depvar, na.rm = T), 
    n_non_missing_hpyprtnr = sum(!is.na(hpyprtnr_depvar))
  ) %>% 
  filter(!is.na(housework_satisfaction)) %>% 
  ggplot(aes(x = age, y = mean_hpyprtnr, color = parenthood)) + 
  geom_point() + 
  geom_smooth(span = 0.75, se = F) + facet_wrap(~ housework_satisfaction) +
  scale_y_continuous(limits = c(5, 10)) +
  scale_x_continuous(limits = c(25, 80))

# plot of happiness with partner over age, by parenthood and partner support
tab_family %>% 
  group_by(parenthood, partner_support, age) %>% 
  summarise(
    n = n(), 
    mean_hpyprtnr = mean(hpyprtnr_depvar, na.rm = T), 
    n_non_missing_hpyprtnr = sum(!is.na(hpyprtnr_depvar))
  ) %>% 
  filter(!is.na(partner_support)) %>% 
  ggplot(aes(x = age, y = mean_hpyprtnr, color = parenthood)) + 
  geom_point() + 
  geom_smooth(span = 0.75, se = F) + facet_wrap(~ partner_support) +
  scale_y_continuous(limits = c(5, 10)) +
  scale_x_continuous(limits = c(25, 80))

# plot happiness with partner over years before and since first child
tab_family %>% 
  filter(sex_fct != "other") %>% 
  mutate(hpyprtnr_depvar = ifelse(hpyprtnr_depvar < 0, NA, hpyprtnr_depvar)) %>% 
  group_by(years_since_first_child, sex_fct) %>% 
  summarise(
    n = n(), 
    mean_hpyprtnr = mean(hpyprtnr_depvar, na.rm = T), 
    n_non_missing_hpyprtnr = sum(!is.na(hpyprtnr_depvar))) %>% 
  ggplot(aes(x = years_since_first_child, y = mean_hpyprtnr, color = sex_fct)) + 
  geom_vline(xintercept = 0, linewidth = 0.5) + 
  geom_point() + 
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(7.5, 10)) + 
  scale_x_continuous(limits = c(-5, 9))


# Happiness with Partner: Multiple Linear Regression ----------------------

# split the data into 10 folds
folds <- 
  vfold_cv(
    tab_family, 
    v = 10, 
    strata = "hpyprtnr_depvar"
    )

# split training and testing data in each fold
train_data <- 
  training(folds$splits[[1]])
test_data <- 
  testing(folds$splits[[1]])

# start the model workforce
lm_wflow <-
  workflow()

# specify the model engine
lm_model <-
  linear_reg() %>% 
  set_engine("lm")

# specify the different models
model_list <- 
  list(
    null = hpyprtnr_depvar ~ 0
  )

# store the model engine and the list of models inside the workflow
model_storage <- 
  workflow_set(
    preproc = model_list, 
    models = list(lm = lm_model)
    )

# fit the model


# save model results


# display model results


# Happiness with Partner: Finite Distributed Lag Model --------------------

# status: not started


# Life Satisfaction: Exploratory Data Analysis ----------------------------

# plot life satisfaction over age between parents and non-parents
tab_family %>% 
  filter(age > 16) %>% 
  mutate(lifesat_depvar = ifelse(lifesat_depvar < 0, NA, lifesat_depvar)) %>% 
  group_by(parenthood, age) %>% 
  summarise(n = n(), mean_lifesat = mean(lifesat_depvar, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = mean_lifesat, color = parenthood)) + 
  geom_point() + 
  geom_smooth() + 
  scale_x_continuous(
    limits = c(16, 85),
    breaks = seq(15, 85, by = 5)) + 
  scale_y_continuous(limits = c(7, 9))

# plot life satisfaction over years before and since first child
tab_family %>% 
  mutate(lifesat_depvar = ifelse(lifesat_depvar < 0, NA, lifesat_depvar)) %>% 
  group_by(years_since_first_child, sex_fct) %>% 
  summarise(
    n = n(), 
    mean_lifesat = mean(lifesat_depvar, na.rm = T),
    n_non_missing_lifesat = sum(!is.na(lifesat_depvar))
    ) %>% 
  ggplot(aes(x = years_since_first_child, y = mean_lifesat, color = sex_fct)) + 
  geom_vline(xintercept = 0, linewidth = 0.5) + 
  geom_point() + 
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(7, 9)) + 
  scale_x_continuous(limits = c(-9, 9)) 


# Life Satisfaction: Multiple Linear Regression ---------------------------

# status: not started


# Life Satisfaction: Finite Distributed Lag Model -------------------------

# status: not started


# Archive -----------------------------------------------------------------

