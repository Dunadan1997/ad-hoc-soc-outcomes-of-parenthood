
# Setting the directory to the data warehouse
setwd("/Users/brunoalvesdecarvalho/Desktop/DataWarehouse_20231015_ve01")

# Loading packages
library(tidyverse)
library(memoise)
library(haven)

# Loading functions from the warehouse
source("R_Scripts/FunctionRepository_20231016_ve01.R")

# Loading data stored in the warehouse
aggregated_data_shp <- 
  readRDS("SHP/Data_Aggregated_1999_2022/cached_data_shp.rds")

# Selecting variables necessary for analysis
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
        "p$$c50" # frequency of moments of worry
      )
    ),
    length(1999:2022)
  )

# Merge data into one single data-set
merged_data_shp <-
  shp_merge_data(aggregated_data_shp, selected_vars_shp)

# Transform data
colnames(merged_data_shp)[str_detect(colnames(merged_data_shp), "\\$$")] <-
  str_replace_all(colnames(merged_data_shp)[str_detect(colnames(merged_data_shp), "\\$$")], "\\$\\$", "")

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

# Create table
tab_family <- 
  merged_data_shp %>% 
  rename(
    nbb = nbb_,
    commune_type = com2_, 
    depvar_hpyprtnr = `p$$f54`, 
    depvar_lifesat = `p$$c44`, 
    depvar_freqjoy = `p$$c47`, 
    depvar_freqanger = `p$$c48`, 
    depvar_freqsad = `p$$c49`, 
    depvar_freqworry = `p$$c50`) %>% 
  select(-matches("\\$"))

# Correct data errors
tab_family[which(tab_family$idpers == 63186101 & tab_family$sex == 2),"sex_fct"] <- "man"
tab_family[which(tab_family$idpers == 63186102 & tab_family$age == 24), c("ownkid")] <- 1

# Modify table
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
    parenthood = ifelse(cumsum > 0, 1, 0),
    # Identify year of first child
    year_of_first_child = ifelse(first_child == 1, year, NA)
  ) %>% 
  fill(year_of_first_child, .direction = "updown") %>% 
  # Identify number of years since and before first child
  mutate(years_since_first_child = year - year_of_first_child) %>% 
  # Identify if the individual is a parent or non-parent
  left_join(
    tab_family %>% group_by(idpers) %>% summarise(sum_nbkids = sum(ifelse(ownkid < 0, 0, ownkid), na.rm = T)), relationship = "many-to-one") %>% 
  mutate(parent = ifelse(sum_nbkids > 0, "parent", "non_parent")) %>% 
  ungroup()


# Exploratory Data Analysis -----------------------------------------------

# plot age distribution over parents and non-parents (would and won't be), by sex
tab_family %>% 
  ggplot() + 
  geom_histogram(aes(x = age)) + 
  facet_wrap(~ parent + parenthood)
tab_family %>% 
  filter(sex_fct != "other") %>% 
  ggplot() + 
  geom_histogram(aes(x = age)) + 
  facet_wrap(~ parent + sex_fct)

# Determine upper age limit for having children
avg_age_first_child <- 
  mean(subset(tab_family, first_child == 1)$age) 
sd_age_first_child <- 
  sd(subset(tab_family, first_child == 1)$age)
upper_age_limit <- 
  avg_age_first_child + (2 * sd_age_first_child)

# plot age distribution of non-parents who "most likely" will never be parents
tab_nonparents <- 
  left_join(
  x = tab_family %>% 
    # Filter individuals who have crossed the upper age limit of having children
    mutate(flag_1 = ifelse(parent == "non_parent" & age > upper_age_limit, 1, 0)) %>% 
    filter(flag_1 == 1) %>% 
    select(idpers) %>% 
    distinct(), 
  y = tab_family, 
  relationship = "one-to-many")
tab_nonparents %>% 
  ggplot() + 
  geom_histogram(aes(age))

# plot age distribution of parents
tab_family %>% 
  filter(parent == "parent" & age > 16) %>% 
  ggplot() + 
  geom_histogram(aes(x = age))

# plot life satisfaction over age, by parent and non-parent
bind_rows(
  x = tab_family %>% 
    filter(parent == "parent" & age > 16), 
  y = tab_nonparents) %>% 
  mutate(depvar_lifesat = ifelse(depvar_lifesat < 0, NA, depvar_lifesat)) %>% 
  group_by(parent, age) %>% 
  summarise(n = n(), mean_lifesat = mean(depvar_lifesat, na.rm = TRUE)) %>% 
  ggplot(aes(x = age, y = mean_lifesat, color = parent)) + 
  geom_vline(xintercept = avg_age_first_child) + 
  geom_point() + 
  geom_smooth() + 
  scale_x_continuous(limits = c(26, 85)) + 
  scale_y_continuous(limits = c(7, 9))

# plot age distribution of mother and father when they have their first child
tab_family %>% 
  filter(first_child == 1) %>% 
  ggplot() + 
  geom_histogram(aes(x = age)) +
  facet_wrap(~ sex_fct)

# plot life satisfaction over years before and since first child
tab_family %>% 
  mutate(depvar_lifesat = ifelse(depvar_lifesat < 0, NA, depvar_lifesat)) %>% 
  group_by(years_since_first_child, sex_fct) %>% 
  summarise(
    n = n(), 
    mean_lifesat = mean(depvar_lifesat, na.rm = T),
    n_non_missing_lifesat = sum(!is.na(depvar_lifesat))) %>% 
  ggplot(aes(x = years_since_first_child, y = mean_lifesat, color = sex_fct)) + 
  geom_vline(xintercept = 0, linewidth = 0.5) + 
  geom_point() + 
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(7, 9)) + 
  scale_x_continuous(limits = c(-10, 10)) 

# plot frequency of joy over years before and since first child
tab_family %>% 
  mutate(depvar_freqjoy = ifelse(depvar_freqjoy < 0, NA, depvar_freqjoy)) %>% 
  group_by(years_since_first_child, sex_fct) %>% 
  summarise(
    n = n(), 
    mean_freqjoy = mean(depvar_freqjoy, na.rm = T), 
    n_non_missing_freqjoy = sum(!is.na(depvar_freqjoy))) %>% 
  ggplot(aes(x = years_since_first_child, y = mean_freqjoy, color = sex_fct)) + 
  geom_vline(xintercept = 0, linewidth = 0.5) + 
  geom_point() + 
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(6, 8)) + 
  scale_x_continuous(limits = c(-10, 10)) 

# plot frequency of anger over years before and since first child
tab_family %>% 
  mutate(depvar_freqanger = ifelse(depvar_freqanger < 0, NA, depvar_freqanger)) %>% 
  group_by(years_since_first_child, sex_fct) %>% 
  summarise(
    n = n(), 
    mean_freqanger = mean(depvar_freqanger, na.rm = T), 
    n_non_missing_freqanger = sum(!is.na(depvar_freqanger))) %>% 
  ggplot(aes(x = years_since_first_child, y = mean_freqanger, color = sex_fct)) + 
  geom_vline(xintercept = 0, linewidth = 0.5) + 
  geom_point() + 
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(2, 6)) + 
  scale_x_continuous(limits = c(-10, 10))   

# plot frequency of saddness over years before and since first child
tab_family %>% 
  mutate(depvar_freqsad = ifelse(depvar_freqsad < 0, NA, depvar_freqsad)) %>% 
  group_by(years_since_first_child, sex_fct) %>% 
  summarise(
    n = n(), 
    mean_freqsad = mean(depvar_freqsad, na.rm = T), 
    n_non_missing_freqsad = sum(!is.na(depvar_freqsad))) %>% 
  ggplot(aes(x = years_since_first_child, y = mean_freqsad, color = sex_fct)) + 
  geom_vline(xintercept = 0, linewidth = 0.5) + 
  geom_point() + 
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(1, 4)) + 
  scale_x_continuous(limits = c(-10, 10))  

# plot frequency of worry over years before and since first child
tab_family %>% 
  mutate(depvar_freqworry = ifelse(depvar_freqworry < 0, NA, depvar_freqworry)) %>% 
  group_by(years_since_first_child, sex_fct) %>% 
  summarise(
    n = n(), 
    mean_freqworry = mean(depvar_freqworry, na.rm = T), 
    n_non_missing_freqworry = sum(!is.na(depvar_freqworry))) %>% 
  ggplot(aes(x = years_since_first_child, y = mean_freqworry, color = sex_fct)) + 
  geom_vline(xintercept = 0, linewidth = 0.5) + 
  geom_point() + 
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(1.5, 4)) + 
  scale_x_continuous(limits = c(-10, 10))  

# plot happiness with partner over years before and since first child
tab_family %>% 
  mutate(depvar_hpyprtnr = ifelse(depvar_hpyprtnr < 0, NA, depvar_hpyprtnr)) %>% 
  group_by(years_since_first_child, sex_fct) %>% 
  summarise(
    n = n(), 
    mean_hpyprtnr = mean(depvar_hpyprtnr, na.rm = T), 
    n_non_missing_hpyprtnr = sum(!is.na(depvar_hpyprtnr))) %>% 
  ggplot(aes(x = years_since_first_child, y = mean_hpyprtnr, color = sex_fct)) + 
  geom_vline(xintercept = 0, linewidth = 0.5) + 
  geom_point() + 
  geom_smooth(se = F) +
  scale_y_continuous(limits = c(7.5, 10)) + 
  scale_x_continuous(limits = c(-5, 9))  




