#----------------------------------------------------------
# EU Statistical Models
#-----------------------
# Setup

library(tidyverse)
library(data.table)
library(fixest)
library(flexplot)
library(glmmTMB)
library(modelsummary)
library(broom)
library(rstanarm)
library(brms)


setwd("/Users/stepanjaburek/Downloads/EU_Thesis_Paper")
data <- read.csv("stance_data.csv")

#------------------------
#----------------------------------

# Event Study Chamber

data$stance_bin <- ifelse(data$label == "supports", 1, 0) # binarize DV

# Now, assign government membership based on party and the government spell
data$government <- case_when(
  (data$gov %in% c("Klaus I", "Klaus II", "Topolánek I", "Topolánek II", "Nečas", "Fiala")) & (data$party == "ODS") ~ 1,
  (data$gov %in% c("Zeman", "Špidla", "Gross", "Paroubek", "Sobotka")) & (data$party == "ČSSD") ~ 1,
  (data$gov %in% c("Klaus I", "Klaus II", "Špidla", "Gross", "Paroubek", "Topolánek II", "Sobotka", "Fiala")) & (data$party == "KDU-ČSL") ~ 1,
  (data$gov %in% c("Klaus I", "Klaus II", "Tošovský")) & (data$party == "ODA") ~ 1,
  (data$gov %in% c("Sobotka", "Babiš I", "Babiš II")) & (data$party == "ANO") ~ 1,
  (data$gov %in% c("Nečas", "Fiala")) & (data$party == "TOP 09") ~ 1,
  (data$gov %in% c("Fiala")) & (data$party == "STAN") ~ 1,
  (data$gov %in% c("Fiala")) & (data$party == "Piráti") ~ 1,
  (data$gov %in% c("Špidla", "Gross", "Paroubek")) & (data$party == "US-DEU") ~ 1,
  (data$gov %in% c("Topolánek II")) & (data$party == "SZ") ~ 1,
  (data$gov %in% c("Nečas")) & (data$party == "VV") ~ 1,
  TRUE ~ 0 # Default to 0 if none of the above conditions are met
)


# Find the earliest date each speaker appears in the data
speaker_career_start <- data %>%
  group_by(speaker) %>%
  summarise(career_start_date = min(month, na.rm = TRUE))

# Assuming 'data' is your main dataframe and 'month' is in Date format
data <- data %>%
  mutate(chamber = case_when(
    month >= "2021-11-01" & month <= "2023-12-01" ~ "9",
    month >= "2017-11-01" & month <= "2021-10-01" ~ "8",
    month >= "2013-11-01" & month <= "2017-10-01" ~ "7",
    month >= "2010-06-01" & month <= "2013-10-01" ~ "6",
    month >= "2006-06-01" & month <= "2010-05-01" ~ "5",
    month >= "2002-07-01" & month <= "2006-05-01" ~ "4",
    month >= "1998-07-01" & month <= "2002-06-01" ~ "3",
    month >= "1996-06-01" & month <= "1998-06-01" ~ "2",
    month >= "1993-01-01" & month <= "1996-05-01" ~ "1",
    TRUE ~ NA_character_
  ))


first_gov_chamber <- data %>%
  filter(government == 1) %>%
  group_by(speaker) %>%
  summarise(first_gov_chamber = first(chamber))


data_with_first_gov <- data %>%
  left_join(first_gov_chamber, by = "speaker")

data_with_first_gov <- data_with_first_gov %>%
  mutate(
    chamber_numeric = as.numeric(chamber),
    first_gov_chamber_numeric = as.numeric(first_gov_chamber),
    relative_chamber = case_when(
      !is.na(first_gov_chamber_numeric) ~ chamber_numeric - first_gov_chamber_numeric,
      TRUE ~ NA_real_ 
    ),
    ever_treated = as.numeric(!is.na(first_gov_chamber_numeric))
  )

data_event_study <- data_with_first_gov %>%
  mutate(
    # Example leads (pre-treatment)
    #pre_treatment2 = as.numeric(relative_chamber < -1 & ever_treated == 1),
    pre_treatment = as.numeric(relative_chamber < 0 & ever_treated == 1),
    
    # Treatment period (relative_chamber == 0)
    treatment = as.numeric(relative_chamber == 0 & ever_treated == 1),
    
    # Example lags (post-treatment)
    post_treatment = as.numeric(relative_chamber > 0 & ever_treated == 1),
    # Example lags (post-treatment)
    #post_treatment2 = as.numeric(relative_chamber >1 & ever_treated == 1),
    # Create an indicator for never-treated parties to include them in the analysis
    # This helps ensure your fixed effects are well-defined if some parties are pure controls.
    never_treated = as.numeric(is.na(first_gov_chamber_numeric))
  )

data_event_study <- data_event_study %>% filter(label!="is neutral towards") # keep binary for a logreg for now 
data_event_study$stance_bin <- ifelse(data_event_study$label == "supports", 1, 0) # binarize DV

data_event_study <- data_event_study %>% filter(never_treated!= 1)

sum(data_event_study$pre_treatment)
sum(data_event_study$treatment)
sum(data_event_study$post_treatment)


#--------------------------------------
# models

data_event_study <- data_event_study %>% filter(year> 2003)



stag.did.mod.base <- feglm(stance_bin ~ treatment + post_treatment 
  | speaker + year
,
                                        family = binomial,
                                        data = data_event_study)

summary(stag.did.mod.base)

stag.did.mod.1 <- feglm(stance_bin ~ treatment + post_treatment  + 
     eu_election_period + eu_summit 
  | speaker + year,
                                        family = binomial,
                                        data = data_event_study)

# Summary of the model
summary(stag.did.mod.1)


stag.did.mod.2 <- feglm(stance_bin ~ treatment + post_treatment  + 
     Inflation + election_period 
  | speaker + year,
                                        family = binomial,
                                        data = data_event_study)

# Summary of the model
summary(stag.did.mod.2)


stag.did.mod.3 <- feglm(stance_bin ~ treatment + post_treatment  + year_centered +
     Inflation + election_period + eu_election_period + eu_summit 
  | speaker,
                                        family = binomial,
                                        data = data_event_study)

# Summary of the model
summary(stag.did.mod.3)


# Tidy the model
tidy_mod <- tidy(stag.did.mod.3, conf.int = TRUE) %>%
  filter(term %in% c("treatment", "post_treatment"))

# Add the baseline (intercept) as a point
tidy_mod <- bind_rows(
  tibble(
    term = "(Intercept)",
    estimate = 0,              # log-odds difference relative to itself = 0
    std.error = NA,
    conf.low = NA,
    conf.high = NA
  ),
  tidy_mod
) %>%
  mutate(period = factor(term,
                         levels = c("(Intercept)", "treatment", "post_treatment"),
                         labels = c("Pre-treatment", "Treatment (1st Gov)", "Post-treatment")))

# Plot
library(ggpubr)
ggplot(tidy_mod, aes(x = period, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
   theme_pubr() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Period relative to first gov chamber",
       y = "Log-odds (Supporting Stance = 1)",
       title = "Chamber-level Event Study Model")


stag.did.mod.3 <- glmmTMB(stance_bin ~ treatment + post_treatment + post_treatment2 + 
     Inflation + election_period + eu_election_period + eu_summit 
   + (1|speaker) + factor(year),
                                        family = binomial(link="logit"),
                                        data = data_event_study)

# Summary of the model
summary(stag.did.mod.3)


modelsummary(list("Model 1" = stag.did.mod.base, "Model 2" = stag.did.mod.1, "Model 3" = stag.did.mod.2, 
"Model 4" = stag.did.mod.3),
             title = "Staggered DiD Logistic Models: Supporting vs Opposing Stance Towards the EU",
             stars = TRUE,
             fmt = 2,
             coef_map = c( # Optional: rename coefficients for better presentation
              "treatment" = "Treatment (1st Chamber in Government)",
               "post_treatment" = "Post-Treatment (Chambers after)",
               "post_treatment2" = "Post-Treatment (2+ Chambers after)",
               "did" = "Government (DiD)",
               "Inflation" = "Inflation",
               "election_period" = "Election Period",
               "eu_election_period" = "EU Election Period",
               "eu_summit" = "EU Summit"
             ),
             output = "stag.did.tex")



flexplot(pre_treatment2~1, data_event_study)



#-------------------------#-------------------------



# 1. Summarize the data to count observations in each 'relative_chamber' period
#    for both treated and never-treated groups.
data_summary <- data_event_study %>%
  mutate(
    # Create a cleaner label for the 'ever_treated' variable
    treatment_status = ifelse(ever_treated == 1, "Treated", "Never Treated"),
    
    # Group the relative chambers for easier visualization,
    # as you did for your model dummies.
    relative_chamber_group = case_when(
      relative_chamber <= -2 ~ "-2 or less",
      relative_chamber == -1 ~ "-1 (Reference)",
      relative_chamber == 0 ~ "0 (Treatment)",
      relative_chamber == 1 ~ "1 (Post-treatment)",
      relative_chamber >= 2 ~ "2 or more"
    )
  ) %>%
  # Count the number of observations in each group
  group_by(treatment_status, relative_chamber_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  # Make sure the groups are ordered correctly for the plot
  mutate(
    relative_chamber_group = factor(
      relative_chamber_group,
      levels = c("-2 or less", "-1 (Reference)", "0 (Treatment)", "1 (Post-treatment)", "2 or more")
    )
  )


# 2. Create the visualization
ggplot(data_summary, aes(x = relative_chamber_group, y = count, fill = treatment_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9), size = 3) +
  labs(
    title = "Number of Observations by Event Time and Treatment Status",
    x = "Relative Chamber (Event Time)",
    y = "Number of Observations",
    fill = "Treatment Status"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("Treated" = "steelblue", "Never Treated" = "gray")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


print(data_summary)



#-------------------------------
# Event Study years
data$month <- as.Date(paste0(data$month, "-01"))


data$government <- case_when(
  (data$gov %in% c("Klaus I", "Klaus II", "Topolánek I", "Topolánek II", "Nečas", "Fiala")) & (data$party == "ODS") ~ 1,
  (data$gov %in% c("Zeman", "Špidla", "Gross", "Paroubek", "Sobotka")) & (data$party == "ČSSD") ~ 1,
  (data$gov %in% c("Klaus I", "Klaus II", "Špidla", "Gross", "Paroubek", "Topolánek II", "Sobotka", "Fiala")) & (data$party == "KDU-ČSL") ~ 1,
  (data$gov %in% c("Klaus I", "Klaus II", "Tošovský")) & (data$party == "ODA") ~ 1,
  (data$gov %in% c("Sobotka", "Babiš I", "Babiš II")) & (data$party == "ANO") ~ 1,
  (data$gov %in% c("Nečas", "Fiala")) & (data$party == "TOP 09") ~ 1,
  (data$gov %in% c("Fiala")) & (data$party == "STAN") ~ 1,
  (data$gov %in% c("Fiala")) & (data$party == "Piráti") ~ 1,
  (data$gov %in% c("Špidla", "Gross", "Paroubek")) & (data$party == "US-DEU") ~ 1,
  (data$gov %in% c("Topolánek II")) & (data$party == "SZ") ~ 1,
  (data$gov %in% c("Nečas")) & (data$party == "VV") ~ 1,
  TRUE ~ 0 # Default to 0 if none of the above conditions are met
)

data <- data %>%
  mutate(chamber = case_when(
    month >= "2021-11-01" & month <= "2023-12-01" ~ "9",
    month >= "2017-11-01" & month <= "2021-10-01" ~ "8",
    month >= "2013-11-01" & month <= "2017-10-01" ~ "7",
    month >= "2010-06-01" & month <= "2013-10-01" ~ "6",
    month >= "2006-06-01" & month <= "2010-05-01" ~ "5",
    month >= "2002-07-01" & month <= "2006-05-01" ~ "4",
    month >= "1998-07-01" & month <= "2002-06-01" ~ "3",
    month >= "1996-06-01" & month <= "1998-06-01" ~ "2",
    month >= "1993-01-01" & month <= "1996-05-01" ~ "1",
    TRUE ~ NA_character_
  ))



# If 'month' is not already in Date format (e.g., it's "YYYY-MM"), convert it first:
data$month <- as.Date(paste0(data$month, "-01"))
# Calculate the first chamber a speaker enters government (for chamber-based treatment)
first_gov_chamber <- data %>%
  filter(government == 1) %>%
  group_by(speaker) %>%
  summarise(first_gov_chamber = first(chamber[order(month)]), .groups = "drop") %>%
  left_join(
    data %>%
      filter(government == 1) %>%
      group_by(chamber) %>%
      summarise(
        first_gov_month = min(month, na.rm = TRUE),
        last_gov_month  = max(month, na.rm = TRUE),
        .groups = "drop"
      ),
    by = c("first_gov_chamber" = "chamber")
  )


# Join all necessary timeline data back to your original data
data_processed <- data %>%
  left_join(first_gov_chamber, by = "speaker")


data_processed <- data_processed %>%
  mutate(
    # Chamber-based calculations
    chamber_numeric = as.numeric(chamber),
    first_gov_chamber_numeric = as.numeric(first_gov_chamber),
    relative_chamber = case_when(
      !is.na(first_gov_chamber_numeric) ~ chamber_numeric - first_gov_chamber_numeric,
      TRUE ~ NA_real_
    ),
    ever_treated = as.numeric(!is.na(first_gov_chamber_numeric)),

    # Month-based calculations (using interval for month difference)
    # Be careful with month differences; lubridate's %--% and as.period are more robust
    # If your 'month' column is truly just YYYY-MM, then simply calculating difference directly might work
    # but `interval(start, end) / months(1)` is safer for full dates.
    # Here, assuming `month` is already normalized to the first day of the month for direct subtraction.
    #relative_month_enter = as.numeric(difftime(month, first_gov_month, units = "days") / ddays(1) / (365.25/12)), # Approximate months
    #relative_month_exit = as.numeric(difftime(month, last_gov_month, units = "days") / ddays(1) / (365.25/12)) # Approximate months
    # Alternatively, if 'month' is consistently the first day of the month, you could do:
     relative_month_enter = interval(first_gov_month, month) %/% months(1),
     relative_month_exit = interval(last_gov_month, month) %/% months(1)
  )


data_event_study <- data_processed %>%
  mutate(
    # Treatment = entire chamber between first_gov_month and last_gov_month
    treatment = as.numeric(
      ever_treated == 1 & month >= first_gov_month & month <= last_gov_month
    ),

    # Pre-treatment periods (relative to entry month)
    relative_month_entry = interval(first_gov_month, month) %/% months(1),
    relative_month_exit  = interval(last_gov_month, month) %/% months(1),

    relative_time_factor = case_when(
      # Pre-treatment
      ever_treated == 1 & relative_month_entry < 0 & relative_month_entry >= -12 ~ "-1",
      ever_treated == 1 & relative_month_entry < -12 & relative_month_entry >= -24 ~ "-2",
      ever_treated == 1 & relative_month_entry < -24 & relative_month_entry >= -36 ~ "-3",
      ever_treated == 1 & relative_month_entry < -36 & relative_month_entry >= -48 ~ "-4",
      
      # Treatment period (the entire chamber)
      treatment == 1 ~ "0",

      # Post-treatment (relative to chamber **exit**)
      ever_treated == 1 & relative_month_exit >= 1 & relative_month_exit <= 12 ~ "+1",
      ever_treated == 1 & relative_month_exit >= 13 & relative_month_exit <= 24 ~ "+2",
      ever_treated == 1 & relative_month_exit >= 25 & relative_month_exit <= 36 ~ "+3",
      ever_treated == 1 & relative_month_exit >= 37 & relative_month_exit <= 48 ~ "+4",

      TRUE ~ NA_character_
    ),

    never_treated = as.numeric(is.na(first_gov_chamber_numeric))
  )

# Convert relative_month_enter_factor to an actual factor if desired for modeling
data_event_study$relative_time_factor <-
  factor(data_event_study$relative_time_factor,
         levels = c("-1","-4","-3","-2", "0", "+1", "+2", "+3", "+4"))


data_event_study <- data_event_study %>% filter(relative_time_factor != "")

flexplot(relative_time_factor~1,data_event_study)
sum(data_event_study$relative_time_factor == "-1")

data_event_study_treated <- data_event_study %>%  filter(never_treated == 0)


data_event_study_treated <- data_event_study_treated %>% filter(label!="is neutral towards") 
data_event_study_treated$stance_bin <- ifelse(data_event_study_treated$label == "supports", 1, 0) # binarize DV

data_event_study_treated <- data_event_study_treated %>%  filter(year > 2003)
#-------------

unique(data_event_study_treated$relative_time_factor)

event.study.mod.base <- feglm(stance_bin ~ relative_time_factor 
  | speaker + year,
                                        family = binomial,
                                        data = data_event_study_treated)

summary(event.study.mod.base)

tidy_mod <- tidy(event.study.mod.base, conf.int = TRUE) %>%
  filter(grepl("relative_time_factor", term))

# Add the baseline (-1) as intercept
tidy_mod <- bind_rows(
  tibble(
    term = "(Intercept)",
    estimate = 0,    # baseline vs itself = 0 log-odds
    std.error = NA,
    conf.low = NA,
    conf.high = NA
  ),
  tidy_mod
) %>%
  mutate(period = factor(term,
                         levels = c("relative_time_factor-3+", "relative_time_factor-2", 
                         "(Intercept)", 
                                    "relative_time_factor0", "relative_time_factor+1", 
                                    "relative_time_factor+2", "relative_time_factor+3+"),
                         labels = c( "-3+", "-2","Pre-Treatment", "Treatment (1st Gov)", "+1", "+2", "+3+")))


ggplot(tidy_mod, aes(x = period, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Period relative to first gov chamber",
       y = "Log-odds (Supporting Stance = 1)",
       title = "Granular Event Study Model")


event.study.mod.1 <- feglm(stance_bin ~ relative_time_factor + eu_election_period + eu_summit
  | speaker + year,
                                        family = binomial,
                                        data = data_event_study_treated)

summary(event.study.mod.1)

event.study.mod.2 <- feglm(stance_bin ~ relative_time_factor + Inflation + election_period
  | speaker + year,
                                        family = binomial,
                                        data = data_event_study_treated)

summary(event.study.mod.2)

event.study.mod.3 <- feglm(stance_bin ~ relative_time_factor + eu_election_period + eu_summit
  + Inflation + election_period | speaker + year,
                                        family = binomial,
                                        data = data_event_study_treated)

summary(event.study.mod.3)



tidy_mod <- tidy(event.study.mod.3, conf.int = TRUE) %>%
  filter(grepl("relative_time_factor", term))

# Add the baseline (-1) as intercept
tidy_mod <- bind_rows(
  tibble(
    term = "(Intercept)",
    estimate = 0,    # baseline vs itself = 0 log-odds
    std.error = NA,
    conf.low = NA,
    conf.high = NA
  ),
  tidy_mod
) %>%
  mutate(period = factor(term,
                         levels = c(
                         "(Intercept)", 
                                    "relative_time_factor0", "relative_time_factor+1", 
                                    "relative_time_factor+2", "relative_time_factor+3",
                                  "relative_time_factor+4", "relative_time_factor+5+"),
                         labels = c("Pre-Treatment", 
                         "Treatment", "+1", "+2", "+3", "+4", "+5+")))


ggplot(tidy_mod, aes(x = period, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
   theme_pubr() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Period relative to first gov chamber",
       y = "Log-odds (Supporting Stance = 1)",
       title = "Granular Event Study Model")



modelsummary(list("Model 1" = event.study.mod.base, "Model 2" = event.study.mod.1,
 "Model 3" = event.study.mod.2, 
"Model 4" = event.study.mod.3),
             title = "Staggered DiD Logistic Models: Supporting vs Opposing Stance Towards the EU",
             stars = TRUE,
             fmt = 2,
             coef_map = c( # Optional: rename coefficients for better presentation
              "relative_time_factor-3+" = "Pre-Treatment (3+ Years before)",
              "relative_time_factor-2" = "Pre-Treatment (2 Years before)",
              "relative_time_factor0" = "Treatment (1st Chamber in Government)",
               "relative_time_factor+1" = "Post-Treatment (1 Year after)",
               "relative_time_factor+2" = "Post-Treatment (2 Years after)",
               "relative_time_factor+3+" = "Post-Treatment (3+ Years after)",
               "did" = "Government (DiD)",
               "Inflation" = "Inflation",
               "election_period" = "Election Period",
               "eu_election_period" = "EU Election Period",
               "eu_summit" = "EU Summit"
             ),
             output = "event_study.tex")


table(data_event_study_treated$relative_time_factor)



data_event_study_treated$relative_time_factor <- factor(
  data_event_study_treated$relative_time_factor,
  levels = c("-3+", "-2", "-1", "0", "+1", "+2", "+3+")
)

data_event_study_treated$relative_time_factor <- relevel(
  data_event_study_treated$relative_time_factor, ref = "-3+"
)
event.study.mod <- feglm(
  stance_bin ~ relative_time_factor | speaker + year,
  family = binomial,
  data = data_event_study_treated
)
summary(event.study.mod)

# Tidy the model
tidy_mod <- tidy(event.study.mod, conf.int = TRUE) %>%
  filter(term %in% c("treatment", "post_treatment", "post_treatment2"))

# Add the baseline (intercept) as a point
tidy_mod <- bind_rows(
  tibble(
    term = "(Intercept)",
    estimate = 0,              # log-odds difference relative to itself = 0
    std.error = NA,
    conf.low = NA,
    conf.high = NA
  ),
  tidy_mod
) %>%
  mutate(period = factor(term,
                         levels = c("(Intercept)", "treatment", "post_treatment", "post_treatment2"),
                         labels = c("Pre-treatment", "Treatment (1st Gov)", "1 chamber after", "2+ chambers after")))

# Plot
ggplot(tidy_mod, aes(x = period, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Period relative to first gov chamber",
       y = "Log-odds (Supporting Stance = 1)",
       title = "Chamber-level Event Study Model")

tidy(event.study.mod) %>%
  filter(str_detect(term, "relative_time_factor")) %>%
  mutate(period = str_remove(term, "relative_time_factor"),
         period = factor(period, levels = c("-2", "-1", "0", "+1", "+2", "+3+"))) %>%
  ggplot(aes(x = period, y = estimate,
             ymin = estimate - 1.96*std.error,
             ymax = estimate + 1.96*std.error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Relative period (baseline = -3+)",
       y = "Log-odds effect on stance_bin",
       title = "Event Study Estimates (baseline = -3+)")












# purely MPs who are in all treated categories

# 1. Identify speakers who have observations in pre, treat, and post periods
valid_speakers <- data_event_study %>%
  group_by(speaker) %>%
  filter(
    any(pre_treatment == 1) &
    any(treatment == 1) &
    any(post_treatment == 1)
  ) %>%
  distinct(speaker)

# 2. Filter the original data_event_study dataset to include only these valid speakers
staggered_data_pure<- data_event_study %>%
  filter(speaker %in% valid_speakers$speaker)

month_data <- data_event_study %>% 
  group_by(speaker,month) %>% 
  reframe(
    n = n(),
    neg_sum = sum(label=="opposes"),
    neg_prop = neg_sum/n,
    year = first(year),
    inflation = first(Inflation),
    galtan = first(galtan),
    government = first(government),
    election_period = first(election_period),
    eu_election_period = first(eu_election_period),
    eu_summit = first(eu_summit),
    treatment = first(treatment),
    post_treatment = first(post_treatment),
    post_treatment2 = first(post_treatment2),
  )

month_data$speaker <- as.factor(month_data$speaker)
month_data$year <- as.factor(month_data$year)

did.mod.neg <- glmmTMB(neg_prop ~ treatment + post_treatment + post_treatment2 
 +speaker + year +  inflation + election_period + eu_election_period + eu_summit , 
                   family = ordbeta(link = "logit"), 
                   data = month_data)
summary(did.mod.neg)



pos_month_data <- data_event_study %>% 
  group_by(speaker,month) %>% 
  reframe(
    n = n(),
    pos_sum = sum(label=="supports"),
    pos_prop = pos_sum/n,
    year = first(year),
    inflation = first(Inflation),
    galtan = first(galtan),
    government = first(government),
    election_period = first(election_period),
    eu_election_period = first(eu_election_period),
    eu_summit = first(eu_summit),
    treatment = first(treatment),
    post_treatment = first(post_treatment),
    post_treatment2 = first(post_treatment2),
  )


did.mod.pos <- glmmTMB(pos_prop ~ treatment + post_treatment + post_treatment2 
 +(1|speaker)+ factor(year) +  inflation + election_period + eu_election_period + eu_summit , 
                   family = ordbeta(link = "logit"), 
                   data = pos_month_data)
summary(did.mod.pos)





first_gov_chamber <- data %>%
  filter(government == 1) %>%
  group_by(speaker) %>%
  summarise(first_gov_chamber = first(chamber))

second_gov_chamber <- data %>%
  filter(government == 1) %>%
  group_by(speaker, chamber) %>%
  reframe() %>% 
   summarise(second_gov_chamber = nth(chamber, 2)) 


second_gov_chamber <- data %>%
  filter(government == 1) %>%
  group_by(speaker) %>%
  arrange(month, .by_group = TRUE) %>%
  summarise(second_gov_chamber = nth(unique(chamber), 3), .groups = "drop") %>%
  filter(!is.na(second_gov_chamber))


data_with_first_gov <- data %>%
  left_join(second_gov_chamber, by = "speaker")

data_with_first_gov <- data_with_first_gov %>%
  mutate(
    chamber_numeric = as.numeric(chamber),
    first_gov_chamber_numeric = as.numeric(second_gov_chamber),
    relative_chamber = case_when(
      !is.na(first_gov_chamber_numeric) ~ chamber_numeric - first_gov_chamber_numeric,
      TRUE ~ NA_real_ 
    ),
    ever_treated = as.numeric(!is.na(first_gov_chamber_numeric))
  )

data_event_study <- data_with_first_gov %>%
  mutate(
    # Example leads (pre-treatment)
    #pre_treatment2 = as.numeric(relative_chamber < -1 & ever_treated == 1),
    pre_treatment = as.numeric(relative_chamber < 0 & ever_treated == 1),
    
    # Treatment period (relative_chamber == 0)
    treatment = as.numeric(relative_chamber == 0 & ever_treated == 1),
    
    # Example lags (post-treatment)
    post_treatment = as.numeric(relative_chamber > 0 & ever_treated == 1),
    # Example lags (post-treatment)
    #post_treatment2 = as.numeric(relative_chamber >1 & ever_treated == 1),
    # Create an indicator for never-treated parties to include them in the analysis
    # This helps ensure your fixed effects are well-defined if some parties are pure controls.
    never_treated = as.numeric(is.na(first_gov_chamber_numeric))
  )

data_event_study <- data_event_study %>% filter(label!="is neutral towards") # keep binary for a logreg for now 
data_event_study$stance_bin <- ifelse(data_event_study$label == "supports", 1, 0) # binarize DV

data_event_study <- data_event_study %>% filter(never_treated!= 1)

sum(data_event_study$pre_treatment2)
sum(data_event_study$pre_treatment)
sum(data_event_study$treatment)
sum(data_event_study$post_treatment)
sum(data_event_study$post_treatment2)

#--------------------------------------
# models

data_event_study <- data_event_study %>% filter(year> 2003)

stag.did.mod.base <- feglm(stance_bin ~ treatment + post_treatment 
  | speaker + year
,
                                        family = binomial,
                                        data = data_event_study)

summary(stag.did.mod.base)
