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
library(ordinal)


data <- read.csv("stance_data.csv")

write.csv(data,"stance_data.csv")




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

data_event_study <- data_event_study %>% filter(never_treated!= 1)

sum(data_event_study$pre_treatment)
sum(data_event_study$treatment)
sum(data_event_study$post_treatment)


#--------------------------------------
# models

data_event_study <- data_event_study %>% filter(year> 2003)



data_event_study <- data_event_study %>%
  mutate(
    stance_temp_factor = as.factor(label),
    stance_ord = ordered(stance_temp_factor,
                         levels = c("opposes", "is neutral towards", "supports")) 
  )
head(data_event_study$stance_ord)
  


mod1.clmm <- clmm(
  formula = stance_ord ~ treatment + post_treatment +
    quarterly_inflation + election_period + eu_election_period + eu_summit + 
    (1 | speaker) + (1 | party),
  data = data_event_study,
  link = "logit"
)
summary(mod1.clmm)


mod2.clmm <- clmm(
  formula = stance_ord ~ treatment + post_treatment +
    (1 | speaker) + (1 | party),
  data = data_event_study,
  link = "logit"
)
summary(mod1.clmm)

mod3.clmm <- clmm(
  formula = stance_ord ~ treatment + post_treatment +
   eu_election_period + eu_summit + 
    (1 | speaker) + (1 | party),
  data = data_event_study,
  link = "logit"
)
summary(mod1.clmm)

mod4.clmm <- clmm(
  formula = stance_ord ~ treatment + post_treatment +
    quarterly_inflation + election_period +
    (1 | speaker) + (1 | party),
  data = data_event_study,
  link = "logit"
)
summary(mod1.clmm)




modelsummary(list("Model 1" = mod1.clmm, "Model 2" = mod2.clmm, "Model 3" = mod3.clmm, "Model 4" = mod4.clmm),
             title = "Multilevel Ordered Logit for Stance Towards the EU (Neg < Neu < Pos)",
             stars = TRUE,
             fmt = 2,
             coef_map = c( # Optional: rename coefficients for better presentation
              "treatment" = "Treatment (1st Chamber in Government)",
               "post_treatment" = "Post-Treatment (Chambers after)",
               "post_treatment2" = "Post-Treatment (2+ Chambers after)",
               "did" = "Government (DiD)",
               "quarterly_inflation" = "Quarterly Inflation",
               "election_period" = "Election Period",
               "eu_election_period" = "EU Election Period",
               "eu_summit" = "EU Summit"
             ),
             output = "multilevel_event.tex")








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



#--------------