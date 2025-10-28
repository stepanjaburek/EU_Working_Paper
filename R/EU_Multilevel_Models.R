#----------------------------------------------------------
# EU Statistical Models
#---------------------------------
# Setup

library(tidyverse)
library(quanteda)
library(data.table)
library(glmmTMB)
library(flexplot)
library(marginaleffects)
library(MASS)
library(fixest)
library(modelsummary)
library(ordinal)
library(ggeffects)
library(lme4)
library(merTools)
library(sjPlot)

#.-----------------------------------------
# Data
#--------
data2004 <- read.csv("imputed_data_2004.csv")
data <- read.csv("stance_data_v2.csv")

#--------------------



log_data <- data %>%   # keep binary for a logreg for now
  filter(label!="is neutral towards") 

log_data2004 <- data2004 %>%   # keep binary for a logreg for now
  filter(label!="is neutral towards") 

log_data$stance_bin <- ifelse(log_data$label == "supports", 1, 0) # binarize DV
log_data2004$stance_bin <- ifelse(log_data2004$label == "supports", 1, 0) # binarize DV


sum(is.na(log_data)) # check for NAs

log_data$government <- as.factor(log_data$government)
log_data$election_period <- as.factor(log_data$election_period)
log_data$eu_election_period <- as.factor(log_data$eu_election_period)
log_data$eu_summit <- as.factor(log_data$eu_summit)

log_data2004$government <- as.factor(log_data2004$government)
log_data2004$election_period <- as.factor(log_data2004$election_period)
log_data2004$eu_election_period <- as.factor(log_data2004$eu_election_period)
log_data2004$eu_summit <- as.factor(log_data2004$eu_summit)




# lets keep at least 500 mentions
atleast500<- c("ANO", "ČSSD", "KSČM", "ODS", "KDU-ČSL","Piráti", "TOP 09", "STAN", "SPD") 

ever_in_government<- c("ANO", "ČSSD", "ODS", "KDU-ČSL","Piráti", "TOP 09", "STAN") 

data_all <- log_data %>% 
  dplyr::filter(party %in% atleast500)

data_gov <- log_data %>% 
  dplyr::filter(party %in% ever_in_government)

data_all_2004 <- log_data2004 %>% 
  dplyr::filter(party %in% atleast500)

data_gov_2004 <- log_data2004 %>% 
  dplyr::filter(party %in% ever_in_government)
#----------------------------------------------------
#----------------------------------------------------
# Regular model
#----------


stance_model_intercept_2004 <- glmer(stance_bin ~ lag_quarterly_inflation + government + election_period   +
                        eu_election_period  + eu_summit  + avg_trust +
                         (1|party),
                      family = binomial,
                      data = data_all_2004)
summary(stance_model_intercept_2004)

ranef(stance_model_intercept_2004)




# Log-Odds = Fixed Intercept + Random Intercept (Deviation)
log_odds_party_intercept <- fixef(stance_model_intercept_2004)["(Intercept)"] + 
                            ranef(stance_model_intercept_2004)$party$"(Intercept)"

# Baseline Probability = exp(Log-Odds) / (1 + exp(Log-Odds))
prob_party_intercept <- 1 / (1 + exp(-log_odds_party_intercept))

prob_party_intercept <- exp(log_odds_party_intercept)/  (1 + exp(log_odds_party_intercept))

party_names <- rownames(ranef(stance_model_intercept_2004)$party)

intercepts <- cbind(party_names,prob_party_intercept)
intercepts <- as.data.frame(intercepts)
intercepts


#----------------------------------------------------
# Regular model
#----------
stance_model_slopes_2004 <- glmer(stance_bin ~ lag_quarterly_inflation   + election_period   +
                        eu_election_period  + eu_summit  + avg_trust + government +
                         (government|party),
                      family = binomial,
                      data = data_gov_2004)
summary(stance_model_slopes_2004)

ranef(stance_model_slopes_2004)


# Log-Odds (Gov=0) = Fixed Intercept + Random Intercept (Deviation)
log_odds_gov0 <- fixef(stance_model_slopes_2004)["(Intercept)"] + 
                 ranef(stance_model_slopes_2004)$party$"(Intercept)"

# Probability (Gov=0)
prob_gov0 <- exp(log_odds_gov0)/  (1 + exp(log_odds_gov0))

# Total Slope = Fixed government1 + Random government1 (Deviation)
total_gov_slope <- fixef(stance_model_slopes_2004)["government1"] + 
                   ranef(stance_model_slopes_2004)$party$"government1"

# Log-Odds (Gov=1) = Log-Odds (Gov=0) + Total Slope
log_odds_gov1 <- log_odds_gov0 + total_gov_slope

# Probability (Gov=1)
prob_gov1 <- exp(log_odds_gov1)/  (1 + exp(log_odds_gov1))

party_names_slopes <- rownames(ranef(stance_model_slopes_2004)$party)

slopes <- cbind(party_names_slopes, prob_gov0,prob_gov1)
slopes <- as.data.frame(slopes)
slopes


plot_model(stance_model_slopes_2004,
           type = "re",
           terms = "party")









#-----------------------------------------------------

stance_model_intercept <- glmmTMB(stance_bin ~ lag_quarterly_inflation + government + election_period   +
                        eu_election_period  + eu_summit   +
                         (1|party),
                      family = binomial,
                      data = data_all)
summary(stance_model_intercept)

ranef(stance_model_intercept)


#----------------------------------------------------
# Regular model
#----------
stance_model_slopes <- glmmTMB(stance_bin ~ lag_quarterly_inflation   + election_period   +
                        eu_election_period  + eu_summit   +
                         (government|party),
                      family = binomial,
                      data = data_gov)
summary(stance_model_slopes)


ranef(stance_model_slopes)


plot_model(stance_model_slopes,
           type = "re",
           terms = "party")


table(data_gov$party, data_gov$government)




#---------------

# Expúrt  models into latex
models <- list(
  "Mixed-Effects GLM" = stance_model,
  "Fixed-Effects GLM" = stance_model_fe
)

modelsummary(list("Intercept (>2004)" = stance_model_intercept_2004,
"Slopes (>2004)" = stance_model_slopes_2004, 
"Intercept (>1993)" = stance_model_intercept,
"Slopes (>1993)" = stance_model_slopes ),
             title = "Supporting Stance Towards the EU: Multilevel Models",
             stars = TRUE,
             fmt = 2,
             coef_omit = "year",
             coef_map = c( # Optional: rename coefficients for better presentation
               "government1" = "Government",
               "lag_quarterly_inflation" = "Quarterly Inflation (Lag)",
               "lag_quarterly_inflation:government1" = "Quarterly Inflation × Government",
               "election_period1" = "Election Period",
               "eu_election_period1" = "EU Election Period",
               "eu_summit1" = "EU Summit",
               "avg_trust" = "Public Trust EU"
             ),
             output = "stance_multilevel.tex")

modelsummary(list( "Logit Model 1" = stance_model_fe,
"Logit Model 2" = stance_model_fe_full),
             title = "Supporting Stance Towards the EU: One-Way Fixed Effects",
             stars = TRUE,
             fmt = 2,
             coef_omit = "year",
             coef_map = c( # Optional: rename coefficients for better presentation
               "government" = "Government",
               "lag_quarterly_inflation" = "Quarterly Inflation (Lag)",
               "lag_quarterly_inflation:government" = "Quarterly Inflation × Government",
               "election_period" = "Election Period",
               "eu_election_period" = "EU Election Period",
               "eu_summit" = "EU Summit"
             ),
             output = "stance2fe.tex")




intercepts$prob_party_intercept <- as.numeric(intercepts$prob_party_intercept)
numeric_cols <- names(intercepts)[sapply(intercepts, is.numeric)]
intercepts[numeric_cols] <- round(intercepts[numeric_cols] * 100, 0)



slopes$prob_gov1 <- as.numeric(slopes$prob_gov1)

slopes$prob_gov0 <- as.numeric(slopes$prob_gov0)


slopes <- slopes %>% 
  mutate(slope = prob_gov1 - prob_gov0)

# 1. Identify all numeric columns (excluding the 'Party' column)
numeric_cols <- names(slopes)[sapply(slopes, is.numeric)]

# 2. Apply the round function to only the numeric columns
slopes[numeric_cols] <- round(slopes[numeric_cols] * 100, 0)
