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
install.packages("ordinal")
library(ordinal)
library(mclogit)


#.-----------------------------------------
# Data
#--------
setwd("C:/Users/stepa/EU_Thesis_Paper")
data <- read.csv("stance_data_v2.csv")

#--------------------
# lets keep at least 500 mentions
atleast500<- c("ANO", "ČSSD", "ODS", "KDU-ČSL","Piráti", "TOP 09", "STAN") 

data <- data %>% 
  dplyr::filter(party %in% atleast500)

log_data <- data %>%   # keep binary for a logreg for now
  filter(label!="is neutral towards") 

log_data$stance_bin <- ifelse(log_data$label == "supports", 1, 0) # binarize DV


log_data<-log_data %>% filter(year>1995) # NAs in inflation at the start of the data from ECB
sum(is.na(log_data)) # check for NAs


#----------------------------------------------------
# Small model with key hypothesized covariates
#----------

stance_model_small <- glmmTMB(stance_bin ~ lag_quarterly_inflation + government +
                        (1|speaker)  + (1|party),
                      family = binomial,
                      data = log_data)
summary(stance_model_small)

#----------------------------------------------------
# Regular model
#----------
stance_model <- glmmTMB(stance_bin ~ lag_quarterly_inflation + government  + election_period   +
                        eu_election_period  + eu_summit +
                        (1|speaker)  + (1|party),
                      family = binomial,
                      data = log_data)
summary(stance_model)

#----------------------------------------------------
# Full model with all covariates
#----------
stance_model_full <- glmmTMB(stance_bin ~ lag_quarterly_inflation * government  + election_period   +
                          eu_election_period + eu_summit  +
                        (1|speaker) + (1|party),
                        family = binomial,
                        data = log_data)
summary(stance_model_full)


#--------------------------------------
# 2FE models same strucutre and logic, without accounting for MP level variation
#------
stance_model_fe_small <- feglm(stance_bin ~ year_centered  + Inflation + government + galtan 
                        |  party,
                         family = binomial,
                         data = log_data)
summary(stance_model_fe_small)

stance_model_fe <- feglm(stance_bin ~ year_centered + Inflation + government + galtan + election_period   + 
                           eu_election_period |  party,
                         family = binomial,
                         data = log_data)
summary(stance_model_fe)

stance_model_fe_full <- feglm(stance_bin ~ year_centered + Inflation * government + galtan + election_period   + 
                           eu_election_period + eu_summit  |  party,
                         family = binomial,
                         data = log_data)
summary(stance_model_fe_full)


#---------------

# Expúrt  models into latex
models <- list(
  "Mixed-Effects GLM" = stance_model,
  "Fixed-Effects GLM" = stance_model_fe
)

modelsummary(list("Multilevel Logistic" = stance_model_small, "Multilevel Logistic" = stance_model,
"Multilevel Logistic" = stance_model_full),
             title = "Negative Mentions of the Left from Individual Level Data",
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
             output = "stance.tex")



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


modelsummary(list("2FE" = stance_model_fe_small, "2FE" = stance_model_fe,
"2FE" = stance_model_fe_full),
             title = "Negative Mentions of the Left from Individual Level Data",
             stars = TRUE,
             fmt = 2,
             coef_omit = "year",
             coef_map = c( # Optional: rename coefficients for better presentation
               "government1" = "Government",
               "Inflation" = "Inflation",
               "Inflation:government1" = "Inflation × Government",
               "galtan"= "GAL-TAN",
               "election_period" = "Election Period",
               "eu_election_period" = "EU Election Period",
               "eu_summit" = "EU Summit",
               "ukraine_invasion" = "Ukraine Invasion",
               "migration_crisis" = "Migration Crisis" 
             ),
             output = "stance2fe.tex")

           



#----------------------------------------------------------------
# Framing models
#----------------------------------------
data <- read.csv("frame_data.csv")

data <-data %>% 
  filter(label=="Consequences" |label=="Appropriateness")

data$frame <- ifelse(data$label=="Appropriateness", 1,0)

data <-data %>% 
  filter(party %in% atleast500)

data <-data %>% filter(year>1995)



#----------------------------------------------------
# Small model with key hypothesized covariates
#----------
discourse_model_small <- glmmTMB(frame ~ year + Inflation + government + galtan +
                        (1|speaker)  + (1|party),
                        family = binomial,
                        data = data)
summary(discourse_model_small)


#----------------------------------------------------
# Regular model
#----------

discourse_model <- glmmTMB(frame ~ year + Inflation + government + galtan + election_period   +
                             eu_election_period   + (1|speaker)  + (1|party),
                        family = binomial,
                        data = data)
summary(discourse_model)


#----------------------------------------------------
# Full model with all covariates
#----------
discourse_model_full <- glmmTMB(frame ~ year + Inflation * government + galtan + election_period   +
                             eu_election_period + eu_summit  + 
                              (1|speaker)  + (1|party),
                        family = binomial,
                        data = data)
summary(discourse_model_full)

#---------------------------------
# 2FE

discourse_model_fe_small <- feglm(frame ~ Inflation + government + galtan
                        |  party + year,
                         family = binomial,
                         data = data)
summary(discourse_model_fe_small)

discourse_model_fe <- feglm(frame ~ Inflation + government + galtan + election_period   + 
                           eu_election_period  |  party + year,
                         family = binomial,
                         data = data)
summary(discourse_model_fe)

discourse_model_fe_full <- feglm(frame ~ Inflation * government + galtan + election_period   + 
                           eu_election_period  + eu_summit |  party + year,
                         family = binomial,
                         data = data)
summary(discourse_model_fe_full)



modelsummary(list("Multilevel Logistic" = discourse_model_small, "Multilevel Logistic" = discourse_model, 
"Multilevel Logistic" = discourse_model_full ),
             title = "Negative Mentions of the Left from Individual Level Data",
             stars = TRUE,
             fmt = 2,
             coef_omit = "year", 
               coef_map = c( # Optional: rename coefficients for better presentation
                 "government1" = "Government",
                 "Inflation" = "Inflation",
                 "Inflation:government1" = "Inflation × Government",
                 "galtan"= "GAL-TAN",
                 "election_period" = "Election Period",
                 "eu_election_period" = "EU Election Period",
                 "eu_summit" = "EU Summit"
               ),
             output = "discourse.tex")



modelsummary(list("2FE" = discourse_model_fe_small, "2FE" = discourse_model_fe, 
"2FE" = discourse_model_fe_full ),
             title = "Negative Mentions of the Left from Individual Level Data",
             stars = TRUE,
             fmt = 2,
             coef_omit = "year", 
               coef_map = c( # Optional: rename coefficients for better presentation
                 "government1" = "Government",
                 "Inflation" = "Inflation",
                 "Inflation:government1" = "Inflation × Government",
                 "galtan"= "GAL-TAN",
                 "election_period" = "Election Period",
                 "eu_election_period" = "EU Election Period",
                 "eu_summit" = "EU Summit"
               ),
             output = "discourse2fe.tex")

summary(model)


#-------------------------------
#- Stance and framing models
data <- read.csv("discourse_data.csv")
frames <- data %>% dplyr::select("label")

data <- read.csv("stance_data.csv")
data <- data %>% rename ("stance" = "label")


data <- cbind(data,frames)


dataa <- data %>%
  filter(stance == "supports" | stance == "opposes")
dataa <- dataa %>%
  filter(label == "Appropriateness" | label == "Consequences")


dataa$support_app <- ifelse(dataa$stance == "supports" & dataa$label == "Appropriateness", 1, 0)
dataa$oppose_app <- ifelse(dataa$stance == "opposes" & dataa$label == "Appropriateness", 1, 0)
dataa$oppose_con <- ifelse(dataa$stance == "opposes" & dataa$label == "Consequences", 1, 0)
dataa$support_con <- ifelse(dataa$stance == "supports" & dataa$label == "Consequences", 1, 0)




support_app_model <- glmmTMB(support_app ~ factor(year) + Inflation * government + election_period   +
                          eu_election_period + eu_summit + galtan + (1|speaker)  + (1|party),
                        family = binomial,
                        data = dataa)
summary(support_app_model)


oppose_app_model <- glmmTMB(oppose_app ~ factor(year) + Inflation * government + election_period   +
                            eu_election_period + eu_summit + galtan + (1|speaker)  + (1|party),
                          family = binomial,
                          data = dataa)
summary(oppose_app_model)


oppose_con_model <- glmmTMB(oppose_con ~ factor(year) + Inflation * government + election_period   +
                              eu_election_period + eu_summit + galtan + (1|speaker)  + (1|party),
                            family = binomial,
                            data = dataa)
summary(oppose_con_model)


support_con_model <- glmmTMB(support_con ~ factor(year) + Inflation * government + election_period   +
                              eu_election_period + eu_summit + galtan + (1|speaker)  + (1|party),
                            family = binomial,
                            data = dataa)
summary(support_con_model)


modelsummary(list("support_app_model" = support_app_model, "oppose_app_model" = oppose_app_model,
                  "oppose_con_model" =oppose_con_model, "support_con_model" = support_con_model ),
             title = "Negative Mentions of the Left from Individual Level Data",
             stars = TRUE,
             fmt = 2,
             coef_omit = "year",
             coef_map = c( # Optional: rename coefficients for better presentation
               "government1" = "Government",
               "Inflation" = "Inflation",
               "Inflation:government1" = "Inflation × Government",
               "galtan"= "GAL-TAN",
               "election_period" = "Election Period",
               "eu_election_period" = "EU Election Period",
               "eu_summit" = "EU Summit"
             ),
             output = "both_multilevel_e.tex")


#------------------------
# 2FE

support_app_model_fe <- feglm(support_app ~ Inflation * government + election_period   +
                               eu_election_period + eu_summit + galtan | party + year,
                             family = binomial,
                             data = dataa)
summary(support_app_model_fe)


oppose_app_model_fe <- feglm(oppose_app ~ Inflation * government + election_period   +
                              eu_election_period + eu_summit + galtan | party + year,
                            family = binomial,
                            data = dataa)
summary(oppose_app_model_fe)


oppose_con_model_fe <- feglm(oppose_con ~  Inflation * government + election_period   +
                              eu_election_period + eu_summit + galtan | party + year,
                            family = binomial,
                            data = dataa)
summary(oppose_con_model_fe)


support_con_model_fe <- feglm(support_con ~ Inflation * government + election_period   +
                               eu_election_period + eu_summit + galtan | party + year,
                             family = binomial,
                             data = dataa)
summary(support_con_model_fe)



modelsummary(list("support_app_model" = support_app_model_fe, "oppose_app_model" = oppose_app_model_fe,
                  "oppose_con_model" =oppose_con_model_fe, "support_con_model" = support_con_model_fe ),
             title = "Negative Mentions of the Left from Individual Level Data",
             stars = TRUE,
             fmt = 2,
             coef_omit = "year",
             coef_map = c( # Optional: rename coefficients for better presentation
               "government1" = "Government",
               "Inflation" = "Inflation",
               "Inflation:government1" = "Inflation × Government",
               "galtan"= "GAL-TAN",
               "election_period" = "Election Period",
               "eu_election_period" = "EU Election Period",
               "eu_summit" = "EU Summit"
             ),
             output = "both_fe_e.tex")


#------------------------------------
# ordered logit stance
data <- read.csv("stance_data.csv")

df <- data %>%
  mutate(
    stance_temp_factor = as.factor(label),
    stance_ord = ordered(stance_temp_factor,
                         levels = c("opposes", "is neutral towards", "supports")) 
  )
head(df$stance_ord)

  df <- df %>% 
  filter(party %in% atleast500)

mod1.clmm <- clmm(
  formula = stance_ord ~ year + Inflation + government + galtan + election_period +
            eu_election_period + eu_summit + (1 | speaker) + (1 | party),
  data = df,
  link = "logit"
)
summary(mod1.clmm)

#------------------------------
# multinomial logit framing

data <- read.csv("frames_data.csv")


df <- 
  data %>%
  mutate(label = as.factor(label),
         label = relevel(label, ref = "Neither"))
         
df <- df %>% 
  filter(party %in% atleast500)


print(levels(df$label))


mod1.mblogit <- mblogit(
  formula = label ~ year + Inflation * government + galtan + election_period +
            eu_election_period + eu_summit,
  random = list(~1|speaker, ~1|party), 
  data = df
)
summary(mod1.mblogit)



stargazer(mod1.mblogit,
          #column.labels = c("Left Mentions"),
          #column.separate = c(4, 4),
          model.numbers = T,
          type = "text", 
          title = "",
          font.size = "large",
          no.space = TRUE,
          omit =  c("year", "party"),
          coef_omit = "year",
          column.sep.width = "4pt",
          omit.stat = c("f", "ser", "adj.rsq"),
          single.row = F,
          digits = 2,
          out = "multinom_frames.tex")

