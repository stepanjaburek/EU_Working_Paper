#----------------------------------------------------------
# Extract keywords in context
#---------------------------------
# Setup

library(tidyverse)
library(quanteda)
library(data.table)

#---------------------------------
# Data
#---------------------------------

data<-fread("eu_data.csv")

data<-data %>% filter(chair==0) # without chairs

#----------------------------------------------------------
# Vocabularies
#--------------------
# Basic EU pattern

pattern_eu <- paste0("\\b(eu|evropsk[áéý] uni[eíi]|evropskou uni[eíi])\\b")


# Full EU pattern based on Rauh and Parizek (2024)
pattern_brusel <- paste0("\\bbrusel\\w*")

# Treaties
pattern_treaties <- paste0(
  "\\b(římsk\\w*|maastrichtsk\\w*|amsterdamsk\\w*|lisabonsk\\w*) smlouv\\w*|",
  "\\bsmlouv\\w* o fungování evropsk\\w* uni\\w*"
)

# Euro, economic cooperation
pattern_euro <- paste0(  "\\beurozón\\w*|",
                           "\\beur[oa]\\b|",
                           "\\beurem\\b")

  
pattern_econ <- paste0( 
  "\\bhospodářsk\\w* a měnov\\w* uni\\w*|",
  "\\b(sgp|pakt\\w* stability a růstu)\\b")
    
pattern_parliament <- paste0("\\bevropsk\\w* parlament\\w*")

pattern_comission <- paste0("\\bevropsk\\w* komis\\w*")

pattern_council <- paste0(  "\\brad\\w* (eu|evropsk\\w* uni\\w*)\\b")

pattern_eu_council <- paste0("\\bevropsk\\w* rad\\w*")

pattern_ecb <- paste0("\\bevropsk\\w* centráln\\w* ban\\w*|",
                      "\\becb\\b")

pattern_ecj <- paste0( "\\bevropsk\\w* soudn\\w* dvůr\\b|",
                              "\\bevropsk\\w* soudn\\w* dvo\\w*")


pattern_eu_rauh_parizek <- paste0(
  pattern_euro,         # European Parliament
  "|",  
  pattern_econ,         # European Parliament
  "|",                        # OR
  pattern_ecb              # OR               # European Court of Justice
)


pattern_eu_institutions <- paste0(
  pattern_eu,
  "|",
  pattern_parliament,         # European Parliament
  "|",                        # OR
  pattern_comission,          # European Commission
  "|",                        # OR
  pattern_council,            # Council of the EU (Rada EU/Rada Evropské unie)
  "|",                        # OR
  pattern_eu_council,         # European Council (Evropská rada)
  "|",                        # OR
  pattern_ecb,                # European Central Bank / ECB
  "|",                        # OR
  pattern_ecj                 # European Court of Justice
)

print(pattern_eu_institutions)


#----------------------------------------------------------
# Mentions extraction

data_new <- data

data$eu_mentions <- str_count(data$text, regex(pattern_eu))
sum(data$eu_mentions)

data$eu_rauh_parizek_mentions <- str_count(data$text, regex(pattern_eu_rauh_parizek))
sum(data$eu_rauh_parizek_mentions)

data$treaties_mentions <- str_count(data$text, regex(pattern_treaties))
sum(data$treaties_mentions)

data$euro_mentions <- str_count(data$text, regex(pattern_euro))
sum(data$euro_mentions)

data$brusel_mentions <- str_count(data$text, regex(pattern_brusel))
sum(data$brusel_mentions)

data$econ_mentions <- str_count(data$text, regex(pattern_econ))
sum(data$econ_mentions)

data$parliament_mentions <- str_count(data$text, regex(pattern_parliament))
sum(data$parliament_mentions)

data$comission_mentions <- str_count(data$text, regex(pattern_comission))
sum(data$comission_mentions)

data$council_mentions <- str_count(data$text, regex(pattern_council))
sum(data$council_mentions)

data$eu_council_mentions <- str_count(data$text, regex(pattern_eu_council))
sum(data$eu_council_mentions)

data$ecb_mentions <- str_count(data$text, regex(pattern_ecb))
sum(data$ecb_mentions)

data$ecj_mentions <- str_count(data$text, regex(pattern_ecj))
sum(data$ecj_mentions)

data$eu_mentions <- str_count(data$text, regex(pattern_eu_institutions))
sum(data$eu_institutions_mentions)

write.csv(data,"eu_data_with_mentions.csv")
#############################################################xx
##### Now we move to key words in context (KWIC) extraction 

data$eu_mentions <- str_count(data$text, regex(pattern_eu))
sum(data$eu_mentions)


data_eu<-data %>% filter(eu_mentions>0)


# Custom function to extract context
extract_kwic_multiword <- function(text, pattern, window = 50) {
  words <- str_split(text, "\\s+")[[1]]
  word_start_positions <- str_locate_all(text, "\\S+")[[1]][, "start"]
  match_locs <- str_locate_all(str_to_lower(text), regex(pattern, ignore_case = TRUE))[[1]]
  if (nrow(match_locs) == 0) return(NULL)
  result <- map_dfr(1:nrow(match_locs), function(i) {
    match_start <- match_locs[i, "start"]
    word_index <- which(word_start_positions >= match_start)[1]
    if (is.na(word_index)) return(NULL)
    start_idx <- max(1, word_index - window)
    end_idx <- min(length(words), word_index + window)
    tibble(
      matched_word = str_sub(text, match_locs[i, "start"], match_locs[i, "end"]),
      context_full = paste(words[start_idx:end_idx], collapse = " ")
    )
  })
  
  return(result)
}


kwic_contexts <- data_eu %>%
  rowwise() %>%
  mutate(context_list = list(extract_kwic_multiword(text, pattern_eu, window = 50))) %>%
  unnest(context_list) %>%
  ungroup() %>%
  select(id, speaker, party, chair, month, year, matched_word, context_full,chamber)
unique(kwic_contexts$matched_word)

# Final dataset of 100 word windows around EU mentions
write.csv(kwic_contexts, "eu.csv")



#--------------------------

data_eu_institutions<-data %>% filter(eu_institutions_mentions>0)


kwic_contexts <- data_eu_institutions %>%
  rowwise() %>%
  mutate(context_list = list(extract_kwic_multiword(text, pattern_eu_institutions, window = 50))) %>%
  unnest(context_list) %>%
  ungroup() %>%
  select(id, speaker, party, chair, month, year, matched_word, context_full,chamber)
unique(kwic_contexts$matched_word)



kwic_contexts$bruxelles<- str_count(kwic_contexts$matched_word, regex(pattern_brusel))

kwic_contexts$treaties<- str_count(kwic_contexts$matched_word, regex(pattern_treaties))

kwic_contexts$euro<- str_count(kwic_contexts$matched_word, regex(pattern_euro))

kwic_contexts$econ<- str_count(kwic_contexts$matched_word, regex(pattern_econ))

kwic_contexts$parliament<- str_count(kwic_contexts$matched_word, regex(pattern_parliament))

kwic_contexts$comission<- str_count(kwic_contexts$matched_word, regex(pattern_comission))

kwic_contexts$council<- str_count(kwic_contexts$matched_word, regex(pattern_council))

kwic_contexts$eu_council<- str_count(kwic_contexts$matched_word, regex(pattern_eu_council))

kwic_contexts$ecb<- str_count(kwic_contexts$matched_word, regex(pattern_ecb))

kwic_contexts$ecj<- str_count(kwic_contexts$matched_word, regex(pattern_ecj))

write.csv(kwic_contexts, "eu_institutions.csv")

##################################
# SWITCH TO PYTHON FOR MACHINE TRANSLATION AND NATURAL LANGUAGE INFERENCE
##################################
