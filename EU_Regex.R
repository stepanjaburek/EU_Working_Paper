#----------------------------------------------------------
# Extract keywords in context
#---------------------------------
# Setup

library(tidyverse)
library(quanteda)
library(data.table)
library(flexplot)

#---------------------------------
# Data
#---------------------------------
data_sk<-fread("~/Documents/EU_Data_Working_Paper/SK_speeches.csv")
data_cz<-fread("~/Documents/EU_Data_Working_Paper/CZ_speeches.csv")
#data_cz_our<-fread("~/Documents/EU_Thesis_Paper/eu_data.csv")
data_hu <- fread("~/Documents/EU_Data_Working_Paper/HU_speeches.csv")
data_pl <- fread("~/Documents/EU_Data_Working_Paper/PL_speeches.csv")



#----------------------------
# Vocabularies
#--------------------
# Basic EU pattern

data_sk<-data_sk %>% filter(chair==0) # without chairs
data_sk$speech_text <- tolower(data_sk$speech_text)


#pattern_eu_sk <-"\\beú\\b" # acronym
#phrase_eu_sk <- "\\beurópsk\\w* úni\\w*"

pattern_eu_sk <- "\\b(eú|európsk\\w* úni\\w*)\\b"



data_sk$eu_mentions <- str_count(data_sk$speech_text, regex(pattern_eu_sk))
sum(data_sk$eu_mentions)


data_hu$date <- as.Date(data_hu$date)

data_sk <- data_sk %>%
  mutate(year = as.numeric(format(date, "%Y")))

data_sk$word_count <- str_count(data_sk$speech_text, boundary("word"))

#------------------
# Czechia
data_cz<-data_cz %>% filter(chair==0) # without chairs
data_cz$speech_text <- tolower(data_cz$speech_text)


#pattern_eu_cz <-"\\beú\\b" # acronym
#phrase_eu_cz <- "\\beurópcz\\w* úni\\w*"

pattern_eu_cz <- "\\b(eu|evropsk\\w* uni\\w*)\\b"

data_cz$eu_mentions <- str_count(data_cz$speech_text, regex(pattern_eu_cz))
sum(data_cz$eu_mentions)



data_cz <- data_cz %>%
  mutate(year = as.numeric(format(date, "%Y")))

data_cz$word_count <- str_count(data_cz$speech_text, boundary("word"))

#------------------
# Hungary 
data_hu<-data_hu %>% filter(chair==0) # without chairs
data_hu$speech_text <- tolower(data_hu$speech_text)


pattern_eu_hu <- "\\b(eu|európa\\w* uni\\w*)\\b"


data_hu$eu_mentions <- str_count(data_hu$speech_text, regex(pattern_eu_hu))
sum(data_hu$eu_mentions)

data_hu$date <- as.Date(data_hu$date)

data_hu <- data_hu %>%
  mutate(year = as.numeric(format(date, "%Y")))

data_hu$word_count <- str_count(data_hu$speech_text, boundary("word"))

#------------------
# Poland
data_pl<-data_pl %>% filter(chair==0) # without chairs
data_pl$speech_text <- tolower(data_pl$speech_text)


pattern_eu_pl <- "\\b(ue|uni\\w* europejsk\\w*)\\b"


data_pl$eu_mentions <- str_count(data_pl$speech_text, regex(pattern_eu_pl))
sum(data_pl$eu_mentions)

data_pl <- data_pl %>%
  mutate(year = as.numeric(format(date, "%Y")))

data_pl$word_count <- str_count(data_pl$speech_text, boundary("word"))


data_sk$country <- "SK"
data_cz$country <- "CZ"
data_hu$country <- "HU"
data_pl$country <- "PL"

data<-rbind(data_sk, data_cz, data_hu, data_pl, fill=TRUE)

write.csv(data_sk, "data_sk_mentions.csv")
write.csv(data_cz, "data_cz_mentions.csv")
write.csv(data_hu, "data_hu_mentions.csv")
write.csv(data_pl, "data_pl_mentions.csv")

write.csv(data, "data_v4_mentions.csv")






#-----------------
# kwics


# Create corpus
corpus_speeches <- corpus(
  data$speech_text,
  docvars = data.frame(
    id = data$speech_id,
    speaker= data$speaker,
    date = data$date
  ))

# Tokenize
tokens_speeches <- tokens(corpus_speeches) 



kwic_eu <- kwic(tokens_speeches, pattern = pattern_eu_sk, valuetype = "regex", window = 50) # acro
kwic_eu2 <- kwic(tokens_speeches, pattern = phrase(phrase_eu), window = 50)

kwiceu <- kwic(
  tokens_speeches,
  pattern = c("európsk\\w* úni\\w*"),
  valuetype = "regex",
  window = 50
)







imf <- data %>%  filter(imf_mentions>0)

imfo <- imf %>% 
  group_by(year) %>% 
  reframe(wto_year = sum(imf_mentions))

flexplot(wto_year~year,imfo)

wto <- data %>%  filter(wto_mentions>0)

write.csv(wto,"wto_data.csv")

wtoo <- wto %>% 
  group_by(year) %>% 
  reframe(wto_year = sum(wto_mentions))

flexplot(wto_year~year,wtoo)
#############################################################xx


##### Now we move to key words in context (KWIC) extraction 
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


kwic_contexts <- imf %>%
  rowwise() %>%
  mutate(context_list = list(extract_kwic_multiword(text, pattern_imf, window = 50))) %>%
  unnest(context_list) %>%
  ungroup() %>%
  select(id, speaker, party, chair, month, year, matched_word, context_full,chamber)
unique(kwic_contexts$matched_word)

# Final dataset of 100 word windows around EU mentions
write.csv(kwic_contexts, "wto_kwic.csv")

