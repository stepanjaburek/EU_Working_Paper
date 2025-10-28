#----------------------------------------------------------
# Add substantively relevant variables 
#---------------------------------
# Setup

library(tidyverse)
library(quanteda)
library(data.table)
library(readxl)

#---------------------------------
# Data
#---------------------------------
setwd("C:/Users/stepa/EU_Thesis_Paper")
data <-fread("~/Documents/EU_Thesis_Paper/stance_data.csv")

inflation <-read_xlsx("~/Documents/EU_Thesis_Paper/czechia_inflation.xlsx")

data<-data %>% select(1:28)
# --------------------------------

data$month <- as.Date(paste0(data$month, "-01")) # as date

#-----------------------------------------------------------------
# add governments

data$gov <- case_when(
  data$month >=("2022-01-01") & data$month <= ("2023-12-01") ~ "Fiala",
  data$month >=("2018-07-01") & data$month <= ("2021-12-01") ~ "Babiš II",
  data$month >=("2018-01-01") & data$month <= ("2018-06-01") ~ "Babiš I",
  data$month >=("2014-02-01") & data$month <= ("2017-12-01") ~ "Sobotka",
  data$month >=("2013-08-01") & data$month <= ("2014-01-01") ~ "Rusnok",
  data$month >=("2010-07-01") & data$month <= ("2013-07-01") ~ "Nečas",
  data$month >=("2009-05-01") & data$month <= ("2010-06-01") ~ "Fischer",
  data$month >=("2007-01-01") & data$month <= ("2009-04-01") ~ "Topolánek II",
  data$month >=("2006-09-01") & data$month <= ("2006-12-01") ~ "Topolánek I",
  data$month >=("2005-05-01") & data$month <= ("2006-08-01") ~ "Paroubek",
  data$month >=("2004-08-01") & data$month <= ("2005-04-01") ~ "Gross",
  data$month >=("2002-08-01") & data$month <= ("2004-07-01") ~ "Špidla",
  data$month >=("1998-08-01") & data$month <= ("2002-07-01") ~ "Zeman",
  data$month >=("1998-01-01") & data$month <= ("1998-07-01") ~ "Tošovský",
  data$month >=("1996-07-01") & data$month <= ("1997-12-01") ~ "Klaus II",
  data$month >=("1993-01-01") & data$month <= ("1996-06-01") ~ "Klaus I",
  TRUE ~ ""
)


#-----------------------------------------------------------------
#   # add government ideologies

data$gov_ideology <- case_when(
  
  grepl("Klaus I|Klaus II|Topolánek I|Topolánek II|Nečas|Fiala", data$gov) ~ "Right_Wing",
  grepl("Zeman|Špidla|Gross|Paroubek|Sobotka", data$gov) ~ "Left_Wing",
  grepl("Babiš I|Babiš II", data$gov) ~ "New_Parties",
  grepl("Tošovský|Fischer|Rusnok", data$gov) ~ "Technocratic",
  TRUE ~ data$gov
)
unique(data$gov_ideology)

data$government <- case_when(
    (data$gov %in% c("Klaus I", "Klaus II","Topolánek I","Topolánek II","Nečas","Fiala")) & (data$party == "ODS") ~ 1,
    (data$gov %in% c("Zeman", "Špidla","Gross","Paroubek", "Sobotka")) & (data$party == "ČSSD") ~ 1,
    (data$gov %in% c("Klaus I", "Klaus II","Špidla","Gross","Paroubek","Topolánek II","Sobotka","Fiala")) & (data$party == "KDU-ČSL") ~ 1,
    (data$gov %in% c("Sobotka","Babiš I","Babiš II")) & (data$party == "ANO") ~ 1,
    (data$gov %in% c("Nečas","Fiala")) & (data$party == "TOP 09") ~ 1,
    (data$gov %in% c("Fiala")) & (data$party == "STAN") ~ 1,
    (data$gov %in% c("Fiala")) & (data$party == "Piráti") ~ 1,
    TRUE ~ 0 # Default to 0 if none of the above conditions are met
  )


#-----------------------------------------------------------------
#  add election year dummy

data$election_year<-ifelse( 
  data$year == 1996, 1, 
  ifelse(data$year == 1998, 1,
         ifelse(data$year == 2002, 1,
                ifelse(data$year == 2006, 1,
                       ifelse(data$year == 2010, 1,
                              ifelse(data$year == 2013, 1,
                                     ifelse(data$year == 2017, 1,
                                            ifelse(data$year == 2021, 1,0))))))))

data$election_month<-ifelse( 
  data$month == "1996-05-01", 1, 
  ifelse(data$month == "1998-06-01", 1,
         ifelse(data$month ==  "2002-06-01", 1,
                ifelse(data$month == "2006-06-01", 1,
                       ifelse(data$month == "2010-05-01", 1,
                              ifelse(data$month == "2013-10-01", 1,
                                     ifelse(data$month == "2017-10-01", 1,
                                            ifelse(data$month == "2021-10-01", 1,0))))))))
data$election_period <- ifelse(
  data$month >= "1995-12-01" & data$month <= "1996-05-01", 1,
  ifelse(data$month >= "1998-01-01" & data$month <= "1998-06-01", 1,
         ifelse(data$month >= "2002-01-01" & data$month <= "2002-06-01", 1,
                ifelse(data$month >= "2006-01-01" & data$month <= "2006-06-01", 1,
                       ifelse(data$month >= "2010-01-01" & data$month <= "2010-05-01", 1,
                              ifelse(data$month >= "2013-05-01" & data$month <= "2013-10-01", 1,
                                     ifelse(data$month >= "2017-05-01" & data$month <= "2017-10-01", 1,
                                            ifelse(data$month >= "2021-05-01" & data$month <= "2021-10-01", 1, 0))))))))

data$eu_election_period <- ifelse(
  data$month >= "2004-01-01" & data$month <= "2004-06-01", 1,
                ifelse(data$month >= "2009-01-01" & data$month <= "2009-06-01", 1,
                       ifelse(data$month >= "2013-12-01" & data$month <= "2014-05-01", 1,
                              ifelse(data$month >= "2018-12-01" & data$month <= "2019-05-01", 1, 0))))



data$eu_treaty_ratification_period <- ifelse(
  # Maastricht
  data$month >= as.Date("1992-02-01") & data$month <= as.Date("1993-11-01"), 1,
  #  Amsterdam
  ifelse(data$month >= as.Date("1997-10-01") & data$month <= as.Date("1999-05-01"), 1,
      # Nice
         ifelse(data$month >= as.Date("2001-02-01") & data$month <= as.Date("2003-02-01"), 1,
              # Lisbon
                ifelse(data$month >= as.Date("2007-12-01") & data$month <= as.Date("2009-12-01"), 1, 0))))




#-----------------------------------------------------------------
# Center the year, Andrew Gelman would be angry otherwise

data$year_centered <- data$year - median(data$year) # centering the year


#-----------------------------------------------------------------
# throw unnasigned MPs into Other

data$party <- case_when(
  grepl("TOP 09 a Starostové", data$party, fixed = TRUE) ~ "TOP 09",
  grepl("Nezařazení|\\(NEZ\\)|ODS \\(NEZ\\)|ČSSD \\(NEZ\\)|SPD \\(NEZ\\)|ÚSVIT \\(NEZ\\)", data$party) ~ "Other",
  grepl("Nez.-SZ", data$party) ~ "Other",
  grepl("TOP 09|TOP09", data$party) ~ "TOP 09",
  TRUE ~ data$party
)

# coalition TOP09 and STAN as only TOP09
data <- data %>% mutate(party = ifelse(party == "STAN" & year < 2016, "TOP 09", party))

data <- data %>%
  mutate(party = case_when(
    grepl("Starostové", party) ~ "TOP 09",
    TRUE ~ party
  ))


# find unique parties
unique(data$party)
# too much, will drop before graphs and stats


#------------------------------------
# designate wing as an analytical category following Jabůrek et al. (2024)

data$wing <- case_when(
  grepl("ČSSD|KSČM|LB", data$party) ~ "Left_Wing",
  grepl("ODS|KDU-ČSL|US|US-DEU|SPR-RSČ|ODA", data$party) ~ "Right_Wing",
  grepl("ANO|Piráti|STAN|SPD|Úsvit|VV|SZ|TOP 09", data$party) ~ "New_Parties",
  TRUE ~ "Other"
)


#----------------------------------
# 

data<-data %>% 
  mutate(galtan = case_when(
  data$party == "ODS" & data$year <2006 ~ 3.890000, 
  data$party == "ODS" & data$year >=2006 & data$year <2010 ~ 3.750000, 
  data$party == "ODS" & data$year >=2010 & data$year <2014 ~ 6.111111, 
  data$party == "ODS" & data$year >=2014 & data$year <2019 ~ 6.000000, 
  data$party == "ODS" & data$year >=2019 ~ 7.037037, 
  
  data$party == "ČSSD" & data$year <2006 ~ 4.690000, 
  data$party == "ČSSD" & data$year >=2006 & data$year <2010 ~ 4.860000, 
  data$party == "ČSSD" & data$year >=2010 & data$year <2014 ~ 3.578948, 
  data$party == "ČSSD" & data$year >=2014 & data$year <2019 ~ 4.428571, 
  data$party == "ČSSD" & data$year >=2019 ~ 4.923077, 
  
  data$party == "KDU-ČSL" & data$year <2006 ~ 7.560000, 
  data$party == "KDU-ČSL" & data$year >=2006 & data$year <2010 ~ 7.170000, 
  data$party == "KDU-ČSL" & data$year >=2010 & data$year <2014 ~ 8.000000, 
  data$party == "KDU-ČSL" & data$year >=2014 & data$year <2019 ~ 7.642857, 
  data$party == "KDU-ČSL" & data$year >=2019 ~ 7.777778, 
  
  data$party == "KSČM" & data$year <2006 ~ 7.820000, 
  data$party == "KSČM" & data$year >=2006 & data$year <2010 ~ 7.670000, 
  data$party == "KSČM" & data$year >=2010 & data$year <2014 ~ 5.666667, 
  data$party == "KSČM" & data$year >=2014 & data$year <2019 ~ 6.571429, 
  data$party == "KSČM" & data$year >=2019 ~ 8.074074,
  

  data$party == "ANO" &  data$year <2019 ~ 4.454545, 
  data$party == "ANO" & data$year >=2019 ~ 5.730769,
  
  data$party == "TOP 09" & data$year <2014 ~ 5.833333, 
  data$party == "TOP 09" & data$year >=2014 & data$year <2019 ~ 5.500000, 
  data$party == "TOP 09" & data$year >=2019 ~ 4.851852,
  
  data$party == "Piráti" ~ 1.000000,
  
  data$party == "SPD"~ 9.370370,
  
  data$party == "STAN" ~ 4.076923,
  TRUE ~ NA_real_  # fallback value for all other cases
  ))




#-------------------------
# EU Summits

data$month <- as.Date(paste0(data$month, "-01"))

eu_summit_dates_all <- c(
  # 1990s
  "1993-06-01", "1993-10-01", "1993-12-01",
  "1994-06-01", "1994-07-01", "1994-12-01",
  "1995-06-01", "1995-10-01", "1995-12-01",
  "1996-03-01", "1996-06-01", "1996-10-01", "1996-12-01",
  "1997-05-01", "1997-06-01", "1997-11-01", "1997-12-01",
  "1998-05-01", "1998-06-01", "1998-10-01", "1998-12-01",
  "1999-02-01", "1999-03-01", "1999-04-01", "1999-06-01",
  "1999-10-01", "1999-12-01",
  
  # 2000s
  "2000-03-01", "2000-06-01", "2000-10-01", "2000-12-01",
  "2001-03-01", "2001-06-01", "2001-09-01", "2001-10-01", "2001-12-01",
  "2002-03-01", "2002-06-01", "2002-10-01", "2002-12-01",
  "2003-02-01", "2003-03-01", "2003-04-01", "2003-06-01",
  "2003-10-01", 
  "2003-12-01",
  "2004-03-01", "2004-06-01", "2004-11-01", "2004-12-01",
  "2005-03-01", "2005-06-01", "2005-10-01", "2005-12-01",
  "2006-03-01", "2006-06-01", "2006-10-01", "2006-12-01",
  "2007-03-01", "2007-06-01", "2007-10-01", "2007-12-01",
  "2008-03-01", "2008-06-01", "2008-07-01", "2008-09-01",
  "2008-10-01", 
  "2008-11-01",
  "2008-12-01",
  "2009-03-01", 
  "2009-04-01",
  "2009-06-01",
  "2009-09-01",
  "2009-10-01",
  "2009-11-01",
  "2009-12-01",
  
  # 2010s
  "2010-02-01", 
  "2010-03-01", 
  "2010-05-01", 
  "2010-06-01", 
  "2010-09-01", 
  "2010-10-01", 
  "2010-12-01", 
  "2011-02-01", 
  "2011-03-01", 
  "2011-06-01", 
  "2011-07-01", 
  "2011-10-01", 
  "2011-12-01",  
  "2012-01-01",  
  "2012-03-01", 
  "2012-05-01", 
  "2012-06-01",  
  "2012-10-01", 
  "2012-11-01", 
  "2012-12-01", 
  "2013-02-01", 
  "2013-03-01", 
  "2013-05-01", 
  "2013-06-01", 
  "2013-10-01", 
  "2013-12-01", 
  "2014-03-01", 
  "2014-05-01", 
  "2014-06-01", 
  "2014-07-01", 
  "2014-08-01", 
  "2014-10-01", 
  "2014-12-01", 
  "2015-02-01", 
  "2015-03-01",
  "2015-04-01", 
  "2015-06-01", 
  "2015-07-01", 
  "2015-09-01", 
  "2015-10-01", 
  "2015-11-01", 
  "2015-12-01", 
  "2016-02-01", 
  "2016-03-01",  
  "2016-06-01", 
  "2016-09-01", 
  "2016-10-01", 
  "2016-12-01", 
  "2017-02-01", 
  "2017-03-01", 
  "2017-04-01", 
  "2017-06-01", 
  "2017-10-01",  
  "2017-11-01", 
  "2017-12-01",  
  "2018-03-01",  
  "2018-06-01",  
  "2018-09-01",  
  "2018-10-01", 
  "2018-12-01", 
  "2019-03-01", 
  "2019-04-01", 
  "2019-05-01", 
  "2019-06-01", 
  "2019-10-01", 
  "2019-12-01", 
  
  # 2020s
  "2020-02-01", 
  "2020-03-01", 
  "2020-04-01", 
  "2020-06-01", 
  "2020-07-01", 
  "2020-08-01", 
  "2020-10-01", 
  "2020-11-01", 
  "2020-12-01", 
  "2021-01-01", 
  "2021-02-01", 
  "2021-03-01", 
  "2021-05-01",
  "2021-06-01", 
  "2021-10-01", 
  "2021-12-01",  
  "2022-02-01", 
  "2022-03-01", 
  "2022-05-01", 
  "2022-06-01", 
  "2022-10-01", 
  "2022-12-01", 
  "2023-02-01", 
  "2023-03-01", 
  "2023-06-01", 
  "2023-10-01", 
  "2023-12-01" 
)#


summit_dates_unique <- unique(as.Date(eu_summit_dates_all))

data$eu_summit <- 0

data$eu_summit[format(data$month, "%Y-%m-01") %in% format(summit_dates_unique, "%Y-%m-01")] <- 1



#------------------------
#- Monthly inflation from the ECB data


# Clean and convert the 'Month' column in the inflation dataset
inflation$Month <- gsub('([0-9]{4})([A-Za-z]{3})', '\\1-\\2-01', inflation$Month)
inflation$Month <- gsub('Jan', '01', inflation$Month)
inflation$Month <- gsub('Feb', '02', inflation$Month)
inflation$Month <- gsub('Mar', '03', inflation$Month)
inflation$Month <- gsub('Apr', '04', inflation$Month)
inflation$Month <- gsub('May', '05', inflation$Month)
inflation$Month <- gsub('Jun', '06', inflation$Month)
inflation$Month <- gsub('Jul', '07', inflation$Month)
inflation$Month <- gsub('Aug', '08', inflation$Month)
inflation$Month <- gsub('Sep', '09', inflation$Month)
inflation$Month <- gsub('Oct', '10', inflation$Month)
inflation$Month <- gsub('Nov', '11', inflation$Month)
inflation$Month <- gsub('Dec', '12', inflation$Month)
inflation$Month <- as.Date(inflation$Month)

# Convert the 'month' column in the data dataset to Date format
data$month <- as.Date(paste0(data$month, "-01"))

# Merge data with inflation
data <- data %>%
  left_join(inflation, by = c("month" = "Month"))

# Create a quarterly date variable
inflation <- inflation %>%
  mutate(quarter = floor_date(Month, "quarter"))

# Aggregate monthly inflation to quarterly inflation
inflation_quarterly <- inflation %>%
  group_by(quarter) %>%
  summarize(quarterly_inflation = mean(Inflation, na.rm = TRUE), .groups = "drop")

# Create a lagged version of quarterly inflation
inflation_quarterly <- inflation_quarterly %>%
  mutate(lag_quarterly_inflation = dplyr::lag(quarterly_inflation))

# Merge the lagged inflation back into the data
data <- data %>%
  mutate(quarter = floor_date(month, "quarter")) %>%
  left_join(inflation_quarterly, by = "quarter")




write.csv(data,"stance_data_v2.csv")

