#----------------------------------------------------------
# Graphs
#---------------------------------
# Setup
library(flexplot)
library(tidyverse)
library(ggpubr)
library(patchwork)
library(ggbeeswarm)
library(data.table)

#---------------------------------
# Data
#---------------------------------

data<-fread("eu_data_with_mentions.csv")

data <- data_cz

total_sal <- data %>%
  group_by(year) %>%
  reframe(
    eu_salience = sum(eu_mentions >=1) / n() # clean EU salience# Rauh and Parizek salience
  ) 
country_sal <- data %>%
  group_by(country, year) %>%
  reframe(
    eu_salience = sum(eu_mentions >= 1) / n(),
    yearly_word_count = sum(word_count),
    eu_salience_per_word = (sum(eu_mentions >= 1) / yearly_word_count) * 1000
)

#----------------------------------------------------------
# Salience graphs
# Total time series

#-----------------------------------
# Figure 6.1
#------------
ggplot(total_sal, aes(x = year, y = eu_salience * 100)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(title = "Direct EU Salience Over Time",
       subtitle = "Dashed line = Czech EU accession (2004)",
       y = "% of speeches mentioned",
       x = "Year") +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "red",linewidth = 0.7) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        title = element_text(size = 16))+
  theme_pubr(base_size = 18)+
   scale_x_continuous(breaks = seq(1993, 2024, 3))





plot_df1 <- country_sal
# Create a duplicate dataframe for the grey background lines
plot_df2 <- plot_df1 %>%
  rename(country_group_for_ghosts = country) 


#-----------------------------------
# Figure 6.5
#------------
ggplot() +
  geom_line(data = plot_df2, aes(x = year, y = eu_salience * 100,
                                 group = country_group_for_ghosts),
            color = "grey",
            linetype = "solid",
            linewidth = 0.4) + #
  geom_line(data = plot_df1, aes(x = year, y = eu_salience * 100,
                                 color = country), 
            linetype = "solid",
            linewidth = 1.2) +
  scale_color_manual(values = c(   "SK" = "#3B82F6",  # muted blue
  "CZ" = "#E879F9",  # soft magenta/pink
  "PL" = "#EF4444",  # toned red
  "HU" = "#22C55E"  )) +
  facet_wrap(~country) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  labs(y = "% of speeches mentioned", x = "Year")+
       theme(axis.text = element_text(size = 18),
         plot.title = element_text(size = 20),  
        plot.subtitle = element_text(size = 14),
        title = element_text(size = 20)) 





ggplot() +
  geom_line(data = plot_df2, aes(x = year, y = eu_salience_per_word,
                                 group = country_group_for_ghosts),
            color = "grey",
            linetype = "solid",
            linewidth = 0.4) + #
  geom_line(data = plot_df1, aes(x = year, y = eu_salience_per_word,
                                 color = country), 
            linetype = "solid",
            linewidth = 1.2) +
  scale_color_manual(values = c(   "SK" = "#3B82F6",  # muted blue
  "CZ" = "#E879F9",  # soft magenta/pink
  "PL" = "#EF4444",  # toned red
  "HU" = "#22C55E"  )) +
  facet_wrap(~country) +
  theme_pubr() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  labs(y = "% of speeches mentioned", x = "Year")+
       theme(axis.text = element_text(size = 18),
         plot.title = element_text(size = 20),  
        plot.subtitle = element_text(size = 14),
        title = element_text(size = 20)) 


