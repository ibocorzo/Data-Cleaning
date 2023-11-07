rm(list = ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(openxlsx)
library(dplyr)
library(plyr)
library(panelr)
renewable_share <- read_excel("renewable_primary_share.xlsx", col_names=TRUE, sheet = 1)
countries_to_select<-c('Austria', 'Belgium', 'Bulgaria', 'Switzerland', 'Croatia', 'Cyprus', 'Czech Republic', 'Denmark', 'Estonia', 'Finland', 'France', 'United Kingdom', 'Germany', 'Greece', 'Hungary', 'Ireland', 'Italy',
                       'Iceland', 'Latvia', 'Lithuania', 'Luxembourg', 'Malta', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Sweden', 'Spain' )
renewable_share <- subset(renewable_share, country %in% countries_to_select)
renewable_share <- filter(renewable_share, year >= 1985)
write.xlsx(renewable_share, "renewable_primary_share1.xlsx", col.names = TRUE, row.names = FALSE)

data_r <- read_excel("Data_r.xlsx", col_names=TRUE, sheet = 3)
df_nuevo <- merge(data_r, renewable_share[, c("country", "year", "Renewables")], by = c("country", "year"), all.x = TRUE)
#df_nuevo <- df_original %>%
# left_join(df_datos %>% select(Pais, Año, valores), by = c("Pais", "Año")) %>%
#  mutate(valores = ifelse(is.na(valores), 0, valores))
write.xlsx(df_nuevo, "data_r2.xlsx", col.names = TRUE, row.names = FALSE)


generation_total <- read_excel("eia_renewables_total.xlsx", col_names=TRUE, sheet = 5)
#long way to convert character variables into numeric
#generation_total$consumption_total<-as.numeric(generation_total$consumption_total)
#generation_total$generation_renew<-as.numeric(generation_total$generation_renew)
#generation_total$generation_total<-as.numeric(generation_total$generation_total)
#generation_total$generation_non_renew<-as.numeric(generation_total$generation_non_renew)
#Short way
#df <- df %>% mutate_at(c('assists', 'rebounds'), as.numeric)
#df <- df %>% mutate_if(is.character, as.numeric)
names <- c('consumption_total','generation_renew', 'generation_total', 'generation_non_renew')
generation_total[,names] <- lapply(generation_total[,names],as.numeric)

countries_to_select<-c('Austria', 'Belgium', 'Bulgaria', 'Switzerland', 'Cyprus',  'Czech Republic','Germany', 'Denmark', 'Spain', 'Estonia', 'Finland', 'France', 'United Kingdom', 'Greece', 'Croatia','Hungary', 'Ireland', 'Iceland', 'Italy',
                        'Lithuania','Luxembourg', 'Latvia',  'Malta', 'Netherlands', 'Norway', 'Poland', 'Portugal', 'Romania', 'Slovakia', 'Slovenia', 'Sweden')
#SORT column of countries of a dataframe based in the order of a created list on countries
# Convert 'Country' column to factor with specified order
generation_total$country <- factor(generation_total$country, levels = countries_to_select, ordered = TRUE)
# Sort the dataframe based on the 'Country' column
generation_total <- generation_total[order(generation_total$country), ]

generation_total <- subset(generation_total, country%in% countries_to_select)
generation_total <- filter(generation_total, year >= 1985)
write.xlsx(generation_total, "renewable_eia_data_new.xlsx", col.names = TRUE, row.names = FALSE)
ee<-generation_total$country%in% countries_to_select
print(ee)
#------LOOP FOR PLOTS----------
#Total consumption
# Create line plots for each country
countries <- unique(generation_total$country)
for (i in countries){
  # Filter data for the current country
  each_country <- subset(generation_total, country == i)
  plot<- ggplot(each_country, aes(x = year, y = consumption_total)) +
    geom_line()+
    labs(title = paste("Line Plot for", i),
         x = "Year",
         y = "Consumption") +
    theme_minimal()
  print(plot)
}
#Renewable and Non renewable
for (i in countries){
  # Filter data for the current country
  each_country <- subset(generation_total, country == i)
  plot<- ggplot(each_country, aes(x = year)) +
    geom_line(aes(y = generation_renew, color = "steelblue", label ="Renewable")) +
    geom_line(aes(y = generation_non_renew, color = "firebrick3", label = "Non Renewable")) +
    labs(title = paste("Line Plot for", i),
         x = "Year",
         y = "electricity generation") +
    theme_minimal()
  print(plot)
}


loop_countries <- filter(generation_total, country=="Belgium")
my_cols <- c("steelblue", "gold", "firebrick", "forestgreen", 
             "orange", "chocolate2", "grey", "black", "chartreuse", "cyan", "coral2", "blueviolet", "brown1", "limegreen", "maroon2", "lightgoldenrod2", "dodgerblue4", "firebrick3", "lawngreen", "indianred3", "mediumblue",
             "purple", "yellow", "turquoise", "violetred2", "sienna3", "moccasin", "orangered", "salmon", "navyblue", "slategray4")
palette(my_cols)
ggplot(loop_countries, aes(x = year, y = consumption_total, group = country, color = country)) +
  geom_line() +
  scale_colour_manual(values=my_cols)
  labs(title = "Line Plots for Each Country",
       x = "Year",
       y = "Value") +
  theme_minimal()

data_r4 <- read_excel("Data_r4.xlsx", col_names=TRUE, sheet = 1)
df_nuevo <- merge(data_r4, generation_total[, c("country", "year", "generation_total")], by = c("country", "year"), all.x = TRUE)
#df_nuevo <- df_original %>%
# left_join(df_datos %>% select(Pais, Año, valores), by = c("Pais", "Año")) %>%
#  mutate(valores = ifelse(is.na(valores), 0, valores))
write.xlsx(df_nuevo, "data_r4.xlsx", col.names = TRUE, row.names = FALSE)


#CUT THE SAMPLE IN 1990
data_cut <- read_excel("data_stata.xlsx", col_names=TRUE)
data_cut <- filter(data_cut, year >= 1990)
