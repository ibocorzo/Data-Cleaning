rm(list = ls())

library(tidyverse)
library(readxl)
library(ludridate)
library(xlsx)
library(openxlsx)
library(dplyr)
library(plyr)
#old-age dependency ratio WORLDINDATA
worldindata<- read_excel("old_age_d_r.xlsx", col_names=TRUE)
WID_data<- worldindata[,1:4]
WID_data$old_age_d_r<- as.numeric(WID_data$old_age_d_r) 
#WID_data$Year<-as.Date(as.character(WID_data$Year), format = "%Y")
countries_to_select<-c('Euro Area', 'United States' )
WID_data <- subset(WID_data, Country %in% countries_to_select)
write.xlsx(WID_data, "WID_oldage.xlsx", col.names = TRUE, row.names = FALSE)

#old-age dependency ratio OCDE
ocdedata<- read_excel("age-dependency-ratio-old.xlsx", col_names = TRUE)
ocdedata<-ocdedata[c("LOCATION", "TIME", "Value")]
ocdedata$Value<- as.numeric(ocdedata$Value) 

countries_to_select<-c('USA')
WID_data <- subset(WID_data, Entity %in% countries_to_select)

#life expectancy WB
lf_wb <- read_excel("life_exp_WB.xlsx", col_names=TRUE)
lf_wb<- lf_wb[,1:4]

#Population growth WB
pg_wb <- read_excel("population_growth.xlsx", col_names=TRUE, sheet = 3)
pg_wb<- pg_wb[,1:4]

#gini index
gini <- read_excel("gini_index_total.xlsx", col_names=TRUE)

#Public debt
public_debt <- read_excel("public_debt_total.xlsx", col_names=TRUE, sheet = 1)
public_debt<- public_debt[,1:4]

#total productivity growth wid

tfp_bbva <- read_excel("tfp_bbva.xlsx", col_names=TRUE)
multifactor <- read_excel("multifactor_prod.xlsx", col_names=TRUE, sheet=1)

#Index of capital account openness
#KA_OPEN
ka_open <- read_excel("kaopen_2020.xlsx", col_names=TRUE)
countries_to_select<-c('Austria', 'Belgium', 'Bulgaria', 'Croatia', 'Cyprus', 'Denmark', 'Estonia', 'Finland', 'France', 'Germany', 'Greece', 'Ireland', 'Italy',
                       'Latvia', 'Lithuania', 'Malta', 'Netherlands', 'Portugal', 'Slovak Republic', 'Slovenia', 'Spain', 'United States' )
ka_open <- subset(ka_open, country_name %in% countries_to_select)
ka_open <- ka_open[, c(2, 3, 4, 6)]
ka_open <- rename(ka_open, Code = ccode, Country = country_name, Year = year, Value = ka_open)
write.xlsx(ka_open, "ka_open_new.xlsx", col.names = TRUE, row.names = FALSE)

#Relative price of capital

#Churn rate: Total indsutry, construction and 

churn_rate<- read_excel("churn_rate_euroarea.xlsx", col_names=TRUE)
churn_rate <- churn_rate[churn_rate$ISIC4 != "Services of the business economy", ]
churn_rate <- churn_rate[churn_rate$Variable != "Churn rate (sum of births and deaths of employer enterprises)", ]
churn_rate$Value<- as.numeric(churn_rate$Value) 
promedio_agregado <- ddply(churn_rate, ~Time, summarize,  cr_promedio= mean(Value))
str(promedio_agregado)
promedio_agregado_ea <- cbind(promedio_agregado, Country = rep('Euro Area', 17), Code = rep('EA', 17))
write.xlsx(promedio_agregado_ea, "churn_rate_euro_area.xlsx", col.names = TRUE, row.names = FALSE)
#CREATE AN EXCEL FILE WITH ALL THE DATA

new_df <- full_join(WID_data, lf_wb, by = c("Country", "Code", "Year")) %>%
  full_join(gini, by = c("Country", "Code", "Year")) %>%
  full_join(public_debt, by = c("Country", "Code", "Year")) %>%
  full_join(tfp_bbva, by = c("Country", "Code", "Year")) %>%
  full_join(multifactor, by = c("Country", "Code", "Year")) %>%
  mutate_at(vars(Value, Value, Value, Value, Value, Value, Value), ~coalesce(., NA))

write.xlsx(new_df, "determinants.xlsx", col.names = TRUE, row.names = FALSE)

new_df2 <- merge(WID_data, lf_wb, by = c("Country", "Code", "Year"), all =TRUE)
new_df2 <- merge(new_df2, gini, by = c("Country", "Code", "Year"), all =TRUE)
new_df2 <- merge(new_df2, public_debt, by = c("Country", "Code", "Year"), all =TRUE)
new_df2 <- merge(new_df2, tfp_bbva, by = c("Country", "Code", "Year"), all =TRUE)
new_df2 <- merge(new_df2, multifactor, by = c("Country", "Code", "Year"), all =TRUE)
new_df2 <- merge(new_df2, pg_wb, by = c("Country", "Code", "Year"), all =TRUE)

write.xlsx(new_df2, "determinants_new.xlsx", col.names = TRUE, row.names = FALSE)

#plots
ggplot(data = gini, aes(x = Year, y = Value, group = Country, color = Country)) +
  geom_line() +
  labs(x = "Year",  y = "Value", color = "Country") +
  theme_bw()

#
ggplot(new_df, aes(x = gini, fill = gini)) + geom_density(alpha = 0.5)