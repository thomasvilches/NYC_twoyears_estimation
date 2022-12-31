setwd("~/PosDoc/Coronavirus/NYC_twoyears_estimation/")
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
theme_set(theme_bw())
enddate=as.Date("2022-12-30")#as.Date("2022-01-31")
startvacdate = as.Date("2020-12-12")
 basedate = as.Date("2020-09-01")

population = 8336817

# Cases --------------------------------------------------------

temp <- tempfile()
download.file("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/cases-by-day.csv",temp)
data.cases <- read.csv(temp,h=T,stringsAsFactors = F)
unlink(temp)
rm(temp)

cases = data.cases %>% mutate(inf = CASE_COUNT+PROBABLE_CASE_COUNT, date = mdy(date_of_interest)) %>% 
  select(date, inf) %>% mutate(inc_cases = inf/population*100000)

ggplot()+geom_line(data = cases,aes(x=date,y=inc_cases), color = "navyblue")
#ggplot()+geom_line(data = data.cases.test,aes(x=date,y=inc_deaths, color = state))


# Deaths --------------------------------------------------------

temp <- tempfile()
download.file("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/deaths-by-day.csv",temp)
data.cases <- read.csv(temp,h=T,stringsAsFactors = F)
unlink(temp)
rm(temp)

deaths = data.cases %>% mutate(death = DEATH_COUNT+PROBABLE_DEATH_COUNT, date = mdy(date_of_interest)) %>% 
  select(date, death) %>% mutate(inc_death = death/population*100000)

ggplot()+geom_line(data = deaths,aes(x=date,y=inc_death), color = "navyblue")
#ggplot()+geom_line(data = data.cases.test,aes(x=date,y=inc_deaths, color = state))



# Hospitalization ---------------------------------------------------------


temp <- tempfile()
download.file("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/trends/hosp-by-day.csv",temp)
data.cases <- read.csv(temp,h=T,stringsAsFactors = F)
unlink(temp)
rm(temp)

names(data.cases)

hosp = data.cases %>% mutate(hos = HOSPITALIZED_COUNT, date = mdy(date_of_interest)) %>% 
  select(date, hos) %>% mutate(inc_hosp = hos/population*100000)

ggplot()+geom_line(data = hosp,aes(x=date,y=inc_hosp), color = "navyblue")
#ggplot()+geom_line(data = data.cases.test,aes(x=date,y=inc_deaths, color = state))



# Joining data ------------------------------------------------------------


df = cases %>% inner_join(deaths, by = "date") %>% inner_join(hosp, by = "date") %>% 
  filter(date >= basedate)


write.csv(df, "nyc_data_dec2022.csv", row.names = F)


