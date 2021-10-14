install.packages("tidyverse")
library(tidyverse)

install.packages("dplyr")
library(dplyr)

#remove all existing variables
rm(list=ls())

#loading data
survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

#equal to (either)
survey <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

survey%>%head()

survey_USD<-survey%>%filter(currency=="USD")

#select observations with annual salary above or equal to 50K using usd data
survey_USD<-survey%>%
  filter(
    annual_salary>=50000
    )

survey_USD%>%
  filter(
    annual_salary > 50000  
  )

#dataset with USD female
survey_USD_female<-survey%>%
  filter(
  currency=="USD" & 
    gender=="Woman"
)

#dataset with AUD or female |
survey_AUD_female<-survey%>%
  filter(
    currency=="AUD" | 
      gender=="Woman"
  )

survey%>%select(timestamp, gender, currency)

#Work with salary filter
survey_USD%>%arrange(annual_salary)

min(survey_USD$annual_salary)

#What is the earliest date of the survey 
min(survey_USD$timestamp)

#What is the latest date of the survey 
max(survey_USD$timestamp)

#select observations with annual salary greater than 0 and save it under the same name
survey_USD<-survey_USD%>%
  filter(
    annual_salary > 0 
  )

#sort in decending order 
survey_USD%>%arrange(
  desc(annual_salary)
)

#what are industries in the dataset
survey_USD%>%distinct(industry)

#how many under each industry 
survey_USD%>%count(industry,sort=TRUE)

#how many under each industry & gender
survey_USD%>%count(industry,gender,sort=TRUE)

survey_USD%>%count(gender,industry,sort=TRUE)

#how many responses from each age category and gender
survey_USD%>%count(how_old_are_you, gender, sort=TRUE)

#Group_by
survey_USD_grouped<-survey_USD%>%
  group_by(gender)

#Mutate ()- create new variable 
survey_USD_avr<-survey_USD%>%
  mutate(
    avr_salary=mean(annual_salary)
    )
#Combine 
survey_USD_grouped<-survey_USD%>%
  group_by(gender)%>%
  mutate(
    avr_salary=mean(annual_salary)
  )
#Calculate average salary for gender in each industry 
survey_USD_grouped<-survey_USD%>%
  group_by(gender, industry)%>%
  mutate(
    avr_salary=mean(annual_salary)
  )

#transmute function
survey_total<-
  survey_USD%>%
  transmute(total_salary=annual_salary+other_monetary_comp)

survey_total<-
  survey_USD%>%
  transmute(total_salary=annual_salary+as.numeric(other_monetary_comp))

#Calculate average salary for gender in each industry 
survey_USD_grouped<-survey_USD%>%
  group_by(gender, industry)%>%
  mutate(
    avr_salary=mean(annual_salary,
    min_salary=min(annual_salary),
    max_salary=max(annual_salary)
  ))%>%
  ungroup()
  
#transmute function
  survey_total<-
    survey_USD%>%
    group_by(industry)%>%
    transmute(
      total_salary=annual_salary+as.numeric(other_monetary_comp),
    min_salary=min(annual_salary),
    max_salary=max(annual_salary)
  )

#summarise
survey_USD_summars<-survey_USD%>%
  group_by(gender, industry)%>%
  summarise(
      avr_salary=mean(annual_salary),
      min_salary=min(annual_salary),
      max_salary=max(annual_salary)
      )%>%
    ungroup()
 
#parse_number

#as.numeric 
survey_USD%>%
  parse_number(how_old_are_you)

class(survey_USD$how_old_are_you)

#case_when