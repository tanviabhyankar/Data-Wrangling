library(dplyr)
library(tidyverse)
library(openintro)
census_data<-openintro::census
head(census_data)
tail(census_data)
select(census_data,census_year,state_fips_code,total_family_income,age,sex,total_personal_income)
select(census_data,-race_general,-marital_status)
arrange(census_data,state_fips_code)
arrange(census_data,-state_fips_code)
rename(census_data,state=state_fips_code)
census_data<-mutate(census_data,total_income=total_family_income+total_personal_income)
View(smoking_data)
smoking_data<-openintro::smoking
View(smoking_data)
select(smoking_data,gender,age,smoke)
select(smoking_data,age,gross_income,amt_weekends,amt_weekdays)
arrange(smoking_data,-age,-gross_income,-amt_weekends,-amt_weekdays)
select(smoking_data,-ethnicity,-nationality)
rename(smoking_data,education=highest_qualification)
filter(census_data, age >= 40) |>
  View()
relocate(total_income,everything())|>
  View()
filter(smoking_data,gender=="Male")
filter(smoking_data,smoke=="No"& age==35)
filter(smoking_data,marital_status!="Divorced",nationality!="English")
filter(smoking_data,age<=20,smoke=="Yes")
filter(smoking_data,smoke=="Yes",highest_qualification=="No Qualification")
filter(smoking_data,region=="London" | region=="Wales")
smoking_data|>
  select(highest_qualification,amt_weekdays)
rename(hq=highest_qualification,smoke_weekdays=amt_weekdays)|>
  smoking_data |>
  group_by(highest_qualification) |>
  summarise(total_cigarettes=sum(amt_weekdays,na.rm=T))
smoking_data |>
  filter(age<30) |>
  arrange(-age)
smoking_data |>
  select(gross_income,amt_weekends) |>
  group_by(gross_income) |>
  summarise(total_cig=sum(amt_weekends,na.rm=T))
smoking_data |>
  group_by(gender) |>
  summarise(average=mean(age))

library(dplyr)
library(tidyverse)
library(openintro)
#diy4doitagain#
smoking_data |>
  select(highest_qualification,amt_weekdays) |>
  rename(hq=highest_qualification,smoke_weekdays=amt_weekdays)|> 
  
  #diy5#
  library(dplyr)
library(tidyverse)
library(openintro)
smoking_data|>
  mutate(
    age_category=case_when(
      age>=15 &
        age<=25 ~ "group1",
      age>=26 &
        age<=40 ~ "group2",
      age>=41 &
        age<59 ~"group3",
      age>=59 ~"group4"))