
##### BLOCK 1
install.packages(c('data.table', 'plyr', 'dplyr', 'magrittr', 'tidylog', 'nlme', 'multcomp'))

library(data.table)
library(plyr)
library(dplyr)
library(magrittr)
library(tidylog) # very helpful for coding
library(nlme)
library(multcomp)
participants_data_long=fread('https://raw.githubusercontent.com/kfitzg13/IMSVisual/main/participants_data_long.csv')
# for the mixed effects models
## This prints out the first 15 lines of the data frame to get a sense of how it is structured.
head(participants_data_long, 15)
# this sets the reference group to be rrms (typically set to be the most common category)
participants_data_long %<>% mutate(mstype=factor(mstype, levels=c('rrms', 'pms')))


## this block of code orders the dataset by ID and by time with the arrange() command
## it then selects the first instance of each ID. 
participants_baseline= participants_data_long %>% arrange(ID, time) %>% 
  distinct(ID, .keep_all=TRUE)

## This prints out the dimensions of the data frame
dim(participants_baseline)

## This prints out the first 6 lines of the data frame
head(participants_baseline)

## we use the baseline dataset
## this will print out mean and sd statistics for progressive participants
participants_baseline %>% filter(mstype=='pms') %>% dplyr::select(age) %>% unlist() %>% mean()
participants_baseline %>% filter(mstype=='pms') %>% dplyr::select(age) %>% unlist() %>% sd()

## now we switch back to the longitudinal dataset
## this groups the participants by ID and then calculate the maximum time minus the minimum time 
## this calculate the average per eye follow up
follow_up_data=participants_data_long %>%
  group_by(ID, eye) %>%
  summarize(follow_up_period = max(time) - min(time)) %>%
  ungroup()

follow_up_data %>% dplyr::select(follow_up_period) %>% unlist() %>% mean()


##### BLOCK 2
# read in cross-sectional data
cross_sectional_data=fread('https://raw.githubusercontent.com/kfitzg13/IMSVisual/main/cross_sectional_data.csv')

## inspect the data to ensure it is as expected
cross_sectional_data %<>% arrange(ID)

# this sets the reference group to be rrms 
cross_sectional_data %<>% mutate(mstype=factor(mstype, levels=c('rrms', 'pms')))
model.1=lme(GCIPL~age+factor(sex)+factor(mstype), 
        random=~1|ID, na.action=na.omit, data=cross_sectional_data)

summary(model.1)
## 95% CI lower bound: -3.81033 - 1.96*1.636977
## 95% CI upper bound: -3.81033 + 1.96*1.636977

##### BLOCK 3
# read in longitudinal data
participants_data_long=fread('https://raw.githubusercontent.com/kfitzg13/IMSVisual/main/participants_data_long.csv')
head(participants_data_long)
# this sets the reference group to be rrms
participants_data_long %<>% mutate(mstype=factor(mstype, levels=c('rrms', 'pms')))

## inspect the data to ensure it is as expected
participants_data_long %<>% arrange(ID, time)
head(participants_data_long, 15)

## 95% CI lower bound: -3.81033 - 1.96*1.636977
## 95% CI upper bound: -3.81033 + 1.96*1.636977

model.2=lme(GCIPL~age+factor(sex)+factor(mstype)+time+time*factor(mstype),
       random=list(ID =pdDiag(~1), eye=~time),
       na.action=na.omit, data=participants_data_long, 	method="REML")
summary(model.2)
coefs <- rep(0, length(coef(model.2)))
names(coefs)=names(coef(model.2))
coefs["time"]=1
coefs["factor(mstype)pms:time"]=1
coefs
K=matrix(coefs,1)
K
r1=summary(glht(model.2, linfct=K));
r1
## 95% CI lower bound: -0.6434 - 1.96*0.1129
## 95% CI upper bound: -0.6434 + 1.96*0.1129





