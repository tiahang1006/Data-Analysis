#### Data Set Up ####

#Set working directory#
input <- "/Users/tianchuhang/Desktop/GH811/PS3"
setwd(input)

#Read in full dataset#
smoking_raw<- read.csv("youth_smoking.csv", header=TRUE)

#Load necessary packages
library(dplyr)
library(ggplot2)

smoking_raw <- as_tibble(smoking_raw)
#View our raw data
head(smoking_raw)

#### Question 1 ####

#Question 1: How many respondents were eliminated as a result of applying the stated inclusion criteria? 
  #how many remain in the final analytic dataset? 

#Inclusion criteria: Youth who have never smoked at baseline

#Apply inclusion criteria using dplyr commands

smoking <- smoking_raw %>% 
  filter(cig_ever_1==0)

#View our subset
head(smoking)

#How many respondents were eliminated as a result of applying the stated inclusion criteria?
dim(smoking_raw)[1]-dim(smoking)[1]
      #Answer: 1552 youth were eliminated 

#How many respondents remain int eh final analytic dataset?
dim(smoking)[1]
      #Answer: 10,146 youth remain. These youth are cigarette naive

#### Question 2 ####

#Question 2: Using a nested ifelse() statement, generate a new variable 
  #that combines the race category variable with the Hispanic ethnicity variable 
  #to produce the following categories: 
  #Non-Hispanic White, Non-Hispanic Black, Hispanic, Non-Hispanic Other. 

#Race category variable ("race"): 
    # 1 = white
    # 2 = Black
    # 3 = other

#Hispanic ethnicity variable ("hisp"):
    # 0 = No, not Hispanic
    # 1 = Hispanic

#Generate new variable:
smoking$race_full <- ifelse(smoking$race==1 & smoking$hisp==0, "NH White",
                            ifelse(smoking$race==2 & smoking$hisp==0, "NH Black",
                                   ifelse(smoking$race==3 & smoking$hisp==0, "NH Other",
                                          "Hispanic")))
table(smoking$race_full)

#### Question 3 ####

#Question3: What percentage of youth reported having ever 
    #tried a cigarette after one year of follow-up?

prop.table(table(smoking$cig_ever_2))
    # 4.57% of youth in our sample reported having tried a cigarette at Wave 2

#### Question 4 ####

#Question 4: Generate descriptive statistics for your sample to go in Table 1. 
#Please include the characteristics of the whole sample as well as characteristics of
#youth who try cigarettes at Wave 2 and youth who do not.

### a) Number and percent of youth in each age category 

table(smoking$age_cat)
prop.table(table(smoking$age_cat))
    #Among whole sample: 55.8% in 12-14 years, 44.2% 15-17 years

prop.table(table(smoking$age_cat, smoking$cig_ever_2), 2)
    #Among never smoking youth: 56.8% 12-14 years, 43.2% 15-17 years
    #Among ever smoking youth: 34.7% 12-14 years, 65.3% 15-17 years

### b) Number and percent of youth who are male 

smoking %>% 
  summarise(mean(male))
    #Among whole sample: 50.9% are male

smoking %>% 
  group_by(cig_ever_2) %>% 
  summarise(mean(male))
    #Among never smoking youth: 50.9% are male
    #Among ever smoking youth: 51.1% are male

### c) Number and percent of youth in each race/ethnicity category you created previously

prop.table(table(smoking$race_full))
    #Among whole sample: 29.1% Hispanic, 14.0% NH Black, 9.1% NH Other, 47.8% NH White

prop.table(table(smoking$race_full, smoking$cig_ever_2), 2)
    #Among never smoking youth: 29.1% Hispanic, 14.2% NH Black, 9.2% NH Other, 47.6% NH White
    #Among ever smoking youth: 28.2% Hispanic, 11.1% NH Black, 8.2% NH Other, 52.6% NH White

### d) Number and percent of youth whose parent completed college or an advanced degree 

#Create a new variable for parent completing college/advanced degree
smoking$parent_college <- ifelse(smoking$parent_educ==4 | smoking$parent_educ==5,1,0)

smoking %>% 
  summarise(mean(parent_college))
    #Among whole sample: 30.7%

smoking %>% 
  group_by(cig_ever_2) %>% 
  summarise(mean(parent_college))
    #Among never smoking youth: 31.0%
    #Among ever smoking youth: 23.7%

### e) Number and percent of youth who live with a tobacco user 

smoking %>% 
  summarise(mean(live_tob_user))
    #Among whole sample: 31.5%

smoking %>% 
  group_by(cig_ever_2) %>% 
  summarise(mean(live_tob_user))
    #Among never smoking youth: 31.0%
    #Among ever smoking youth: 43.3%

### f) Number and percent of youth who have tried alcohol 

smoking %>% 
  summarise(mean(ever_alcohol))
    #Among whole sample: 31.4%

smoking %>% 
  group_by(cig_ever_2) %>% 
  summarise(mean(ever_alcohol))
    #Among never smoking youth: 30.2%
    #Among ever smoking youth: 55.4%

### g) Number and percent of youth who have ever used e-cigarettes 

smoking %>% 
  summarise(mean(ecig_ever))
    #Among whole sample: 4.2%

smoking %>% 
  group_by(cig_ever_2) %>% 
  summarise(mean(ecig_ever))
    #Among never smoking youth: 3.6%
    #Among ever smoking youth: 16.8%

#### Question 5 ####

#Describe the pattern of characteristics for youth smokers versus non-smokers in Table 1

#### Question 6 ####

#Question 6: Install ggplot2 and use this package to create some plots 
    #to visualize the prevalence of reporting having tried a cigarette at Wave 2: 

#Create new variables that are labeled versions of cig_ever_2 and male
smoking$cig_ever_new <- ifelse(smoking$cig_ever_2==1,"Ever Smoked","Never Smoked")
smoking$sex_new<-ifelse(smoking$male==1,"Male","Female")

#Correct plots for (a) and (b) in base R
barplot(prop.table(table(smoking$cig_ever_new, smoking$sex_new),2)[1,])

barplot(prop.table(table(smoking$cig_ever_new, smoking$race_full),2)[1,])

### a) Create a bar chart showing the prevalence of ever cigarette use at Wave 2 by sex 

#Create a new dataframe storing the proportions of cigarette use by sex
    #and the 95% confidence intervals for these proportions
q6a <- smoking %>%
  group_by(sex_new) %>%
  summarise(prop=sum(cig_ever_2)/length(cig_ever_2),
            low=prop.test(sum(cig_ever_2),length(cig_ever_2))$conf.int[1],
            upper=prop.test(sum(cig_ever_2),length(cig_ever_2))$conf.int[2])


#Create the plot
ggplot(q6a, aes(x=sex_new, y=prop,ymin=low,ymax=upper))+
  geom_bar(aes(fill=sex_new), stat="identity")+
  geom_errorbar()+
  labs(title="Figure 1. Prevalence of Cigarette Use at Wave 2 By Sex for US Youth", x="Sex", y="Prevalence (%)")+
  guides(fill=guide_legend(title="Cigarette Use"))+
  theme(plot.title = element_text(size=12, hjust = 0.5))+
  scale_y_continuous(labels = scales::percent)


### b) Create a bar chart showing the prevalence of ever cigarette use at Wave 2 by race/ethnicity

#Create a new dataframe storing the proportions of cigarette use by race/ethnicity
    #and the 95% confidence intervals for these proportions
q6b <- smoking %>%
  group_by(race_full) %>%
  summarise(prop=sum(cig_ever_2)/length(cig_ever_2),
            low=prop.test(sum(cig_ever_2),length(cig_ever_2))$conf.int[1],
            upper=prop.test(sum(cig_ever_2),length(cig_ever_2))$conf.int[2])

#make the plot
ggplot(q6b, aes(x=race_full, y=prop,ymin=low,ymax=upper))+
  geom_bar(aes(fill=race_full), stat="identity")+
  geom_errorbar()+
  labs(title="Figure 1. Prevalence of Cigarette Use at Wave 2 By Sex for US Youth", x="Sex", y="Prevalence (%)")+
  guides(fill=guide_legend(title="Cigarette Use"))+
  theme(plot.title = element_text(size=12, hjust = 0.5))+
  scale_y_continuous(labels = scales::percent)

### c)  Combine these plots into a single bar chart that shows prevalence of 
      #ever cigarette use by both sex and race/ethnicity

#Create a new dataframe storing the proportions of cigarette use for males by race/ethnicity 
q6c <- smoking %>%
  group_by(sex_new, race_full) %>%
  summarise(prop=sum(cig_ever_2)/length(cig_ever_2),
            low=prop.test(sum(cig_ever_2),length(cig_ever_2))$conf.int[1],
            upper=prop.test(sum(cig_ever_2),length(cig_ever_2))$conf.int[2])

#Option 1 for plot: all 8 bars in a single facet
ggplot(q6c,aes(as.factor(race_full),fill=factor(sex_new),y=prop,ymin=low,ymax=upper))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(position="dodge")+
  ggtitle("Fig 3. Prevalence of cigarette initiation by sex and race")+
  xlab("race/ethnicity")+ylab("% initiating cigarettes") + 
  scale_y_continuous(labels = scales::percent)+
  theme(plot.title = element_text(hjust = 0.5))

#Option 2 for plot: two facets
ggplot(q6c,aes(as.factor(race_full),fill=factor(race_full),y=prop,ymin=low,ymax=upper))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(position="dodge")+
  facet_wrap("sex_new") +
  ggtitle("Fig 3. Prevalence of cigarette initiation by sex and race")+
  xlab("race/ethnicity")+ylab("% initiating cigarettes") + 
  scale_y_continuous(labels = scales::percent)+
  theme(plot.title = element_text(hjust = 0.5))


#### Question 7 ####

#Question 7: Investigate the association between living with a tobacco user and 
#trying cigarettes at Wave 2 using a logistic regression.
#Report the odds ratio, 95% confidence intervals, and p-value. Interpret your results

#run the logistic regression
model_1 <-glm(cig_ever_2~live_tob_user,family=binomial(link="logit"),data=smoking)

#look at the summary for this model
summary(model_1)

#Get the OR and the 95% CI
exp(cbind(OR = coef(model_1), confint(model_1))) 

    #Answer:
        # OR= 1.70 
        # 95% CI= (1.41, 2.06)
        # p-value= <0.001

    #Interpretation: cigarette naive youth who live with a tobacco user had 1.7 times the odds
          # of trying cigarettes after 1 year of follow up compared with youth who 
          #do not live with a tobacco user. 

#### Question 8 ####

#Question 8: Use a multivariable logistic regression to identify predictors 
#of cigarette initiation. Include all the variables from Table 1. 
#Present results (odds ratios, 95% confidence intervals, p-values) in Table 2

#Run the regression 
model_2 <- glm(cig_ever_2~as.factor(age_cat)+male+parent_college +ever_alcohol+
                 ecig_ever+as.factor(race_full) +live_tob_user,
               family=binomial(link="logit"),data=smoking) 

#look at the summary for this model
summary(model_2)

#Get the OR and the 95% CI
exp(cbind(OR = coef(model_2), confint(model_2))) 

    #Answer:
        # See table 2

#### Question 9 ####

#Question 9: Describe the results in table 2

#9A: Which characteristics are significant predictors?
    # older age, sex, parental education, alcohol use, 
    #ecigarette use, living with a tobacco user

#9B:  Choose 3 significant predictors of cigarette initiation 
#and interpret their odds ratios in sentence form 

    #Answers will vary

    #Sample answer: after adjusting for sex, parental education, 
    #alcohol use, ecigarette use, and race/ethnicity, cigarette naive youth who 
    #live with a tobacco user had 1.5 times the odds (95% CI: 1.22, 1.81) 
    # of trying cigarettes after 1 year of follow up compared with youth who 
    #do not live with a tobacco user. 


