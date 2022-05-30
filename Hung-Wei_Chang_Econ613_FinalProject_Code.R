#------------------
library('tidyverse')
library('dplyr')
library('stringr')
library('tidyr')
library("readr")
library('ggplot2')
library('lubridate')
library('AER')
library('plm')
library('bife')
library('naniar')
library('data.table')
#------------------


df <- read.csv('beeps_vi_es_csv.csv')



#==============================
# for our probit choice b1 (public company == 1, not public == 0)
#==============================

df  <- df%>% 
  mutate(b1_final = case_when(
    b1 == 1 ~ 1, 
    b1 > 1  ~ 0,
    b1 == -9 ~ -9
  ))

print(summary(as.factor(df$b1_final)))
df$j2_percent<-df$j2/100
df$a14y_trend<-df$a14y - 2017
df$d2_sq<- df$d2^2
df$k3a_percent<-df$k3a/100

change_dummy <- function(df, x1) {
  df[[x1]][df[[x1]] >0] <- 1
  df[[x1]][df[[x1]] <= 0] <- 0 
  df[is.na(df)] <- 0
  return (df) 
}
change_dummy2 <- function(df, x1) {
  df[[x1]][df[[x1]] =='-9'] <- 2
  df[[x1]][df[[x1]] =='1'] <- 1
  df[is.na(df)] <- 2
  return (df) 
}

df1<-change_dummy(df, 'd30b')
df2<-change_dummy(df, 'bmj4a')
df3<-change_dummy(df, 'bmj4b')
df4<-change_dummy(df, 'bmj4c')
df5<-change_dummy2(df, 'bmb4')
df6<-change_dummy2(df, 'bmb5')


df$d30b<-as.factor(df1$d30b)
df$bmj4a<-as.factor(df2$bmj4a)
df$bmj4b<-as.factor(df3$bmj4b)
df$bmj4c<-as.factor(df4$bmj4c)
df$bmb4<-as.factor(df5$bmb4)
df$bmb5<-as.factor(df6$bmb5)
summary(df$bmj4a)
summary(df$bmj4c)
summary(df$bmj4b)
summary(df$d30b)
summary(df$bmb4)
summary(df$bmb5)
summary(as.factor(df$bmj4a))
summary(as.factor(df$bmj4c))
summary(as.factor(df$bmj4b))
summary(as.factor(df$d30b))
summary(as.factor(df$bmb4))
summary(as.factor(df$bmb5))



#==============================
# filter out all the 'do not know' value (-9) 
# treat 'do not know' value as NA
#==============================

# first specify the variables that we're interested in 

df_complete <- df%>%
  replace_with_na_at(.vars = c("b1_final", "b4", 'b5', 'b7'),
                     condition = ~.x %in% common_na_numbers)
df_regulation_dummy <- df%>%
  replace_with_na_at(.vars = c("b1_final", "b4", 'b5', 'b7'),
                     condition = ~.x %in% common_na_numbers)


#==============================
# briefly check the total number of null value and summary statistics 
#==============================

print(summary(as.factor(df_complete$b1_final)))
print(summary(as.factor(df_regulation_dummy$b1_final)))


#==============================
# all categorical variables in our model
#==============================
category_list <- c( 'b1' ,'b4', 'b2a', 'b2b', 'b2c', 'b2d','bmb4','bmb5', 'country','d30b','a14y','bmj4a','bmj4b','bmj4c','j30a','j30b','j30c','j30e','j30f')

for (i in category_list){
  df_complete[[i]] = as.factor(df_complete[[i]])
}
for (i in category_list){
  df_regulation_dummy[[i]] = as.factor(df_regulation_dummy[[i]])
}



#==============================
# group by country
#==============================

# see the number of each country 
g_df_complete <-  df_complete %>% group_by(country) %>%
  summarize(number = n())
g_df_regulation_dummy <-  df_regulation_dummy %>% group_by(country) %>%
  summarize(number = n())

# see the nums of na value in each column
num_na <- as.data.frame(colSums(is.na(df)))

# see the nums of na value (the variables in our model)
for (i in category_list){
  print(i)
  print(num_na[i, ])
} 


#==============================
# Plotting the Graph in the project using ggplot 
#==============================


# Basic barplot
p<-ggplot(data=country_first_round, aes(x=as.factor(country), y=NumObs)) +geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle =- 90, vjust = 0.5)) +labs(y= "Num. of Obs.", x = "Countries")
p
# insert ggplot code
ggsave("Countries_2018_2020.png", dpi = 400  )



# Second round of survey data 
country_family_holding  <- df_complete  %>%group_by(country) %>%
  summarize(AvgFamilyHolding = mean(bmb1)) 
country_family_holding


# Basic barplot
p_2<-ggplot(data=country_family_holding, aes(x=as.factor(country), y=AvgFamilyHolding)) +geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle =- 90, vjust = 0.5)) +labs(y= "Avg. Family Holdings %", x = "Countries")
p_2
# insert ggplot code
ggsave("Countries_avg_holding.png", dpi = 400  )



# Second round of survey data 
country_time_fe  <-  df_complete  %>% group_by(country) %>% group_by(a14y) %>%
  summarize(surveytime =n()) %>% drop_na()

country_time_fe$surveytime


# Basic barplot
p_3<-ggplot(data=country_time_fe, aes(x=as.factor(country), y=surveytime)) +geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle =- 90, vjust = 0.5)) +labs(y= "Num. of Companies", x = "Survey Year")
p_3
# insert ggplot code
ggsave("Year_num.png", dpi = 400  )


#==============================
# change to factor variable 
#==============================

summary(as.factor(df_regulation_dummy$bmj4a))
summary(as.factor(df_regulation_dummy$bmj4c))
summary(as.factor(df_regulation_dummy$bmj4b))
summary(as.factor(df_regulation_dummy$d30b))
summary(as.factor(df_regulation_dummy$bmb4))
summary(as.factor(df_regulation_dummy$bmb5))



#==============================
# different specifications of models
#==============================
##################################---Model 1~4----##################################
# not specifying countries fixed effect 
##corporate governance
model0 <- glm(as.factor(b1_final) ~ as.factor(b4) + b5 + b7 , family = binomial(link = "probit"), 
              data = df_regulation_dummy)
summary(model0)

model0 <- glm(as.factor(b1_final) ~ b4 + b5 + b7 , family = binomial(link = "probit"), 
              data = df_regulation_dummy)
summary(model0)
model00 <- glm(as.factor(b1_final) ~ b4 + b5 + b7 + strata  , family = binomial(link = "probit"),  data = df_regulation_dummy)
summary(model00)
model00 <- glm(as.factor(b1_final) ~ b4 + b5 + b7 + strata + as.factor(country)  , family = binomial(link = "probit"),  data = df_regulation_dummy)
summary(model00)
model00 <- glm(as.factor(b1_final) ~ b4 + b5 + b7 + strata + as.factor(country)+as.factor(a14y)  , family = binomial(link = "probit"),  data = df_regulation_dummy)
summary(model00)



##############-----b4,bmb1,bmb4,bmb5:Corporate Governance------##############
##############-----d2,d2_sq,k3a,h5.strata:Firm Specific------##############
##############-----d30b,bmj4b,bmj4c:Regulatory------##############


model05 <- glm(as.factor(b1_final) ~ b4  + d2 + d2_sq +bmj4c  +as.factor(country) +as.factor(a14y) , family = binomial(link = "probit"),  data = df_regulation_dummy)
summary(model05)

model06 <- glm(as.factor(b1_final) ~ b4 +d2+d2_sq +b5 +b7+bmj4c+as.factor(country)+as.factor(a14y)  , family = binomial(link = "probit"),  data = df_regulation_dummy)
summary(model06)

model07 <- glm(as.factor(b1_final) ~ b4 +d2+d2_sq +b5 +b7+bmj4c+k3a+as.factor(country)+as.factor(a14y)  , family = binomial(link = "probit"),  data = df_regulation_dummy)
summary(model07)

model08 <- glm(as.factor(b1_final) ~ b4 +d2+d2_sq +b5 +b7+as.factor(bmj4c)+k3a+h5+as.factor(country)+as.factor(a14y)  , family = binomial(link = "probit"),  data = df_regulation_dummy)
summary(model08)

model09 <- glm(as.factor(b1_final) ~ b4 +d2+d2_sq +b5 +b7+as.factor(bmj4c)+k3a+h5+bmb1+as.factor(bmb4)+as.factor(bmb5)+as.factor(d30b)+as.factor(bmj4b)+as.factor(country)+as.factor(a14y)  , family = binomial(link = "probit"),  data = df_regulation_dummy)
summary(model09)

model1_3 <- bife(as.factor(b1_final) ~ as.factor(b4)+as.factor(bmj4c)+as.factor(bmj4b)+as.factor(d30b) + b5 + b7 +k3a +strata+bmb1+as.factor(bmb4)+as.factor(bmb5)+h5+as.factor(a14y) | country, df_regulation_dummy, "probit")
summary(model1_3)
ME_model1_3<- get_APEs(model1_3)
summary(ME_model1_3)





