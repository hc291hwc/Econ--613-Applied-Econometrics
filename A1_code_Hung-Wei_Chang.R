
#------------------
library('tidyverse')
library('dplyr')
library('stringr')
library('tidyr')
library("readr")

#------------------
# Reading data 

datjss <- read.csv('datjss.csv')
datsss <- read.csv('datsss.csv')
datstu <- read.csv('datstu.csv')

#=========================
#Exercise 1
#=========================

#------------------------
# Number of students = 340823
#there are no rows with all values and identical. 
#------------------------

datstu_withoutNA <- filter(datstu, is.na(datstu))
datstu_distinct <- distinct(datstu)
datstu_distinct[datstu_distinct==""]<-NA

sum(is.na(datstu$X))
nrow(datstu_distinct)


#------------------------
# Number of schools = 898
#------------------------

# Group by school code and school name and sum up by count function n()

g_schools_byName <- datsss %>%
  group_by(schoolname) %>%
  dplyr::summarize(n = n())
nrow(g_schools_byName)

g_schools_byCode <- datsss %>%
  group_by(schoolcode) %>%
  dplyr::summarize(n = n())
nrow(g_schools_byCode)

# Or just calculate distinct values by 'schoolname' and 'schoolcode' columns 
n_schools_byName  <- dplyr::n_distinct(datsss$schoolname) # 842 
n_schools_byCode <- dplyr::n_distinct(datsss$schoolcode) #898 


#------------------------
# Number of programs = 165
#------------------------

# create 2 lists to loop from, 
# schools = [ schoolcode1 ~ schoolcode6]
# programs = [ choice1 ~ choice6 ]
schools = datstu[5:10]
programs = datstu[11: 16]

# Create an empty data frame, group by different choicepgm, 
# then bind rows to the same data frame named as choice_program

choice_programs <- data.frame()

for (i in 1:6 ){
  g_school_and_program <- datstu  %>%
    group_by(programs[i]) %>%
    dplyr::summarize(n = n())
  colnames(g_school_and_program) <- c('choicepgm', 'n')
  choice_programs = dplyr::bind_rows(choice_programs, g_school_and_program)
}

# clear the choice_programs data frame
# remove distinct values, and place empty strings 
# count the nrow then we can know how many programs 

choice_programs <- distinct(choice_programs)
choice_programs[choice_programs==""] <- NA
which(is.na(choice_programs))
choice_programs <- drop_na(choice_programs)
sum(is.na(choice_programs))
nrow(choice_programs)


#------------------------
# Number of  choices (School and Program) = 2773, after dropping the NA values 
#------------------------

schools = datstu[5:10]
programs = datstu[11: 16]

# Create an empty data frame, group by schoolcode, choicepgm, 
# then bind rows to the same data frame named as potential_choices

potential_choices <- data.frame()

for (i in 1:6 ){
  g_school_and_program <- datstu  %>%
    group_by(schools[i], programs[i]) %>%
    dplyr::summarize(n = n())
  colnames(g_school_and_program) <- c('schoolcode', 'choicepgm')
  potential_choices = dplyr::bind_rows(potential_choices, g_school_and_program[1:2])
}


# clear the potentail_choices data frame
# remove distinct values, and place empty strings 
# count the nrow then we can know how many programs 

potential_choices <- distinct(potential_choices)
potential_choices[potential_choices==""] <- NA
which(is.na(potential_choices))
potential_choices <- drop_na(potential_choices)
sum(is.na(potential_choices))
nrow(potential_choices)


#------------------------
# How many missing test scores = 179887 
#------------------------

# summing over the null value in the 'score' column 
sum(is.na(datstu$score))


#------------------------
# numbers of students that apply to the same school = 134122
#------------------------

# select and transpose the data frame that is related to schoolcode1 to schoolcode6
# fill in the potential empty sting with 'NA'

schools_df <- as.data.frame(t((datstu[, 5:10])))
schools_df[schools_df ==""] <- NA

# calculate the distinct value number in each column(each school that the student applied to)
# filter out those that applied to less than 6 schools, ie, applied to the same school 
# calculate the length of those less than 6  

num_schools_applied <- sapply(schools_df, function(x) n_distinct(x, na.rm = TRUE))
num_morethan1_school <- num_schools_applied[num_schools_applied < 6]
length(num_morethan1_school)



#------------------------
# Apply to less than 6 choices = 321639
#------------------------

# select the data frame with columns choicepgm1 to choicepgm6 
# fill in empty sting 
program_df <- as.data.frame((datstu[, 11:15]))
program_df[program_df==""]<-NA 

# drop the rows with NA values
# calculate the remaining row numbers, ie, the number of students that applied to less than 6
#  choices 

lessthan6_program_df <- drop_na(program_df)
nrow(lessthan6_program_df)


#=========================
# Exercise 2 Data
#=========================


# create a school_name_for_merging df, so that it could be merge later, 
# group by schoolcode, schoolname, sssdistrict, ssslong, ssslat 

school_name_for_merging <- datsss %>%
  group_by(  schoolcode, schoolname, sssdistrict, ssslong, ssslat) 

school_name_for_merging[school_name_for_merging==""]<-NA 
school_name_for_merging<-  na.omit(school_name_for_merging) 
school_name_for_merging <- distinct(school_name_for_merging, schoolcode)
school_name_for_merging <- distinct(school_name_for_merging, schoolname)



# 2 lists for looping 

schools = datstu[5:10]
programs = datstu[11: 16]


# Create an empty data frame, group by schools[i], programs[i], rankplace, 
# then bind rows to the same data frame named as choice_program

datstu_for_merging <- data.frame()

for (i in 1:6 ){
  g_school_and_program <- datstu  %>%
    group_by(schools[i], programs[i], rankplace) %>%
    dplyr::summarize(nstudents = n())
  g_school_and_program <- filter(g_school_and_program, rankplace == i )
  
  colnames(g_school_and_program)<- c('schoolcode', 'choicepgm', 'rank', 'nstudents' )
  datstu_for_merging <- dplyr::bind_rows(datstu_for_merging, g_school_and_program)
}

# group again by only schoolcode, choicepgm, so we can sum over different admitted ranks,
# and calculate the total admitted number

datstu_for_merging <- datstu_for_merging %>%
  group_by(schoolcode, choicepgm) %>%
  dplyr::summarize(nstudents = sum(nstudents))

sum(is.na(datstu_for_merging))
 

# Create an empty data frame, group by schools[i], programs[i], rankplace, 
# in the summarize function, calculate the cutoff, quality_sum, quality for future merging
# then bind rows to the same data frame named as choice_program

cutoff_quality_merging <- data.frame()

for (i in 1:6 ){
  g_school_and_program <- datstu  %>%
    group_by(schools[i], programs[i], rankplace) %>%
    dplyr::summarize(cutoff = min(score),
                     quality_sum = sum(score),
                     quality = mean(score),
                     nstudents = n())
  g_school_and_program <- filter(g_school_and_program, rankplace == i )
  
  colnames(g_school_and_program)<- c('schoolcode', 'choicepgm', 'rank', 'cutoff', 
                                     'quality_sum', 'quality', 'nstudents' )
  cutoff_quality_merging <- dplyr::bind_rows(cutoff_quality_merging, g_school_and_program)
}

# group the cutoff_quality_merging by schoolcode, choicepgm, then calculate the 
#  the cutoff, quality_sum, quality, the number of student again for the same rank 

cutoff_quality_merging <- cutoff_quality_merging %>%
  group_by(schoolcode, choicepgm) %>%
  dplyr::summarize(cutoff = min(cutoff),
                   quality_sum = sum(quality_sum),
                   quality = sum(quality_sum)/ sum(nstudents),
                   nstudents = sum(nstudents))


# joinging the datstu_for_merging and cutoff_quality_merging
# left_join by the same column 'schoolcode', 'choicegpm', 'nstudents'
datstu_students_cutoff_quality <- left_join(datstu_for_merging, cutoff_quality_merging, 
                                            by =  c('schoolcode' =  'schoolcode' ,
                                                    'choicepgm' = 'choicepgm',
                                                    'nstudents' = 'nstudents'))



# joinging the data with school_name_for_mergin 
eg2_data_final <- left_join(datstu_students_cutoff_quality, school_name_for_merging,
                            by = c('schoolcode' = 'schoolcode' ))

# remove potential duplicated rows produced by left_join
eg2_data_final <- distinct(eg2_data_final)
# reorder the columns
eg2_data_final <- eg2_data_final[c("schoolcode", 'schoolname', "choicepgm", "sssdistrict",
                                   'ssslat','ssslong','cutoff', 
                                   'quality', 'nstudents', 'quality_sum' )]


# remove unnecessary column
eg2_data_final <- select(eg2_data_final, -c(quality_sum) )
print(eg2_data_final[1:20, ])

#=========================
# Exercise 3 
#=========================


# create 2 lists to loop 

schools = datstu[5:10]
programs = datstu[11: 16]

# create an empty df to bind all the grouped rows into 
# the rows are grouped into 'schoolcode', 'choicepgm', 'jssdistrict', 'rank', 'nstudents'

datstu_school_program_jssdistrict <- data.frame()

for (i in 1:6 ){
  g_school_and_program <- datstu  %>%
    group_by(schools[i], programs[i], jssdistrict, rankplace) %>%
    dplyr::summarize(nstudents = n())
  g_school_and_program <- filter(g_school_and_program, rankplace == i )
  
  colnames(g_school_and_program)<- c('schoolcode', 'choicepgm', 'jssdistrict', 'rank', 
                                     'nstudents')
  datstu_school_program_jssdistrict <- dplyr::bind_rows(datstu_school_program_jssdistrict, 
                                                        g_school_and_program)
}


# joining datstu_school_program_jssdistrict with datjss

datstu_school_program_jssdistrict <- left_join(datstu_school_program_jssdistrict, datjss)

datstu_school_program_jssdistrict <- distinct(datstu_school_program_jssdistrict)

eg3_school_program_level_data<- left_join( eg2_data_final,
                                                    datstu_school_program_jssdistrict,
                                                    by = c ('schoolcode', 'choicepgm'))

# Checking Na values and dropping them 

which(is.na(eg3_school_program_level_data))


eg3_school_program_level_data <- distinct(eg3_school_program_level_data)
eg3_school_program_level_data[eg3_school_program_level_data==""] <- NA
which(is.na(eg3_school_program_level_data))
eg3_school_program_level_data <- drop_na(eg3_school_program_level_data)
sum(is.na(eg3_school_program_level_data))


eg3_school_program_level_data <- mutate(eg3_school_program_level_data, 
                                        (69.172*(ssslong - point_x)*cos(point_y/57.3))^2 
                                        + (69.172* (ssslat - point_y))^2 )
colnames(eg3_school_program_level_data)[16] = c('distance')


eg3_school_program_level_data <-  select(eg3_school_program_level_data, -c(rank, nstudents.y, X))
eg3_for_printing <- select(eg3_school_program_level_data, c(sssdistrict, ssslong, ssslat, 
                                                            jssdistrict,point_x, point_y, distance))
print(eg3_for_printing[1:20,])

#==========================
# Exercise 4 
#==========================


# 2 lists for looping 

schools = datstu[5:10]
programs = datstu[11: 16]


# Create an empty data frame, group by schools[i], programs[i], rankplace, 
# then bind rows to the same data frame 

datstu_school_program_jssdistrict <- data.frame()

for (i in 1:6 ){
  g_school_and_program <- datstu  %>%
    group_by(schools[i], programs[i], jssdistrict, rankplace) %>%
    dplyr::summarize(cutoff = min(score),
                     quality_sum = sum(score),
                     quality = mean(score),
                     nstudents = n())
  g_school_and_program <- filter(g_school_and_program, rankplace == i )
  
  colnames(g_school_and_program)<- c('schoolcode', 'choicepgm', 'jssdistrict', 'rank', 'cutoff', 
                                     'quality_sum', 'quality', 'nstudents')
  datstu_school_program_jssdistrict <- dplyr::bind_rows(datstu_school_program_jssdistrict, 
                                                        g_school_and_program)
}

# datstu_school_program_jssdistrict <- distinct(datstu_school_program_jssdistrict)

# left join with the eg3_for_printing data to get distance
datstu_school_program_jssdistrict_final <- left_join(datstu_school_program_jssdistrict, eg3_for_printing,
                                                     by = c('schoolcode', 'jssdistrict'))

# drop duplicated rows and replace ""
datstu_school_program_jssdistrict_final <- distinct(datstu_school_program_jssdistrict_final)
datstu_school_program_jssdistrict_final[datstu_school_program_jssdistrict_final ==""] <- NA


# group by rank again to come up with the descriptive statistics 
g_datstu_school_program_jssdistrict_final <- datstu_school_program_jssdistrict_final %>%
  group_by(rank) %>%
  dplyr::summarize(avg_cutoff = mean(cutoff, na.rm = TRUE),
                   sd_cutoff = sd(cutoff, na.rm = TRUE),
                   avg_quality = mean(quality, na.rm = TRUE),
                   sd_quality = sd(quality, na.rm = TRUE),
                   avg_distance = mean(distance, na.rm = TRUE),
                   sd_distance = sd(distance, na.rm = TRUE)
  )
g_datstu_school_program_jssdistrict_final


# ---------------------------
# differentiating by student test score quantile
# ---------------------------

# group the score into 4 buckets
datstu_quantile_group <- datstu %>%
  mutate(quantilegroup = ntile(score, 4)) 
datstu_school_program_jssdistrict_quantile <- data.frame()

# store a df with grouped df
# the logic is the same as before, but now we need an aditional quantilegroup 

for (i in 1:6 ){
  g_school_and_program <- datstu_quantile_group  %>%
    group_by(schools[i], programs[i], jssdistrict, rankplace, quantilegroup) %>%
    dplyr::summarize(nstudents = n(),
                     cutoff = min(score),
                     qualitysum = sum(score),
                     quality = mean(score))
  g_school_and_program <- filter(g_school_and_program, rankplace == i )
  
  colnames(g_school_and_program)<- c('schoolcode', 'choicepgm', 'jssdistrict', 'rank', 
                                     'quantilegroup', 'nstudents', 'cutoff', 'qualitysum', 'quality')
  datstu_school_program_jssdistrict_quantile <- dplyr::bind_rows(datstu_school_program_jssdistrict_quantile, 
                                                                 g_school_and_program)
}

# check if there's duplicate rows (No)
datstu_school_program_jssdistrict_quantile <- distinct(datstu_school_program_jssdistrict_quantile)

# left join with the eg3_for_printing data to get distance
datstu_school_program_jssdistrict_quantile <- left_join(datstu_school_program_jssdistrict_quantile, eg3_for_printing,
                                                        by = c('schoolcode', 'jssdistrict'))

# drop duplicated rows and replace ""
datstu_school_program_jssdistrict_quantile <- distinct(datstu_school_program_jssdistrict_quantile)
datstu_school_program_jssdistrict_quantile[datstu_school_program_jssdistrict_quantile==""] <- NA


# group by quantilegroup again to come up with the descriptive statistics 
datstu_school_program_jssdistrict_quantile_final <- datstu_school_program_jssdistrict_quantile %>%
  group_by(quantilegroup) %>%
  dplyr::summarize(avg_cutoff = mean(cutoff, na.rm = TRUE),
                   sd_cutoff = sd(cutoff, na.rm = TRUE),
                   avg_quality = mean(quality, na.rm = TRUE),
                   sd_quality = sd(quality, na.rm = TRUE),
                   avg_distance = mean(distance, na.rm = TRUE),
                   sd_distance = sd(distance, na.rm = TRUE)
  )
datstu_school_program_jssdistrict_quantile_final




#===========================
# Exercise 5 
#===========================


# setting the seed 

set.seed(123)

nobs = 10000
nvar = 3 
size = 1

x1 = runif(nobs, min = 1, max = 3)
x2 = rgamma(nobs, shape = 3, scale = 2)
x3 = rbinom( nobs, size,  prob = 0.3)
e = rnorm(nobs, mean = 2, sd = 1)
x0 = replicate(10000, 1)
# Exercise 6
true_par = c(0.5, 1.2, -0.9, 0.1)
X = cbind(x0, x1, x2, x3)
y = X %*% true_par + e


#=============================
# Exercise 6 
#=============================

# est_par = (X'X)^(-1) (x'y)
est_par = solve( t(X) %*% X)  %*% t(X) %*% y
## est_par
## x0  2.5155965
## x1  1.1970917
## x2 -0.9033656
## x3  0.1045148

#----------------------
# Correlation = 0.21
#----------------------
cor(x1, y)

## Standard Error of the coefficients 
#  x0          x1          x2          x3 
#  0.041078996 0.017358550 0.002876599 0.021694530 


# The formula for the standard errors of parameters are 
# sigma_e_hat = (e'e)/ (N-K)
# cov_matrix_est_par = sigam_e_hat * (X'X)^(-1)
# standard errors are the diagonal elements of the covariance matrix 

e_hat = y - X %*% est_par
sigma_hat_squared = (t(e_hat) %*% e_hat) / (nobs - (nvar+1)) 
cov_matrix_est_par = sigma_hat_squared[1,1] * solve( t(X) %*% X )
std_error_estimates <- sqrt(diag(cov_matrix_est_par))
std_error_estimates

# Checking answers using built-in OLS library 
data = cbind(y, X)
data_df <- as.data.frame(data)
colnames(data_df) = c('y', 'x0', 'x1', 'x2', 'x3')
model1 <- lm( y ~  x1 + x2 + x3, data = data_df   )
summary(model1)
# The coefficient estimates and the standard errors are the same
est_eg6 = cbind(true_par, summary(model1)$coefficients[, 1],summary(model1)$coefficients[, 2],
                est_par, std_error_estimates)
colnames(est_eg6) = c("True parameter","R: GLM : est","R: GLM :se","R: own : est","R: own :se")
est_eg6


#==============================
#Exercise 7 
#==============================

# fitting the model and calculate classify ydum  
yhat = true_par[1] + true_par[2]*x1 + true_par[3]*x2 + true_par[4]*x3 + e
yvar = as.numeric(yhat> mean(yhat))


#hist(yhat)
table(yvar)

#==========================
# probit in R 
#==========================

reg1 = glm(yvar~x1+x2+x3,family = binomial(link = "probit"))
summary(reg1)

#==========================
# Programming the likelihood
#==========================

# using the code from the lecture example 

flike = function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  pr              = pnorm(xbeta)
  #  pr              = exp(beta)/(1+exp(beta)) logit
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}

# test if the function is correct
test_par = reg1$coefficients
flike(test_par,x1,x2,x3,yvar)
logLik(reg1)

ntry = 100
out = mat.or.vec(ntry,4)
for (i0 in 1:ntry)
{
  start    = runif(4,-10,10)
  #res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=x1,x2=x2,x3=x3,yvar=yvar)
  res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,maxit=1000),
                   x1=x1,x2=x2,x3=x3,yvar=yvar)
  out[i0,] = res$par
}



#===========================================
start = runif(4)
res  = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),
             x1=x1,x2=x2,x3=x3,yvar=yvar,hessian=TRUE)

fisher_info = solve(res$hessian)       # standard formula is -res$hessian but flike is return -like
prop_sigma  = sqrt(diag(fisher_info))
prop_sigma

est_probit = cbind(true_par,summary(reg1)$coefficients[, 1],summary(reg1)$coefficients[, 2],res$par,prop_sigma)
colnames(est_probit) = c("True parameter","R: GLM : est","R: GLM :se","R: own : est","R: own :se")
est_probit


#======================================
# Logit model 
#======================================


reg2 = glm(yvar~x1+x2+x3,family = binomial(link = "logit"))
summary(reg2)

flike_logit = function(par,x1,x2,x3,yvar)
{
  xbeta           = par[1] + par[2]*x1 + par[3]*x2 + par[4]*x3
  #pr              = pnorm(xbeta)
  pr              = exp(xbeta)/(1+exp(xbeta)) 
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = yvar*log(pr) + (1-yvar)*log(1-pr)
  return(-sum(like))
}

# test if the function is correct
test_par = reg2$coefficients
flike_logit(test_par,x1,x2,x3,yvar)
logLik(reg2)

ntry = 100
out_logit = mat.or.vec(ntry,4)
for (i0 in 1:ntry)
{
  start    = runif(4,-10,10)
  #res      = optim(start,fn=flike,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),x1=x1,x2=x2,x3=x3,yvar=yvar)
  res      = optim(start,fn=flike_logit,method="BFGS",control=list(trace=6,maxit=1000),
                   x1=x1,x2=x2,x3=x3,yvar=yvar)
  out_logit[i0,] = res$par
}



#===========================================
start = runif(4)
res  = optim(start,fn=flike_logit,method="BFGS",control=list(trace=6,REPORT=1,maxit=1000),
             x1=x1,x2=x2,x3=x3,yvar=yvar,hessian=TRUE)

fisher_info = solve(res$hessian)       # standard formula is -res$hessian but flike is return -like
prop_sigma  = sqrt(diag(fisher_info))
prop_sigma

est_logit = cbind(true_par,summary(reg2)$coefficients[, 1],summary(reg2)$coefficients[, 2],
                  res$par,prop_sigma)
colnames(est_logit) = c("True parameter","R: GLM : est","R: GLM :se","R: own : est","R: own :se")
est_logit



#======================================
# Linear Probability model 
#======================================

yvar

reg3 = glm(yvar~x1+x2+x3)
summary(reg3)

# est_par = (X'X)^(-1) (x'y)
est_par_linear = solve( t(X) %*% X)  %*% t(X) %*% yvar

e_hat = yvar - X %*% est_par_linear
sigma_hat_squared = (t(e_hat) %*% e_hat) / (nobs - (nvar+1)) 
cov_matrix_est_par_linear = sigma_hat_squared[1,1] * solve( t(X) %*% X )
std_error_estimates_linear <- sqrt(diag(cov_matrix_est_par_linear))
std_error_estimates_linear



est_linear = cbind(true_par,summary(reg3)$coefficients[, 1], summary(reg3)$coefficients[, 2],
                   est_par_linear, std_error_estimates_linear)
colnames(est_linear) = c("True parameter","R: GLM : est","R: GLM :se","R: own : est","R: own :se")
est_linear



#-------------------------
# interpret and compare the estimated coefficients 
#-------------------------
# the coefficient estimates are different across all 3 models 
# Intercept, x1, x2 are significant acrorr all 3, but x3 is not significant across all 3 



#======================================
# Exercise 8
#======================================


#==========================================
# numerical gradient in the lecture code example 
#==========================================

fn = function(x)
{
  out = pnorm(x)
  return(out)
}

num_gradient = function(x, fn)
{
  h   = 0.00001
  ef1 =  fn(x+h)
  ef0 =  fn(x-h)
  dfn = (ef1-ef0)/(2*h)
  return(dfn)
}

#------------------------------------------

R    = 99                       # number of bootstraps
nind = nobs                      # number of individuals
nvar = 4                          # number of variables

mar_effect_probit = mat.or.vec(R,nvar)
set.seed(123)

# create a df called data_eg8 to store all values 

data_eg8 = cbind(yvar, x1, x2, x3)
data_eg8 <- as.data.frame(data_eg8)


# bootstrapping the mean marginal effect from using the probit model 
for (i in 1:R)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = data_eg8[samp,]
  reg1     = glm(yvar~x1+x2+x3, family = binomial(link = "probit"), data = dat_samp)
  mar_effect_probit[i, ] <- mean(dnorm(predict(reg1, type ='link'))) * reg1$coefficients
}


colnames(mar_effect_probit) = c('x0','x1',
                                'X2','X3')

# calculate the mean and standard deviation for every bootstrapping sample 

mean_ME = apply(mar_effect_probit,2,mean)
sd_ME   = apply(mar_effect_probit,2,sd)


est_ME_probit = cbind(mean_ME,
                      sd_ME)

colnames(est_ME_probit) = c("BT: mean_ME","BT: sd_ME")

est_ME_probit




#==========================
# Logit Models Marginal Effect
#==========================

fn_logis = function(x)
{
  out = plogis(x)
  return(out)
}

R    = 99                       # number of bootstraps
nind = nobs                      # number of individuals
nvar = 4                          # number of variables

outs = mat.or.vec(R,nvar)
mar_effect_logit = mat.or.vec(R,nvar)
set.seed(123)

data_eg8 = cbind(yvar, x1, x2, x3)
data_eg8 <- as.data.frame(data_eg8)

# Repeating the same process, but now change the model to logit while fitting the model

for (i in 1:R)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = data_eg8[samp,]
  reg1     = glm(yvar~x1+x2+x3, family = binomial(link = "logit"), data = dat_samp)
  mar_effect_logit[i, ] <- mean(dnorm(predict(reg1, type ='link'))) * reg1$coefficients
  outs[i,] = reg1$coefficients
}

mar_effect_logit

colnames(mar_effect_logit) = c('x0','x1', 'X2','X3')


mean_ME_logit = apply(mar_effect_logit,2,mean)
sd_ME_logit   = apply(mar_effect_logit,2,sd)


est_ME_logit = cbind(mean_ME_logit, sd_ME_logit)

colnames(est_ME_logit) = c("BT: mean_ME","BT: sd_ME")

est_ME_logit

#=============================
# End 
#=============================




# library('margins')
# te = glm(yvar~x1+x2+x3, family = binomial(link = "logit"),data = dat_samp)
# marg1 = margins(te, vce = 'bootstrap')
# summary(marg1)
# 
# est_ME_logit
