#------------------
library('tidyverse')
library('dplyr')
library('stringr')
library('tidyr')
library("readr")
library('bayesm')
library(snow)
library(nloptr)
library(boot)
library(data.table)

#------------------

data(margarine)
 
# Brands
#$Pk	#Parkay
#$BB	#BlueBonnett
#$Fl	#Fleischmanns
#$Hse	#house
#$Gen	#generic
#$Imp	#Imperial
#$SS	#Shed Spread

# Product Type 
#$_Stk	#stick
#$_Tub	#tub


#===================#===================
# Exercise 1 Market share and product characteristic
#===================#===================

data_characteristics <- (margarine$choicePrice[,3:12])
summary(data_characteristics)


data_choicePrice <- margarine$choicePrice

product_characteristics <-  apply(data_choicePrice,2,mean)
product_characteristics2 <- apply(data_characteristics, 2, sd)
product_characteristics <- bind_rows(product_characteristics, product_characteristics2)
product_characteristics <- product_characteristics[, 3:12]
product_characteristics = t(product_characteristics)
colnames(product_characteristics) = c('avgprice', 'dispersion')

product_characteristics
#               avg  dispersion
# PPk_Stk  0.5184362 0.15051740
# PBB_Stk  0.5432103 0.12033186
# PFl_Stk  1.0150201 0.04289519
# PHse_Stk 0.4371477 0.11883123
# PGen_Stk 0.3452819 0.03516605
# PImp_Stk 0.7807785 0.11464607
# PSS_Tub  0.8250895 0.06121159
# PPk_Tub  1.0774094 0.02972613
# PFl_Tub  1.1893758 0.01405451
# PHse_Tub 0.5686734 0.07245500


g_choicePrice <- margarine$choicePrice %>%
  group_by(choice) %>%
  dplyr::summarize(market_share = n()/nrow(data_characteristics))
g_choicePrice
#    choice market_share
# *  <dbl>        <dbl>
# 1      1      0.395  
# 2      2      0.156  
# 3      3      0.0544 
# 4      4      0.133  
# 5      5      0.0705 
# 6      6      0.0166 
# 7      7      0.0714 
# 8      8      0.0454 
# 9      9      0.0503 
# 10     10     0.00738




# merge data of the choicePrice and margarine, so  I can use this for future use 

data_merge <- left_join(data_choicePrice, margarine$demos,
          by = c('hhid' = 'hhid' ))

all_product <- data_merge[2:12]

#===================#===================
# choice frequency by price bins
#===================#===================

product_characteristics <- as.data.frame( product_characteristics)
price_bins <- product_characteristics%>%
  mutate(MySpecificBins = cut(avgprice, breaks = 4))


hist(product_characteristics$avgprice)



#===================#===================
# Illustrating the observed attributes and choices
#===================#===================

# Graph out all the individual characteristics on the choice of different products 
# if the variables are categorical, then it reflect the proportion of using that specific product. 
# To interpret the graph, 
# income has no significant impact on individual's choice. 
# households with Family size less than 3, 4 are less likely to buy product 3 and 6
# households with Family size = are less likely to buy product 1-9, more likely to buy product 10
# college educated household are more likely to buy 3, 6, 10
# whtcollar households are more likely to buy 10
# retired households are more likely to buy 3, 6, 9

att_choices <- aggregate(data_merge, by=list(data_merge$choice), FUN=mean) 
att_choices <- select(att_choices, c(choice, Income, Fs3_4, Fs5., Fam_Size, college, whtcollar, retired) )


for (i in 2:8){
  #png(file = paste(as.character(colnames(att_choices)[i]) , '.png' ))
  barplot(att_choices[,i], names.arg = as.character(pull(att_choices, var = choice)) ,
          main= as.character(colnames(att_choices)[i])
          )
  }

# dev.cur()
#dev.off() #where i = index of device to be switched off


#===================#===================
# Exercise 2 First Model
#===================#===================
# Here we use the conditional logit model.
# We assume that every single product and every single choice will share the same beta
# the effect of the dependent variables is the same across all products
# ie, the effect of prices and product types are the same across all potential choices



# This function is finding the average price of all different choice (product characteristics)
# I loop through all the choice (1 to 10) to find the choice of a certain household, then find 
#     group it by choice, and then find the avgprice of a specific choice  
# I later found out that this piece of code is redundant, because this is exactly the same as the product_charcteristics
#     data frame in exercise 1 
# However, to avoid potential error, I leave this code here. 

findNew_product_price = function(data_merge){
  
product_price <- data.frame()
for (i in 1:10){
  value = data_merge[i, 2]          
  product_price[i, 1] <- data_merge[i,value+2]
  
  data_product_sort <- arrange(data_merge, choice)
  g_product_avgprice <- data_product_sort %>%
    group_by(choice) %>%
    dplyr::summarize(avgprice = mean(data_product_sort[[i+2]]))
  
  product_price[i, 1] = g_product_avgprice[1, 2]
}

product_price[1:6, 2 ] = 1
product_price[7:10, 2 ] = 0 

colnames(product_price) <- c('avgprice', 'type_stk')
product_price = add_column(product_price, choice = 1:10)

return(product_price)
}

product_price = findNew_product_price(data_merge = data_merge)

#==========================================
# conditional logit, writing the likelihood
#==========================================

ni = nrow(data_merge)
nj = 10
ut = mat.or.vec(ni,nj)

like_fun = function(param, product_price, data_merge)
{
  ch         =  product_price$choice
  Income     =  data_merge$Income
  Fs3_4      =  data_merge$Fs3_4
  Fs5        =  data_merge$Fs5.
  Fam_Size   =  data_merge$Fam_Size
  college    =  data_merge$college
  whtcollar  =  data_merge$whtcollar
  retired    =  data_merge$retired
  avgprice   =  product_price$avgprice
  type_stk   =  product_price$type_stk
  
  
  ni = nrow(data_merge)
  nj = 10
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  pn1    = param[1:(nj-1)]
  #pn2    = param[(nj+1):(2*nj)]
  #pn3    = param[(2*nj+1):(3*nj)]
  #pn4    = param[(3*nj+1):(4*nj)]
  
  pn1[nj] = 0 
  #  +  param[2] + param[3] + param[4] + param[5] + param[6] +  param[7] +  param[8] +  param[9] +
  
  for (j in 1:nj)
  {
    # conditional logit
    ut[,j] = pn1[j]  + param[10]*type_stk[j]+ param[11]*avgprice[j]
    # conditional logit
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,data_merge$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}




#========================================#========================================
# derivative free likelihood estimation using the code example in lecture 
#========================================#========================================

# I do it many times, and I chose the final parameters 

npar = 11
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = runif(npar) 

#res_probit_sub   = nloptr(start,eval_f=like_fun, lb=lower,ub=upper,
#                         opts=list("algorithm"="NLOPT_LN_SBPLX","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=10000),
#                          product_price = product_price, data_merge = data_merge)

res_probit_bobyqa1  = nloptr(start,eval_f=like_fun, lb=lower,ub=upper,
                            opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=10000),
                            product_price = product_price, data_merge = data_merge)

res_probit_bobyqa1$solution
res_probit_bobyqa1$objective

# > res_probit_bobyqa$solution

# Final choice 
# > res_probit_bobyqa1$solution
# [1]  1.2552641  0.2859292 -1.5803062  0.3034809 -0.1714953 -2.3673339  1.8286619  0.9436849  0.8544401
# [10]  2.8109093  1.7160433
# > res_probit_bobyqa1$objective
# [1] 8285.857

# eg1
# [1]  1.47435539  0.51358244 -1.18959221  0.49447820 -0.01224732 -2.05757526  1.91728151  1.11950795
# [9]  1.06895948  2.57445615  1.37043642

# eg2 
# [1]  1.32277720  0.32218582 -2.13931204  0.47355241  0.11447933 -2.63080707  1.50515237  0.30183389
# [9]  0.07132595  2.80677816  2.97770103


#pp = cbind(res_probit_sub$solution,res_probit_bobyqa$solution)
#cbind(res_probit_sub$objective,res_probit_bobyqa$objective)


#========================================#========================================
# coefficients and standard error of the estimation using bootstrap 
#========================================#========================================

npar = 11
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = res_probit_bobyqa1$solution
 

# Using bootstrap to find the coefficient and standard errors 
# R = 20 
R = 10
nind = nj
outs = mat.or.vec(R, 11)
product_price = findNew_product_price(data_merge)

for (i in 1:R)
{ 
  samp     = sample(1:ni,ni,rep=TRUE)
  data_merge_samp = data_merge[samp,]
  #product_price = findNew_product_price(data_merge)
  
  res_probit_bobyqa  = nloptr(start,eval_f=like_fun, lb=lower,ub=upper,
                              opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=10000),
                              product_price = product_price, data_merge = data_merge_samp)
  
    outs[i,] = res_probit_bobyqa$solution
}

outs <- as.data.frame(outs)
colnames(outs) <- c('intercept1', 'intercept2', 'intercept3', 'intercept4', 'intercept5', 'intercept6', 'intercept7',
                    'intercept8', 'intercept9', 'type_stk', 'avgprice')

mean_est = apply(outs, 2, mean)
sd_est = apply(outs,2 , sd)
est <- cbind(mean_est, sd_est)

est
#               mean_est    sd_est
# intercept1  1.4521701 0.05913733
# intercept2  0.5372918 0.05884810
# intercept3 -0.7255409 0.10614626
# intercept4  0.4115808 0.07617041
# intercept5 -0.2163323 0.08129442
# intercept6 -1.8082718 0.13286357
# intercept7  2.1051800 0.11035976
# intercept8  1.5751585 0.14907458
# intercept9  1.6191546 0.11911277
# type_stk    2.4717621 0.12152047
# avgprice    0.3781054 0.20483094

# Interpretation of avgprice: more expensive margarine will be demanded more 
# ie, the choice probability of more expensive margarine will be higher than other less expensive margarine



#===================#===================
# Exercise 3 Second Model, multinomial logit, writing the likelihood 
#===================#===================
# using multinomial logit model here, because some product may share some characteristics 
#   and differ in other characteristics 
# the brands of butter is mainly about branding, because the content is almost the same  


ni = nrow(data_merge)
nj = nrow(product_price) 
nj_new = nj -1

like_fun_eg3 = function(param, product_price, data_merge)
{
  ch         =  product_price$choice
  Income     =  data_merge$Income
  Fs3_4      =  data_merge$Fs3_4
  Fs5        =  data_merge$Fs5.
  Fam_Size   =  data_merge$Fam_Size
  college    =  data_merge$college
  whtcollar  =  data_merge$whtcollar
  retired    =  data_merge$retired
  avgprice   =  product_price$avgprice
  type_stk   =  product_price$type_stk
  
  ni = nrow(data_merge)
  nj = nrow(product_price) 
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  nj_new = nj - 1
  pn0    = param[1:nj_new]
  pn1    = param[(nj_new+1):(2*nj_new)]
  pn2    = param[(2*nj_new+1):(3*nj_new)]
  pn3    = param[(3*nj_new+1):(4*nj_new)]
  pn4    = param[(4*nj_new+1):(5*nj_new)]
  #pn5    = param[(4*nj_new+1):(5*nj_new)]
  #pn6    = param[(5*nj_new+1):(6*nj_new)]
  
  pn0[10] = 0 
  pn1[10] = 0
  pn2[10] = 0
  pn3[10] = 0
  pn4[10] = 0
  
  for (j in 1:nj)
  {
    # multinomial logit           avgscore08[j]*male*pn3[j] + colonial[j]*jqual*pn4[j] 
    ut[,j] = pn0[j] +  retired*pn1[j] + college*pn2[j] + whtcollar*pn3[j] + Income*pn4[j] 
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   <- sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  prob_second_model <- prob
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,data_merge$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}



#========================================
# derivative free likelihood estimation 
#========================================

# I do it many times, and I chose the final parameters 

npar = 5 * nj_new
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = runif(npar) 

  
res_probit_bobyqa  = nloptr(start,eval_f=like_fun_eg3, lb=lower,ub=upper,
                              opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=10000),
                              product_price = product_price, data_merge = data_merge)

res_probit_bobyqa$solution
res_probit_bobyqa$objective

# Final choice 
#  > > res_probit_bobyqa$solution
# [1]  0.799011059  0.758880417  0.998971917 -5.153272759  0.629563596  0.928671501  0.700726648  0.659299641
# [9]  0.617158043  0.998458574  0.218959941  0.002078645  0.272408587  0.237615660  0.045112607  0.901159222
# [17]  0.672183040  0.514405010  0.932636375  0.707174564  0.482954222  0.229477842  0.362588238  0.410470887
# [25]  0.200804434  0.433160455  0.939878740  0.811408190  0.456721401  0.474538597  0.652314228  0.203289156
# [33]  0.404280555  0.512221201  0.374369522  0.272617786  0.764496925  0.051121695  0.225748844  0.973974647
# [41]  0.380872470  0.045807528  0.113795263  0.381625552  0.667233317
# > res_probit_bobyqa$objective
# [1] 26091.89


# eg1
# > res_probit_bobyqa$solution
# [1] 0.224594694 0.463644620 0.230027229 0.475398005 0.984697489 0.006114513 0.718796754 0.120403062
# [9] 0.259710688 0.963068364 0.835928673 0.192270843 0.550325115 0.698897082 0.835562951 0.659617144
# [17] 0.624192079 0.647111056 0.373021157 0.836582830 0.638493530 0.669786059 0.037974847 0.452116072
# [25] 0.464265268 0.794942923 0.308079254 0.269210821 0.335565269 0.158800804 0.694585725 0.771795487
# [33] 0.264493925 0.515760635 0.259634132 0.132887232 5.145258105 0.798025694 0.637626787 0.425105169
# [41] 0.440894528 0.319478356 0.907471095 0.043067315 0.016737088
# > res_probit_bobyqa$objective
# [1] 37809.16

#eg2
# > res_probit_bobyqa$solution
# [1] 0.93557527 0.87769635 0.79138456 5.95155902 0.67256093 0.32224343 0.73629999 0.32924511 0.37432691
# [10] 0.29620833 0.22449010 0.72423536 0.67045437 0.17416505 0.87197499 0.88255369 0.41719569 0.01931923
# [19] 0.93694108 0.91536633 0.29128412 0.41147507 0.08148284 0.69174598 0.88667144 0.51470590 0.75421136
# [28] 0.33759545 0.97265448 0.29609647 0.21980489 0.23466171 0.74369263 0.73765458 0.49947422 0.19283732
# [37] 0.90899089 0.12895718 0.89431146 0.60349426 0.44013529 0.58846432 0.61544317 0.49908936 0.52635621
# > res_probit_bobyqa$objective
# [1] 25878.91



#=============================
# bootstrap
#=============================

# Using bootstrap to find the coefficient and standard errors 
R = 10 
 

npar = 5 * nj_new
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = res_probit_bobyqa$solution

outs_2 = mat.or.vec(R, npar)

for (i in 1:R)
{ 
  samp     = sample(1:ni,ni,rep=TRUE)
  data_merge_samp = data_merge[samp,]
  product_price = findNew_product_price(data_merge)

  res_probit_bobyqa  = nloptr(start,eval_f=like_fun_eg3, lb=lower,ub=upper,
                              opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=10000),
                              product_price = product_price, data_merge = data_merge_samp)
  
  outs_2[i,] = res_probit_bobyqa$solution
}

outs_2



#==========================#==========================
# find the coefficients and standard error
#==========================#==========================

mean_est_2 = apply(outs_2, 2, mean)
sd_est_2 = apply(outs_2,2 , sd)
est_2 = cbind(mean_est_2, sd_est_2)

est_2 <- as.data.frame(est_2)


#eg3_df  <- (res_probit_bobyqa$solution)
#dim(eg3_df) <- c(45, 1)
#eg3_df <- as.data.frame(eg3_df)

# Individuals are more likely to choose product 1 - 9, compared to products 10, when they have more income, 
# if we keep at everything else equal

x = c('intercept.', 'retired.', 'college.', 'whtcollar.', 'Income.')
li = c()

for (i in 1:5) {
    xs = x[i]
    for (i in 1:9){
      
      x1 = paste0(xs, i)
      li <- c(li, x1)
    }
}

rownames(est_2) <-  li
est_2

#              mean_est_2     sd_est_2
# intercept.1  0.376331689 3.355132e-01
# intercept.2  6.929357077 2.078510e-01
# intercept.3  1.160767106 1.682322e-01
# intercept.4 -5.177512728 3.991829e-04
# intercept.5  0.635851146 1.322490e-04
# intercept.6  0.928167806 3.951947e-04
# intercept.7  0.701056902 2.471735e-06
# intercept.8  0.659331828 4.029242e-07
# intercept.9  0.617154833 2.443411e-07
# retired.1    0.998455369 5.245813e-08
# retired.2    0.218960564 1.356562e-08
# retired.3    0.002078685 9.738239e-10
# retired.4    0.272408552 7.165209e-10
# retired.5    0.237615666 4.032311e-09
# retired.6    0.045112607 5.107508e-10
# retired.7    0.901159222 6.827285e-12
# retired.8    0.672183040 0.000000e+00
# retired.9    0.514405010 0.000000e+00
# college.1    0.932636375 0.000000e+00
# college.2    0.707174564 0.000000e+00
# college.3    0.482954222 0.000000e+00
# college.4    0.229477842 0.000000e+00
# college.5    0.362588238 0.000000e+00
# college.6    0.410470887 0.000000e+00
# college.7    0.200804434 0.000000e+00
# college.8    0.433160455 0.000000e+00
# college.9    0.939878740 0.000000e+00
# whtcollar.1  0.811408190 0.000000e+00
# whtcollar.2  0.456721401 0.000000e+00
# whtcollar.3  0.474538597 0.000000e+00
# whtcollar.4  0.652314228 0.000000e+00
# whtcollar.5  0.203289156 0.000000e+00
# whtcollar.6  0.404280555 0.000000e+00
# whtcollar.7  0.512221201 0.000000e+00
# whtcollar.8  0.374369522 0.000000e+00
# whtcollar.9  0.272617786 0.000000e+00
# Income.1     0.764496925 0.000000e+00
# Income.2     0.051121695 0.000000e+00
# Income.3     0.225748844 0.000000e+00
# Income.4     0.973974647 0.000000e+00
# Income.5     0.380872470 0.000000e+00
# Income.6     0.045807528 0.000000e+00
# Income.7     0.113795263 0.000000e+00
# Income.8     0.381625552 0.000000e+00
# Income.9     0.667233317 0.000000e+00

# Households are more likely to choose product 1 - 9, compared to products 10, when they have more income, 
# if we keep everything else equal


#==========================
# Find probability
#==========================
# here I use the prob matrix in the likelihood function to store prob for further use 

find_prob1 = function(param, product_price, data_merge)
{
  ch         =  product_price$choice
  Income     =  data_merge$Income
  Fs3_4      =  data_merge$Fs3_4
  Fs5        =  data_merge$Fs5.
  Fam_Size   =  data_merge$Fam_Size
  college    =  data_merge$college
  whtcollar  =  data_merge$whtcollar
  retired    =  data_merge$retired
  avgprice   =  product_price$avgprice
  type_stk   =  product_price$type_stk
  
  
  ni = nrow(data_merge)
  nj = 10
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  pn1    = param[1:(nj-1)]
  pn2    = param[(nj+1):(2*nj)]
  pn3    = param[(2*nj+1):(3*nj)]
  pn4    = param[(3*nj+1):(4*nj)]
  
  pn1[nj] = 0 
  #  +  param[2] + param[3] + param[4] + param[5] + param[6] +  param[7] +  param[8] +  param[9] +
  
  for (j in 1:nj)
  {
    # conditional logit
    ut[,j] = pn1[j]  + param[10]*type_stk[j]+ param[11]*avgprice[j]
    # conditional logit
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  return(prob)
}



find_prob2 = function(param, product_price, data_merge)
{
  ch         =  product_price$choice
  Income     =  data_merge$Income
  Fs3_4      =  data_merge$Fs3_4
  Fs5        =  data_merge$Fs5.
  Fam_Size   =  data_merge$Fam_Size
  college    =  data_merge$college
  whtcollar  =  data_merge$whtcollar
  retired    =  data_merge$retired
  avgprice   =  product_price$avgprice
  type_stk   =  product_price$type_stk
  
  ni = nrow(data_merge)
  nj = nrow(product_price) 
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  nj_new = nj - 1
  pn0    = param[1:nj_new]
  pn1    = param[(nj_new+1):(2*nj_new)]
  pn2    = param[(2*nj_new+1):(3*nj_new)]
  pn3    = param[(3*nj_new+1):(4*nj_new)]
  pn4    = param[(4*nj_new+1):(5*nj_new)]
  #pn5    = param[(4*nj_new+1):(5*nj_new)]
  #pn6    = param[(5*nj_new+1):(6*nj_new)]
  
  pn0[10] = 0 
  pn1[10] = 0
  pn2[10] = 0
  pn3[10] = 0
  pn4[10] = 0
  
  for (j in 1:nj)
  {
    # multinomial logit           avgscore08[j]*male*pn3[j] + colonial[j]*jqual*pn4[j] 
    ut[,j] = pn0[j] +  retired*pn1[j] + college*pn2[j] + whtcollar*pn3[j] + Income*pn4[j] 
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   <- sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct

  return(prob)
}




prob1 <- find_prob1(est[,1], product_price, data_merge)

prob2 <- find_prob2(est_2[,1], product_price, data_merge)


#===================#===================
# Exercise 4 Marginal Effect (model 1)
#===================#===================

#--------------------#--------------------
# Conditional Logit marginal effect 
#--------------------#--------------------

#prob is stored in the previous code 
# to compute marginal effect, I loop through all the probability in prob matrix
# the marginal effect of the diagonal and off-diagonal are different, so index j and k
#     are used to account for this and calculate the ME 
# mar1 is the off-diagonal ME, mar2 is the diagonal ME 
# to avoid running the loop too long, I am just calculating the marginal effect for household 1, 
# if we want to calculate the marginal effect for everyone, just add another for loop outside for i

mar1 = c()
mar2 = c() 
mar1_ind = c()
mar2_ind = c()

prob = prob1
for (j in 1: 10){
  
  for (k in 1:10) {
    
    if (j != k) {
      
      mar1 <- c(mar1, prob[1, j] * ( 0 - prob[1, k] )* as.data.frame(est)$mean_est)
      mar1_ind = c(mar1_ind, paste(as.character(j), as.character(k), sep = ',' ))
      
      
    } else {
      
      mar2 <- c(mar2, prob[1, j] *( 1 - prob[1, k] )* as.data.frame(est)$mean_est)
      mar2_ind = c(mar2_ind, paste(as.character(j), as.character(k), sep = ',' ))
      
    }
    
  }
  
}

ME1 <- data.frame(mar1)
ME1_matrix <- as.matrix(ME1)
dim(ME1_matrix) <- c(11, 90)
ME1 <- data.frame(ME1_matrix)
ME1 <- ME1[10:11, ]
ME1 = t(ME1)
rownames(ME1) = mar1_ind
colnames(ME1) = c('ME: type', 'ME: avgPrice')
# prob[1, 2] * ( 0 - prob[1, 3] )* as.data.frame(est)$mean_est

ME1
#          ME: type  ME: avgPrice
# 1,2  -0.1935720019 -1.337651e-02
# 1,3  -0.0661923501 -4.574127e-03
# 1,4  -0.1610538079 -1.112939e-02
# 1,5  -0.0867814772 -5.996908e-03
# 1,6  -0.0196126889 -1.355307e-03
# 1,7  -0.0887255694 -6.131252e-03
# 1,8  -0.0578924547 -4.000574e-03
# 1,9  -0.0613483730 -4.239391e-03
# 1,10 -0.0089450546 -6.181351e-04
# 2,1  -0.1935720019 -1.337651e-02
# 2,3  -0.0259380814 -1.792414e-03
# 2,4  -0.0631104164 -4.361154e-03
# 2,5  -0.0340061203 -2.349944e-03
# 2,6  -0.0076854126 -5.310893e-04
# 2,7  -0.0347679307 -2.402588e-03
# 2,8  -0.0226856910 -1.567662e-03
# 2,9  -0.0240399244 -1.661244e-03
# 2,10 -0.0035052019 -2.422219e-04 ......

# Marginal effect of choice j on choice k, when j not equal to k for individual 1 
# eg, Marginal effect of choice 1 on choice 2 is (1, 2)


ME2 <- data.frame(mar2)
ME2_matrix <- as.matrix(ME2)
dim(ME2_matrix) <- c(11, 10)
ME2 <- data.frame(ME2_matrix)
ME2 <- ME2[10:11, ]
ME2 = t(ME2)
rownames(ME2) = mar2_ind
colnames(ME2) = c('ME: type', 'ME: avgPrice')

ME2
#         ME: type ME: avgPrice
# 1,1   0.74412378  0.051421598
# 2,2   0.40931078  0.028284830
# 3,3   0.15703318  0.010851551
# 4,4   0.35115252  0.024265887
# 5,5   0.20226130  0.013976975
# 6,6   0.04837804  0.003343095
# 7,7   0.20644321  0.014265960
# 8,8   0.13831541  0.009558086
# 9,9   0.14614302  0.010099002
# 10,10 0.02225767  0.001538084 ......

# Marginal effect of choice j on choice k, when j equal to k for individual 1
# Marginal effect of choice 1 on choice 1 is (1, 1)



#===================#===================
# Exercise 4 Marginal Effect (model 2)
#===================#===================
# marginal effect for multinomial logit model 
# don't have to account for diagonal or off diagonal here 
# use the coefficient(est_2) to calculate ME for product 
# the marginal effect is stored as the sequence of partial derivative of (pij) over partial derivative of partial xi
# i from 1: 4470
# j from 1:10
# this code will take too long to run, so I just calculated the ME for the first 10 households

est2_matrix <- as.matrix(est_2[,1])

dim(est2_matrix) <- c(9, 5)
#est2_matrix <- rbind(est2_matrix, rep(0, 5))


est2_df <- as.data.frame(est2_matrix)
colnames(est2_df) = c('coef', 'retired', 'college', 'whtcollar', 'Income' )
#est2_df <- rbind(est2_df, rep(0, 5))
#est2_df = select(est2_df, c( -coef))




ME_model2 <- NULL
prob = prob2
mar3_ind <- c()
for (i in 1:10){
  for (j in 1:9) {
  # cc = data_merge$choice[i]
  ME_model2 <- rbind(ME_model2, prob[i, j] * (est2_df[j, ] - ( prob[i, 1:9]  %*% as.matrix(est2_df)[,1] +
                                                 prob[i, 1:9]  %*% as.matrix(est2_df)[,2] +
                                                  prob[i, 1:9]  %*% as.matrix(est2_df)[,3] +
                                                  prob[i, 1:9]  %*% as.matrix(est2_df)[,4] +
                                                  prob[i, 1:9]  %*% as.matrix(est2_df)[,5] 
                                
                                
                                                )
                                )   )
  mar3_ind = c(mar3_ind, paste(as.character(i), as.character(j), sep = ',' ))
 
  }
}

rownames(ME_model2) <- mar3_ind

head(ME_model2)
#              coef       retired       college     whtcollar        Income
# 1,1 -2.051334e-01  1.270358e-01  9.189317e-02  2.716608e-02  2.118848e-03
# 1,2  7.214326e-08 -6.333533e-09 -6.239641e-10 -3.552966e-09 -8.296376e-09
# 1,3  2.743243e-09 -5.198429e-09 -1.902499e-09 -1.960180e-09 -3.665390e-09
# 1,4 -2.661423e+00 -2.187748e-01 -2.380162e-01 -4.850151e-02  9.566629e-02
# 1,5 -8.775791e-08 -3.680679e-07 -2.801022e-07 -3.922297e-07 -2.672323e-07
# 1,6  2.551058e-12 -1.088687e-11 -5.327017e-12 -5.421218e-12 -1.087630e-11 ... 

# Marginal effect for every choice and individual (i, j) = (individual, choice)




#==============================
# Exercise 5 Mixed Logit Model 
#=============================

ni = nrow(data_merge)
nj = nrow(product_price) 
nj_new = nj -1

like_fun_mixed = function(param, product_price, data_merge)
{
  ch         =  product_price$choice
  Income     =  data_merge$Income
  Fs3_4      =  data_merge$Fs3_4
  Fs5        =  data_merge$Fs5.
  Fam_Size   =  data_merge$Fam_Size
  college    =  data_merge$college
  whtcollar  =  data_merge$whtcollar
  retired    =  data_merge$retired
  avgprice   =  product_price$avgprice
  type_stk   =  product_price$type_stk
  
  ni = nrow(data_merge)
  nj = nrow(product_price) 
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  nj_new = nj - 1
  pn0    = param[1:nj_new]
  pn1    = param[(nj_new+1):(2*nj_new)]
  pn2    = param[(2*nj_new+1):(3*nj_new)]
  #pn3    = param[(3*nj_new+1):(4*nj_new)]
  #pn4    = param[(4*nj_new+1):(5*nj_new)]
  #pn5    = param[(4*nj_new+1):(5*nj_new)]
  #pn6    = param[(5*nj_new+1):(6*nj_new)]
  
  pn0[10] = 0 
  pn1[10] = 0
  pn2[10] = 0
  #pn3[10] = 0
  # pn4[10] = 0
  
  for (j in 1:nj)
  {
    # multinomial logit           avgscore08[j]*male*pn3[j] + colonial[j]*jqual*pn4[j] 
    ut[,j] = pn0[j] +  retired*pn1[j] +  Income*pn2[j] + param[28]*avgprice[j]
    
   # ut[,j] = pn1[j] +param[10]*type_stk[j] + param[11]*avgprice[j]
    
    
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,data_merge$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}


#=============================
# find the coefficient of mixed logit 
#=============================


start  = runif(npar)
#start = good_candidate
#start = res_probit_bobyqa_mixed$solution

npar = 3 * nj_new + 1 
lower  = rep(-10,npar)
upper  = rep(10,npar)

res_probit_bobyqa_mixed  = nloptr(start,eval_f=like_fun_mixed, lb=lower,ub=upper,
                           opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=10000),
                           product_price = product_price, data_merge = data_merge_samp)

# res_probit_sub   = nloptr(start,eval_f=like_fun_mixed, lb=lower,ub=upper,
#                           opts=list("algorithm"="NLOPT_LN_SBPLX","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=10000),
#                           product_price = product_price, data_merge = data_merge)

res_probit_bobyqa_mixed$solution
res_probit_bobyqa_mixed$objective



lik_full <- -res_probit_bobyqa_mixed$objective

# final choice 
# > res_probit_bobyqa_mixed$solution
# [1] -0.05158833  0.51810400  6.24418464  5.50626245  0.52243700  0.79131500  0.25828926  5.95163500  0.32072300
# [10]  0.08839700  0.49808300  0.33135907  0.64161157  0.41715700  0.95150100 -4.75583511  0.60344498  0.56264500
# [19]  0.94444800  0.85104104  0.15504200  0.57855700  0.79523700  0.64272400  0.92892100  0.31017300  0.76649800
# [28]  0.38355900
# > res_probit_bobyqa_mixed$objective
# [1] 16012.23


# eg1
# > res_probit_bobyqa_mixed$solution
# [1]  1.118118664  0.361221076  0.008022905  0.651736462  0.390117291  0.967443631  0.223204500  0.854653919
# [9]  0.339617913  0.704305237  0.133174411  0.842106484  0.610539748  0.195893193  0.249345670  0.824528740
# [17]  0.734699934  0.566776929  0.385051685  0.332802545  0.346402187  0.473775618  0.187397246 -5.705079233
# [25]  0.324541994  0.110411555  0.307806087  0.908172731
# > res_probit_bobyqa_mixed$objective
# [1] 16649.42

# eg2
# > res_probit_bobyqa_mixed$solution
# [1] 0.51870157 0.80481387 0.39382104 0.49947195 0.24735506 0.26338272 0.70769505 0.04789825 0.65431662
# [10] 0.49315201 0.15551725 0.75587575 0.36926060 0.90740566 0.54382076 0.13337268 0.93398661 0.17156661
# [19] 5.21936365 0.68925098 0.80172082 0.24901595 0.90590397 0.33406773 0.25028052 0.52722054 0.98331293
# [28] 0.23899859
# > res_probit_bobyqa_mixed$objective
# [1] 37837.26




#=============================
# bootstrap
#=============================

# Using bootstrap to find the coefficient and standard errors 
#R = 50
R = 20 

npar = 3 * nj_new + 1 
lower  = rep(-10,npar)
upper  = rep(10,npar)
start  = res_probit_bobyqa_mixed$solution


outs_mixed = mat.or.vec(R, npar)
product_price = findNew_product_price(data_merge)


for (i in 1:R)
{ 
  samp     = sample(1:ni,ni,rep=TRUE)
  data_merge_samp = data_merge[samp,]
  res_probit_bobyqa  = nloptr(start,eval_f=like_fun_mixed, lb=lower,ub=upper,
                              opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=10000),
                              product_price = product_price, data_merge = data_merge_samp)
  
  outs_mixed[i,] = res_probit_bobyqa$solution
}

outs_mixed



#==========================
# find the coefficients and standard error
#==========================

mean_est_mixed = apply(outs_mixed, 2, mean)
sd_est_mixed = apply(outs_mixed,2 , sd)
est_mixed = cbind(mean_est_mixed, sd_est_mixed)

est_mixed <- as.data.frame(est_mixed)

# ut[,j] = pn0[j] +  retired*pn1[j] +  Income*pn2[j] + param[28]*avgprice[j]


x = c('intercept.', 'retired.', 'Income.')
li_mixed = c()

for (i in 1: length(x)) {
  xs = x[i]
  for (i in 1:9){
    
    x1 = paste0(xs, i)
    li_mixed <- c(li_mixed, x1)
  }
}


li_mixed = c(li_mixed, 'avgprice')
rownames(est_mixed) <-  li_mixed

est_mixed

#             mean_est_mixed sd_est_mixed
# mean_est_mixed sd_est_mixed
# intercept.1    1.118118655 1.771670e-08
# intercept.2    0.537997783 1.813691e-01
# intercept.3    0.008022905 3.804102e-16
# intercept.4   -4.525040757 1.813689e-01
# intercept.5    0.390117291 3.712897e-16
# intercept.6    0.967443631 3.854350e-16
# intercept.7    0.223204500 3.906594e-16
# intercept.8    0.854653919 4.027202e-16
# intercept.9    0.339617913 3.712897e-16
# retired.1      0.704305237 4.027202e-16
# retired.2      0.133174411 3.906594e-16
# retired.3      0.842106484 4.027202e-16
# retired.4      0.610539748 4.027202e-16
# retired.5      0.195893193 3.906594e-16
# retired.6      0.249345670 3.906594e-16
# retired.7      0.824528740 4.027202e-16
# retired.8      0.734699934 4.027202e-16
# retired.9      0.566776929 4.027202e-16
# Income.1       0.385051301 2.793731e-07
# Income.2       0.332802563 2.446802e-08
# Income.3       0.346402081 1.165858e-07
# Income.4       0.473775626 9.644558e-09
# Income.5       0.187397246 0.000000e+00
# Income.6      -5.705079233 0.000000e+00
# Income.7       0.324541994 0.000000e+00
# Income.8       0.110411555 0.000000e+00
# Income.9       0.307806087 0.000000e+00
# avgprice       0.908172731 0.000000e+00


#========================
# Exercise 5 IIA
#========================

# I removed choice 10, but I think it should be noted that 
#    choice 10 accounted only a small proportion of total choice


data_iia <- data_merge

data_iia <- filter(data_iia, choice != 10)



product_price_iia <- data.frame()
for (i in 1:9){
  value = data_iia[i, 2]          
  product_price_iia[i, 1] <- data_iia[i,value+2]
  
  #data_product_sort <- arrange(data_iia, choice)
  g_product_avgprice <- data_iia %>%
    group_by(choice) %>%
    dplyr::summarize(avgprice = mean(data_iia[[i+2]]))
  
  product_price_iia[i, 1] = g_product_avgprice[1, 2]
}

colnames(product_price_iia) <- c('avgprice')
product_price_iia = add_column(product_price_iia, choice = c(1:9) )



#=============================#=============================
# find the coefficient of the reduced mixed logit model  
#=============================#=============================



ni = nrow(data_iia)
nj = nrow(product_price_iia) 
nj_new = nj -1 

like_fun_mixed_r = function(param, product_price_iia, data_iia)
{
  ch         =  product_price_iia$choice
  Income     =  data_iia$Income
  Fs3_4      =  data_iia$Fs3_4
  Fs5        =  data_iia$Fs5.
  Fam_Size   =  data_iia$Fam_Size
  college    =  data_iia$college
  whtcollar  =  data_iia$whtcollar
  retired    =  data_iia$retired
  avgprice   =  product_price_iia$avgprice
  type_stk   =  product_price_iia$type_stk
  
  ni = nrow(data_iia)
  nj = nrow(product_price_iia) 
  ut = mat.or.vec(ni,nj)
  # multinomial logit
  nj_new = nj - 1
  pn0    = param[1:nj_new]
  pn1    = param[(nj_new+1):(2*nj_new)]
  pn2    = param[(2*nj_new+1):(3*nj_new)]
  #pn3    = param[(3*nj_new+1):(4*nj_new)]
  #pn4    = param[(4*nj_new+1):(5*nj_new)]
  #pn5    = param[(4*nj_new+1):(5*nj_new)]
  #pn6    = param[(5*nj_new+1):(6*nj_new)]
  
  # choice 1, 2, 3, 4, 5, 6, 7, 8, 9
  # Note: since I remove one choice (choice 10), pn0[9] is benchmark for choice 9
  pn0[9] = 0   
  pn1[9] = 0
  pn2[9] = 0
  # pn3[9] = 0
  # pn4[10] = 0
  
  for (j in 1:nj)
  {
    # multinomial logit           avgscore08[j]*male*pn3[j] + colonial[j]*jqual*pn4[j] 
    ut[,j] = pn0[j] +  retired*pn1[j] +  Income*pn2[j] + param[25]*avgprice[j]
    
    # ut[,j] = pn1[j] +param[10]*type_stk[j] + param[11]*avgprice[j]
    
    
  }
  prob   = exp(ut)            # exp(XB)
  #sprob  = rowsums(prob)      # sum_j exp(XB) denominator
  prob   = sweep(prob,MARGIN=1,FUN="/",STATS=rowSums(prob)) # an example of how to construct
  # match prob to actual choices
  probc = NULL
  for (i in 1:ni)
  {
    probc[i] = prob[i,data_iia$choice[i]]
  }
  probc[probc>0.999999] = 0.999999
  probc[probc<0.000001] = 0.000001
  like = sum(log(probc))
  return(-like)
}




#====================
# find coefficient
#====================
npar = 3 * ( nj_new ) + 1 
start  = runif(npar)
#start = res_probit_bobyqa_mixed_r$solution
lower  = rep(-10,npar)
upper  = rep(10,npar)
res_probit_bobyqa_mixed_r  = nloptr(start, eval_f=like_fun_mixed_r, lb=lower,ub=upper,
                            opts=list("algorithm"="NLOPT_LN_BOBYQA","xtol_rel"=1.0e-10, print_level = 3,"maxeval"=10000),
                            product_price_iia = product_price_iia, data_iia = data_iia)

res_probit_bobyqa_mixed_r$solution
res_probit_bobyqa_mixed_r$objective

lik_reduced <- -res_probit_bobyqa_mixed_r$objective

# final choice
# > res_probit_bobyqa_mixed_r$solution
# [1]  0.83166636  0.80329249  0.97849238  0.11610812  0.28010713  0.05886399 -5.40400729  0.45433795  0.88712140
# [10]  0.61993448  0.66983454  0.76054503  0.24924597  0.28240588  0.68445655  0.82432474  0.88940000  0.72026958
# [19]  0.22586156  0.66852258  0.62570474  0.12106922  0.99346016  0.65634381  0.02721955  0.76863695  0.11660540
# [28]  0.66794721
# > res_probit_bobyqa_mixed_r$objective
# [1] 19744.5

#====================
# compute test statistic
#====================


MTT <-  -2 * ( lik_reduced - lik_full)

MTT
#[1] 7451.606



pval <- pchisq(MTT, df = 3, lower.tail = FALSE)
pval 
# > pval
# [1] 0



#====================
# comment on IIA 
#====================

# In the lecture, professor mentioned that it was very rare that the IIA was not violated.
# here in my estimation, the p-value is 0 and thus significant in the log likelihood ratio test, 
# so IIA is violated as expected. 


curve(dchisq(x, df = 3), from = 0, to = 40)






