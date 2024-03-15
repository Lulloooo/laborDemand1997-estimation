#############################################################################################################################
#                          LABOR DEMAND PROJECT
#*************************************************************************************
# Program = QM_albertini_2021_01_05.r
# Programmer = Luca Albertini
# Date first logger: 2020 12 12
#
#            Description: The aim of this program is to estimate the labor demand function
#                         ln = f(ly, lw). It will start with a cross-sectional approach
#                         by doing the preliminary analysis of the three variables. After 
#                         this has been done, an OLS estimation of the demand function will
#                         be computed. Furthermore, the labor demand will be estimated using
#                         the IV method, using as instrument the 1-period lagged data.
#                         In conclusion, a panel-approach will be used to compute data. 
#                         In this context, a pooled OLS model and a fixed effects model will be
#                         compared.
#            
#            Input files: /in_data/labor_data.dta
#            Temp files: none
#            Output files: /out_data
#*************************************************************************************

#install packages you might need (if not done yet)
#install.packages("systemfit")
#install.packages("moments")
#install.packages("MASS")
#install.packages("sandwich")
#install.packages("gridExtra")
#install.packages("fBasics")
#install.packages("ivpack")
#install.packages("plm")
#install.packages ("olsrr")
#install.packages ("readxl)
#install.packages ("reshape")
#install.packages ("Hmisc")
#install.packages("expss")
#install.packages("WDI")
#install.packages("reshape2")
#install.packages("normtest")
#install.packages("tseries")
#install.packages("pastecs")
#install.packages("psych")
#install.packages("moments")
#install.packages("estimatr")
#install.packages("Jmisc")

#set the working directory
#Luca's Macbook
setwd("~/Personal/CaseStudies/laborDemand1997-estimation")
# set working directory
# setwd("")

#load needed packages
library("expss")
library("WDI")
library("reshape2")
require("normtest")
library("readxl")
library("reshape")
library("Hmisc")
library("tseries")
library("pastecs")
library("psych")
library("moments")
library("AER") 
library("systemfit")
library("moments") # kurtosis and skewness calculations
library("stargazer") # table format for regression results
library("MASS")
library("lmtest") # required for coefci and coeftest
library("sandwich") # required for heteroskedastic robust standard errors calculation
library("plm")
library("gridExtra") # arrange graphs
library("olsrr") # Breusch-Pagan Test
library("fBasics") # dagoTest (approximates sktest)
#library("ivpack")
library("ggplot2")
library("haven") 
library("dplyr")
library("foreign")
library("broom")
library("estimatr")
library("Jmisc")

#clear workspace
rm(list = ls())

###################################### USEFUL FUNCTIONS ############################################################


# function to count mild outliers
find_mild_out <- function(X) {
  iqr = IQR(X)
  lowerq = quantile(X)[2]
  upperq = quantile(X)[4]
  lowmildborder = lowerq - (iqr*1.5)
  upmildborder = upperq + (iqr*1.5)
  print(mild_outliers <- length(which( X > upmildborder | X < lowmildborder)))
}
# function to count severe outliers
find_severe_out <- function(X) {
  iqr = IQR(X)
  lowerq = quantile(X)[2]
  upperq = quantile(X)[4]
  lowsevborder = lowerq - (iqr*3)
  upsevborder = upperq + (iqr*3)
  print(severe_outliers <- length(which( X > upsevborder | X < lowsevborder)))
}
# function to count total outliers
total_outliers <- function(X) {
  print(find_mild_out(X))
}
# this because mild outliers count total outliers too

###################################### DATA PREPARATION #########################################################
# load data from dati_esame.dta (a stata (.dta) file)
data_marshall <- read_dta("in_data/labor_data.dta")
# quick look to data
data_marshall %>%
  glimpse()
# data on the job market -> 1438 obs of 12 variables
# drop observations where id >= 11 & id < 20
data_exam <- data_marshall %>%
  dplyr::filter(
    !dplyr::between(id, 11, 19)
  )
# quick look to the new dataset
data_exam %>% glimpse()
# 1420 observations of 12 variables --> 18 observations dropped
#let R understand this is a panel data
paneldata <- data_exam %>% 
  pdata.frame(index = c("id", "year"))
######### DATA EXPLORATION
#inspect if paneldata is a dataframe
paneldata %>% is.data.frame()
##[TRUE]--> it is a dataframe
#check dimensions of our dataframe
paneldata %>% dim()
## 1420 observations; 12 variables
#check the structure
paneldata %>% str()
# list the first 5 obs
paneldata %>% head(5)
# summarize the variables 'id' and 'year'
paneldata[, c(1, 2)] %>% summary()
#        id         year    
# 1      :   2   1996:710  
# 2      :   2   1997:710  
# 3      :   2             
# 4      :   2             
# 5      :   2             
# 6      :   2             
# (Other):1408 
#set missing values (if there are) as "NA", as required by R
paneldata[paneldata == "n/a"] <- NA
paneldata[paneldata == ""] <- NA
#check if there are some missing values
any(is.na(paneldata))
#no NA, so balanced panel data
#save the dataframe in the out_data folder
write.dta(paneldata, file = "out_data/paneldata.dta")
#df with 1420 obs and 72 vars.
#id: factor var (710 levels, one for each firm id)
#year: factor var (2 levels, time period of the obs)
#panel data of 710x2 obs. No NA -> balanced panel.

######################################## EXPLANATORY STATISTICS  #########################################################
## Preliminary analysis on the dependant (ln) and explanatory vars (lw; ly)
# summarize variables altogether
paneldata[,c("ln", "lw","ly")] %>% 
  summary()

######################### ln: log(emp)
# Preliminary analysis (N,median, mean, St.Deviation, trimmed mean )
paneldata$ln %>% describe()
# trimmed mean
mean(paneldata$ln, trim = 0.1)
# trimmed mean exclduding just outliers
mean(paneldata$ln, trim = 0.02)
# Identify outliers:
#compute interquartile range
iqrn_ln <- IQR(paneldata$ln, na.rm = T)
# show interquartile range
print(iqrn_ln)
# Mild outliers = values outside the range [(Q1 - 1.5*IQR), (Q3 + 1.5*IQR)] but inside the range [(Q1 - 3*IQR), (Q3 + 3*IQR)]
# Number of mild outliers
find_mild_out(paneldata$ln)
# 28 outliers
# Identifying mild outliers
mildlowborder_ln <- 3.9120 - 1.5 * iqrn_ln
mildupborder_ln <- 5.7877 + 1.5 * iqrn_ln
# View mild outliers
# upper mild outliers
ln_upmild <- ((paneldata$ln[which(paneldata$ln > mildupborder_ln)]))
View(ln_upmild)
# lower mild outliers
ln_lowmild <- ((paneldata$ln[which(paneldata$ln < mildlowborder_ln)]))
View(ln_lowmild)
# 27 upper mild outlier and 1 lower mild outliers
# Severe outliers = values outside the range [(Q1 - 3*IQR), (Q3 + 3*IQR)]
# number of severe outliers
find_severe_out(paneldata$ln)
# 0 outliers -> find where are the outliers is not useful 
#total outliers
invisible(capture.output(total_out_ln <- total_outliers(paneldata$ln)))
total_out_ln
# 28
# ln boxplot
boxplot_ln <- ggplot(aes(y = ln, x = factor(0)),
                     data = data_exam) + 
  geom_boxplot(outlier.color = "lightsalmon", outlier.size= 1, outlier.shape = 1, col = "black", fill = "lightsalmon") +
  theme(legend.position = "left", ) +
  labs(title="boxplot ln",
       y="ln")
boxplot_ln
#save it
pdf("out_data/ln_boxplot.pdf")
# ln boxplot
boxplot_ln <- ggplot(aes(y = ln, x = factor(0)),
                     data = data_exam) + 
  geom_boxplot(outlier.color = "lightsalmon", outlier.size= 1, outlier.shape = 1, col = "black", fill = "lightsalmon") +
  theme(legend.position = "left", ) +
  labs(title="boxplot ln",
       y="ln")
boxplot_ln
dev.off()
## Normality:
# value of skewness (normal: skewness = 0)
skewness(paneldata$ln)
# 0.5019807
# value of kurtosis (normal: kurtosis = 3)
kurtosis(paneldata$ln)
# 1.184731
# Kernel density plot:
kernel_ln <- density(paneldata$ln)
plot(kernel_ln, col = "lightsalmon", main = "Kernel ln", xlab = "ln")
curve(dnorm(x,mean=mean(paneldata$ln),sd=sd(paneldata$ln)), add = TRUE, col="royalblue2")
#save it
pdf("out_data/kernel_ln.pdf")
kernel_ln <- density(paneldata$ln)
plot(kernel_ln, col = "lightsalmon", main = "Kernel ln", xlab = "ln")
curve(dnorm(x,mean=mean(paneldata$ln),sd=sd(paneldata$ln)), add = TRUE, col="royalblue2")
dev.off()
# Normality tests: 
# H0: ln is distributed as normal (kurtosis 3, skewness 0),
# Skewness test
skewness.norm.test(paneldata$ln)
# p-value: < 2.2e-16
# Kurtosis test
kurtosis.norm.test(paneldata$ln)
# T = 4.1906
# adjusted jarque-bera Normality test
ajb.norm.test((paneldata$ln))
# p-value: < 2.2e-16
# strongly reject H0
# ln is not normally distributed 
#summary for ln
stargazer(paneldata[c("ln")], summary = TRUE, type = "text", median = TRUE, digits = 4, title = "ln")

######################### lw: log(wage/emp)
# Preliminary analysis (N,median, mean, St.Deviation, trimmed mean )
paneldata$lw %>% describe()
# trimmed mean
mean(paneldata$lw, trim = 0.1)
# trimmed mean excluding just outliers
mean(paneldata$lw, trim = 0.01)
##Identify outliers:
#compute interquartile range
iqrn_lw <- IQR(paneldata$lw, na.rm = T)
# show interquartile range
print(iqrn_lw)
# Mild outliers = values outside the range [(Q1 - 1.5*IQR), (Q3 + 1.5*IQR)] but inside the range [(Q1 - 3*IQR), (Q3 + 3*IQR)]
# Number of mild outliers
find_mild_out(paneldata$lw)
# 14 mild outliers
# Identifying mild outliers
mildlowborder_lw <- 4.0946 - 1.5 * iqrn_lw
mildupborder_lw <- 4.8410 + 1.5 * iqrn_lw
# View mild outliers
#upper mild outliers
lw_upmild <- ((paneldata$lw[which(paneldata$lw > mildupborder_lw)]))
View(lw_upmild)
#lower mild outliers
lw_lowmild <- ((paneldata$lw[which(paneldata$lw < mildlowborder_lw)]))
View(lw_lowmild)
# 3 upper mild outliers and 10 lower mild outliers
# Severe outliers = values outside the range [(Q1 - 3*IQR), (Q3 + 3*IQR)]
# number of severe outliers
find_severe_out(paneldata$lw)
#  1 severe outlier
# identifying severe outliers
severelowborder_lw <- 4.0946 - 3 * iqrn_lw
severeupborder_lw <- 4.8410 + 3 * iqrn_lw
# view severe outliers:
#upper severe otuliers
lw_upsev <- ((paneldata$lw[which(paneldata$lw > severeupborder_lw )]))
View(lw_upsev)
#lower severe otuliers
lw_lowsev <- ((paneldata$lw[which(paneldata$lw < severelowborder_lw)]))
View(lw_lowsev)
# 1 lower severe outlier and 0 upper severe outlier
#total outliers
invisible(capture.output(total_out_lw <- total_outliers(paneldata$lw)))
total_out_lw
# 14
#lw boxplot
boxplot_lw <- ggplot(aes(y = lw, x = factor(0)),
                     data = data_exam) + 
  geom_boxplot(outlier.color = "red", outlier.size= 1, outlier.shape = 1, col = "black", fill = "red") +
  #geom_point(size =1, color = "blue", shape = 1, aes(x = factor(0), y =(1.756118))) +
  #scale_color_hue(labels = c("severe outlier"))
  #theme(legend.position = "left", ) +
  labs(title="boxplot lw",
       y="lw") 
boxplot_lw
#save it
pdf("out_data/lw_boxplot.pdf")
#lw boxplot
boxplot_lw <- ggplot(aes(y = lw, x = factor(0)),
                     data = data_exam) + 
  geom_boxplot(outlier.color = "red", outlier.size= 1, outlier.shape = 1, col = "black", fill = "red") +
  #geom_point(size =1, shape = 1, aes(x = factor(0), y =(1.756118), color = "yellow")) +
  #scale_color_hue(labels = c("severe outlier"))
  #theme(legend.position = "left", ) +
  labs(title="boxplot lw",
       y="lw")
boxplot_lw
dev.off()
##Normality:
#value of skewness (normal: skewness = 0)
skewness(paneldata$lw)
# -0.3211482
#value of kurtosis (normal: kurtosis = 3)
kurtosis(paneldata$lw)
# 0.3218507
# Kernel density plot:
kernel_lw <- density(paneldata$lw)
plot(kernel_lw, col = "red", main = "Kernel lw", xlab = "lw")
curve(dnorm(x,mean=mean(paneldata$lw),sd=sd(paneldata$lw)), add = TRUE, col="blue")
#save it
pdf("out_data/kernel_lw.pdf")
kernel_lw <- density(paneldata$lw)
plot(kernel_lw, col = "red", main = "Kernel lw", xlab = "lw")
curve(dnorm(x,mean=mean(paneldata$lw),sd=sd(paneldata$lw)), add = TRUE, col="blue")
dev.off()
# Normality tests: 
# H0: ln is distributed as normal (kurtosis 3, skewness 0),
# Skewness test
skewness.norm.test(paneldata$lw)
# p-value: < 2.2e-16
# Kurtosis test
kurtosis.norm.test(paneldata$lw)
# T = 3.3265
# adjusted jarque-bera Normality test
ajb.norm.test((paneldata$lw))
# p-value = < 2.2e-16
# strongly reject H0
# lw is not normally distributed 
##summary for lw
stargazer(paneldata[c("lw")], summary = TRUE, type = "text", median = TRUE, digits = 4, title = "lw")

######################### ly: log(output)
# Preliminary analysis (N,median, mean, St.Deviation, trimmed mean )
paneldata$ly %>% describe()
#trimmed mean
mean(paneldata$ly, trim = 0.1)
#trimmed mean excluding just outliers
mean(paneldata$ly, trim = 0.01)
## Identify outliers:
#compute interquartile range
iqrn_ly <- IQR(paneldata$ly, na.rm = T)
# show interquartile range
print(iqrn_ly)
# Mild outliers = values outside the range [(Q1 - 1.5*IQR), (Q3 + 1.5*IQR)] but inside the range [(Q1 - 3*IQR), (Q3 + 3*IQR)]
# Number of mild outliers
find_mild_out(paneldata$ly)
# 13
# Identifying mild outliers
mildlowborder_ly <- 6.7558 - 1.5 * iqrn_ly
mildupborder_ly <- 9.6786 + 1.5 * iqrn_ly
# View mild outliers
#upper mild outliers
ly_upmild <- ((paneldata$ly[which(paneldata$ly > mildupborder_ly)]))
View(ly_upmild)
#lower mild outliers
ly_lowmild <- ((paneldata$ly[which(paneldata$ly < mildlowborder_ly)]))
View(ly_lowmild)
# 12 upper mild outliers and 1 lower mild outliers
# Severe outliers = values outside the range [(Q1 - 3*IQR), (Q3 + 3*IQR)]
# number of severe outliers
find_severe_out(paneldata$ly)
# 0
#total outliers
invisible(capture.output(total_out_ly <- total_outliers(paneldata$ly)))
total_out_ly
# 13
#ly boxplot
boxplot_ly <- ggplot(aes(y = ly, x = factor(0)),
                     data = data_exam) + 
  geom_boxplot(outlier.color = "lightskyblue3", outlier.size= 1, outlier.shape = 1, col = "black", fill = "lightskyblue3") +
  theme(legend.position = "left", ) +
  labs(title="boxplot ly",
       y="ly")
boxplot_ly
#save it
pdf("out_data/ly_boxplot.pdf")
#lw boxplot
boxplot_ly <- ggplot(aes(y = ly, x = factor(0)),
                     data = data_exam) + 
  geom_boxplot(outlier.color = "lightskyblue3", outlier.size= 1, outlier.shape = 1, col = "black", fill = "lightskyblue3") +
  theme(legend.position = "left", ) +
  labs(title="boxplot ly",
       y="ly")
boxplot_ly
dev.off()
##Normality:
# value of skewness (normal: skewness = 0)
skewness(paneldata$ly)
# 0.2316579
# value of kurtosis (normal: kurtosis = 3)
kurtosis(paneldata$ly)
# 0.2690086
# Kernel density plot:
kernel_ly <- density(paneldata$ly)
plot(kernel_ly, col = "lightskyblue3", main = "Kernel ly", xlab = "ly")
curve(dnorm(x,mean=mean(paneldata$ly),sd=sd(paneldata$ly)), add = TRUE, col="lightgoldenrod2")
#save it
pdf("out_data/kernel_ly.pdf")
kernel_ly <- density(paneldata$lw)
plot(kernel_ly, col = "lightskyblue3", main = "Kernel ly", xlab = "ly")
curve(dnorm(x,mean=mean(paneldata$ly),sd=sd(paneldata$ly)), add = TRUE, col="lightgoldenrod2")
dev.off()
# Normality tests: 
# H0: ln is distributed as normal (kurtosis 3, skewness 0),
# Skewness test
skewness.norm.test(paneldata$ly)
# p-value: < 2.2e-16
# Kurtosis test
kurtosis.norm.test(paneldata$ly)
# T = 3.2736
# adjusted jarque-bera Normality test
ajb.norm.test((paneldata$ly))
# H0 rejected (p-value < 2.2e-16)
# strongly reject H0
# ly is not normally distributed 
#summary for ly
stargazer(paneldata[c("ly")], summary = TRUE, type = "text", median = TRUE, digits = 4, title = "ly")

############# Plot vars altogether  
# plot all the boxplot together
grid.arrange(boxplot_ln, boxplot_lw, boxplot_ly, ncol=3, nrow = 1)
# save it
pdf("out_data/ln_lw_ly_boxplot.pdf")
grid.arrange(boxplot_ln, boxplot_lw, boxplot_ly, ncol=3, nrow = 1)
dev.off()
#plot all the kernel together
par(mfrow = c(3, 1))
kernel_ln <- density(paneldata$ln)
plot(kernel_ln, col = "lightsalmon", main = "Kernel ln", xlab = "ln")
curve(dnorm(x,mean=mean(paneldata$ln),sd=sd(paneldata$ln)), add = TRUE, col="royalblue2")
kernel_lw <- density(paneldata$lw)
plot(kernel_lw, col = "red", main = "Kernel lw", xlab = "lw")
curve(dnorm(x,mean=mean(paneldata$lw),sd=sd(paneldata$lw)), add = TRUE, col="blue")
kernel_ly <- density(paneldata$ly)
plot(kernel_ly, col = "lightskyblue3", main = "Kernel ly", xlab = "ly")
curve(dnorm(x,mean=mean(paneldata$ly),sd=sd(paneldata$ly)), add = TRUE, col="lightgoldenrod2")
#save it
pdf("out_data/ln_lw_ly_kernel.pdf")
par(mfrow = c(3, 1))
kernel_ln <- density(paneldata$ln)
plot(kernel_ln, col = "lightsalmon", main = "Kernel ln", xlab = "ln")
curve(dnorm(x,mean=mean(paneldata$ln),sd=sd(paneldata$ln)), add = TRUE, col="royalblue2")
kernel_lw <- density(paneldata$lw)
plot(kernel_lw, col = "red", main = "Kernel lw", xlab = "lw")
curve(dnorm(x,mean=mean(paneldata$lw),sd=sd(paneldata$lw)), add = TRUE, col="blue")
kernel_ly <- density(paneldata$ly)
plot(kernel_ly, col = "lightskyblue3", main = "Kernel ly", xlab = "ly")
curve(dnorm(x,mean=mean(paneldata$ly),sd=sd(paneldata$ly)), add = TRUE, col="lightgoldenrod2")
dev.off()
# restore the plot space to a 1x1 dimension
par(mfrow = c(1, 1))


#######################################  LABOR DEMAND ESTIMATION   ########################################################
### Labor demand function: ln = f(lw, ly)
# estimate the labor demand function for the year 1997
###### prepare data for the OLS
#keep only observation for 1997
year1997 <- dplyr::filter(paneldata, year == "1997")
#keep only the vars needed
year1997ols <-  select(year1997, id, ln, lw,ly)
#save it
write.dta(year1997ols, file = "out_data/year1997ols.dta")
# a concrete look: Scatter plot, combinations of ln, ly, lw
lwlyln_scatplot1997 <- pairs(~ln + lw + ly, data = year1997, col = "lightblue",  upper.panel = NULL)
#save it
pdf("out_data/ln_lw_ly_scatplot1997.pdf")
pairs(~ln + lw + ly, data = year1997,col = "lightblue", upper.panel = NULL)
dev.off()
###### compute the OLS model
# OLS model with just one regressor: lw
OLS_lw <- year1997ols %>%
  lm(ln ~ lw, data = .)
#show the model
summary(OLS_lw)
stargazer(OLS_lw, type = "text")
# lw is relevant in explaining ln
# OLS model with just one regressor: ly
OLS_ly <- year1997ols %>%
  lm(ln ~ ly, data = .)
#show it
summary(OLS_ly)
stargazer(OLS_ly, type = "text")
# ly is relevant in explaining ln
#OLS regression of the labor demand function in 1997 (ln = f(lw, ly))
OLS_labdem_1 <- year1997ols %>%
  lm(ln ~ lw + ly, data = .)
#show it
summary(OLS_labdem_1)
stargazer(OLS_labdem_1, type = "text")
#coefficient are all highly significant (p-value < 2.2e-16)
#show all the model together
stargazer(OLS_lw, OLS_ly, OLS_labdem_1, type = "text")
### F-test on joint significance of lw and ly (with robust formula)
linearHypothesis(OLS_labdem_1, c("lw = 0", "ly = 0"), white.adjust = "hc1") # H0: regressor are not jointly statistically relevant
# Reject H0 (p-value < 2.2e-16) --> Coefficient are jointly statistically relevant
# check for regressors' perfect multicollinearity (and singolar correlation with dependent variable)
year1997ols %>%
  dplyr::select(ln, lw, ly) %>%
  cor()
# regressors are correlated, but not perfectly
#save the residuals
year1997ols$residuals <- residuals(OLS_labdem_1)
###### Testing for normality
# D'Agostino normality test - H0 for normality not rejected
dagoTest(OLS_labdem_1$residuals)
# P VALUE:
# H0 of symmetric errors: p-value < 2.2e-16 
# Skewness Test: < 2.2e-16 
# H0 of Kurtosis: p-value: < 2.2e-16 
# Omnibus  Test: < 2.2e-16
# jarque-bera normality test
# for ln:
ajb.norm.test(year1997ols$ln)
# AJB: 62.7
# for lw:
ajb.norm.test(year1997ols$lw)
# AJB: 14.743
# for ly:
ajb.norm.test(year1997ols$ly)
# AJB: 7.3467 
normalTest(OLS_labdem_1$residuals, method = "jb")
# X-squared: 989.742; p Value: < 2.2e-16 
# residual are not normally distributed
###### Testing for residuals’ heteroskedasticity
# Breusch-Pagan test for constant variance of error term
# H0: error term variance is constant--> homoskedasticity
ols_test_breusch_pagan(OLS_labdem_1)
#reject H0 for every alpha (p-value = 0.0003850961) 
#presence of heteroskedasticity
# another test for heteroskedasticity
ncvTest(OLS_labdem_1)
# confirms the previous results
#Zeileis BP Test
#H0: residuals have constant variance --> presence of homosekdasticity
bptest(OLS_labdem_1, studentize = F)
# reject H0 for every alpha (p-value = 3.006e-05) (confirm the previous results)
# presence of heteroskedasticity
###### Regression Specification Error Test (RESET) test
# Ramsey RESET test 
# H0: no omitted variables
resettest(OLS_labdem_1, power = 2:4)
# Reject H0 (p-value = 0.00105) --> omitted variable bias
###### robust standard errors
t(sapply(c("const", "HC0" , "HC1" , "HC2" , "HC3"), function(x) sqrt(diag(vcovHC(OLS_labdem_1, type = x)))))
# note: standard error with formula HC1 are the robust standard erros
# print robust coefficient summary
coeftest(OLS_labdem_1, vcov. = vcovHC, type = "HC1")
# Wald interval
coefci(OLS_labdem_1, level = 0.95, vcov. = vcovHC, type = "HC1")
# returns a matrix (or vector) with columns giving lower (2.5%) and upper (97.5%) confidence limits for each parameter.
# do the model with robust SE estimation
OLS_labdem_rob <- lm_robust(ln ~ lw + ly, data = year1997)
# summarize the model
summary(OLS_labdem_rob, vcov. = vcovHC, type = "HC1")
# shape the model to let it fit in stargazer
OLS_labdem_rob1 <- year1997ols %>%
  lm(ln ~ lw + ly, data = .)
# Adjust standard errors
cov1         <- vcovHC(OLS_labdem_rob1, type = "HC1")
robust_se    <- sqrt(diag(cov1))
###### compare two model above (robust and not-robust)
stargazer(OLS_labdem_1, OLS_labdem_rob1, type = "text",
          se = list(NULL, robust_se))


####################################### LABOR DEMAND WITH IV METHOD   ########################################################
# instrument var : 1 period lagged data
###### prepare the data for the IV
# construct a dataframe for the 1st period lagged (1996)
# keep only observation of 1997
year1996 <- paneldata %>% 
  dplyr::filter(year == "1996") %>% 
  select(id, ln, lw,ly) #keep only the needed vars
# rename variables to understand that are lagged
names(year1996)[names(year1996) == "lw"] <- "lwlag"
names(year1996)[names(year1996) == "ln"] <- "lnlag"
names(year1996)[names(year1996) == "ly"] <- "lylag"
# save it
write.dta(year1996, file = "out_data/year1996.dta")
# put year 1997 and 1996 together 
year1996_1 <- select(year1996, lnlag, lwlag,lylag)
iv_data <- cbind(year1997ols, year1996_1)
write.dta(iv_data, file = "out_data/iv_data")
###### obtain the same dataframe by using command lag
# Select data we are interested in
ivdata <-  select(paneldata, id, year, ln, lw,ly)
# obtain the lagged data(the ones we are going to use as instrument)
ivdata <- ivdata %>% 
  group_by(id) %>%
  mutate(lwlag1 = lag(lw), lylag1 = lag(ly))
# data are exactly the same than before
###### IV ESTIMATION
# Instrument one-stage estimation
OLS_labdem_iv <- iv_data %>%
  ivreg(ln ~ lw + ly | lwlag + lylag, data = .)
OLS_labdem_iv
summary(OLS_labdem_iv)
# all the instrument are statistically significant
# check for regressors' perfect multicollinearity (and singular correlation with dependent variable)
iv_data %>%
  dplyr::select(ln, lwlag, lylag) %>%
  cor()
# check for correlation between variables and instrument
iv_data %>%
  dplyr::select(ly,lw, lwlag, lylag) %>%
  cor()
# Do an F-test to check joint relevance
# H0: lw and ly are irrelevant in explaining ln
linearHypothesis(OLS_labdem_iv , c("lw = 0", "ly = 0"), white.adjust = "hc1")
# rejected H0 (p-value < 2.2e-16) --> lw and ly are relevant in explaining ln
# Do the Hausman test
# H0: the regressor is exogenous
summary(OLS_labdem_iv, diagnostic = TRUE)
# Reject H0 (p-value < 2e-16) --> the regressor is endogenous
# note: Sargan test can not be done cause the model is not overidentified
# compare IV model and OLS model together
stargazer(OLS_labdem_iv, OLS_labdem_rob1, type ="text", 
          se = list(NULL, robust_se))
###### these commands do exactly the same model as before, but starting from the dataset ivdata
# Estimate the iv model
#OLS_labdem_iv1 <- ivdata %>%
#  ivreg(ln ~ lw + ly | lwlag1 + lylag1, data = .)
#OLS_labdem_iv1
# Do an F-test to check joint relevance
#linearHypothesis(OLS_labdem_iv1 , c("lw = 0", "ly = 0"), white.adjust = "hc1")
# Do the Hausman test
#summary(OLS_labdem_iv1, diagnostic = TRUE)
###### results are exactly the same as before.

#######################################  PANEL APPROACH   ########################################################
# Estimate the labor demand function ln = f(lw, ly)
###### pooled OLS model
# pooled model with just ly as regressor
pooled_ly <- plm(ln ~ ly, data = paneldata, 
                 model = "pooling")
# pooled model with just lw as regressor
pooled_lw <- plm(ln ~ lw, data = paneldata, 
                 model = "pooling")
# pooled model with both lw and ly as regressors
pooled_OLS <- plm(ln ~ lw + ly, data = paneldata, 
                  model = "pooling")
summary(pooled_OLS)
stargazer(pooled_lw, pooled_ly, pooled_OLS, type ="text")
stargazer(pooled_OLS, type = "text")
# check for regressors' perfect multicollinearity (and singular correlation with dependent variable)
paneldata %>%
  dplyr::select(ln, lw, ly) %>%
  cor()
#test the joint hypothesis in the pooled OLS model
linearHypothesis(pooled_OLS, 
                 test = "F",
                 c("lw", "ly"), 
                 vcov. = vcovHC, type = "HC1")
# Strongly reject H0 (p-value < 2.2e-16)
###### fixed effects model
# fixed-effects model with just ly as regressor
fixed_effectsly <- plm (ln ~ ly, data = paneldata, 
                        model = "within",
                        effects = "individual")
# fixed-effects model with just lw as regressor
fixed_effectslw <- plm (ln ~ lw, data = paneldata, 
                        model = "within",
                        effects = "individual")
# fixed-effects model with ly and lw as regressors
fixed_effects <- plm (ln ~ lw + ly, data = paneldata, 
                      model = "within",
                      effects = "individual")
summary(fixed_effects)
stargazer(fixed_effects, type ="text")
# compare the fixed-effects models
stargazer(fixed_effectslw,fixed_effectsly, fixed_effects, type = "text" )
# test joint hypothesis in the fixed_effects model
linearHypothesis(fixed_effects, 
                 test = "F",
                 c("lw", "ly"), 
                 vcov. = vcovHC, type = "HC1")
# Strongly reject H0 (p-value < 2.2e-16)
###### list the standard errors for both model
list_se <- list(sqrt(diag(vcovHC(pooled_OLS, type = "HC1"))),
                sqrt(diag(vcovHC(fixed_effects, type = "HC1"))))
list_se
###### F test for the individual effect
# H0: individual effect not statistically significant
pFtest(fixed_effects, pooled_OLS)
# Reject H0 (p-value  < 2.2e-16) -->  fixed-effects are statistically significant
###### Compare the OLS model and the fixed_effect model
stargazer(pooled_OLS, fixed_effects, 
          digits = 3,
          header = FALSE,
          type = "text", 
          se = list_se,
          title = "Linear Panel Regression Models of demand function",
          model.numbers = FALSE,
          column.labels = c("OLS model", "Fixed-effect model"))
################################################  THE END  ########################################################
