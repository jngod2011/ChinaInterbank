#Loading the mice package
library(mice)
#Loading the following package for looking at the missing values
library(VIM)
library(lattice)
utils::data(nhanes)
data <- all.bid.ON
# First look at the data
str(all.bid.ON)

#Convert Age to factor

#understand the missing value pattern
md.pattern(data)
#plot the missing values
nhanes_miss = aggr(data, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=1, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

#Imputing missing values using mice
mice_imputes = mice(data[,1:10], m=1, maxit = 2,remove_collinear = FALSE)

#What methods were used for imputing
mice_imputes$method

mice_imputes$imp$SOBC.ON
names(mice_imputes$imp)
#Imputed dataset
Imputed_data=complete(mice_imputes,1)
x <- xts(Imputed_data, as.Date(Date, format='%Y-%m-%d'))
#Plotting and comparing values with xyplot()
xyplot(mice_imputes, bmi ~ chl | .imp, pch = 20, cex = 1.4)

#make a density plot
densityplot(mice_imputes)

#fit a linear model on all datasets together
lm_5_model=with(mice_imputes,lm(chl~age+bmi+hyp))

#Use the pool() function to combine the results of all the models
combo_5_model=pool(lm_5_model)
