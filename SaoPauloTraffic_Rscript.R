#Read SaoPaulo document:
SaoPaulo<- read.csv2("SaoPaulo_traffic_data.csv")

## Check if the variables are being well readen:
str(SaoPaulo)
summary(SaoPaulo)

##Check if there appear NA:
is.na(SaoPaulo)
sum(is.na(SaoPaulo))

#Once there aren`t NA`s we can proceed reading the data:
colnames(SaoPaulo)

#We will rename the columns( variables) in order to ease the process:
colnames(SaoPaulo)<-c("Hr","Ib","Bt","ve","av","Ro","Fv","Oif","Iidf","Loe","F","Pof","Mani","dinot","Tree","seof","Isem","Slownessintraffic")


### Correcting the form in how the binary variables are interpreted (the other ones are okay):

data.frame(apply(SaoPaulo[,c(4,7,8,9,11,13,15,17)],2,factor))->SaoPaulo[,c(4,7,8,9,11,13,15,17)]

# The variable "Hr"(the coded hours) is going to be interpreted as a binary variable, being the splited into regular hour(less slowness in traffic) and rush hour (higher slowness):
# Split integrated data -> day1,day2,day3,day4,day5
day1<-SaoPaulo[c(1:27),c(2:18)] 
day2<-SaoPaulo[c(28:54),c(2:18)]
day3<-SaoPaulo[c(55:81),c(2:18)]
day4<-SaoPaulo[c(82:108),c(2:18)]
day5<-SaoPaulo[c(109:135),c(2:18)]

#see the graph with plot function column 18(slowness in traffic), in order to see which hour is the "turning point" in the Slownes of traffic:

plot(day1$Slownessintraffic,xlab = "day1 hour", ylab = "Slownessin trafic")
plot(day2$Slownessintraffic,xlab =  "day2 hour", ylab = "Slownessin trafic")
plot(day3$Slownessintraffic,xlab =  "day3 hour", ylab = "Slownessin trafic")
plot(day4$Slownessintraffic,xlab =  "day4 hour", ylab = "Slownessin trafic")
plot(day5$Slownessintraffic,xlab =  "day5 hour", ylab = "Slownessin trafic")

#find the standard (turning point) to seperate the hours

Hstd<-mean(c(12,18,14,19,17)) #mean of every turning points by each day -> the result is 16

SaoPaulo$Hr<-ifelse(SaoPaulo$Hr >=16,"rush_hour","regular_hour")
str(SaoPaulo)
data.frame(apply(SaoPaulo[,c(1,4,7,8,9,11,13,15,17)],2,factor))->SaoPaulo[,c(1,4,7,8,9,11,13,15,17)]

#visualize and confirm that Hour is reasonable explanatory variable to slowness in traffic:
library(ggplot2)
gg=  ggplot(data =SaoPaulo, aes(y= Slownessintraffic , x=Hr, fill = Hr))
gg = gg +geom_violin(colour= "black", size = 2)
gg = gg+ xlab("Type of hour") +ylab("slowness")
gg

#### Ordering the data by "factor", "numeric" and "integer"
SP<-SaoPaulo[,c(1,4,7,8,9,11,13,15,17,2,3,5,6,10,12,14,16,18)]
summary(SP)

##################### Descriptive Measures ################

# Binary variables (frequency distribution)
for(i in c(1:9)){
  print(table(SP[,c(i)]))
}

# Integer and Numeric variables

# Mean, Standard deviation, Median, InterQuantile Range, Coefficient of variation:
apply(SP[,10:18],2,mean)

apply(SP[,10:18],2,sd)

apply(SP[,10:18],2,median)

apply(SP[,10:18],2,function(x) quantile(x,0.75)-quantile(x,0.25))

apply(SP[,10:18],2,function(x) {sd(x)/mean(x)})

######################## Graphic distribution of the dependent variable #################

hist(SP$Slownessintraffic,main="Histogram - Slownessintraffic",col="Black")

boxplot(SP$Slownessintraffic)

# Total Occurences of the "integer" variables:
SPInteger<-SP[,c(10:17)]
saosum<-data.frame(lapply(SPInteger, sum))
barplot(as.matrix(saosum))



#------------------------------- Regresssion  Model selection--------------------------

# MODEL 1

mod1res <- lm(Slownessintraffic~. ,data =SP)
summary(mod1res)
# Adjusted R^2  0.5231 and R^2 0.5836
# Doesn`t` have NA values (NA normally means that exists multicollinearity), so that is good
# Has plenty non statistically signifacnt variables so we need to do a step of the model

# MODEL 2

mod2res <- step(mod1res)
summary(mod2res)
# Adjusted R^2  0.5484 (increases) and R^2 0.572
# We still have variables which we consider not being significant, once the p-values are higher than 0.1, so we will exclude them for the next model

# MODEL 3

mod3res <- update(mod2res,~.- Fv - Mani - seof)
summary(mod3res)
# Adjusted R^2  0.5329  and R^2 0.5469 (both decrease slightly comparing to model 2)
# We will do an update of this model, by removing the variable with the lower significance level

#MODEL 4

mod4res <-update(mod3res,~.-dinot)
summary(mod4res)
# Adjusted R^2  0.5245  and R^2 0.5351 (both decrease slightly again)
# so, we think "dinot" variable is trivial


mod5res <-update(mod4res,~.-Loe)
summary(mod5res)
# Adjusted R^2  0.4732  and R^2 0.4811 (both decrease significantly)
# so, we don't need to remove "Loe" variable 


mod6res <-update(mod5res,~.-Pof)
summary(mod6res)
# Adjusted R^2  0.3806  and R^2 0.3852 (both decrease significantly again)
# so, we don't need to remove "Pof" variable 


AIC(mod1res)
#699.5965
AIC(mod2res)
#683.3064 (lowest AIC but it has too many variables)
AIC(mod3res)
#685.0208 (just a bit larger than mod2res)
AIC(mod4res)
#686.4732 ( just a bit larger than mod3res)
AIC(mod5res)
#699.3109 (much Larger than mod4res)
AIC(mod6res)
#720.3109 (much Larger than mod5res)

#plot by each model and choose the best model
AIC <-c(AIC(mod1res),AIC(mod2res),AIC(mod3res),AIC(mod4res),AIC(mod5res),AIC(mod6res))
plot(AIC,xlab = "Modnumber",ylab="AIC")

# Model 4 presents quite fair Adjusted R^2 and AIC so this is the one we choose to proceed with the validation




############################### For the normality #####################################

#We are going to perform an hypothesis test between the MODEL 4(reduced) and MODEL 1(complete)
#H0 : reduced model is satisfactory comparing to the most complete one
#h1 : reduced model is not satisfactory comparing to the most complete one

anova(mod4res,mod1res)

#Analysing the data:by adding the 10 variables/parameters (Df=10) to the reduced model, it shows that the model doesn`t improve significantly, once the anova p-value is very large (0.4844)
#(wehaveto thinkabout thispart)So we conclude that the MODEL 4 is satisfatory, and reject MODEL 1,2,3 because it doesn`t improve the reduced model, the p-value is way lower than 5%.

#We will proceed with the MODEL 4


############################### Multicollinearity #####################################

# Analysis of the correlation between the integer variables of MODEL 4
Mod4var<-SP[,c(14,15)]
cor(Mod4var)
#the correlation valu is not significant enough to assume that the variables are dependent of each other, once the value are low

#however we will confirm if there is not really correlation between Loe and Pof:

install.packages("GGally")
library(GGally)
GGally::ggpairs(SP[,c(14,15)])
pairs(SP[,c(14,15)])

install.packages("car")
library(car)
vif(mod4res)

# The values are all lower than ten so there`s is no signs of severe multicollinearity

##############################Autocorrelation################################

# Once we transformed the variable "Hr" into a factor (binary one) there`s no need to analyse the autocorrelation, once this is no longer a time series data base`

####heteroscedasticity

#Breush-Pagan Test
#H0: homocedastic model
#H1: heterocedastic model

library(lmtest)
residualPlots(mod4res)
bptest(mod4res)

#p-value almost 0, so o reject H0, so there is evidence for heteroscedasticity in the model and we have to fix it
#The result of bptest of mod4res showed 4.9*10^-5 that pvalue was very low compared to 5 percent which is critical region.

#filter the data with removing the data that loe and Pof are both zero
library(dplyr)
SPF<-filter(SP,SP[,14]|SP[,15]>0)
SPF

#modeling linear regression with filtered data and do bptest again
modf<-lm(Slownessintraffic~Hr + Pof + Loe, data = SPF)
bptest(modf)

#p-value 0.1472, so accept H0 
#eventhough it has low p_value of bptest of mod4res but we will select the mod4res is optimal model 



#outlier test

residual<-mod4res$resi
rstu<-rstudent(mod4res)       
hat<-hatvalues(mod4res)
rstu
#hat value is less important because most of the data is gathered in zero 

#make resid analysis data called "ra"

resid.analsys<-cbind(residual,rstu,hat)
resid.analsys<-round(resid.analsys,4)
ra<-data.frame(resid.analsys)
ra$row<-1:135


#outlierTest with Rstudent
outlierTest(mod4res)

#our data has a lot of zero so we put 1% critical region to reject

#p-value is lower than 0.01 

#draw the leverage and Rstudent graph with library olsrr
install.packages("olsrr")
library(olsrr)

#draw the graph

#1st leverage vs rstudent it shows eventhough ourdata has a lot of leverage but less rstudent
ols_plot_resid_lev(mod4res)

#2nd draw the graph with only rstudent
ols_plot_resid_stand(mod4res)


#we will delete the data which has lower Rstudent than -2 or higher than 2
summary(mod4res)
rownumber
rownumber<-filter(ra,ra$rstu>2 | ra$rstu < -2)$row

#this row (16  18  77  81 105 106 132) data will be deleted 
SPO <- SP[-rownumber,]

#get linearmodel again from data deleted outlier
mod4out<-lm(Slownessintraffic~Hr+Loe+Pof,data = SPO)

#still Hour and Loe and Pof has siginificant pvalue
#do outlierTest with model four without outlier,



outlierTest(mod4out)
# P-value is 0.02 so it's bigger than critical region(1%) so we will select this model


#draw the graph with only rstudent
ols_plot_resid_stand(mod4out)


bptest(mod4out)
#Breusch-Pagan test, p-value = 0.01092

#we decide final model 
finalmodel<-mod4out

######################## visualize Final Model ############################


#draw the 3d model
library(rgl)


#each axis x = Lack of electricity , y= Point of flooding , z = slowness in traffic

#x,y,z about when hour is rush_hour

x<-SPO$Loe[SPO$Hr=="rush_hour"]
y<-SPO$Pof[SPO$Hr=="rush_hour"]
z<-SPO$Slownessintraffic[SPO$Hr=="rush_hour"]

#x1,y1,z1 about when hour is regular_hour

x1<-SPO$Loe[SPO$Hr=="regular_hour"]
y1<-SPO$Pof[SPO$Hr=="regular_hour"]
z1<-SPO$Slownessintraffic[SPO$Hr=="regular_hour"]

#plotted data when hour is rush_hour
plot3d(x, y, z,type="s",xlab="Loe",ylab ="Pof",zlab="Slownessintraffic",
       col="blue",size =0.5 )

#added data when hour is regular_hour
points3d(x1,y1,z1,col="red",size=5)

#we have to make plane equation with final model
#plane3d have to make final model equation shape  like ax+by+cz+d=0 

#draw plane of rush hour 
planes3d(-coef(finalmodel)[3], -coef(finalmodel)[4], 1, -coef(finalmodel)[1]-coef(finalmodel)[2], alpha=0.2,col="blue")

#draw plane of regular hour
planes3d(-coef(finalmodel)[3], -coef(finalmodel)[4], 1, -coef(finalmodel)[1], alpha=0.2,col="red")

#coef(finalmodel)
#(Intercept) Hrrush_hour   Loe         Pof 
#7.576435    4.209041    2.333685    1.413210 


######################## plot Prediction Final Model ############################

#make fittedslowness object
fittedslowness<-fitted(finalmodel)

#to get information about fittedvalue
summary(fittedslowness)

#make cfd object which has fit,lwr,upper data
Cfd <- predict(finalmodel ,interval = "confidence")
hourindex<-c(1:135)

#finalmodel is used by filtered data so we have to do this process

Cfd<-cbind(Cfd ,hourindex[-rownumber])
colnames(Cfd)<-c("fit","lwr","upr","hour")
Cfd<-data.frame(Cfd)


#plot fitted value and draw 95% confidence interval line
confidence_interval_graph =ggplot(Cfd, aes(x = hour, y = fit)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = upr, ymin = lwr))

confidence_interval_graph


#make Pdic object which has fit,lwr,upper data
Pdic <- predict(finalmodel ,interval = "prediction")


#finalmodel is used by filtered data so we have to do this process

Pdic<-cbind(Pdic ,hourindex[-rownumber])
colnames(Pdic)<-c("fit","lwr","upr","hour")

Pdic<-data.frame(Pdic)

#plot fitted value and draw 95% prediction interval line

predict_interval_graph =ggplot(Pdic, aes(x = hour, y = fit)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = upr, ymin = lwr))

predict_interval_graph
