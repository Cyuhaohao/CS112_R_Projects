install.packages("plotrix")

library(foreign)
library(plotrix)
library(Matching)
library(rgenoud)


##### Replication Part #####

# Import the dataset from https://www.thebalance.com/unemployment-rate-by-year-3305506 and https://nces.ed.gov/programs/digest/d17/tables/dt17_302.60.asp
rates=read.csv("rates.csv")
head(rates)
# Plot the graph
twoord.plot(lx=rates$Year,ly=rates$Enrate/100,rx=rates$Year,ry=rates$Unrate,lwd=2,main="Levels",xlab="Year", ylab="Enrollment", rylab="Unemployment Rate", type=c("line","line"),do.first=grid(col="lightgray",lty=1,lwd=0.5))
abline(v=c(1979.5,1980,1981,1982.4,1990,1991,2000,2001,2007,2009),col="gray40",lty=2,lwd=1.5)

# Detrend the enrollment rate. Read the instruction from https://www.statisticshowto.datasciencecentral.com/detrend-data/ 
lm1=lm(rates$Enrate~rates$Year)
summary(lm1)
lm1 #0.4914 for Enrate
average=(1978+2013)/2*0.4914-946.711
xr=c(1978:2013)
yr=xr*0.4914-946.7110
plot(rates$Year,rates$Enrate/100,type="lines",lwd=2,main="Regression Graph for Year and Enrollment Rate",xlab="Year",ylab="Enrollment",do.first=grid(col="lightgray",lty=1,lwd=0.5))
lines(xr,yr/100,col="red",lwd=2)

# Plot the detrended-enrollment graph
for (i in c(1:36)){
  rates[i,"detrended"]=rates$Enrate[i]-(i-18)*0.4914-average
}
twoord.plot(lx=rates$Year,ly=rates$detrended/100,rx=rates$Year,ry=rates$Unrate,lwd=2,main="Detrended",xlab="Year", ylab="Enrollment Detrended", rylab="Unemployment Rate", type=c("line","line"),do.first=grid(col="lightgray",lty=1,lwd=0.5))
abline(v=c(1979.5,1980,1981,1982.4,1990,1991,2000,2001,2007,2009),col="gray40",lty=2,lwd=1.5)



##### Extension Part #####

### Deal with data
# Get the data from https://www.census.gov/cps/about/faq.html
cps=read.dta("cps_00015.dta")
head(cps)

# Extract the data we need
subset=c("year","statefip", "sex", "race", "schlcoll", "ftotval")
data=cps[subset]

# set male as 0, female as 1
data$sex2 = as.numeric(data$sex)-1 

# Set white people as 1, other race as 0
data$race2 = as.numeric(data$race)
data[data[,"race2"]==1,"whiteornot"]=1 
data[data[,"race2"]!=1,"whiteornot"]=0 

# Turn State info into numbers
data$statefip2 = as.numeric(data$statefip)

data$schlcoll2 = as.numeric(data$schlcoll)
data[data[,"schlcoll2"]==6,"multi_coll"]=0  # not enrolled in college
data[data[,"schlcoll2"]==5,"multi_coll"]=1  # enrolled pt (part-time) in college
data[data[,"schlcoll2"]==4,"multi_coll"]=1  # enrolled ft (full-time) college
data=data[ which(data[,"multi_coll"]==0|data[,"multi_coll"]==1),]  # get rid of high school students 
data[, "multi_coll"]=as.factor(data[,"multi_coll"])

# Plot the graph to make sure in which period the unemployment rate increases and which part unemploment rate decrease
unrate=read.csv("UNRATE.csv")
unrate[,"time"]=as.Date(unrate$DATE,"%Y-%m-%d")
unrate=unrate[c(637:792),]
plot(unrate$time,unrate$UNRATE,type="line",xlab="Year", ylab="Unemployment Rate",main="Detailed Graph of Unployment Rate Change",do.first=grid(col="lightgray",lty=1,lwd=0.5))

# Control group: 18-19 years old people in 2004-2007; Treatment group: 18-19 years old people in 2007-2010
datas=data[data[,"year"]>2003 & data[,"year"]<2010,]
datas[datas[,"year"]>2003 & datas[,"year"]<2007,"Tr"]=0
datas[datas[,"year"]>2006 & datas[,"year"]<2010,"Tr"]=1
datas[datas[,"year"]==2004 | datas[,"year"]==2007,"yearnum"]=1
datas[datas[,"year"]==2005 | datas[,"year"]==2008,"yearnum"]=2
datas[datas[,"year"]==2006 | datas[,"year"]==2009,"yearnum"]=3

### Find out the treatment effect!!!

## Do Logistic Regression
glm1=glm(multi_coll ~ ftotval+sex2+whiteornot+statefip2+Tr+yearnum, data=datas, family = binomial)
summary(glm1)
# Use the mean of the covariates to calculate the treatment effect
treat_test1=data.frame(ftotval=mean(datas$ftotval),sex2=mean(datas$sex2),whiteornot=mean(datas$whiteornot),statefip2=mean(datas$statefip2),Tr=1,yearnum=mean(datas$yearnum))
control_test1=data.frame(ftotval=mean(datas$ftotval),sex2=mean(datas$sex2),whiteornot=mean(datas$whiteornot),statefip2=mean(datas$statefip2),Tr=0,yearnum=mean(datas$yearnum))
# Calculate the treatment effect by using values of predicted results of treatment group to minus values of predicted results of control group
predict1=predict(glm1,newdata=treat_test1,type="response")-predict(glm1,newdata=control_test1,type="response")
predict1 
# Get the treatment effect is -0.01092228


## Do Propensity Score Matching
glm2=glm(Tr ~ ftotval+sex2+whiteornot+statefip2+yearnum, data=datas, family = binomial)
summary(glm2)
# Do Match
mm2=Match(Tr=datas$Tr, X=glm2$fitted, Y=datas$multi_coll, replace = T,ties = T,BiasAdjust = T)
# Check the Match Balance, p-value is <2.22e-16, which means we have a bad match
mb2=MatchBalance(datas$Tr ~  datas$ftotval + datas$sex2 + datas$whiteornot + datas$statefip2 + datas$yearnum, match.out =mm2, nboots=500)
summary(mm2) 
# Get the treatment effect is -0.0079229 

# The match above is very bad, therefore, we choose three covariates which have high p-values in the regression model to redo the match
glm4=glm(Tr ~ ftotval+whiteornot+yearnum, data=datas, family = binomial)
summary(glm4)
mm4=Match(Tr=datas$Tr, X=glm4$fitted, Y=datas$multi_coll, replace = T,ties = T,BiasAdjust = T)
# Check the Match Balance, p-value is <2.22e-16. The match is still bad.
mb4=MatchBalance(datas$Tr ~  datas$ftotval + datas$whiteornot+datas$yearnum , match.out =mm4, nboots=500)
summary(mm4) 
# Treatment effect is -0.0068036 



## Do Genetic Matching
# Since there are too many data in the dataset, running the genetic matching will be super time-consuming. Therefore, we let the popsize and maxgenerations to be small.
# However, this constrict did not influence the results, since the model does not have any improvement after several genrations
# Select the variables for GenMatch
X=cbind(datas$ftotval + datas$sex2 + datas$whiteornot + datas$statefip2 + datas$yearnum)
# Use GenMatch to get the weights
genout3=GenMatch(Tr=datas$Tr,X=X,estimand="ATT",M=1,pop.size=30,max.generations = 20, wait.generations = 3)
summary(genout3)
# Match the treatment and control groups
mm3= Match(Tr=datas$Tr, X=X,estimand = "ATT", M=1, Weight.matrix = genout3)
# Check the Match Balance
mb3= MatchBalance(datas$Tr ~  datas$ftotval + datas$sex2 + datas$whiteornot + datas$statefip2 + datas$yearnum,match.out=mm3, nboots=500)
# Calculate the Treatment effects
mm31=Match(Tr=datas$Tr, X=X,Y=datas$multi_coll,estimand = "ATT", M=1, Weight.matrix = genout3,BiasAdjust = T)
mm32=Match(Tr=datas$Tr, X=X,Y=datas$multi_coll,estimand = "ATT", M=1, Weight.matrix = genout3,BiasAdjust = F)
# For BiasAdjust=True Situation
summary(mm31) 
# Treatment effect is -0.015702 
# For BiasAdjust=False Situation
summary(mm32) 
# Treatment effect is -0.015634 



### Test whether or not the two dataset has difference in the trend of enrollment rate(one for 18-19 year-old student, one for 18-24 year-old)
at_enroll_2004=length(data[data[,"year"]==2004 & data[,"multi_coll"]!=0,]$year)/length(data[data[,"year"]==2004,]$year)
at_enroll_2005=length(data[data[,"year"]==2005 & data[,"multi_coll"]!=0,]$year)/length(data[data[,"year"]==2005,]$year)
at_enroll_2006=length(data[data[,"year"]==2006 & data[,"multi_coll"]!=0,]$year)/length(data[data[,"year"]==2006,]$year)
at_enroll_2007=length(data[data[,"year"]==2007 & data[,"multi_coll"]!=0,]$year)/length(data[data[,"year"]==2007,]$year)
at_enroll_2008=length(data[data[,"year"]==2008 & data[,"multi_coll"]!=0,]$year)/length(data[data[,"year"]==2008,]$year)
at_enroll_2009=length(data[data[,"year"]==2009 & data[,"multi_coll"]!=0,]$year)/length(data[data[,"year"]==2009,]$year)
at_enroll_2010=length(data[data[,"year"]==2010 & data[,"multi_coll"]!=0,]$year)/length(data[data[,"year"]==2010,]$year)
at_enroll=c(at_enroll_2004,at_enroll_2005,at_enroll_2006,at_enroll_2007,at_enroll_2008,at_enroll_2009,at_enroll_2010)
twoord.plot(lx=rates$Year[27:33],ly=rates$Enrate[27:33]/100,rx=c(2004:2010),ry=at_enroll,lwd=2,main="Comparison of Enrollment Trend between Two Dataset Used",xlab="Year", ylab="Enrollment 18~24", rylab="Enrollment 18~19", type=c("line","line"),do.first=grid(col="lightgray",lty=1,lwd=0.5))
# Conclusion: No obvious difference in Trend


            
            