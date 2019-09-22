#Question1 for CS112 Assignment2
set.seed(123) #set random results the same every time we run the code
x=10*runif(99) #get values for x
y=10+60*x+rnorm(99) #define a relationship between x and y
lm1=lm(y~x) #find the regression relationship between x and y
summary(lm1) #check the regression relation we find
x2=append(x,300) #add the 100th point to the original 99 points
y2=append(y,-50)
lm2=lm(y2~x2) #find the new regression relationship between x and y
summary(lm2)
plot(x2,y2,main="Regression Models of 99 and 100 Points",xlab="x",ylab="y") #plot the 100 points in the graph
abline(lm1,col="red") #draw the regression line for the original 99 points
abline(lm2,col="green") #draw the regression line for all 100 points
legend("topright",inset=0.001,c("99 Points Coef=59.96","100 Points Coef=-0.65"),fill=c("Red","Green"),horiz=F,cex=1) #add the legend to tell the different lines
lm1
lm2
#we can find the original coefficient is 59.96, and the later coefficient is -0.6518


#Question2
library(Matching)
library(arm)
data("lalonde")
summary(lalonde)
set.seed(1000)
control_group=lalonde[lalonde$treat==0,] #choose the data with no treatment
lm1=lm(re78 ~ age+educ+re74+re75+educ*re74+educ*re75+age*re74+age*re75+re74*re75, data=control_group) #get the regression model
sim_results=sim(lm1,n.sims=10000) #simulate with 10000 random coefficients and sigmas
median_educ=median(control_group$educ) #get the median of educ
median_re74=median(control_group$re74) #get the median of re74
median_re75=median(control_group$re75) #get the median of re75
min(control_group$age) #the minimum age is 17
max(control_group$age) #the maximum age is 55
result_matrix_1=matrix(NA,nrow=10000,ncol=39) #build space matrix to store data
result_matrix_2=matrix(NA,nrow=10000,ncol=39) 
#predict the re78 for different ages while holding educ, re74, and re75 at their medians
for(age in 17:55){ #calculate for each age
  for(i in 1:10000){ #get the prediction for 10000 simulations we did
    predict_re78=sim_results@coef[i,1]+sim_results@coef[i,2]*age+sim_results@coef[i,3]*median_educ+sim_results@coef[i,4]*median_re74+sim_results@coef[i,5]*median_re75+sim_results@coef[i,6]*median_educ*median_re74+sim_results@coef[i,7]*median_educ*median_re75+sim_results@coef[i,8]*age*median_re74+sim_results@coef[i,9]*age*median_re75+sim_results@coef[i,10]*median_re74*median_re75+rnorm(1,0,sim_results@sigma[i])
    result_matrix_1[i,age-16]=predict_re78 #store the results into a matrix
  }
}
#get the 2.5% and 97.5% quantiles of the result_matix_1 we get above
result1=apply(result_matrix_1,2,quantile,na.rm=T,probs=c(0.025,0.975))
#plot the confidence intervals on the graph
plot(x=c(1:10000),y=c(1:10000),type="n",xlim=c(15,57),ylim=c(-10000,20000),xlab="Age",ylab="Predicted Re78",main="Predicted Revenues in 1978(Fixed Median)")
for(age in 17:55){
  segments(x0=age,y0=result1[1,age-16],x1=age,y1=result1[2,age-16]) #set the upper and lower broad
}
#get 90% quantiles for educ, re74 and re75
quantile_educ=as.numeric(quantile(control_group$educ,probs=0.9))
quantile_re74=as.numeric(quantile(control_group$re74,probs=0.9))
quantile_re75=as.numeric(quantile(control_group$re75,probs=0.9))
#predict the re78 for different ages while holding educ, re74, and re75 at their 90% quantiles
for(age in 17:55){
  for(i in 1:10000){
    predict_re78_2=sim_results@coef[i,1]+sim_results@coef[i,2]*age+sim_results@coef[i,3]*quantile_educ+sim_results@coef[i,4]*quantile_re74+sim_results@coef[i,5]*quantile_re75+sim_results@coef[i,6]*quantile_educ*quantile_re74+sim_results@coef[i,7]*quantile_educ*quantile_re75+sim_results@coef[i,8]*age*quantile_re74+sim_results@coef[i,9]*age*quantile_re75+sim_results@coef[i,10]*quantile_re74*quantile_re75+rnorm(1,0,sim_results@sigma[i])
    result_matrix_2[i,age-16]=predict_re78_2
  }
}
#get the 2.5% and 97.5% quantiles of the result_matix_2 we get above
result2=apply(result_matrix_2,2,quantile,na.rm=T,probs=c(0.025,0.975))
#plot the confidence intervals on the graph
plot(x=c(1:10000),y=c(1:10000),type="n",xlim=c(15,57),ylim=c(-10000,20000),xlab="Age",ylab="Predicted Re78",main="Predicted Revenues in 1978(Fixed Quantile)")
for(age in 17:55){
  segments(x0=age,y0=result2[1,age-16],x1=age,y1=result2[2,age-16])
}


#question3
library(foreign)
library(boot)
nsw=read.dta("nsw.dta")
summary(nsw)
set.seed(100)
lm1=lm(re78~treat,data=nsw) #find the regression relationship between re78 and treat
re78=nsw$re78
treat=nsw$treat
a=c() #create a space vector to store values
index=c(1:722)
for(i in 1:10000){
  y=sample(index, size = 722, replace = TRUE) #do bootstrapping to select 722 values from the index list with replacement
  lm_coef=lm(re78[y]~treat[y])$coefficients[2] #find out the corresponding values of re78 and treat for a selected index, and get the regression relationship
  a=append(a,lm_coef) #add the coefficient of the regression relationship into the space vector we create
}
hist(a,main="Frequency of Coefficients with Bootstrapping",xlab="Coefficient") #plot the coefficients and their appearing frequency
quantile(a,0.025) #get the 2.5% quantile for the coefficients
quantile(a,0.975) #get the 97.5% quantile for the coefficients
confint(lm(re78~treat,data=nsw)) #get the analytical confidence interval of coeffient


#question4
#get the function by calculating the result with the formula R-squared=SSR/SST
r_squared=function(y,predicted_y){
  mean_y=mean(y)
  return (sum((predicted_y-mean_y)**2)/sum((y-mean_y)**2))
}
#use the nsw dataset to test the function
predict_re78=predict(lm1,nsw) #predict re78 in dataset with the regression model we get
r_squared(re78,predict_re78) #use function to calculate the R-squared
summary(lm1) #get the R-squared by using summary()


#question5
summary(nsw)
glm1=glm(treat~age+education+black+hispanic+married+nodegree+re75,family=binomial(link='logit'),data=nsw) #find the binomial regression relationship between treat and other characteristics
control_data=nsw[nsw$treat==0,] #get the control group
treat_data=nsw[nsw$treat==1,] #get the treatment group
glm_control=predict(glm1,type="response",newdata=control_data) #get the predicted values in control group
glm_treat=predict(glm1,type="response",newdata=treat_data) #get the predicted values in treatment group
#plot the graph to show the frequency of the predicted values 
#plot the graph for control group
hist(glm_control,xlab="Estimated Probabilities",main="Distribution of The Treatment and Control Groups' Estimated Probabilities",col=rgb(0, 0, 255, 180, maxColorValue=255),xlim=c(0.32,0.57))
#plot the graph for treatment group
hist(glm_treat,col=rgb(255, 0, 0, 180, maxColorValue=255),add=T)
#add the legend
legend("topright",inset=0.001,c("Control Group","Treatment Group"),fill=c("Blue","Red"),horiz=F,cex=1)
