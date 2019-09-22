library(Matching)
library(rgenoud)

#import the dataset into the R-notebook
foo = read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
# extract relevant columns
foo = foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]
# remove 2 rows with missing data (there are better ways to handle missing data)
foo = foo[c(-19, -47), ]
# check that all missing data is gone...
which(is.na(foo) == TRUE)
# take a peek at the data set (identify the columns)
head(foo)
summary(foo)


# Question(2)
# Create test sets for both treatment and control groups
foo_test_treat=data.frame(wardur=1:320,wartype=mean(foo$wartype),logcost=mean(foo$logcost),factnum=mean(foo$factnum),factnum2=mean(foo$factnum2),trnsfcap=mean(foo$trnsfcap),untype4=1,treaty=mean(foo$treaty),develop=mean(foo$develop),exp=mean(foo$exp),decade=mean(foo$decade))
foo_test_control=data.frame(wardur=1:320,wartype=mean(foo$wartype),logcost=mean(foo$logcost),factnum=mean(foo$factnum),factnum2=mean(foo$factnum2),trnsfcap=mean(foo$trnsfcap),untype4=0,treaty=mean(foo$treaty),develop=mean(foo$develop),exp=mean(foo$exp),decade=mean(foo$decade))

# Run binomial regression for the relationship between pbs2s3 and covariates
glm1=glm(pbs2s3 ~  wartype + logcost + wardur + factnum + factnum2 + trnsfcap + untype4 + treaty + develop + exp + decade, data = foo, family = binomial)

# Calculate the treatment effects for original regression model by using values of predicted results of treatment group to minus values of predicted results of control group
predict1=predict(glm1,newdata=foo_test_treat,type="response")-predict(glm1,newdata=foo_test_control,type="response")

# Run binomial regression for the relationship between pbs2s3 and covariates when considering interaction between wardur and untype4 as a covariate
glm2=glm(pbs2s3 ~  wartype + logcost + wardur + factnum + factnum2 + trnsfcap + untype4 + treaty + develop + exp + decade + wardur*untype4, data = foo, family = binomial)

# Calculate the treatment effects for regression model with interaction term by using values of predicted results of treatment group to minus values of predicted results of control group
predict2=predict(glm2,newdata=foo_test_treat,type="response")-predict(glm2,newdata=foo_test_control,type="response")

# Plot the treatment effects for the model with interaction term
plot(predict2,type="l",xlab = "Duration of wars in months", ylab = "Marginal effects of UN peacekepping operations")
# Plot the treatment effects for the original model
points(predict1,type="l",lty=2)
# Add legends
legend(140,0.45,legend="Dotted:Original model", cex=0.8, box.lty=0)
legend(30,0.23,legend="Model with interaction term", cex=0.8, box.lty=0)


# Question(3)
Tr <- rep(0, length(foo$untype))
Tr
Tr[which(foo$untype != "None")] <- 1
Tr


# Question(4)
foo2 = read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
foo2 = foo2[, c(6:8, 11:16, 34, 35, 99, 50,55, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]
foo2 = foo2[c(-19, -47), ]

# Tell the treatment and the control group, mark treatment group as 1 and control group as 0
Tr=rep(0, length(foo2$untype))
Tr[which(foo2$untype != "None")]=1
foo2=data.frame(foo2,Tr)
foo3 = foo2[c(-4,-16,-82,-91,-96),]
# Transfer the result to 0 and 1
levels(foo2$pbs2l)[1]=0
levels(foo2$pbs2l)[2]=1
levels(foo3$pbs5l)[1]=0
levels(foo3$pbs5l)[2]=1
Tr=rep(0, length(foo3$untype))
Tr[which(foo3$untype != "None")]=1
foo3$pbs5l

# Do logistic regression for pbs2l
glm3=glm(pbs2l ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + Tr + treaty + develop + exp + decade, data = foo2, family = binomial)
summary(glm3)
# Use the mean of the covariates to calculate the treatment effect
foo_test_treat2=data.frame(wardur=mean(foo2$wardur),wartype=mean(foo2$wartype),logcost=mean(foo2$logcost),factnum=mean(foo2$factnum),factnum2=mean(foo2$factnum2),trnsfcap=mean(foo2$trnsfcap),Tr=1,treaty=mean(foo2$treaty),develop=mean(foo2$develop),exp=mean(foo2$exp),decade=mean(foo2$decade))
foo_test_control2=data.frame(wardur=mean(foo2$wardur),wartype=mean(foo2$wartype),logcost=mean(foo2$logcost),factnum=mean(foo2$factnum),factnum2=mean(foo2$factnum2),trnsfcap=mean(foo2$trnsfcap),Tr=0,treaty=mean(foo2$treaty),develop=mean(foo2$develop),exp=mean(foo2$exp),decade=mean(foo2$decade))
# Calculate the treatment effect by using values of predicted results of treatment group to minus values of predicted results of control group
predict3=predict(glm3,newdata=foo_test_treat2,type="response")-predict(glm3,newdata=foo_test_control2,type="response")
predict3 #the answer is 0.1740779 

# Do logistic regression for pbs5l
glm4=glm(pbs5l ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + Tr + treaty + develop + exp + decade, data = foo3, family = binomial)
summary(glm4)
foo_test_treat3=data.frame(wardur=mean(foo3$wardur),wartype=mean(foo3$wartype),logcost=mean(foo3$logcost),factnum=mean(foo3$factnum),factnum2=mean(foo3$factnum2),trnsfcap=mean(foo3$trnsfcap),Tr=1,treaty=mean(foo3$treaty),develop=mean(foo3$develop),exp=mean(foo3$exp),decade=mean(foo3$decade))
foo_test_control3=data.frame(wardur=mean(foo3$wardur),wartype=mean(foo3$wartype),logcost=mean(foo3$logcost),factnum=mean(foo3$factnum),factnum2=mean(foo3$factnum2),trnsfcap=mean(foo3$trnsfcap),Tr=0,treaty=mean(foo3$treaty),develop=mean(foo3$develop),exp=mean(foo3$exp),decade=mean(foo3$decade))
predict4=predict(glm4,newdata=foo_test_treat3,type="response")-predict(glm4,newdata=foo_test_control3,type="response")
predict4 #the answer is 0.2020409 

set.seed(123)

# Do propensity score matching for pbs2l 
# Get the propensity score
glm3_2=glm(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade, data = foo2, family = binomial)
# Do Match
mm1=Match(Tr=foo2$Tr, X=glm3_2$fitted, Y=foo2$pbs2l,replace = T,ties = T,BiasAdjust = T)
# Check the Match Balance, p-value is 0.098
mb1=MatchBalance(foo2$Tr ~  foo2$wartype + foo2$logcost + foo2$wardur + foo2$factnum + foo2$factnum2 + foo2$trnsfcap  + foo2$treaty + foo2$develop + foo2$exp +foo2$decade, match.out =mm1, nboots=500)
summary(mm1) # Tmt effect is 0.19466 

# Do propensity score matching for pbs5l 
glm4_2=glm(Tr ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + treaty + develop + exp + decade, data = foo3, family = binomial)
mm2=Match(Tr=foo3$Tr, X=glm4_2$fitted, Y=foo3$pbs5l,replace = T,ties = T,BiasAdjust = T)
# p-value is 0.006
mb2=MatchBalance(foo3$Tr ~  foo3$wartype + foo3$logcost + foo3$wardur + foo3$factnum + foo3$factnum2 + foo3$trnsfcap  + foo3$treaty + foo3$develop + foo3$exp +foo3$decade, match.out =mm2, nboots=500)
summary(mm2) # Tmt effect is 0.39389 

# Do GenMatch for pbs2l
# Select the variables for GenMatch
X=cbind(foo2$wartype,foo2$logcost,foo2$wardur,foo2$factnum,foo2$factnum2,foo2$trnsfcap,foo2$treaty,foo2$develop,foo2$exp,foo2$decade)
# Use GenMatch to get the weights
genout1=GenMatch(Tr=foo2$Tr,X=X,estimand="ATT",M=1,pop.size=200,max.generations = 70, wait.generations = 20)
summary(genout1)
# Match the treatment and control groups
mm11= Match(Tr=foo2$Tr, X=X,estimand = "ATT", M=1, Weight.matrix = genout1)
# Check the Match Balance
mb11= MatchBalance(foo2$Tr ~  foo2$wartype + foo2$logcost + foo2$wardur + foo2$factnum + foo2$factnum2 + foo2$trnsfcap  + foo2$treaty + foo2$develop + foo2$exp +foo2$decade,match.out=mm11, nboots=500)
# Calculate the Treatment effects
mm111=Match(Tr=foo2$Tr, X=X,Y=foo2$pbs2l,estimand = "ATT", M=1, Weight.matrix = genout1,BiasAdjust = T)
mm112=Match(Tr=foo2$Tr, X=X,Y=foo2$pbs2l,estimand = "ATT", M=1, Weight.matrix = genout1,BiasAdjust = F)
# For BiasAdjust=True Situation
summary(mm111) # The answer is 0.20761
# For BiasAdjust=False Situation
summary(mm112) # The answer is 0.23985

# Do GenMatch for pbs5l
X2=cbind(foo3$wartype,foo3$logcost,foo3$wardur,foo3$factnum,foo3$factnum2,foo3$trnsfcap,foo3$treaty,foo3$develop,foo3$exp,foo3$decade)
genout2=GenMatch(Tr=foo3$Tr,X=X2,estimand="ATT",M=1,pop.size=200,max.generations = 70, wait.generations = 20)
summary(genout2)
mm21= Match(Tr=foo3$Tr, X=X2,estimand = "ATT", M=1, Weight.matrix = genout2)
mb21= MatchBalance(foo3$Tr ~  foo3$wartype + foo3$logcost + foo3$wardur + foo3$factnum + foo3$factnum2 + foo3$trnsfcap  + foo3$treaty + foo3$develop + foo3$exp +foo3$decade,match.out=mm21, nboots=500)
mm211=Match(Tr=foo3$Tr, X=X2,Y=foo3$pbs2l,estimand = "ATT", M=1, Weight.matrix = genout2,BiasAdjust = T)
mm212=Match(Tr=foo3$Tr, X=X2,Y=foo3$pbs2l,estimand = "ATT", M=1, Weight.matrix = genout2,BiasAdjust = F)
summary(mm211) # The answer is 0.25 
summary(mm212) # The answer is 0.15152 

