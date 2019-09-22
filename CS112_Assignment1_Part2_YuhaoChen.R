#CS112-Assignment1-Yuhao Chen

#Clear the working directory
rm(list = ls())


#Import the dataset
CS112data = read.csv("DatasetforCS112.csv",stringsAsFactors=FALSE)


#Transfer the date-code to date
CS112data$approval.date = as.Date(CS112data$approval.date-25569, origin = as.Date('1970-01-01'))
CS112data$implementation.start.date = as.Date(CS112data$implementation.start.date-25569, origin = as.Date('1970-01-01'))
CS112data$original.completion.date = as.Date(CS112data$original.completion.date-25569, origin = as.Date('1970-01-01'))
CS112data$revised.completion.date = as.Date(CS112data$revised.completion.date-25569, origin = as.Date('1970-01-01'))
CS112data


#Check the questions need to do
#Surname=Chen, first 3 digits:"CHE"-->385
set.seed(385)
my_questions_are_these = sort(sample(c(1:10), 3, replace = FALSE))
my_questions_are_these
#The three questions are 4,7 and 9


#Exclude projects approved pre-1/1/1995 & post- 1/1/2017
data_selected = which(CS112data$approval.date >= as.Date("1995-01-01") & CS112data$approval.date < as.Date("2017-01-01"))
CS112data_selected = CS112data[data_selected, ]
CS112data_selected


#Question 4
round(length(CS112data_selected$approval.date)/(2017-1995))
#About 236 projects each year


#Question 7
quantile(CS112data_selected$project.budget)
'
The results:
    0%    25%    50%    75%   100% 
 0.009  0.330  0.600  1.000 47.030 
Quantile of R tells us that the lowest project budget is 0.009 while the largest is 47.030.
We can find that 75% of the projects have a budget lower than 1.000, which shows the normal 
level of budget. Meanwhile, we knows that there exist some big projects which have large amount
of budget.
'


#Question 9
assessed=0
for (a in is.na(CS112data_selected$success.rating)){
    if (a){
      assessed = assessed + 1
  }
}
fraction=assessed/length(CS112data_selected$success.rating)
fraction
#The fraction is 0.4735725, which means about 47.4% of the projects are assessed.
