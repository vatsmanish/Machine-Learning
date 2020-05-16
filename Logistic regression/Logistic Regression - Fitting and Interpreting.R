#=========================================================================
# FITTING AND INTERPRETING A LOGISTIC REGRESSION MODEL
#=========================================================================

#Data - Death Penalty Data
#------------------------------------------------------------------------
death = read.csv("C:\\Users\\Gourab\\Downloads\\DeathPenalty.csv")
View(death)

#Aggravation Index - Measuring the severity of the crime (from 1 through 6 (really bad))
#VRace - Victim's Race (0 - Black, 1 - White)
#Death - whether a victim is given a death sentence (1: given)

dim(death)

#Baseline accuracy
table(death$Death)
prop.table(table(death$Death))
59/(303+59)


#Train-Test Split (Performing a stratified sampling)
#-------------------------------------------------------------------------
install.packages("caTools")
library(caTools)
set.seed(88)
split <- sample.split(death$Death, SplitRatio = 0.75)

#split

#get training and test data
train <- subset(death, split == TRUE)
test  <- subset(death, split == FALSE)

#Checks
prop.table(table(death$Death))*100
prop.table(table(train$Death))*100
prop.table(table(test$Death))*100



#Fitting a model on the train data
#-------------------------------------------------------------------------
logit1 = glm(Death ~ ., data=train, family = binomial)

summary(logit1)


#Calculation of Odds Ratio:
#1) Agg
exp(1.4400)

#2) VRace
exp(2.0080)



#Probabilistic Prediction on Test data
#-------------------------------------------------------------------------

pred = predict(logit1, newdata = test, type = "response")
pred[1:5]



#Visual Interpretation of the Model
#-------------------------------------------------------------------------

#Is the death Penalty more likely if the victim is white?

agg = seq(1,6,length.out = 500)

black = exp(logit1$coef[1]+agg*logit1$coef[3])/ (1+exp(logit1$coef[1]+agg*logit1$coef[3]))

white = exp(logit1$coef[1]+logit1$coef[2]+agg*logit1$coef[3])/
  (1+exp(logit1$coef[1]+logit1$coef[2]+agg*logit1$coef[3]))


plot(black~agg,type="l",col="black",ylab="Prob[Death]",
     xlab="Aggravation",ylim=c(0,1),
     main="red line for white victim; black line for black victim")


points(white~agg,type="l",col="red")





#Choosing a cut-off point and assessing the model accuracy
#-------------------------------------------------------------------------

#Let t = 0.5
pred1 = ifelse(pred > 0.5, 1, 0)

#Confusion matrix
table(test$Death, pred1)


#Accuracy metrics
library(InformationValue)

#Sensitivity or recall
?sensitivity
sensitivity(test$Death, pred)

#Specificity
specificity(test$Death, pred)

#Precision
precision(test$Death, pred)

#Youden's Index (Sensitivity + Specificity - 1)
youdensIndex(test$Death, pred)

#Mis-classification Error
misClassError(test$Death, pred)



#Choosing Optimal Cutoff to detect more 1's
?optimalCutoff
ones = optimalCutoff(test$Death, pred, "Ones")
ones
#Conf. Matrix
table(test$Death, pred > ones)



#Choosing Optimal Cutoff to detect more 0's
#Based on the Youden's Index

zeros = optimalCutoff(test$Death, pred, "Zeros")
zeros
#Conf. Matrix
table(test$Death, pred > zeros)




#Choosing Optimal Cutoff to minimizes mis-classification error
both = optimalCutoff(test$Death, pred, "Both")
both
#Conf. Matrix
table(test$Death, pred > both)





#Plotting an ROC curve (Choosing Cut-off using ROC curve)
#------------------------------------------------------------------------

library(ROCR)
ROCpred <- prediction(pred,test$Death)
ROCperf <- performance(ROCpred,"tpr","fpr")
plot(ROCperf)
plot(ROCperf, colorize=T, 
     print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))



#OTHER USEFUL PERFORMANCE METRICS
#--------------------------------

#AUROC
#-------------------------------------------------------------------------
auroc = AUROC(test$Death, pred)
auroc   #Closer the AUROC curve value to 1, better is the model




#Concordance
#------------------------------------------------------------------------

#zeros = test[test$Death == 0, ]
#ones  = test[test$Death == 1, ]

#write.csv(zeros, "concordance zeros.csv")
#write.csv(ones, "concordance ones.csv")

Conc = Concordance(test$Death, pred)
Conc
C = Conc$Concordance
D = Conc$Discordance
T = Conc$Tied


#Goodman - Kruskal Gamma
#------------------------------------------------------------------------
gamma = (C-D)/(C+D+T)
gamma



#Somer D
#------------------------------------------------------------------------
D = (C-D)/(C+D)
D





#Model Selection
#------------------------------------------------------------------------

#Refer to this artical:
#http://www.utstat.toronto.edu/~brunner/oldclass/appliedf11/handouts/2101f11StepwiseLogisticR.pdf







#Model Comparison
#-------------------------------------------------------------------------

logit2 = glm(Death ~ Agg, data = train, family = binomial)
summary(logit2)

logit3 = glm(Death ~ VRace, data = train, family = binomial)
summary(logit3)


#Likelihood Ratio Test
anova(logit3, logit2, test='Chisq')

anova(logit2, logit1, test='Chisq')

anova(logit1, logit3, test='Chisq')



AUROC(test$Death, predict(logit2, newdata = test, type="response"))
AUROC(test$Death, predict(logit3, newdata = test, type="response"))
AUROC(test$Death, predict(logit1, newdata = test, type="response"))



#Understanding Variable Importance (Information Value)
#------------------------------------------------------------------------

library(InformationValue)

#For the variable Agg
IV(X=factor(death$Agg), Y=death$Death)

#For the variable VRace
IV(X=factor(death$VRace), Y=death$Death)


#Both the variables goes into the model



#Weight of Evidence Transformation
#------------------------------------------------------------------------

#For the variable VRace
WOETable(factor(death$VRace), death$Death)
death$VRace_WOE = WOE(factor(death$VRace), death$Death)


#For the variable Agg
WOETable(factor(death$Agg), death$Death)
death$Agg_WOE = WOE(factor(death$Agg), death$Death)

View(death)



#Using the WOE transformed variable for logistic regression
#-------------------------------------------------------------------------

logit_WOE = glm(Death ~ VRace_WOE + Agg_WOE, data = death, family = binomial)
summary(logit_WOE)

#O.R.
exp(0.83)
exp(1.1066)


#table(test$Death, predict(logit_WOE, test, type="response") > 0.5)



#Getting Scores:
#------------------------------------------------------------------------

#Score for VRace = White
1.000247*0.8324


#Score for VRace = Black
-1.109242*0.8324


#Score for Agg = 5
1.1066*3.1695431


#Scores for Agg = 5 and VRace = White
1.000247*0.8324 + 1.1066*3.1695431


#Scores for Agg = 5 and VRace = Black
-1.109242*0.8324 + 1.1066*3.1695431






#Lift chart and Gain Chart
#--------------------------------------------------------------------------

#CREATING A GAIN TABLE
#---------------------

#STEP 1 - Create a data frame with two columns - actual and the predicted proabilities
#-------------------------------------------------------------------------------------
pred1 = predict(logit1, newdata=test, type="response")
actual = test$Death
newdata = data.frame(actual,pred1)
View(newdata)


#STEP 2 - Sort the data frame by the predicted probability
#-------------------------------------------------------------------------------------
newdata = newdata[order(-newdata$pred1), ]



#STEP 3 - Divide the data into 10 equal parts according to the values of the predicted probabilities
#-------------------------------------------------------------------------------------
#3A. -> Assign a group te each of the observations in the sorted data in sequence
#-------------------------------------------------------------------------------------
..
#How many observations should each groups contain?
nrow(newdata)/10

#Create the groups in using index
groups = rep(1:10,each=floor(nrow(newdata)/10)) 
extra = rep(10, nrow(newdata)-length(groups))   #Filling up the extras
groups = c(groups,extra)
groups

#Attach the groups to the data
newdata$groups = groups
View(newdata)


#3B -> Creating a Gain Table
#--------------------------------------------------------------------------------------

#We will use SELECT query from the sqldf library
library(sqldf)


#Calculate the number of Bads (or 1's) in each of the groups (keeping track of the total counts in each groups)
gainTable = sqldf("select groups, count(actual) as N, 
                  sum(actual) as N1 from newdata 
                  group by groups ")
class(gainTable)
View(gainTable)

#Calculate the cumulative sum of bads (or 1's)
gainTable$cumN1 = cumsum(gainTable$N1)


#Calculate the cumulative percentage of bads (or 1's)
gainTable$Gain = round(gainTable$cumN1/sum(gainTable$N1)*100,3)


#Calculate Cumulative Lift
gainTable$Lift = round(gainTable$Gain/((1:10)*10),3)


#Print the Gain Table
gainTable


#3C -> Plot the Cumulative Gain and Cumulative Lift Chart
#-------------------------------------------------------------------------------------

#Gain Chart
plot(gainTable$groups, gainTable$Gain, type="b", 
     main = "Gain Plot",
     xlab = "Groups", ylab = "Gain")



#Lift Chart
plot(gainTable$groups, gainTable$Lift, type="b", 
     main = "Lift Plot",
     xlab = "Groups", ylab = "Lift")





#INTERPRETATION:
#--------------

#Interpretation of Cumulative Gain:
#% of targets (events) covered at a given decile level. For example, 80% of targets 
#covered in top 20% of data based on model. 
#
#Interpretation (in this example):





#Other Example:
#In the case of propensity to buy model, we can say we can identify and target 80% 
#of customers who are likely to buy the product by just sending email to 20% of total 
#customers.
#
#How can we interpret the same in case of the attrition problem? (Think)




#Interpretation of Lift:
#The Cum Lift of 4.03 for top two deciles, means that when selecting 20% of the records 
#based on the model, one can expect 4.03 times the total number of targets (events) 
#found by randomly selecting 20%-of-file without a model.
#
#Interpretation (in this example):



#Try interpreting the same thing for the propensity or attrition problem.





#REFERENCE:
#--------------

#1. https://www.listendata.com/2014/08/excel-template-gain-and-lift-charts.html
#2. http://dni-institute.in/blogs/predictive-model-performance-statistics/
#3. https://www.listendata.com/2015/06/r-function-gain-and-lift-table.html



#K-S Statistic
#------------------------------------------------------------------------

#Creating an initial table
ks = sqldf("select groups, count(actual) as N, sum(actual) as N1, 
           count(actual)-sum(actual) as N0 from newdata group by groups ")

View(ks)


#Calculate Percentage Events and Non-Events
ks$PerN0 = round(ks$N0/sum(ks$N0)*100,2)
ks$perN1 = round(ks$N1/sum(ks$N1)*100,2)


#Calculate Cumulative Percentage of Events and Non-Events
ks$CumPerN0 = cumsum(ks$PerN0)
ks$CumPerN1 = cumsum(ks$perN1)


#Calculation of KS
ks$KS = abs(ks$CumPerN0 - ks$CumPerN1)


#Print the Table
ks


#Plot the Graph
plot(ks$groups, ks$CumPerN0, type="l", col = "Green")
lines(ks$groups, ks$CumPerN1, col = "Red")


#INTERPRETATION (This example):
#-----------------------------





#KS Statictics if also calculated to understand how good the model is performing compared
#to the random model.
library(InformationValue)
ks_plot(newdata$actual, newdata$pred1)
ks_stat(newdata$actual, newdata$pred1)




#IMPORTANT NOTE
#--------------

#Important Note - In this case, KS is maximum at third decile and KS score is 88.07. 
#Ideally, it should be in first three deciles and score lies between 40 and 70. And 
#there should not be more than 10 points (in absolute) difference between training and 
#validation KS score. Score above 70 is susceptible and might be overfitting so rigorous 
#validation is required.




#REFERENCE
#--------------

#1. https://www.listendata.com/2015/01/model-validation-in-logistic-regression.html
#2. https://www.youtube.com/watch?v=hNs9o5qiY8U 
#3. https://www.youtube.com/watch?v=U6EDXqw2oBw
#4. http://thestatsgeek.com/2014/02/16/the-hosmer-lemeshow-goodness-of-fit-test-for-logistic-regression/
 




#Hosmer - Lameshow Goodness of Fit
#------------------------------------------------------------------------

install.packages("MKmisc")
library(MKmisc)
HLgof.test(fit = pred, obs = test$Death)


install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(test$Death, pred, g=10)



#READ:
#1. http://thestatsgeek.com/2014/02/16/the-hosmer-lemeshow-goodness-of-fit-test-for-logistic-regression/
#2. http://thestatsgeek.com/2014/02/16/the-hosmer-lemeshow-goodness-of-fit-test-for-logistic-regression/


#READ:
#https://www.listendata.com/2015/01/model-performance-in-logistic-regression.html
