### Step 1 - setwd

getwd()
setwd("/Users/sarmadbatt/Desktop/R_Studio")


### Step 2 - Load Data

logdata <- read.csv("AAP_Capstone_Project.csv")
View(logdata)


### Step 3 - Explore Data- understand the logdata, check for missing values

dim(logdata)
class(logdata)
ncol(logdata)
names(logdata)
head(logdata,10)
tail(logdata,10)
str(logdata)


### Step 4 - Data Cleaning: Check for missing values.

summary(logdata$Default_On_Payment)
summary(as.factor(logdata$Default_On_Payment))
summary_logdata = summary(logdata)
write.csv(summary_logdata,"summary_logdata.csv",row.names=F)

install.packages("Amelia")
library(Amelia)
missmap(logdata, main = "Missing values vs observed")



### Remove two rows with missing values 
### Using function "complete.cases" to remove the missing value entries since there are only few missing values

logdata <- logdata[complete.cases(logdata),]
summary(as.factor(logdata$Default_On_Payment))

### Step 5 - Drop the first column "Customer_ID" - it is only an index and doesnot have any prediction power

logdata <- logdata[,c(-1)]


### Step 6 - Select the sample data to create 60% & 40% split in the data.

newdata60 <- logdata[sample(nrow(logdata), size=0.6*nrow(logdata),replace= F),]


### Step 7- Bivariate Analyis 

install.packages("gmodels")
library(gmodels)

CrossTable(newdata60$Status_Checking_Acc,newdata60$Default_On_Payment,expected=FALSE, prop.c=FALSE,prop.t=FALSE, prop.chisq=FALSE,chisq=FALSE) 
CrossTable(newdata60$Duration_in_Months,newdata60$Default_On_Payment,expected=FALSE, prop.c=FALSE,prop.t=FALSE, prop.chisq=FALSE,chisq=FALSE) 
CrossTable(newdata60$Credit_History,newdata60$Default_On_Payment,expected=FALSE, prop.c=FALSE,prop.t=FALSE, prop.chisq=FALSE,chisq=FALSE) 
CrossTable(newdata60$Purposre_Credit_Taken,newdata60$Default_On_Payment,expected=FALSE, prop.c=FALSE,prop.t=FALSE, prop.chisq=FALSE,chisq=FALSE) 
CrossTable(newdata60$Savings_Acc,newdata60$Default_On_Payment,expected=FALSE, prop.c=FALSE,prop.t=FALSE, prop.chisq=FALSE,chisq=FALSE) 
CrossTable(newdata60$Age,newdata60$Default_On_Payment,expected=FALSE, prop.c=FALSE,prop.t=FALSE, prop.chisq=FALSE,chisq=FALSE) 
CrossTable(newdata60$Property,newdata60$Default_On_Payment,expected=FALSE, prop.c=FALSE,prop.t=FALSE, prop.chisq=FALSE,chisq=FALSE) 
CrossTable(newdata60$Years_At_Present_Employment,newdata60$Default_On_Payment,expected=FALSE, prop.c=FALSE,prop.t=FALSE, prop.chisq=FALSE,chisq=FALSE) 
CrossTable(newdata60$Housing,newdata60$Default_On_Payment,expected=FALSE, prop.c=FALSE,prop.t=FALSE, prop.chisq=FALSE,chisq=FALSE) 
CrossTable(newdata60$Other_Inst_Plans,newdata60$Default_On_Payment,expected=FALSE, prop.c=FALSE,prop.t=FALSE, prop.chisq=FALSE,chisq=FALSE) 


### Step 8 - WOE & IV

install.packages("Information")
library(Information)
install.packages("riv")
library(riv)
install.packages("devtools")
library(devtools)
install.packages("woe")
library(woe)
install.packages("gridExtra")
library(gridExtra)


### Step 8a Generate IV of each independent factor
### What significant variables we want to select for building model?

stat <- create_infotables(data=logdata, y = "Default_On_Payment")
grid.table(stat$Summary, rows=NULL)
write.csv(stat$Summary,"IV_summary.csv",row.names=F)


### Step 8b- subset the data to select only 10 significant variables
### And the dependent variable Default_On_Payment

newdata <- subset(newdata60, select = c(Status_Checking_Acc, Duration_in_Months, Credit_History, Savings_Acc, Purposre_Credit_Taken, Age, Property, Years_At_Present_Employment, Housing, Other_Inst_Plans, Default_On_Payment))


### Step 8c Generate WOE table for each independed factor

stat <- create_infotables(data=newdata, y = "Default_On_Payment")
grid.table(stat$Tables$Status_Checking_Acc, rows=NULL)
grid.table(stat$Tables$Duration_in_Months, rows=NULL)
grid.table(stat$Tables$Credit_History, rows=NULL)
grid.table(stat$Tables$Savings_Acc, rows=NULL)
grid.table(stat$Tables$Purposre_Credit_Taken, rows=NULL)
grid.table(stat$Tables$Age, rows=NULL)
grid.table(stat$Tables$Property, rows=NULL)
grid.table(stat$Tables$Years_At_Present_Employment, rows=NULL)
grid.table(stat$Tables$Housing, rows=NULL)
grid.table(stat$Tables$Other_Inst_Plans, rows=NULL)



### Step 9 - Build Linear Reg model

install.packages("car")
library(car)

linreg = lm(Default_On_Payment~., data = newdata)
summary(linreg)
plot(predict(linreg)) 
# predicted values are negative and exceed the limit of 1
#Values lie outside 0 to 1 range - hence improper model fit

plot(linreg)


### Step 10 - Transformmation/Dummy coding of variables for getting the best model

write.csv(stat$Tables$Duration_in_Months,"Duration_summary.csv",row.names=F)
contrasts(newdata$Duration_in_Months)
newdata$Duration_Category <- ifelse(newdata$Duration_in_Months %in% c("4","5","6", "7", "8", "9", "10", "11","12", "13", "14", "15", "16", "17", "18"),"lessthan20", ifelse(newdata$Duration_in_Months %in% c("20","21","22", "24", "26", "27", "28", "30","33", "36", "39"), "20to40", "morethan40"))
table(newdata$Duration_in_Months, newdata$Duration_Category)
stat <- create_infotables(data=newdata, y = "Default_On_Payment")

contrasts(as.factor(newdata$Duration_Category))
newdata$Duration_Dummy_20 <- ifelse(newdata$Duration_Category == "lessthan20", 1,0)
newdata$Duration_Dummy_40 <- ifelse(newdata$Duration_Category == "20to40", 1,0)

contrasts(newdata$Status_Checking_Acc)
newdata$Status_Checking_Acc_A11 <- ifelse(newdata$Status_Checking_Acc == "A11", 1,0)
newdata$Status_Checking_Acc_A12 <- ifelse(newdata$Status_Checking_Acc == "A12", 1,0)
newdata$Status_Checking_Acc_A13 <- ifelse(newdata$Status_Checking_Acc == "A13", 1,0)

contrasts(newdata$Credit_History)
newdata$Credit_History_A30 <- ifelse(newdata$Credit_History == "A30", 1,0)
newdata$Credit_History_A31 <- ifelse(newdata$Credit_History == "A31", 1,0)
newdata$Credit_History_A32 <- ifelse(newdata$Credit_History == "A32", 1,0)
newdata$Credit_History_A33 <- ifelse(newdata$Credit_History == "A33", 1,0)

contrasts(newdata$Savings_Acc)
newdata$Savings_Acc_A61 <- ifelse(newdata$Savings_Acc == "A61", 1,0)
newdata$Savings_Acc_A62 <- ifelse(newdata$Savings_Acc == "A62", 1,0)
newdata$Savings_Acc_A63 <- ifelse(newdata$Savings_Acc == "A63", 1,0)
newdata$Savings_Acc_A64 <- ifelse(newdata$Savings_Acc == "A64", 1,0)

contrasts(newdata$Purposre_Credit_Taken)
newdata$Purposre_Credit_Taken_A40 <- ifelse(newdata$Purposre_Credit_Taken == "A40", 1,0)
newdata$Purposre_Credit_Taken_A41 <- ifelse(newdata$Purposre_Credit_Taken == "A41", 1,0)
newdata$Purposre_Credit_Taken_A42 <- ifelse(newdata$Purposre_Credit_Taken == "A42", 1,0)
newdata$Purposre_Credit_Taken_A43 <- ifelse(newdata$Purposre_Credit_Taken == "A43", 1,0)
newdata$Purposre_Credit_Taken_A44 <- ifelse(newdata$Purposre_Credit_Taken == "A44", 1,0)
newdata$Purposre_Credit_Taken_A45 <- ifelse(newdata$Purposre_Credit_Taken == "A45", 1,0)
newdata$Purposre_Credit_Taken_A46 <- ifelse(newdata$Purposre_Credit_Taken == "A46", 1,0)
newdata$Purposre_Credit_Taken_A47 <- ifelse(newdata$Purposre_Credit_Taken == "A47", 1,0)
newdata$Purposre_Credit_Taken_A48 <- ifelse(newdata$Purposre_Credit_Taken == "A48", 1,0)
newdata$Purposre_Credit_Taken_A49 <- ifelse(newdata$Purposre_Credit_Taken == "A49", 1,0)

contrasts(newdata$Property)
newdata$Property_A121 <- ifelse(newdata$Property == "A47", 1,0)
newdata$Property_A122 <- ifelse(newdata$Property == "A48", 1,0)
newdata$Property_A123 <- ifelse(newdata$Property == "A49", 1,0)

contrasts(newdata$Years_At_Present_Employment)
newdata$Years_At_Present_Employment_A71 <- ifelse(newdata$Years_At_Present_Employment == "A71", 1,0)
newdata$Years_At_Present_Employment_A72 <- ifelse(newdata$Years_At_Present_Employment == "A72", 1,0)
newdata$Years_At_Present_Employment_A73 <- ifelse(newdata$Years_At_Present_Employment == "A73", 1,0)
newdata$Years_At_Present_Employment_A74 <- ifelse(newdata$Years_At_Present_Employment == "A74", 1,0)

contrasts(newdata$Housing)
newdata$Housing_151 <- ifelse(newdata$Housing == "A151", 1,0)
newdata$Housing_152 <- ifelse(newdata$Housing == "A152", 1,0)

contrasts(newdata$Other_Inst_Plans)
newdata$Other_Inst_Plans_141 <- ifelse(newdata$Other_Inst_Plans == "A141", 1,0)
newdata$Housing_142 <- ifelse(newdata$Other_Inst_Plans == "A142", 1,0)

contrasts(as.factor(newdata$Default_On_Payment))
newdata$Default_On_Payment1 <- as.factor(ifelse(newdata$Default_On_Payment == 1,"1","0"))


### Step 11 - Build logistic regression
install.packages("ROCR")
library(ROCR)

logreg <- glm(Default_On_Payment ~ Duration_Dummy_20+Duration_Dummy_40+Status_Checking_Acc_A11+Status_Checking_Acc_A12+Status_Checking_Acc_A13+Credit_History_A30+Credit_History_A31+Credit_History_A32+Credit_History_A33+Savings_Acc_A61+Savings_Acc_A62+Savings_Acc_A63+Savings_Acc_A64+Purposre_Credit_Taken_A40+Purposre_Credit_Taken_A41+Purposre_Credit_Taken_A42+Purposre_Credit_Taken_A43+Purposre_Credit_Taken_A44+Purposre_Credit_Taken_A45+Purposre_Credit_Taken_A46+Purposre_Credit_Taken_A47 +Purposre_Credit_Taken_A48 +Purposre_Credit_Taken_A49+Property_A121 +Property_A122+Property_A123+Years_At_Present_Employment_A71+Years_At_Present_Employment_A72+Years_At_Present_Employment_A73+Years_At_Present_Employment_A74+Housing_151+Housing_152+Other_Inst_Plans_141+Housing_142+Age, family = binomial("logit"),data = newdata)

summary(logreg)
plot(predict(logreg,type="response")) #note plot option has type to get inverse of log_odds
#predicted values range between the probability of 0 to 1

newdata$predicted = predict(logreg,type="response")
write.csv(newdata,"output_logreg.csv", row.names = F)

capture.output(summary(logreg), file = "summary_logreg.csv")
summary_residuals_model.csv<-residuals(logreg, type="deviance")
write.csv(summary_residuals_model.csv, "summary_residuals_model.csv")

### Step 12 - Model Diagnostics

# ROCR

newdata$predicted = predict(logreg,type="response")
pred<-prediction(newdata$predicted,newdata$Default_On_Payment)
perf <- performance(pred,"tpr","fpr")
plot(perf)
abline(a=0, b=1, col="Red")

# AUC
auc.perf = performance(pred, measure = "auc")
auc.perf@y.values

# Create Decile by scorebands
install.packages("dplyr")
library(dplyr)
newdata$decile <- ntile(-newdata$predicted,10)
write.csv(newdata, "newdata.csv")

# KS

max(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]])

# Lift
lift.obj <- performance(pred, "lift", x.measure = "rpp")
plot(lift.obj,
     main=" Lift over the Random",
     xlab="% Populations",
     ylab="Lift",
     col="blue")
abline(1,0,col="grey")

# Lorenz and Gini
install.packages("ineq")
library(ineq)
# Gini Index
ineq(newdata$predicted,type="Gini")

## Lorenz Curve
plot(Lc(newdata$predicted),col="darkred",lwd=2)

##Get Concordance/Pairs Stats

# High-Low Ratio of bad rate by decile in Excel. 
# Confidence interval for ROC, KS, Gini
#  mydata[ which(mydata$gender=='F' & mydata$age > 65), ]

Concordance = function(y,yhat) 
{
  outcome_and_fitted_col<-data.frame(y, yhat)
  colnames(outcome_and_fitted_col)<-c("Responder","fitted.values")
  # get a subset of outcomes where the event actually happened
  ones = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 1,]
  # get a subset of outcomes where the event didn't actually happen
  zeros = outcome_and_fitted_col[outcome_and_fitted_col[,1] == 0,]
  # Equate the length of the event and non-event tables
  if (length(ones[,1])>length(zeros[,1])) {ones = ones[1:length(zeros[,1]),]}
  else {zeros = zeros[1:length(ones[,1]),]}
  # Following will be c(ones_outcome, ones_fitted, zeros_outcome, zeros_fitted)
  ones_and_zeros = data.frame(ones, zeros)
  # initiate columns to store concordant, discordant, and tie pair evaluations
  conc = rep(NA, length(ones_and_zeros[,1]))
  disc = rep(NA, length(ones_and_zeros[,1]))
  ties = rep(NA, length(ones_and_zeros[,1]))
  for (i in 1:length(ones_and_zeros[,1])) {
    # This tests for concordance
    if (ones_and_zeros[i,2] > ones_and_zeros[i,4])
    {conc[i] = 1
    disc[i] = 0
    ties[i] = 0
    }
    # This tests for a tie
    else if (ones_and_zeros[i,2] == ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 0
      ties[i] = 1
    }
    # This should catch discordant pairs.
    else if (ones_and_zeros[i,2] < ones_and_zeros[i,4])
    {
      conc[i] = 0
      disc[i] = 1
      ties[i] = 0 
    }
  }
  # Here we save the various rates
  conc_rate = mean(conc, na.rm=TRUE)
  disc_rate = mean(disc, na.rm=TRUE)
  tie_rate = mean(ties, na.rm=TRUE)
  return(list(concordance=conc_rate, num_concordant=sum(conc), discordance=disc_rate, num_discordant=sum(disc), tie_rate=tie_rate,num_tied=sum(ties)))
}

Concordance_test<-Concordance(newdata$Default_On_Payment,newdata$predicted)

Concordance_test









