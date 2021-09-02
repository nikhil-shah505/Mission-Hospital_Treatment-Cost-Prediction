setwd("C:/Users/Souvik/Downloads/PPA")

library(car)
library(corrplot)
library(caret)
library(caTools)
library(psych)

Mission_Hospital <- read.csv("Mission Hospital.csv", header = TRUE)
dim(Mission_Hospital)
str(Mission_Hospital)
summary(Mission_Hospital)
which(is.na(Mission_Hospital))
which(is.na(Mission_Hospital$BODY_WEIGHT))

#Simple Linear Regression to check if there is association b/w  Total Cost and Body Weight 
model0 <- lm(TOTAL_COST_TO_HOSPITAL  ~ BODY_WEIGHT, data = Mission_Hospital)
summary(model0)

#the correlation between variable "Age", "Body Weight", "Body Height", 
#"Total Length of Stay", "Length of Stay ICU", "Cost of Implant", "Total Cost to Hospital"

which(is.na(Mission_Hospital$AGE))
which(is.na(Mission_Hospital$BODY_HEIGHT))
which(is.na(Mission_Hospital$TOTAL_LENGTH_OF_STAY))
which(is.na(Mission_Hospital$LENGTH_OF_STAY_ICU))
which(is.na(Mission_Hospital$COST_OF_IMPLANT))
which(is.na(Mission_Hospital$TOTAL_COST_TO_HOSPITAL))

cr <- cor(Mission_Hospital[c("AGE","BODY_WEIGHT","BODY_HEIGHT","TOTAL_LENGTH_OF_STAY",
                             "LENGTH_OF_STAY_ICU","COST_OF_IMPLANT","TOTAL_COST_TO_HOSPITAL")])
cr
corrplot(cr, type = "full")
corrplot(cr,method = "number")
corrplot.mixed(cr)

#pairs panels 
pairs.panels(Mission_Hospital[c("AGE","BODY_WEIGHT","BODY_HEIGHT","TOTAL_LENGTH_OF_STAY",
                                "LENGTH_OF_STAY_ICU","COST_OF_IMPLANT","TOTAL_COST_TO_HOSPITAL")])

#Models Creating

model1 <- lm(TOTAL_COST_TO_HOSPITAL  ~ AGE, data = Mission_Hospital)
summary(model1)

model2 <- lm(TOTAL_COST_TO_HOSPITAL  ~ AGE+BODY_WEIGHT , data = Mission_Hospital)
summary(model2)

model3 <- lm(TOTAL_COST_TO_HOSPITAL  ~ AGE+BODY_HEIGHT , data = Mission_Hospital)
summary(model3)

model4 <- lm(TOTAL_COST_TO_HOSPITAL  ~ AGE+TOTAL_LENGTH_OF_STAY , data = Mission_Hospital)
summary(model4)

model5 <- lm(TOTAL_COST_TO_HOSPITAL  ~ AGE+TOTAL_LENGTH_OF_STAY+LENGTH_OF_STAY_ICU , data = Mission_Hospital)
summary(model5)

model6 <- lm(TOTAL_COST_TO_HOSPITAL  ~ AGE+TOTAL_LENGTH_OF_STAY+LENGTH_OF_STAY_ICU+COST_OF_IMPLANT , data = Mission_Hospital)
summary(model6)

#Heteroscedasticity check 
plot(model6$fitted.values,model6$residuals)

#to remove outliers
findoutliers<- function(TOTAL_LENGTH_OF_STAY){
  lowerq= quantile(TOTAL_LENGTH_OF_STAY)
  upperq= quantile(TOTAL_LENGTH_OF_STAY)
  iqr= upperq-lowerq
  extremelower= lowerq-(iqr*3)
  extremeupper= upperq+(iqr*3)
  result<- which(TOTAL_LENGTH_OF_STAY>extremeupper|TOTAL_LENGTH_OF_STAY<extremelower)
  return(result)
}
View(Mission_Hospital[findoutliers(Mission_Hospital$TOTAL_LENGTH_OF_STAY)])


