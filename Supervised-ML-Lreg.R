library(tidyverse)
library(ggplot2)
library(ggtext)
install.packages("ggplot2")
install.packages("tidyverse")
library(gtools)
library(dslabs)
library(dplyr)
library(purrr)
library(pkgbuild)
sys.which("make")
find_rtools()
install.packages("jsonlite", type= source)
install.packages("pkgbuild")
install.packages("gtools")
install.packages("Metrics")
PATH="${RTOOLS40_HOME}\usr\bin;${PATH}"
writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
Sys.which("make")
install.packages("jsonlite", type= source)
cd rtools-packages/mingw-w64-arrow
makepkg-mingw --syncdeps

# Importing csv file
read <- read.csv("https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv")
datapoll <- data.frame(read)
View(datapoll)
is.na(datapoll) # No missing data 

# Data Exploration
#1..Fitting Data (Normality) for linear Regression Model

ggplot(data= datapoll ,aes(x= Hours, y= Scores))+
  geom_point(data= datapoll ,
             aes(Hours, Scores), 
             color= 'orange', 
             size= 4)+
  geom_point()+
  labs(title= "Fitting Data Linear Model",y= "Scores", x= "Number of Hours")

#..the plot describes a positive linear pattern, so we will use linear regression 

#2..Data Visualization Linear Regression Model. 
linearreg <- lm(Scores ~ Hours, data = datapoll) #Data frame as I mentioned before 
summary(linearreg) 

ggplot(data= datapoll ,aes(x= Hours, y= Scores))+
  geom_point(data= datapoll ,
             aes(Hours, Scores), 
             color= 'orange', 
             size= 4)+
  geom_point()+
  geom_smooth(method = "lm", se = TRUE)+
  labs(title= "Linear Regression Model",y= "Scores", x= "Number of Hours")

#Data Splitting into training and testing data
set.seed(1)
dsplitting <- sort(sample(nrow(datapoll),nrow(datapoll)*0.2))
View(dsplitting)

train_part <- datapoll[dsplitting, ]
test_full <- datapoll[-dsplitting, ]

# Data Training
x <- train_part$Hours
y <- train_part$Scores

linearreg_train_part<- lm(y ~ x, data = train_part)
summary(linearreg_train_part)

#Linear Regression Model for the Training data -"Scores"
ggplot(data= train_part,aes(x= Hours, y= Scores))+
  geom_point(color= 'red', size= 4)+
  geom_smooth(method = "lm", se = TRUE)+
  labs(title= "Train Data Scores Linear Model",y= "Scores", x= "Number of Hours")

# Predicting required data - "Scores" 
y <- train_part$Scores
actual_scores <- train_part$Scores
predict_scores <- predict(linearreg_train_part, train_part)

#compare actual vs. predictions Scores values
compare_scores <- data.frame(actual_scores,predict_scores)
compare_scores

#Predictions to Linear Model, Predict a "Score" for 9.25hrs/day
prediction <- predict(linearreg, data.frame(Hours= 9.25)) 
prediction

predict(linearreg, data.frame(Hours)) <--# the prediction Model
  
#Evaluating the Model,by Root Metric Squared Error RMSE
library(Metrics)
rmse(actual_scores,predict_scores)
