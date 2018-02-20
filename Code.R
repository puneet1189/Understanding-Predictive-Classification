#Loading required packages:
if (!require("readr")) {
  suppressWarnings(install.packages("twitteR", repos="http://cran.rstudio.com/"))
  suppressWarnings(library("readR"))
}
if (!require("tidyr")) {
  suppressWarnings(install.packages("twitteR", repos="http://cran.rstudio.com/"))
  suppressWarnings(library("tidyr"))
}
if (!require("dplyr")) {
  suppressWarnings(install.packages("twitteR", repos="http://cran.rstudio.com/"))
  suppressWarnings(library("dplyr"))
}
if (!require("catools")) {
  suppressWarnings(install.packages("twitteR", repos="http://cran.rstudio.com/"))
  suppressWarnings(library("catools"))
}
if (!require("ggplot2")) {
  suppressWarnings(install.packages("twitteR", repos="http://cran.rstudio.com/"))
  suppressWarnings(library("ggplot2"))
}
if (!require("caret")) {
  suppressWarnings(install.packages("twitteR", repos="http://cran.rstudio.com/"))
  suppressWarnings(library("caret"))
}
if (!require("randomforest")) {
  suppressWarnings(install.packages("twitteR", repos="http://cran.rstudio.com/"))
  suppressWarnings(library("randomforest"))
}

#----------------------------------------------------------------------------------------------------------------------------------------------------

#Set working directory:
setwd("C:/Users/punee/Desktop/New/Logistic Reg")

#----------------------------------------------------------------------------------------------------------------------------------------------------

#Load CSVs:
dat1 <- read_csv("adult.csv",col_names = TRUE,na = c("", "NA"),quoted_na = TRUE)

#----------------------------------------------------------------------------------------------------------------------------------------------------

#Working on Factors:
table(dat1$workclass)
dat1$workclass[dat1$workclass=="Federal-gov"|dat1$workclass=="Local-gov"|dat1$workclass=="State-gov"] <- "Government"
dat1$workclass[dat1$workclass=="Never-worked"|dat1$workclass=="Without-pay"] <- "No Earnings"
dat1$workclass[dat1$workclass=="Self-emp-inc"|dat1$workclass=="Self-emp-not-inc"] <- "Self Employed"
dat1$workclass <- as.factor(dat1$workclass)

table(dat1$`marital-status`)
dat1$`marital-status`[dat1$`marital-status`=="Married-AF-spouse" | dat1$`marital-status`=="Married-civ-spouse" | dat1$`marital-status`=="Married-spouse-absent"] <- "Married"
dat1$`marital-status`[dat1$`marital-status`=="Never-married" | dat1$`marital-status`=="Divorced" | dat1$`marital-status`=="Widowed"] <- "Unmarried"
dat1$`marital-status` <- as.factor(dat1$`marital-status`)

table(dat1$race)
dat1$race <- as.factor(dat1$race)

table(dat1$`educational-num`)
dat1$`educational-num` <- as.factor(dat1$`educational-num`)

table(dat1$education)
dat1$education <- as.factor(dat1$education)

table(dat1$occupation)
dat1$occupation <- as.factor(dat1$occupation)

table(dat1$relationship)
dat1$relationship <- as.factor(dat1$relationship)

table(dat1$gender)
dat1$gender <- as.factor(dat1$gender)

table(dat1$income)
dat1$income <- as.factor(dat1$income)

table(dat1$`native-country`)
eur.cntry <- c("Yugoslavia","Scotland","England","France","Germany","Greece","Holand-Netherlands","Hungary","Ireland","Italy","Laos","Philippines","Poland","Portugal")
asi.cntry <- c("Vietnam","Cambodia","China","Hong","India","Iran","Japan","Taiwan","Thailand")
noram.cntry <- c("United-States","Trinadad&Tobago","Puerto-Rico","Canada","Cuba","Dominican-Republic","El-Salvador","Guatemala","Haiti","Honduras","Jamaica","Mexico","Nicaragua","Outlying-US(Guam-USVI-etc)")
souam.cntry <- c("Columbia","Ecuador","Peru")
oth.cntry <- c("South","?")
dat1$`native-country`[dat1$`native-country` %in% eur.cntry] <- "Europe"
dat1$`native-country`[dat1$`native-country` %in% asi.cntry] <- "Asia"
dat1$`native-country`[dat1$`native-country` %in% noram.cntry] <- "North America"
dat1$`native-country`[dat1$`native-country` %in% souam.cntry] <- "South America"
dat1$`native-country`[dat1$`native-country` %in% oth.cntry] <- "Other"
dat1$`native-country` <- as.factor(dat1$`native-country`)

#----------------------------------------------------------------------------------------------------------------------------------------------------

#Check structure again
str(dat1)

#----------------------------------------------------------------------------------------------------------------------------------------------------

dat1[dat1 == "?"] <- NA

#Omitting NA value rows. NAs
na_count <-sapply(dat1, function(dat1) sum(length(which(is.na(dat1)))))
na_count <- data.frame(na_count)
na_count
#We'll omit NAs because of the less number of NAs
dat1 <- na.omit(dat1)


#----------------------------------------------------------------------------------------------------------------------------------------------------

#Exploratory Analysis:

ggplot(dat1, aes(x=`native-country`,fill=income)) + geom_bar(color = "black")
#North Americans earn most >50k. Maybe because of the high count of North Americans in the data and the strong power of dollar over other currencies

ggplot(dat1, aes(x=`marital-status`,fill=income)) + geom_bar(color = "black")
#We can infer that Married people earn more. This seems logical because a stable life leads to a stable stable income.

ggplot(dat1, aes(x=age,fill=income)) + geom_histogram(color = "black",binwidth = 1)
#We can see that people from ages 25-60 tend to earn more than those not in ages 25-60. Sounds logical!

ggplot(dat1, aes(x=dat1$`hours-per-week`,fill=income)) + geom_histogram(color = "black",binwidth = 8)
#People working 32-48 hours a week earn most. Of course!

#----------------------------------------------------------------------------------------------------------------------------------------------------

#Build Model:
#CLASSIFICATION SUPERVISED LEARNING: LOGISTIC REGRESSION:

# Set seed
set.seed(42)

# Shuffle row indices: rows
rows<-sample(nrow(dat1))

# Randomly order data
dat1 <- dat1[rows, ]

#SPLIT DATA 80-20
# Determine row to split on: split
split <- round(nrow(dat1)*.80)

# Create train
train<-dat1[1:split,]

# Create test
test<-dat1[(split+1):nrow(diamonds),]

# Fit glm model: model
model <- glm(income ~ ., family= "binomial" ,train)
summary(model)

# Predict on test: p
p <- predict(model,test,type="response")

#CONFUSION MATRIX:
# Calculate class probabilities: p_class
p_class <- ifelse(p>.53,">50K","<=50K")
confusionMatrix(p_class,test[["income"]])
#We can see Accuracy of model ~ 84%, which is pretty good, also greater than no info rate. Good model.

library(pROC)
roc_curve   <- roc(response = test$income, predictor = p)
plot(roc_curve, print.thres = "best")
abline(v = 1, lty = 2)
abline(h = 1, lty = 2)
text(.90, .97, labels = "Ideal Model")
points(1,1, pch = "O", cex = 1.5)
#AUC 90% - Good Model!
#----------------------------------------------------------------------------------------------------------------------------------------------------

#Let's try and build a better model
#Let's try cross Validation instead of 80-20 sampling:
#Google what is cross validation. 

# Create trainControl object: myControl
#You can use the trainControl() function in caret to use AUC (instead of acccuracy), to tune the parameters of your models. 
#The twoClassSummary() convenience function allows you to do this easily.
myControl <- trainControl(
  method = "cv", #Can use bootstrap resampling, but data too large. Can use method="boot"
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

#When using twoClassSummary(), be sure to always include the argument classProbs = TRUE or your model will throw an error! 
#(You cannot calculate AUC with just class predictions. You need to have class probabilities as well.)
# Train glm with custom trainControl: model
dat2 <- dat1
dat2$income <- as.character(dat2$income)
dat2$income[dat2$income=="<=50K"] <- "Less.than."
dat2$income[dat2$income==">50K"] <- "Greater.than"
dat2$income <- as.factor(dat2$income)

model_cv <- train(income ~., dat2,method = "glm",trControl = myControl)
model_cv #ROC~90%, good model
summary(model_cv)
#It is the estimated amount by which the log odds of income would increase if Estimate Vars were one unit higher. 
#Here is how to interpret data: https://stats.stackexchange.com/questions/86351/interpretation-of-rs-output-for-binomial-regression
#Residual deviance is a measure of the lack of fit of your model taken as a whole
#Null deviance is such a measure for a reduced model that only includes the intercept
#----------------------------------------------------------------------------------------------------------------------------------------------------

#RANDOM FOREST:

#Changing column names because hyphens are not accepted by RF!
dat3 <- train
colnames(dat3)[which(names(dat3) == "educational-num")] <- "edu.num"
colnames(dat3)[which(names(dat3) == "marital-status")] <- "marital.status"
colnames(dat3)[which(names(dat3) == "capital-gain")] <- "capital.gain"
colnames(dat3)[which(names(dat3) == "capital-loss")] <- "capital.loss"
colnames(dat3)[which(names(dat3) == "hours-per-week")] <- "hours.per.week"
colnames(dat3)[which(names(dat3) == "native-country")] <- "native.country"

dat4 <- test
colnames(dat4)[which(names(dat4) == "educational-num")] <- "edu.num"
colnames(dat4)[which(names(dat4) == "marital-status")] <- "marital.status"
colnames(dat4)[which(names(dat4) == "capital-gain")] <- "capital.gain"
colnames(dat4)[which(names(dat4) == "capital-loss")] <- "capital.loss"
colnames(dat4)[which(names(dat4) == "hours-per-week")] <- "hours.per.week"
colnames(dat4)[which(names(dat4) == "native-country")] <- "native.country"
# Fit random forest: model_rf
set.seed(818)

model_rf <- randomForest(income ~ ., data=dat3, mtry=3,importance=TRUE,ntree=2000)
#Finding Important Variables
varImpPlot(model_rf)

prediction <- predict(model_rf, dat4)
p_class <- ifelse(p>.5,">50K","<=50K")
confusionMatrix(p_class,dat4[["income"]])
#Similar Accuracy ~ 84%

#----------------------------------------------------------------------------------------------------------------------------------------------------

#Trying glmnet:
set.seed(555)
# Create custom trainControl: myControl
myControl <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = T, # IMPORTANT!
  verboseIter = TRUE
)


# Fit glmnet model: model
model <- train(income~., dat2,method = "glmnet",trControl = myControl)
summary(model)
# Print maximum ROC statistic
max(model[["results"]][["ROC"]])

#ROC .9 is very good.
