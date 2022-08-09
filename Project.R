##### Project-Piot
library(here)
library(tidyverse)
library(caret)
library(DescTools)
library(car)
library(leaps)

# read the data
Heart<-read.csv(here("Heart/Heart_Train.csv"))

# overview about the data
glimpse(Heart)  

# convert all character to factor 
Heart<-Heart[,-1] %>%
  mutate_if(is.character, factor) 

# overview
glimpse(Heart)
# all variables are numeric or factor (the categorial Variables)


# create for each categorial variable a dummy, convert to numeric to be able to see the
# correlation Matrix
dummy<-dummyVars("~.",data=Heart, fullRank=T) # without response HeartDesease (Y)
data<-data.frame(predict(dummy, newdata = Heart))
glimpse(data)
# Now we have 38 numeric variables! Can do Correlation analysis

cors<-data%>%
  cor()
# HeartDisease corralte most with Smoking, Stroke, DiffWalking, Sex, AgeVategory, Diabetic
#, GenHealth and Kidney. 
# Diffwalking can be represented or assosiatec with PhysicalHealth or PhysicalActivity, so 
# we need only one of this



##### now Split the Data to train and test, make Variable Selection with train data
set.seed(123)
# work wtih original data without dummies, works too in this way
# important to do it with this command, because we should have 
# in each dataset the same distribution of class labels
train.id<-createDataPartition(y=Heart$HeartDisease, times = 1, p=0.8, list = F)
train<-Heart[train.id,]
test<-Heart[-train.id,]

table(Heart$HeartDisease)/dim(Heart)[1]
table(train$HeartDisease)/dim(train)[1]
table(test$HeartDisease)/dim(test)[1]
# same Distribution of classes in each dataset


##### Model selection with best subset, forward and backward
# Best Subset Selection 
# Perform Forward stepwise selection in order to identify a satisfactory model
# that uses just a subset of the predictors
reg_fit<-regsubsets(HeartDisease~., data = train, nvmax = 37)
reg_summary<-summary(reg_fit)
reg_summary

measures<-tibble(variables=1:37,
                 CP=reg_summary$cp,
                 BIC=reg_summary$bic,
                 Adj.R2=reg_summary$adjr2) %>%
  pivot_longer(2:4, names_to = "measures")
measures
# compute optimal numbers of variables along with standard errors of measures
stats<-measures %>%
  group_by(measures) %>%
  mutate(optimal= case_when(measures=="CP"~min(value),
                            measures=="BIC"~min(value),
                            measures=="Adj.R2"~max(value)),
         SD=sd(value)) %>%
  filter(value==optimal)
stats

# visualization
ggplot(measures, aes(x=factor(variables), y=value, group=1))+
  geom_line(color="steelblue", size=1)+
  geom_hline(data = stats, aes(yintercept=optimal+0.2*SD),
             linetype="dashed", color="red")+
  geom_hline(data = stats, aes(yintercept=optimal-0.2*SD),
             linetype="dashed", color="red")+
  labs(x="Number of Variables")+
  facet_wrap(~measures, scales = "free_y", ncol=1)

# CP, BIC and R2 scores show that size 13 is the minimum for the subset for which the scores
# are within 0.2 standard deviations of the optimum. We pick up 13 as the best subset size and find
# the best 13 variables based on the eintire data set

reg_summary # the 13te modell with 13 variablen
coefi<-coef(reg_fit, id=13)
vars<-names(coefi)[-c(1)] #wtihout intercept
vars

#[1] "SmokingYes"             "StrokeYes"              "DiffWalkingYes"        
#[4] "SexMale"                "AgeCategory65-69"       "AgeCategory70-74"      
#[7] "AgeCategory75-79"       "AgeCategory80 or older" "DiabeticYes"           
#[10] "GenHealthFair"          "GenHealthGood"          "GenHealthPoor"         
#[13] "KidneyDiseaseYes"      



####### Forward
# Perform Forward stepwise selection in order to identify a satisfactory model
# that uses just a subset of the predictors
reg_fit<-regsubsets(HeartDisease~., data = train, nvmax = 37, method = "forward")
reg_summary<-summary(reg_fit)
reg_summary

measures<-tibble(variables=1:37,
                 CP=reg_summary$cp,
                 BIC=reg_summary$bic,
                 Adj.R2=reg_summary$adjr2) %>%
  pivot_longer(2:4, names_to = "measures")
measures
# compute optimal numbers of variables along with standard errors of measures
stats<-measures %>%
  group_by(measures) %>%
  mutate(optimal= case_when(measures=="CP"~min(value),
                            measures=="BIC"~min(value),
                            measures=="Adj.R2"~max(value)),
         SD=sd(value)) %>%
  filter(value==optimal)
stats

# visualization
ggplot(measures, aes(x=factor(variables), y=value, group=1))+
  geom_line(color="steelblue", size=1)+
  geom_hline(data = stats, aes(yintercept=optimal+0.2*SD),
             linetype="dashed", color="red")+
  geom_hline(data = stats, aes(yintercept=optimal-0.2*SD),
             linetype="dashed", color="red")+
  labs(x="Number of Variables")+
  facet_wrap(~measures, scales = "free_y", ncol=1)

# CP, BIC and R2 scores show that size 13 is the minimum for the subset for which the scores
# are within 0.2 standard deviations of the optimum. We pick up 13 as the best subset size and find
# the best 13 variables based on the eintire data set

reg_summary # das 13te modell mit 13 variablen
coefi<-coef(reg_fit, id=13)
vars<-names(coefi)[-c(1)]
vars# same like best subset

#[1] "SmokingYes"             "StrokeYes"              "DiffWalkingYes"        
#[4] "SexMale"                "AgeCategory65-69"       "AgeCategory70-74"      
#[7] "AgeCategory75-79"       "AgeCategory80 or older" "DiabeticYes"           
#[10] "GenHealthFair"          "GenHealthGood"          "GenHealthPoor"         
#[13] "KidneyDiseaseYes"    



#### backward
# Perform Backward stepwise selection in order to identify a satisfactory model
# that uses just a subset of the predictors
reg_fit<-regsubsets(HeartDisease~., data = train, nvmax = 37, method = "backward")
reg_summary<-summary(reg_fit)
reg_summary

measures<-tibble(variables=1:37,
                 CP=reg_summary$cp,
                 BIC=reg_summary$bic,
                 Adj.R2=reg_summary$adjr2) %>%
  pivot_longer(2:4, names_to = "measures")
measures
# compute optimal numbers of variables along with standard errors of measures
stats<-measures %>%
  group_by(measures) %>%
  mutate(optimal= case_when(measures=="CP"~min(value),
                            measures=="BIC"~min(value),
                            measures=="Adj.R2"~max(value)),
         SD=sd(value)) %>%
  filter(value==optimal)
stats

# visualization
ggplot(measures, aes(x=factor(variables), y=value, group=1))+
  geom_line(color="steelblue", size=1)+
  geom_hline(data = stats, aes(yintercept=optimal+0.2*SD),
             linetype="dashed", color="red")+
  geom_hline(data = stats, aes(yintercept=optimal-0.2*SD),
             linetype="dashed", color="red")+
  labs(x="Number of Variables")+
  facet_wrap(~measures, scales = "free_y", ncol=1)

# CP, BIC and R2 scores show that size 13 is the minimum for the subset for which the scores
# are within 0.2 standard deviations of the optimum. We pick up 13 as the best subset size and find
# the best 13 variables based on the eintire data set

reg_summary # the 13th modell mit 13 variablen
coefi<-coef(reg_fit, id=13)
vars<-names(coefi)[-c(1)]
vars

#[1] "SmokingYes"             "StrokeYes"              "DiffWalkingYes"        
#[4] "SexMale"                "AgeCategory65-69"       "AgeCategory70-74"      
#[7] "AgeCategory75-79"       "AgeCategory80 or older" "DiabeticYes"           
#[10] "GenHealthFair"          "GenHealthGood"          "GenHealthPoor"         
#[13] "KidneyDiseaseYes"  

# Now use only the subset Variablses
mod<-train(HeartDisease~Smoking+Stroke+DiffWalking+Sex+AgeCategory+
             Diabetic+GenHealth+KidneyDisease,
           data = train,
           method="glm",
           family=binomial(),
           trControl=trainControl(method = "none"))
summary(mod)
# all Levels are significant, only two not because:
table(train$HeartDisease, train$AgeCategory)
# not so many observations at 25-29
table(train$HeartDisease, train$Diabetic)
# not so many observations by Yes(durign pregnancy)

##### Now I write a Function that makes predicitions and other measures
evalTrain <- function(model, newdata, dep.var){
  
  # model:object of type 'train'
  # newdata: data frame with regressors (no tibble)
  
  pred.resp <- predict(object = model, newdata = newdata, type = "prob") 
  pred <- ifelse(test = pred.resp$Yes > 0.5, yes = "Yes", no = "No")
  cf.mat <- table(prediction = pred, reference = newdata[, dep.var])
  
  # measures
  accuracy <- sum(diag(cf.mat)) / sum(cf.mat) # how many right all predicitons
  Error_Rate<-1-accuracy #how many we predicted wrong of all
  TPR <- cf.mat[2, 2] / sum(cf.mat[, 2]) # how many right of Yes"s we predcited
  FPR<-1-TPR #How many errors of Yes"s predicted
  TNR<-cf.mat[1, 1] / sum(cf.mat[, 1]) # how many right of No"s we predcited
  FNR<-1-TNR # How many Errors of No"s we predicted
  auc <- roc(response = newdata[, dep.var], predictor = pred.resp$Yes)$auc
  
  out <- list(cf.mat = cf.mat, accuracy = accuracy,Error_Rate=Error_Rate, TPR = TPR, FPR=FPR,
              TNR=TNR, FNR=FNR, auc = auc)
  
  return(out)
  
}

# in-sample
evalTrain(model = mod, newdata = train, dep.var = "HeartDisease")
# high Accuracy, but really high FPR= Predict Yes level very bad, only No"predicted good. 

# out-sample
evalTrain(model = mod, newdata = test, dep.var = "HeartDisease")
# same here
# The problem ist, that we have unbalanced data
table(Heart$HeartDisease)
# more No"s than Yes"s


##### We have to Upsample the HeartDisease
#random oversampling
set.seed(123)
train.us<-upSample(x=train[,-c(1)], y=train$HeartDisease, list = FALSE, yname = "HeartDisease")
table(train.us$HeartDisease)
# same "no" and "Yes"

mod1<-train(HeartDisease~Smoking+Stroke+DiffWalking+Sex+AgeCategory+
              Diabetic+GenHealth+KidneyDisease,
            data = train.us,
            method="glm",
            family=binomial(),
            trControl=trainControl(method = "none"))
summary(mod1)
# all levels significat now!

evalTrain(model = mod1, newdata = train.us, dep.var = "HeartDisease")
# less accurace but much higher TPR!!!
evalTrain(model = mod1, newdata = test, dep.var = "HeartDisease")
## much better predictions !! TPR High



### 5 fold CV
# wtih normal train data
set.seed(123)
#ii) write own summary "mySummary" to the Accuracy and TPR per fold
mySummary<-function(data, lev=NULL, model=NULL){
  out<-c(postResample(pred = data[,"pred"], obs = data[,"obs"]),
         sensitivity(data = data[,"pred"], reference = data[,"obs"], positive = lev[2]))
  names(out)<-c("Accuracy", "Kappa", "Sens")
  return(out)
}

folds<-createMultiFolds(y=train$HeartDisease, k=5, times = 1)
class(folds)
names(folds)

mod2<-train(HeartDisease~Smoking+Stroke+DiffWalking+Sex+AgeCategory+
              Diabetic+GenHealth+KidneyDisease, 
            data=train,
            method="glm", 
            family="binomial", 
            trControl=trainControl(method = "repeatedcv",
                                   index = folds, summaryFunction = mySummary))
summary(mod2)
mod2$resample$Accuracy

# wrong way, because with UpSampling-Data before CV
mod3<-train(HeartDisease~Smoking+Stroke+DiffWalking+Sex+AgeCategory+
              Diabetic+GenHealth+KidneyDisease, 
            data=train.us,
            method="glm", 
            family="binomial", 
            trControl=trainControl(method = "repeatedcv",
                                   index = folds, summaryFunction = mySummary))
summary(mod3)


# right way, Upsampling during CV
mod4<-train(HeartDisease~Smoking+Stroke+DiffWalking+Sex+AgeCategory+
              Diabetic+GenHealth+KidneyDisease, 
            data=train,
            method="glm", 
            family="binomial", 
            trControl=trainControl(method = "repeatedcv", index = folds, sampling = "up",
                                   summaryFunction = mySummary))
summary(mod4)

# plot the boxplot and distribution of error rates and FNR per model
#Error Rates
par(mfrow=c(2,1))
boxplot(1-mod2$resample$Accuracy,
        1-mod3$resample$Accuracy,
        1-mod4$resample$Accuracy,
        names = c("mod2", "mod3", "mod4"),
        col = c("blue", "red","darkgreen"),
        ylab="Error Rate=1-Accuracy")
# mod2 has the  smalles Error rate, but accuracy is alway good by unbalanced
# data, because we predicted the most class "No".

# FNR
boxplot(1-mod2$resample$Sens,
        1-mod3$resample$Sens,
        1-mod4$resample$Sens,
        names = c("mod2", "mod3", "mod4"),
        col = c("blue", "red","darkgreen"),
        ylab="Miss Error Rate=1-Accuracy")
# mod 4 has the smallest False negative Rate, so fraction of positive examples,
# that are classified as negative
# Mod3 has a very high Error Rate (boxplot1) so we would take mod2 with wrong upsampling,
# but mod2 and mod3 have a realy high miss-Error (boxplot 2)
# So mod4 ist the best, predict best, UpSampling during the 5-fold CV!

evalTrain(model = mod4, newdata = test, dep.var = "HeartDisease")
# good Predicitons!!


######## 10-fold CV
set.seed(123)
folds<-createMultiFolds(y=train$HeartDisease, k=10, times = 1)
class(folds)
names(folds)

mod5<-train(HeartDisease~Smoking+Stroke+DiffWalking+Sex+AgeCategory+
              Diabetic+GenHealth+KidneyDisease, 
            data=train,
            method="glm", 
            family="binomial", 
            trControl=trainControl(method = "repeatedcv",
                                   index = folds, summaryFunction = mySummary))
summary(mod5)
mod5$resample$Accuracy

# wrong way, because with UpSampling-Data before CV
mod6<-train(HeartDisease~Smoking+Stroke+DiffWalking+Sex+AgeCategory+
              Diabetic+GenHealth+KidneyDisease, 
            data=train.us,
            method="glm", 
            family="binomial", 
            trControl=trainControl(method = "repeatedcv",
                                   index = folds, summaryFunction = mySummary))
summary(mod6)


# right way, Upsampling during CV
mod7<-train(HeartDisease~Smoking+Stroke+DiffWalking+Sex+AgeCategory+
              Diabetic+GenHealth+KidneyDisease, 
            data=train,
            method="glm", 
            family="binomial", 
            trControl=trainControl(method = "repeatedcv", index = folds, sampling = "up",
                                   summaryFunction = mySummary))
summary(mod7)

# plot the boxplot and distribution of error rates and FNR per model
#Error Rates
par(mfrow=c(2,1))
boxplot(1-mod5$resample$Accuracy,
        1-mod6$resample$Accuracy,
        1-mod7$resample$Accuracy,
        names = c("mod5", "mod6", "mod7"),
        col = c("blue", "red","darkgreen"),
        ylab="Error Rate=1-Accuracy")
# mod5 has the  smalles Error rate, but accuracy is always good by unbalanced
# data, because we predicted the most class "No".

# FNR
boxplot(1-mod5$resample$Sens,
        1-mod6$resample$Sens,
        1-mod7$resample$Sens,
        names = c("mod5", "mod6", "mod7"),
        col = c("blue", "red","darkgreen"),
        ylab="Miss Error Rate=1-Accuracy")
# mod 7 has the smallest False negative Rate, so fraction of positive examples,
# that are classified as negative
# Mod6 has a very high Error Rate (boxplot1) so we would take mod5 with wrong upsampling,
# but mod5 and mod6 have a really high miss-Error (boxplot 2)
# So mod7 ist the best, predict best
#### Same result with 10-fold-CV

evalTrain(model = mod7, newdata = test, dep.var = "HeartDisease")
# good Predicitons!!


###### So the Final Model for Logistic Regression is to do The UpSampling during the CV! In 
# this case we get the Highest PRediction for Both Classes!!