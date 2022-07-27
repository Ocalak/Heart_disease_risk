library(here)
library(tidyverse)
library(caret)

# read the data
Heart<-read.csv(here("Heart/Heart_Train.csv")) %>%
  as_tibble()

# overview about the data
glimpse(Heart)  

# convert all character to factor and order in Age-Categories, Remove X
heart<-Heart[,-1] %>%
  mutate_if(is.character, factor) %>%
  arrange(AgeCategory)

# overview
glimpse(heart)
# all variables are numeric or factor (the categorial Variables)


# create for each categorial variable a dummy, convert to numeric
dummy<-dummyVars("~.",data=heart[,-1], fullRank=T) # without response HeartDesease (Y)
data<-data.frame(predict(dummy, newdata = heart[,-1]))
dataset<-cbind(heart[,1], data)
glimpse(dataset)
# Now we have 38 numeric variables! Can do Correlation analysis



# Slide 4-20, we have to use CV already by pro-processing clean data
# Now Divide datset into 80/20
set.seed(123)
train.id<-createDataPartition(y=dataset$HeartDisease, times = 1, p=0.8, list = F)

train<-dataset[train.id,]
test<-dataset[-train.id,]

# now do the correlation analysis to look for coliniarity ( correlation between Predictors)
cors<- train%>%
  select_if(is.numeric)%>%
  cor()

cors %>%
  as_tibble()%>%
  mutate(var1=colnames(cors)) %>%
  pivot_longer(-var1, names_to = "var2", values_to = "value") %>%
  ggplot(aes(x=var1, y=var2, fill=value))+
  geom_tile()

# or 
?cor()
?findCorrelation
cor1<-cor(train[,-1], method = "spearman", use = "pairwise.complete.obs") # do correlations
highC<-findCorrelation(cor1, cutoff = 0.55, exact = T) #search for correlation 55%
train<-train[,-highC] # remove this Variable
# Race.Other out, has the highest correlation with race white

# Now look at logistic regression
glimpse(train)
train<-as_tibble(train)
log_fit<-train(HeartDisease~.,
               data=train,
               method="glm",
               family=binomial(),
               trControl=trainControl(method = "none"))
summary(log_fit)
# AgeCategorie 25-29, Race.White, Diabetic.Yes..during.pregnancy.and PhysicalActivity.Yes
# seems to be not sifnificant, so we can remove them


log_pred<- log_fit$finalModel %>%
  predict(test,type="response") %>%
  as_tibble() %>%
  mutate(Observed=test$HeartDisease,
         Predicted=ifelse(value>= 0.5, "Yes", "No"))

table(Predicted=log_pred$Predicted, Observed=log_pred$Observed)

# mean of all correct predictions
log_pred %>%
  summarise(perc_correct=mean(Observed==Predicted))
# seems high correct prediction 91,6%, but comes all from predicted "No"!



# Remove all no significant variables
log_fit2<-train(HeartDisease~.-AgeCategory.25.29-Race.White-Diabetic.Yes..during.pregnancy.
               -PhysicalActivity.Yes,
               data=train,
               method="glm",
               family=binomial(),
               trControl=trainControl(method = "none"))
summary(log_fit2)
# All predictors seem to be significant!


log_pred2<- log_fit2$finalModel %>%
  predict(test,type="response") %>%
  as_tibble() %>%
  mutate(Observed=test$HeartDisease,
         Predicted=ifelse(value>= 0.5, "Yes", "No"))

table(Predicted=log_pred2$Predicted, Observed=log_pred2$Observed)

# mean of all correct predictions
log_pred2 %>%
  summarise(perc_correct=mean(Observed==Predicted))
# seems high correct prediction like before 91,6%, but comes all from predicted "No"!


# so we can use all these Variables (Maybe), start from these data set
glimpse(dataset)

data_set<-dataset %>%
  as_tibble()%>%
  select(-c(AgeCategory.25.29,Race.White,Diabetic.Yes..during.pregnancy.,
            PhysicalActivity.Yes))
data_set # With this data set starts our Analysis?
# We have to look, that we are able predict good both classes "Yes" and "no",
# and no only "No"!
