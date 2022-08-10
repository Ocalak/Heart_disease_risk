# Task: predict Heart Disease Risk for people in the US (Classification).
# Method: Decision Tree
# Trees can easily handle qualitative predictors without the need to create dummy variables(lecture slide p.326).
# Predicted value: HeartDisease

install.packages("corrplot")

library(readr)
library(tidyverse)
library(caret)
library(rpart)
library(rpart.plot)
library(tidymodels)
library(tree)
library(MASS)
library(corrplot)




set.seed(111)

'linear_function <- function(factor) {
  reg <- lm(heart_disease$HeartDisease ~ factor)
  return(reg$coefficient)
}
col <- c(names(heart_disease))
data_col <- rep("heart_disease$", 8)
sum <- paste(data_col,col)
sapply(sum, function(factor) linear_function(factor))
linear_function

reg <- lm(HeartDisease ~ ., data = heart_disease)
log(reg$coefficients)
var(heart_disease$Race)



pc <- prcomp(data)
summary(pc)
names(pc)
pc$rotation
pc_data <- pc[1:4]
summary(pc_data)'

# 1. Loading and sampling the dataset

raw_data <- read_csv("Heart_Train.csv")
heart_disease <- raw_data[,-1] %>% # eliminating non-significant variables
  #select(-c(BMI, AlcoholDrinking, MentalHealth, Race, SleepTime)) %>%
  mutate_if(is.character, factor)

dummy<-dummyVars("~.",data=heart_disease, fullRank=T)
data<-data.frame(predict(dummy, newdata = heart_disease))

AgeCategory <- ifelse(raw_data$AgeCategory == "25-29", 0,
         ifelse(raw_data$AgeCategory == "30-34", 0,1))
GenHealth <- ifelse(raw_data$GenHealth == "Poor", 0,
                    ifelse(raw_data$GenHealth == "Fair", 1,
                           ifelse(raw_data$GenHealth == "Good", 2,3)))

'heart_age <- rbind(data$AgeCategory.25.29, data$AgeCategory.30.34,
                   data$AgeCategory.35.39, data$AgeCategory.40.44,
                   data$AgeCategory.45.49, data$AgeCategory.50.54,
                   data$AgeCategory.55.59, data$AgeCategory.60.64,
                   data$AgeCategory.65.69, data$AgeCategory.70.74,
                   data$AgeCategory.75.79, data$AgeCategory.80.or.older)'

heart_age_adj <- cbind(data, AgeCategory)
heart_age_adj <- heart_age_adj[,-c(10:21)]

heart_adj <- cbind(heart_age_adj, GenHealth)
heart_adj <- heart_adj[, -c(19:22)]

nearZeroVar(heart_adj, saveMetrics = TRUE) # checking near zero variables
heart_var_adj <- heart_adj[, -nearZeroVar(heart_adj)] # eliminating near zero variables

cor <- cor(heart_var_adj[,-1])
cor_result <- c(cor(heart_var_adj[,-1]$GenHealth, heart_var_adj[,-1]$PhysicalHealth),
                cor(heart_var_adj[,-1]$GenHealth, heart_var_adj[,-1]$DiffWalking.Yes),
                cor(heart_var_adj[,-1]$DiffWalking.Yes, heart_var_adj[,-1]$PhysicalHealth))
# -0.5316947 -0.4437107  0.4278656

principle_component <- prcomp(heart_var_adj[,-1], scale. =T)
summary(principle_component)
screeplot(principle_component, type="lines", pch=1, main="scree plot")
desc(principle_component$rotation[,6:10])

df <- as.matrix(heart_var_adj[,-1]) %*% principle_component$rotation[,1:12]
df <- cbind(df, as.data.frame(heart_var_adj$HeartDisease.Yes))
colnames(df)[13] <- "Heart_Disease"
head(df)


# 2. Creating training set and fit a tree

train_id <- sample(1:nrow(data), nrow(data)*0.75)
heart_train <- data[train_id,]
heart_test <- data[-train_id,]
heart_adj_id <- sample(1:nrow(heart_adj), nrow(heart_adj)*0.75)
heart_adj_train <- heart_adj[train_age_id,]
heart_adj_test <- heart_adj[-train_age_id,]

train_df_id <- sample(1:nrow(df), nrow(df)*0.75)
df_train <- df[train_id,]
df_test <- df[-train_id,]

fit <- rpart(HeartDisease.Yes ~., data = data, method = "anova")
fit_adj <- rpart(HeartDisease.Yes ~., data = heart_adj, method = "anova")
summary(fit_adj)
plot(fit_adj)
text(fit_age,pretty=0)

# 3. Prediction Checking the validity

head(predict(fit_age, newdata = heart_age_test))
tail(predict(fit_age, newdata = heart_age_test))

prune.c <- rpart::prune(fit_age, 0.010167) # pruning
plot(prune.c)
rpart.plot(prune.c)
text(prune.c)


printcp(fit_age)
plotcp(fit_age)

'tree_fit <- train(
  HeartDisease ~.,
  data = heart_train,
  method = "rpart",
  trControl = trainControl(method = "none"),
  tuneGrid = tibble(cp = 0.01) # default in rpart
  )'

log_fit<-train(HeartDisease.Yes~.,
               data=heart_adj_train,
               method="glm",
               family=binomial(),
               trControl=trainControl(method = "none"))
summary(log_fit)

df_fit <- train(Heart_Disease~.,
                data=df_train,
                method="glm",
                family=binomial(),
                trControl=trainControl(method = "none"))
summary(df_fit)


log_pred<- log_fit$finalModel %>%
  predict(heart_adj_test,type="response") %>%
  as_tibble() %>%
  mutate(Observed=heart_adj_test$HeartDisease.Yes,
         Predicted=ifelse(value>= 0.5, "Yes", "No"))

df_pred <- df_fit$finalModel %>%
  predict(df_test, type = "response") %>%
            as_tibble() %>%
            mutate(Predicted = ifelse(value>=0.5, "Yes", "No"))

table(Predicted=df_pred$Predicted)
71467/71953*100
487/71953*100

df_pred <- df_fit$finalModel %>%
  predict(df_test, type = "response")
results <- data.frame(df_pred, df_test$Heart_Disease)
results <- as_tibble(results)
colnames(results) <- c("prob", "Heart_Disease")
head(results)

results <- results %>%
  mutate(Predicted =
  ifelse(
  prob >= 0.5, "Yes", "No"
  ))
