# Task: predict Heart Disease Risk for people in the US (Classification).

# Process:
# 1. Adjusting AgeCategory(<35 : 0, >= : 1) and GenHealth variables(0,1,2,3)
# 2. Checking variables with near zero variance and remove them
# 3. Checking corelation -> we need to deal with corelation between GenHealth, DiffWalking, and PhysicalHealth
# 4. Applying the Principle component Analysis
# PCA: Principal component analysis (PCA) is the process of computing the principal components and using them to perform a change of basis on the data, sometimes using only the first few principal components and ignoring the rest.
# https://en.wikipedia.org/wiki/Principal_component_analysis

library(readr)
library(tidyverse)
library(caret)
library(tidymodels)
library(MASS)



set.seed(111)

# 1. Loading and sampling the dataset

raw_data <- read_csv("Heart_Train.csv")
heart_disease <- raw_data[,-1] %>% # eliminating non-significant variables
  mutate_if(is.character, factor)

dummy<-dummyVars("~.",data=heart_disease, fullRank=T)
data<-data.frame(predict(dummy, newdata = heart_disease))

AgeCategory <- ifelse(raw_data$AgeCategory == "25-29", 0,
                      ifelse(raw_data$AgeCategory == "30-34", 0,1))
GenHealth <- ifelse(raw_data$GenHealth == "Poor", 0,
                    ifelse(raw_data$GenHealth == "Fair", 1,
                           ifelse(raw_data$GenHealth == "Good", 2,3)))


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
# Genhealth, DiffWalking.Yes, PhysicalHealth have pretty high correlation

principle_component <- prcomp(heart_var_adj[,-1], scale. =T)
summary(principle_component)
screeplot(principle_component, type="lines", pch=1, main="scree plot")
desc(principle_component$rotation[,6:10])

df <- as.matrix(heart_var_adj[,-1]) %*% principle_component$rotation[,1:12]
df <- cbind(df, as.data.frame(heart_var_adj$HeartDisease.Yes))
colnames(df)[13] <- "Heart_Disease"
head(df)


# 2. Creating training set

train_df_id <- sample(1:nrow(df), nrow(df)*0.75)
df_train <- df[train_id,]
df_test <- df[-train_id,]


# 3. Prediction Checking the validity

df_fit <- train(Heart_Disease~.,
                data=df_train,
                method="glm",
                family=binomial(),
                trControl=trainControl(method = "none"))

summary(df_fit)


df_pred <- df_fit$finalModel %>%
  predict(df_test, type = "response") %>%
  as_tibble() %>%
  mutate(Predicted = ifelse(value>=0.5, "Yes", "No"))

df_table <- table(Predicted=df_pred$Predicted)
prob_table <- df_table %>%
  as.data.frame() %>%
  mutate(prob = c(99.32, 0.68))
