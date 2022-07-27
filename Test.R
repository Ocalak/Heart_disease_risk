library(tidyverse)
library(dplyr)
library(caret)
library(lattice)
data <- read.csv("~/Downloads/Heart_Train.csv")
data<-Heart
data <- as_tibble(data)
glimpse(data)
data2 <- data %>% mutate_if(is.character,as.factor)


data2$AgeCategory <- unclass(data2$AgeCategory)
data2$Race <- unclass(data2$Race)
data2$Diabetic <- unclass(data2$Diabetic)
data2$GenHealth <- unclass(data2$GenHealth)
data2 <- data2 %>% mutate(HeartDisease = ifelse(data2$HeartDisease == "No",0,1))
levels(data2$HeartDisease) = c("No","Yes")
data2 <- data2 %>% mutate(Smoking = ifelse(data2$Smoking == "Yes",1,0))
levels(data2$Smoking) =  c("Yes","No")
data2 <- data2 %>% mutate(AlcoholDrinking = ifelse(data2$AlcoholDrinking == "Yes",1,0))
levels(data2$AlcoholDrinking) =  c("Yes","No")
data2 <- data2 %>% mutate(Stroke = ifelse(data2$Stroke == "Yes",1,0))
levels(data2$Stroke) =  c("Yes","No")
data2 <- data2 %>% mutate(DiffWalking = ifelse(data2$DiffWalking == "Yes",1,0))
levels(data2$DiffWalking) =  c("Yes","No")
data2 <- data2 %>% mutate(Asthma = ifelse(data2$Asthma == "Yes",1,0))
levels(data2$Asthma) =  c("Yes","No")
data2 <- data2 %>% mutate(KidneyDisease = ifelse(data2$KidneyDisease == "Yes",1,0))
levels(data2$KidneyDisease) =  c("Yes","No")
data2 <- data2 %>% mutate(SkinCancer = ifelse(data2$SkinCancer == "Yes",1,0))
levels(data2$SkinCancer) =  c("Yes","No")
data2 <- data2 %>% mutate(Sex = ifelse(data2$Sex == "Male",0,1))
levels(data2$SkinCancer) =  c("Male","Female")
data2 <- data2 %>% mutate(PhysicalActivity = ifelse(data2$PhysicalActivity == "No",1,0))
levels(data2$PhysicalActivity) =  c("No","Yes")


data3 <- data2[,-1]
corrr <- matrix(ncol=18,nrow=18)
for (i in 1:18) {
  for (j in 1:18) {
    corrr[i,j] <- cor(data3[,i],data3[,j])
  }
}
colnames(corrr) <- names(data3)
rownames(corrr) <- names(data3)
corrr <- round(corrr,2)
corrplot::corrplot(corrr)


palette = colorRampPalette(c("green", "white", "red"))(20)
heatmap(x = corrr, col = palette, symm = TRUE)



par(mar = c(4.4, 4.4, 1, 4), family = "Times") ## plotting area and font
cred <- c(.5, 0, 0, 0, 1, 1, 1) ## red
cgreen <- c(.5, 0, 1, 1, 1, 0, 0) ## green
cblue <- c(.5, 1, 1, 0, 0, 0, 1) ## blue
crange <- c(-1, 1) ## range

## colors in plot:
matrix_red <- approxfun(
  seq(crange[1], crange[2], length = length(cred)), cred)(corrr)
matrix_green <- approxfun(
  seq(crange[1], crange[2], length = length(cgreen)), cgreen)(corrr)
matrix_blue <- approxfun(
  seq(crange[1], crange[2], length = length(cblue)), cblue)(corrr)
plot_cols <- rgb(matrix_red, matrix_green, matrix_blue, alpha = .5)
crange_legend <- seq(crange[1], crange[2], length = 100)
legend_red <- approxfun(
  seq(crange[1], crange[2], length = length(cred)), cred
)(crange_legend)
legend_green <- approxfun(
  seq(crange[1], crange[2], length = length(cgreen)), cgreen
)(crange_legend)
legend_blue <- approxfun(
  seq(crange[1], crange[2], length = length(cblue)), cblue
)(crange_legend)
legend_cols <- rgb(legend_red, legend_green, legend_blue, alpha = .5)
plotrix::color2D.matplot(corrr,
                         cellcolors = plot_cols,
                         show.values = 2,
                         vcol = rgb(0, 0, 0),
                         vcex = .6,
                         axes = FALSE
)
axis(1,1:18-.5, 1:18, cex.axis = .8) ## draw x-axis
axis(2, 18:1 - .5, 1:18, cex.axis = .8, las = 2)+
  theme(axis.text.x = element_text(angle= 90, hjust = 1))## draw y-axis+library(tidyverse)





#####DOnt mind the below that........
library(dplyr)
library(caret)
library(lattice)
data <- read.csv("~/Downloads/Heart_Train.csv")
data <- as_tibble(data)
glimpse(data)

data <- data %>% mutate(Sex = ifelse(data$Sex == "Male",0,1))
levels(data$Sex) = c("Male","Female")

data <- data %>% mutate(HeartDisease = ifelse(data$HeartDisease == "Yes",1,0))
levels(data$HeartDisease) =  c("Yes","No")

data <- data %>% mutate(Smoking = ifelse(data$Smoking == "Yes",1,0))
levels(data$Smoking) =  c("Yes","No")

data <- data %>% mutate(AlcoholDrinking = ifelse(data$AlcoholDrinking == "Yes",1,0))
levels(data$AlcoholDrinking) =  c("Yes","No")

data <- data %>% mutate(SkinCancer = ifelse(data$SkinCancer == "Yes",1,0))
levels(data$SkinCancer) =  c("Yes","No")

data <- data %>% mutate(KidneyDisease = ifelse(data$KidneyDisease == "Yes",1,0))
levels(data$KidneyDisease) =  c("Yes","No")

data <- data %>% mutate(Asthma  = ifelse(data$Asthma  == "Yes",1,0))
levels(data$Asthma ) =  c("Yes","No")

data <- data %>% mutate(PhysicalActivity  = ifelse(data$PhysicalActivity  == "Yes",1,0))
levels(data$PhysicalActivity) =  c("Yes","No")

data <- data %>% mutate(Diabetic  = ifelse(data$Diabetic  == "Yes",1,0))
levels(data$Diabetic) =  c("Yes","No")

data <- data %>% mutate(DiffWalking  = ifelse(data$DiffWalking  == "Yes",1,0))
levels(data$DiffWalking) =  c("Yes","No")

data <- data %>% mutate(Stroke  = ifelse(data$Stroke  == "Yes",1,0))
levels(data$Stroke) =  c("Yes","No")
sdfsdfsdfsdfsdf
adc <- data[-c(1,3,7,8,11,12,15)]

adc[,1]
length(adc)
corrr <- matrix(ncol=19,nrow=19)
for (i in 1:19) {
  for (j in 1:19) {
  corrr[i,j] <- cor(data2[,i],data2[,j])
  }
}
colnames(corrr) <- names(data2)
rownames(corrr) <- names(data2)
corrr <- round(corrr,2)
#There diff methods to get corr plot
#1
corrplot::corrplot(corrr)


#2
palette = colorRampPalette(c("green", "white", "red"))(20)
heatmap(x = corrr, col = palette, symm = TRUE)







#3
par(mar = c(4.4, 4.4, 1, 4), family = "Times") ## plotting area and font
cred <- c(.5, 0, 0, 0, 1, 1, 1) ## red
cgreen <- c(.5, 0, 1, 1, 1, 0, 0) ## green
cblue <- c(.5, 1, 1, 0, 0, 0, 1) ## blue
crange <- c(-1, 1) ## range
colnames(corrr) <- colnames(data2)
rownames(corrr) <- colnames(data2)

## colors in plot:
matrix_red <- approxfun(
  seq(crange[1], crange[2], length = length(cred)), cred)(corrr)
matrix_green <- approxfun(
  seq(crange[1], crange[2], length = length(cgreen)), cgreen)(corrr)
matrix_blue <- approxfun(
  seq(crange[1], crange[2], length = length(cblue)), cblue)(corrr)
plot_cols <- rgb(matrix_red, matrix_green, matrix_blue, alpha = .5)
crange_legend <- seq(crange[1], crange[2], length = 100)
legend_red <- approxfun(
  seq(crange[1], crange[2], length = length(cred)), cred
)(crange_legend)
legend_green <- approxfun(
  seq(crange[1], crange[2], length = length(cgreen)), cgreen
)(crange_legend)
legend_blue <- approxfun(
  seq(crange[1], crange[2], length = length(cblue)), cblue
)(crange_legend)
legend_cols <- rgb(legend_red, legend_green, legend_blue, alpha = .5)
plotrix::color2D.matplot(corrr,
                cellcolors = plot_cols,
                show.values = 2,
                vcol = rgb(0, 0, 0),
                vcex = .6,
                axes = FALSE
  )
axis(1,1:18-.5, 1:18, cex.axis = .8) ## draw x-axis
axis(2, 18:1 - .5, 1:18, cex.axis = .8, las = 2)+
  theme(axis.text.x = element_text(angle= 90, hjust = 1))## draw y-axis+




asd <- glm(HeartDisease ~ Diabetic + Sex +SleepTime+ AlcoholDrinking + DiffWalking +Smoking,data= adc,
           family = "binomial")


#### PAvlo Try :)
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
# and no only "No"!aaa
