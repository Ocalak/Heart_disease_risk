library(tidyverse)
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

adc <- data[-c(1,3,7,8,11,12,15)]

adc[,1]
length(adc)
corrr <- matrix(1:144,ncol = 12)
for (i in 1:12) {
  for (j in 1:12) {
  corrr[i,j] <- cor(adc[,i],adc[,j])
  }
}
corrplot::corrplot(corrr)
corrr <- round(corrr,2)


palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = corrr, col = palette, symm = TRUE)








par(mar = c(4.4, 4.4, 1, 4), family = "Times") ## plotting area and font
cred <- c(.5, 0, 0, 0, 1, 1, 1) ## red
cgreen <- c(.5, 0, 1, 1, 1, 0, 0) ## green
cblue <- c(.5, 1, 1, 0, 0, 0, 1) ## blue
crange <- c(-1, 1) ## range
colnames(corrr) <- colnames(adc)
rownames(corrr) <- colnames(adc)

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
axis(1,1:12-.5, 1:12, cex.axis = .8) ## draw x-axis
axis(2, 12:1 - .5, 1:12, cex.axis = .8, las = 2) ## draw y-axis

