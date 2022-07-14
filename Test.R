data <- read_csv("~/Downloads/Heart_Train.csv")
data <- as.tibble(data)
data <- data %>% mutate(Sex = ifelse(data$Sex == "Male",0,1))
data <- data %>% mutate(HeartDisease = ifelse(data$HeartDisease == "Yes",1,0))
chisq.test(data$Sex,data$HeartDisease)
plot(data$Sex,type = "")