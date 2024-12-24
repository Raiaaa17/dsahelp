setwd("~/DSARAY")
set.seed(310)
library(class)

data <- read.csv("Patient Satisfaction.csv")
head(data)

names(data)[4] <- "Surgical"
head(data)

surgical_table <- table(data$Surgical)
surgical_table

surgical_table[2]/sum(surgical_table)
# 1 
# 0.56
# The percentage of patients that had surgery is 56%


hist(data$Satisfaction, probability = TRUE, 
     main = "Histogram of Satisfaction with Normal Density Curve",
     xlab = "Number of Satisfaction", col = "lightblue", border = "black")

x_values <- seq(min(data$Satisfaction), max(data$Satisfaction), length = 100)
y_values <- dnorm(x_values, mean = mean(data$Satisfaction), sd = sd(data$Satisfaction))
lines(x_values, y_values, col = "red", lwd = 2)

qqnorm(data$Satisfaction,  pch = 20)
qqline(data$Satisfaction, col = "red")

# From the qqline it can be seen the observed value is close to predicted value
# for almost every data point. Hence, it can be argued that the data follow
# the normal distribution

boxp <- boxplot(data$Satisfaction ~ data$Surgical)
boxp$out
boxp$stats

# There are no outliers found from the plotted data for both patients who had and
# don't had surgery. The median for patients who had surgery is 75, and for patients
# who don't had surgery is 64.5. The distribution for both surgical category 
# are quite symmetric with patients who don't had surgery show more range.
# Since most of the range of these two boxplots overlap, the difference can be
# be argued not significant


# 6. (6 points) Write code to create a scatter plot of satisfaction score against the age of patients
# for which the points are classied by the surgical status: points of patients had surgical is
# in red color and points of patients with no surgical is in blue color. Give your comments.
# 


plot(data$Age, data$Satisfaction)
points(data$Satisfaction[data$Surgical==1] ~ data$Age[data$Surgical==1], 
       col = "red", pch = 20)
points(data$Satisfaction[data$Surgical==0]~ data$Age[data$Surgical==0], 
       col = "blue", pch = 20)


data <- read.csv("Patient Satisfaction.csv")
head(data)
names(data)[4] <- "Surgical"
head(data)
data$Surgical <- as.factor(data$Surgical)
str(data)
attach(data)
M <- lm(Satisfaction ~ Age + Severity + Anxiety + Surgical)
summary(M)

# p values of the regressor that are not significant at signicance level 0.1. 
# are the p values of
# anxiety = 0.4058 and
# Surgical1 = 0.5968

predict(M, newdata=data.frame(Age = 35, Severity = 45, Anxiety = 2.5, Surgical = '0'))
#        1 
# 82.19378 
predict(M, newdata=data.frame(Age = 60, Severity = 40, Anxiety = 3, Surgical = '1'))
#        1 
# 58.83136 


### part 3

data <- read.csv("Patient Satisfaction.csv")
data$S <- ifelse(data$Satisfaction > 70, "Good", "Bad")
data$Surgical <- as.factor(data$Surgical)
str(data)
head(data)

data.X <- data.frame(Age=scale(data$Age), Severity=scale(data$Severity), Anxiety=scale(data$Anxiety))
head(data.X)
set.seed(310)

X <- data.X
Y <- data$S

n <- dim(data)[1]
index <- sample(1:n, 15)

training.X <- X[index, ]
test.X <- X[-index, ]
training.Y <- Y[index]
test.Y <- Y[-index]

res <- data.frame()
for (k in c(3,5,7,9,11)) {
  knn.pred <- knn(training.X, test.X, training.Y, k = k) 
  
  confusion.matrix <- table(test.Y, knn.pred)
  accuracy <- sum(diag(confusion.matrix))/sum(confusion.matrix)
  res <- rbind(res, data.frame(k, accuracy))
  print(k)
  print(confusion.matrix)
  print(accuracy)
}

res
# k accuracy
# 3      0.8
# 5      0.8
# 7      0.8
# 9      0.7
# 11     0.7

plot(res$k, res$accuracy)
# using accuracy as the criterion, the best k found are k=3 or k=5 or k=7.
# Since three of them have the same result of accuracy of 80%

# A: Age = 35, Severity = 45, Anxiety = 2.5 , and had no surgical.

Age <- (35 - mean(data$Age))/sd(data$Age)
Severity <- (42 - mean(data$Severity))/sd(data$Severity)
Anxiety <- (2.5 - mean(data$Anxiety))/sd(data$Anxiety)
Surgical <- as.factor(0)

for (k in c(3,5,7)) {
  knn.pred <- knn(training.X, test.X, training.Y, k = k) 
  predict(knn.pred, newdata=data.frame(Age = Age, Severity = Severity, Anxiety = Anxiety, Surgical = Surgical))
}



cost <- 100000
salary <- 50000/12
saved <- 30000
month <- 0
rate <- 0.05
portion <- 0.2


function.year <- function(salary, cost, saved, rate, portion) {
  while(saved < cost) {
    month = month +1
    saved = salary * portion + saved 
    if (month %% 12 == 0) {
      salary = salary*(1+rate)
    }
  }
  return(month/12)
}

function.year(salary, cost, saved, rate, interest)
# 6.17 year
