heart <- read.csv("heart1.csv", header = F, sep = ",")
library(e1071)



heart$V1 <- cut(heart$V1, c(-1, -0.5, 0, 0.5, 1))
heart$V4 <- cut(heart$V4, c(-1, -0.5, 0, 0.5, 1))
heart$V5 <- cut(heart$V5, c(-1, -0.5, 0, 0.5, 1))
heart$V8 <- cut(heart$V8, c(-1, -0.5, 0, 0.5, 1))
heart$V10 <- cut(heart$V10, c(-1, -0.5, 0, 0.5, 1))


heart$V2 <- as.factor(heart$V2)
heart$V3 <- as.factor(heart$V3)
heart$V6 <- as.factor(heart$V6)
heart$V7 <- as.factor(heart$V7)
heart$V9 <- as.factor(heart$V9)
heart$V11 <- as.factor(heart$V11)
heart$V12 <- as.factor(heart$V12)
heart$V13 <- as.factor(heart$V13)
heart$V14 <- as.factor(heart$V14)


model <- naiveBayes(V14~., data = heart)

preee <- predict(model, heart)

library(PRROC)

PRR <- roc.curve(preee, heart$V14, curve = TRUE)
plot(PRR)

library(MLmetrics)
heartAccuracy <- Accuracy(preee, heart$V14)

model_svm <- svm(heart$V14~ ., data = heart) 
preee_svm <- predict(model_svm, heart)

PRR_svm <- roc.curve(preee_svm, heart$V14, curve = TRUE)
plot(PRR_svm)

c_svm <- Accuracy(preee_svm, heart$V14)



