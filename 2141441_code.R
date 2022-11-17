install.packages("bnlearn")
install.packages("snow")
library('snow')
library(bnlearn)
library(readxl)
library(rlang)

x=heartfailure #Import data set heart failure

class(x)


colnames(x)
xx= data.frame(x)
sd(x$anaemia)
sd(x$diabetes)
mean(x$anaemia)
mean(x$diabetes)
median(x$anaemia)
median(x$diabetes)
hist(x$anaemia)
hist(x$diabetes)
hist(x$age)
barplot(x$anaemia)
boxplot(x$diabetes)

X=x$age
Y=x$anaemia


lin_model <- lm(Y~X)
coef(lin_model)
c <- 0.208143546
m <- 0.003670562 
y_cap <- c + (m*X)
length(y_cap)

plot(Y~X,
     main='Regression for Age and Anemia',
     xlab='Age',ylab='Anemia', col=3)

lines(X,y_cap , col = 5 , lwd = 3)


Z=x$diabetes


lin_model <- lm(Z~X)
coef(lin_model)
c <- 0.673300159
m <- -0.004195687 
y_cap <- c + (m*X)
length(y_cap)

plot(Z~X,
     main='Regression for Age and Diabetes',
     xlab='Age',ylab='Diabetes', col=3)

lines(X,y_cap , col = 5 , lwd = 3)


install.packages("Amelia")
library(Amelia)
missmap(x, main = "Missing values vs observed")
data <- subset(x,select=c(2,3,5,6,7,8,10,12))
zz=data$anaemia
zz[is.na(zz)] <- mean(zz,na.rm=T)

is.factor(data$sex)
is.factor(data$high_blood_pressure)

contrasts(data$sex)
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL
train <- data[1:150,]
test <- data[150:220,]
model <- glm(sex ~.,family=binomial(link='logit'),data=train)
summary(model)
anova(model, test="Chisq")
t= data$anaemia
install.packages("pscl")
library(pscl)
pR2(model)


fitted.results <- predict(model,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$sex)
print(paste('Accuracy',1-misClasificError))




install.packages(ROCR)
library(ROCR)
p <- predict(model)
d=test$anemia
pp=p[1:70]

pr <- prediction(pp,d)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

install.packages("ISLR")
library(ISLR)
names(x)
summary(x)
head(x)


install.packages("corrplot")
library(corrplot)
correlations <- cor(x[,1:8])
corrplot(correlations, method="circle")

pairs(x, col=x$sex)
install.packages("caret")
library(caret)
x <- x[,1:8]
y <- x[,9]
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)
str(x)
head(x)
intrain <- createDataPartition(y = x$anaemia, p= 0.7, list = FALSE)
training <- x[intrain,]
testing <- x[-intrain,]
dim(training); 
dim(testing);
anyNA(x)
summary(x)
training[["anaemia"]] = factor(training[["anaemia"]])
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
install.packages("kernlab")
library(kernlab)
svmm <- train(anaemia ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svmm

test_prediction <- predict(svmm, newdata = testing)
test_prediction

confusionMatrix(table(test_prediction, testing$anaemia))


gridd <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
svmm_Grid <- train(anaemia ~., data = training, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = gridd,
                         tuneLength = 10)
svmm_Grid
plot(svmm_Grid)

test_prediction_grid <- predict(svmm_Grid, newdata = testing)
test_prediction_grid


confusionMatrix(table(test_prediction_grid, testing$anaemia))
