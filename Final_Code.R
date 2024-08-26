dataset = read.csv("C:/Users/Pratik Pansare/Downloads/Data/diabetes.csv")
dataset

############################(b)#########################################
Data = dataset[, -which(names(dataset) == "Pregnancies")]
Data

Data$Glucose[Data$Glucose == 0]= NaN
Data$Insulin[Data$Insulin == 0]= NaN
Data$BMI[Data$BMI == 0]= NaN
Data$SkinThickness[Data$SkinThickness == 0]= NaN
Data$DiabetesPedigreeFunction[Data$DiabetesPedigreeFunction == 0]= NaN
Data$Age[Data$Age == 0]= NaN
Data$BloodPressure[Data$BloodPressure == 0]= NaN
summary(Data)

###################################(c)###########################################
diabetes1 = cbind(Data, Pregnancies = dataset$Pregnancies) 
diabetes1

summary(diabetes1$Glucose)
diabetes1$Glucose[is.na(diabetes1$Glucose)] = mean(diabetes1$Glucose, na.rm = TRUE)
par(mfrow=c(1,2))
qqnorm(diabetes1$Glucose, col = "Orange", main = "Glucose Data")
qqline(diabetes1$Glucose)
hist(diabetes1$Glucose, probability =  T,col = "light green", main = " Glucose ", ylab="Probability", 
     xlab="Values")
lines(density(diabetes1$Glucose), lwd = 3)
curve(dnorm(x,mean=mean(diabetes1$Glucose), 
            sd=sd(diabetes1$Glucose)), add=T, col="Red",lwd=3)

summary(diabetes1$Pregnancies)
diabetes1$Pregnancies[is.na(diabetes1$Pregnancies)] = mean(diabetes1$Pregnancies, na.rm = TRUE)
par(mfrow=c(1,2))
qqnorm(diabetes1$Pregnancies, col = "Orange", main = "Pregnancies Data")
qqline(diabetes1$Pregnancies)
hist(diabetes1$Pregnancies, probability =  T,col = "light green", main = " Pregnancies ", ylab="Probability", 
     xlab="Values")
lines(density(diabetes1$Pregnancies), lwd = 3)
curve(dnorm(x,mean=mean(diabetes1$Pregnancies), 
            sd=sd(diabetes1$Pregnancies,)), add=T, col="Red",lwd=3)



summary(diabetes1$BloodPressure)
diabetes1$BloodPressure[is.na(diabetes1$BloodPressure)] = mean(diabetes1$BloodPressure, na.rm = TRUE)
par(mfrow=c(1,2))
qqnorm(diabetes1$BloodPressure, col = "Orange", main = "BloodPressure Data")
qqline(diabetes1$BloodPressure)
hist(diabetes1$BloodPressure, probability =  T,ylim=c(0,0.040), col = "light green", main = " BloodPressure ", ylab="Probability", 
     xlab="Values")
lines(density(diabetes1$BloodPressure), lwd = 3)
curve(dnorm(x,mean=mean(diabetes1$BloodPressure), 
            sd=sd(diabetes1$BloodPressure)), add=T, col="Red",lwd=3)


summary(diabetes1$SkinThickness)
diabetes1$SkinThickness[is.na(diabetes1$SkinThickness)] = mean(diabetes1$SkinThickness, na.rm = TRUE)
par(mfrow=c(1,2))
qqnorm(diabetes1$SkinThickness, col = "Orange", main = "SkinThickness")
qqline(diabetes1$SkinThickness)
hist(diabetes1$SkinThickness, probability =  T,ylim=c(0,0.14),xlim=c(0,60), col = "light green", main = " SkinThickness ", ylab="Probability", 
     xlab="Values")
lines(density(diabetes1$SkinThickness), lwd = 3)
curve(dnorm(x,mean=mean(diabetes1$SkinThickness), 
            sd=sd(diabetes1$SkinThickness)), add=T, col="Red",lwd=3)

summary(diabetes1$Insulin)
diabetes1$Insulin[is.na(diabetes1$Insulin)] = mean(diabetes1$Insulin, na.rm = TRUE)
par(mfrow=c(1,2))
qqnorm(diabetes1$Insulin, col = "Orange", main = "Insulin")
qqline(diabetes1$Insulin)
hist(diabetes1$Insulin, probability =  T,ylim=c(0,0.012),xlim=c(0,800), col = "light green", main = " Insulin ", ylab="Probability", 
     xlab="Values")
lines(density(diabetes1$Insulin), lwd = 3)
curve(dnorm(x,mean=mean(diabetes1$Insulin), 
            sd=sd(diabetes1$Insulin)), add=T, col="Red",lwd=3)

summary(diabetes1$BMI)
diabetes1$BMI[is.na(diabetes1$BMI)] = mean(diabetes1$BMI, na.rm = TRUE)
par(mfrow=c(1,2))
qqnorm(diabetes1$BMI, col = "Orange", main = "BMI Data")
qqline(diabetes1$BMI)
hist(diabetes1$BMI, probability =  T, col = "light green", main = " BMI ", ylab="Probability", 
     xlab="Values")
lines(density(diabetes1$BMI), lwd = 3)
curve(dnorm(x,mean=mean(diabetes1$BMI), 
            sd=sd(diabetes1$BMI)), add=T, col="Red",lwd=3)

summary(diabetes1$DiabetesPedigreeFunction)
diabetes1$DiabetesPedigreeFunction[is.na(diabetes1$DiabetesPedigreeFunction)] = mean(diabetes1$DiabetesPedigreeFunction, na.rm = TRUE)
par(mfrow=c(1,2))
qqnorm(diabetes1$DiabetesPedigreeFunction, col = "Orange", main = "DiabetesPedigreeFunction Data")
qqline(diabetes1$DiabetesPedigreeFunction)
hist(diabetes1$DiabetesPedigreeFunction, probability =  T,ylim=c(0,2.0), col = "light green", main = " DiabetesPedigreeFunction ", ylab="Probability", 
     xlab="Values")
lines(density(diabetes1$DiabetesPedigreeFunction), lwd = 3)
curve(dnorm(x,mean=mean(diabetes1$DiabetesPedigreeFunction), 
            sd=sd(diabetes1$DiabetesPedigreeFunction)), add=T, col="Red",lwd=3)

summary(diabetes1$Age)
diabetes1$Age[is.na(diabetes1$Age)] = mean(diabetes1$Age, na.rm = TRUE)
par(mfrow=c(1,2))
qqnorm(diabetes1$Age, col = "Orange", main = "Age")
qqline(diabetes1$Age)
hist(diabetes1$Age, probability =  T, col = "light green", main = " Age ", ylab="Probability", 
     xlab="Values")
lines(density(diabetes1$Age), lwd = 3)
curve(dnorm(x,mean=mean(diabetes1$Age), 
            sd=sd(diabetes1$Age)), add=T, col="Red",lwd=3)
####################################(d)###########################################
#########t-test###################
t.test(SkinThickness ~ Outcome, data = diabetes1 , paired = FALSE)   ## SkinThickness
t.test(BloodPressure ~ Outcome,data = diabetes1 , paired = FALSE)   ## BloodPresure
t.test(BMI ~ Outcome,data = diabetes1 , paired = FALSE)   ## BMI

head(diabetes1$SkinThickness)
#### wilcox.test ##############################

wilcox.test(Pregnancies~Outcome, data = diabetes1 ,  alternative = "two.sided", paired = FALSE)
wilcox.test(Glucose ~ Outcome,data = diabetes1, alternative = "two.sided",paired = FALSE)
wilcox.test(Insulin~ Outcome, data = diabetes1,alternative = "two.sided")
wilcox.test(DiabetesPedigreeFunction~ Outcome,data = diabetes1, alternative = "two.sided")
wilcox.test(Age ~Outcome, data = diabetes1,alternative = "two.sided")

#####################boxplot##############################
install.packages("ggplot2")
library(ggplot2)

##par(mfrow = c(2,4))
boxplot(diabetes1$Pregnancies~diabetes1$Outcome,,main="Boxplot for Pregnancies",outpch=20,outbg="Limegreen",
        xlab="Outcome", ylab="Pregnancies", col="orange", border="brown")

boxplot(diabetes1$Glucose~diabetes1$Outcome,main="Boxplot for Glucose",outpch=20,outbg="Limegreen",
        xlab="Outcome", ylab="Glucose",col="orange",border="brown")

boxplot(diabetes1$SkinThickness~diabetes1$Outcome,main="Boxplot for SkinThickness",outpch=20,outbg="Limegreen",
        xlab="Outcome", ylab="SkinThickness",col="orange",border="brown")

boxplot(diabetes1$BloodPressure~diabetes1$Outcome,main="Boxplot for BloodPressure",outpch=20,outbg="Limegreen",
        xlab="Outcome",ylab="BloodPressure",col="orange",border="brown")

boxplot(diabetes1$Insulin~diabetes1$Outcome,  main="Boxplot for Insulin",outpch=20,outbg="Limegreen",
        xlab="Outcome",ylab="Insulin",col="orange",border="brown")

boxplot(diabetes1$BMI~diabetes1$Outcome,main="Boxplot for BMI",outpch=20,outbg="Limegreen",
        xlab="Outcome",ylab="BMI",col="orange",border="brown")

boxplot(diabetes1$DiabetesPedigreeFunction~diabetes1$Outcome,main="Boxplot for DiabetesPedigreeFunction",outpch=20,outbg="Limegreen",
        xlab="Outcome",ylab="DiabetesPedigreeFunction",col="orange",border="brown")

boxplot(diabetes1$Age~diabetes1$Outcome,main="Boxplot for Age",outpch=20,outbg="Limegreen",
        xlab="Outcome",ylab="Age",col="orange",border="brown")

dev.off()
########################(e)########################

install.packages("corrplot")
library(corrplot)
dev.off()
corrplot1 = cor(diabetes1)
corrplot1
plot(corrplot1, main = "sss")
pairs(corrplot1[,1:9], pch = 18)
corrplot(corrplot1, tl.col = "cyan4", bg = "White", tl.srt = 20, 
         title = "\n\n Correlation Plot \n",
         addCoef.col = "black", type = "full")
#############################(f)#######################################
###################logistic regression model###################
# Split the dataset into training and testing sets
set.seed(123)
split <- sample(nrow(diabetes1), floor(0.7 * nrow(diabetes1)))
train <- diabetes1[split, ]
test <- diabetes1[-split, ]

# Fit a logistic regression model to the training data
model <- glm(Outcome ~ ., data = train, family = binomial(link = "logit"))

# View the model summary
summary(model)

# Make predictions on the test set
pred <- predict(model, newdata = test, type = "response")

# Evaluate model performance
actual <- test$Outcome
pred <- ifelse(pred > 0.5, 1, 0)
accuracy <- sum(diag(table(actual, pred))) / sum(table(actual, pred))
print(paste0("Accuracy: ", round(accuracy,3)))
#################Linear regression model #############################
model_lm = lm(diabetes1$Outcome ~ ., data = diabetes1)
model_lm
summary(model_lm)
############################(g)########################################
diabetes_sub <- subset(diabetes1, select = -Glucose)
diabetes_sub
diabetes = cbind(diabetes_sub, Glucose = dataset$Glucose) 
diabetes
zero_rows <- which(diabetes$Glucose == 0)

# Print the row numbers and the corresponding values of Glucose
cat("Rows with Glucose value of 0:\n")
for (i in zero_rows) {
  cat("Row number:", i, "Glucose value:", diabetes$Glucose[i], "\n")
}
diabetes_sub1 <- diabetes[diabetes$Glucose != 0, ]
diabetes_sub2 <- diabetes[diabetes$Glucose == 0, ]
diabetes_sub2
install.packages("caret")
library(caret)

set.seed(123)
training <- diabetes_sub1
test <- diabetes[diabetes$Glucose == 0, ]

control <- trainControl(method="cv", number=5)

knn_model <- train(Glucose ~ Age, data=training, method="knn", trControl=control)
predicted_glucose <- predict(knn_model, newdata=diabetes_sub2)
predicted_glucose
####################################
