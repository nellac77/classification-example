# Logistic Regression Example - Classification Template

## The simple linear regression and the sigmoid function together yield the 
## logistic regression equation. This template serves as a guide for future 
## logistic regression models.

## Step 1 - Pre-process data
dataset <- read.csv('Social_Network_Ads.csv')
dataset <- dataset[,3:5]

### Splitting the dataset into Training and Test sets
# install.packages("caTools")
library(caTools)
set.seed(123)
split <- sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

### Feature scaling
training_set[,1:2] <- scale(training_set[,1:2])
test_set[,1:2] <- scale(test_set[,1:2])

## Step 2 - Fitting a logistic regression to the Training set
classifier <- glm(formula = Purchased ~ ., # Purchased is the dependent var, the comma implies take all independent vars
                  family = binomial, # logistic regression models are binomial
                  data = training_set)

## Step 3 - Prediciting the Test set results
### Summary of what we are doing: we are prediciting the test_set observations 
### using our classifier (logistic regression classifier)
prob_pred <- predict(classifier, type = 'response', newdata = test_set[-3]) # vector of the predicted probabilities of test_set obsv's by the clasifier 
y_pred <- ifelse(prob_pred > 0.5, 1, 0)

## Step 4 - Make the confusion matrix to evaluate the predictions
cm <- table(test_set[,3],y_pred)
accuracy <- (cm[2,2] + cm[1,1]) / sum(cm)
misclass_rate <- (cm[1,2] + cm[2,1]) / sum(cm)

## Step 5 - Visualizing the training set results
# install.packages("ElemStatLearn")
library(ElemStatLearn)
set <- training_set
X1 <- seq(min(set[,1]) - 1, max(set[,1]) + 1, by = 0.01)
X2 <- seq(min(set[,2]) - 1, max(set[,2]) + 1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c('Age', 'EstimatedSalary')
prob_set <- predict(classifier, type = 'response', newdata = grid_set)
y_grid <- ifelse(prob_set > 0.5,1,0)
plot(set[,-3],
     main = 'Logistic Regression (Training Set)',
     xlab = 'Age', ylab = 'Esitmated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[,3] == 1, 'green4', 'red3'))








