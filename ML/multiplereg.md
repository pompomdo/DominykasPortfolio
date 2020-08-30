# Building up a  multiple regression model for cancer risk based on health data.
# Combining statistics theory to increase machine learning model efficiency
# Fine tuning based on statistical visualizations from car package to increase model accuracy.

train <- read.csv("train.csv")
test <- read.csv("test_set.csv")

if(!require(corrplot)) install.packages("corrplot") # install corrplot if not yet installed

M <- cor(Filter(is.numeric, train))
# Not including any non numeric values
corrplot(M, method = "circle")
# Visualising correlation matrix

# Basic model
mod0 <- lm(target ~ . - id, data = train)
# regression on target by all variables
summary(mod0)
# Visualizing outlying variables
car::vif(mod0)

# Model selection

modglm <- glm(target ~ cp+exang+ca+thal+thalach, data = train)
summary(modglm)
car::vif(modglm)
# Generalized linear model, logistic regression, performs better than linear types.

xmod11 <- glm(target~thal+cp+ca+thalach+exang+age+ADD+sex,data=data)
summary(xmod11)
car::vif(xmod11)
#Log regression excludes too many variables that positively contribute to model accuracy

# Results
predict_test <- predict(xmod11, newdata = test, type = "response")

# make predictions on the test data
predict_test <- ifelse(predict_test >= 0.5, 1, 0)

# recode probabilities above 0.5 as 1 and 0 otherwise
