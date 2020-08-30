# Principal Compenent Analysis: Dimensionality reduction technique

    train <- read.csv('train.csv')

    test <- read.csv('test.csv')

    if(!require(caret)){install.packages('caret')}

    train <- train[,-nearZeroVar(train)]


##### Full logistic reg

    full_model <- glm(factor(X1) ~ ., data = train, family = 'binomial')

##### make predictions

    predictions_full <- predict(full_model, newdata = test, type = 'response')

##### Rescore predictions

    full_predicted_logistic <- ifelse(predictions_full <= .5, 8, 9)

##### Fitting a PCA model on the predictions

    pca_model_train <- prcomp(train[,-1], scale. = T)

##### making a scree plot to inspect the explained variance of your components

    screeplot(pca_model_train, type = 'l',ylim=c(-1,40))

##### Visualizing to inspect explained variance

    rcompanion::plotNormalHistogram(pca_model_train$x[,1])
    rcompanion::plotNormalHistogram(pca_model_train$x[,292])

##### More visualizing to inspect explained variance

    x <- cumsum(pca_model_train$sdev^2 / sum(pca_model_train$sdev^2))
    var.expl <- data.frame(var = x, pc = 1:length(x))

    plot(cumsum(pca_model_train$sdev^2 / sum(pca_model_train$sdev^2)), type="b", ylab = 'Variance', xlab = 'Components')
    abline(a=0.5,b=127)


### Deciding how many dimensions to include based on visualizations and statistical theory. Training and testing.

    pca_train <- data.frame(X1 = train[,1], pca_model_train$x)[,1:131]

##### We want more variance to be explained, approximately 95%, thus we pick an optimal number of PCs.

    pca_test <- data.frame(predict(pca_model_train, test[-1])[,1:131])


##### fit linear model

    logistic_model <- glm(factor(X1) ~ ., data = pca_train, family = 'binomial')

##### make predictions with model

    predictions_logistic <- predict(logistic_model, newdata = pca_test, type = 'response')

##### rescore classes

    classes_predicted <- ifelse(predictions_logistic <= .5, 8, 9)
