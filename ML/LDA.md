# Linear Discriminant Analysis: Dimensionality Reduction technique

#### Step 1

    train <- read.csv('train.csv')
    test <- read.csv('test.csv')
    View(train)

#### Step 2

##### sample 20% of index row numbers from the train set
    validation_index <- sample(1:nrow(train), size = 0.2 * nrow(train))

##### use the index to split the train set into validation test set and the validation train set
    validation_test_set <- train[validation_index, ]
    validation_train_set <- train[-validation_index, ]
    View(validation_train_set)

    library(MASS)
    source("https://bit.ly/lda_test")

##### Fitting the model

    lda_model = MASS::lda(position ~ age + overall + international_reputation + weak_foot +
        skill_moves + contract_valid_until + crossing + finishing +
        heading_accuracy + short_passing + volleys + dribbling +
        curve + fk_accuracy + long_passing + ball_control + acceleration +
        sprint_speed + agility + reactions + balance + shot_power +
        jumping + stamina + strength + long_shots + aggression +
        interceptions + positioning + vision + penalties + composure +
        marking + standing_tackle + sliding_tackle + gk_diving +
        gk_handling + gk_kicking + gk_positioning + gk_reflexes +
        log(value_numeric_pounds) + log(wage_numeric_pounds) + weight_pds +
        log(release_clause_num), validation_train_set)

### Step 3

##### Make predictions on the validation test set
    predictions_validation <- predict(lda_model, prior=lda_model$prior, newdata=validation_test_set)

##### make a confusion matrix
    confusion_matrix <- table(truth = validation_test_set$position, classified = predictions_validation$class)
    addmargins(confusion_matrix)

##### check classification accuracy
    mean(validation_test_set$position == predictions_validation$class)

### Step 4

##### Setting up k-fold cross-validation. Otherwise, prediction accuracy will be misleading, as predictions will be fitted on the same validation data set, overfitting issue.

    k <- 5

##### randomly shuffle the data and cut into 1:k seq
    train <- train[sample(nrow(train)), ]
    folds <- cut(1:nrow(train), breaks = k, labels = FALSE)

##### making accuracy vector
    accuracy_vector <- numeric(5)


    for (i in 1:k) {

    ##### segmenting data by folds
      test_indexes <- which(folds == i)
      test_fold <- train[test_indexes, ]
      train_folds <- train[-test_indexes, ]

    ##### fitting model on training data + fine tuning
      model <-  MASS::lda(position ~ age + overall + international_reputation + weak_foot +
        skill_moves + contract_valid_until + crossing + finishing +
        heading_accuracy + short_passing + volleys + dribbling +
        curve + fk_accuracy + long_passing + ball_control + acceleration +
        sprint_speed + agility + reactions + balance + shot_power +
        jumping + stamina + strength + long_shots + aggression +
        interceptions + positioning + vision + penalties + composure +
        marking + standing_tackle + sliding_tackle + gk_diving +
        gk_handling + gk_kicking + gk_positioning + gk_reflexes +
        log(value_numeric_pounds) + log(wage_numeric_pounds) + weight_pds +
        log(release_clause_num), train_folds)

    ##### evaluation on test data
      predicted <- predict(model,newdata=test_fold)

    #### calculating accuracy
      accuracy_vector[i]<- mean(test_fold$position == predicted$class)

    }


    mean(accuracy_vector)

    predictions_full <- predict(model, test)
