#Short description:

# I use a variety of open source frameworks and educational resources available #online to build a Convolutional Neural Network (CNN) for image classification #problem.
# The deep neural network architecture model was created to identify the differences
# between pictures of fashion items in two formats. Format 1 is when a fashion item #is worn by a human model. In Format 2 a fashion item is the only thing in the  #picture, no #human body part sare present in the picture.

# It was trained on a small sample of 3080 pictures (equal split), and validation #sample containing 500 pictures for each format. After passing the ML algorithm for 10 times in mini batch mode samples, the model shows #99,2+% accuracy on training #samples and ~97% average on validation sample.

# The model could achieve higher validation accuracy by adding new features(e.g. #grey scale; data augmentation in training sample), changes in the available data #(e.g. higher sample size) and optimization (parameter tuning).


### CNN

# Installing required packages and frameworks: keras with Tensorflow end


# Package to connect R and Python.
if (!require('reticulate')) install.packages('reticulate')
library(reticulate)

# Package to install latest versions of other packages directly from github
if (!require('devtools')) install.packages('devtools')
library(devtools)

# Package for image processing
if (!require('BiocManager')) install.packages('BiocManager')
library(BiocManager)
BiocManager::install('EBImage')
library(EBImage)

# Package for deep neural architecture models, integrating artificial neural networks with deep learning.
if(!require('keras')) devtools::install_github("rstudio/keras")
library(keras)


use_virtualenv("myenv")
install_keras(method="virtualenv", envname="myenv")
Sys.setenv("CUDA_VISIBLE_DEVICES" = -1)


### Data prep

# Specifying existing classes
class_list <- c("Model", "Product")

# Specifying the number of classes
output_n <- length(class_list)

# Specifying image size for rescaling
img_width <- 150
img_height <- 150
target_size <- c(img_width, img_height)

# Pictures contain all colours and are of high quality, thus RGB suffices here.
channels <- RGB <- 3


## Create a bunch of folders in a systemic and simple way. Also, rename and select pictures
base_dir <- "C:/Users/ddomi_000/Desktop/Otrium/SVM model/pictures_models_and_products"
dir.create(base_dir)
train_dir <- file.path(base_dir, "train")
dir.create(train_dir)
validation_dir <- file.path(base_dir, "validation")
dir.create(validation_dir)
test_dir <- file.path(base_dir, "test")
dir.create(test_dir)

# Train folder
train_model_dir <- file.path(train_dir, "model")
dir.create(train_model_dir)
train_product_dir <- file.path(train_dir, "product")
dir.create(train_product_dir)

# Validation folder
validation_model_dir <- file.path(validation_dir, "model")
dir.create(validation_model_dir)
validation_product_dir <- file.path(validation_dir, "product")
dir.create(validation_product_dir)

# Test folder
test_model_dir <- file.path(test_dir, "model")
dir.create(test_model_dir)
test_product_dir <- file.path(test_dir, "product")
dir.create(test_product_dir)


# 2531 total number of pictures: model.
fnames <- paste0("model (", 1:1530, ").jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(train_model_dir))

fnames <- paste0("model (", 1531:2030, ").jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(validation_model_dir))

fnames <- paste0("model (", 2031:2531, ").jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(test_model_dir))

# 2799 total number of pictures: product.
fnames <- paste0("product (", 1:1530, ").jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(train_product_dir))

fnames <- paste0("product (", 1531:2030, ").jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(validation_product_dir))

fnames <- paste0("product (", 2031:2527, ").jpg")
file.copy(file.path(original_dataset_dir, fnames),
          file.path(test_product_dir))


# Data processing into arrays using image processing function in Keras.
# Converts a dataset of images into a matrix in .CSV file.

train_image_files_path <- "C:/Users/ddomi_000/Desktop/Otrium/SVM model/pictures_models_and_products/train"

valid_image_files_path <- "C:/Users/ddomi_000/Desktop/Otrium/SVM model/pictures_models_and_products/validation"

test_image_files_path <- "C:/Users/ddomi_000/Desktop/Otrium/SVM model/pictures_models_and_products/test"

train_data_gen = image_data_generator(
  rescale = 1/255
)


valid_data_gen = image_data_generator(
  rescale = 1/255
)

test_data_gen = image_data_generator(
  rescale = 1/255
)

train_image_array_gen <- flow_images_from_directory(
  train_image_files_path,                 
  train_data_gen,              
  target_size = target_size,
  classes = class_list,
  class_mode = "categorical",
  seed = 42
)

valid_image_array_gen <- flow_images_from_directory(
  valid_image_files_path,
  valid_data_gen,
  target_size = target_size,
  classes = class_list,
  class_mode = "categorical",
  seed = 42
)

test_image_array_gen <- flow_images_from_directory(
  test_image_files_path,                 
  test_data_gen,              
  target_size = target_size,
  classes = class_list,
  class_mode = "categorical",
  seed = 42
)



cat("Number of images per class:")
table(factor(train_image_array_gen$classes))
cat("\nClass label vs index mapping:\n")
train_image_array_gen$class_indices
class_classes_indices <- train_image_array_gen$class_indices
save(class_classes_indices, file = "/class_classes_indices.RData")

# Finishing touches, identifying number of samples and specifying hyperparameters
train_samples <- train_image_array_gen$n
valid_samples <- valid_image_array_gen$n
test_samples <- test_image_array_gen$n

batch_size <- 32
epochs <- 10

### Model

# Using keras and TensorFlow end
model <- keras_model_sequential()

# Adding layers 6 layers
model %>%
  layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", input_shape = c(img_width, img_height, channels)) %>%
  layer_activation("relu") %>%

  layer_conv_2d(filter = 16, kernel_size = c(3,3), padding = "same") %>%
  layer_activation_leaky_relu(0.5) %>%
  layer_batch_normalization() %>%

  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%

  # Flatten max filtered output into feature vector
  # and feed into dense layer
  layer_flatten() %>%
  layer_dense(100) %>%
  layer_activation("relu") %>%
  layer_dropout(0.5) %>%

  # Outputs from dense layer are projected onto output layer
  layer_dense(output_n) %>%
  layer_activation("softmax")

# Compiling model
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_rmsprop(lr = 0.0001, decay = 1e-6),
  metrics = "accuracy"
)



# Fitting the model, visualizing results and evaluating

hist <- model %>% fit_generator(
  train_image_array_gen,
  steps_per_epoch = as.integer(train_samples / batch_size),
  epochs = epochs,
  validation_data = valid_image_array_gen,
  validation_steps = as.integer(valid_samples / batch_size),
  verbose = 2,
  callbacks = list(
    callback_model_checkpoint("C:/Users/ddomi_000/Desktop/Otrium/SVM model/class_checkpoints.h5", save_best_only = TRUE),
    callback_tensorboard(log_dir = "C:/Users/ddomi_000/Desktop/Otrium/SVM model/logs")
  )
)

plot(hist)


# Evaluation & Prediction - train data
model %>% evaluate_generator(test_image_array_gen, steps=test_samples)
model %>% predict_generator(test_image_array_gen, steps=test_samples)
