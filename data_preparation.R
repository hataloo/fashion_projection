library(keras)
library(Splinets)
source("data_preparation_functions.R")

fashion_mnist <- dataset_fashion_mnist()

c(train_images, train_labels) %<-% fashion_mnist$train

class_names = c('T-shirt/top',
                'Trouser',
                'Pullover',
                'Dress',
                'Coat', 
                'Sandal',
                'Shirt',
                'Sneaker',
                'Bag',
                'Ankle boot')

number_of_images <- dim(train_labels)
image_size <- dim(train_images)[2]

train_images_by_class <- separate_data_by_class(train_images, train_labels, length(class_names))
train_vectors_by_class <- class_images_into_vectors(train_images_by_class)

class_image_means <- list()
class_vector_means <- list()

for (class_index in 1:length(class_names)){
  class_image_means[[class_index]] <- apply(train_images_by_class[[class_index]], c(2,3), mean)
  class_vector_means[[class_index]] <- apply(train_vectors_by_class[[class_index]], 2, mean)
}
class_vector_means_mat <- matrix(nrow = 10, ncol = image_size^2)
for(class_index in 1:length(class_vector_means)){
  class_vector_means_mat[class_index,] <- class_vector_means[[class_index]]
}


train_images_by_class_centered <- center_images(train_images_by_class, class_image_means)
train_vectors_by_class_centered <- center_vectors(train_vectors_by_class, class_vector_means)

