library(keras)
library(Splinets)

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