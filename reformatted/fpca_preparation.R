library(Splinets)
library(keras)
library(pbapply)
library(tictoc)
library(pracma)
library(RcppCNPy)

fashion_mnist <- dataset_fashion_mnist()

c(train_images, train_labels) %<-% fashion_mnist$train
c(test_images, test_labels) %<-% fashion_mnist$test
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
order <- 4
number_of_knots <- image_size*8
knots <- seq(from = 1, to = image_size^2, length.out = number_of_knots)


flipped_train_images <- array(0,dim = c(number_of_images, image_size, image_size))
flipped_test_images <- array(0, dim = c(length(test_labels), image_size, image_size))
for(i in 1:number_of_images){
  flipped_train_images[i,,] <- t(apply(train_images[i,,],2,rev))/255
}
for(i in 1:length(test_labels)){
  flipped_test_images[i,,] <- t(apply(test_images[i,,],2,rev))/255
}
train_vectors <- flipped_train_images
dim(train_vectors) <- c(number_of_images, image_size^2)
test_vectors <- flipped_test_images
dim(test_vectors) <- c(length(test_labels), image_size^2)

class_means <- array(dim = c(length(class_names), image_size^2))
tic("Calculating and projecting means")
for (class_index in 1:length(class_names)){
  class_means[class_index,] <- colMeans(train_vectors[train_labels == class_index -1, ])
}
class_mean_splines <- project(cbind(seq(image_size^2), t(class_means)), knots, order)
toc()
#To check if mean is similar.
#image(1:28, 1:28, matrix(class_means[3,], ncol = 28, nrow = 28, byrow = FALSE), col = gray((0:255/(255))), xaxt = "n", yaxt = "n")
#image(1:28, 1:28, matrix(evspline(class_mean_splines$sp, x = seq(28^2))[,10], ncol = 28, nrow = 28, byrow = FALSE), col = gray((0:255/(255))), xaxt = "n", yaxt = "n")

tic("Projecting the data onto Splines")
train_vector_splines <- project(cbind(seq(image_size^2), t(train_vectors)), knots, order)
toc()
test_vector_splines <- project(cbind(seq(image_size^2), t(test_vectors)), knots, order)

npySave("principalcoeffdata/train_labels.npy", train_labels)
npySave("principalcoeffdata/test_labels.npy", test_labels)

