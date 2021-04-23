library(keras)
library(Splinets)
#source("data_preparation_functions.R")

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

class_index <- 5

for(i in 1:total_number_of_images){
  flipped_train_images[i,,] <- t(apply(train_images[i,,],2,rev))
}

x_class <- flipped_train_images[train_labels == class_index-1,,]/255
dim(x_class) <- c(dim(x_class)[1], image_size^2)

x_mean <- array(0, dim = image_size^2)
for (i in 1:image_size^2){x_mean[i] = mean(x_class[,i])}
#image(1:28, 1:28, matrix(x_mean, ncol = 28, nrow = 28, byrow = TRUE), col = gray((0:255/(255))), xaxt = "n", yaxt = "n")

x_class_centered <- array(dim = dim(x_class))
for (i in 1:(dim(x_class_centered)[1])){x_class_centered[i,] <- x_class[i,] - x_mean}
x_cov <- cov(x_class_centered)
x_eigen <- eigen(x_cov)

for(i in 1:3){
image(1:28, 1:28, matrix(x_eigen$vectors[,i], ncol = 28, nrow = 28, byrow = FALSE), col = gray((0:255/(255))), xaxt = "n", yaxt = "n")
}


x_eigen_coeffs <- x_class_centered %*% x_eigen$vectors

x_eigen_approx <- array(0, dim = image_size^2)
sample_index <- 60
number_of_comps <- 10
for(i in 1:number_of_comps){x_eigen_approx <- x_eigen_approx + x_eigen_coeffs[sample_index,i]*x_eigen$vectors[,i]}
image(1:28, 1:28, matrix(x_eigen_approx, ncol = 28, nrow = 28, byrow = FALSE), col = gray((0:255/(255))), xaxt = "n", yaxt = "n")
#image(1:28, 1:28, matrix(x_eigen_approx + x_mean, ncol = 28, nrow = 28, byrow = FALSE), col = gray((0:255/(255))), xaxt = "n", yaxt = "n")
image(1:28, 1:28, matrix(x_class_centered[sample_index,], ncol = 28, nrow = 28, byrow = FALSE), col = gray((0:255/(255))), xaxt = "n", yaxt = "n")



order <- 4
number_of_knots <- image_size*4
knots <- seq(from = 1, to = image_size^2, length.out = number_of_knots)

x_spline <- project(cbind(seq(image_size^2), t(x_class)), knots, order)
x_spline_mean <- project(cbind(seq(image_size^2), x_mean), knots, order)

x_spline_cov <- cov(x_spline$coeff - x_spline_mean$coeff[1,])
x_spline_sigma <- eigen(x_spline_cov)

x_spline_sp <- lincomb(x_spline$basis, t(x_spline_sigma$vectors))

x_spline_eval <- evspline(x_spline_sp, sID = 1:5, seq(1,28^2,1)) 
image(1:28, 1:28, matrix(x_spline_eval[,2], ncol = 28, nrow = 28, byrow = FALSE), col = gray((0:255/(255))), xaxt = "n", yaxt = "n")

