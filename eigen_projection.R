

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


for (i in 0:(length(class_names)-1)){
  images <- train_images[train_labels == i,,] / 255
  for (j in 1:(dim(images)[1])){ 
    #Puts the pictures in the upward orientation when plotted.
    images[j,,] <- t(apply(images[j,,],2,rev))
  }
  if(i == 0){
    train_images_by_class = list(images)
    train_vectors_by_class = list(matrix(images, dim(images)[1], prod(dim(images)[2:3])))
  }else{
  train_images_by_class[[i+1]] <- images
  train_vectors_by_class[[i+1]] <- matrix(images, dim(images)[1], prod(dim(images)[2:3]))
  }
}
image(1:28, 1:28, train_images_by_class[[2]][150,,], col = gray((0:255/(255))), xaxt = "n", yaxt = "n")

knots <- seq(from = -10, to = image_size^2+10, length.out = image_size*4)
order <- 4
#examine_new_sample(knots, order, train_images, class_names)

data_values = cbind(seq(image_size^2), t(train_vectors_by_class[[1]]))

class_proj <- project(data_values, knots, 4)
sigma <- cov(class_proj$coeff)
spect <- eigen(Sigma, symmetric = T)

plot(spect$values, type ='l',col='blue', lwd=4 )

eigen_class_sp <- lincomb(class_proj$basis, t(spect$vectors))

C <- class_proj$coeff %*% spect$vectors
eigenfunctions5=subsample(eigen_class_sp,1:5)
eigenfunctions10=subsample(eigen_class_sp,1:10)
eigenfunctions20=subsample(eigen_class_sp,1:20)

eigen_proj5 <- lincomb(eigenfunctions5, C[1, 1:5, drop = F])
eigen_proj10 <- lincomb(eigenfunctions10, C[1, 1:10, drop = F])
eigen_proj20 <- lincomb(eigenfunctions20, C[1, 1:20, drop = F])


par(mfrow = c(4,1))
plot(data_values[,1], data_values[,2], type = "l", xaxt = "n", yaxt = "n", ylab = "")
plot(eigen_proj5, type = "l")
plot(eigen_proj10, type = "l")
plot(eigen_proj20, type = "l")



image(1:28, 1:28, images[1,,] / 255, col = gray((0:255/(255))), xaxt = "n", yaxt = "n", main = name)


