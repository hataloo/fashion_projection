

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

train_images_by_class = list()
train_vectors_by_class = list()
for (i in 0:(length(class_names)-1)){
  images <- train_images[train_labels == i,,] / 255
  for (j in 1:(dim(images)[1])){ 
    #Puts the pictures in the upward orientation when plotted.
    images[j,,] <- t(apply(images[j,,],2,rev))
  }
  train_images_by_class[[i+1]] <- images
  train_vectors_by_class[[i+1]] <- matrix(images, dim(images)[1], prod(dim(images)[2:3]))
}

image(1:28, 1:28, train_images_by_class[[2]][150,,], col = gray((0:255/(255))), xaxt = "n", yaxt = "n")

knots <- seq(from = -10, to = image_size^2+10, length.out = image_size*4)
order <- 4
#examine_new_sample(knots, order, train_images, class_names)
class_index <- match("Bag", class_names) #Gets index of a particular class.
sample_size <- 500

data_values = cbind(seq(image_size^2), t(train_vectors_by_class[[class_index]][1:min(sample_size,number_of_images),]))

class_proj <- project(data_values, knots, 4)
sigma <- cov(class_proj$coeff)
spect <- eigen(sigma, symmetric = T)

plot(spect$values, type ='l',col='blue', lwd=4 )

eigen_class_sp <- lincomb(class_proj$basis, t(spect$vectors))


C <- class_proj$coeff %*% spect$vectors
eigen_length <- c(3,10,25,60)
eigenfunctions <- list()
for (i in 1:length(eigen_length)){
  eigenfunctions[[i]] <- subsample(eigen_class_sp,1:eigen_length[i])
}

sample_index <- 10
eigen_projections <- list()
for (i in 1:length(eigen_length)){
  eigen_projections[[i]] <- lincomb(eigenfunctions[[i]], C[sample_index, 1:eigen_length[i], drop = F])
}


par(mfrow = c(length(eigen_length)+1,1))
plot(data_values[,1], data_values[,sample_index+1], type = "l", xaxt = "n", yaxt = "n", ylab = "")
for (i in 1:length(eigen_length)){ plot(eigen_projections[[i]], type = "l")}

#Plots a sample of the original data along with the projection onto
#eigenbases with different number of eigenvectors.


#image(1:28, 1:28, images[1,,] / 255, col = gray((0:255/(255))), xaxt = "n", yaxt = "n", main = name)


