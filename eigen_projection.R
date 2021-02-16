

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

image(1:28, 1:28, train_images_by_class[[2]][175,,], col = gray((0:255/(255))), xaxt = "n", yaxt = "n")

knots <- seq(from = -10, to = image_size^2+10, length.out = image_size*4)
order <- 4
#examine_new_sample(knots, order, train_images, class_names)
class_index <- match("Bag", class_names) #Gets index of a particular class.
sample_size <- 6000
eigen_projection_size <- 500 #Number of samples used for eigen-decomp.

class_proj <- list()
sigma <- list()
spect <- list()
eigen_class_sp <- list()
eigen_coeffs <- list()
data_values <- list() 

totStart.time <- Sys.time()
for (class_index in 1:length(class_names)){
  data_values[[class_index]] = cbind(seq(image_size^2), t(train_vectors_by_class[[class_index]][1:min(sample_size,number_of_images),]))
  cat(sprintf("Currently projecting: %s, total elapsed time: %f\n", class_names[class_index], Sys.time()-totStart.time))
  iterStart.time <- Sys.time()
  
  
  class_proj[[class_index]] <- project(data_values[[class_index]], knots, 4)
  
  
  cat(sprintf("Finished projecting: %s. %d/%d, elapsed time: %f\n", class_names[class_index], class_index, length(class_names), Sys.time()-iterStart.time))
  
  sigma[[class_index]] <- cov(class_proj[[class_index]]$coeff[1:eigen_projection_size,])
  spect[[class_index]] <- eigen(sigma[[class_index]], symmetric = T)
  
  eigen_class_sp[[class_index]] <- lincomb(class_proj[[class_index]]$basis, t(spect[[class_index]]$vectors))
  
  eigen_coeffs[[class_index]] <- class_proj[[class_index]]$coeff %*% spect[[class_index]]$vectors
}
cat(sprintf("All projections finished, total elapsed time: %f\n", Sys.time()-totStart.time))

class_index <- match("Coat", class_names) #Gets index of a particular class.
par(mfrow = c(5,2))
for (class_index in 1:length(class_names)){
  plot(spect[[class_index]]$values, type ='l',col='blue', lwd=4 ,main = class_names[class_index], ylab = "")
}


eigen_length <- c(3,10,25,60)

test <- list()
test[["1,1"]] <- 1

eigenfunctions <- list()
for (i in 1:length(class_names)){
  eigenfunctions[[i]] <- list()
  for (j in 1:length(eigen_length)){
    eigenfunctions[[i]][[j]] <- subsample(eigen_class_sp[[i]],1:eigen_length[j])
  }
}

sample_class <- match("Bag", class_names)
sample_index <- 4520
eigen_projections <- list()
sample_eigen_coeffs <- list()
for (i in 1:length(class_names)){
  eigen_projections[[i]] <- list()
    for (j in 1:length(eigen_length)){
    sample_eigen_coeffs[[i]] <- class_proj[[sample_class]]$coeff[sample_index,] %*% spect[[i]]$vectors
    eigen_projections[[i]][[j]] <- lincomb(eigenfunctions[[i]][[j]], t(sample_eigen_coeffs[[i]][1:eigen_length[j]]))
  }
}

projection_class <- match("Bag", class_names)
#par(mfrow = c(length(eigen_length)+1,1))
plotpath <- sprintf("./plots/class_projections/%sOnto%s_%d.pdf",class_names[sample_class],class_names[projection_class],sample_index)
#pdf(plotpath, width = 5, height = 12)
layoutmat <- matrix(c(1,1,2,3), nrow = 2,ncol = 2, byrow = TRUE)
for(i in 1:(length(eigen_length)+1)){layoutmat <- rbind(layoutmat, c(i+3,i+3))}
layout(layoutmat, heights = c(1, rep(1,each = 1+length(eigen_length))))
par(mar = c(0,0,0,0))
plot.new()
text(0.5,0.5,sprintf("%s projected onto %s", class_names[sample_class], class_names[projection_class]), cex = 2)

par(mar = c(3,3,3,3))
image(1:28, 1:28, train_images_by_class[[sample_class]][sample_index,,], col = gray((0:255/(255))), xaxt = "n", yaxt = "n", main = class_names[sample_class])
image(1:28, 1:28, train_images_by_class[[projection_class]][sample_index,,], col = gray((0:255/(255))), xaxt = "n", yaxt = "n", main = class_names[projection_class])


plot(seq(image_size^2), train_vectors_by_class[[class_index]][sample_index,], type = "l", ylab = "", xlab = "", main = sprintf("Original data for %s",class_names[sample_class] ))
par(mar = c(3,1,3,1))
for (j in 1:length(eigen_length)){ 
  plot(eigen_projections[[projection_class]][[j]], type = "l", 
       main = sprintf("Projected onto %s with %d eigenvectors", 
                      class_names[projection_class], eigen_length[j]))
}
#plotpath <- sprintf("./plots/class_projections/%sOnto%s_%d.pdf/",class_names[sample_class],class_names[projection_class],sample_index)
#dev.copy2pdf(file = plotpath)
#dev.off()
#Plots a sample of the original data along with the projection onto
#eigenbases with different number of eigenvectors.

max_eigen_coeff <- as.matrix(lapply(sample_eigen_coeffs,max))
max_class_index <- which.max(max_eigen_coeff)
max_eigen_pos <- as.matrix(lapply(sample_eigen_coeffs,which.max))[max_class_index]
max_class <- class_names[max_class_index]
cat(sprintf("Predicted class: %s\nActual class: %s",max_class, class_names[sample_class]))
