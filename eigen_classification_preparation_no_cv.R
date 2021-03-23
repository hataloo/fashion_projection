library(Splinets)
library(pbapply)
library(tictoc)
library(pracma)

total_number_of_images <- length(train_labels)
#Rearrange the data so its in the "correct" orientation when shown using image()

for(i in 1:total_number_of_images){
  flipped_train_images[i,,] <- t(apply(train_images[i,,],2,rev))/255
}


order <- 4
number_of_knots <- image_size*8
knots <- seq(from = 1, to = image_size^2, length.out = number_of_knots)



class_means <- array(dim = c(length(class_names), image_size^2))


tic("Calculating and projecting means")

for (class_index in 1:length(class_names)){
  temp <- flipped_train_images[train_labels== (class_index-1),,]
  dim(temp) <- c(6000,28^2)
  class_means[class_index,] <- colMeans(temp)
}
data_values <- cbind(seq(image_size^2), t(class_means))
class_means_splines <- project(data_values, knots, order)
toc()


number_to_project <- total_number_of_images
labels_projected <- train_labels[1:number_to_project]

data_values <- flipped_train_images[1:number_to_project,,]
dim(data_values) <- c(number_to_project, image_size^2)

tic("Projecting the data onto Splines")
train_vector_splines <- project(cbind(seq(image_size^2), t(data_values)), knots, order) 
toc()

##FPCA centered around every class
#number_of_knots - order -1 is the number of coefficients the Splines have.
number_of_eigenvalues_to_save <- min(80, number_of_knots - order -1)
eigen_coeffs_array <- array(dim = c(number_to_project, length(class_names), number_of_eigenvalues_to_save))

sigma <- array(dim = c(length(class_names), number_of_knots - order -1, number_of_knots - order -1))
spect <- list()
eigen_class_sp <- list()

for(projection_index in 1:length(class_names)){
  sigma[projection_index,,] <- cov(train_vector_splines$coeff[labels_projected == (projection_index-1), ] - class_means_splines$coeff[projection_index,])
  spect[[projection_index]] <- eigen(sigma[projection_index,,], symmetric = T)
  
  eigen_class_sp[[projection_index]] <- lincomb(train_vector_splines$basis, t(spect[[projection_index]]$vectors))
  temp <- (train_vector_splines$coeff - class_means_splines$coeff[projection_index,]) %*% spect[[projection_index]]$vectors
  eigen_coeffs_array[,projection_index,] <- temp[,1:number_of_eigenvalues_to_save]
}

eigen_coeffs_matrix <- matrix(eigen_coeffs_array, nrow = number_to_project, ncol = length(class_names)*number_of_eigenvalues_to_save)

#The coefficients are saved as a long vector for every sample
#From:     vector_1  vector_2
# class_1:
# class_2
# class_3:
#To: 
# class_1,vector_1:
# class_2,vector_1:
# class_3,vector_1:
# class_1,vector_2:
# class_2,vector_2:
# class_3,vector_2:

save(eigen_coeffs_matrix, file = "eigencoeffdata/total_eigen_coeffs_matrix.RData")
library(RcppCNPy)
npySave("eigencoeffdata/total_eigen_coeffs_matrix.npy", eigen_coeffs_matrix)
npySave("eigencoeffdata/total_eigen_coeffs_matrix_cut.npy", eigen_coeffs_matrix[,1:(10*10)])
npySave("eigencoeffdata/train_labels.npy", train_labels)

##FPCA centered around entire dataset
number_of_eigenvalues_to_save <- min(80, number_of_knots - order -1)
eigen_coeffs_matrix_all <- array(dim = c(number_to_project, number_of_eigenvalues_to_save))

dataset_mean_spline_coeffs <- colMeans(train_vector_splines$coeff)
sigma_all <- cov(train_vector_splines$coeff - dataset_mean_spline_coeffs)
spect_all <- eigen(sigma_all, symmetric = T)
eigen_class_sp_all <- lincomb(train_vector_splines$basis, t(spect_all$vectors))
eigen_coeffs_matrix_all <- ((train_vector_splines$coeff - dataset_mean_spline_coeffs) %*% spect_all$vectors)[,1:number_of_eigenvalues_to_save]

npySave("eigencoeffdata/eigen_coeffs_matrix_entire_dataset.npy", eigen_coeffs_matrix_all)


all_distances <- array(dim = c(length(train_labels), length(class_names)))
for(sample_index in 1:length(train_labels)){
  for(projection_index in 1:length(class_names)){
    temp <- flipped_train_images[sample_index,,]
    dim(temp) <- c(28^2,1)
    all_distances[sample_index,projection_index] <- sum((temp - (class_means[projection_index,]))^2)
  } 
}
##To check that we have around 68% accuracy.
correct <- 0
for(i in 1:60000){
  ind <- which.min(all_distances[i,])
  if(ind == train_labels[i]+1){correct = correct + 1}
}
print(correct/60000)

npySave("eigencoeffdata/all_distances.npy", all_distances)


test <- evspline(eigen_class_sp[[10]], sID = 1, seq(1,784))[,2]
dim(test) <- c(28,28)
image(1:28, 1:28, test, col = gray((0:255/(255))), asp = 1, bty = "n",xaxt = "n", yaxt = "n", xlab = "", ylab = "")

 
for(my_ind in 1:10){
  test <- (train_vector_splines$coeff[train_labels == my_ind-1,] - class_means_splines$coeff[my_ind,]) %*% spect[[my_ind]]$vectors
  test2 <- colMeans(test)
  print(max(test2))
  print(min(test2))
}

mean_coeffs <- array(dim = c(10,40))
max_across_eigen <- array(dim = c(40))

for(class_index in 1:10){
  for(eigen_index in 1:40){
    mean_coeffs[class_index,eigen_index] <- mean(eigen_coeffs_array[train_labels==(class_index-1), class_index, eigen_index])/sqrt(spect[[class_index]]$values[eigen_index])
  }
}
for(eigen_index in 1:40){
  max_across_eigen[eigen_index] <- max(abs(mean_coeffs[,eigen_index]))
}

hist(mean_coeffs, breaks = 100, freq = FALSE)

hist(eigen_coeffs_array[train_labels == 0, 1, 1]/sqrt(spect[[1]]$values[2]), breaks = 100, freq = F)
