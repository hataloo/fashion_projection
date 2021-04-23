library(Splinets)
library(pbapply)
library(tictoc)
library(pracma)


###FPCA centered around every class
#Consider lowering number_of_components_to_save to lower RAM-usage.
number_of_components_to_save <- min(800, number_of_knots - order -1)
principal_coeffs_array <- array(dim = c(number_of_images, length(class_names), number_of_components_to_save))
normalized_principal_coeffs_array <- array(dim = c(number_of_images, length(class_names), number_of_components_to_save))
test_principal_coeffs_array <- array(dim = c(length(test_labels), length(class_names), number_of_components_to_save))
test_normalized_principal_coeffs_array <- array(dim = c(length(test_labels), length(class_names), number_of_components_to_save))


sigma <- array(dim = c(length(class_names), number_of_knots - order -1, number_of_knots - order -1))
spect <- list()
principal_class_sp <- list()
for(projection_index in 1:length(class_names)){
  #centered_coeffs <- array(dim = c(sum(train_labels == projection_index-1), number_of_knots-order-1))
  #train_class_spline_coeffs <- (train_vector_splines$coeff[train_labels==(projection_index-1),])
  #for(sample_index in 1:(dim(centered_coeffs)[1])){centered_coeffs[sample_index,] <- train_class_spline_coeffs[sample_index,] - class_mean_splines$coeff[projection_index,] }
  #sigma[projection_index,,] <- cov(centered_coeffs)
  sigma[projection_index,,] <- cov(sweep(train_vector_splines$coeff[train_labels == (projection_index-1),],2,class_mean_splines$coeff[projection_index,]))
  #sigma[projection_index,,] <- cov(train_vector_splines$coeff[train_labels == (projection_index-1),] - class_mean_splines$coeff[projection_index,])
  spect[[projection_index]] <- eigen(sigma[projection_index,,], symmetric = TRUE)
}
pb <- timerProgressBar(min = 0, max = length(class_names))
for(projection_index in 1:length(class_names)){
  principal_class_sp[[projection_index]] <- lincomb(train_vector_splines$basis, t(spect[[projection_index]]$vectors))
  setTimerProgressBar(pb, value = projection_index)
}

for(projection_index in 1:length(class_names)){
  principal_coeffs_array[,projection_index,] <- (sweep(train_vector_splines$coeff, 2, class_mean_splines$coeff[projection_index,]) %*% spect[[projection_index]]$vectors)[,1:number_of_components_to_save]
  test_principal_coeffs_array[,projection_index,] <- (sweep(test_vector_splines$coeff, 2, class_mean_splines$coeff[projection_index,]) %*% spect[[projection_index]]$vectors)[,1:number_of_components_to_save]
  for(component_index in 1:number_of_components_to_save){
    normalized_principal_coeffs_array[,projection_index,component_index] <- principal_coeffs_array[,projection_index, component_index]/sqrt((spect[[projection_index]]$values[component_index]))
    test_normalized_principal_coeffs_array[,projection_index,component_index] <- test_principal_coeffs_array[,projection_index, component_index]/sqrt((spect[[projection_index]]$values[component_index]))
  }
}

### Testing if the principal components look reasonable.
#Reasonable if they have some resemblance of the original class.
class_index <- 1
component_index <- 1
spline_eval <- evspline(principal_class_sp[[class_index]], sID = 1:5, seq(image_size^2))
for(i in 1:3){image(1:28, 1:28, matrix(spline_eval[,i+1], ncol = 28, nrow = 28, byrow = FALSE), col = gray((0:255/(255))), xaxt = "n", yaxt = "n")
}
mean_eval <- evspline(class_mean_splines$sp, sID = class_index, seq(image_size^2))
image(1:28, 1:28, matrix(spline_eval[,component_index+1]*spect[[class_index]]$values[component_index] + mean_eval[,2], ncol = 28, nrow = 28, byrow = FALSE), col = gray((0:255/(255))), xaxt = "n", yaxt = "n")

eig_funcs_to_use <- 5
spline_eval <- evspline(principal_class_sp[[class_index]], sID = 1:eig_funcs_to_use, seq(image_size^2))
mean_eval <- evspline(class_mean_splines$sp, sID = class_index, seq(image_size^2))
pixel_value <- array(mean_eval[,2], dim = c(image_size^2))
for(i in 1:eig_funcs_to_use){
  pixel_value <- pixel_value + principal_coeffs_array[sample_index,class_index,i]*spline_eval[,i+1]
}


#Verify that the coefficients have mean ~0 and var ~1,
#hist should hopefully resemble a Gaussian but not necessary.
class_index <- 1
component_index <- 1
hist(normalized_principal_coeffs_array[train_labels == class_index-1,class_index,component_index], breaks = 40)
mean(normalized_principal_coeffs_array[train_labels == class_index-1,class_index,component_index])
var(normalized_principal_coeffs_array[train_labels == class_index-1,class_index,component_index])

number_of_components_in_matrix <- 80
principal_coeffs_matrix <- matrix(principal_coeffs_array, nrow = number_of_images, ncol = length(class_names)*number_of_components_in_matrix)
normalized_principal_coeffs_matrix <- matrix(normalized_principal_coeffs_array, nrow = number_of_images, ncol = length(class_names)*number_of_components_in_matrix)
test_principal_coeffs_matrix <- matrix(test_principal_coeffs_array, nrow = length(test_labels), ncol = length(class_names)*number_of_components_in_matrix)
test_normalized_principal_coeffs_matrix <- matrix(test_normalized_principal_coeffs_array, nrow = length(test_labels), ncol = length(class_names)*number_of_components_in_matrix)


save(principal_coeffs_array, file = "principalcoeffdata/principal_coeffs_array.RData")
save(normalized_principal_coeffs_array, file = "principalcoeffdata/principal_coeffs_array.RData")
save(principal_class_sp, file = "principalcoeffdata/principal_class_sp.RData")
save(spect, file = "principalcoeffdata/spect_classwise.RData")
npySave("principalcoeffdata/principal_coeffs_matrix.npy", principal_coeffs_matrix)
npySave("principalcoeffdata/principal_coeffs_matrix_cut.npy", principal_coeffs_matrix[,1:(10*10)])
npySave("principalcoeffdata/normalized_principal_coeffs_matrix.npy", normalized_principal_coeffs_matrix)
npySave("principalcoeffdata/normalized_principal_coeffs_matrix_cut.npy", normalized_principal_coeffs_matrix[,1:(10*10)])


npySave("principalcoeffdata/test_principal_coeffs_matrix.npy", test_principal_coeffs_matrix)
npySave("principalcoeffdata/test_normalized_principal_coeffs_matrix.npy", test_normalized_principal_coeffs_matrix)














