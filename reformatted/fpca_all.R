library(Splinets)
library(pbapply)
library(tictoc)
library(pracma)

###FPCA centered around entire dataset
number_of_components_to_save <- min(800, number_of_knots - order -1)
principal_coeffs_matrix_all <- array(dim = c(number_of_images, number_of_components_to_save))
normalized_principal_coeffs_matrix_all <- array(dim = c(number_of_images, number_of_components_to_save))
test_principal_coeffs_matrix_all <- array(dim = c(length(test_labels), number_of_components_to_save))
test_normalized_principal_coeffs_matrix_all <- array(dim = c(length(test_labels), number_of_components_to_save))



dataset_mean_spline_coeffs <- colMeans(train_vector_splines$coeff)
sigma_all <- cov(sweep(train_vector_splines$coeff,2,dataset_mean_spline_coeffs))
spect_all <- eigen(sigma_all, symmetric = T)

principal_class_sp_all <- lincomb(train_vector_splines$basis, t(spect_all$vectors))
principal_coeffs_matrix_all <- ((sweep(train_vector_splines$coeff,2,dataset_mean_spline_coeffs)) 
                                %*% spect_all$vectors)[,1:number_of_components_to_save]
for(component_index in 1:number_of_components_to_save){
  normalized_principal_coeffs_matrix_all[,component_index] <- principal_coeffs_matrix_all[,component_index]/sqrt(spect_all$values[component_index])
}
test_principal_coeffs_matrix_all <- ((sweep(test_vector_splines$coeff,2,dataset_mean_spline_coeffs)) 
                                %*% spect_all$vectors)[,1:number_of_components_to_save] 
for(component_index in 1:number_of_components_to_save){
  test_normalized_principal_coeffs_matrix_all[,component_index] <- test_principal_coeffs_matrix_all[,component_index]/sqrt(spect_all$values[component_index])
}
npySave("principalcoeffdata/principal_coeffs_matrix_entire_dataset.npy", principal_coeffs_matrix_all)
npySave("principalcoeffdata/test_principal_coeffs_matrix_entire_dataset.npy", test_principal_coeffs_matrix_all)
npySave("principalcoeffdata/normalized_principal_coeffs_matrix_entire_dataset.npy", normalized_principal_coeffs_matrix_all)
npySave("principalcoeffdata/test_normalized_principal_coeffs_matrix_entire_dataset.npy", test_normalized_principal_coeffs_matrix_all)



