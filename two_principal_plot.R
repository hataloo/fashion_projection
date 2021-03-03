library("colorspace")
library("ggplot2")
library("pbapply")
library("dplyr")

source("data_preparation_functions.R")
source("eigen_projection_functions.R")

eigen_length = 2
class_index = 4
projection_index <- 1
number_of_classes <- 2
number_of_samples <- 10

eigenfunctions <- get_eigenfunctions(eigen_class_sp[[1]], eigen_length)

eigen_coeff_df <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(eigen_coeff_df) <- c("Class","x","y","ProjClass")

pb <- timerProgressBar(min = 1, max = number_of_classes^2, style = 3)
for (projection_index in 1:number_of_classes){
  for (j in 1:number_of_classes){
    centered_vector_data <- matrix(nrow = number_of_samples, ncol = image_size^2)
    for (i in 1:number_of_samples){
      #print(sprintf("Class %d / %d, sample: %d / %d", j,number_of_classes,i,number_of_samples))
      centered_vector_data[i,] <- train_vectors_by_class[[j]][i,] - class_vector_means[[projection_index]]
    }
    eigen_projection_info <- spline_and_eigen_project_samples(centered_vector_data,knots, order, eigenfunctions, spect[[projection_index]], eigen_length)
    eigen_test <- eigen_projection_info[["coeffs"]]
    eigen_coeff_df <- rbind(eigen_coeff_df, data.frame(Class = class_names[j], 
                                                       x = eigen_projection_info[["coeffs"]][,1],
                                                       y = eigen_projection_info[["coeffs"]][,2],
                                                       ProjClass = class_names[projection_index]))
    setTimerProgressBar(pb, getTimerProgressBar(pb)+1)
  }
}

projclass <- "Trouser"
p <- ggplot(dplyr::filter(eigen_coeff_df, ProjClass == projclass), aes(x = x, y = y, color = Class, size = 0.5)) + geom_point() +
  labs(x = "First principal component", 
       y = "Second principal component",
       title = sprintf("Data projected onto principal components of %s", projclass)) + 
  theme(plot.title = element_text(hjust = 0.5))
p



#Old sol:
#pb <- timerProgressBar(min = 1, max = number_of_classes*number_of_samples, style = 3)
#for (j in 1:number_of_classes){
#centered_vector_data <- matrix(nrow = number_of_samples, ncol = image_size^2)

#for (i in 1:number_of_samples){
#print(sprintf("Class %d / %d, sample: %d / %d", j,number_of_classes,i,number_of_samples))
#centered_vector_data[i,] <- train_vectors_by_class[[j]][i,] - class_vector_means[[projection_index]]
#eigen_projection_info <- spline_and_eigen_project_sample(train_vectors_by_class[[j]][i,] - class_vector_means[[projection_index]],
#knots, order, eigenfunctions, spect[[projection_index]], eigen_length)
# eigen_coeff_matrix[i,] <- eigen_projection_info[["coeffs"]]
#  eigen_coeff_df <- rbind(eigen_coeff_df, data.frame(Class = class_names[j], x = eigen_projection_info[["coeffs"]][1], y = eigen_projection_info[["coeffs"]][2]))
#   setTimerProgressBar(pb,getTimerProgressBar(pb)+1)  
#}
#plot(eigen_coeff_matrix, col = pal[j], add = T)
#}
