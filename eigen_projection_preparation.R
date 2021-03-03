source("data_preparation_functions.R")
source("eigen_projection_functions.R")
library(pbapply)
#Run data_preparation.R prior to running this file.


knots <- seq(from = 0, to = image_size^2, length.out = image_size*8)
order <- 4
#examine_new_sample(knots, order, train_images, class_names)
class_index <- match("Bag", class_names) #Gets index of a particular class.
sample_size <- 1000
eigen_projection_size <- 500 #Number of samples used for eigen-decomp.

class_projection <- list()
sigma <- list()
spect <- list()
eigen_class_sp <- list()
eigen_coeffs <- list()


#class_projection <- pblapply(train_vectors_by_class_centered, project_class_onto_splines,knots,order,sample_size)
#eigen_info <- pblapply(class_projection, eigen_decompose_class, eigen_projection_size)

mean_projection <- project_class_onto_splines(class_vector_means_mat, knots, order, sample_size = length(class_names))
non_centered_class_projection <- pblapply(train_vectors_by_class, project_class_onto_splines,knots,order,sample_size)

pb <- timerProgressBar(min = 1, max = length(class_names), style = 3)
eigen_info <- list()
for(projection_index in 1:length(class_names)){
  eigen_info[[projection_index]] <- eigen_decompose_class_non_centered(non_centered_class_projection[[projection_index]], mean_projection, eigen_projection_size, projection_index)
  setTimerProgressBar(pb, getTimerProgressBar(pb)+1)
}


for (i in 1:length(class_names)){
  sigma[[i]] = eigen_info[[i]][["sigma"]]
  spect[[i]] = eigen_info[[i]][["spect"]]
  eigen_class_sp[[i]] = eigen_info[[i]][["eigen_class_sp"]]
  eigen_coeffs[[i]] = eigen_info[[i]][["coeffs"]]
}