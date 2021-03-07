library(Splinets)
library(pbapply)
library(tictoc)

order <- 4
number_of_knots_cv <- c(image_size*4, image_size*8, image_size*16)
number_of_eigenvalues_cv <- c(5,10,20)

K = 3
total_number_of_images <- length(train_labels)
removed_images_per_validation <- total_number_of_images/K
remaining_images_per_validation <- total_number_of_images-removed_images_per_validation

cross_val_means <- array(dim = c(K, length(class_names), image_size^2))
cross_val_means_splines <- list()

tic("Calculating and projecting means")
pb <- timerProgressBar(0, K*length(number_of_knots_cv), style = 3)

for(validation_index in 1:K){
  #Partition data into removed and remaining.
  removed_data <- train_images[((total_number_of_images/K*(validation_index-1)+1):(total_number_of_images/K*validation_index)),,]/255
  dim(removed_data) <- c(removed_images_per_validation, image_size^2)
  removed_labels <- train_labels[((total_number_of_images/K*(validation_index-1)+1):(total_number_of_images/K*validation_index))]
  
  remaining_data <- train_images[-((total_number_of_images/K*(validation_index-1)+1):(total_number_of_images/K*validation_index)),,]/255
  dim(remaining_data) <- c(remaining_images_per_validation, image_size^2)
  remaining_labels <- train_labels[-((total_number_of_images/K*(validation_index-1)+1):(total_number_of_images/K*validation_index))]
  
  #Calculate mean for each class in the remaining_data
  for(class_index in 1:length(class_names)){
    cross_val_means[validation_index, class_index,] <- apply((remaining_data[remaining_labels == (class_index-1), ]), c(2), mean)
  }
  
  #Convert the mean vectors into splines for each number of knots.
  cross_val_means_splines[[validation_index]] <- list()
  for (knot_index in 1:length(number_of_knots_cv)){
    knots <- seq(from = 0, to = image_size^2, length.out = number_of_knots_cv[knot_index])
    
    data_values <- cbind(seq(image_size^2), t(cross_val_means[validation_index,,]))
    cross_val_means_splines[[validation_index]][[knot_index]] <- project(data_values, knots, order)
    setTimerProgressBar(pb, getTimerProgressBar(pb)+1)
  }
}
toc()

#Project ALL data (uncentered) onto Splines for each choice of number of knots.
train_splines <- list()

number_to_project <- total_number_of_images
time_to_project <- array(dim = c(length(number_of_knots_cv)))

tic("Projecting all data onto Splines")
pb <- timerProgressBar(0, length(number_of_knots_cv), style = 3)
for(knot_index in 1:length(number_of_knots_cv)){
  tic(knot_index)
  knots <- seq(from = 0, to = image_size^2, length.out = number_of_knots_cv[knot_index])
  data_values <- train_images[1:number_to_project,,]/255
  dim(data_values) <- c(number_to_project, image_size^2)
  data_values <- cbind(seq(image_size^2), t(data_values))
  train_splines[[knot_index]] <- project(data_values, knots, order)
  setTimerProgressBar(pb, getTimerProgressBar(pb)+1)
  temp <- toc(quiet = T)
  time_to_project[knot_index] <- temp$toc - temp$tic
}
toc()


pb <- timerProgressBar(0, K*length(number_of_knots_cv), style = 3)

#Lists indexed over the knots, not CV_index
removed_eigen_coeffs <- list()
remaining_eigen_coeffs <- list()

for(knot_index in 1:length(number_of_knots_cv)){
  #[K, #samples, #classes, coeff_index]
  removed_eigen_coeffs_arr <- array(dim = c(K, removed_images_per_validation, length(class_names), number_of_knots_cv[knot_index]-order-1))
  remaining_eigen_coeffs_arr <- array(dim = c(K, remaining_images_per_validation, length(class_names) ,number_of_knots_cv[knot_index]-order-1))
  
  for(validation_index in 1:K){
    removed_coeffs <- train_splines[[knot_index]]$coeff[((total_number_of_images/K*(validation_index-1)+1):(total_number_of_images/K*validation_index)),]
    removed_labels <- train_labels[((total_number_of_images/K*(validation_index-1)+1):(total_number_of_images/K*validation_index))]

    remaining_coeffs <- train_splines[[knot_index]]$coeff[-((total_number_of_images/K*(validation_index-1)+1):(total_number_of_images/K*validation_index)),]
    remaining_labels <- train_labels[-((total_number_of_images/K*(validation_index-1)+1):(total_number_of_images/K*validation_index))]
    
    #For each class, project the removed and remaining data onto the eigenvectors of the remaining data. Only saves the coefficients.
    for(projection_index in 1:length(class_names)){
      sigma <- cov(remaining_coeffs[remaining_labels == projection_index-1,] - cross_val_means_splines[[validation_index]][[knot_index]]$coeff[projection_index,])
      spect <- eigen(sigma, symmetric = T)
      
      removed_eigen_coeffs_arr[validation_index, ,projection_index,] <- (removed_coeffs - cross_val_means_splines[[validation_index]][[knot_index]]$coeff[projection_index,]) %*% spect$vectors
      remaining_eigen_coeffs_arr[validation_index, ,projection_index,] <- (remaining_coeffs - cross_val_means_splines[[validation_index]][[knot_index]]$coeff[projection_index,]) %*% spect$vectors
    }
    setTimerProgressBar(pb,getTimerProgressBar(pb)+1)
  }
  removed_eigen_coeffs[[knot_index]] <- removed_eigen_coeffs_arr
  remaining_eigen_coeffs[[knot_index]] <- remaining_eigen_coeffs_arr
}


#### SAVES EVERYTHING INTO NUMPY ARRAYS ###
library(RcppCNPy)
save_data <- FALSE
if(save_data){
  for(validation_index in 1:K){
    for(knot_index in 1:length(number_of_knots_cv)){
      folderpath <- "eigencoeffdata/"
      filepath <- sprintf("removed_coeff_val_%d-%d_knot_%d-%d.npy", validation_index, K, knot_index, length(number_of_knots_cv))
      npySave(paste(folderpath,filepath, sep = ""), matrix(data = removed_eigen_coeffs[[knot_index]][validation_index, , ,], 
                                                           nrow = removed_images_per_validation, ncol = length(class_names)*(number_of_knots_cv[knot_index]-order-1))
              )
      filepath <- sprintf("remaining_coeff_val_%d-%d_knot_%d-%d.npy", validation_index, K, knot_index, length(number_of_knots_cv))
      npySave(paste(folderpath,filepath, sep = ""), matrix(data = remaining_eigen_coeffs[[knot_index]][validation_index, , ,], 
                                                           nrow = remaining_images_per_validation, ncol = length(class_names)*(number_of_knots_cv[knot_index]-order-1))
      )
    }
  }
}
