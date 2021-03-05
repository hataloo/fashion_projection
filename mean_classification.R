library("pbapply")
K = 10
total_number_of_images <- length(train_labels)
training_images_per_validation <- total_number_of_images*(1-1/K)
test_images_per_validation <- total_number_of_images/K
cross_val_means <- array(dim = c(K, length(class_names),image_size^2))

removed_accuracy <- array(dim = c(K))
remaining_accuracy <- array(dim = c(K))

pb <- timerProgressBar(min = 0, max = K*class_index, style = 3)
for(validation_index in 1:K){
  removed_data <- train_images[((total_number_of_images/K*(validation_index-1)+1):(total_number_of_images/K*validation_index)),,]/255
  dim(removed_data) <- c(test_images_per_validation, image_size^2)
  removed_labels <- train_labels[((total_number_of_images/K*(validation_index-1)+1):(total_number_of_images/K*validation_index))]
  
  remaining_data <- train_images[-((total_number_of_images/K*(validation_index-1)+1):(total_number_of_images/K*validation_index)),,]/255
  dim(remaining_data) <- c(training_images_per_validation, image_size^2)
  remaining_labels <- train_labels[-((total_number_of_images/K*(validation_index-1)+1):(total_number_of_images/K*validation_index))]
  
  removed_distances <- array(dim = c(test_images_per_validation, length(class_names)))
  remaining_distances <- array(dim = c(training_images_per_validation, length(class_names)))
  
  
  for(class_index in 1:length(class_names)){
    temp <- apply((remaining_data[remaining_labels == class_index-1,]), c(2), mean)
    cross_val_means[validation_index,class_index,] <- temp
    for(sample_index in 1:test_images_per_validation){
      removed_distances[sample_index, class_index] <- mean((removed_data[sample_index, ] - cross_val_means[validation_index, class_index,])^2)
    }
    for(sample_index in 1:training_images_per_validation){
      remaining_distances[sample_index, class_index] <- mean((remaining_data[sample_index,] - cross_val_means[validation_index, class_index,])^2)
    }
    setTimerProgressBar(pb, getTimerProgressBar(pb)+1)
  }
  removed_classification <- apply(removed_distances, c(1), which.min)-1
  removed_accuracy[validation_index] = mean(removed_classification == removed_labels)
  remaining_classification <- apply(remaining_distances, c(1), which.min)-1
  remaining_accuracy[validation_index] = mean(remaining_classification == remaining_labels)
}

for(i in 0:9){print(sum(remaining_classification==i))}
for(i in 0:9){print(sum(removed_classification==i))}

print(removed_accuracy)
print(remaining_accuracy)
#Impressive results, around 68% accurate.
print(mean(abs(removed_accuracy- remaining_accuracy)))
#The average accuracy barely differs. 
