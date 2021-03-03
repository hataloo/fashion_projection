library("pbapply")
K = 10
total_number_of_images <- length(train_labels)
training_images_per_validation <- total_number_of_images*(1-1/K)
test_images_per_validation <- total_number_of_images/K
cross_val_means <- array(dim = c(K, length(class_names),image_size^2))

removed_accuracy <- array(dim = c(K))
remaining_accuracy <- array(dim = c(K))

pb <- timerProgressBar(min = 0, max = K*class_index, style = 3)
for(i in 1:K){
  removed_data <- train_images[((total_number_of_images/K*(i-1)+1):(total_number_of_images/K*i)),,]/255
  dim(removed_data) <- c(test_images_per_validation, image_size^2)
  removed_labels <- train_labels[((total_number_of_images/K*(i-1)+1):(total_number_of_images/K*i))]
  
  remaining_data <- train_images[-((total_number_of_images/K*(i-1)+1):(total_number_of_images/K*i)),,]/255
  dim(remaining_data) <- c(training_images_per_validation, image_size^2)
  remaining_labels <- train_labels[-((total_number_of_images/K*(i-1)+1):(total_number_of_images/K*i))]
  
  removed_distances <- array(dim = c(test_images_per_validation, length(class_names)))
  remaining_distances <- array(dim = c(training_images_per_validation, length(class_names)))
  
  
  for(class_index in 1:length(class_names)){
    temp <- apply((remaining_data[remaining_labels == class_index-1,]), c(2), mean)
    dim(temp) <- c(image_size^2)
    cross_val_means[i,class_index,] <- temp
    removed_distances[, class_index] <- apply((removed_data-cross_val_means[i,class_index,])^2, c(1) ,sum)
    remaining_distances[, class_index] <- apply((remaining_data-cross_val_means[i,class_index,])^2, c(1) ,sum)
    setTimerProgressBar(pb, getTimerProgressBar(pb)+1)
  }
  removed_classification <- apply(removed_distances, c(1), which.min)-1
  removed_accuracy[i] = mean(removed_classification == removed_labels)
  remaining_classification <- apply(remaining_distances, c(1), which.min)-1
  remaining_accuracy[i] = mean(remaining_classification == remaining_labels)
}

print(removed_accuracy)
print(remaining_accuracy)
#Meh results, around 15% accurate.

#Inspecting the result of the prints below, we see that
#the classifier is heavily biased to some classes
for(i in 0:9){print(sum(remaining_classification==i))}
for(i in 0:9){print(sum(removed_classification==i))}