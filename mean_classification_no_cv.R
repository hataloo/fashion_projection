library(pbapply)
total_mean_images <- list()
for(class_index in 1:length(class_names)){
  total_mean_images[[class_index]] <- colMeans(train_vectors_by_class[[class_index]])
}

all_distances <- array(dim = c(10,6000,10))
#[class_index_data,sample_index, distance_to_class_index]

pb <- timerProgressBar(min = 0, max = 10*10*6000, style = 3)
for(class_index in 1:length(class_names)){
  for(distance_to_class_index in 1:length(class_names)){
    for(sample_index in 1:6000){
      all_distances[[class_index, sample_index, distance_to_class_index]] <- sum((train_vectors_by_class[[class_index]][sample_index,] - total_mean_images[[distance_to_class_index]])^2)
      setTimerProgressBar(pb, 1+ getTimerProgressBar(pb))
    }
  }
}

classification_matrix <- array(dim = c(10, 6000))
correct_classifications <- 0

for(class_index in 1:length(class_names)){
    for(sample_index in 1:6000){
      classification_matrix[class_index, sample_index] <- which.min(all_distances[class_index, sample_index,])
      if(classification_matrix[class_index, sample_index] == class_index){
        correct_classifications <- 1 + correct_classifications
      }
    }
}
print(correct_classifications/number_of_images)
