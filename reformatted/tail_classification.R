pcs_to_include = c(10, 20, 30, 60, 90, 120, 150, 180)

for(included_pc in pcs_to_include){
  tail_distance <- array(dim = c(60000,10))
  tail_classification <- array(dim = c(60000))
  for(i in 1:60000){
    for(class_index in 1:10){
      tail_distance[i,class_index] <- sum(principal_coeffs_array[i,class_index,included_pc:dim(principal_coeffs_array)[3]]^2)
    }
    tail_classification[i] <- which.min(tail_distance[i,])
  }
  print(sprintf("%d included PCs: %0.2f percent", included_pc, 100*mean((tail_classification-1) == train_labels)))
}