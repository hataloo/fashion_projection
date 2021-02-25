library(ggplot2)

#class_index <- match("Bag", class_names)

for(class_index in 1:length(class_names)){
  sample_index <- 1
  folderpath <- "./plots/data_visualisation"
  imagepath <- sprintf("./plots/data_visualisation/%s_%d_image.pdf",gsub("/","-",class_names[class_index]),sample_index)
  vectorpath <- sprintf("./plots/data_visualisation/%s_%d_vector.pdf",gsub("/","-",class_names[class_index]),sample_index)
  
  pdf(imagepath, width = 3, height = 3)
  par(mfrow = c(1,1))
  
  image(1:28, 1:28, train_images_by_class[[class_index]][sample_index,,], col = gray((0:255/(255))), asp = 1, bty = "n",xaxt = "n", yaxt = "n", xlab = "", ylab = "")
  dev.off()
  
  p <- ggplot(data.frame(pos = seq(image_size^2), value = train_vectors_by_class[[class_index]][sample_index,]),
              aes(x = pos, y= value)) + geom_area() + 
        theme(axis.title.x=element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.y=element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank())
  #show(p)
  ggsave(vectorpath, width = 18, height = 3)
}