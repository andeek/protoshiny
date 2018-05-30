## library
library(protoclust)

## data
faces <- as.matrix(read.csv("application/data/faces.csv", head = FALSE))
faces <- t(apply(faces,2,function(xx) as.vector(matrix(xx,64,64,byrow=T))))

## save faces as images
library(ggplot2)
library(dplyr)

for(i in seq_len(nrow(faces))) {
  expand.grid(x = 1:64, y = 1:64) %>%
    ggplot() +
    geom_raster(aes(x = x, y = y, fill = rev(faces[i,]))) +
    scale_fill_continuous(low = "black", high = "white") +
    theme_void() +
    theme(legend.position = "hidden", aspect.ratio = 1) +
    theme(plot.margin = grid::unit(c(0,0,-1,-1), "mm")) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) -> p
  
  ggsave(paste0("application/data/img_labels/faces/", i, ".png"), p, width = 0.25, height = 0.25)
}

## hclust object
d <- dist(faces)
hc <- protoclust(d)
hc$labels <- rep(1:40, each=10)

## add image labels
## in order of data records
hc$img = paste0(seq_len(nrow(faces)), ".png")

## where images are stored
img_dir <- "application/data/img_labels/faces/"

## save objects
save(hc, img_dir, file = "application/data/faces_hc.Rdata")



  
