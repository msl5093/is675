library(fpc)

protein <- read.csv("protein.csv")
str(protein)



pro.sources <- protein[2:10]
pro.sources_z <- as.data.frame(lapply(pro.sources, scale))

set.seed(123)
protein_clusters <- kmeans(pro.sources_z, 5)

protein_clusters$size
protein_clusters$centers

protein$cluster <- protein_clusters$cluster

aggregate(data = protein, RedMeat ~ cluster, mean)
aggregate(data = protein, Milk ~ cluster, mean)


plotcluster(protein[2:10], protein$cluster)