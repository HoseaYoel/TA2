library(readxl)
library(dplyr)

# Read the data
malaria = read_excel("Malaria rate.xlsx")

# Normalize the data
cols_to_normalize = c("qx (under 5)", "qx (5-14)", "qx (15-49)", "qx (50-69)", "qx (70+)", "qx (all ages)", "qx (standardize)")
malaria[cols_to_normalize] = apply(malaria[cols_to_normalize], 2, function(x) {
  (x - min(x)) / (max(x) - min(x))
})

# Clustering
cols_for_clustering = setdiff(names(malaria), c("Entity", "Year"))

# Elbow method
test = NULL
for(k in 1:15){
  cluster_mod = kmeans(malaria[cols_for_clustering], centers = k, nstart = 10)
  test = c(test, cluster_mod$tot.withinss)
}
plot(1:15, test, type = "b", pch = 19,
     xlab = "number of clusters",
     ylab = "total variations")

#3 Cluster is the optimal number of cluster
