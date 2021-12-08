# Start of Subtask E
# Making an algorithm to find the optimal solution for parameters for DBSCAN
# Between 2 and 15 clusters, and outlier rate 10% or less

set.seed(9)

optimal_purity = 0; # var to store the optimal purity

for (points in 1:10){ # minPoint from 1 to 10
  for (epsilon in seq(0, 30, length.out = 300)){ # try 300 epsilon values x 10 minPoints (total 3000 combos)
    dbs_t = dbscan::dbscan(Complex9.features, eps = epsilon, minPts = points) #dbscan
    num_clusters = (length(table(dbs_t$cluster))-1)#get total classes
    
    if (num_clusters > 2 && num_clusters < 15) {
      purity_t = purity(dbs_t$cluster, Complex9$V3, T)[1] #get purity
      outlier_t = purity(dbs_t$cluster, Complex9$V3, T)[2] #get outlier rate
      
      if (outlier_t < 0.10 && purity_t > optimal_purity){
        optimal_dbscan = dbs_t
        optimal_purity = purity_t
        optimal_outlier = outlier_t
        optimal_total_clusters = num_clusters
      }
    }
  }
} # End of parameter testing loops

optimal_dbscan

cat("The optimal DBSCAN obtained was with parameters: ", "Epsilon: ", optimal_dbscan$eps, " Min Points: ", optimal_dbscan$minPts)
cat("We get a purity of: ", optimal_purity, " and a outlier rate of: ",
    optimal_outlier, " with a number of clusters of: ", optimal_total_clusters)

#plotting optimal DBSCAN
custom_colors = c("#bb9ed3", "#2d3c71", "#9cee9f", "#a70a6d", "#cfca5a", "#87926c",
                  "#1d69c3", "#c8002a", "#3d2733", "#87926c", "#d996a4", "#00730d",
                  "#f95237", "#feaf42", "#7096d8", "#d996a4")

plot(Complex9[c("V1","V2")], main="Complex9 Optimal DBSCAN", col = custom_colors[optimal_dbscan$cluster])

# End of Subtask E