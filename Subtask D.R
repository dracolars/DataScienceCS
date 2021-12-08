# Start of Subtask D

#install needed packages
install.packages("factoextra")
install.packages("dbscan")
library(dbscan)

#working with a subset of z-score values (without class) for this question
sub_zpidd = apply(pidd.clean[1:6], MARGIN = 2, FUN = scale)
#getting dbscan and seeing its purity and outlier rate
set.seed(9)
dbs = dbscan::dbscan(sub_zpidd, eps = 1.2, minPts = 2)
dbs
t_purity = purity(dbs$cluster, pidd.clean$V9, T)[1]
t_outlier = purity(dbs$cluster, pidd.clean$V9, T)[2]

cat("The purity of this DBSCAN (eps = 1.2, minPts = 2) is:" , t_purity)
cat("Its outlier rate is: " , t_outlier)

# End of Subtask D