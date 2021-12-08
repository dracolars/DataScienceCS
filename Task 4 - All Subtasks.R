#Task 4 - K-means and DBSCAN
#Alex Laris

#Subtask a: Write a purity function
#Credit to online solution I found to guide me: https://github.com/dnguyen7667/DataMiningProjec1/blob/master/code.R

#a is assigments/cluster of objects, b is "ground truth", if a is 0 then it is outlier cluster
#the function returns purity, and vector of purity and percentage of outliers if outliers flag is set to TRUE
purity <- function(a,b,outliers = FALSE){
  
  #create confusion matrix where
  #columns = counts of ground truths in each cluster
  #rows = name of clusters
  c_matrix = table(a,b)
  elements = sum(c_matrix)
  classes = rownames(c_matrix)

  #checking if there is any outliers, then storing that and elimintating from c_matrix
  if (is.element("0", classes)){
    num_outliers = sum(c_matrix[which(rownames(c_matrix) == 0), ])
    #eliminate the row of outliers
    c_matrix = c_matrix[-which(rownames(c_matrix) == 0), ]
  }
  #if no outliers, we set it to 0.
  else
    num_outliers = 0
  
  total_counts = 0 #counts of all max values in each cluster (rows)
  
  #increment total_counts by maximum value in each row/cluster
  nrows = nrow(c_matrix)
  
  #avoid error: argument of length 0 if nrow yields NULL
  if (is.null(nrows))
    nrows = 0
  
  # for the cases later when all we get is outlier class
  if(nrows == 0){
    if (outliers == T)
      return (c(0,0))
    else
      return (0)
  }#end if
  
  
  for (i in 1:nrows)
    total_counts = total_counts + max(c_matrix[i, ])
  
  
  #purity = total_counts / total elements in ground truth w/o outliers (0 cluster)
  purity = total_counts / sum(c_matrix)
  
  #if outliers flag parameter is set to TRUE, then return the vector of purity and percentage of outliers
  if (outliers == TRUE){
    prct_of_outliers = num_outliers / elements
    return (c(purity,prct_of_outliers))
  }
  
  else
    return (purity)
} 
# End of Subtask A ----

#    ---------------------------------------------------

# Start of Sub task B: K-means for Complex 9 data set 

#Loading data set
Complex9 = read.delim("http://www2.cs.uh.edu/~ceick/UDM/DataSets/Complex9.txt", sep=",", header = FALSE)
View(Complex9.features)
#view plot 
plot(Complex9[c("V1","V2")], col = Complex9$V3+1)

#create a new data set that has the same values, then remove the class column (3rd variable)
Complex9.features = Complex9
Complex9.features$V3 <- NULL
View(Complex9.features)

#kmeans9
set.seed(9) #make results reproducible
kmeans9 = kmeans(Complex9.features, 9, nstart = 20)
#view plot of k means = 9
plot(Complex9[c("V1","V2")], main="Complex9 k means = 9", col = kmeans9$cluster)

#kmeans13
set.seed(9) #make results reproducible
kmeans13 = kmeans(Complex9.features, 13, nstart = 20)
#view plot of k means = 13
plot(Complex9[c("V1","V2")], main="Complex9 k means = 13", col = kmeans13$cluster)

#purity
purityk9 = purity(kmeans9$cluster, Complex9$V3)
purityk13 = purity(kmeans13$cluster, Complex9$V3)

cat("The purity of k-means k = 9 is: " , purityk9)
cat("The purity of k-means k = 13 is: " , purityk13)

# End of Sub task B 

#    ---------------------------------------------------

# Start Sub task C 
#load data set
pidd = read.csv("http://www2.cs.uh.edu/~ceick/DM/Pid-clean.csv", header = FALSE)
#make a separate data set where there we eliminate skin thickness and insulin (v4 and v5)
pidd.clean = pidd 
pidd.clean$V4 <- NULL
pidd.clean$V5 <- NULL
#z-scored data set
zpidd = apply(pidd.clean[1:6], MARGIN = 2, FUN = scale)
colnames(zpidd) = c("pregnancies", "glucose", "bloodpressure", "bmi", "diabetespedigree", "age")

#k-means k = 3
set.seed(9) #make results reproducible
kmeans3 = kmeans(zpidd, 3)
#purity
purityk3 = purity(kmeans3$cluster, pidd.clean$V9)
cat("The purity of k-means k = 3 is: " + purityk3)

#create a boxplot for all z-scored values
boxplot(zpidd, main = "Boxplots of all z-score values")

zpidd = cbind(zpidd, kmeans3$cluster)
colnames(zpidd)[7]<- "clas"
zpidd.df = data.frame(zpidd)

#creating the boxplots:
#pregnancies
boxplot(unlist(pregnancies)~unlist(clas), 
       data= zpidd.df, main = "Pregnancies Boxplots Per Cluster", 
       xlab = "Cluster Number", ylab = "Z-Scored values") 
#glucose
boxplot(unlist(glucose)~unlist(clas), 
        data= zpidd.df, main = "Glucose Boxplots Per Cluster", 
        xlab = "Cluster Number", ylab = "Z-Scored values") 
#bloodpressure
boxplot(unlist(bloodpressure)~unlist(clas), 
        data= zpidd.df, main = "Bloodpressure Boxplots Per Cluster", 
        xlab = "Cluster Number", ylab = "Z-Scored values") 
#bmi
boxplot(unlist(bmi)~unlist(clas), 
        data= zpidd.df, main = "Bmi Boxplots Per Cluster", 
        xlab = "Cluster Number", ylab = "Z-Scored values") 
#diabetespedigree
boxplot(unlist(diabetespedigree)~unlist(clas), 
        data= zpidd.df, main = "Diabetespedigree Boxplots Per Cluster", 
        xlab = "Cluster Number", ylab = "Z-Scored values") 
#age
boxplot(unlist(age)~unlist(clas), 
        data= zpidd.df, main = "Age Boxplots Per Cluster", 
        xlab = "Cluster Number", ylab = "Z-Scored values") 

# End of Subtask C

#      ------------------------------------------------------

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

#      ------------------------------------------------------

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