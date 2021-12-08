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