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