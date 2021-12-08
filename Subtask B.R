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