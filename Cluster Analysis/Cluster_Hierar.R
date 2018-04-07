# Prepare Data
mydata <- na.omit(mtcars) # listwise deletion of missing
mydata <- scale(mtcars) # standardize variables

# Ward Hierarchical Clustering
d <- dist(mtcars, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

# Ward Hierarchical Clustering with Bootstrapped p values
install.packages("pvclust")
library(pvclust)

mydata <- t(mtcars)
fit <- pvclust(mydata, method.hclust="ward",
               method.dist="euclidean")
plot(fit) # dendogram with p values
# add rectangles around groups highly supported by the data
pvrect(fit, alpha=.95)

