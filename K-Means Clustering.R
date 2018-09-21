#first read-in data#
mydata = Retail_bank_online_banking_attitudes_survey
mydata[1] <- NULL 

# Prepare Data #
mydata <- na.omit(mydata) # listwise deletion of missing
mydata.orig = mydata #save orig data copy
mydata <- scale(mydata) # standardize variables

# Scree plot to determine number of factors to extract
# install.packages("nFactors") 
library(nFactors)
ev <- eigen(cor(mydata)) 
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)

# Perform factor analysis
res1a = factanal(mydata, factors = 9, rotation = "varimax", scores="regression")
res1a

# Calculate factor scores
mydata2= res1a$scores

# Loop through factors and sort loading scores in ascending order
mylist=list(list())
for (i in 1:9)
   {
     v=sort(res1a$loadings[,i])
     mylist[[i]]=round(v,2)
     }

# k-means #
# Determine number of clusters #
wss <- (nrow(mydata2)-1)*sum(apply(mydata2,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata2,centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# Look for an "elbow" in the scree plot #

# K-Means Cluster Analysis
k1=6
fit <- kmeans(mydata2, k1) # k1 cluster solution

# get cluster means for factors
output = aggregate(mydata2,by=list(fit$cluster),FUN=mean)

# write cluster means to tab delimited file
write.table(output, "C:/Users/Ansar/Desktop/output2.txt", sep = "\t")


