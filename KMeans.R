library(reshape2)
library(cluster)
#library(rgl)
#library(fpc)
library(plot3D)
library(plotly)
library(reshape2)
library(ggplot2)
library(car)
###############################################################################################
# digits for clustering

digit1 <- 1
digit2 <- 5
digit3 <- 7

# Value of K 
K <- 3

#number of Principal Components to be used
NPCs <- 5

#number of iterations ot run for KMeans
n_iter <- 15

#############################################################################################
# Read the data

f_read <- read.csv("C:/Users/YVASKA/Desktop/Git_Prj/mnist_train.csv")
m_read <- data.matrix(f_read) ; dim(m_read)

X0 <- subset(m_read, m_read[,1] == digit1 | m_read[,1] == digit2 | m_read[,1] == digit3)
#XRsf <- X0[sample(nrow(X0)),] 
##############################################################################################

# Calculate the PCA

PCA_res <- prcomp(X0 [,-1], center = TRUE, scale = FALSE)

# Reduced PCAs
XR <- data.frame(predict(PCA_res, newdata = X0[,-1])[,1:NPCs])

############################################################################################
# Check the Variance explained by the PCAs
variance_explained = PCA_res$sdev^2 / sum(PCA_res$sdev^2)
XNew <- cbind(XR, X0[,1])

# plot percentage of variance explained for each principal component    
barplot(100*variance_explained[1:15], las=2, xlab='', ylab='% Variance Explained')

ggplot(XR, aes(x=XNew$PC1)) + geom_density(aes(group=XNew$`X0[, 1]`, colour=XNew$`X0[, 1]`, fill=XNew$`X0[, 1]`), alpha=0.3)

###################################################################################################
# Visualize the PCAs 

df.m <- melt(XR)
ggplot(df.m, aes(depth, fill = variable, colour = variable)) + geom_density(aes(x = value, y = ..density.., colour = variable),alpha = 0.20) + theme_bw()


  
#####################################################################################################

# KMeans Cluster


N <- dim(XR)[1]
d <- dim(XR)[2]

#U[k] <- random(min(X[j]) Max(X[j]))
U <- matrix(nrow = K,ncol = d)
Unew <- matrix(0,nrow = K,ncol = d)
for (k in 1:K){
    for(j in 1:d)
      {
        
      U[k,j] <- runif(1,min(XR[,j]):max(XR[,j]),1)
    }
}
Unew <- data.frame(Unew)
U    <- data.frame(U)


# Class labels of size N
C <-  matrix(0,nrow = N,ncol = 1)
C <- data.frame(C)


dist <- matrix(0,nrow=K, ncol=d)
dist <- data.frame(dist)
Meansum <- data.frame(matrix(0,nrow=N, ncol=K))


for (iter in 1:n_iter)
{
  
    for(i in 1:N)
      {
        for(k in 1:K)
          {
            for(j in 1:d)
              {
              dist[k,j] <- (XR[i,j]-U[k,j])^2
              }
              Meansum[i,k] <- sum(dist[k,])
        
          }
        Meansum$Class[i] <- which.min(Meansum[i,1:K])
      }
    Xmerg <- XR
    Xmerg$Class <- Meansum$Class
    for(k in 1:K)
    {

    Unew[k,] <- colMeans(Xmerg[which(Xmerg$Class==k),1:NPCs])
    }
  Error = max(U-Unew); print(Error)
  U <- Unew
}


finalRes <- data.frame(XRsf[,1])
finalRes$Predicted <-Xmerg$Class

m <- kmeans(XR, centers=K)
finalRes$RKm <- m$cluster
finalRes <- data.frame(finalRes)

####################################################################################################
#Plot Clusters

cluster::clusplot(Xmerg[,1:2], Xmerg$Class, color=TRUE, shade=FALSE, labels=0, lines=2)

plot3d(Xmerg[,1:3], col=Xmerg$Class,size=5)

scatter3d(Xmerg[,1],Xmerg[,2],Xmerg[,3], grid = FALSE, surface = FALSE,ellipsoid = FALSE,groups = factor(finalRes$XRsf...1.),surface.col = c("RED", "BLUE", "GREEN"))
#####################################################################################################