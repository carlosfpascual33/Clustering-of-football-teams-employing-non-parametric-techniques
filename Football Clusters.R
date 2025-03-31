###Exercise 3.22

#get the necessary packages

packages<-c("ks", "viridis", "readxl", "dplyr")

lapply(packages, library, character.only = TRUE)

#read the data

data<-read_excel("C:/Users/USUARIO/Documents/Universidad/Máster/Segundo cuatrimestre/Tercer semicuatrimestre/Non Parametric Inference/la-liga-2015-2016.xlsx")

## a)How many clusters are detected for the variables Yellow.cards
#and Red.cards? And for Yellow.cards, Red.cards, and Fouls.made?
#Interpret the results.

#Get the clusters for the variables Yellow Cards and Red Cards
var<-c("Yellow.cards", "Red.cards")
data1<-data[,var]

kms <- ks::kms(x = data1)

# Number of clusters found
kms$nclust # 6

# Sizes of clusters
kms$nclust.table # 1 2 11 2 3 1

#Plot the result
plot(kms, pch=19, col = viridis::viridis(kms$nclust, option="turbo"), xlab = "Yellow Cards", ylab = "Red Cards", main="Clusters for two variables ")


#Get the same result for three variables

var2<-c("Yellow.cards", "Red.cards", "Fouls.made")
data2<-data[,var2]

kms2 <- ks::kms(x = data2)

# Number of clusters found
kms2$nclust # 3

# Sizes of clusters
kms2$nclust.table # 1 1 18

#Plot the result
plot(kms2, pch=19, col = viridis::viridis(kms2$nclust, option="turbo"))
################################################################################

##b) Standardize the previous variables (divide them by their standard deviation) and 
#recompute a. 
#Are the results the same? Why? Does kmeans have a similar behavior?

#First, let's do it for two variables
#Standardize the data
data1_st <- as.data.frame(lapply(data1, function(x) x / sd(x)))

#Obtain the clusters
kms_st <- ks::kms(x = data1_st)

# Number of clusters found
kms_st$nclust # 6

# Sizes of clusters
kms_st$nclust.table # 1 2 11 2 3 1

#Results are the same

#Now, let's see if the same happens for the case with three variables

data2_st <- as.data.frame(lapply(data2, function(x) x / sd(x)))

kms2_st <- ks::kms(x = data2_st)

# Number of clusters found
kms2_st$nclust #3

# Sizes of clusters
kms2_st$nclust.table # 1 1 18

#The results are again the same

#Let's check now whether kmenas behaves similarly

#Case of two variables without standardizing

km<-kmeans(data1,centers=6) #we select the same number of clusters found for the kms

#Size of the clusters
km$size

#Case of two variables standardinzing

km_st<-kmeans(data1_st,centers=6)

#Size of the clusters
km_st$size

#In this case, the clusters are different!

#Let's plot the results for both clusters

#Get the label of the individuals depending on their cluster
data1$Cluster<-as.factor(km$cluster)

colors<-rainbow(6) #select a scale of colors

plot(data1$Yellow.cards, data1$Red.cards, col = colors[data1$Cluster], pch = 19, main = "K-Means original data", xlab = "Yellow Cards", 
     ylab = "Red Cards")

data1_st$Cluster<-as.factor(km_st$cluster)

colors_st<-rainbow(6) #select a scale of colors

plot(data1_st$Yellow.cards, data1_st$Red.cards, col = colors[data1_st$Cluster], pch = 19, main = "K-Means standardized data", xlab = "Yellow Cards", 
     ylab = "Red Cards")

#Let's check also the case with three variables

km2<-kmeans(data2,centers=3) #we select the same number of clusters found for the kms

#Size of the clusters
km2$size

#Now with the standardized data

km_st2<-kmeans(data2_st,centers=3)

#Size of the clusters
km_st2$size

#The clusters also change in this case

#Let's see the results graphically

#First, the original data

data2$Cluster<-as.factor(km2$cluster)

colors2<-rainbow(3)

# We plot all variables 2 by 2
par(mfrow = c(2, 2))

plot(data2$Yellow.cards, data2$Red.cards, 
     col = colors2[data2$Cluster], 
     pch = 19,
     main = "Yellow cards vs Red cards",
     xlab = "Yellow cards",
     ylab = "Red cards")

plot(data2$Yellow.cards, data2$Fouls.made, 
     col = colors2[data2$Cluster], 
     pch = 19,
     main = "Yellow cards vs Fouls made",
     xlab = "Yellow cards",
     ylab = "Fouls made")

plot(data2$Red.cards, data2$Fouls.made, 
     col = colors2[data2$Cluster], 
     pch = 19,
     main = "Red cards vs Fouls made",
     xlab = "Red Cards",
     ylab = "Fouls made")

#Get the legend in the free space

plot.new()  
title("Cluster Legend")  
legend("center", 
       legend = paste("Cluster", 1:3), 
       col = colors2, 
       pch = 19, 
       bty = "n", 
       cex = 1.2) 

#Now, the standardized case

data2_st$Cluster<-as.factor(km_st2$cluster)


par(mfrow = c(2, 2))


plot(data2_st$Yellow.cards, data2$Red.cards, 
     col = colors2[data2_st$Cluster], 
     pch = 19,
     main = "Yellow cards vs Red cards",
     xlab = "Yellow cards",
     ylab = "Red cards")


plot(data2_st$Yellow.cards, data2$Fouls.made, 
     col = colors2[data2_st$Cluster], 
     pch = 19,
     main = "Yellow cards vs Fouls made",
     xlab = "Yellow cards",
     ylab = "Fouls made")


plot(data2$Red.cards, data2$Fouls.made, 
     col = colors2[data2_st$Cluster], 
     pch = 19,
     main = "Red cards vs Fouls made",
     xlab = "Red Cards",
     ylab = "Fouls made")


plot.new()  
title("Cluster Legend")  
legend("center", 
       legend = paste("Cluster", 1:3), 
       col = colors2, 
       pch = 19, 
       bty = "n",   
       cex = 1.2)   
################################################################################

##c)Run a PCA on the dataset after removing Points and Matches and standardizing the variables. 
#Then perform a clustering on the scores of as many PCs as to explain the 85% of the variance.
#How many clusters are detected? What teams are associated with each of them?
#Are the clusters interpretable? Do you see something strange?

#Get the variables we want
data_pc <- data %>% select(-"Team", -"Matches", -"Points")

#Standardize the data
data_pc<- as.data.frame(lapply(data_pc, function(x) x / sd(x)))

#Obtain the principal components of the data
pc<-princomp(data_pc)

summary(pc) #3 principal components explain 85.63% of the variance

#Obtain the values for the individuals for the three first principal components
pcs<-pc$scores[,1:3]

#Obtain the clusters
kms_pc <- ks::kms(x = pcs)

# Number of clusters found
kms_pc$nclust # 4

# Sizes of clusters
kms_pc$nclust.table # 2 1 16 1

#Get the teams associated with each cluster

PC_clusters<-data.frame(Position=1:nrow(data), Team=positions, Cluster=kms_pc$label)

print(PC_clusters) 
##################################################################################

##d) Run kmeans on the data used in c with k=3,4,5 and compare the results graphically.

#Define a function to compute the k-means algorithm depending on the number of clusters
km_pc<-function(k){
  set.seed(170225) #set a seed to ensure reproducibility
  datak<-as.data.frame(pcs) #we want to use the pcs from the previous section
  
  kmk<-kmeans(datak,centers=k) #get the clusters
  
  #Size of the clusters
  size<-kmk$size # 8 2 10
  
  datak$Cluster<-as.factor(kmk$cluster)
  
  #Get a dataframe with the clusters associated to each team
  results<-data.frame(Position=1:nrow(datak), Team=positions, Cluster=datak$Cluster)
  
  #Plot the results
  colorsk<-rainbow(k)
  
  par(mfrow = c(2, 2))
  
  
  plot(datak$Comp.1, datak$Comp.2, 
       col = colorsk[datak$Cluster], 
       pch = 19,
       main = paste("k= ", k),
       xlab = "PC1",
       ylab = "PC2")
  
  
  plot(datak$Comp.2, datak$Comp.3, 
       col = colorsk[datak$Cluster], 
       pch = 19,
       main = paste("k= ", k),
       xlab = "PC1",
       ylab = "PC3")
  
  
  plot(datak$Comp.2, datak$Comp.3, 
       col = colorsk[datak$Cluster], 
       pch = 19,
       main = paste("k= ", k),
       xlab = "PC2",
       ylab = "PC3")
  
  
  plot.new()  
  legend("center", 
         legend = paste("Cluster", 1:k), 
         col = colorsk, 
         pch = 19, 
         bty = "n",   
         cex = 1,
         xpd=TRUE)   
  return(list(sizes=paste("Size of the clusters:", paste(size, collapse = " ")), Results=results))
}

#Results for k=3
km_pc(3)

#Results for k=4
km_pc(4)

#Results for k=5
km_pc(5)

