# Practical Problem Set I
# Name - Preet Paul
# Registration number - 23414110019



# Problem 1
# Loading the dataset

Concrete <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\3rd Semester\\Concrete.csv")

View(Concrete)
head(Concrete,5)

# Checking for any missing value present in the dataset

which(is.na(Concrete))

# Using the covariance matrix S
#  Principal Component Analysis

S <- cov(Concrete)
S

model <- prcomp(S)	# prcomp stands for principal component analysis

# Summary of the model


summary1 <- summary(model)
summary1


# Display the Loadings of the variables on the principal components


model$rotation


# Applying screeplot in the model

library("MASS")
plot(model,xlab="Principal Components")
screeplot(model, type="l", main="Scree plot")
# npcs stands for number of principal components
screeplot(model, npcs=5, type="l", main="Scree plot with PC=5")	


# No. of variables which explains 90% of the total variation

var_proportion1 <- summary1$importance["Cumulative Proportion", ]
var_proportion1

# Hence, there are 3 principal components which explains atleast 90% of the variation.

# Applying biplot in the model 


require(graphics)
biplot(prcomp(S), scale = TRUE, col=c("green","blue3"), 
       main="Biplot of the Principal Components",xlab="PC1 score", cex=c(0.5,0.5),
       ylab="PC2 score",xlim=c(-1,1))


# Using the correlation matrix R
#  Principal Component Analysis


R <- cor(Concrete)
R

result <- prcomp(R)	# prcomp stands for principal component analysis

# Summary of the model

summary2 <- summary(result)
summary2


# Display the Loadings of the variables on the principal components


result$rotation


# Applying screeplot in the model

library("MASS")
plot(result,xlab="Principal Components")
screeplot(result, type="l", main="Scree plot")
# npcs stands for number of principal components
screeplot(result, npcs=5, type="l", main="Scree plot with PC=5")	


### No. of variables which explains 90% of the total variation

var_proportion2 <- summary2$importance["Cumulative Proportion", ]
var_proportion2

# Hence, there are 5 principal components which explains atleast 90% of the variation.

# Applying biplot in the model 

require(graphics)
biplot(prcomp(R), scale = TRUE, col=c("green","blue3"), 
       main="Biplot of the Principal Components",xlab="PC1 score",cex=c(0.5,0.5),
       ylab="PC2 score",xlim=c(-1,1))

#########################################################################################

# Problem 2
# Loading the dataset

wine <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\3rd Semester\\winequality-red.csv",sep=";")

View(wine)
head(wine,5)


# Finding the optimal number of factors using prcomp() and screeplot


prin <- prcomp(wine)
screeplot(prin, type="l",main="Screeplot")


# Clearly, we see that the slope is changing after the 3rd component. So, the optimal number of factors is 3.

# Performing Factor Analysis

result2 <- factanal(x=wine, factors=3, rotation="varimax")
result2

# Interpretation to the factors for the best model


result2$loadings

# This shows there is high loading in Factor 1 and then of Factor2.

# Generating biplot for the analysis

require(graphics)
biplot(prin, scale = TRUE, col=c("yellow","blue4"),cex=c(0.7,0.7))   


# This biplot suggests that there is high correlation between each of the variables and less contribution in PC1 scores.

#####################################################################################################

# Problem 3
# Loading the dataset

Pottery <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\3rd Semester\\Pottery.csv")
View(Pottery)
head(Pottery,5)


# Creating a new dataframe after eliminating the index column "x"

data <- Pottery[-c(1)]
head(data,5)


# Creating the Distance Matrix

DM <- dist(data)
DM


# Performing metric multidimensional scaling

library(MASS)
result3 <- cmdscale(DM, k=2, eig=T)
result3


# Creating 2-D MDS plot

x <- result3$points[,1]
y <- result3$points[,2]

plot(x, y, type="n",xlab="MDS Dimension 1",ylab="MDS Dimension 2",
     main="Metric MDS plot")
points(x, y,lty=2, col="blue")
text(x, y, cex=0.7, pos=2)


# Modifying the plot using the given information for different kiln

kiln <- c(rep("red",21),rep("blue",12),rep("yellow",2),rep("purple",5),rep("green",5))
plot(x, y, type="p",xlab="MDS Dimension 1",ylab="MDS Dimension 2",
     main="Metric MDS plot",col=kiln)
text(x, y, pos=2, cex=0.7)

# Modifying the plot using the given information for different regions

region <- c(rep("red",21),rep("blue",14),rep("orange",10))
plot(x, y, type="p",xlab="MDS Dimension 1",ylab="MDS Dimension 2",
     main="Metric MDS plot",col=region)
text(x, y, pos=2, cex=0.7)

####################################################################################################

# Problem 4
# Loading the dataset


garden <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\3rd Semester\\Garden.csv")
head(garden,5)
View(garden)


# Creating new dataset after eliminating the 1st column

data1 <- garden[-c(1)]

# Performing Non-metric MDS

library(MASS)
DM1 <- dist(data1)  #Distance matrix of the dataset
y <- cmdscale(data1, k=2)
result4 <- isoMDS(DM1, y, k=2)
result4

# Creating 2-D MDS plot 


x1 <- result4$points[,1]
y1 <- result4$points[,2]

plot(x1, y1, type="n",xlab="MDS Dimension 1", ylab="MDS Dimension 2",
     main="Non-metric MDS Plot")
points(x1, y1, col="red")
text(x1, y1, cex=0.7, pos=2)


# Obtaining Kruskal's stress value

cat("Kruskal's stress value is",result4$stress)

###################################################################################################

# Problem 5
# Loading the dataset

fitness <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\3rd Semester\\Fitness.csv")
head(fitness,5)
View(fitness)

# Creating new dataset after eliminating the 1st column

data2 <- fitness[-c(1)]

# Conducting any Test of Significance
# Performing Pearsonian Chi-square test of significance

physiological <- data2[,c(1,2,3)]
exercise <- data2[,c(4,5,6)]

chisq.test(physiological, exercise)
# From the above, we clearly see that p-value is greater than 0.05. S, we can say that the two sets of variables are dependent to one another.

# Obtaining the Correlation matrix of 6 variables

R1 <- cor(data2)
R1

# Performing Canonical Correlation Analysis

cca <- cancor(physiological, exercise)
cca

# Canonical coefficient of physiological

cca$xcoef

# Canonical coefficient of exercise

cca$ycoef

# Canonical Variates

phys1 <- as.matrix(physiological) %*% cca$xcoef[,1]
phys2 <- as.matrix(physiological) %*% cca$xcoef[,2]
phys3 <- as.matrix(physiological) %*% cca$xcoef[,3]
exer1 <- as.matrix(exercise) %*% cca$ycoef[,1]
exer2 <- as.matrix(exercise) %*% cca$ycoef[,2]
exer3 <- as.matrix(exercise) %*% cca$ycoef[,3]

# Correlation between Physiological variables & their Canonical covariates

cor(physiological, cbind(phys1, phys2, phys3))

# Correlation between Exercise variables & their Canonical covariates

cor(exercise, cbind(exer1, exer2, exer3))

# Correlation between Exercise variables &  Canonical covariates of Physiological variable

cor(exercise, cbind(phys1, phys2, phys3))

# Correlation between Physiological variables &  Canonical covariates of Exercise variable

cor(physiological, cbind(exer1, exer2, exer3))


