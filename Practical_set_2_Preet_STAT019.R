
# PRACTICAL PROBLEM SET 2

# Problem 1

nutrition <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\3rd Semester\\nutrient_data.csv")
head(nutrition)
View(nutrition)

# Eliminating the index column from our dataset

nutrition <- nutrition[,-c(1)]

# Calculating the mean of each columns in the dataset

avg = apply(nutrition, MARGIN = 2, FUN = mean)
avg

S = var(nutrition)  ## Sample Var-Cov matrix
S

mu = c(1000,15,60,800,75)  ## Specified value of mean  under null 

# Loading the package "ICSNP"
library(ICSNP)

## We use the built-in R function HotellingT2() under the library "ICSNP" to
## perform Hotelling T^2 test.

# Performing Hotelling T^2 test

test = HotellingsT2(nutrition, mu=mu, test="f")
test

## Thus, we see that as the p-value for the test is very small (< 2.2e-16),
## H0 is rejected at 1% level of significance.

# Finding the Simultaneous confidence intervals for the mean intake

n = nrow(nutrition)
p = ncol(nutrition)
attach(nutrition)

# Simultaneous confidence interval for calcium
# Here qf() stands for the quantile function of F-distribution

ci.calcium = c(mean(calcium)-(sqrt(p*(n-1)*var(calcium))*qf(0.99, p, (n-p))/sqrt(n*(n-p))),
               mean(calcium)+(sqrt(p*(n-1)*var(calcium))*qf(0.99, p, (n-p))/sqrt(n*(n-p))))

# Simultaneous confidence interval for iron
ci.iron = c(mean(iron)-(sqrt(p*(n-1)*var(iron))*qf(0.99, p, (n-p))/sqrt(n*(n-p))),
            mean(iron)+(sqrt(p*(n-1)*var(iron))*qf(0.99, p, (n-p))/sqrt(n*(n-p))))

# Simultaneous confidence interval for protein
ci.protein = c(mean(protein)-(sqrt(p*(n-1)*var(protein))*qf(0.99, p, (n-p))/sqrt(n*(n-p))),
               mean(protein)+(sqrt(p*(n-1)*var(protein))*qf(0.99, p, (n-p))/sqrt(n*(n-p))))

# Simultaneous confidence interval for Vitamin A
ci.a = c(mean(nutrition$a)-(sqrt(p*(n-1)*var(nutrition$a))*qf(0.99, p, (n-p))/sqrt(n*(n-p))),
         mean(nutrition$a)+(sqrt(p*(n-1)*var(nutrition$a))*qf(0.99, p, (n-p))/sqrt(n*(n-p))))

# Simultaneous confidence interval for Vitamin C
ci.c = c(mean(c)-(sqrt(p*(n-1)*var(c))*qf(0.99, p, (n-p))/sqrt(n*(n-p))),
         mean(c)+(sqrt(p*(n-1)*var(c))*qf(0.99, p, (n-p))/sqrt(n*(n-p))))

# Showing the all the outputs as a list

confidence <- list(ci.calcium,ci.iron,ci.protein,ci.a,ci.c)
names(confidence) <- c("ci.calcium","ci.iron","ci.protein","ci.a","ci.c")
confidence
     

# Plot the confidence interval and confidence ellipsoid

library(ellipse)
plot(ellipse(cor(calcium,iron)),type="l",xlab=expression(mu[calcium]),
     ylab=expression(mu[iron]),main="Confidence ellipse of Calcium and Iron")

# Finding the Bonferroni confidence intervals for the mean intake
a = 0.01

# Bonferroni confidence interval for calcium
bon.calcium = c(mean(calcium)-(sqrt(var(calcium))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)),
                mean(calcium)+(sqrt(var(calcium))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)))

# Bonferroni confidence interval for iron
bon.iron = c(mean(iron)-(sqrt(var(iron))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)),
             mean(iron)+(sqrt(var(iron))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)))

# Bonferroni confidence interval for protein
bon.protein = c(mean(protein)-(sqrt(var(protein))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)),
                mean(protein)+(sqrt(var(protein))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)))

# Bonferroni confidence interval for Vitamin A
bon.a = c(mean(nutrition$a)-(sqrt(var(nutrition$a))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)),
          mean(nutrition$a)+(sqrt(var(nutrition$a))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)))

# Bonferroni confidence interval for Vitamin C
bon.c = c(mean(c)-(sqrt(var(c))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)),
          mean(c)+(sqrt(var(c))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)))

# Showing the all the outputs as a list

bonferroni.confidence <- list(bon.calcium,bon.iron,bon.protein,bon.a,bon.c)
names(bonferroni.confidence) <- c("bon.calcium","bon.iron","bon.protein","bon.a","bon.c")
bonferroni.confidence

# Generating Profile Plots 

## Loading the package
library(plotrix)

# Standardizing each of the observations by dividing them by their hypothesized means

Z <- nutrition/mu
View(Z)

# Finding the means of the standardized variables
avg_new <- apply(Z,2, FUN = mean)
avg_new

# Generating Profile Plots for Simultaneous confidence intervals

sim_ci <- data.frame(c(ci.calcium[1],ci.calcium[2]),c(ci.iron[1],ci.iron[2]),c(ci.protein[1],ci.protein[2]),
             c(ci.a[1],ci.a[2]),c(ci.c[1],ci.c[2]))
colnames(sim_ci) <- c("calcium","iron","protein","a","c")
rownames(sim_ci) <- c("lower bound","upper bound")
sim_ci

# Standardizing the simultaneous confidence intervals

sim_std <- data.frame(sim_ci$calcium/1000,sim_ci$iron/15,sim_ci$protein/60,sim_ci$a/800,sim_ci$c/75)
colnames(sim_std) <- c("calcium","iron","protein","a","c")
sim_std

Result <- apply(sim_std,2, FUN = mean)
Result

#Profile Plot
plotCI(x=c(1:5), y=Result, li=sim_std[1,], ui=sim_std[2,],xlab="Nurients",
       ylab="Ratio",main="Profile plot of nutrients for Simultaneous confidence intervals")
abline(h=1)

# Generating Profile Plots for Bonferroni confidence intervals

bon_ci <- data.frame(c(bon.calcium[1],bon.calcium[2]),c(bon.iron[1],bon.iron[2]),c(bon.protein[1],bon.protein[2]),
                      c(bon.a[1],bon.a[2]),c(bon.c[1],bon.c[2]))
colnames(bon_ci) <- c("calcium","iron","protein","a","c")
rownames(bon_ci) <- c("lower bound","upper bound")
bon_ci

# Standardizing the simultaneous confidence intervals

bon_std <- data.frame(bon_ci$calcium/1000,bon_ci$iron/15,bon_ci$protein/60,bon_ci$a/800,bon_ci$c/75)
colnames(bon_std) <- c("calcium","iron","protein","a","c")
bon_std

Result1 <- apply(bon_std,2, FUN = mean)
Result1

#Profile Plot
plotCI(x=c(1:5), y=Result1, li=bon_std[1,], ui=bon_std[2,],xlab="Nurients",
       ylab="Ratio",main="Profile plot of nutrients for Bonferroni confidence intervals")
abline(h=1)

##########################################################################################

# Problem 2

# Loading the dataset

shoe <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\3rd Semester\\shoe.csv")
head(shoe)
View(shoe)
which(is.na(shoe))  #Checking for missing values

shoe <- shoe[-c(1),]
colnames(shoe) <- shoe[c(1),]  # Giving column names to the dataset
shoe <- shoe[-c(1),-c(1)]
View(shoe)

## Subsetting Model1 and Model2 from the dataset

Model1 <- data.matrix(shoe[,c(1:5)])
View(Model1)
Model2 <- data.matrix(shoe[,c(7:11)])
View(Model2)

which(is.na(Model1))
which(is.na(Model2))

# Part (a)

Y <- Model1 - Model2

# Calculating the means of the variables of Y
Y_avg <- apply(Y,2, FUN = mean)
Y_avg

# Performing Hotelling's  T^2 test

HotellingsT2(Y, test="f")

## Since the p-value is 0.01497, we reject our null hypothesis H0 at 5% level of significance.
## Thus, we can conclude that there is a significant difference between two show models at 5% level of significance.

# Calculating Simultaneous confidence intervals

Y <- as.data.frame(Y)
n = nrow(Y)
p = ncol(Y)
f.value <- qf(0.95, p, (n-p))
a = 0.05

ci.style <- c(mean(Y$Style)-(sqrt(p*(n-1)*var(Y$Style))*qf((1-a), p, (n-p))/sqrt(n*(n-p))),
              mean(Y$Style)+(sqrt(p*(n-1)*var(Y$Style))*qf((1-a), p, (n-p))/sqrt(n*(n-p))))

ci.comfort <- c(mean(Y$Comfort)-(sqrt(p*(n-1)*var(Y$Comfort))*qf((1-a), p, (n-p))/sqrt(n*(n-p))),
                mean(Y$Comfort)+(sqrt(p*(n-1)*var(Y$Comfort))*qf((1-a), p, (n-p))/sqrt(n*(n-p))))

ci.stability <- c(mean(Y$Stability)-(sqrt(p*(n-1)*var(Y$Stability))*qf((1-a), p, (n-p))/sqrt(n*(n-p))),
                  mean(Y$Stability)+(sqrt(p*(n-1)*var(Y$Stability))*qf((1-a), p, (n-p))/sqrt(n*(n-p))))

ci.cushion <- c(mean(Y$Cushion)-(sqrt(p*(n-1)*var(Y$Cushion))*qf((1-a), p, (n-p))/sqrt(n*(n-p))),
                mean(Y$Cushion)+(sqrt(p*(n-1)*var(Y$Cushion))*qf((1-a), p, (n-p))/sqrt(n*(n-p))))

ci.durability <- c(mean(Y$Durability)-(sqrt(p*(n-1)*var(Y$Durability))*qf((1-a), p, (n-p))/sqrt(n*(n-p))),
                   mean(Y$Durability)+(sqrt(p*(n-1)*var(Y$Durability))*qf((1-a), p, (n-p))/sqrt(n*(n-p))))


# Showing the all the outputs as a list

confidence1 <- list(ci.style,ci.comfort,ci.stability,ci.cushion,ci.durability)
names(confidence1) <- c("ci.style","ci.comfort","ci.stability","ci.cushion","ci.durability")
confidence1

# Calculating Bonferroni confidence intervals


bon.style <- c(mean(Y$Style)-(sqrt(var(Y$Style))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)),
                mean(Y$Style)+(sqrt(var(Y$Style))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)))

bon.comfort <- c(mean(Y$Comfort)-(sqrt(var(Y$Comfort))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)),
                 mean(Y$Comfort)+(sqrt(var(Y$Comfort))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)))

bon.stability <- c(mean(Y$Stability)-(sqrt(var(Y$Stability))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)),
                   mean(Y$Stability)+(sqrt(var(Y$Stability))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)))

bon.cushion <- c(mean(Y$Cushion)-(sqrt(var(Y$Cushion))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)),
                 mean(Y$Cushion)+(sqrt(var(Y$Cushion))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)))

bon.durability <- c(mean(Y$Durability)-(sqrt(var(Y$Durability))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)),
                    mean(Y$Durability)+(sqrt(var(Y$Durability))*qt(1-(a/(2*p)), df=n-1)/sqrt(n)))

# Showing the all the outputs as a list

bonferroni.confidence1 <- list(bon.style,bon.comfort,bon.stability,bon.cushion,bon.durability)
names(bonferroni.confidence1) <- c("bon.style","bon.comfort","bon.stability","bon.cushion","bon.durability")
bonferroni.confidence1

# Generating Profile Plots for Bonferroni confidence intervals

## Loading the package
library(plotrix)

bon_ci1 <- data.frame(c(bon.style[1],bon.style[2]),c(bon.comfort[1],bon.comfort[2]),c(bon.stability[1],bon.stability[2]),
                      c(bon.cushion[1],bon.cushion[2]),c(bon.durability[1],bon.durability[2]))
colnames(bon_ci1) <- c("style","comfort","stability","cushion","durability")
rownames(bon_ci1) <- c("lower bound","upper bound")
bon_ci1

Result2 <- apply(bon_ci1,2, FUN = mean)
Result2

#Profile Plot
plotCI(x=c(1:5), y=Result2, li=bon_ci1[1,], ui=bon_ci1[2,],xlab="Components",
       ylab="Ratio",main="Profile plot of Components of shoes for Bonferroni confidence intervals")
abline(h=0)

# Generating Profile Plots for simultaneous confidence intervals

sim_ci1 <- data.frame(c(ci.style[1],ci.style[2]),c(ci.comfort[1],ci.comfort[2]),c(ci.stability[1],ci.stability[2]),
                      c(ci.cushion[1],ci.cushion[2]),c(ci.durability[1],ci.durability[2]))
colnames(sim_ci1) <- c("style","comfort","stability","cushion","durability")
rownames(sim_ci1) <- c("lower bound","upper bound")
sim_ci1

Result3 <- apply(sim_ci1,2, FUN = mean)
Result3

#Profile Plot
plotCI(x=c(1:5), y=Result3, li=sim_ci1[1,], ui=sim_ci1[2,],xlab="Components",
       ylab="Ratio",main="Profile plot of Components of shoes for Simultaneous confidence intervals")
abline(h=0)

## So, from the profile plot of bonferroni confidence interval, we see that style has a significant difference.

########################################################################################################

# Problem 3

# Loading the dataset

data1 <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\3rd Semester\\drug.csv")
head(data1)
View(data1)

which(is.na(data1))  # Checking for missing values
data1 <- data1[-c(1),]  # Eliminating the empty row

colnames(data1) <- data1[c(1),]  # Renaming the columns of the dataset
data1 <- data1[-c(1),]
View(data1)

# Subsetting the dataset into Drug and Placebo

drug <- data1[,c(1:3)]
placebo <- data1[,c(5:7)]
drug
View(placebo)
placebo <- placebo[-c(19,20),]
placebo

drug$Fever <- as.numeric(drug$Fever)
drug$Pressure <- as.numeric(drug$Pressure)
drug$Aches <- as.numeric(drug$Aches)
drug

placebo$Fever <- as.numeric(placebo$Fever)
placebo$Pressure <- as.numeric(placebo$Pressure)
placebo$Aches <- as.numeric(placebo$Aches)
placebo

# Part (a) - Population covariance matrices are equal

# Performing Hotelling T^2 test

test <- HotellingsT2(drug, placebo)
test

# Since the p-value is very small, we reject the null hypothesis H0 at 5% level of significance.

# Part (b) - Population covariance matrices are unequal 

a = 0.05
p = ncol(drug)
n1 = nrow(drug)
n2 = ncol(placebo)

# mean of drug and placebo
x1.bar = apply(drug,2, FUN = mean)
x2.bar = apply(placebo,2, FUN = mean)

# covariance matrices of drug and placebo
S1 = cov(drug)
S2 = cov(placebo)

# pooled covariance matrix
Sp = ((n1 - 1)*S1 + (n2 - 1)*S2)/(n1+n2-2) 

# Obtaining Hotelling T^2 test statistic

T2 = t(x1.bar - x2.bar) %*% solve(Sp*((1/n1)+(1/n2))) %*% (x1.bar - x2.bar)
T2
F.statistic = T2*(n1+n2-p-1)/(p*(n1+n2-2))
F.statistic  # Obtaining the F-statistic

#critical point at 5% level of significance
critical = qf((1-a), p, n1+n2-p-1)
critical

# Rejection of Null hypothesis 
F.statistic > critical

## Hence, we reject the null  hypothesis H0 at 5% level of significance
## Therefore, we see that the drug is effective reducing at reducing these three symptoms.

##################################################################################################

# Problem 4

# Loading the dataset

soil <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\3rd Semester\\soil.csv")
head(soil)
View(soil)
which(is.na(soil))  # Checking for missing values
names(soil)[1] <- "Soil.Type"

# Performing MANOVA on the dataset

model <- manova(cbind(yield, water, herbicide)~Soil.Type, data = soil)
model

# SUmmary of the model

summary(model, test="Wilks")   # Wilk's lambda test
summary(model, test="Pillai")  # The Pillai test
summary(model, test="Roy")     # Roy's Union-Intersection test
summary(model, test="Hotelling-Lawley")  # Lawley-Hotelling test

## From the above 4 test, we reject the null hypothesis H0 at 5% level of significance.
## Thus, there is significant difference of 4 levels of soil type.

# Performing tests on clay and salty groups

data2 <- soil[which(soil$Soil.Type == "clay") | which(soil$Soil.Type == "salty"), ]
data2
model2 <- manova(cbind(yield,water,herbicide) ~ Soil.Type, data = data2)
model2

# Performing the tests

summary(model2, test="Wilks")   # Wilk's lambda test
summary(model2, test="Pillai")  # The Pillai test
summary(model2, test="Roy")     # Roy's Union-Intersection test
summary(model2, test="Hotelling-Lawley")  # Lawley-Hotelling test

## From the above, we can clearly see that there is significant difference between clay and salty groups

# Performing tests on loam and sandy groups

data3 <- soil[which(soil$Soil.Type == "loam" | soil$Soil.Type == "sandy"), ]
data3
model3 <- manova(cbind(yield,water,herbicide) ~ Soil.Type, data = data3)
model3

# Performing the tests

summary(model3, test="Wilks")   # Wilk's lambda test
summary(model3, test="Pillai")  # The Pillai test
summary(model3, test="Roy")     # Roy's Union-Intersection test
summary(model3, test="Hotelling-Lawley")  # Lawley-Hotelling test

# Hence, at 5% level of significance, we accept the null hyppthesis H0.
# Therefore, the difference between loam and sandy groups is insignificant.

# performing tests for clay and salty vs. loam and sandy

data4 <- soil
data4[which(soil$Soil.Type == "loam" | soil$Soil.Type == "sandy"),1] <- "Group1"
data4[which(soil$Soil.Type == "clay" | soil$Soil.Type == "salty"),1] <- "Group2"


model4 <- manova(cbind(yield,water,herbicide) ~ Soil.Type, data = data4)
model4

# Performing the tests

summary(model4, test="Wilks")   # Wilk's lambda test
summary(model4, test="Pillai")  # The Pillai test
summary(model4, test="Roy")     # Roy's Union-Intersection test
summary(model4, test="Hotelling-Lawley")  # Lawley-Hotelling test

## From the above, we can clearly see that there is significant difference between clay and salty groups vs. loam and sandy groups

# Finding 95% Simultaneous & Bonferroni CI for the individual means

Group1 <- data.matrix(data4[1:16,-c(1)])
Group2 <- data.matrix(data4[17:32,-c(1)])

Y = Group1 - Group2
Y.bar = apply(Y,2, FUN = mean)
Y.bar
S <- cov(Y)
p <- ncol(Y)
n <- nrow(Y)
alpha <- 0.05

# Calculating Simultaneous CI

MargSC <- sqrt(p * (n-1) * qf((1-alpha),p,(n-p))/(n-p))*sqrt(diag(S)/n)
MargSC <- as.matrix(MargSC)
MargSC
SCI <- data.frame("lower bound" = as.vector(Y.bar - MargSC),
                  "upper bound" = as.vector(Y.bar - MargSC))
rownames(SCI) <- c("yield","water","herbicide")
SCI

# Calculating Bonferroni Interval

MargBI <- qt(1 - (alpha /(2*p)), (n-1))*sqrt(diag(S)/n)
MargBI <- as.matrix(MargBI)
MargBI
BI <- data.frame("lower bound" = as.vector(Y.bar - MargBI),
                  "upper bound" = as.vector(Y.bar + MargBI))
rownames(BI) <- c("yield","water","herbicide")
BI

##############################################################################################

# Problem 5

# Loading the dataset

study <- read.csv("C:\\Users\\PREET PAUL\\Desktop\\Presidency University M.Sc. Notes\\3rd Semester\\study.csv")
head(study)
View(study)

# Performing two-way MANOVA

model5 <- manova(cbind(kindness,optimism) ~ gender + economic + gender*economic, data=study)
model5

# Performing the tests

summary(model5, test="Wilks")   # Wilk's lambda test
summary(model5, test="Pillai")  # The Pillai test
summary(model5, test="Roy")     # Roy's Union-Intersection test
summary(model5, test="Hotelling-Lawley")  # Lawley-Hotelling test

# Hence, we can clearly see that the mean effects as well as the interaction effects differs 
# significantly at 5% level of significance.


