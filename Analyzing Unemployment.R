rm(list=ls())


library(readr)
library(tidyverse)
library(corrgram)
library(GGally)
library(magrittr)
library(funModeling)
library(gridExtra)
library(cluster)
library(NbClust)
library(fpc) 
library(factoextra) 
library(nnet) # used for multinomial regression
library(InformationValue)
library(caret)
library(pscl) # used for validating glm models
library(rms)
library(e1071) # used for SVM models
library(arules) # used for association rule mining
library(arulesViz)


# Set themes for all charts
thm <- theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent", colour = NA), 
    plot.background = element_rect(fill = "transparent", colour = NA),
    legend.position = "top",
    legend.background = element_rect(fill = "transparent", colour = NA),
    legend.key = element_rect(fill = "transparent", colour = NA)
  )
theme_set(thm)


# Read in data
headers <- read.csv('/Users/nonyeo./Downloads/USCensus1990raw-attr.csv', header = FALSE,
                    stringsAsFactors = FALSE)
headers <- t(headers) # Transpose header to rows to prepare for column names
data <- read.table('/Users/nonyeo./Downloads/USCensus1990raw.data.txt', sep = '\t',
                   header = FALSE, stringsAsFactors = FALSE)


# Select some of the data for project remove unwanted columns and scale all variables
dataRI <- filter(data, data[93] == 44)
colnames(dataRI) <- headers
censusRI <- dataRI[c(13, 49, 51, 54, 55, 57, 59, 61, 65:73, 76, 79, 81, 83, 84, 90, 
                     91, 95, 97, 103, 105, 108, 109, 113, 119, 120, 123, 125)]
str(censusRI)
censusRI <- na.omit(censusRI)


# Test for multicolinearity and remove unimportant highly correlated columns 
ggcorr(censusRI, method = c("everything", "pearson"), label = TRUE)
censusRI <- censusRI[c(1:7, 8:10, 20:22, 25, 29:31, 33:35)]
write_csv(censusRI, 
          '/Users/nonyeo./Documents/JWU/Spring_2020/DATA5550 - Optimization Simulation/Project/90CensusRI.csv')
censusRI <- read_csv('90CensusRI.csv')




# ************************************************
# Exploratory Data Analysis on Final Data
# ************************************************
basic_eda <- function(x)
{
  glimpse(x)
  df_status(x)
  freq(x) 
  profiling_num(x)
  plot_num(x)
  describe(x)
}

basic_eda(censusRI)




# ************************************************
# K-Means Algorithm
# ************************************************
# Visualize correlation distances in data
census_dist <- get_dist(censusRI)
fviz_dist(census_dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Scale data so that no variable skews cluster results
censusRI_s <- scale(censusRI)
censusfinal <- as.data.frame(censusRI_s) # save scaled data just in case


#.......... Apply kmeans algorithm to distributed data............#
#.................................................................#
# Determine optimal number of clusters
kmean_wssd <- function(k) {
  cluster <- kmeans(census_dist, k)
  return (cluster$tot.withinss)
}
max_k <- 20 # Assign maximum k
wss <- sapply(2:max_k, kmean_wssd) # Run kmean_withinss function
elbow <-data.frame(2:max_k, wss) # Save output into dataframe
ggplot(elbow, aes(x = X2.max_k, y = wss)) + # Plot wss output
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1)) +
  labs(x = 'k Values', y = 'WSS')
# Optimal k = 6

##### With k = 6, run k-means model
set.seed(234)
kd6 <- kmeans(census_dist, centers = 6, nstart = 25)
str(kd6)
fviz_cluster(kd6, data = census_dist, geom = c("point"), 
             main = NULL, ggtheme = thm)

# A look at the distributed dataset
head(round(as.matrix(census_dist), 2))[, 1:6]
max(as.matrix(census_dist), 2)

# Evaluation of clusters
set.seed(234) # Results will change with each run so set seed to get reproducible results
kd6_cboot <- clusterboot(census_dist, clustermethod = kmeansCBI,
                        k = 6) 

# Return vector of cluster stabilities. 
kd6_cboot$bootmean 

# Count of how many times each cluster was dissolved. 
kd6_cboot$bootbrd 


#..............Apply kmeans algorithm to scaled data..............#
#.................................................................#

set.seed(234)
# Determine optimal number of clusters
fviz_nbclust(censusRI_s, kmeans, method = "gap_stat")
fviz_nbclust(censusRI_s, kmeans, method = "wss")
fviz_nbclust(censusRI_s, kmeans, method = "silhouette")

# Use function to find optimal k with wss
kmean_withinss <- function(k) {
  cluster <- kmeans(censusRI_s, k)
  return (cluster$tot.withinss)
}
max_k <- 20 # Assign maximum k
wss <- sapply(2:max_k, kmean_withinss) # Run kmean_withinss function
elbow <-data.frame(2:max_k, wss) # Save output into dataframe
ggplot(elbow, aes(x = X2.max_k, y = wss)) + # Plot wss output
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1)) +
  labs(x = 'k Values', y = 'WSS')
# Optimal k = 6


##### With k = 6, run k-means model
set.seed(234)
k6 <- kmeans(censusRI_s, centers = 6, nstart = 25)
fviz_cluster(k6, data = censusRI_s, geom = c("point"), 
             main = NULL, ggtheme = thm)

# Evaluation of clusters
set.seed(234)
k6_cboot <- clusterboot(censusRI_s, clustermethod = kmeansCBI,
                            k = 6) # Results change with each run

# Return vector of cluster stabilities. 
k6_cboot$bootmean 

# Count of how many times each cluster was dissolved. 
k6_cboot$bootbrd 

##### Assign cluster numbers to final dataset
censusfinal$CLUSTER <- as.factor(k6$cluster)
censusRI$CLUSTER <- as.factor(k6$cluster)




# ************************************************
# Exploratory Data Analysis on Clusters
# ************************************************
# EDA on scaled data with clusters
basic_eda(censusfinal)
# EDA on original dataset to understand the clusters formed
basic_eda(censusRI)

# Frequency plots
p <- ggplot(censusRI, aes(x = CLUSTER)) 
p1 <- p + geom_col(aes(y = AGE))
p2 <- p + geom_col(aes(y = AVETS1))
p3 <- p + geom_col(aes(y = AWORK89))
p4 <- p + geom_col(aes(y = CITIZEN))
p5 <- p + geom_col(aes(y = CLASS))
p6 <- p + geom_col(aes(y = DISABL1))
p7 <- p + geom_col(aes(y = ENGLISH))
p8 <- p + geom_col(aes(y = FERTIL))
p9 <- p + geom_col(aes(y = IMMIGR))
p10 <- p + geom_col(aes(y = INCOME1))
p11 <- p + geom_col(aes(y = MEANS))
p12 <- p + geom_col(aes(y = MIGSTATE))
p13 <- p + geom_col(aes(y = MILITARY))
p14 <- p + geom_col(aes(y = RACE))
p15 <- p + geom_col(aes(y = RSPOUSE))
p16 <- p + geom_col(aes(y = RVETSERV))
p17 <- p + geom_col(aes(y = SEX))
p18 <- p + geom_col(aes(y = WORK89))
p19 <- p + geom_col(aes(y = YEARSCH))
p20 <- p + geom_col(aes(y = YRSSERV))
grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,
             p11, p12, p13, p14, p15, p16, p17, p18,
             p19, p20, nrow=5)

# Scatter plots
q <- ggplot(censusRI, aes(x = CLUSTER)) 
q1 <- q + geom_point(aes(y = AGE))
q2 <- q + geom_point(aes(y = AVETS1))
q3 <- q + geom_point(aes(y = AWORK89))
q4 <- q + geom_point(aes(y = CITIZEN))
q5 <- q + geom_point(aes(y = CLASS))
q6 <- q + geom_point(aes(y = DISABL1))
q7 <- q + geom_point(aes(y = ENGLISH))
q8 <- q + geom_point(aes(y = FERTIL))
q9 <- q + geom_point(aes(y = IMMIGR))
q10 <- q + geom_point(aes(y = INCOME1))
q11 <- q + geom_point(aes(y = MEANS))
q12 <- q + geom_point(aes(y = MIGSTATE))
q13 <- q + geom_point(aes(y = MILITARY))
q14 <- q + geom_point(aes(y = RACE))
q15 <- q + geom_point(aes(y = RSPOUSE))
q16 <- q + geom_point(aes(y = RVETSERV))
q17 <- q + geom_point(aes(y = SEX))
q18 <- q + geom_point(aes(y = WORK89))
q19 <- q + geom_point(aes(y = YEARSCH))
q20 <- q + geom_point(aes(y = YRSSERV))
grid.arrange(q1, q2, q3, q4, q5, q6, q7, q8, q9, q10,
             q11, q12, q13, q14, q15, q16, q17, q18,
             q19, q20, nrow=5)

# Create objects for each cluster
c1 <- subset(censusRI, CLUSTER == 1)
c2 <- subset(censusRI, CLUSTER == 2)
c3 <- subset(censusRI, CLUSTER == 3)
c4 <- subset(censusRI, CLUSTER == 4)
c5 <- subset(censusRI, CLUSTER == 5)
c6 <- subset(censusRI, CLUSTER == 6)

# Frequency charts per cluster
plot_num(c1)
plot_num(c2)
plot_num(c3)
plot_num(c4)
plot_num(c5)
plot_num(c6)

# Density Plots of some key variables
ggplot(censusRI, aes(x = AGE)) +
  geom_density(aes(color = CLUSTER))
ggplot(censusRI, aes(x = YEARSCH)) +
  geom_density(aes(color = CLUSTER))
ggplot(censusRI, aes(x = WORK89)) +
  geom_density(aes(color = CLUSTER))
ggplot(censusRI, aes(x = ENGLISH)) +
  geom_density(aes(color = CLUSTER))

# Box Plots of of clusters
r <- ggplot(censusRI, aes(x = CLUSTER)) 
r1 <- r + geom_boxplot(aes(y = AGE))
r2 <- r + geom_boxplot(aes(y = AVETS1))
r3 <- r + geom_boxplot(aes(y = AWORK89))
r4 <- r + geom_boxplot(aes(y = CITIZEN))
r5 <- r + geom_boxplot(aes(y = CLASS))
r6 <- r + geom_boxplot(aes(y = DISABL1))
r7 <- r + geom_boxplot(aes(y = ENGLISH))
r8 <- r + geom_boxplot(aes(y = FERTIL))
r9 <- r + geom_boxplot(aes(y = IMMIGR))
r10 <- r + geom_boxplot(aes(y = INCOME1))
r11 <- r + geom_boxplot(aes(y = MEANS))
r12 <- r + geom_boxplot(aes(y = MIGSTATE))
r13 <- r + geom_boxplot(aes(y = MILITARY))
r14 <- r + geom_boxplot(aes(y = RACE))
r15 <- r + geom_boxplot(aes(y = RSPOUSE))
r16 <- r + geom_boxplot(aes(y = RVETSERV))
r17 <- r + geom_boxplot(aes(y = SEX))
r18 <- r + geom_boxplot(aes(y = WORK89))
r19 <- r + geom_boxplot(aes(y = YEARSCH))
r20 <- r + geom_boxplot(aes(y = YRSSERV))
grid.arrange(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10,
             r11, r12, r13, r14, r15, r16, r17, r18,
             r19, r20, nrow=5)




# ************************************************
# Multinomial Logistic Algorithm
# ************************************************
# Model will be run on both sets of data, scaled and unscaled

#...................Model on Unscaled Data...................#
#.........................................................#
# Create partitions of the data
set.seed(987)
smpsize <- floor(0.70 * nrow(censusRI))
trainind <- sample(seq_len(nrow(censusRI)), size = smpsize)
trn <- censusRI[trainind, ]
tst <- censusRI[-trainind, ]

#### Apply algorithm
multinomfit <- multinom(CLUSTER ~ ., data = trn) 
summary(multinomfit)

# Predict the values for test dataset, build classification table and calculate accuracy
tst$predicted <- predict(multinomfit, newdata = tst, "class")

# Create confusion matrix and view accuracy
confusionMatrix(tst$predicted, tst$CLUSTER)

# Check pseudo R2
round(pR2(multinomfit), 7)


#...................Model on Scaled Data..................#
#.........................................................#
# Create partitions of the data
set.seed(987)
smp_size <- floor(0.70 * nrow(censusfinal))
train_ind <- sample(seq_len(nrow(censusfinal)), size = smp_size)
train <- censusfinal[train_ind, ]
test <- censusfinal[-train_ind, ]

#### Apply algorithm
multinom.fit <- multinom(CLUSTER ~ ., data = train) 
summary(multinom.fit)

# Predict the values for test dataset, build classification table and calculate accuracy
test$predicted <- predict(multinom.fit, newdata = test, "class")

# Create confusion matrix and view accuracy
confusionMatrix(test$predicted, test$CLUSTER)

# Check pseudo R2
round(pR2(multinom.fit), 7)




# ************************************************
# Support Vector Machine Algorithm
# ************************************************
# Again model will be run on scaled and unscaled data

#...................Model on Unscaled Data...................#
#.........................................................#
set.seed(987)
svmfit <- svm(CLUSTER ~ ., data = trn, type = 'C-classification')
summary(svmfit)

# Predict test$CLUSTER
predictsvm <- predict(svmfit, newdata = tst)

# Create confusion matrix and view accuracy
confusionMatrix(predictsvm,tst$CLUSTER)


#...................Model on Scaled Data..................#
#.........................................................#
set.seed(987)
svm.fit <- svm(CLUSTER ~ ., data = train, type = 'C-classification')
summary(svm.fit)

# Predict test$CLUSTER
predict_svm <- predict(svm.fit, newdata = test)

# Create confusion matrix and view accuracy
confusionMatrix(predict_svm,test$CLUSTER)




# ************************************************
# Machine Learning - Association rule mining
# ************************************************
# Convert all values to factors
censusRIf <- censusRI
col_names <- names(censusRIf)
censusRIf[,col_names] <- lapply(censusRIf[,col_names] , factor)
str(censusRIf)

# Find association rules with default settings
rules <- apriori(censusRIf)
inspect(head(rules))

# Check variable importance from multinomial glm model
order(varImp(multinomfit), decreasing = TRUE)
unique(censusRI[18])

# View rules with rhs containing only the most important variable
rules <- apriori(censusRIf,
                 parameter = list(minlen = 2, supp = 0.85, conf = 0.9),
                 appearance = list(rhs = c('WORK89=1', 'WORK89=2'), default="lhs"),
                 control = list(verbose=F))
rules.sorted <- sort(rules, by="lift")
inspect(head(rules.sorted))

# Find and remove redundant rules
redundant <- is.redundant(rules.sorted)
rules.pruned <- rules.sorted[!redundant]
inspect(head(rules.pruned))

# Visualize association rules
plot(rules.pruned, jitter = 0)
plot(rules.pruned, method = 'grouped')
plot(rules.pruned, method = 'graph', control = list(type = 'items'))
plot(rules.pruned, method = 'paracoord', control = list(reorder = TRUE))
