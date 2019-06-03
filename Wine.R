#############################################################################################

# The purpose of this is to perform EDA on the white wine dataset at UCI repository
# And to create a model to predict quality.

#############################################################################################

library(tidyverse)
library(caret)
library(stargazer)
library(ggpubr)
library(reshape2)

wine <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", header = TRUE, sep = ";", stringsAsFactors = FALSE)

head(wine)
colnames(wine)
dim(wine)


cols <- colnames(wine)
stargazer(
    wine[, cols], type = "text", 
    summary.stat = c("min", "p25", "median", "mean", "p75", "max", "sd", "n")
)


winenorm <- as.data.frame(scale(wine))


cor_coef <- as.data.frame(as.table(round(cor(wine), 2)))
cor_coef1 <- subset(cor_coef, abs(Freq) > 0.5)
(cor_coef2 <- subset(cor_coef1, abs(Freq) < 1.0))
cor(wine$residual.sugar, wine$alcohol)
cor(wine$residual.sugar, wine$quality)
cor(wine$alcohol, wine$quality)


x <- as.data.frame(cbind(density = wine$density,
                                residualsugar = wine$residual.sugar,
                                totalsulfur = wine$total.sulfur.dioxide,
                                freesulphur = wine$free.sulfur.dioxide,
                                alcohol = wine$alcohol))
col_den <- as.data.frame(scale(x))
col_den1<- melt(col_den)
ggplot(col_den1, aes(x = value, fill = variable)) + geom_density(alpha = 0.35)



boxplot(winenorm[, c(4, 6, 7, 8,11)], range=0.0, horizontal=TRUE, 
        outline=TRUE, boxwex=0.4, border=c("dark blue", "purple", "brown", "dark green"))












# cor_coeff <- as.data.frame(as.table(round(cor(winenorm), 2)))
# cor_coeff1 <- subset(cor_coef, abs(Freq) > 0.5)
# (cor_coeff2 <- subset(cor_coef1, abs(Freq) < 1.0))



# (den <- as.data.frame(cbind(density = wine$density,
#                                 residualsugar = wine$residual.sugar,
#                                 totalsulfur = wine$total.sulfur.dioxide,
#                                 freesulphur = wine$free.sulfur.dioxide,
#                                 alcohol = wine$alcohol)))


# (col_den1<- melt(col_den))
# ggplot(col_den1, aes(x = value, fill = variable)) + geom_density(aes(group=factor),alpha = 0.25)
# ggplot(col_den1, aes(x=value)) + geom_density(aes(group=factor))
# 
# plot(density(wine$total.sulfur.dioxide))
# lines(density(wine$alcohol))
# plot <- ggplot(wine)
# plot + geom_point(mapping = aes(x = fixed.acidity, 
#                                 y = volatile.acidity, 
#                                 position = "jitter"))
# 
# plot + geom_point(mapping = aes(x = fixed.acidity, 
#                                 y = alcohol))
# 
# plot + geom_point(mapping = aes(x = residual.sugar, 
#                                 y = alcohol)) +
#     geom_abline()
# plot + geom_point(mapping = aes(y = pH,
#                                 x = density))
# 
# plot(density(wine$residual.sugar))
# plot(density(wine$alcohol))
# ggqqplot(wine$residual.sugar)
# ggqqplot(wine$alcohol)
# 
# 
# max(wine$residual.sugar)
# 
# min(wine$residual.sugar)
# 
# plot + geom_boxplot(mapping = aes(y = residual.sugar, x = pH)) 


# ggplot(wine) + 
#    geom_point(mapping = aes(x = fixed.acidity, y = volatile.acidity)) +
#    goem_jitter


ggplot(col_den1, aes(x=value)) + geom_density(aes(group=factor))