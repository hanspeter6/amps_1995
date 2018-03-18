# libraries
library(stringr)
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
library(scatterplot3d)
library(rgl)
library(kohonen)
library(caret)
library(randomForest)
library(MASS)
library(CCA)
library(nFactors)
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(ggplot2)
# 
#  read in dataset
set95 <- readRDS("set95.rds")
set95_simple <- readRDS("set95_simple.rds")

#
# consider some correlations

png('corTypePlot1995.png')
corrplot(cor(set95[,c("newspapers","magazines","radio", "tv")]),
         method = "pie",
         order = "hclust",
         hclust.method = "complete",
         tl.col = 'black',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         tl.pos = TRUE)
dev.off()

## consider kmeans
wss <- vector()
for(k in c(1,2,3,4,5,6)) {
        temp <- kmeans(set95[,c("newspapers","magazines","radio", "tv", "all")],
                       centers = k,
                       nstart = 3,
                       iter.max = 20)
        wss <- append(wss,temp$tot.withinss)
}

png('kmeansTypePlot1995.png')
plot(c(1,2,3,4,5,6), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(56)
kmeans95 <- kmeans(set95[,c("newspapers","magazines","radio", "tv", "all")],
                   centers = 4,
                   nstart = 20)
set.seed(56)
kmeans95_simple <- kmeans(set95_simple[,c("newspapers","magazines","radio", "tv", "all")],
                          centers = 4,
                          nstart = 20)


# Comparing 1995 with 2002... will change colours if necessary to reflect meaning based on 2012:

# red stays red:  1 stays 1
# green stays green: 2 stays 2
# blue becomes lilac:   3 becomes 4
# lilac becomes blue: 4 becomes 3
kmeans95$cluster <- ifelse(kmeans95$cluster == 1, 6, kmeans95$cluster)
kmeans95$cluster <- ifelse(kmeans95$cluster == 2, 7, kmeans95$cluster)
kmeans95$cluster <- ifelse(kmeans95$cluster == 3, 9, kmeans95$cluster)
kmeans95$cluster <- ifelse(kmeans95$cluster == 4, 8, kmeans95$cluster)
kmeans95$cluster <- kmeans95$cluster - 5



# need to try id them... so will come back here

# add cluster labels to the dataset
set95c <- set95 %>%
        mutate(cluster = factor(kmeans95$cluster))
set95c_simple <- set95_simple %>%
        mutate(cluster = factor(kmeans95_simple$cluster))

saveRDS(set95c, "set95c.rds")
saveRDS(set95c_simple, "set95c_simple.rds")



# some plots
# boxplots of clusters and media types
p1 <- ggplot(set95c, aes(cluster, all, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "all")
p2 <- ggplot(set95c, aes(cluster, newspapers, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "newspapers")
p3 <- ggplot(set95c, aes(cluster, magazines, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "magazines")
p4 <- ggplot(set95c, aes(cluster, radio, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "radio")
p5 <- ggplot(set95c, aes(cluster, tv, fill = cluster)) +
        geom_boxplot() +
        guides(fill = FALSE) +
        labs(title = "tv")

jpeg('typeBoxPlots_95.jpeg', quality = 100, type = "cairo")
grid.arrange(p1, p2, p3, p4, p5,  ncol=3, nrow = 2)
dev.off()

# try to make sense of demographics
d1 <- ggplot(set95c, aes(race, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "race", y = "", x = "") +
        scale_x_discrete(labels=c("black", "coloured", "indian", "white"))
d2 <- ggplot(set95c, aes(edu, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "education", y = "", x = "") +
        scale_x_discrete(labels=c("<matric", "matric",">matric"))
d3 <- ggplot(set95c, aes(age, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "age", y = "", x = "") +
        scale_x_discrete(labels=c("15-24","25-44", "45-54","55+"))
# d4 <- ggplot(set95c, aes(lsm, cluster, fill = cluster)) +
#         geom_col() +
#         labs(title = "lsm", y = "", x = "") +
#         scale_x_discrete(labels=c("1-2", "3-4", "5-6", "7-8", "9-10"))

jpeg('typeDemogPlots1_95.jpeg', quality = 100, type = "cairo")
grid.arrange(d1, d2, d3, ncol=2, nrow = 2)
dev.off()

d5 <- ggplot(set95c, aes(sex, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "gender", y = "", x = "") +
        scale_x_discrete(labels=c("male", "female"))
d6 <- ggplot(set95c, aes(hh_inc, cluster, fill = cluster)) +
        geom_col() +
        labs(title = "household income", y = "", x = "") +
        scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
# d7 <- ggplot(set95c, aes(lifestages, cluster, fill = cluster)) +
#         geom_col() +
#         labs(title = "lifestages", y = "", x = "")# +
# scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
# d8 <- ggplot(set95c, aes(lifestyle, cluster, fill = cluster)) +
#         geom_col() +
#         labs(title = "lifestyle", y = "", x = "")# +
# scale_x_discrete(labels=c("<5000","5000-10999","11000-19999",">=20000"))
jpeg('typeDemogPlots2_95.jpeg', quality = 100, type = "cairo")
grid.arrange(d5, d6, ncol=2, nrow = 2)
dev.off()


# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub95 <- set95[sample(nrow(set95), size = 9500),]

# distance matrix and MDS
sub95_dist <- dist(sub95[,c("newspapers","magazines","radio", "tv", "internet")])
mds95 <- cmdscale(sub95_dist)
plot(mds95, col = as.numeric(sub95$cluster) + 1, pch = 19, ylab = "", xlab = "")

# 3D scaling
mds3 <- cmdscale(dist(sub95[,c("newspapers", "magazines", "radio", "tv", "internet")]), k = 3)
mds3 <- as.data.frame(mds3)

# 2D & 3D Scatterplots of 5 centers
jpeg('kmeans2DPlot2095.jpeg')
plot(mds95, col = as.numeric(sub95$cluster) + 1, ylab = "", xlab = "", pch = 19)
dev.off()

jpeg('kmeans3DPlot2095.jpeg')
scatterplot3d(mds3, color = as.numeric(sub95$cluster) + 1, xlab = '', ylab = '', zlab = '')
dev.off()

# Spinning 3D for 5 classes
jpeg('kmeansSpinningPlot2095.png')
plot3d(jitter(mds3$V1), jitter(mds3$V2), jitter(mds3$V3), col= as.numeric(sub95$cluster) + 1, size=5, xlab = '', ylab = '', zlab = '', pch = 19)
dev.off()

# try some Self Organising Maps.... try to explain the differences....

# set up somgrid
grid <- somgrid(xdim = 95, ydim = 95, topo = "hexagonal")

# run som
# set up as data matrix
mat_sub <- as.matrix(sub95[,c('newspapers', 'magazines', 'radio', 'tv','internet')])
som_sub <- som(mat_sub, grid = grid, rlen = 95000) 

par(mfrow = c(1,1))
plot(som_sub, type = "codes")
plot(som_sub, type = "changes")
plot(som_sub, type = "counts")
plot(som_sub, type = "dist.neighbours")
plot(som_sub, type = "quality")

par(mfrow = c(3,2))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,1], main = names(sub95['newspapers']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,2], main = names(sub95['magazines']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,3], main = names(sub95['radio']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,4], main = names(sub95['tv']))
plot(som_sub, type = "property", property = as.data.frame(som_sub$codes)[,5], main = names(sub95['internet']))

par(mfrow = c(1,1))
plot(som_sub, type = "mapping", bgcol = sub95$cluster ) # not very good organising??


# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train <- createDataPartition(set95$cluster, p = 0.7, list = FALSE)
training <- set95[ind_train,]
testing <- set95[-ind_train,]

# # using random forest:
forest95_type <- randomForest(cluster ~ newspapers
                              + tv
                              + radio
                              + magazines
                              + internet,
                              data = training )

pred_forest95_type <- predict(forest95_type, newdata = testing)

confusionMatrix(pred_forest95_type, testing$cluster) 

# with lda. Although given accuracy of forest,  no real need.
set.seed(56)
lda95 <- lda(cluster ~ newspapers
             + tv
             + radio
             + magazines
             + internet,
             data = training)
summary(lda95)

pred_lda95 <- predict(lda95, newdata = testing)
confusionMatrix(pred_lda95$class, testing$cluster) # 

# using only demographic information
forest95_demogr <- randomForest(cluster ~ age
                                + sex
                                + edu
                                + hh_inc
                                + race
                                + lang
                                + lifestages
                                + mar_status
                                + lsm
                                + lifestyle
                                + attitudes,
                                data = training)

pred_forest95_demogr <- predict(forest95_demogr, newdata = testing)

confusionMatrix(pred_forest95_demogr, testing$cluster)

# with lda
set.seed(56)
lda95_demogr <- lda(cluster ~ age
                    + sex
                    + edu
                    + hh_inc
                    + race
                    + lang
                    + lifestages
                    + mar_status
                    + lsm
                    + lifestyle
                    + attitudes,
                    data = training)

pred_lda95_demogr <- predict(lda95_demogr, newdata = testing)
confusionMatrix(pred_lda95_demogr$class, testing$cluster)

##  some qualitative consideration of the four types:

# consider a single tree partitioning to try to add meaning to the six clusters
control <- rpart.control(maxdepth = 4, cp = 0.001)
tree95 <- rpart(cluster ~ newspapers + tv + radio + magazines + internet, 
                data = set95,
                control = control) # weights = pwgt
par(mfrow = c(1,1))
plot(tree95, uniform = TRUE, margin = 0.2)
text(tree95, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree95, type = 4, extra = 1, cex = 0.5)

percentile <- ecdf(set95$internet)
percentile(1.4)

# some plots
jpeg('typeBoxPlots_95.jpeg', quality = 950, type = "cairo")
par(mfrow = c(2,3))
plot(set95$radio ~ set95$cluster, col = c(2,3,4,6), main = "radio", xlab = "cluster", ylab = '')
plot(set95$tv ~ set95$cluster, col = c(2,3,4,6), main = "tv", xlab = "cluster", ylab = '')
plot(set95$newspapers ~ set95$cluster, col = c(2,3,4,6), main = "newspapers", xlab = "cluster", ylab = '')
plot(set95$magazines ~ set95$cluster, col = c(2,3,4,6), main = "magazines", xlab = "cluster", ylab = '')
plot(set95$all ~ set95$cluster, col = c(2,3,4,6), main = "all", xlab = "cluster", ylab = '')
dev.off()

# try to make sense of demographics
jpeg('typeDemogPlots1_95.jpeg', quality = 950, type = "cairo")
par(mfrow = c(2,2))
plot(set95$cluster ~ factor(set95$race,labels = c("black", "coloured", "indian", "white")), col = c(2,3,4,6), main = "race", xlab = "", ylab = "")
plot(set95$cluster ~ factor(set95$edu, labels = c("<matric", "matric",">matric" )), col = c(2,3,4,6), main = "education", xlab = "", ylab = "")
plot(set95$cluster ~ factor(set95$age, labels = c("15-24","25-44", "45-54","55+")), col = c(2,3,4,6), main = "age", xlab = "", ylab = "")
dev.off()

jpeg('typeDemogPlots2_95.jpeg', quality = 950, type = "cairo")
par(mfrow = c(2,2))
plot(set95$cluster ~ factor(set95$sex, labels = c("male", "female")), col = c(2,3,4,6), main = "sex", xlab = "", ylab = "")
plot(set95$cluster ~ factor(set95$hh_inc, labels = c("<2500","2500-5999","6000-8999",">=9000")), col = c(2,3,4,6), main = "hh_inc", xlab = "", ylab = "")
plot(set95$cluster ~ set95$lifestages, col = c(2,3,4,6), main = "lifestages", xlab = "", ylab = "")
dev.off()
