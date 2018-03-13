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
# 

# read datafiles
magazines_engagement_95 <- readRDS("magazines_engagement_95.rds")
newspapers_engagement_95 <- readRDS("newspapers_engagement_95.rds")
radio_engagement_95 <- readRDS("radio_engagement_95.rds")
tv_engagement_95 <- readRDS("tv_engagement_95.rds")
internet_engagement_95 <- readRDS("internet_engagement_95.rds")

media_type_95 <- readRDS("media_type_95.rds")
media_type_95_simple <- readRDS("media_type_95_simple.rds")
media_vehicles_95 <- readRDS("media_vehicles_95.rds")
media_vehicles_95_simple <- readRDS("media_vehicles_95_simple.rds")
demographics_95 <- readRDS("demographics_95.rds")

#reducing levels of categorical variables and setting factor types for demographics:

# # age:
# demographics_95$age <- ifelse(demographics_95$age %in% c(1,2), 1, demographics_95$age)
# demographics_95$age <- ifelse(demographics_95$age %in% c(3,4), 2, demographics_95$age)
# demographics_95$age <- ifelse(demographics_95$age %in% c(5,6), 3, demographics_95$age)
# demographics_95$age <- ifelse(demographics_95$age %in% c(7,8), 4, demographics_95$age)
demographics_95$age <- factor(demographics_95$age, ordered = TRUE)

# sex:
demographics_95$sex <- factor(demographics_95$sex, ordered = FALSE)

#edu:
demographics_95$edu <- ifelse(demographics_95$edu %in% c(1,2,3,4), 1, demographics_95$edu)
demographics_95$edu <- ifelse(demographics_95$edu %in% c(5), 2, demographics_95$edu)
demographics_95$edu <- ifelse(demographics_95$edu %in% c(6,7,8), 3, demographics_95$edu)
demographics_95$edu <- factor(demographics_95$edu, ordered = TRUE)

#hh_inc
demographics_95$hh_inc <- ifelse(demographics_95$hh_inc %in% c(1,2,3,4), 1, demographics_95$hh_inc)
demographics_95$hh_inc <- ifelse(demographics_95$hh_inc %in% c(5,6), 2, demographics_95$hh_inc)
demographics_95$hh_inc <- ifelse(demographics_95$hh_inc %in% c(7), 3, demographics_95$hh_inc)
demographics_95$hh_inc <- ifelse(demographics_95$hh_inc %in% c(8), 4, demographics_95$hh_inc)
demographics_95$hh_inc <- factor(demographics_95$hh_inc, ordered = TRUE)

demographics_95$race <- factor(demographics_95$race, ordered = FALSE)
demographics_95$province <- factor(demographics_95$province, ordered = FALSE)
demographics_95$metro <- factor(demographics_95$metro, ordered = FALSE)
demographics_95$lang <- factor(demographics_95$lang, ordered = FALSE)
# demographics_95$lifestages <- factor(demographics_95$lifestages, ordered = FALSE)
demographics_95$mar_status <- factor(demographics_95$mar_status, ordered = FALSE)
# demographics_95$pers_inc <- factor(demographics_95$pers_inc, ordered = TRUE)

# # lsm
# demographics_95$lsm <- ifelse(demographics_95$lsm %in% c(1,2), 1, demographics_95$lsm)
# demographics_95$lsm <- ifelse(demographics_95$lsm %in% c(3,4), 2, demographics_95$lsm)
# demographics_95$lsm <- ifelse(demographics_95$lsm %in% c(5,6), 3, demographics_95$lsm)
# demographics_95$lsm <- ifelse(demographics_95$lsm %in% c(7,8), 4, demographics_95$lsm)
# demographics_95$lsm <- ifelse(demographics_95$lsm %in% c(9,10), 5, demographics_95$lsm)
# demographics_95$lsm <- factor(demographics_95$lsm, ordered = TRUE)

# demographics_95$lifestyle <- factor(demographics_95$lifestyle, ordered = FALSE) # not for 2095 yet
# demographics_95$attitudes <- factor(demographics_95$attitudes, ordered = FALSE) # not for 2095 yet

# #create single dataset minus non metropolitans
set95 <- demographics_95 %>%
        left_join(media_type_95) %>%
        left_join(media_vehicles_95) %>%
        filter(metro != 0)
set95_simple <- demographics_95 %>%
        left_join(media_type_95_simple) %>%
        left_join(media_vehicles_95_simple) %>%
        filter(metro != 0)

# consider some correlations

png('corTypePlot2095.png')
corrplot(cor(set95[,c("newspapers","magazines","radio", "tv", "internet")]),
         method = "pie",
         order = "hclust",
         hclust.method = "complete",
         tl.col = 'black',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         tl.pos = TRUE)
dev.off()

# # consider some clustering
# # construct distance matrix for newspapers, magazines, radio, tv and internet engagement:
# 
# dist95 <- dist(set95[,c("newspapers","magazines","radio", "tv", "internet")])
# clust95 <- hclust(dist95, method = "complete")
# plot(clust95) # messy, unhelpful

## consider kmeans
wss <- vector()
for(k in c(3,4,5,6,7,8,9,95,11,95)) {
        temp <- kmeans(set95[,c("newspapers","magazines","radio", "tv", "internet")],
                       centers = k,
                       nstart = 3,
                       iter.max = 20)
        wss <- append(wss,temp$tot.withinss)
}

png('kmeansTypePlot2095.png')
plot(c(3,4,5,6,7,8,9,95,11,95), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(56)
kmeans95 <- kmeans(set95[,c("newspapers","magazines","radio", "tv", "internet")],
                   centers = 5,
                   nstart = 20)
set.seed(12)
kmeans95_simple <- kmeans(set95_simple[,c("newspapers","magazines","radio", "tv", "internet")],
                          centers = 5,
                          nstart = 20)


# add cluster labels to the dataset
set95 <- set95 %>%
        mutate(cluster = factor(kmeans95$cluster))
set95_simple <- set95_simple %>%
        mutate(cluster = factor(kmeans95_simple$cluster))

# trying out idea of first pc scores as measure of media type mix...kinda engagement...think about this

pc_type <- princomp(set95[,c('newspapers', 'magazines', 'tv', 'radio', 'internet')])
screeplot(pc_type, type = "lines")
pc_type_simple <- princomp(set95_simple[,c('newspapers', 'magazines', 'tv', 'radio', 'internet')])
screeplot(pc_type_simple, type = "lines")

set95 <- set95 %>%
        mutate(typePC = scale(pc_type$scores[,1]))
set95_simple <- set95_simple %>%
        mutate(typePC = scale(pc_type_simple$scores[,1]))


saveRDS(set95, "set95.rds")
saveRDS(set95_simple, "set95_simple.rds")
set95 <- readRDS("set95.rds")
set95_simple <- readRDS("set95_simple.rds")

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

# Try pca to get sense of relative use of media type... not very helpful since in most cases require many components to reflect variation in the data.

mags_pca <- princomp(scale(magazines_engagement_95))
screeplot(mags_pca, type = "lines")
newsp_pca <- princomp(scale(newspapers_engagement_95))
screeplot(newsp_pca, type = "lines")
tv_pca <- princomp(scale(tv_engagement_95))
screeplot(tv_pca, type = "lines")
rad_pca <- princomp(scale(radio_engagement_95[,-60])) # cant divide by zero
screeplot(rad_pca, type = "lines")
int_pca <- princomp(scale(internet_engagement_95))
screeplot(int_pca, type = "lines")

all_pca <- princomp(set95[,c('newspapers','magazines', 'tv', 'radio', 'internet')])
screeplot(all_pca, type = "lines")
summary(all_pca) # first component could be useful (@~40% of variation) to give relative multimedia scores

# try kmeans on the first pca and compare with cluster values...
test <- kmeans(all_pca$scores[,1], centers = 6)
test$cluster
set95$cluster
cor(test$cluster, as.numeric(set95$cluster))

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
plot(set95$radio ~ set95$cluster, col = c(2,3,4,5,6), main = "radio", xlab = "cluster", ylab = '')
plot(set95$tv ~ set95$cluster, col = c(2,3,4,5,6), main = "tv", xlab = "cluster", ylab = '')
plot(set95$newspapers ~ set95$cluster, col = c(2,3,4,5,6), main = "newspapers", xlab = "cluster", ylab = '')
plot(set95$magazines ~ set95$cluster, col = c(2,3,4,5,6), main = "magazines", xlab = "cluster", ylab = '')
plot(set95$internet ~ set95$cluster, col = c(2,3,4,5,6), main = "internet", xlab = "cluster", ylab = '')
dev.off()

# try to make sense of demographics
jpeg('typeDemogPlots1_95.jpeg', quality = 950, type = "cairo")
par(mfrow = c(2,2))
plot(set95$cluster ~ factor(set95$race,labels = c("black", "coloured", "indian", "white")), col = c(2,3,4,5,6), main = "race", xlab = "", ylab = "")
plot(set95$cluster ~ factor(set95$edu, labels = c("<matric", "matric",">matric" )), col = c(2,3,4,5,6), main = "education", xlab = "", ylab = "")
plot(set95$cluster ~ factor(set95$age, labels = c("15-24","25-44", "45-54","55+")), col = c(2,3,4,5,6), main = "age", xlab = "", ylab = "")
plot(set95$cluster ~ factor(set95$lsm, labels = c("1-2", "3-4", "5-6", "7-8", "9-95")), col = c(2,3,4,5,6), main = "LSM", xlab = "", ylab = "")
dev.off()

jpeg('typeDemogPlots2_95.jpeg', quality = 950, type = "cairo")
par(mfrow = c(2,2))
plot(set95$cluster ~ factor(set95$sex, labels = c("male", "female")), col = c(2,3,4,5,6), main = "sex", xlab = "", ylab = "")
plot(set95$cluster ~ factor(set95$hh_inc, labels = c("<2500","2500-6999","7000-11999",">=95000")), col = c(2,3,4,5,6), main = "hh_inc", xlab = "", ylab = "")
plot(set95$cluster ~ set95$lifestages, col = c(2,3,4,5,6), main = "lifestages", xlab = "", ylab = "")
# plot(set95$cluster ~ set95$lifestyle, col = c(2,3,4,5,6), main = "lifestyle", xlab = "", ylab = "")
dev.off()
