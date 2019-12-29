## Loading the libraries
library(factoextra)
library(corrplot) # to plot nice correlations
library(clusterSim) # to normalize data quickly
library(smacof)
library(labdsv)
library(tidyverse)
library(corrplot)

#Installing and loading packages

requiredPackages = c("tidyverse","factoextra","stats","clustertend","flexclust","ggforce"
                     ,"fpc","cluster","ClusterR","knitr","kableExtra","DataExplorer",
                     "reshape2","corrplot","labdsv","smacof","clusterSim","pastecs","psych",
                     "pca3d","pls","caret") 
for(i in requiredPackages){if(!require(i,character.only = TRUE)) install.packages(i)} 
for(i in requiredPackages){library(i,character.only = TRUE)} 

# Loading up the data
data_full <- read.csv("top50.csv",stringsAsFactors = F)
data_full <- data_full[,2:14]

data_init <- data_full

# Creating aggregate genre
data_init$aggregate.genre <- NA
data_init$aggregate.genre <- ifelse(data_init$Genre  %in% c("canadian pop","pop","dance pop",
                                                  "electropop","panamanian pop","pop house","australian pop"),"pop",
                               ifelse(data_init$Genre  %in% c("dfw rap","country rap","canadian hip hop","atl hip hop"),"hip hop",
                                      ifelse(data_init$Genre  %in% c("r&b en espanol","latin"),"latin",
                                             ifelse(data_init$Genre  %in% c("reggaeton flow","reggeaeton"),"reggaeton",
                                                    ifelse(data_init$Genre  %in% c("edm","trap music","brostep"),"electronic",
                                                           ifelse(data_init$Genre  %in% c("escape room","boy band","big room"),"other",data_init$Genre))))))

data_init$aggregate.genre <- as.factor(data_init$aggregate.genre)
unique(data_init$aggregate.genre)

typeof(data)

# Distribution of genres and aggregate genres
data_init %>% count(Genre) %>%
  ggplot(aes(reorder(Genre, n), n)) + geom_col(fill="cyan3") + coord_flip() +
  ggtitle("Top genre") + xlab("Genre") + ylab("Count")

data_init %>% count(aggregate.genre) %>%
  ggplot(aes(reorder(aggregate.genre, n), n)) + geom_col(fill="cyan3") + coord_flip() +
  ggtitle("Top aggregated genre") + xlab("Genre") + ylab("Count")


# Summary statistics of the data
x1 <- t(round(stat.desc(data_init[,c(4:13)]),2))
x1 <- x1[,c(4:6,8:13)]

kable(x1,caption = "Table 1. Summary statistics of the dataset")%>% 
  kable_styling(latex_options="scale_down",bootstrap_options = c("striped", "hover"))
#################################

# Distribution of the variables
gather(data_init[,c(4:13)]) %>% 
  ggplot(., aes(value)) + 
  geom_histogram(aes(y =..density..), 
                 col="black", 
                 fill="lightblue", 
                 alpha=.2) + 
  geom_density(col="cyan3")+
  labs(title="Graph 2. Histograms of variables")+
  facet_wrap(~key, scales = 'free')

# Log transformation of variables Liveness and Speechiness
transformed_var <- c("Liveness", "Speechiness.")
data_init <- data_init %>% mutate_at(vars(transformed_var), log)

gather(data_init[,c("Liveness", "Speechiness.")]) %>% 
  ggplot(., aes(value)) + 
  geom_histogram(aes(y =..density..), 
                 col="black", 
                 fill="lightblue", 
                 alpha=.2) + 
  geom_density(col="cyan3")+
  labs(title="Graph 2. Histograms of variables")+
  facet_wrap(~key, scales = 'free')

#################################
# Box plots for outlier detection


# Correlation analysis
data_corr<- select_if(data_init, is.numeric)
corrplot(cor(data_corr), type="lower", method="number",order ="alphabet")

# Spliting for train and test sample for later PCA regression pourpuses
## 75% of the sample size
smp_size <- floor(0.80 * nrow(data_init))

## set the seed to make partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data_init)), size = smp_size)

data <- data_init[train_ind, ]
data_test <- data_init[-train_ind, ]

#################################

# Normalization of data

data_pca <- data_init %>%  dplyr::select(-Popularity) %>% select_if(is.numeric)
data_pca_stand<-data.Normalization(data_pca, type="n1", normalization="column") #clusterSim::



###### PCA 
pca1<-prcomp(data_pca_stand, center=TRUE,scale. = TRUE)
x2 <- round(summary(pca1)$importance,2)

kable(x2,caption = "Table 2. Summary statistics of the PCA") %>% 
kable_styling(latex_options="scale_down",bootstrap_options = c("striped", "hover"))

# Visualize eigenvalues (scree plot). 
#Show the percentage of variances explained by each principal component.
fviz_eig(pca1)

# Calculating PCA results for variables

#var.coord = loadings * the component standard deviations
#var.cos2 = var.coord^2
#var.contrib. The contribution of a variable to a given principal component is (in percentage) : (var.cos2 * 100) / (total cos2 of the component)
# Helper function 
#::::::::::::::::::::::::::::::::::::::::
var_coord_func <- function(loadings, comp.sdev){
  loadings*comp.sdev
}
# Compute Coordinates
#::::::::::::::::::::::::::::::::::::::::
loadings <- pca1$rotation
sdev <- pca1$sdev
var.coord <- t(apply(loadings, 1, var_coord_func, sdev)) 
head(var.coord[,1:5])

# Compute Cos2
#::::::::::::::::::::::::::::::::::::::::
var.cos2 <- var.coord^2
head(var.cos2[, 1:4])

# Compute contributions
#::::::::::::::::::::::::::::::::::::::::
comp.cos2 <- apply(var.cos2, 2, sum)
contrib <- function(var.cos2, comp.cos2){var.cos2*100/comp.cos2}
var.contrib <- t(apply(var.cos2,1, contrib, comp.cos2))
head(var.contrib[, 1:4])


#Graph of variables. Positive correlated variables point to the same side of the plot. 
#Negative correlated variables point to opposite sides of the graph
fviz_pca_var(pca1,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Graph of individuals. Individuals with a similar profile are grouped together.
fviz_pca_ind(pca1,
             col.ind = "cos2",# Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
) + theme_minimal()

fviz_pca_ind(pca1,label="none",
             habillage=data_init$aggregate.genre,
             addEllipses=TRUE) + theme_minimal()


fviz_pca_biplot(pca1, label ="var",
                habillage=data_init$aggregate.genre,
                repel = TRUE) + theme_minimal()


# Rotated PCA
n = 3
pca_rot<-principal(data_pca_stand, nfactors=n, rotate="varimax")
x3 <- pca_rot
pca_rot$loadings[1]

x5 <- loadings(pca_rot)
filter(loadings(pca_rot)[,1:3]>0.4)


kable(round(x3$loadings[,1:n],2),caption = "Output 3") %>% 
kable_styling(latex_options = "hold_position",bootstrap_options = c("striped", "hover"),)

print(loadings(pca_rot), digits=3, cutoff=0.4, sort=TRUE)

### Quality measures of PCA
#1 Complexity
pca_rot$complexity
pca_rot$uniqueness

kable(round(cbind(pca_rot$complexity,pca_rot$uniqueness),2),col.names = c("Complexity","Uniqueness"),caption = "Output 3") %>% 
  kable_styling(latex_options = "hold_position",bootstrap_options = c("striped", "hover"),)


zzz.pca<-princomp(data_pca_stand)$scores[, 1:2] # stats:: package
km<-KMeans_rcpp(zzz.pca, clusters=5, num_init=5, max_iters = 100) # from ClusterR::
plot_2d(zzz.pca, km$clusters, km$centroids)

# Principal components regression
# Forecasting with pcr() (split the sample into train and test data)

# prediction from the pcr model
data_pca_regression <- data.Normalization(data_init %>% select_if(is.numeric), 
type="n1", normalization="column")

reg.pcr.train<- pcr(Popularity~., data=data_pca_regression, scale=TRUE, 
                    center = TRUE,validation="CV",ncomp = 9)
##########################
model <- train(
  Popularity~., data = data_pca_regression, method = "pls",
  scale = TRUE,
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
# Plot model RMSE vs different values of components
plot(model)
# Print the best tuning parameter ncomp that
# minimize the cross-validation error, RMSE
model$bestTune
##########################
summary(reg.pcr.train)
kable(summary(reg.pcr.train),caption = "Output 3") %>% 
kable_styling(latex_options = "hold_position",bootstrap_options = c("striped", "hover"),)

predplot(reg.pcr.train)
summary(reg.pcr.train)

reg.pcr.train$validation

data_test_regression <- data.Normalization(data_test[,c(4:12)], type="n1", normalization="column")

reg.pcr.pred<-predict(reg.pcr.train, data_test[,c(4:12)], ncomp = 3)


data_test_regression <- data.Normalization(data_test %>% select_if(is.numeric), type="n1", normalization="column")

reg.pcr.pred<-predict(reg.pcr.train, data_test_regression, ncomp = 3)
mean((reg.pcr.pred - data_test_regression[,10])^2)

summary(reg.pcr.pred)

###
# Model performance metrics
data.frame(
  RMSE = caret::RMSE(reg.pcr.pred, data_test_regression$Popularity),
  Rsquare = caret::R2(reg.pcr.pred, data_test_regression$Popularity)
)
pred_fit <- cbind(reg.pcr.pred,data_test_regression[,10])

typeof(data_test_regression$Popularity)
# comparison of pcr coefficients and R2 in full regression and train model

validationplot(reg.pcr.train, val.type = "R2")
validationplot(reg.pcr.train, val.type = "R2", add=TRUE, lwd=2)


