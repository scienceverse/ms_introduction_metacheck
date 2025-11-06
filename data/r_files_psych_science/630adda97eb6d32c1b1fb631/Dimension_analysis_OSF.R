# Read in data for each group
# Please change PATH to your local path
#for CelebA online rating--fig1b
data <- read.csv("../Data/CelebA_Rating_NT.csv", header=T, row.names=1)
data <- read.csv("../Data/CelebA_Rating_ASD.csv", header=T, row.names=1)

#for CelebA in-lab rating--fig4b
#data <- read.csv("/PATH/CelebA_Rating_NT.csv", header=T, row.names=1)
#data <- read.csv("/PATH/CelebA_Rating_ASD.csv", header=T, row.names=1)

#for unfamiliar face control rating--fig5b:
#data <- read.csv("/PATH/Rate2_NT.csv", header=T, row.names=1)
#data <- read.csv("/PATH/Rate2_ASD.csv", header=T, row.names=1)

# Rotate the original data to face x trait for PCA 
data_PCA <- data.frame(t(data))

# plot heatmap of correlation between traits
library(reshape2)
library(ggplot2)
melted_cormat <- melt(cor(data_PCA))
svg("/Users/rncao/Library/CloudStorage/Box-Box/Private/Experiments/CelebARateAutism/OSF/Figures/Corr_Traits.svg",
    width = 8, height = 7)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_viridis_c(option = "B") + #inferno instead of viridis
  scale_x_discrete(expand = c(0,0)) + scale_y_discrete(expand = c(0,0)) +
  xlab("") + ylab("") + 
  theme(axis.text.y = element_text(size=14, face = "bold"),
        axis.text.x = element_text(size=14, face = "bold", angle = 45, hjust = 1)) 
dev.off()

# Compute explained variance of PCs without rotation
library(factoextra)
PCA <- prcomp(x=data_PCA, retx = TRUE, scale = FALSE)
fviz_eig(PCA)

library(psych)
PCA_none <- principal(data_PCA, nfactors=4, rotate="none")
print(PCA_none, sort=TRUE)

# Extract 4 PCs with Varimax Rotation for interpretation
PCA_varimax <- principal(data_PCA, nfactors=4, rotate="varimax")
print(PCA_varimax, sort=TRUE)

# Plot factor loadings
loading <- data.frame(PCA_varimax$loadings[1:8,])
names(loading) <- c("Warmth","Competence","Femininity","Youth")
# Seperate traits loaded most in each of the factor
loading$F1 <- 0
loading$F2 <- 0
loading$F3 <- 0
loading$F4 <- 0
for (i in c(1:nrow(loading))) {
  ifelse(max(abs(loading[i,]))==abs(loading[i,1]), loading$F1[i]<-1,
         ifelse(max(abs(loading[i,]))==abs(loading[i,2]), loading$F2[i]<-1,
                ifelse(max(abs(loading[i,]))==abs(loading[i,3]), loading$F3[i]<-1,loading$F4[i]<-1)))
}
loading_F1 <- loading[loading$F1 == 1,1:4]
loading_F2 <- loading[loading$F2 == 1,1:4]
loading_F3 <- loading[loading$F3 == 1,1:4]
loading_F4 <- loading[loading$F4 == 1,1:4]
# Sort traits according to the magnitude of the loadings within each most loaded factors
loading_F1 <- loading_F1[rev(order(loading_F1$`Warmth`)),]
loading_F2 <- loading_F2[rev(order(loading_F2$`Competence`)),]
loading_F3 <- loading_F3[rev(order(loading_F3$`Femininity`)),]
loading_F4 <- loading_F4[rev(order(loading_F4$`Youth`)),]
# Merge sorted data and Plot
loading_sorted <- rbind(loading_F1,loading_F2,loading_F3,loading_F4)
# convert wide-form data into long-form data
library(reshape2)
loading_sorted$X<- rownames(loading_sorted)
loading.m <- melt(loading_sorted, id="X",measure=c("Warmth", "Competence", "Femininity", "Youth"), variable.name="Factor", value.name="Loading")
loading.m$Traits <- factor(loading.m$X, levels = loading.m$X[rev(c(1:nrow(loading)))])
library(ggplot2)
svg("../Figures/PCA_Loading.svg",width=6,height=4)
ggplot(loading.m, aes(Traits, abs(Loading), fill=Loading)) + 
  facet_wrap(~ Factor, nrow=1) + #place the factors in separate facets
  geom_bar(stat="identity") + #make the bars
  coord_flip() + #flip the axes so the test names can be horizontal  
  #define the fill color gradient: blue=negative, red=positive
  scale_fill_gradient2(name = "Loading", 
                       high = "#f78a36", mid = "white", low = "#c322e6", 
                       midpoint=0, guide="none") +
  ylab("Loading Strength") + #improve y-axis label
  theme_bw(base_size=10) + #use a black-and0white theme with set font size
  theme(axis.title = element_text(size=10)) + theme(axis.text = element_text(size=10))
dev.off()

# Tucker indices of factor congruence
#install.packages("pracma") # procrustes rotation to align the factor structure between two groups before computing Tucker indices
library(pracma)
library(psych)
loading_NT <- data.frame(principal(data.frame(t(data)), nfactors=4, rotate="varimax")$loadings[1:8,])
loading_ASD <- data.frame(principal(data.frame(t(data)), nfactors=4, rotate="varimax")$loadings[1:8,])
M1 <- as.matrix(loading_NT)
M2 <- as.matrix(loading_ASD)
M2_tran <- procrustes(A=M1, B=M2)$P
Loadings <- list(M1, M2_tran)
M <- factor.congruence(x=Loadings, y=NULL,digits=2,use=NULL,structure=FALSE)
M[1:4, 5:8]



