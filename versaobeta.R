#Última versão 19-06-2023

# libraries
library(cluster)
library(factoextra)
library(stats)
library(scatterplot3d)
library(openxlsx)
library(car)
library(rgl)


# Load data
dadosara<- read.delim("sara1.txt") # # dados em cossenos diretores e todos os parâmetros 
#dados <- read.delim("hammah2.txt") # dados em cossenos diretores e todos os parâmetros
dados<-dadosara
head(dados)

dados1 <- read.delim("hammah2.txt") # dados originais em dip/direction e grupos originais.
head(dados1)

dori=cbind(dados1[,1:2], dados,dados1$cluster.original) # dados com dip/direction, parâmetros e grupos originais.
head(dori)

# Remove any missing value (i.e, NA values for not available)
dados <- na.omit(dados)

# Scale variables


#dados<- scale(dados[,4:5], center = TRUE, scale = TRUE)


#Determine the optimal number of clusters for k-means clustering:

fviz_nbclust(dados, kmeans,
             method = "gap_stat")

#Compute and visualize k-means clustering:

set.seed(123)
km.res <- kmeans(dados, 4, nstart = 25) # numero de famílias (4)
#Km.res= resultado dos agrupamentos.


# Visualize

fviz_cluster(km.res, data = dados, 
             ellipse.type = "convex",
             palette = "jco",
             repel = TRUE,
             ggtheme = theme_minimal())





result=cbind(dados1[,1:2], cluster=km.res$cluster) # adiciona o resultado de agrupamento K-means à matriz original de dados
head(result)




# exporta o resultado do kmeans para o arquivo txt

#write.table(x =result, file = "resultadokmeans.txt", sep = " \t",
#  row.names = FALSE, col.names = TRUE)




# Scatter Plot plota o resultado do k-means

shapes = c(15, 16, 13, 19) 
shapes <- shapes[as.numeric(result$cluster)]
colors <- c("red", "#E69F00", "black", "#0000FF")
colors <- colors[as.numeric(result$cluster)]


scatterplot3d(result$Dip,result$dir,dori$fill, angle =30, pch = shapes,color=colors, grid=TRUE, box=FALSE)


legend("topright",legend=c("Cluster 1", "Cluster 2","Cluster 3","Cluster 4"), col=c("red", "#E69F00", "black","#0000FF"),
       pch=c(15, 16, 13, 19) , bty="n")



#graph 3D
Dip=dori$Dip
Direction=dori$dir
Pren=dori$fill
Prenp=dori$pf

scatter3d(x=Dip,y=Prenp,z=Direction, groups=as.factor(result$cluster),grid = TRUE, surface=FALSE,surface.col = c("red", "#E69F00", "black","#0000FF"),
          axis.ticks = TRUE, revolutions=0,sphere.size=2, xlab="Dip",ylab = "Fill",zlab = "Direction")



#Identify3d(x=Dip,y=Pren,z=Direction,labels = 1:length(x))


#exporta os resultados do K-means para o excel

write.xlsx(result, file = "resulkm.xlsx")


# PCA

# compute PCA


res.pca <- prcomp(dados, scale = TRUE)

#Visualize eigenvalues (scree plot). Show the percentage of variances explained by each principal component.

#Graph of variables. Positive correlated variables point to the same side of the plot. Negative correlated variables point to opposite sides of the graph.

fviz_eig(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)


res.pca$x


#acess to pca results

# Eigenvalues
eig.val <- get_eigenvalue(res.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
# Results for individuals
res.ind <- get_pca_ind(res.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

Feature=res.ind$coord[,1:3] 
head(Feature)
