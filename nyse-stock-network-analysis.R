################################## Libraries ###################################
library(igraph)
library(readxl)
library(png)
library(plot.matrix)
library(entropy)
library(Matrix)
library(lattice)
library(vcd)
library(ggplot2)
library(qgraph)
library(devtools)
library(ComplexHeatmap)
library(stats)
library(nonlinearTseries)
library(infotheo)
library("writexl")
################################################################################
stocks1<-read_excel("stocks1.xlsx")

#############################################################################
# 1es diafores

stocks1_diff <- as.data.frame(diff(as.matrix(stocks1),differences = 2))
plot.ts(stocks1_diff$Ford)

Tesla_series_diff2 <- diff(stocks1$Tesla,differences = 2)
Tesla <- cbind(stocks1$Tesla, Tesla_series_diff2)
head(Tesla)
plot.ts(Tesla)

#Mutual Information
MI<- data.frame(matrix(ncol = 87, nrow = 87))
colnames(MI)<-colnames(stocks1_diff)
row.names(MI)<-colnames(stocks1_diff)

for (i in 1:length(stocks1_diff))
{
  for (j in 1:length(stocks1_diff)) {
    
    
    
    
    x<-mutualInformation(
      as.matrix(stocks1_diff[,i:j]),
      lag.max = 0,
      n.partitions = NULL,
      units = "Bits",
      do.plot = F
    )
    
    MI[i,j]<-as.numeric(x$mutual.information)
    
    
    
  }
}


diag(MI) = 0

Heatmap_colors=heat.colors(256, rev = T) 
Heatmap(as.data.frame(MI),
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        show_heatmap_legend=T,
        show_row_dend=F,
        show_column_dend = F,
        column_names_rot=90)



########  Row Data -> Correlations  ############################################
#Pearson Correlation
stocks1_diff_matrix_of_correlations       = cor(stocks1_diff,  method = "pearson")
diag(stocks1_diff_matrix_of_correlations) = 0

Heatmap_colors=heat.colors(256, rev = T) 
Heatmap(stocks1_diff_matrix_of_correlations,
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        show_heatmap_legend=T,
        show_row_dend=F,
        show_column_dend = F,
        column_names_rot=90)

########  Correlations -> Matrices -> Graphs  ##################################
#Matrices
cut_off=0
stocks1_diff_weight_matrix          = stocks1_diff_matrix_of_correlations*(stocks1_diff_matrix_of_correlations<=-cut_off) + stocks1_diff_matrix_of_correlations*(stocks1_diff_matrix_of_correlations>=cut_off)
stocks1_diff_positive_weight_matrix = stocks1_diff_matrix_of_correlations*(stocks1_diff_matrix_of_correlations>=cut_off)
stocks1_diff_negative_weight_matrix = abs(stocks1_diff_matrix_of_correlations*(stocks1_diff_matrix_of_correlations<=-cut_off))
stocks1_diff_abs_weight_matrix      = abs(stocks1_diff_weight_matrix)


# vgazw to cut off
#weight_matrix          = stocks1_diff_matrix_of_correlations
#positive_weight_matrix = stocks1_diff_matrix_of_correlations*(stocks1_diff_matrix_of_correlations>=0)
#negative_weight_matrix = abs(stocks1_diff_matrix_of_correlations*(stocks1_diff_matrix_of_correlations<=0))
#abs_weight_matrix      = abs(weight_matrix)

#Graphs with igraph
stocks1_diff_weighted_graph          = graph_from_adjacency_matrix(stocks1_diff_weight_matrix, weighted = T, mode = "undirected", diag = F)
stocks1_diff_positive_weighted_graph = graph_from_adjacency_matrix(stocks1_diff_positive_weight_matrix, weighted = T, mode = "undirected", diag = F)
#mst<- mst(weighted_graph)
#average.path.length(mst)
#set.seed(37)
#plot(mst, vertex.size=5, vertex.label.size=0.5, layout=layout_with_lgl, algorithm = "Prim")
stocks1_diff_negative_weighted_graph = graph_from_adjacency_matrix(stocks1_diff_negative_weight_matrix, weighted = T, mode = "undirected", diag = F)
stocks1_diff_abs_weighted_graph = graph_from_adjacency_matrix(stocks1_diff_abs_weight_matrix, weighted = T, mode = "undirected", diag = F)


e<-eigen_centrality(stocks1_diff_weighted_graph)
sort(e$vector, decreasing = T)
########  Index -> Centrality (LOCAL)  ############
#Degree
stocks1_diff_positive_weighted_degree        = rowSums(stocks1_diff_positive_weight_matrix)
stocks1_diff_negative_weighted_degree        = rowSums(stocks1_diff_negative_weight_matrix)
stocks1_diff_abs_weighted_degree             = rowSums(stocks1_diff_abs_weight_matrix)

stocks1_diff_positive_weighted_degree_sorted = sort(stocks1_diff_positive_weighted_degree,decreasing = T)
stocks1_diff_negative_weighted_degree_sorted = sort(stocks1_diff_negative_weighted_degree,decreasing = T)
stocks1_diff_abs_weighted_degree_sorted      = sort(stocks1_diff_abs_weighted_degree,decreasing = T)

#Degree Centrality
N=87
stocks1_diff_positive_weighted_degree_centrality = stocks1_diff_positive_weighted_degree/(N-1)
stocks1_diff_negative_weighted_degree_centrality = stocks1_diff_negative_weighted_degree/(N-1)
stocks1_diff_abs_weighted_degree_centrality = stocks1_diff_abs_weighted_degree/(N-1)


stocks1_diff_positive_weighted_degree_centrality_sorted = sort(stocks1_diff_positive_weighted_degree_centrality,decreasing = T)
stocks1_diff_negative_weighted_degree_centrality_sorted = sort(stocks1_diff_negative_weighted_degree_centrality,decreasing = T)
stocks1_diff_abs_weighted_degree_centrality_sorted = sort(stocks1_diff_abs_weighted_degree_centrality,decreasing = T)

# we make the heatmap for degree centrality
HEATMATRIX_stocks1_diff_positive_weighted_degree_centrality = cbind(stocks1_diff_positive_weighted_degree_centrality)
HEATMATRIX_stocks1_diff_negative_weighted_degree_centrality = cbind(stocks1_diff_negative_weighted_degree_centrality)
HEATMATRIX_stocks1_diff_abs_weighted_degree_centrality = cbind(stocks1_diff_abs_weighted_degree_centrality)

# the most influential node
HEATMATRIX_stocks1_diff_positive_weighted_degree_centrality_sorted = sort(rowSums(HEATMATRIX_stocks1_diff_positive_weighted_degree_centrality), decreasing = T)
HEATMATRIX_stocks1_diff_negative_weighted_degree_centrality_sorted = sort(rowSums(HEATMATRIX_stocks1_diff_negative_weighted_degree_centrality), decreasing = T)
HEATMATRIX_stocks1_diff_abs_weighted_degree_centrality_sorted = sort(rowSums(HEATMATRIX_stocks1_diff_abs_weighted_degree_centrality), decreasing = T)

Heatmap_colors=heat.colors(256, rev = T) 
Heatmap(HEATMATRIX_stocks1_diff_positive_weighted_degree_centrality, column_title="Heatmap for positive weighted degree centrality", 
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)

Heatmap(HEATMATRIX_stocks1_diff_negative_weighted_degree_centrality, column_title="Heatmap for negative weighted degree centrality",
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)

Heatmap(HEATMATRIX_stocks1_diff_abs_weighted_degree_centrality, column_title="Heatmap for absolute weighted degree centrality",
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)



######  Entropic Indices (LOCAL): Vertex Entropy ########
#Vertex Entropy 
stocks1_diff_positive_weighted_probability_distribution_matrix = stocks1_diff_positive_weight_matrix/stocks1_diff_positive_weighted_degree
log2matrix = log2(stocks1_diff_positive_weighted_probability_distribution_matrix)
#kano midenika ta (-inf) tou probability_distribution_matrix 
log2matrix[is.infinite(log2matrix)] <- 0
stocks1_diff_positive_weighted_vertex_entropy = -rowSums(stocks1_diff_positive_weighted_probability_distribution_matrix*log2matrix)
stocks1_diff_positive_weighted_normalized_vertex_entropy = stocks1_diff_positive_weighted_vertex_entropy/log2(N-1)

stocks1_diff_negative_weighted_probability_distribution_matrix = stocks1_diff_negative_weight_matrix/stocks1_diff_negative_weighted_degree
stocks1_diff_negative_weighted_probability_distribution_matrix[is.nan(stocks1_diff_negative_weighted_probability_distribution_matrix)]<-0
log2matrix = log2(stocks1_diff_negative_weighted_probability_distribution_matrix)
#kano midenika ta (-inf) tou probability_distribution_matrix 
log2matrix[is.infinite(log2matrix)] <- 0
stocks1_diff_negative_weighted_vertex_entropy = -rowSums(stocks1_diff_negative_weighted_probability_distribution_matrix*log2matrix)
stocks1_diff_negative_weighted_normalized_vertex_entropy = stocks1_diff_negative_weighted_vertex_entropy/log2(N-1)

stocks1_diff_abs_weighted_probability_distribution_matrix = stocks1_diff_abs_weight_matrix/stocks1_diff_abs_weighted_degree
log2matrix = log2(stocks1_diff_abs_weighted_probability_distribution_matrix)
#kano midenika ta (-inf) tou probability_distribution_matrix 
log2matrix[is.infinite(log2matrix)] <- 0
stocks1_diff_abs_weighted_vertex_entropy = -rowSums(stocks1_diff_abs_weighted_probability_distribution_matrix*log2matrix)
stocks1_diff_abs_weighted_normalized_vertex_entropy = stocks1_diff_abs_weighted_vertex_entropy/log2(N-1)


stocks1_diff_positive_weighted_normalized_vertex_entropy_sorted = sort(stocks1_diff_positive_weighted_normalized_vertex_entropy,decreasing = T)
stocks1_diff_negative_weighted_normalized_vertex_entropy_sorted = sort(stocks1_diff_negative_weighted_normalized_vertex_entropy,decreasing = T)
stocks1_diff_abs_weighted_normalized_vertex_entropy_sorted = sort(stocks1_diff_abs_weighted_normalized_vertex_entropy,decreasing = T)

# we make the heatmap
HEATMATRIX_stocks1_diff_positive_weighted_normalized_vertex_entropy = cbind(stocks1_diff_positive_weighted_normalized_vertex_entropy)
HEATMATRIX_stocks1_diff_negative_weighted_normalized_vertex_entropy = cbind(stocks1_diff_negative_weighted_normalized_vertex_entropy)
HEATMATRIX_stocks1_diff_abs_weighted_normalized_vertex_entropy = cbind(stocks1_diff_abs_weighted_normalized_vertex_entropy)



# the most influential node
HEATMATRIX_stocks1_diff_positive_weighted_normalized_vertex_entropy_sorted = sort(rowSums(HEATMATRIX_stocks1_diff_positive_weighted_normalized_vertex_entropy), decreasing = T)
HEATMATRIX_stocks1_diff_negative_weighted_normalized_vertex_entropy_sorted = sort(rowSums(HEATMATRIX_stocks1_diff_negative_weighted_normalized_vertex_entropy), decreasing = T)
HEATMATRIX_stocks1_diff_abs_weighted_normalized_vertex_entropy_sorted = sort(rowSums(HEATMATRIX_stocks1_diff_abs_weighted_normalized_vertex_entropy), decreasing = T)



Heatmap_colors=heat.colors(256, rev = T) 
Heatmap(HEATMATRIX_stocks1_diff_positive_weighted_normalized_vertex_entropy,column_title="Heatmap for positive vertex entropy",
        col = Heatmap_colors,
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom",
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)

Heatmap(HEATMATRIX_stocks1_diff_negative_weighted_normalized_vertex_entropy,column_title="Heatmap for negative vertex entropy",
        col = Heatmap_colors,
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom",
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)

Heatmap(HEATMATRIX_stocks1_diff_abs_weighted_normalized_vertex_entropy,column_title="Heatmap for absolute vertex entropy",
        col = Heatmap_colors,
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom",
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)

################################################################################
################################################################################
################################################################################

rescale <- function(nchar,low,high) {   
  min_d <- min(nchar)                  
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low  
  rscl
}

# Make the distribution of vertex entropy
par(mfrow=c(1,2))
hist(stocks1_diff_abs_weighted_normalized_vertex_entropy,                        
     breaks="Sturges",
     col="turquoise",
     xlab="Entropy", 
     ylab="Frequency",
     main=list("Distribution of Vertex Entropy",
               cex=0.6) )


# Plot the network according to vertex entropy 
sizec <-3*log(rescale(stocks1_diff_abs_weighted_normalized_vertex_entropy, 1, 87))
sizec
c3 <- rainbow(n = 25, alpha = 0.6)
c1 <- rainbow( 7, alpha = .7)[4]

plot(stocks1_diff_abs_weighted_graph , layout=layout_with_lgl,            
     edge.width=2,                  
     edge.color="darkgray",           
     vertex.shape="circle",           
     vertex.frame.color= c1,     
     vertex.color= c3[1:25], 
     vertex.size=sizec,                  
     vertex.label.font=2,
     vertex.label.family="sans",
     vertex.label.cex=0.7,          
     vertex.label.dist=2,       
     vertex.label.degree=-pi/2,
     main = list("Vertex Entropy",
                 cex=1) )
dev.off()

########################  GLOBAL Indices: Density ##############################
#Density
N=87
stocks1_diff_positive_weighted_density = sum(stocks1_diff_positive_weight_matrix)/(N*(N-1))
stocks1_diff_negative_weighted_density = sum(stocks1_diff_negative_weight_matrix )/(N*(N-1))
stocks1_diff_abs_weighted_density = sum(stocks1_diff_abs_weight_matrix)/(N*(N-1))





mi.empirical(mi, unit = "log2")


m<-mutualInformation(stocks1_diff$Ford,  to="all", units = "Bits")














################################################################################
################################################################################
################################################################################
################################################################################

################################################################################
stocks2<-read_excel("stocks2.xlsx")

#############################################################################
# 1es diafores

stocks2_diff <- as.data.frame(diff(as.matrix(stocks2),differences = 2))
plot.ts(stocks2_diff$Ford)

tseries_diff2 <- diff(stocks2$Tesla,differences = 2)
tm <- cbind(stocks2$Tesla, tseries_diff2)
head(tm)
plot.ts(tm)


#Mutual Information
MI<- data.frame(matrix(ncol = 87, nrow = 87))
colnames(MI)<-colnames(stocks2_diff)
row.names(MI)<-colnames(stocks2_diff)

for (i in 1:length(stocks2_diff))
{
  for (j in 1:length(stocks2_diff)) {
    
    
    
    
    x<-mutualInformation(
      as.matrix(stocks2_diff[,i:j]),
      lag.max = 0,
      n.partitions = NULL,
      units = "Bits",
      do.plot = F
    )
    
    MI[i,j]<-as.numeric(x$mutual.information)
    
    
    
  }
}


diag(MI) = 0

Heatmap_colors=heat.colors(256, rev = T) 
Heatmap(as.data.frame(MI),
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        show_heatmap_legend=T,
        show_row_dend=F,
        show_column_dend = F,
        column_names_rot=90)


########  Row Data -> Correlations  ############################################
#Pearson Correlation
stocks2_diff_matrix_of_correlations       = cor(stocks2_diff,  method = "pearson")
diag(stocks2_diff_matrix_of_correlations) = 0


Heatmap_colors=heat.colors(256, rev = T) 
Heatmap(stocks2_diff_matrix_of_correlations,
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        show_heatmap_legend=T,
        show_row_dend=F,
        show_column_dend = F,
        column_names_rot=90)


########  Correlations -> Matrices -> Graphs  ##################################
#Matrices
cut_off=0
stocks2_diff_weight_matrix          = stocks2_diff_matrix_of_correlations*(stocks2_diff_matrix_of_correlations<=-cut_off) + stocks2_diff_matrix_of_correlations*(stocks2_diff_matrix_of_correlations>=cut_off)
stocks2_diff_positive_weight_matrix = stocks2_diff_matrix_of_correlations*(stocks2_diff_matrix_of_correlations>=cut_off)
stocks2_diff_negative_weight_matrix = abs(stocks2_diff_matrix_of_correlations*(stocks2_diff_matrix_of_correlations<=-cut_off))
stocks2_diff_abs_weight_matrix      = abs(stocks2_diff_weight_matrix)


# vgazw to cut off
#weight_matrix          = stocks2_diff_matrix_of_correlations
#positive_weight_matrix = stocks2_diff_matrix_of_correlations*(stocks2_diff_matrix_of_correlations>=0)
#negative_weight_matrix = abs(stocks2_diff_matrix_of_correlations*(stocks2_diff_matrix_of_correlations<=0))
#abs_weight_matrix      = abs(weight_matrix)

#Graphs with igraph
stocks2_diff_weighted_graph          = graph_from_adjacency_matrix(stocks2_diff_weight_matrix, weighted = T, mode = "undirected", diag = F)
stocks2_diff_positive_weighted_graph = graph_from_adjacency_matrix(stocks2_diff_positive_weight_matrix, weighted = T, mode = "undirected", diag = F)
#mst<- mst(weighted_graph)
#average.path.length(mst)
#set.seed(37)
#plot(mst, vertex.size=5, vertex.label.size=0.5, layout=layout_with_lgl, algorithm = "Prim")
stocks2_diff_negative_weighted_graph = graph_from_adjacency_matrix(stocks2_diff_negative_weight_matrix, weighted = T, mode = "undirected", diag = F)
stocks2_diff_abs_weighted_graph = graph_from_adjacency_matrix(stocks2_diff_abs_weight_matrix, weighted = T, mode = "undirected", diag = F)


########  Index -> Centrality (LOCAL)  ############
#Degree
stocks2_diff_positive_weighted_degree        = rowSums(stocks2_diff_positive_weight_matrix)
stocks2_diff_negative_weighted_degree        = rowSums(stocks2_diff_negative_weight_matrix)
stocks2_diff_abs_weighted_degree             = rowSums(stocks2_diff_abs_weight_matrix)

stocks2_diff_positive_weighted_degree_sorted = sort(stocks2_diff_positive_weighted_degree,decreasing = T)
stocks2_diff_negative_weighted_degree_sorted = sort(stocks2_diff_negative_weighted_degree,decreasing = T)
stocks2_diff_abs_weighted_degree_sorted      = sort(stocks2_diff_abs_weighted_degree,decreasing = T)

#Degree Centrality
N=87
stocks2_diff_positive_weighted_degree_centrality = stocks2_diff_positive_weighted_degree/(N-1)
stocks2_diff_negative_weighted_degree_centrality = stocks2_diff_negative_weighted_degree/(N-1)
stocks2_diff_abs_weighted_degree_centrality = stocks2_diff_abs_weighted_degree/(N-1)


stocks2_diff_positive_weighted_degree_centrality_sorted = sort(stocks2_diff_positive_weighted_degree_centrality,decreasing = T)
stocks2_diff_negative_weighted_degree_centrality_sorted = sort(stocks2_diff_negative_weighted_degree_centrality,decreasing = T)
stocks2_diff_abs_weighted_degree_centrality_sorted = sort(stocks2_diff_abs_weighted_degree_centrality,decreasing = T)

# we make the heatmap for degree centrality
HEATMATRIX_stocks2_diff_positive_weighted_degree_centrality = cbind(stocks2_diff_positive_weighted_degree_centrality)
HEATMATRIX_stocks2_diff_negative_weighted_degree_centrality = cbind(stocks2_diff_negative_weighted_degree_centrality)
HEATMATRIX_stocks2_diff_abs_weighted_degree_centrality = cbind(stocks2_diff_abs_weighted_degree_centrality)

# the most influential node
HEATMATRIX_stocks2_diff_positive_weighted_degree_centrality_sorted = sort(rowSums(HEATMATRIX_stocks2_diff_positive_weighted_degree_centrality), decreasing = T)
HEATMATRIX_stocks2_diff_negative_weighted_degree_centrality_sorted = sort(rowSums(HEATMATRIX_stocks2_diff_negative_weighted_degree_centrality), decreasing = T)
HEATMATRIX_stocks2_diff_abs_weighted_degree_centrality_sorted = sort(rowSums(HEATMATRIX_stocks2_diff_abs_weighted_degree_centrality), decreasing = T)

Heatmap_colors=heat.colors(256, rev = T) 
Heatmap(HEATMATRIX_stocks2_diff_positive_weighted_degree_centrality,column_title="Heatmap for positive weighted degree centrality",  
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)

Heatmap(HEATMATRIX_stocks2_diff_negative_weighted_degree_centrality, column_title="Heatmap for negative weighted degree centrality", 
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)

Heatmap(HEATMATRIX_stocks2_diff_abs_weighted_degree_centrality, column_title="Heatmap for absolute weighted degree centrality", 
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)



######  Entropic Indices (LOCAL): Vertex Entropy ########
#Vertex Entropy 
stocks2_diff_positive_weighted_probability_distribution_matrix = stocks2_diff_positive_weight_matrix/stocks2_diff_positive_weighted_degree
log2matrix = log2(stocks2_diff_positive_weighted_probability_distribution_matrix)
#kano midenika ta (-inf) tou probability_distribution_matrix 
log2matrix[is.infinite(log2matrix)] <- 0
stocks2_diff_positive_weighted_vertex_entropy = -rowSums(stocks2_diff_positive_weighted_probability_distribution_matrix*log2matrix)
stocks2_diff_positive_weighted_normalized_vertex_entropy = stocks2_diff_positive_weighted_vertex_entropy/log2(N-1)

stocks2_diff_negative_weighted_probability_distribution_matrix = stocks2_diff_negative_weight_matrix/stocks2_diff_negative_weighted_degree
stocks2_diff_negative_weighted_probability_distribution_matrix[is.nan(stocks2_diff_negative_weighted_probability_distribution_matrix)]<-0
log2matrix = log2(stocks2_diff_negative_weighted_probability_distribution_matrix)
#kano midenika ta (-inf) tou probability_distribution_matrix 
log2matrix[is.infinite(log2matrix)] <- 0
stocks2_diff_negative_weighted_vertex_entropy = -rowSums(stocks2_diff_negative_weighted_probability_distribution_matrix*log2matrix)
stocks2_diff_negative_weighted_normalized_vertex_entropy = stocks2_diff_negative_weighted_vertex_entropy/log2(N-1)

stocks2_diff_abs_weighted_probability_distribution_matrix = stocks2_diff_abs_weight_matrix/stocks2_diff_abs_weighted_degree
log2matrix = log2(stocks2_diff_abs_weighted_probability_distribution_matrix)
#kano midenika ta (-inf) tou probability_distribution_matrix 
log2matrix[is.infinite(log2matrix)] <- 0
stocks2_diff_abs_weighted_vertex_entropy = -rowSums(stocks2_diff_abs_weighted_probability_distribution_matrix*log2matrix)
stocks2_diff_abs_weighted_normalized_vertex_entropy = stocks2_diff_abs_weighted_vertex_entropy/log2(N-1)


stocks2_diff_positive_weighted_normalized_vertex_entropy_sorted = sort(stocks2_diff_positive_weighted_normalized_vertex_entropy,decreasing = T)
stocks2_diff_negative_weighted_normalized_vertex_entropy_sorted = sort(stocks2_diff_negative_weighted_normalized_vertex_entropy,decreasing = T)
stocks2_diff_abs_weighted_normalized_vertex_entropy_sorted = sort(stocks2_diff_abs_weighted_normalized_vertex_entropy,decreasing = T)

# we make the heatmap
HEATMATRIX_stocks2_diff_positive_weighted_normalized_vertex_entropy = cbind(stocks2_diff_positive_weighted_normalized_vertex_entropy)
HEATMATRIX_stocks2_diff_negative_weighted_normalized_vertex_entropy = cbind(stocks2_diff_negative_weighted_normalized_vertex_entropy)
HEATMATRIX_stocks2_diff_abs_weighted_normalized_vertex_entropy = cbind(stocks2_diff_abs_weighted_normalized_vertex_entropy)



# the most influential node
HEATMATRIX_stocks2_diff_positive_weighted_normalized_vertex_entropy_sorted = sort(rowSums(HEATMATRIX_stocks2_diff_positive_weighted_normalized_vertex_entropy), decreasing = T)
HEATMATRIX_stocks2_diff_negative_weighted_normalized_vertex_entropy_sorted = sort(rowSums(HEATMATRIX_stocks2_diff_negative_weighted_normalized_vertex_entropy), decreasing = T)
HEATMATRIX_stocks2_diff_abs_weighted_normalized_vertex_entropy_sorted = sort(rowSums(HEATMATRIX_stocks2_diff_abs_weighted_normalized_vertex_entropy), decreasing = T)



Heatmap_colors=heat.colors(256, rev = T) 
Heatmap(HEATMATRIX_stocks2_diff_positive_weighted_normalized_vertex_entropy,column_title="Heatmap for positive vertex entropy", 
        col = Heatmap_colors,
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom",
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)

Heatmap(HEATMATRIX_stocks2_diff_negative_weighted_normalized_vertex_entropy,column_title="Heatmap for negative vertex entropy", 
        col = Heatmap_colors,
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom",
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)

Heatmap(HEATMATRIX_stocks2_diff_abs_weighted_normalized_vertex_entropy,column_title="Heatmap for absolute vertex entropy", 
        col = Heatmap_colors,
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom",
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)
################################################################################
################################################################################
################################################################################
rescale <- function(nchar,low,high) {   
  min_d <- min(nchar)                  
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low  
  rscl
}

# Make the distribution of vertex entropy
par(mfrow=c(1,2))
hist(stocks2_diff_abs_weighted_normalized_vertex_entropy,                        
     breaks="Sturges",
     col="turquoise",
     xlab="Entropy", 
     ylab="Frequency",
     main=list("Distribution of Vertex Entropy",
               cex=0.6) )


# Plot the network according to vertex entropy 
sizec <-3*log(rescale(stocks2_diff_abs_weighted_normalized_vertex_entropy, 1, 87))
sizec
c3 <- rainbow(n = 25, alpha = 0.6)
c1 <- rainbow( 7, alpha = .7)[4]

plot(stocks2_diff_abs_weighted_graph , layout=layout_with_lgl,            
     edge.width=2,                  
     edge.color="darkgray",           
     vertex.shape="circle",           
     vertex.frame.color= c1,     
     vertex.color= c3[1:25], 
     vertex.size=sizec,                  
     vertex.label.font=2,
     vertex.label.family="sans",
     vertex.label.cex=0.7,          
     vertex.label.dist=2,       
     vertex.label.degree=-pi/2,
     main = list("Vertex Entropy",
                 cex=1) )
dev.off()

########################  GLOBAL Indices: Density ##############################
#Density
N=87
stocks2_diff_positive_weighted_density = sum(stocks2_diff_positive_weight_matrix)/(N*(N-1))
stocks2_diff_negative_weighted_density = sum(stocks2_diff_negative_weight_matrix )/(N*(N-1))
stocks2_diff_abs_weighted_density = sum(stocks2_diff_abs_weight_matrix)/(N*(N-1))



################################################################################
################################################################################
################################################################################
################################################################################

################################################################################
stocks3<-read_excel("stocks3.xlsx")

#############################################################################
# 1es diafores

stocks3_diff <- as.data.frame(diff(as.matrix(stocks3),differences = 2))
plot.ts(stocks3_diff$Ford)

tseries_diff2 <- diff(stocks3$Tesla,differences = 2)
tm <- cbind(stocks3$Tesla, tseries_diff2)
head(tm)
plot.ts(tm)


#Mutual Information
MI<- data.frame(matrix(ncol = 87, nrow = 87))
colnames(MI)<-colnames(stocks3_diff)
row.names(MI)<-colnames(stocks3_diff)

for (i in 1:length(stocks3_diff))
{
  for (j in 1:length(stocks3_diff)) {
    
    
    
    
    x<-mutualInformation(
      as.matrix(stocks3_diff[,i:j]),
      lag.max = 0,
      n.partitions = NULL,
      units = "Bits",
      do.plot = F
    )
    
    MI[i,j]<-as.numeric(x$mutual.information)
    
    
    
  }
}


diag(MI) = 0

Heatmap_colors=heat.colors(256, rev = T) 
Heatmap(as.data.frame(MI),
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        show_heatmap_legend=T,
        show_row_dend=F,
        show_column_dend = F,
        column_names_rot=90)


########  Row Data -> Correlations  ############################################
#Pearson Correlation
stocks3_diff_matrix_of_correlations       = cor(stocks3_diff,  method = "pearson")
diag(stocks3_diff_matrix_of_correlations) = 0


Heatmap_colors=heat.colors(256, rev = T) 
Heatmap(stocks3_diff_matrix_of_correlations,
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        show_heatmap_legend=T,
        show_row_dend=F,
        show_column_dend = F,
        column_names_rot=90)

########  Correlations -> Matrices -> Graphs  ##################################
#Matrices
cut_off=0
stocks3_diff_weight_matrix          = stocks3_diff_matrix_of_correlations*(stocks3_diff_matrix_of_correlations<=-cut_off) + stocks3_diff_matrix_of_correlations*(stocks3_diff_matrix_of_correlations>=cut_off)
stocks3_diff_positive_weight_matrix = stocks3_diff_matrix_of_correlations*(stocks3_diff_matrix_of_correlations>=cut_off)
stocks3_diff_negative_weight_matrix = abs(stocks3_diff_matrix_of_correlations*(stocks3_diff_matrix_of_correlations<=-cut_off))
stocks3_diff_abs_weight_matrix      = abs(stocks3_diff_weight_matrix)


# vgazw to cut off
#weight_matrix          = stocks3_diff_matrix_of_correlations
#positive_weight_matrix = stocks3_diff_matrix_of_correlations*(stocks3_diff_matrix_of_correlations>=0)
#negative_weight_matrix = abs(stocks3_diff_matrix_of_correlations*(stocks3_diff_matrix_of_correlations<=0))
#abs_weight_matrix      = abs(weight_matrix)

#Graphs with igraph
stocks3_diff_weighted_graph          = graph_from_adjacency_matrix(stocks3_diff_weight_matrix, weighted = T, mode = "undirected", diag = F)
stocks3_diff_positive_weighted_graph = graph_from_adjacency_matrix(stocks3_diff_positive_weight_matrix, weighted = T, mode = "undirected", diag = F)
#mst<- mst(weighted_graph)
#average.path.length(mst)
#set.seed(37)
#plot(mst, vertex.size=5, vertex.label.size=0.5, layout=layout_with_lgl, algorithm = "Prim")
stocks3_diff_negative_weighted_graph = graph_from_adjacency_matrix(stocks3_diff_negative_weight_matrix, weighted = T, mode = "undirected", diag = F)
stocks3_diff_abs_weighted_graph = graph_from_adjacency_matrix(stocks3_diff_abs_weight_matrix, weighted = T, mode = "undirected", diag = F)


########  Index -> Centrality (LOCAL)  ############
#Degree
stocks3_diff_positive_weighted_degree        = rowSums(stocks3_diff_positive_weight_matrix)
stocks3_diff_negative_weighted_degree        = rowSums(stocks3_diff_negative_weight_matrix)
stocks3_diff_abs_weighted_degree             = rowSums(stocks3_diff_abs_weight_matrix)

stocks3_diff_positive_weighted_degree_sorted = sort(stocks3_diff_positive_weighted_degree,decreasing = T)
stocks3_diff_negative_weighted_degree_sorted = sort(stocks3_diff_negative_weighted_degree,decreasing = T)
stocks3_diff_abs_weighted_degree_sorted      = sort(stocks3_diff_abs_weighted_degree,decreasing = T)

#Degree Centrality
N=87
stocks3_diff_positive_weighted_degree_centrality = stocks3_diff_positive_weighted_degree/(N-1)
stocks3_diff_negative_weighted_degree_centrality = stocks3_diff_negative_weighted_degree/(N-1)
stocks3_diff_abs_weighted_degree_centrality = stocks3_diff_abs_weighted_degree/(N-1)


stocks3_diff_positive_weighted_degree_centrality_sorted = sort(stocks3_diff_positive_weighted_degree_centrality,decreasing = T)
stocks3_diff_negative_weighted_degree_centrality_sorted = sort(stocks3_diff_negative_weighted_degree_centrality,decreasing = T)
stocks3_diff_abs_weighted_degree_centrality_sorted = sort(stocks3_diff_abs_weighted_degree_centrality,decreasing = T)

# we make the heatmap for degree centrality
HEATMATRIX_stocks3_diff_positive_weighted_degree_centrality = cbind(stocks3_diff_positive_weighted_degree_centrality)
HEATMATRIX_stocks3_diff_negative_weighted_degree_centrality = cbind(stocks3_diff_negative_weighted_degree_centrality)
HEATMATRIX_stocks3_diff_abs_weighted_degree_centrality = cbind(stocks3_diff_abs_weighted_degree_centrality)

# the most influential node
HEATMATRIX_stocks3_diff_positive_weighted_degree_centrality_sorted = sort(rowSums(HEATMATRIX_stocks3_diff_positive_weighted_degree_centrality), decreasing = T)
HEATMATRIX_stocks3_diff_negative_weighted_degree_centrality_sorted = sort(rowSums(HEATMATRIX_stocks3_diff_negative_weighted_degree_centrality), decreasing = T)
HEATMATRIX_stocks3_diff_abs_weighted_degree_centrality_sorted = sort(rowSums(HEATMATRIX_stocks3_diff_abs_weighted_degree_centrality), decreasing = T)

Heatmap_colors=heat.colors(256, rev = T) 
Heatmap(HEATMATRIX_stocks3_diff_positive_weighted_degree_centrality, column_title="Heatmap for positive weighted degree centrality", 
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)

Heatmap(HEATMATRIX_stocks3_diff_negative_weighted_degree_centrality,  column_title="Heatmap for negative weighted degree centrality", 
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)

Heatmap(HEATMATRIX_stocks3_diff_abs_weighted_degree_centrality, column_title="Heatmap for absolute weighted degree centrality", 
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom", 
        col = Heatmap_colors,
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)



######  Entropic Indices (LOCAL): Vertex Entropy ########
#Vertex Entropy 
stocks3_diff_positive_weighted_probability_distribution_matrix = stocks3_diff_positive_weight_matrix/stocks3_diff_positive_weighted_degree
log2matrix = log2(stocks3_diff_positive_weighted_probability_distribution_matrix)
#kano midenika ta (-inf) tou probability_distribution_matrix 
log2matrix[is.infinite(log2matrix)] <- 0
stocks3_diff_positive_weighted_vertex_entropy = -rowSums(stocks3_diff_positive_weighted_probability_distribution_matrix*log2matrix)
stocks3_diff_positive_weighted_normalized_vertex_entropy = stocks3_diff_positive_weighted_vertex_entropy/log2(N-1)

stocks3_diff_negative_weighted_probability_distribution_matrix = stocks3_diff_negative_weight_matrix/stocks3_diff_negative_weighted_degree
stocks3_diff_negative_weighted_probability_distribution_matrix[is.nan(stocks3_diff_negative_weighted_probability_distribution_matrix)]<-0
log2matrix = log2(stocks3_diff_negative_weighted_probability_distribution_matrix)
#kano midenika ta (-inf) tou probability_distribution_matrix 
log2matrix[is.infinite(log2matrix)] <- 0
stocks3_diff_negative_weighted_vertex_entropy = -rowSums(stocks3_diff_negative_weighted_probability_distribution_matrix*log2matrix)
stocks3_diff_negative_weighted_normalized_vertex_entropy = stocks3_diff_negative_weighted_vertex_entropy/log2(N-1)

stocks3_diff_abs_weighted_probability_distribution_matrix = stocks3_diff_abs_weight_matrix/stocks3_diff_abs_weighted_degree
log2matrix = log2(stocks3_diff_abs_weighted_probability_distribution_matrix)
#kano midenika ta (-inf) tou probability_distribution_matrix 
log2matrix[is.infinite(log2matrix)] <- 0
stocks3_diff_abs_weighted_vertex_entropy = -rowSums(stocks3_diff_abs_weighted_probability_distribution_matrix*log2matrix)
stocks3_diff_abs_weighted_normalized_vertex_entropy = stocks3_diff_abs_weighted_vertex_entropy/log2(N-1)


stocks3_diff_positive_weighted_normalized_vertex_entropy_sorted = sort(stocks3_diff_positive_weighted_normalized_vertex_entropy,decreasing = T)
stocks3_diff_negative_weighted_normalized_vertex_entropy_sorted = sort(stocks3_diff_negative_weighted_normalized_vertex_entropy,decreasing = T)
stocks3_diff_abs_weighted_normalized_vertex_entropy_sorted = sort(stocks3_diff_abs_weighted_normalized_vertex_entropy,decreasing = T)

# we make the heatmap
HEATMATRIX_stocks3_diff_positive_weighted_normalized_vertex_entropy = cbind(stocks3_diff_positive_weighted_normalized_vertex_entropy)
HEATMATRIX_stocks3_diff_negative_weighted_normalized_vertex_entropy = cbind(stocks3_diff_negative_weighted_normalized_vertex_entropy)
HEATMATRIX_stocks3_diff_abs_weighted_normalized_vertex_entropy = cbind(stocks3_diff_abs_weighted_normalized_vertex_entropy)



# the most influential node
HEATMATRIX_stocks3_diff_positive_weighted_normalized_vertex_entropy_sorted = sort(rowSums(HEATMATRIX_stocks3_diff_positive_weighted_normalized_vertex_entropy), decreasing = T)
HEATMATRIX_stocks3_diff_negative_weighted_normalized_vertex_entropy_sorted = sort(rowSums(HEATMATRIX_stocks3_diff_negative_weighted_normalized_vertex_entropy), decreasing = T)
HEATMATRIX_stocks3_diff_abs_weighted_normalized_vertex_entropy_sorted = sort(rowSums(HEATMATRIX_stocks3_diff_abs_weighted_normalized_vertex_entropy), decreasing = T)



Heatmap_colors=heat.colors(256, rev = T) 
Heatmap(HEATMATRIX_stocks3_diff_positive_weighted_normalized_vertex_entropy,column_title="Heatmap for positive vertex entropy",
        col = Heatmap_colors,
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom",
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)

Heatmap(HEATMATRIX_stocks3_diff_negative_weighted_normalized_vertex_entropy,column_title="Heatmap for negative vertex entropy",
        col = Heatmap_colors,
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom",
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)

Heatmap(HEATMATRIX_stocks3_diff_abs_weighted_normalized_vertex_entropy,column_title="Heatmap for absolute vertex entropy",
        col = Heatmap_colors,
        column_names_side = "top",
        row_names_side = "left", 
        row_dend_side = "right",
        column_dend_side = "bottom",
        column_labels = "",
        show_heatmap_legend=T,
        show_row_dend=F,
        column_names_rot=0)
################################################################################
################################################################################
################################################################################
rescale <- function(nchar,low,high) {   
  min_d <- min(nchar)                  
  max_d <- max(nchar)
  rscl <- ((high-low)*(nchar-min_d))/(max_d-min_d)+low  
  rscl
}

# Make the distribution of vertex entropy
par(mfrow=c(1,2))
hist(stocks3_diff_abs_weighted_normalized_vertex_entropy,                        
     breaks="Sturges",
     col="turquoise",
     xlab="Entropy", 
     ylab="Frequency",
     main=list("Distribution of Vertex Entropy",
               cex=0.6) )


# Plot the network according to vertex entropy 
sizec <-3*log(rescale(stocks3_diff_abs_weighted_normalized_vertex_entropy, 1, 87))
sizec
c3 <- rainbow(n = 25, alpha = 0.6)
c1 <- rainbow( 7, alpha = .7)[4]

plot(stocks3_diff_abs_weighted_graph , layout=layout_with_lgl,            
     edge.width=2,                  
     edge.color="darkgray",           
     vertex.shape="circle",           
     vertex.frame.color= c1,     
     vertex.color= c3[1:25], 
     vertex.size=sizec,                  
     vertex.label.font=2,
     vertex.label.family="sans",
     vertex.label.cex=0.7,          
     vertex.label.dist=2,       
     vertex.label.degree=-pi/2,
     main = list("Vertex Entropy",
                 cex=1) )
dev.off()

########################  GLOBAL Indices: Density ##############################
#Density
N=87
stocks3_diff_positive_weighted_density = sum(stocks3_diff_positive_weight_matrix)/(N*(N-1))
stocks3_diff_negative_weighted_density = sum(stocks3_diff_negative_weight_matrix )/(N*(N-1))
stocks3_diff_abs_weighted_density = sum(stocks3_diff_abs_weight_matrix)/(N*(N-1))





