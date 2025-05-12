## ------------------------------------------------------------------------------------------------------
# read data
town <- readxl::read_excel('town.xlsx')

## ------------------------------------------------------------------------------------------------------

# delete non-value data
cols <- paste0("x", 1:48)

town <- town[complete.cases(town[, cols]) & apply(town[, cols], 1, function(row) all(suppressWarnings(!is.na(as.numeric(row))))), ]
town

## ------------------------------------------------------------------------------------------------------

library(clusterSim)


## ------------------------------------------------------------------------------------------------------

data_matrix <- as.matrix(town[, paste0("x", 1:48)])

data_matrix <- scale(data_matrix)


## ------------------------------------------------------------------------------------------------------
dbi_values <- numeric()
clusters <- 3:10  
for (k in clusters) {
  set.seed(123)  
  kmeans_result <- kmeans(data_matrix, centers = k)
  dbi <- index.DB(data_matrix, kmeans_result$cluster)$DB
  dbi_values <- c(dbi_values, dbi)
}

best_k <- clusters[which.min(dbi_values)]

cat("Best cluster number:", best_k, "\n")



## ------------------------------------------------------------------------------------------------------

library(ggplot2)
data<-data.frame(dbi_values

ggplot(dbi_values, aes(x = x, y = y)) +
  geom_line(color = "blue", size = 1.2) +  
  geom_point(color = "red", size = 3) +   
  labs(x = "cluster", y = "DBI", title = "DBI-cluster") +  
  theme_minimal() 

## ------------------------------------------------------------------------------------------------------
kmeans_result <- kmeans(data_matrix, centers = 6)

## ------------------------------------------------------------------------------------------------------
#
set.seed(123)
final_kmeans <- kmeans(data_matrix, centers = 6)

# 
town$cluster <- final_kmeans$cluster

## ------------------------------------------------------------------------------------------------------
library(readr)
# Specify the correct file path and use write_csv function properly
write_excel_csv(town, 'town')

## ------------------------------------------------------------------------------------------------------
# using 8 to cluster
set.seed(123)
final_kmeans <- kmeans(data_matrix, centers = 8)

# save result
town$cluster_8 <- final_kmeans$cluster

library(readr)
# Specify the correct file path and use write_csv function properly
write_excel_csv(town, 'town1.xlxs')

## ------------------------------------------------------------------------------------------------------
library(readr)
# Specify the correct file path and use write_csv function properly
write_excel_csv(town, 'town1.csv')





