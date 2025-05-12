## ------------------------------------------------------------------------------------------------------
new_center<-readxl::read_excel('shanghai_poi9_TableToExcel.xlsx')
new_center

## ------------------------------------------------------------------------------------------------------
colnames(new_center)


## ------------------------------------------------------------------------------------------------------
new_center[is.na(new_center) | new_center < 0] <- 0
# summary_
summary_stats <- summary(new_center[, c("aver_flow_center1","poi_center_result" ,"flow_result1","pop_scale_1",                "flow_weekday_center","flow_weekend_center","aver_flow_weekend_center","aver_flow_weekday_center" ,"floor_poi_center_1" )])
print(summary_stats)


## ------------------------------------------------------------------------------------------------------
library(mcr)
library(vegan)
## ------------------------------------------------------------------------------------------------------

# variables
variables <- c("aver_flow_center1","poi_center_result" ,"flow_result1","pop_scale_1","flow_weekday_center","flow_weekend_center","aver_flow_weekend_center","aver_flow_weekday_center" ,"floor_poi_center_1")

# pair-to-pair
combinations <- combn(variables, 2, simplify = FALSE)

# Create an empty vector to store the results of the Mantel statistic
r_values <- numeric(length(combinations))
p_values <- numeric(length(combinations))

var1 <- character(length(combinations))
var2 <- character(length(combinations))

#  Mantel statistic
for (i in seq_along(combinations)) {
  pair <- combinations[[i]]
  var1[i] <- pair[1]  
  var2[i] <- pair[2]  
  
  x_var <- new_center[[pair[1]]]
  y_var <- new_center[[pair[2]]]
  
  data_clean <- na.omit(data.frame(x_var, y_var))
  
  # sampling
  if (nrow(data_clean) > 0) {
    sample_size <- max(1, floor(0.01 * nrow(data_clean))) 
    set.seed(123)  
    data_sample <- data_clean[sample(nrow(data_clean), sample_size), ]
    
    # Check whether the variable has sufficient variability
    if (nrow(data_sample) > 1 && 
        length(unique(data_sample$x_var)) > 1 && 
        length(unique(data_sample$y_var)) > 1) {
      
      # Calculate the Euclidean distance matrix and conduct the Mantel test
      mantel_test <- mantel(dist(data_sample$x_var), dist(data_sample$y_var))
      
      # r and p 
      r_values[i] <- mantel_test$statistic
      p_values[i] <- mantel_test$signif
    } else {
      # If the variable does not have sufficient variability or there are not enough data rows, set it to NA
      r_values[i] <- NA
      p_values[i] <- NA
    }
  } else {
    # If there are no valid data rows, set it to NA如果没有有效数据行，设置为 NA
    r_values[i] <- NA
    p_values[i] <- NA
  }
}

# data framework
mantel_results <- data.frame(
  Variable1 = var1,
  Variable2 = var2,
  r_value = r_values,
  significance = p_values
)

# result
print(mantel_results)