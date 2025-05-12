df_grid<-readxl::read_excel( "D:/NYU/论文研究/核酸亭研究/熊月/MyProject10/shanghai_poi15_ExportFeature_TableToExcel_1.xlsx",sheet = "shanghai_poi15_ExportFeature")
# 确保数据没有缺失值
df_grid <- df_grid %>%
  filter(!is.na(flow_center), !is.na(LNdist_urbancenters), !is.na(NEAR_DIST_newsubcenters))


library(ggplot2)
library(dplyr)
library(vip)
library(randomForest)
library(caret)#用于数据划分

set.seed(123)#设置随机种子以保证可重复
#1.划分数据集(80% 训练集，20% 测试集)
train_index <-createDataPartition(df_grid$flow_center, p = 0.8, list = FALSE)
train_data <- df_grid[train_index,]
test_data <- df_grid[-train_index,]
#2.使用 tuneRF 寻找最优 mtry
best_mtry <- tuneRF(
  x= train_data[,c("NEAR_DIST_center","NEAR DIST metro","NEAR_DIST_newsubcenters")],
  y= train_data$flow_center,
  stepFactor=1.5,#每次增加 mtry 的步长
  improve = 0.01,#误差减少至少 1% 才继续调整
  ntreeTry = 500,#试验时的树的数量
  doBest = TRUE
)

rf_model<-randomForest(
flow_center ~ NEAR_DIST_center + NEAR_DIST_metro + NEAR_DIST_newsubcenters,
data = train_data,
importance = TRUE,# 计算变量重要性
ntree = 500,#树的数量
mtry=best_mtry$mtry #使用最优 mtry
)

# 输出模型摘要
print(rf_model)