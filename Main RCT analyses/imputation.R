# 加载必要的包
library(mice)
library(dplyr)
library(ggplot2)
library(cobalt)
library(here)

# 读取数据
raw_data <- readRDS(here::here("Main RCT analyses/raw_data.rds"))

# 选择变量
vars_of_interest <- c(
  "WINS.ID",
  "Treatment",
  "HEI2015_TOTAL_SCORE_bl",
  "HEI2015_TOTAL_SCORE_el",
  "HEI2015_TOTAL_SCORE_change",
  "Age_years",
  "Sex_bcf",
  "Race2_bcf",
  "Ethnicity_bcf",
  "Education_grouped"
)

# 选择分析数据集
analysis_data <- raw_data %>%
  select(all_of(vars_of_interest))

# 1. 检查缺失模式
# 查看缺失数据的总体情况
missing_summary <- analysis_data %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(pct_missing = (n_missing / nrow(analysis_data)) * 100) %>%
  arrange(desc(n_missing))

print("缺失数据汇总:")
print(missing_summary)

# 使用mice包查看缺失模式
md.pattern(analysis_data, rotate.names = TRUE)

# 2. 设置插补方法
# 初始化插补
init <- mice(analysis_data, maxit = 0)

# 查看预测矩阵和方法
predM <- init$predictorMatrix
meth <- init$method

# 根据变量类型调整插补方法
# WINS.ID不需要插补
meth["WINS.ID"] <- ""
predM[, "WINS.ID"] <- 0

# Treatment (有序因子) - 使用polr
if("Treatment" %in% names(meth)) {
  meth["Treatment"] <- "polr"
}

# 连续变量使用pmm (predictive mean matching)
continuous_vars <- c("HEI2015_TOTAL_SCORE_bl", "HEI2015_TOTAL_SCORE_el",
                     "HEI2015_TOTAL_SCORE_change", "Age_years")
for(var in continuous_vars) {
  if(var %in% names(meth)) {
    meth[var] <- "pmm"
  }
}

# 分类变量使用logreg (logistic regression)
categorical_vars <- c("Sex_bcf", "Race2_bcf", "Ethnicity_bcf", "Education_grouped")
for(var in categorical_vars) {
  if(var %in% names(meth)) {
    meth[var] <- "logreg"
  }
}

print("插补方法:")
print(meth)

# 3. 执行多重插补
# m = 插补数据集的数量
# maxit = 最大迭代次数
# seed = 随机种子，确保结果可重复
set.seed(123)

imputed_data <- mice(
  analysis_data,
  m = 5,              # 创建5个插补数据集
  method = meth,
  predictorMatrix = predM,
  maxit = 20,         # 迭代20次
  print = TRUE
)

# 4. 检查插补质量

# 4.1 收敛性诊断 - 绘制trace plots
png("imputation_trace_plots.png", width = 1200, height = 800)
plot(imputed_data)
dev.off()

# 4.2 检查插补值的分布
# 对于连续变量，比较原始数据和插补数据的密度分布
png("imputation_density_plots.png", width = 1200, height = 1000)
densityplot(imputed_data)
dev.off()

# 4.3 带状图 - 比较观测值和插补值
png("imputation_stripplots.png", width = 1200, height = 1000)
stripplot(imputed_data, pch = 20, cex = 1.2)
dev.off()

# 5. 提取完整数据集
# 提取第一个插补数据集
complete_data_1 <- complete(imputed_data, 1)

# 提取所有插补数据集（长格式）
complete_data_long <- complete(imputed_data, "long")

# 提取所有插补数据集（列表格式）
complete_data_list <- lapply(1:5, function(i) complete(imputed_data, i))

# 6. 保存结果
# 保存插补对象
saveRDS(imputed_data, here::here("Main RCT analyses/imputed_data_mice.rds"))

# 保存完整数据集
saveRDS(complete_data_1, here::here("Main RCT analyses/complete_data_imputed.rds"))
saveRDS(complete_data_list, here::here("Main RCT analyses/complete_data_list.rds"))

# 7. 创建插补诊断报告
sink(here::here("Main RCT analyses/imputation_report.txt"))
cat("=== 多重插补诊断报告 ===\n\n")
cat("样本量:", nrow(analysis_data), "\n")
cat("插补数据集数量:", imputed_data$m, "\n")
cat("迭代次数:", imputed_data$iteration, "\n\n")

cat("缺失数据模式:\n")
print(missing_summary)

cat("\n\n插补方法:\n")
print(data.frame(
  Variable = names(meth)[meth != ""],
  Method = meth[meth != ""]
))

cat("\n\n各插补数据集的描述统计:\n")
for(i in 1:imputed_data$m) {
  cat("\n--- 插补数据集", i, "---\n")
  complete_i <- complete(imputed_data, i)
  print(summary(complete_i))
}
sink()

cat("\n插补完成！\n")
cat("插补对象已保存到: Main RCT analyses/imputed_data_mice.rds\n")
cat("完整数据集已保存到: Main RCT analyses/complete_data_imputed.rds\n")
cat("诊断报告已保存到: Main RCT analyses/imputation_report.txt\n")

# 8. 后续分析示例：池化分析结果
# 如果你要进行回归分析，可以使用with()和pool()函数

# 示例：线性回归
fit <- with(imputed_data,
            lm(HEI2015_TOTAL_SCORE_change ~ Treatment + Age_years + Sex_bcf +
                 Race2_bcf + Ethnicity_bcf + Education_grouped + HEI2015_TOTAL_SCORE_bl))

# 池化结果
pooled_results <- pool(fit)
summary(pooled_results)

# 保存池化结果
pooled_summary <- summary(pooled_results)
saveRDS(pooled_summary, here::here("Main RCT analyses/pooled_regression_results.rds"))

print("池化回归结果:")
print(pooled_summary)

