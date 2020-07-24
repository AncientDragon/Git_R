library(openxlsx)
test_93157 = read.xlsx("GSE93157.xlsx")
test_93157_train_sub = sample(nrow(test_93157), 9 / 10 * nrow(test_93157))
test_93157_train_data = test_93157[test_93157_train_sub,]
test_93157_test_data = test_93157[-test_93157_train_sub,]

library(pROC)
library(e1071)
test_93157_train_data$group = as.factor(test_93157_train_data$group)
test_93157_test_data$group = as.factor(test_93157_test_data$group)
test_93157_svm =
  svm(
    formula = group ~ DEFB1 + C4BPA + IL2 + LAMP1 + CAMP + IL17B + FCGR3A +
      KIT + POU2F2 + IFNL2 + BLK,
    data = test_93157_train_data,
    type = 'C',
    kernel = 'radial'
  )
test_93157_pre_svm = predict(test_93157_svm, newdata = test_93157_test_data)
test_93157_obs_p_svm = data.frame(prob = test_93157_pre_svm, obs = test_93157_test_data$group)
test_93157_table = table(test_93157_test_data$group,
                         test_93157_pre_svm,
                         dnn = c("real", "pre"))
test_93157_svm_roc = roc(test_93157_test_data$group, as.numeric(test_93157_pre_svm))
#test_93157_pre_svm = predict(test_93157_svm, newdata = test_93157)
#test_93157_obs_p_svm = data.frame(prob = test_93157_pre_svm, obs = test_93157$group)
#test_93157_table = table(test_93157$group, test_93157_pre_svm, dnn = c("real", "pre"))
#test_93157_svm_roc = roc(test_93157$group, as.numeric(test_93157_pre_svm))
plot(
  test_93157_svm_roc,
  grid = c(0.05, 0.05),
  grid.col = c("black", "green"),
  print.auc = TRUE,
  auc.polygon = TRUE,
  max.auc.polygon = TRUE,
  auc.polygon.col = "yellow",
  print.thres = TRUE,
  main = 'GSE93157SVM模型ROC曲线 kernel = radial'
)
test_93157_pre_svm
test_93157_table
