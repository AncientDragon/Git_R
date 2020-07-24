library(openxlsx)
test_GSE136961 = read.xlsx("GSE136961.xlsx")
test_GSE136961_train_sub = sample(nrow(test_GSE136961), 8 / 10 * nrow(test_GSE136961))
test_GSE136961_train_data = test_GSE136961[test_GSE136961_train_sub, ]
test_GSE136961_test_data = test_GSE136961[-test_GSE136961_train_sub, ]

library(pROC)
library(e1071)
test_GSE136961_train_data$group = as.factor(test_GSE136961_train_data$group)
test_GSE136961_test_data$group = as.factor(test_GSE136961_test_data$group)
test_GSE136961_svm =
  svm(
    formula = group ~ TNFRSF9 + CD83 + GPR18 + MRC1 + HLA_B + IKZF3 + CD3G +
      PTPN6 + HLA_DOA + CD80 + CYBB + HLA_E + FAS + CD37 + GBP1 + PSMB9 + TARP +
      CD3D + KLRK1 + KRT5 + GRAP2 + IL10 + HLA_DRA + TGFB1 + TIGIT + FASLG + CD52 +
      IFIT1 + CD163 + IFIT2 + AIF1 + STAT1 + CD6 + HAVCR2 + CD28 + CD48 + SLAMF8 +
      CD2 + DDX58 + IFIT3 + ITK + IFIH1 + CD226 + HLA_DPB1 + LEXM + CD53 + TNFRSF14 +
      CXCL10 + HLA_DMA + IKZF1 + TRIM29 + CD33 + IL7 + CD74 + CD4 + GZMA + CD3E +
      CD38 + TNFSF13B + C1QA + CCR7 + NRP1 + CD8B + CD1D + NCF1 + CTSS + CIITA +
      ITGAL + HERC6 + HLA_C + IL2RB + IL2RG + FYB + CD63 + BST2 + GNLY + KLRG1 +
      CCR2 + ICAM1 + TYROBP + KLRB1 + SAMHD1 + EOMES + CD79B + ITGB7 + SH2D1A +
      CSF2RB + ICOS + KLRD1 + FCGR1A + IGSF6 + CD244 + IL10RA + LST1 + PTPRC +
      ZAP70 + PDCD1LG2,
    data = test_GSE136961_train_data,
    type = 'C',
    kernel = 'radial'
  )
test_GSE136961_pre_svm = predict(test_GSE136961_svm, newdata = test_GSE136961_test_data)
test_GSE136961_obs_p_svm = data.frame(prob = test_GSE136961_pre_svm, obs = test_GSE136961_test_data$group)
test_GSE136961_table = table(test_GSE136961_test_data$group,
                             test_GSE136961_pre_svm,
                             dnn = c("real", "pre"))
test_GSE136961_svm_roc = roc(test_GSE136961_test_data$group,
                             as.numeric(test_GSE136961_pre_svm))
#test_GSE136961_pre_svm = predict(test_GSE136961_svm, newdata = test_GSE136961)
#test_GSE136961_obs_p_svm = data.frame(prob = test_GSE136961_pre_svm, obs = test_GSE136961$group)
#test_GSE136961_table = table(test_GSE136961$group, test_GSE136961_pre_svm, dnn = c("real", "pre"))
#test_GSE136961_svm_roc = roc(test_GSE136961$group, as.numeric(test_GSE136961_pre_svm))
plot(
  test_GSE136961_svm_roc,
  grid = c(0.05, 0.05),
  grid.col = c("black", "green"),
  print.auc = TRUE,
  auc.polygon = TRUE,
  max.auc.polygon = TRUE,
  auc.polygon.col = "yellow",
  print.thres = TRUE,
  main = 'SVM模型ROC曲线 kernel = radial'
)
test_GSE136961_obs_p_svm
test_GSE136961_table
