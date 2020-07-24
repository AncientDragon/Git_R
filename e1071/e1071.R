#加载openxlsx 包，用来打开excel文件
library(openxlsx)
#读取excel文件到wine变量
wine = read.xlsx("winequality-red.xlsx")
#调用sample函数取样，nrow 查看excel文件行数【不包括首行标题】，在nrow(wine)个行中，随机抽取70%的行作为训练集,并将行号传值给train_sub
train_sub = sample(nrow(wine), 7 / 10 * nrow(wine))
#读取excle里面上面获取的随机行号的内容，传值给train_data[list类型数据]
train_data = wine[train_sub, ]
#读取excle里面排除上面获取的随机行号的内容，传值给test_data[list类型数据]
test_data = wine[-train_sub, ]

#加载pROCb包，用来绘制ROC曲线
library(pROC)
#加载e1071包，用来处理数据
library(e1071)
#数据预处理，将训练集的等级列传值到train_data$dj，as.factor 将数据类型转换为factor【因子类型】
train_data$dj = as.factor(train_data$dj)
#将测试集的等级列传值到test_data$dj
test_data$dj = as.factor(test_data$dj)
#根据各种自变量生成算法wine-svm,由于是分类问题，此处我们选择C-classification
wine_svm =
  svm(
    formula = dj ~ fhf + hf + nms + syt + lhw + yle + eyh + nd + pH + lsy + jj +
      zl,
    data = train_data,
    type = 'C',
    kernel = 'radial'
  )
#导入test_data到函数wine_svm预测结果[预测值]
pre_svm = predict(wine_svm, newdata = test_data)
#将预测结果和真实结果合并成数据框[类似excel，列标题分别设置为prob和obs]
obs_p_svm = data.frame(prob = pre_svm, obs = test_data$dj)
#输出混淆矩阵[便于查看数据，对本次作图无意义]
table_ = table(test_data$dj, pre_svm, dnn = c("real", "pre"))
#获取AUC值
svm_roc = roc(test_data$dj, as.numeric(pre_svm))
#绘制ROC曲线
plot(
  #加载svm_roc数据
  svm_roc,
  #设置背景网格竖线j间距，横线间距
  grid = c(0.05, 0.05),
  #设置背景网格竖线颜色，横线颜色
  grid.col = c("black", "green"),
  #是否输出AUC值
  print.auc = TRUE,
  #是否画AUC
  auc.polygon = TRUE,
  #是否显示完整图表[非AUC部分]
  max.auc.polygon = TRUE,
  #设置auc填充颜色
  auc.polygon.col = "yellow",
  #是否显示曲线上的阈值点
  print.thres = TRUE,
  #设置图片标题
  main = 'SVM模型ROC曲线 kernel = radial'
)
