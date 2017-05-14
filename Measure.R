## 判斷模型好壞的測量值:Performance measures
##
#1.混淆矩陣confusion matrix
#以預測值和真實值所組成的表格
#   P    N  (實際)
# p TP   FN   
# n FP   TN  F
# 偽陽性率(False Positive Rate; FPR):FP/(TN+FP)
# 特異度 (Specificity):TN/(FP+TN)
# 敏感度(Sensitivity):TP/(TP+FN)  
# 回現率(Recall):TP/(TP+FN) 
# 精確率(Precision):TP/(TP+FP)實體預測為正的準確度
#EX:以iris為例，以rpart方法使用
library(titanic)
str(titanic)
titanic<-titanic_train
tree<-rpart(Survived~Sex+Pclass+Age,data=titanic,method="class")
pred<-predict(tree,titanic,"class")
#confusion matrix
cof<-table(titanic$Survived,pred)
#2.ROC曲線
#提供分類效益評估
#ROC曲線在判別時，會以對角線為一個參考線，
#若是檢驗工具的ROC曲線剛好落在對角的參考線上，
#則表示檢驗工具對於此疾病的診斷沒有鑑別性。
#若是ROC曲線愈往左上方移動，表示檢驗工具對於疾病的敏感度愈高，
#且偽陽性率愈低，亦即此工具的鑑別力較佳
#1.圖形顯示:
#EX1:以下為package範例
library(ROCR)
data(ROCR.hiv)
attach(ROCR.hiv)
pred.svm <- prediction(hiv.svm$predictions, hiv.svm$labels)
perf.svm <- performance(pred.svm, "tpr", "fpr")
pred.nn <- prediction(hiv.nn$predictions, hiv.svm$labels)
perf.nn <- performance(pred.nn, "tpr", "fpr")
plot(perf.svm, lty=3, col="red", 
     main="HIV-1 coreceptor-SVM與NN ROC curve")
plot(perf.nn, lty=3, col="blue", add=TRUE)
plot(perf.svm, avg="vertical", lwd=3, col="red", 
     spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
plot(perf.nn, avg="vertical", lwd=3, col="blue", 
     spread.estimate="stderror",plotCI.lwd=2,add=TRUE)
legend(0.8,0.2, c("SVM","NN"),col=c("red","blue"),lwd=3)
#EX2:算出AU面積
perf.nn.au <- performance(pred.nn, "auc")
##AUC面積
#AUC=0.5 (no discrimination 無鑑別力)
#0.7≦AUC≦0.8 (acceptable discrimination 可接受的鑑別力)
#0.8≦AUC≦0.9 (excellent discrimination 優良的鑑別力)
#0.9≦AUC≦1.0 (outstanding discrimination 極佳的鑑別力)
perf.nn.au@y.values

##3.RMSE和R-squared
#迴歸模型衡量模型解釋能力
##R^2=1-ss_res/ss_tot
model<-lm(dist~speed,data=cars)
summary(model)
yhat<-predict(model)
res<-cars$dist-yhat
rmse<-sqrt(mean(res^2))

ss_res<-sum(res^2)
ss_tot<-sum((cars$dist-mean(cars$dist))^2)
r_sq<-1-ss_res/ss_tot
