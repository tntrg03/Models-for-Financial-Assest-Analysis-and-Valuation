# 1. Tính chuỗi lợi suất
for(j in 2:ncol(X50_CP_HNX)) {
  for(i in 2:nrow(X50_CP_HNX)) {
    a=X50_CP_HNX[[i,j]]
    b=X50_CP_HNX[[i-1,j]]
    Empty[[i,j]]=log(a/b)
  }
}
View(Empty)

stock=Empty[-1,]

library(zoo)
library(lmtest)
library(xts)
library(stargazer)
#datanew<-data.frame(xts(X50_CP_HNX_ff[,-1],order.by = as.Date(X50_CP_HNX_ff$Date)))
datanew<-data.frame(X50_CP_HNX_ff,Stock[,-1])
View(datanew)

coef_table=data.frame() # tạo 1 bảng dữ liệu để lưu trữ các hệ số ước lượng
dependent=c(datanew[,7:56])
for (rate in dependent){
  model <- lm(rate~Market_Factor+SMB+HML,data = datanew)
  #print(summary(model))
  #stargazer(model,type = "text")
  coef_values<-coef(model)
  coef_table<-rbind(coef_table,coef_values)
  
}
colnames(coef_table)=c("alpha_i","beta_MarketFactor","beta_SMB","beta_HML")
# Sử dụng tập dữ liệu "coef_table" để ước lượng phần bù rủi ro nhân tố
View(coef_table)