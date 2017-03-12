library(SoDA)
library(data.table)
load('/Users/lxy/Documents/R/ShenyangAirport/runway24.Rdata')


#高度范围(feet)
minHeight<-200
maxHeight<-1000

#跑道经纬度
w1<-41+37/60+49.61/60/60
j1<-123+28/60+8.58/60/60
w2<-41+38/60+58.22/60/60
j2<-123+29/60+52.08/60/60
#放入LIST
w<-c(w1,w2)
j<-c(j1,j2)    
wj<-rbind(w,j)
#建立方程并求参数
lm<-matrix(c(w1,1,w2,1),nrow=2,byrow=TRUE)# 方程左矩阵
rm<-matrix(c(j1,j2),nrow=2)# 方程右矩阵
result<-solve(lm,rm)#求方程的两个参数
f<-function(x){result[1,1]*x+result[2,1]}#跑道的直线方程
plot(f,40,50,type="l")
#方程参数
A<-result[1,1]
B<--1
C<-result[2,1]
# 点到直线距离,没用
# point2lineDis<-function(x,y){        
#   abs(A*x+B*y+C)/sqrt(A^2+B^2)
# }
#求垂足FUNCTION
footpoint<- function(y,x){
  x1 <- ( B^2*x-A*B*y-A*C)/(A^2+B^2);
  y1 <- (-A*B*x+A^2*y-B*C)/(A^2+B^2);
  ft<-data.table(y1,x1)
  return(ft)
}
#提取处理后数据中经纬度
LDpoints<-processedData24[,.(LONPC,LATPC)] 
#将处理过的经纬度通过向量方式传入计算垂足FUNCTION中
positionOnLine<-mapply(footpoint,LDpoints[,1],LDpoints[,2],SIMPLIFY = FALSE)
#将计算出的垂足坐标与真实数据放入一个结果LIST中
positionList<-cbind(LDpoints,positionOnLine$LONPC)
#更改列名字符串
cnames=c("LONPC","LATPC","LONR","LATR")
#RENAME COLUMN
colnames(positionList)=cnames
#转换成MATRIX方便取值
positionMatrix<-as.matrix(positionList)
distances<-data.table()                         
# 循环计算点之间的地理距离,使用geosphere中的distm function

distances<-mapply(geoDist,positionMatrix[,1],positionMatrix[,2],positionMatrix[,3],positionMatrix[,4])
distances<-as.data.table(distances)
colnames(distances)='dis'

#距离单位为米meter
maxlength<-4500
#频率分布的间隔
histBreak=seq(0,maxlength,1) 
distancesfiltered<-distances[dis<=maxlength]

#画分布图
data=as.numeric(distancesfiltered$dis)

histPlot<-hist(data,breaks=histBreak,xlab='Distance to Runway(m)',ylab='Sample Size'
               ,main='',col="blue",cex.axis=1.3,cex.lab=1.4,ylim=c(0,160000))
histx<-hist(data,seq(0,maxlength,1))$mids
histy<-hist(data,seq(0,maxlength,1))$counts




#拟合曲线方程
plot(histx, histy,pch=16,col='blue',ylab='Sample Size'
     ,main='',cex.axis=1.3,cex.lab=1.4,xlim=c(0,150),ylim=c(0,40000))
plot(density(data))
dataMean<-weighted.mean(histx,histy/length(data))
mean(data)
dataStd<-sd(data)
curve(dnorm(x, dataMean, dataStd), from = 0, to = 6000,ylab='Probability',xlab='Distance',main='Normal Distribution',
      col='blue',cex.axis=1.3,cex.lab=1.3,cex.main=1.7,xaxs="i",yaxs="i",ylim=c(0,0.020))



dat = data.table(x = rnorm(data, dataMean, dataStd))
dat = dat[,list(Freq = .N), by = x]
dat = dat[,Total:= Freq/sum(Freq)][order(x,decreasing=F),]

sum(dat[abs(x)>=3500 & abs(x)<=4500,Total])

