#데이터 처리
korean=read.csv("korean.csv",header = T)
head(korean)
korean=korean[,2:14]
attach(korean);head(korean)
y.tmp = rep(1,nrow(korean))
y.tmp[which((korean[,3]>=19)&(korean[,3]<=29))] = 1
y.tmp[which((korean[,3]>=30)&(korean[,3]<=39))] = 2
y.tmp[which((korean[,3]>=40)&(korean[,3]<=49))] = 3
y.tmp[which((korean[,3]>=50)&(korean[,3]<=59))] = 4
y.tmp[which((korean[,3]>=60)&(korean[,3]<=69))] = 5
y.tmp[which((korean[,3]>=70)&(korean[,3]<=79))] = 6
korean[,3] = y.tmp
korean[korean==99]<-NA
korean <- na.omit(korean)
#범주형으로 예상되는 범주가 integer로 지정되어 있을 때 범주형으로 바꿈 
korean$SQ1 = factor(korean$SQ1)
korean$SQ2 = factor(korean$SQ2)
korean$SQ3 = factor(korean$SQ3)
korean$SQ4 = factor(korean$SQ4)
korean$SQ5 = factor(korean$SQ5)
attach(korean);head(korean)
x=korean[,6:13]; attach(x)

#기초통계량
#범주형 자료
#지역별
table(korean$SQ1)
table(korean$SQ1)/sum(table(korean$SQ1))
#성별
table(korean$SQ2)
table(korean$SQ2)/sum(table(korean$SQ2))
#연령
table(korean$SQ3)
table(korean$SQ3)/sum(table(korean$SQ3))
#학력
table(korean$SQ4)
table(korean$SQ4)/sum(table(korean$SQ4))
#혼인상태
table(korean$SQ5)
table(korean$SQ5)/sum(table(korean$SQ5))

bar1=barplot(table(korean$SQ1), main="거주지역", ylab="명수",names.arg=c("서울","부산","대구","인천",
                                                                   "광주","대전","울산","세종","경기",
                                                                   "강원","충북","충남","전북","전남",
                                                                   "경북","경남","제주"),col=rainbow(17),ylim=c(0,1200))
text(bar1, y=table(korean$SQ1), labels = table(korean$SQ1),pos=3)

bar2=barplot(table(korean$SQ2), main="성별", ylab="명수", names.arg=c("남","여"),col=rainbow(2),ylim=c(0,3000))
text(bar2, y=table(korean$SQ2), labels = table(korean$SQ2),pos=3)

bar3=barplot(table(korean$SQ3), main="연령", ylab="명수", names.arg=c("19~29세","30~39세","40~49세",
                                                                  "50~59세","60~69세","70~79세"),col=rainbow(6),ylim=c(0,1400))
text(bar3, y=table(korean$SQ3), labels = table(korean$SQ3),pos=3)

bar4=barplot(table(korean$SQ4), main="학력", ylab="명수", names.arg=c("중학교 졸업 이하","고등학교 졸업","대학교 재학 및 졸업",
                                                                  "대학원 졸업"),col=rainbow(4),ylim=c(0,2500))
text(bar4, y=table(korean$SQ4), labels = table(korean$SQ4),pos=3)

bar5=barplot(table(korean$SQ5), main="혼인상태", ylab="명수", names.arg=c("미혼","기혼-결혼 생활 중","기혼-사별,이혼,별거"),col=rainbow(3),ylim=c(0,3400))
text(bar5, y=table(korean$SQ5), labels = table(korean$SQ5),pos=3)

library(plotrix)
test1 <-table(korean$SQ1)
area1 <-c("서울","부산","대구","인천",
          "광주","대전","울산","세종","경기",
          "강원","충북","충남","전북","전남",
          "경북","경남","제주")
area1 <- paste(area1,test1)
area1 <- paste(area1, "명", sep="")
pie3D(table(korean$SQ1), labels=area1, main= "거주지역")


test2 <- table(korean$SQ2)
area2 <- c("남","여")
area2 <- paste(area2,test2)
area2 <- paste(area2, "명", sep="")
pie3D(table(korean$SQ2), main="성별",labels =area2)

test3 <- table(korean$SQ3)
area3 <- c("19~29세","30~39세","40~49세",
           "50~59세","60~69세","70~79세")
area3 <- paste(area3,test3)
area3 <- paste(area3, "명", sep="")
pie3D(table(korean$SQ3), main="연령",labels =area3)

test4 <- table(korean$SQ4)
area4 <- c("중학교 졸업 이하","고등학교 졸업","대학교 재학 및 졸업","대학원 졸업")
area4 <- paste(area4,test4)
area4 <- paste(area4, "명", sep="")
pie3D(table(korean$SQ4), main="학력",labels =area4)

test5 <- table(korean$SQ5)
area5 <- c("미혼","기혼-결혼 생활 중","기혼-사별,이혼,별거")
area5 <- paste(area5,test5)
area5 <- paste(area5, "명", sep="")
pie3D(table(korean$SQ5), main="혼인상태",labels =area5)

#척도형 자료
summary(x)
sqrt(cov(x))
boxplot(x,xlab="항목",ylab="척도",main="항목별 다중 상자그림",col=rainbow(8))
plot(x,main="산점도")

#다변량 통계분석
#상관분석
cor(x)
cor.test(Q1,Q2)
cor.test(Q1,Q3)
cor.test(Q1,Q4)
#H0:상관관계가 없다 vs h1:상관관계가 있다.
#모든 변수가 상관관계가 있음

library(psych)
corPlot(x)
library(corrplot)
corrplot(cor(x))
corrplot(cor(x),method = "number")
corrplot.mixed(cor(x), upper="circle")

#주성분분석 (표본공분산행렬 이용)
#고유값
s=cov(x);eigen(s)
p_cov=princomp(x, cor = F)
summary(p_cov)     

p_cov$loadings   #표본 주성분 계수 

biplot(p_cov)   #상관행렬을 이용한 주성분 그래프

#스크리 그래프 적절한 주성분 개수
screeplot(p_cov,npcs = 8, type = "lines",main="Scree Plot")

#loading plot
plot(p_cov$loadings[,1],p_cov$loadings[,2],xlab = "factor1", ylab="factor2", main="Principal Component Loadings",type="n",xlim=c(-0.2,0.5),ylim=c(-1,0.4))
text(y=p_cov$loadings[,2],x=p_cov$loadings[,1], label=c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8"), cex=0.8)
for(i in 1:8){arrows(0,0,0.8*p_cov$loadings[i,1],0.8*p_cov$loadings[i,2],length=0.1)}
abline(0,0,col="red")
abline(0,10^7,col="red")

#loading plot
plot(p_cov$loadings[,2],p_cov$loadings[,3],xlab = "factor2", ylab="factor3", main="Principal Component Loadings",type="n",xlim=c(-0.8,0.6),ylim=c(-1,0.7))
text(y=p_cov$loadings[,3],x=p_cov$loadings[,2], label=c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8"), cex=0.8)
for(i in 1:8){arrows(0,0,0.8*p_cov$loadings[i,2],0.8*p_cov$loadings[i,3],length=0.1)}
abline(0,0,col="red")
abline(0,10^7,col="red")

library(psych)
library(GPArotation)
x_factor=principal(x, rotate = "none", nfactors=4 ,cor = "cov")
x_factor$values
x_factor

#인자분석
#정규성 확인=카이제곱그림
chi2.plot<-function(x){
  n=dim(x)[[1]]      #관측값
  vp=dim(x)[[2]]     #변수 개수
  xbar=colMeans(x)
  S=cov(x) 
  d=mahalanobis(x,colMeans(x),S)
  d2=sort(d)
  tt=seq(1,n)
  t=(tt-0.5)/n
  q=qchisq(t,vp)
  plot(q,d2,pch="*",main="Chisquare plot for multivariate normality",
       xlab="chi2 quantile", ylab="ordered Mahalanobis d^2")
  abline(0,1)
  return(list(xbar,S))}
chi2.plot(x)
#마할라노비스 거리의 순서통계량과 자유도가 2인 카이제곱 분포로부터 사분위수간에 선형성을 크게 벗어남음을 볼 수 있다. 
#그림을 보면 데이터가 정규성을 따른다고 단언하기에는 무리가 있어 보인다. 
ks.test(x,"pnorm",mean=colMeans(x),sd=cov(x))


#인자분석 
library(psych)
#주축인자법 이용
x_factor=fa(x, nfactors=4 ,rotate = "none" ,covar = T,fm="pa") ;x_factor
#가중최소제곱법이용 fm="wls" 최대우도 fm="ml" 
#최대우도법 이용
#x_factor=fa(x, nfactors=4 ,rotate = "none" ,covar = T,fm="ml") ;x_factor
#인자개수 검정 카이제곱통계량
x_factor$STATISTIC
#인자계수
x_factor$loadings

#plot of factor pattern
namevar=names(x_factor$loadings)=c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8")
plot(x_factor$loadings[,1],x_factor$loadings[,2],pch=16, xlab="factor1", ylab = "factor2",
     main="factor pattern_회전 전",xlim=c(-0.3,1.6),ylim=c(-1.,1.2))
text(x=x_factor$loadings[,1], y=x_factor$loadings[,2], labels=namevar, adj=0)
abline(v=0,h=0,col="red")

#주축인자법 promax 회전 
x_promax =fa(x, nfactors=4 ,rotate = "promax" ,covar = T,fm="pa") ;x_promax
#인자개수 검정 카이제곱통계량
x_promax$STATISTIC
#인자계수
x_promax$loadings
#plot of factor pattern
namevar=names(x_promax$loadings)=c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8")
plot(x_promax$loadings[,1],x_promax$loadings[,2],pch=16, xlab="factor1", ylab = "factor2",
     main="factor pattern_promax회전 후",xlim=c(-0.3,1.6),ylim=c(-1.,1.5))
text(x=x_promax$loadings[,1], y=x_promax$loadings[,2], labels=namevar, adj=0)
abline(v=0,h=0,col="red")

#군집분석

#향목간의 군집분석
tx=t(x)
#계층적 군집분석- Ward 방법
hc1= hclust(dist(tx), method = "ward.D")
#나무 구조 그림
plot(hc1,main="Ward method",hang=-1)
rect.hclust(hc1,k=4)
#계층적 군집분석- 최장연결법
hc2= hclust(dist(tx), method =  "complete" )
#나무 구조 그림
plot(hc2,hang=-1,main="complete linkage")
rect.hclust(hc2,k=4)


#표본간의 군집분석
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(cluster)    # for agnes function
#계층적 군집분석
#군집의 개수 정하기
# Elbow methods
fviz_nbclust(x, FUN = hcut, method = "wss")
#엘보우 방법으로는 군집을 2개 또는 3개로 나누어짐
dist <- dist(x, method = "euclidean")
hc4 <- agnes(dist, method = "ward" ) #Ward's minimum variance method
#시각화1
pltree(hc4, cex = 0.6, main="Clustering with Ward's linkage", hang = -1)
rect.hclust(hc4, k = 2, border = 2:5)
sub_grp4 <- cutree(hc4, k = 2)  #ward's
table(sub_grp4) # ward's
#시각화2
fviz_cluster(list(data = x, cluster = sub_grp4))
#Ward 방법을 이용한 군집 결과의 평균값
x1=x[(sub_grp4==1),]; colMeans(x1)
x2=x[(sub_grp4==2),]; colMeans(x2)


#비계층적 군집분석
#군집의 개수 정하기
# Elbow methods
set.seed(123)
fviz_nbclust(x, FUN = kmeans, method = "wss")
#엘보우 방법으로는 군집을 2개 또는 3개로 나누어짐 

set.seed(12367)
final <- kmeans(x, centers=2, nstart = 25)
table(final$cluster)
final$centers
#군집별 평균 값을 확인해보면 군집1과 2모두 Q4,Q5가 다른 만족도에 비해 떨어지는 것을 확인할 수 있다. 
colMeans(x)

#시각화1
fviz_cluster(final, data = x)
#시각화2 
plot(x,pch=final$cluster,col=final$cluster,main="K-means clustering")
text(x,adj=0,cex=0.5)


y1=korean[(final$cluster==1),] #높은 만족도의 군집
y2=korean[(final$cluster==2),] #낮은 만족도의 군집

#지역별 군집 비율
round(table(y1$SQ1)/table(SQ1),digits=3)
round(table(y2$SQ1)/table(SQ1),digits=3)
#울산의 삶의 만족도
y_W=y1[(y1$SQ1==7),] 
table(y_W$SQ1)
colMeans(y_W[,6:13])
#인천의 삶의 만족도 
y_I=y1[(y1$SQ1==4),] 
table(y_I$SQ1)
colMeans(y_I[,6:13])
#서울의 삶의 만족도 
y_S=y1[(y1$SQ1==1),] 
table(y_S$SQ1)
colMeans(y_S[,6:13])
#제주의 삶의 만족도 
y_J=y1[(y1$SQ1==17),] 
table(y_J$SQ1)
colMeans(y_J[,6:13])


#성별 군집 비율
round(table(y1$SQ2)/table(SQ2),digits=3)
round(table(y2$SQ2)/table(SQ2),digits=3)
#남성의 삶의 만족도
y_m=y1[(y1$SQ2==1),] 
table(y_m$SQ2)
colMeans(y_m[,6:13])
#여성의 삶의 만족도 
y_f=y1[(y1$SQ2==2),] 
table(y_f$SQ2)
colMeans(y_f[,6:13])


#연령별 군집
round(table(y1$SQ3)/table(SQ3),digits=3)
round(table(y2$SQ3)/table(SQ3),digits=3)
y20=y1[(y1$SQ3==1),] 
colMeans(y20[,6:13])
y30=y1[(y1$SQ3==2),] ; table(y30$SQ3)
colMeans(y30[,6:13])
y40=y1[(y1$SQ3==3),] ; table(y40$SQ3)
colMeans(y40[,6:13])
y50=y1[(y1$SQ3==4),] ; table(y50$SQ3)
colMeans(y50[,6:13])
y60=y1[(y1$SQ3==5),] ; table(y60$SQ3)
colMeans(y60[,6:13])
y70=y1[(y1$SQ3==6),] ; table(y70$SQ3)
colMeans(y70[,6:13])

#학력별 군집 
round(table(y1$SQ4)/table(SQ4),digits=3)
round(table(y2$SQ4)/table(SQ4),digits=3)
#중졸 삶의 만족도
y_mid=y1[(y1$SQ4==1),] 
table(y_mid$SQ4)
colMeans(y_mid[,6:13])
#고졸의 삶의 만족도 
y_hi=y1[(y1$SQ4==2),] 
table(y_hi$SQ4)
colMeans(y_hi[,6:13])
#대졸의 삶의 만족도 
y_uni=y1[(y1$SQ4==3),] 
table(y_uni$SQ4)
colMeans(y_uni[,6:13])
#대학원 졸업의 삶의 만족도 
y_ma=y1[(y1$SQ4==4),] 
table(y_ma$SQ4)
colMeans(y_ma[,6:13])

#혼인상태별 군집 
round(table(y1$SQ5)/table(SQ5),digits=3)
round(table(y2$SQ5)/table(SQ5),digits=3)
#미혼 삶의 만족도
y_single=y1[(y1$SQ5==1),] 
table(y_single$SQ5)
colMeans(y_single[,6:13])
#기혼-결혼생활 중의 삶의 만족도 
y_married=y1[(y1$SQ5==2),] 
table(y_married$SQ5)
colMeans(y_married[,6:13])
#기혼-사별,이혼,별거의 삶의 만족도 
y_seperate=y1[(y1$SQ5==3),] 
table(y_seperate$SQ5)
colMeans(y_seperate[,6:13])

