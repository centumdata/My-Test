## group project_KC House Sales

options(repos="https://cran.rstudio.com")
install.packages("readxl")
install.packages(("ggmap"))
install.packages(("ggplot2"))
install.packages(("nortest"))
install.packages("car")
install.packages("lm.beta")
library(car)
library(lm.beta)
library(readxl)
library(ggplot2)
library(corrplot)
library(ggmap)
library(nortest)
set.seed(1234)

kchouse_raw = readxl::read_excel(path="D:/R/project/kc_house_data.xlsx",
                                 sheet=1,
                                 col_names = TRUE)

plot(kchouse_raw$long,kchouse_raw$lat, main = "Latitude and longitude plot",xlab = "Longitude",ylab = "Latitude")
#위도와 경도를 plot으로 표현

hist(kchouse_raw$price, main = "Price histogram", xlab = "Price")
#price histogram : 분포 모양이 exponential하기 때문에 그냥 price로 쓰기 보단 price를 다르게 가공하는 것이 좋을듯

kchouse_raw$price_per_sqft <- kchouse_raw$price/kchouse_raw$sqft_living 
#태형님의 분석 결과에 따라서 price는 sqft_living과 가장 상관관계가 크고 sqft_lot은 상관관계가 적으므로 price를 sqft_living으로 나누어서 면적 당 가격으로 계산함. 기준 종속변수는 price/sqft_living

hist(kchouse_raw$price_per_sqft, main = "Price per Square feet histogram", xlab = "Price per square feet") 
#히스토그램을 그려보면 그냥 price로 할 때보다 피트면적 당 가격이 더 정규분포에 가까운 모양을 보임

kcnumeric <- kchouse_raw[,c(3:16,20:22)]
#numeric value를 갖는 값들만 따로 지정해주기

corrplot(cor(kcnumeric), method = "circle") 
#correlation plot을 찍어보니 price는 bathroom, sqft_living, grade, sqft_above와 가장 큰 상관관계를 보이며 price_per_sqft는 다른 변수들과 유의미한 상관관계를 보이지 않음. 즉 우리의 가정대로 지역 별로 집을 나눈 뒤에야 독립 변수와 종속변수 사이에 유의미한 상관관계가 있을 것으로 판단

samplerows <- sample(1:nrow(kchouse_raw), 200, replace = FALSE)
#무작위로 200개 표본 추출

kcsample <- kcnumeric[samplerows,]
pairs(kcsample, gap = 0, cex.labels = 0.9)
#pairs함수 통해 변수들 간의 상관관계 한눈에 보기 좋음. corrplot 써도 상관없을 듯.

kccluster8 <- kmeans(scale(kchouse_raw[,c(18,19,22)]),8,500)
#kmeans 함수 통해 latitude, longitude, price_per_sqft 세 가지 변수를 8가지 cluster로 나눔. 표본은 500개 사용

kchouse_raw$cluster<-factor(kccluster8$cluster)
#원데이터에 cluster 변수 추가

ggplot(data= kchouse_raw, aes(x = long, y = lat)) + geom_point(aes(color=cluster))
#cluster 반영해서 ggplot 그리기

dummies <- data.frame(matrix(nrow = nrow(kchouse_raw), ncol = 8))
#더미변수 8개 추가

for(i in 1:8){
  dummies[ ,i] <- ifelse(kchouse_raw$cluster == i, 1, 0)
}

sum(dummies) == nrow(kchouse_raw)
#더미변수의 합과 row의 합이 같은 지 확인(ommitted variable 없는지 보기 위해서)

kc <- cbind(kchouse_raw,dummies)
#더미변수 원데이터에 column으로 추가

linearmodel_nodummy <- lm(price_per_sqft ~ bedrooms + bathrooms + floors + waterfront + view + condition + grade, data = kc)
#우선 더미변수 없이 선형 회귀분석 해봄

summary(linearmodel_nodummy)
#R^2 값도 0.17이라는 매우 낮은 숫자

linearmodel <- lm(price_per_sqft ~ bedrooms + bathrooms + floors + waterfront + view + condition + grade + X1 + X2 + X3 + X4 + X5 + X6 + X7, data = kc)
#더미변수 추가해서 선형회귀방정식 도출

summary(linearmodel)
#R^2 0.9017로 매우 높은 값 나옴

car::vif(linearmodel)
#다중공선성 테스트 결과 독립변수들 간에 상관관계 존재 없음

lm.beta::lm.beta(linearmodel)
#변수들의 설명력 확인

## 회귀분석 테스트해보기
prediction <- predict(linearmodel,
                      newdata = data.frame(kc[2,]),
                      interval = "predict")

PRICE = kc[2,6]*prediction
PRICE


###클러스터를 50개로 나누면 어떻게 될까###
kccluster50 <- kmeans(scale(kchouse_raw[,c(18,19,22)]),50,500)
kchouse_raw$cluster50 <-factor(kccluster50$cluster)
dummies50 <- data.frame(matrix(nrow = nrow(kchouse_raw),ncol = 50))
for(i in 1:50){
  dummies[,i] <- ifelse(kchouse_raw$cluster50 == i, 1,0)
}
kc50 <- cbind(kchouse_raw,dummies)

linearmodel50 <- lm(price_per_sqft ~ bedrooms + bathrooms + floors + waterfront + view + condition + grade +X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36+V37+V38+V39+V40+V41+V42+V43+V44+V45+V46+V47+V48+V49, data = kc50)
summary(linearmodel50)
#50클러스터로 나누면 R^2가 높아지기는 함. 어느 정도 의미가 있는지는...