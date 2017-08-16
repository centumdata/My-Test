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
#������ �浵�� plot���� ǥ��

hist(kchouse_raw$price, main = "Price histogram", xlab = "Price")
#price histogram : ���� ����� exponential�ϱ� ������ �׳� price�� ���� ���� price�� �ٸ��� �����ϴ� ���� ������

kchouse_raw$price_per_sqft <- kchouse_raw$price/kchouse_raw$sqft_living 
#�������� �м� ����� ���� price�� sqft_living�� ���� ������谡 ũ�� sqft_lot�� ������谡 �����Ƿ� price�� sqft_living���� ����� ���� �� �������� �����. ���� ���Ӻ����� price/sqft_living

hist(kchouse_raw$price_per_sqft, main = "Price per Square feet histogram", xlab = "Price per square feet") 
#������׷��� �׷����� �׳� price�� �� ������ ��Ʈ���� �� ������ �� ���Ժ����� ����� ����� ����

kcnumeric <- kchouse_raw[,c(3:16,20:22)]
#numeric value�� ���� ���鸸 ���� �������ֱ�

corrplot(cor(kcnumeric), method = "circle") 
#correlation plot�� ���� price�� bathroom, sqft_living, grade, sqft_above�� ���� ū ������踦 ���̸� price_per_sqft�� �ٸ� ������� ���ǹ��� ������踦 ������ ����. �� �츮�� ������� ���� ���� ���� ���� �ڿ��� ���� ������ ���Ӻ��� ���̿� ���ǹ��� ������谡 ���� ������ �Ǵ�

samplerows <- sample(1:nrow(kchouse_raw), 200, replace = FALSE)
#�������� 200�� ǥ�� ����

kcsample <- kcnumeric[samplerows,]
pairs(kcsample, gap = 0, cex.labels = 0.9)
#pairs�Լ� ���� ������ ���� ������� �Ѵ��� ���� ����. corrplot �ᵵ ������� ��.

kccluster8 <- kmeans(scale(kchouse_raw[,c(18,19,22)]),8,500)
#kmeans �Լ� ���� latitude, longitude, price_per_sqft �� ���� ������ 8���� cluster�� ����. ǥ���� 500�� ���

kchouse_raw$cluster<-factor(kccluster8$cluster)
#�������Ϳ� cluster ���� �߰�

ggplot(data= kchouse_raw, aes(x = long, y = lat)) + geom_point(aes(color=cluster))
#cluster �ݿ��ؼ� ggplot �׸���

dummies <- data.frame(matrix(nrow = nrow(kchouse_raw), ncol = 8))
#���̺��� 8�� �߰�

for(i in 1:8){
  dummies[ ,i] <- ifelse(kchouse_raw$cluster == i, 1, 0)
}

sum(dummies) == nrow(kchouse_raw)
#���̺����� �հ� row�� ���� ���� �� Ȯ��(ommitted variable ������ ���� ���ؼ�)

kc <- cbind(kchouse_raw,dummies)
#���̺��� �������Ϳ� column���� �߰�

linearmodel_nodummy <- lm(price_per_sqft ~ bedrooms + bathrooms + floors + waterfront + view + condition + grade, data = kc)
#�켱 ���̺��� ���� ���� ȸ�ͺм� �غ�

summary(linearmodel_nodummy)
#R^2 ���� 0.17�̶�� �ſ� ���� ����

linearmodel <- lm(price_per_sqft ~ bedrooms + bathrooms + floors + waterfront + view + condition + grade + X1 + X2 + X3 + X4 + X5 + X6 + X7, data = kc)
#���̺��� �߰��ؼ� ����ȸ�͹����� ����

summary(linearmodel)
#R^2 0.9017�� �ſ� ���� �� ����

car::vif(linearmodel)
#���߰����� �׽�Ʈ ��� ���������� ���� ������� ���� ����

lm.beta::lm.beta(linearmodel)
#�������� ������ Ȯ��

## ȸ�ͺм� �׽�Ʈ�غ���
prediction <- predict(linearmodel,
                      newdata = data.frame(kc[2,]),
                      interval = "predict")

PRICE = kc[2,6]*prediction
PRICE


###Ŭ�����͸� 50���� ������ ��� �ɱ�###
kccluster50 <- kmeans(scale(kchouse_raw[,c(18,19,22)]),50,500)
kchouse_raw$cluster50 <-factor(kccluster50$cluster)
dummies50 <- data.frame(matrix(nrow = nrow(kchouse_raw),ncol = 50))
for(i in 1:50){
  dummies[,i] <- ifelse(kchouse_raw$cluster50 == i, 1,0)
}
kc50 <- cbind(kchouse_raw,dummies)

linearmodel50 <- lm(price_per_sqft ~ bedrooms + bathrooms + floors + waterfront + view + condition + grade +X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18+X19+X20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35+V36+V37+V38+V39+V40+V41+V42+V43+V44+V45+V46+V47+V48+V49, data = kc50)
summary(linearmodel50)
#50Ŭ�����ͷ� ������ R^2�� ��������� ��. ��� ���� �ǹ̰� �ִ�����...