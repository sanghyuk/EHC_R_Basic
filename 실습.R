library(mgcv)

shock<-read.csv(file="https://cutt.ly/shock")
shock<-read.csv(file="D:/Dropbox/환경보건센터/2021년/환경보건 통계 워크숍/R 기본 강의/shock_excercise_202009.csv")

head(shock)
tail(shock)

#변수 척도 확인
summary(shock$AGE)
summary(shock$SEX)

#SEX는 명목척도이나 연속형으로 입력된 상태. 명목척도로 변환이 필요.
shock$SEX1<-as.factor(shock$SEX)
summary(shock$SEX1)

#연속변수를 범주를 나누어 명목척도로 바꿀 수도 있음.
#나이 70세를 기준으로 두 그룹으로 나뉜 명목 척도 만들기
shock$AGEX<-ifelse(shock$AGE<70,1,NA)
shock$AGEX<-ifelse(shock$AGE>=70,2,shock$AGEX)
summary(shock$AGEX)
shock$AGEX<-as.factor(shock$AGEX)
summary(shock$AGEX)

#몽땅 한 줄의 명령으로도 가능 (참고하세요)
shock$AGEX1<-as.factor(ifelse(shock$AGE<70,1,2))
summary(shock$AGEX1)

#세 그룹으로 나누는 경우에는 위의 방법은 안됩니다.
#정상 hemoglobin 범위를 10-12라고 하고 정상 이하, 정상, 정상 이상 세 그룹으로 나누기
shock$HBX<-ifelse(shock$HB<10.0,1,NA)
shock$HBX<-ifelse(shock$HB>=10 & shock$HB<12.0,2,shock$HBX)
shock$HBX<-ifelse(shock$HB>=12,3,shock$HBX)
shock$HBX<-as.factor(shock$HBX)
summary(shock$HBX)

#데이터셋을 조건에 따라 나누기 (row를 나눔)
#나이에 따라 2그룹
shock_u70<-subset(shock,AGEX==1)
shock_o70<-subset(shock,AGEX==2)

#나눈 데이터를 다시 합하기 (row를 합함)
shock_all<-rbind(shock_u70,shock_o70)

#데이터셋의 일부 변수만 추출하기 (column을 나눔)
#NO, AGE, SEX만 골라 따로 데이터셋 만들기
library(dplyr)
shock_basic<-select(shock,c(NO, AGE,SEX))

#NO, HEIGHT 골라서 따로 만들기
shock_height<-select(shock, c(NO, HEIGHT))

#나눈 데이터 다시 합하기 (column 합하기)
library(plyr)
shock_combine<-join(shock_basic, shock_height, by='NO')


#########################################################


#변수의 분포 확인
summary(shock$AGE)
shapiro.test(shock$AGE)
hist(shock$AGE)
qqnorm(shock$AGE)
qqline(shock$AGE,col="red")
boxplot(shock$AGE)

#평균과 표준편차 구하기
mean(shock$AGE,na.rm=T)
sd(shock$AGE,na.rm=T)
#TYPE에 따라 통계치 산출하기
ddply(shock,~TYPE,summarise,평균=mean(AGE),표준편차=sd(AGE),최소값=min(AGE),중간값=median(AGE),최대값=max(AGE))

#빈도 산출
#성별의 분포 
library(gmodels)
CrossTable(shock$SEX)

#빈도 비교
CrossTable(shock$SEX,shock$TYPE,expected=T)
CrossTable(shock$AGEX,shock$SEX,expected=T,fisher=T)

#두 군의 평균 비교
var.test(shock$SP1~shock$AGEX)
t.test(shock$SP1~shock$AGEX,var.equal=T)
t.test(shock$SP1~shock$AGEX,var.equal=F)

wilcox.test(shock$SP1~shock$AGEX)

#짝지은 두 군의 평균 비교
#HR1과 HR2은 같은 사람에게서 다른 시기에 측정한 값이므로 짝지은 연속형 변수
#2가지 방법이 있습니다. 
#1. 2 변수의 차이를 계산한 새로운 변수를 만들고 이 새로운 변수의 평균이 0인지 검정
shock$HR.d<-shock$HR2-shock$HR1
summary(shock$HR.d)
t.test(shock$HR.d)

#2. t.test 함수에 paired=T옵션을 줘서 검정
t.test(shock$HR1, shock$HR2, paired=T)

#짝지은 두 그룹의 비모수적 비교 (Wicoxon signed rank test)
shapiro.test(shock$HR1)
shapiro.test(shock$HR2)
wilcox.test(shock$HR1, shock$HR2, paired=T)

#세 군의 평균 비교
#shock TYPE에 따라 SP1이 다른지 검정
shock$TYPE<-as.factor(shock$TYPE)
boxplot(SP1~TYPE, data=shock)
fit<-aov(SP1~TYPE, data=shock)
summary(fit)
bartlett.test(SP1~TYPE, data=shock)

TukeyHSD(fit)
install.packages("agricolae")
library(agricolae)
duncan.test(fit,"TYPE",alpha=0.05,console=T)

#상관 분석
#HEIGHT와 BSA의 상관 관계
cor.test(shock$HEIGHT, shock$BSA, method="pearson")
plot(shock$HEIGHT, shock$BSA)

#다중 회귀
#재원기간(TIME)을 예측할 수 있는 회귀 모형 구축
fit.time<-glm(TIME~AGE+SEX+SP1+TYPE, family=gaussian(), data=shock)
summary(fit.time)

#어떤 변수가 유효한 변수인지 알기 위해 변수 선택법을 사용
library(MASS)
full.time<-glm(TIME~AGE+HEIGHT+SEX+SP1+DP1+HR1+DP2+SP2+HR2+BSA+UO+HB+TYPE, family=gaussian(), data=shock)
summary(full.time)
step.time<-stepAIC(full.time, direction="both", trace=F)
summary(step.time)

#다중공선성 확인
library(car)
vif(step.time)
vif(full.time)

#로짓 회귀 분석
#생존여부 (SURVIVAL)의 확률이 shock 타입 (TYPE)에 따라 다른지 검정
shock$SURVIVAL<-as.factor(shock$SURVIVAL)
fit.surv<-glm(SURVIVAL~TYPE, family=binomial, data=shock)
summary(fit.surv)
exp(cbind(coef(fit.surv),confint(fit.surv)))

multifit.surv<-glm(SURVIVAL~TYPE+AGE+SEX+SP1+BSA+UO+HB, family=binomial, data=shock)
summary(multifit.surv)
exp(cbind(coef(multifit.surv),confint(multifit.surv)))

#stepwise
step.surv<-stepAIC(multifit.surv, direction="both", trace=F)
summary(step.surv)
exp(cbind(coef(multifit.surv),confint(multifit.surv)))
