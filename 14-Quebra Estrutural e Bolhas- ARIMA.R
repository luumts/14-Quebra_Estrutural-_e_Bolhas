                    #Aula 14 - Quebra Estrural e Bolhas

install.packages("strucchange")

library(strucchange)
library(readxl)


BITCOIN <- read_excel("C:/Econometria/Bitcoin.xls")

Bitcoin <- ts <- ts(BITCOIN$Close, start = 2014, frequency = 365)

plot(Bitcoin)

#Teste de Chow

chow <- Fstats(Bitcoin~1)    #Executa o Teste de F de Chow
sctest(chow)                 #Retorna a Estatística de Teste e o p-valor

plot(Bitcoin)
lines(breakpoints(chow))

plot(chow)
    
#Teste Bai Perron

bp_ts <- breakpoints(Bitcoin ~ 1)

bp_ts

summary(bp_ts)

#ci_ts <- confint(bp_ts)

plot(Bitcoin)               
lines(bp_ts)            #Gráfico com os breakpoints


#Gráfico com as linhas de tendências para os três períodos

fm0 <- lm(Bitcoin ~ 1)
fm1 <- lm(Bitcoin ~ breakfactor(bp_ts, breaks = 1))
fm2 <- lm(Bitcoin ~ breakfactor(bp_ts, breaks = 2))
plot(Bitcoin)
lines(ts(fitted(fm0), start = 2014, freq=365), col = 3)
lines(ts(fitted(fm1), start = 2014, frequency=365), col = 4)
lines(ts(fitted(fm2), start = 2014, frequency=365), col = 1)
lines(bp_ts)


#Modelo Integrado de Ordem 1

MIO1 <- diff(Bitcoin)
plot(MIO1)

#É estacionária?

Não

#FAC e FACP

acf(Bitcoin$Close, main="Criptomoeda Bitcoin - FAC")   	
pacf(Bitcoin$Close, main="Criptomoeda Bitcoin - FACP") 

#Qual a ordem do modelo ARIMA(p,d,q)

ARIMA(1,d,32)

#Quais combinações a serem estimadas?

AR1 <- arima(Perto, order = c(1,0,0))
AR1

MA1 <- arima(Perto,order = c(0,0,1))
MA1
MA2 <- arima(Perto,order = c(0,0,2))
MA2
MA3 <- arima(Perto,order = c(0,0,3))
MA3
MA4 <- arima(Perto,order = c(0,0,4))
MA4
MA5 <- arima(Perto,order = c(0,0,5))
MA5
MA6 <- arima(Perto,order = c(0,0,6))
MA6
MA7 <- arima(Perto,order = c(0,0,7))
MA7
MA8 <- arima(Perto,order = c(0,0,8))
MA8
MA9 <- arima(Perto,order = c(0,0,9))
MA9
MA10 <- arima(Perto,order = c(0,0,10))
MA10
MA11 <- arima(Perto,order = c(0,0,11))
MA11
MA12 <- arima(Perto,order = c(0,0,12))
MA12
MA13 <- arima(Perto,order = c(0,0,13))
MA13
MA14 <- arima(Perto,order = c(0,0,14))
MA14
MA15 <- arima(Perto,order = c(0,0,17))
MA15
MA16 <- arima(Perto,order = c(0,0,16))
MA16
MA17 <- arima(Perto,order = c(0,0,17))
MA17
MA18 <- arima(Perto,order = c(0,0,18))
MA18
MA19 <- arima(Perto,order = c(0,0,19))
MA19
MA20 <- arima(Perto,order = c(0,0,20))
MA20
MA21 <- arima(Perto,order = c(0,0,21))
MA21
MA22 <- arima(Perto,order = c(0,0,22))
MA22
MA23 <- arima(Perto,order = c(0,0,23))
MA23
MA24 <- arima(Perto,order = c(0,0,24))
MA24
MA25 <- arima(Perto,order = c(0,0,25))
MA25
MA26 <- arima(Perto,order = c(0,0,26))
MA26
MA27 <- arima(Perto,order = c(0,0,27))
MA27
MA28 <- arima(Perto,order = c(0,0,28))
MA28
MA29 <- arima(Perto,order = c(0,0,29))
MA29
MA30 <- arima(Perto,order = c(0,0,30))
MA30
MA31 <- arima(Perto,order = c(0,0,31))
MA31
MA32 <- arima(Perto,order = c(0,0,32))
MA32

ARMA11 <- arima(Perto,order = c(1,0,1))
ARMA11
ARMA12 <- arima(Perto,order = c(1,0,2))
ARMA12
ARMA13 <- arima(Perto,order = c(1,0,3))
ARMA13
ARMA14 <- arima(Perto,order = c(1,0,4))
ARMA14
ARMA15 <- arima(Perto,order = c(1,0,5))
ARMA15
ARMA16 <- arima(Perto,order = c(1,0,6))
ARMA16
ARMA17 <- arima(Perto,order = c(1,0,7))
ARMA17
ARMA18 <- arima(Perto,order = c(1,0,8))
ARMA18
ARMA19 <- arima(Perto,order = c(1,0,9))
ARMA19
ARMA110 <- arima(Perto,order = c(1,0,10))
ARMA110
ARMA111 <- arima(Perto,order = c(1,0,11))
ARMA111
ARMA112 <- arima(Perto,order = c(1,0,12))
ARMA112
ARMA113 <- arima(Perto,order = c(1,0,13))
ARMA113
ARMA114 <- arima(Perto,order = c(1,0,14))
ARMA114
ARMA115 <- arima(Perto,order = c(1,0,17))
ARMA115
ARMA116 <- arima(Perto,order = c(1,0,16))
ARMA116
ARMA117 <- arima(Perto,order = c(1,0,17))
ARMA117
ARMA118 <- arima(Perto,order = c(1,0,18))
ARMA118
ARMA119 <- arima(Perto,order = c(1,0,19))
ARMA119
ARMA120 <- arima(Perto,order = c(1,0,20))
ARMA120
ARMA121 <- arima(Perto,order = c(1,0,21))
ARMA121
ARMA122 <- arima(Perto,order = c(1,0,22))
ARMA122
ARMA123 <- arima(Perto,order = c(1,0,23))
ARMA123
ARMA124 <- arima(Perto,order = c(1,0,24))
ARMA124
ARMA125 <- arima(Perto,order = c(1,0,25))
ARMA125
ARMA126 <- arima(Perto,order = c(1,0,26))
ARMA126
ARMA127 <- arima(Perto,order = c(1,0,27))
ARMA127
ARMA128 <- arima(Perto,order = c(1,0,28))
ARMA128
ARMA129 <- arima(Perto,order = c(1,0,29))
ARMA129
ARMA130 <- arima(Perto,order = c(1,0,30))
ARMA130
ARMA131 <- arima(Perto,order = c(1,0,31))
ARMA131
ARMA132 <- arima(Perto,order = c(1,0,32))
ARMA132

#Os comandos AIC e BIC.

AIC(AR1)
BIC(AR1)
AIC(MA1)
BIC(MA1)
AIC(MA2)
BIC(MA2)
AIC(MA3)
BIC(MA3)
AIC(MA4)
BIC(MA4)
AIC(MA5)
BIC(MA5)
AIC(MA6)
BIC(MA6)
AIC(MA7)
BIC(MA7)
AIC(MA8)
BIC(MA8)
AIC(MA9)
BIC(MA9)
AIC(MA10)
BIC(MA10)
AIC(MA11)
BIC(MA11)
AIC(MA12)
BIC(MA12)
AIC(MA13)
BIC(MA13)
AIC(MA14)
BIC(MA14)
AIC(MA15)
BIC(MA15)
AIC(MA16)
BIC(MA16)
AIC(MA17)
BIC(MA17)
AIC(MA18)
BIC(MA18)
AIC(MA19)
BIC(MA19)
AIC(MA20)
BIC(MA20)
AIC(MA21)
BIC(MA21)
AIC(MA22)
BIC(MA22)
AIC(MA23)
BIC(MA23)
AIC(MA24)
BIC(MA24)
AIC(MA25)
BIC(MA25)
AIC(MA26)
BIC(MA26)
AIC(MA27)
BIC(MA27)
AIC(MA28)
BIC(MA28)
AIC(MA29)
BIC(MA29)
AIC(MA30)
BIC(MA30)
AIC(MA31)
BIC(MA31)
AIC(MA32)
BIC(MA32)
AIC(ARMA11)
BIC(ARMA11)
AIC(ARMA12)
BIC(ARMA12)
AIC(ARMA13)
BIC(ARMA13)
AIC(ARMA14)
BIC(ARMA14)
AIC(ARMA15)
BIC(ARMA15)
AIC(ARMA16)
BIC(ARMA16)
AIC(ARMA17)
BIC(ARMA17)
AIC(ARMA18)
BIC(ARMA18)
AIC(ARMA19)
BIC(ARMA19)
AIC(ARMA110)
BIC(ARMA110)
AIC(ARMA111)
BIC(ARMA111)
AIC(ARMA112)
BIC(ARMA112)
AIC(ARMA113)
BIC(ARMA113)
AIC(ARMA114)
BIC(ARMA114)
AIC(ARMA115)
BIC(ARMA115)
AIC(ARMA116)
BIC(ARMA116)
AIC(ARMA117)
BIC(ARMA117)
AIC(ARMA118)
BIC(ARMA118)
AIC(ARMA119)
BIC(ARMA119)
AIC(ARMA120)
BIC(ARMA120)
AIC(ARMA121)
BIC(ARMA121)
AIC(ARMA122)
BIC(ARMA122)
AIC(ARMA123)
BIC(ARMA123)
AIC(ARMA124)
BIC(ARMA124)
AIC(ARMA125)
BIC(ARMA125)
AIC(ARMA126)
BIC(ARMA126)
AIC(ARMA127)
BIC(ARMA127)
AIC(ARMA128)
BIC(ARMA128)
AIC(ARMA129)
BIC(ARMA129)
AIC(ARMA130)
BIC(ARMA130)
AIC(ARMA131)
BIC(ARMA131)
AIC(ARMA132)
BIC(ARMA132)

