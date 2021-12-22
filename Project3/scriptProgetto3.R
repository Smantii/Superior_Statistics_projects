#carico i dati e li trasformo
A = read.csv("RAILFRTINTERMODAL.csv", row.names = 1, stringsAsFactors = F)
ts = ts(A, deltat=1/12, start = c(2000,1))

#primo plot
plot(ts, main = "Unità merci intermodali per trasporti ferroviari")

#guardo l'autocorrelazione
acf(ts, 30)

#provo a decomporre la serie additivamente e moltiplicativamente
ts.da = decompose(ts, type = "a")
ts.dm = decompose(ts, type = "m")
plot(ts.da)
plot(ts.dm)

#diamo un'occhiata all'autocorrelazione e al plot dei residui di entrambe le decomposizioni
ts.dar=as.vector(window(ts.da$random,c(2000,7),c(2018,6)))
plot(ts.dar, pch=20)
acf(ts.dar)
ts.dmr=as.vector(window(ts.dm$random,c(2000,7),c(2018,6)))
ts.dmrl = log(ts.dmr)
plot(ts.dmrl, pch=20)
acf(ts.dmrl)
var(ts.dar)/var(window(ts,c(2000,7),c(2018,6)))
var(ts.dmrl)/var(window(log(ts),c(2000,7),c(2018,6)))


#proviamo ad utilizzare stagionalità variabile
ts.stl = stl(ts[,1], 7)
plot(ts.stl)
plot(ts.stl$time.series[,3], main = "Serie dei residui")


#proviamo ad utilizzare e confrontare HW additivo e moltiplicativo
ts.hwa = HoltWinters(ts, seasonal = "a")
ts.hwm = HoltWinters(ts, seasonal = "m")
ts.plot(ts,ts.hwa$fitted[,1],ts.hwm$fitted[,1],col=c("black","blue","red"))

#guardiamo i coefficienti scelti da HW di default
C = matrix(nrow = 4, ncol = 3)
C[,1] = c(" ", "alpha", "beta", "gamma")
C[1, 2] = "HW Additivo"
C[1, 3] = "HW Moltiplicativo"
C[2,2] = ts.hwa$alpha
C[2,3] = ts.hwm$alpha
C[3,2] = ts.hwa$beta
C[3,3] = ts.hwm$beta
C[4,2] = ts.hwa$gamma
C[4,3] = ts.hwm$gamma


layout(t(1:2))
# estrazione dei residui
ts.hwa.r=resid(ts.hwa)
ts.hwm.r=resid(ts.hwm)
# proporzione di varianza non spiegata
var(ts.hwa.r)/var(window(ts,1950))
var(ts.hwm.r)/var(window(ts,1950))
# rappresentazione grafica rispetto al tempo
plot(ts.hwa.r, type = "p", pch = 20)
plot(ts.hwm.r, type = "p", pch = 20)
# rappresentazione grafica rispetto ai valori stimati
plot(as.numeric(ts.hwa$fitted[,1]), as.numeric(ts.hwa.r),type="p",pch=20)
plot(as.numeric(ts.hwm$fitted[,1]), as.numeric(ts.hwm.r),type="p",pch=20)
# autocorrelazione
acf(ts.hwa.r)
acf(ts.hwm.r)
# densità empiriche
hist(ts.hwa.r, 20, freq = F)
lines(density(ts.hwa.r),col="blue")
lines(sort(ts.hwa.r), dnorm(sort(ts.hwa.r), mean(ts.hwa.r), sd(ts.hwa.r)), col = "red")
hist(ts.hwm.r, 20, freq = F)
lines(density(ts.hwm.r),col="blue")
lines(sort(ts.hwm.r), dnorm(sort(ts.hwm.r), mean(ts.hwm.r), sd(ts.hwm.r)), col = "red")
# grafico quantile-quantile
qqnorm(ts.hwa.r, pch = 20)
qqline(ts.hwa.r)
qqnorm(ts.hwm.r, pch = 20)
qqline(ts.hwm.r)
# test
shapiro.test(ts.hwa.r)
shapiro.test(ts.hwm.r)
layout(1)

#autovalidation
l=length(ts)
res.hwa=rep(0,24)
res.hwm=rep(0,24)
j=1
for(i in (l-24):(l-1)){
  ap_cv=ts(ts[1:i],frequency=12,start=c(1949,1))
  ts.hwa=HoltWinters(ts_cv,seasonal="additive")
  ts.hwm=HoltWinters(ts_cv,seasonal="multiplicative")
  ts.hwa.p=predict(ts.hwa,1)
  ts.hwm.p=predict(ts.hwm,1)
  res.hwa[j]=ts.hwa.p - ts[i+1]
  res.hwm[j]=ts.hwm.p - ts[i+1]
  j=j+1
}
sqrt(mean(res.hwa^2))
sqrt(mean(res.hwm^2))
plot(res.hwa,type="b",pch=20,col="blue")
lines(res.hwm,type="b",pch=20,col="green3")






