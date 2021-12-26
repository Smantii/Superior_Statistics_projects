#carico i dati e li trasformo
A = read.csv("RAILFRTINTERMODAL.csv", row.names = 1, stringsAsFactors = F)
ts = ts(A, deltat=1/12, start = c(2000,1))

#primo plot
plot(ts, main = "Unità merci intermodali per trasporti ferroviari")

#guardo l'autocorrelazione
acf(ts, 48, main = "ACF serie storica")

#provo a decomporre la serie additivamente e moltiplicativamente
ts.da = decompose(ts, type = "a")
ts.dm = decompose(ts, type = "m")
plot(ts.da)
plot(ts.dm)

#diamo un'occhiata all'autocorrelazione e al plot dei residui di entrambe le decomposizioni
ts.dar=as.vector(window(ts.da$random,c(2000,7),c(2018,6)))
plot(ts.dar,pch = 20, main = "Residui della decomposizione additiva")
acf(ts.dar, main = "ACF residui decomposizione additiva")
ts.dmr=as.vector(window(ts.dm$random,c(2000,7),c(2018,6)))
plot(log(ts.dmr),pch = 20, main = "Logaritmo dei residui della decomposizione moltiplicativa")
acf(log(ts.dmr), main = "ACF residui decomposizione moltiplicativa")


#proviamo ad utilizzare stagionalità variabile
ts.stl = stl(ts[,1], 7)
plot(ts.stl)
plot(ts.stl$time.series[,3], main = "Serie dei residui")


#proviamo ad utilizzare e confrontare HW additivo e moltiplicativo
ts.hwa = HoltWinters(ts, seasonal = "a")
ts.hwm = HoltWinters(ts, seasonal = "m")
ts.plot(ts,ts.hwa$fitted[,1],col=c("black","red"), main = "Holt-Winters additivo")
ts.plot(ts,ts.hwm$fitted[,1],col=c("black","red"), main = "Holt-Winters moltiplicativo")


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


#studiamo i residui
ts.hwa.r=resid(ts.hwa)
ts.hwm.r=resid(ts.hwm)
# proporzione di varianza non spiegata
vra =var(ts.hwa.r)/var(window(ts,2000))
vrm =var(ts.hwm.r)/var(window(ts,2000))
cat("Varianza non spiegata HW additivo:", vra)
cat("Varianza non spiegata HW moltiplicativo:", vrm)
# rappresentazione grafica rispetto al tempo
plot(ts.hwa.r, type = "p", pch = 20, main = "Rappresentazione grafica residui HW-a rispetto al tempo")
plot(ts.hwm.r, type = "p", pch = 20, main = "Rappresentazione grafica residui HW-m rispetto al tempo")
# rappresentazione grafica rispetto ai valori stimati
plot(as.numeric(ts.hwa$fitted[,1]),as.numeric(ts.hwa.r),type="p",pch=20, main = "Rappresentazione grafica residui HW-a rispetto ai valori stimati")
plot(as.numeric(ts.hwm$fitted[,1]),as.numeric(ts.hwm.r),type="p",pch=20, main = "Rappresentazione grafica residui HW-m rispetto ai valori stimati")# autocorrelazione
acf(ts.hwa.r, main = "ACF residui HW-a")
acf(ts.hwm.r, main = "ACF residui HW-m")


#autovalidation
train = window(ts, end = c(2016, 12))
test = window(ts, start = c(2017,1), end = c(2018,12))
tscv.hwa.p = predict(HoltWinters(train, seasonal = "a"), 24)
tscv.hwm.p = predict(HoltWinters(train, seasonal = "m"), 24)
ts.plot(test, tscv.hwa.p, tscv.hwm.p, col = c("black", "red", "blue"), main = "Previsione (24 mesi)")
sqrt(mean((tscv.hwa.p - test)^2))
sqrt(mean((tscv.hwm.p - test)^2))



l=length(ts)
res.hwa=rep(0,24)
res.hwm=rep(0,24)
j=1
for(i in (l-24):(l-1)){
  ts_cv=ts(ts[1:i],frequency=12,start=c(2000,1))
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

#guardiamo la funzione di autocorrelazione parziale
pacf(ts, main = "PACF serie storica")

#confrontiamo Yule-Walker e minimi quadrati
ts.ar = ar(ts)
ts.ls = ar(ts, method = "ols")
ts.plot(ts, ts - ts.ar$resid, col = c("black", "red"), main = "Metodo di Yule Walker")
ts.plot(ts, ts - ts.ls$resid, col = c("black", "blue"), main = "Metodo dei minimi quadrati")
C = matrix(nrow = 2, ncol = 15)
C[1,] = c(ts.ar$ar, NA, NA)
C[2,] = c(ts.ls$ar)
C = data.frame(C)
rownames(C) = c("YW", "OLS")

#analisi dei residui
ts.ar.r=na.omit(ts.ar$resid)
ts.ls.r=na.omit(ts.ls$resid)
# proporzione di varianza non spiegata
vra =var(ts.ar.r)/var(window(ts,2000))
vrm =var(ts.ls.r)/var(window(ts,2000))
# rappresentazione grafica rispetto al tempo
plot(ts.ar.r, type = "p", pch = 20)
plot(ts.ls.r, type = "p", pch = 20)
# autocorrelazione parziale
acf(ts.ar.r, main = "ACF residui YW")
acf(ts.ls.r, main = "ACF residui OLS")

#autovalidation
train = window(ts, end = c(2016, 12))
test = window(ts, start = c(2017,1), end = c(2018,12))
tscv.ar.p = predict(ar(train), n.ahead = 24, se.fit = FALSE)
tscv.ls.p = predict(ar(train, method = "ols"), n.ahead = 24, se.fit = FALSE)
ts.plot(test, tscv.ar.p, tscv.ls.p, col = c("black", "red", "blue"), main = "Previsione (24 mesi)")
legend("topleft",legend=c("Yule Walker", "Minimi quadrati"), col=c("red", "blue"), lty=1, cex=0.8)

l=length(ts)
res.arv=rep(0,24)
res.lsv=rep(0,24)
j=1
for(i in (l-24):(l-1)){
  ts_cv=ts(ts[1:i],frequency=12,start=c(2000,1))
  ts.arv=ar(ts_cv)
  ts.lsv=ar(ts_cv, method = "ols")
  ts.arv.p=predict(ts.arv,n.ahead = 1, se.fit = FALSE)
  ts.lsv.p=predict(ts.lsv,n.ahead= 1, se.fit = FALSE)
  res.arv[j]=ts.arv.p - ts[i+1]
  res.lsv[j]=ts.lsv.p - ts[i+1]
  j=j+1
}
cat(" Varianza dell'errore nel metodo Yule Walker:", sqrt(mean(res.arv^2)))
cat(" Varianza dell'errore nel metodo dei minimi quadrati:", sqrt(mean(res.lsv^2)))
plot(res.arv,type="b",pch=20,col="red", main = "Errore nella previsione (24 mesi)")
lines(res.lsv,type="b",pch=20,col="blue")
legend("topright",legend=c("Yule Walker", "Minimi quadrati"), col=c("red", "blue"), lty=1, cex=0.8)


#vediamo se per OLS si ha l'ipotesi di gaussianità
hist(ts.ls.r, 20, freq = F)
lines(density(ts.ls.r),col="blue")
lines(sort(ts.ls.r), dnorm(sort(ts.ls.r), mean(ts.ls.r), sd(ts.ls.r)), col = "red")
qqnorm(ts.ls.r, pch = 20)
qqline(ts.ls.r)
shapiro.test(ts.ls.r)

#serie più lunga fino al 2019
N = read.csv("Serie2.csv", row.names = 1, stringsAsFactors = F)
ts2 = ts(N, deltat=1/12, start = c(2019,1))


#predizione
ts.2015 = window(ts, start = c(2017,1))
ts.ls.2015 = window(ts - ts.ls$resid, start = c(2017,1))
ts.ls.p = predict(ts.ls, n.ahead = 12, se.fit = FALSE)
ts.plot(ts.2015, ts.ls.2015, ts2, ts.ls.p, col = c("black","cyan","blue","red"))
lines(ts.ls.p+quantile(ts.ls.r,0.05),col="green3")
lines(ts.ls.p+quantile(ts.ls.r,0.95),col="green3")
legend("topleft",legend=c("Serie storica", "Minimi quadrati", "Serie nel 2019", "Previsione", "Incertezza"), col=c("black","cyan","blue","red", "green3"), lty=1, cex=0.4)


