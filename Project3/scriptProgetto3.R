#carico i dati e li trasformo
A = read.csv("RAILFRTINTERMODAL.csv", row.names = 1, stringsAsFactors = F)
ts = ts(A, deltat=1/12, start = c(2000,1))

#primo plot
plot(ts)

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


#vediamo se è presente stagionalità studiando la serie al netto del trend
plot(diff(ts))
acf(diff(ts))



#proviamo ad utilizzare e confrontare HW additivo e moltiplicativo
ts.hwa = HoltWinters(ts, seasonal = "a")
ts.hwm = HoltWinters(ts, seasonal = "m")
ts.plot(ts,ts.hwa$fitted[,1],ts.hwm$fitted[,1],col=c("black","blue","red"))

#guardiamo i coefficienti scelti da HW di default
print(ts.hwa$alpha)
print(ts.hwa$beta)
print(ts.hwa$gamma)
print(ts.hwm$alpha)
print(ts.hwm$beta)
print(ts.hwm$gamma)




