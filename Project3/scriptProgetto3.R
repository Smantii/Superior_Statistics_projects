#carico i dati e li trasformo
A = read.csv("XTEXVA01ITM664N.csv", row.names = 1, stringsAsFactors = T)
ts = ts(A, deltat=1/12, start = c(1990,1))

#primo plot
plot(ts)

#guardo l'autocorrelazione
acf(ts)

#provo a decomporre la serie
plot(decompose(ts))

#vediamo se è presente stagionalità studiando la serie al netto del trend
plot(diff(ts))
acf(diff(ts))
