#librerie utilizzate
library(ggfortify)
library(corrplot)
library(knitr)

#carico la tabella e la rinomino per semplicità
A = read.csv("tabellaprogetto1.csv", row.names = 1)
colnames(A) = c("F1","F2","F3","F4","F5","F6","F7","F8","F9","F10","F11")
row.names(A) = c(1:18)

#grafico delle correlazioni
corrplot(cor(A), addCoef.col = 'black', tl.pos = 'd', cl.pos = 'n')

#eseguo la PCA
fer.pca = princomp(scale(A))
fer.ld = loadings(fer.pca)

#plot delle varianze spiegate e della varianza cumulata rispetto alle componenti
screeplot(fer.pca, col = "black")
plot(cumsum(fer.pca$sdev^2)/sum(fer.pca$sdev^2),type="b",ylim=c(0,1), xlab = "Componenti", ylab = "Varianza Cumulata")

#calcolo i loadings e la proiezione delle componenti nei piani principali
corrplot(fer.ld,"number", tl.cex = 0.6, number.cex = 0.7,  cl.pos = 'n')
biplot(fer.pca,col=c("gray","red"),choices=c(1,2))
biplot(fer.pca,col=c("gray","red"),choices=c(1,3))


#ruoto alcune delle componenti principali per interpretarle meglio
v1 = varimax(fer.ld[,1:3])
v2 = varimax(fer.ld[,2:3])
v1$loadings
v2$loadings

#rappresentazione delle regioni nei piani principali
orig=rep(2,18)
orig[c(1:7)] = 1
orig[c(17,18)] = 1
orig[16] = 3
autoplot(fer.pca, col=orig, label = TRUE, shape = FALSE)
autoplot(fer.pca, x=1, y=3, col = orig, label = TRUE, shape = FALSE)

#valutazione della stabilità del risultato
fer_s=data.frame(scale(A))
res=rep(0,18)
for(i in 1:18){
  fer_r=fer_s[-i,]
  fer_r.pca=princomp(fer_r)
  fer_p=predict(fer_r.pca,newdata=fer_s[i,])[1:2]
  res[i]=mean((fer_p-predict(fer.pca)[i,1:2])^2)
}
res


