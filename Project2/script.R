#librerie utilizzate
library(cluster)
library(MASS)
library(knitr)
library(ggfortify)

#carico la tabella e salvo la pca
A = read.csv("tabella.csv", row.names = 1)
colnames(A) = c("F1","F2","F3","F4","F5","F6","F7")
B = scale(A)
pca = princomp(B)

#riportiamo qui alcuni degli strumenti più utilizzati per semplicità
pr.km = kmeans(B,4, nstart = 10, iter.max = 50)
isFirst <- ifelse(pr.km$cluster == 1,"red","grey")
isSecond <- ifelse(pr.km$cluster == 2,"green","grey")
isThird <- ifelse(pr.km$cluster == 3,"blue","grey")
isFourth <- ifelse(pr.km$cluster == 4,"cyan","grey")
d<-dist(B)
pr.hcc=hclust(d,"complete")
pr.hca=hclust(d,"average")
pr.hcs=hclust(d,"single")



#summary della tabella
C = as.data.frame(sapply(A, summary))
knitr::kable(C, format="markdown")

#silhouette media per k-means e pam
as1=rep(0,15)
for(k in 2:15){
  cl=kmeans(B,k,nstart=10,iter.max = 50)$cluster
  as1[k]=mean(silhouette(cl,dist(B))[,3])
}
plot(2:15,as1[2:15],type="b",pch=20, xlab = "Numero di cluster", ylab = "Silhouette media per k-means")

as2=rep(0,15)
for(k in 2:15){
  as2[k]= pam(B,k, metric = "euclidean")$silinfo$avg.width
}
plot(2:15,as2[2:15],type="b",pch=20, xlab = "Numero di cluster", ylab = "Silhouette media per pam")

#wss per k-means
wss1=rep(0,15)
for(k in 2:15){
  wss1[k]=kmeans(B,k,nstart=15)$tot.withinss
}
plot(2:15,wss1[2:15],type="b",pch=20, xlab = "Numero di cluster", ylab = "WSS per k-means")


#silhouette per k-means
plot(silhouette(kmeans(B,4,nstart=10, iter.max = 50)$cluster,dist(B)),  col=2:5, border=NA)
plot(silhouette(kmeans(B,5,nstart=10, iter.max = 50)$cluster,dist(B)), col=6:10, border=NA)
plot(silhouette(kmeans(B,6,nstart=10, iter.max = 50)$cluster,dist(B)), col=11:16, border=NA)
plot(silhouette(kmeans(B,7,nstart=10, iter.max = 50)$cluster,dist(B)), col=17:23, border=NA)

#silhouette per pam
plot(pam(B,3, metric = "euclidean"))
plot(pam(B,4, metric = "euclidean"))

#dendogrammi per i tre metodi gerarchici
plot(pr.hcc,hang=-1,cex=0.5, ylab = "complete linkage")
plot(pr.hca,hang=-1,cex=0.5, ylab = "average linkage")
plot(pr.hcs,hang=-1,cex=0.5, ylab = "single linkage")


#silhouette media per complete e average linkage
as1=rep(0,15)
as2 = rep(0,15)
for(i in 2:15){
  pr.cut=cutree(pr.hcc,i)
  as1[i]=mean(silhouette(pr.cut,d)[,3])
}
for(i in 2:15){
  pr.cut=cutree(pr.hca,i)
  as2[i]=mean(silhouette(pr.cut,d)[,3])
}
plot(2:20,as1[2:20],type="b", xlab = "Numero di cluster", ylab = "Complete linkage")
plot(2:20,as2[2:20],type="b", xlab = "Numero di cluster", ylab = "Average linkage")

#numerosità dei cluster per alcuni casi interessanti (maggiori dettagli nella relazione)
F = matrix(nrow = 5, ncol = 10)
F[1,] = c(table(cutree(pr.hcc, 6)),NA,NA,NA,NA)
F[2,] = c(table(cutree(pr.hcc, 7)),NA,NA,NA)
F[3,] = c(table(cutree(pr.hcc, 8)),NA,NA)
F[4,] = c(table(cutree(pr.hcc, 9)),NA)
F[5,] = c(table(cutree(pr.hcc, 10)))
F = data.frame(F)
colnames(F) = c("1", "2","3","4","5","6","7","8","9","10")
G = matrix(nrow = 5, ncol = 11)
G[1,] = c(table(cutree(pr.hca, 7)),NA,NA,NA,NA)
G[2,] = c(table(cutree(pr.hca, 8)),NA,NA,NA)
G[3,] = c(table(cutree(pr.hca, 9)),NA,NA)
G[4,] = c(table(cutree(pr.hca, 10)),NA)
G[5,] = c(table(cutree(pr.hca, 11)))
G = data.frame(G)
colnames(G) = c("1", "2","3","4","5","6","7","8","9","10", "11")
knitr::kable(F, format="markdown")
knitr::kable(G, format="markdown")


#silhouette per complete linkage
plot(silhouette(cutree(pr.hcc,7),d), col=2:8)
plot(silhouette(cutree(pr.hcc,8),d), col=9:16)
plot(silhouette(cutree(pr.hcc,9),d), col=17:25)


#confronto tra k-means e complete linkage
autoplot(pca, col=1+pr.km$cluster, label = TRUE, shape = FALSE, label.size = 2)
autoplot(pca, col=1+cutree(pr.hcc,7), label = TRUE, shape = FALSE, label.size = 2)


#proiezione sul piano principale delle osservazioni e visione dei cluster per k-means
autoplot(pca, col=1+pr.km$cluster, label = TRUE, shape = FALSE, label.size = 2)

#coordinate parallele
parcoord(B, col = isFirst)
parcoord(B, col = isSecond)
parcoord(B, col = isThird)
parcoord(B, col = isFourth)


#tabella osservazioni "anomale"
C = A[c(6, 18, 22), ]
knitr::kable(C, format="markdown")

