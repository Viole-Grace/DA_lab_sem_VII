knn<-read.csv('3.csv',header = TRUE, sep=',')
summary(knn)
knn
x=100
y=1000
dist <- transform(knn, distance=sqrt((x-knn$age)^2+(y-knn$loan)^2))
dist
odis <- dist[order(dist$distance), c(3,4)]
odis

k=8

nn<-head(odis,k)
knearest <-table(nn$defaulter)
t<-names(which(knearest==max(knearest)))
cat("Class : ",t)
