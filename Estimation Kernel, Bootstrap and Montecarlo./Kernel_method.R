library(purrr)

M1=rexp(100,2)

hist(M1,freq = FALSE)


K<-function(x){
    k=(2*pi)**(-1/2)*exp((-1/2)*x**2)
    return(k)
}

sumar<-function(x,X,h){
    n=length(X)
    suma=c(1:n)
    for (i in 1:n){
        suma[i]=K((x-X[i])/h)
    }
    s=sum(suma)
    return(s)
}

fgorro<-function(x,X,h){
    n=length(X)
    f=(1/(n*h))*sumar(x,X,h)
    return(f)
}

y<-c()
for (i in 1:100){
    t<-seq(0.02,2,by=0.02)
    y<-c(y,fgorro(t[i], M1,0.1))
}
print(y)



t=c(seq(0.02,2,by=0.02))

plot(t,y)
dx <- density(M1)
hist(M1,freq = FALSE)
lines(t,y, col="blue")
lines(dx, lwd = 2, col = "red")


datoh<- read.csv("/Users/omarguerra/Documents/2022/cursos semestre 9/modelación 2/2da parte prof martiza/informe 1/heart.csv",header=TRUE)
attach(datoh)
head(datoh)
X1<-heart.rate
Y<-X1[c(1:100)]


z<-c()
for (i in 1:100){
    t1<-seq(40.8,120,by=0.8)
    z<-c(z,fgorro(t1[i], Y,3))
}
print(z)



t1=c(seq(40.8,120,by=0.8))

plot(t1,z)
dx1 <- density(Y)
hist(Y,freq = FALSE)
lines(t1,z, col="blue")
lines(dx1, lwd = 2, col = "red")
