library(purrr) 
# 
# 1 muestra bootstrap
# A=rexp(10,1)
# 
# generamos los xi's uno por uno
# gener<-function(X) {
#   n=length(X)
#   i=rdunif(1,n,1)
#   xstar=X[i]
#   return(xstar)
# }
# 
# generamos una muestra bootstrap en base a ciclo for
# boot<-function(X) {
#   xstar=c(1:length(X))
#   for (i in 1:length(X)){
#     xstar[i]=gener(X)
#   }
#   return(xstar)
# }

#generamos una muestra bootstrap sin ciclo
gener3<-function(X){
  n=length(X)
  I=rdunif(n,n,1)
  xstar=X[I]
  return(xstar)
}

# 2 estimador bootstrap de la varianza

var1<-function(X,B){
  T=c(1:B)
  for (j in 1:B){
    xstar=gener3(X)
    T[j]=1/(mean(xstar))
  }
  V=(1/B)*sum(T*T)-((1/B)*sum(T))**2
  return(V)
}

#3 estimador sesgo

sesgoboot<-function(X,B){
  T=c(1:B)
  for (j in 1:B){
    xstar=gener3(X)
    T[j]=1/(mean(xstar))
  }
  sesg=(1/B)*sum(T)-1/mean(X)
  return(sesg)
}

# problema 5

Ex=rexp(100,2) #muestra exponencial de tamaño 100 y parámetro 2

datosgraficases<-function(X){
  a=seq(10,2000,10)
  m=length(a)
  S=c()
  for (B in seq(10,2000,10)){
    s=sesgoboot(X,B)
    S=c(S,s)
  }
  return(S)
}
a=seq(10,2000,10)

SES=datosgraficases(Ex)


plot1=plot(a,SES)
ab1=abline(2/99,1,2/99, col="red")

# problema 6

datosgraficavar<-function(X){
    a=seq(10,2000,10)
    m=length(a)
    S=c()
    for (B in seq(10,2000,10)){
        s=var1(X,B)
        S=c(S,s)
    }
    return(S)
}

VAR=datosgraficavar(Ex)
varmues=(((100**2)*(2**2))/((99**2)*(98)))
plot2=plot(a,VAR)
ab2=abline(varmues,1,varmues, col="red")

# problema 7, repetimos para una muestra de mayor tamaño, n=500
Ex1=rexp(500,2)

# problema 7.5

SES1=datosgraficases(Ex1)
plot3=plot(a,SES1)
ab3=abline(2/499,1,2/499, col="red")

# problema 7.6

VAR1=datosgraficavar(Ex1)
varmues1=(((500**2)*(2**2))/((499**2)*(498)))
plot4=plot(a,VAR1)
ab4=abline(varmues1,c(a),varmues1,col="red")









