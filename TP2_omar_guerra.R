library(purrr) 

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

#TP2

#problema 2

M1=rexp(100,2)
M2=rexp(500,2)
M3=rexp(1000,2)
M4=rexp(2000,2)

IC<-function(X,al){
    n=length(X)
    q1=qgamma(al/2,n,1)
    q2=qgamma(1-(al/2),n,1)
    a=q1/sum(X)
    b=q2/sum(X)
    return(c(a,b))
}
IC(M1,0.1)
IC(M2,0.1)
IC(M3,0.1)
IC(M4,0.1)


# problema 3

MON<-function(m){
    S=0
    for (i in 1:m){
        X=rexp(100,2)
        t=IC(X,0.1)
        if ((2>=t[1])&(2<=t[2])){
            S=S+1
        }
    }
    return(S/m)
}

## Problema 2
# intervalo normal

ICnorm<-function(X,alf,B){
    z=qnorm(1-(alf/2))
    s=sesgoboot(X,B)
    v=var1(X,B)
    th=1/mean(X)
    a=th-s-z*(v**(1/2))
    b=th-s+z*(v**(1/2))
    return(c(a,b))
}



# estimador boot

bo<-function(X,B){
    T=c(1:B)
    for (i in 1:B){
        x=gener3(X)
        T[i]=1/mean(x)
    }
    return(T)
}

# intervalo basic0

ICbasic<-function(X,alf,B){
    q1=quantile(bo(X,B),1-(alf/2),type=1)
    q2=quantile(bo(X,B),alf/2,type=1)
    th=1/mean(X)
    a=2*th-q1
    b=2*th-q2
    return(c(a,b))
}

#intervalo percentil

ICper<-function(X,alf,B){
    q1=quantile(bo(X,B),alf/2,type=1)
    q2=quantile(bo(X,B),1-(alf/2),type=1)
    return(c(q1,q2))
}

zj<-function(X,B){
    Z=c(1:B)
    T=c(1:B)
    th=1/mean(X)
    for (i in 1:B){
        x=gener3(X)
        T[i]=bo(X,B)
        Z[i]=(T[i]-th)/(sqrt(var1(x,B)))
    }
    return(Z)
}

# intervalo studentizado

ICstu<-function(X,alf,B){
    q1=quantile(zj(X,B),1-(alf/2),type=1)
    th=1/mean(X)
    a=th-q1*sqrt(var1(X,B))
    b=th+q1*sqrt(var1(X,B))
    return(c(a,b))
}

# 2
#mostecarlo para para ic normal
MONicnorm<-function(m){
    S=0
    for (i in 1:m){
        X=rexp(500,2)
        t=ICnorm(X,0.1,1000)
        if ((2>=t[1])&(2<=t[2])){
            S=S+1
        }
    }
    return(S/m)
}

MONicnorm(1000)

# montecarlo para ic basico

MONicbasic<-function(m){
    S=0
    for (i in 1:m){
        X=rexp(500,2)
        t=ICbasic(X,0.1,1000)
        if ((2>=t[1])&(2<=t[2])){
            S=S+1
        }
    }
    return(S/m)
}

MONicbasic(1000)

# montecarlo para ic percentil

MONicper<-function(m){
    S=0
    for (i in 1:m){
        X=rexp(500,2)
        t=ICper(X,0.1,1000)
        if ((2>=t[1])&(2<=t[2])){
            S=S+1
        }
    }
    return(S/m)
}

MONicper(1000)

# montecarlo para ic studentizado

MONicstu<-function(m){
    S=0
    for (i in 1:m){
        X=rexp(500,2)
        t=ICstu(X,0.1,1000)
        if ((2>=t[1])&(2<=t[2])){
            S=S+1
        }
    }
    return(S/m)
}

MONicstu(1000)

#3

#para B=200

#mostecarlo para para ic normal
MONicnorm1<-function(m){
    S=0
    for (i in 1:m){
        X=rexp(500,2)
        t=ICnorm(X,0.1,200)
        if ((2>=t[1])&(2<=t[2])){
            S=S+1
        }
    }
    return(S/m)
}

MONicnorm1(1000)

# montecarlo para ic basico

MONicbasic1<-function(m){
    S=0
    for (i in 1:m){
        X=rexp(500,2)
        t=ICbasic(X,0.1,200)
        if ((2>=t[1])&(2<=t[2])){
            S=S+1
        }
    }
    return(S/m)
}

MONicbasic1(1000)

# montecarlo para ic percentil

MONicper1<-function(m){
    S=0
    for (i in 1:m){
        X=rexp(500,2)
        t=ICper(X,0.1,200)
        if ((2>=t[1])&(2<=t[2])){
            S=S+1
        }
    }
    return(S/m)
}

MONicper1(1000)

# montecarlo para ic studentizado

MONicstu1<-function(m){
    S=0
    for (i in 1:m){
        X=rexp(500,2)
        t=ICstu(X,0.1,200)
        if ((2>=t[1])&(2<=t[2])){
            S=S+1
        }
    }
    return(S/m)
}

MONicstu1(1000)

# Para B=2000

#mostecarlo para para ic normal
MONicnorm2<-function(m){
    S=0
    for (i in 1:m){
        X=rexp(500,2)
        t=ICnorm(X,0.1,2000)
        if ((2>=t[1])&(2<=t[2])){
            S=S+1
        }
    }
    return(S/m)
}

MONicnorm2(1000)

# montecarlo para ic basico

MONicbasic2<-function(m){
    S=0
    for (i in 1:m){
        X=rexp(500,2)
        t=ICbasic(X,0.1,2000)
        if ((2>=t[1])&(2<=t[2])){
            S=S+1
        }
    }
    return(S/m)
}

MONicbasic2(1000)

# montecarlo para ic percentil

MONicper2<-function(m){
    S=0
    for (i in 1:m){
        X=rexp(500,2)
        t=ICper(X,0.1,2000)
        if ((2>=t[1])&(2<=t[2])){
            S=S+1
        }
    }
    return(S/m)
}

MONicper(2000)

# montecarlo para ic studentizado

MONicstu<-function(m){
    S=0
    for (i in 1:m){
        X=rexp(500,2)
        t=ICstu(X,0.1,2000)
        if ((2>=t[1])&(2<=t[2])){
            S=S+1
        }
    }
    return(S/m)
}

MONicstu2(1000)

#4

## coste muy alto al parecer...


