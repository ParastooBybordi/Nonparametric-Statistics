##Probability integral transform
## Exponential distribution
f<-function(u,l){            #f= f inverse exponential distribution
  x=((-log(1-u))/l)
  return(x)
}
u<-runif(1000)
l<-2                        #lambda=2
y<-rexp(1000,2)
xi<-f(u,l)
print(c(mean(xi),mean(y),var(xi),var(y)))
plot(density(xi),col="red")
lines(density(y),col="blue")
## Uniform distribution
f1<-function(u){           #f= f inverse uniform distribution
  s=u
  return(s)
}
xi1<-f1(u)
z<-runif(1000)
print(c(mean(xi1),mean(z),var(xi1),var(z)))
plot(density(xi1),col="red")
lines(density(z),col="green")
