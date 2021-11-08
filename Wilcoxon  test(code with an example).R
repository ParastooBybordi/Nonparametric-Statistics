#Wilcoxon Signed Rank Test(with an example)
theta=1 #median=1
y=c(-1.5,3,2,4,0.5)
x=y-theta
z=abs(x)
r=rank(z)
s=c()
for (i in 1:5) {
  if (x[i] >= 0) {
    s[i]=1
  } else if (x[i] < 0) {
    s[i]=0
  }
  
}
n=length(y)
w=sum(s*r)        #W+
ew=(n*(n+1))/4    #E[W]
varw=(n*(n+1)*((2*n)+1))/24  #Var(W+)
wstar=(w-ew)/sqrt(varw)      #W*
if(abs(wstar)>qnorm(0.975,0,1)){
  print("Reject H0")
}else{
  print("Accept H0")
}
############################
y=c(-1.5,3,2,4,0.5)
wilcox.test(y,mu=1)
