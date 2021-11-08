#Spearman(example)
English_scores<-c(50,23,28,34,14,54,46,52,53)
Math_scores<-c(38,28,14,26,18,40,23,30,27)
z=rank(English_scores)
r=rank(Math_scores)
d2=sum((z-r)^2)
n=length(English_scores)
rs=1-((6*d2)/(n*((n^2)-1)))
zn=rs*sqrt(n-1)
alpha=0.05
if(zn>qnorm(1-alpha,0,1)){
  print("Reject H0")
}else{
  print("Accept H0")
}
##################################
cor.test(English_scores,Math_scores,alternative ="greater" ,method = "spearman")
