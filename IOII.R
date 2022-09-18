library(tidyverse)          #Algoritmo para obter solução ótima
n<-13
k<-500
h<-0.001
sum<-0
r<-matrix(0,1,n)
c<-matrix(1000000,1,n+1)
c[n+1]<-0
c[n]<-k
r[1]=199914
r[2]=202323
r[3]=205806
r[4]=209649
r[5]=209841
r[6]=208867
r[7]=209977
r[8]=209386
r[9]=208904
r[10]=210054
r[11]=210937
r[12]=213389
r[13]=216550
z<-matrix(0,1,n)
I<-matrix(0,1,n)
I[n]=n
for(i in 1:n-1)
{
  z[i]=n-i
}
for(g in as.numeric(1:(n-1))){
  i=z[g]
  for(j in as.numeric(i:n)){
    sum=0
    if(i!=j){
      for(y in as.numeric((i+1):j))  
      {
        sum=sum + r[y]*(y-i)
      }}
    if(c[i]>c[j+1]+k+h*sum){
      c[i]=c[j+1]+k+h*sum
      I[i]=j}
    if(i==j){
      if(c[i]>(c[i+1]+k)){
        c[i]=c[i+1]+k
        I[i]=i}}}
}
Q<-matrix(0,1,n)
stop<-FALSE
while(stop!=TRUE)
{
  a=I[i]
  for(j in i:a)
  {
    Q[i]=Q[i]+r[j]
  }
  if(I[i]==n)
  {
    stop=TRUE
  }
  i=a+1
}





J<-matrix(0,1,n)                      #Heurística
custo<-function(i,j,n){
  sum=k
  if(j>n)
    sum=1000000
  if(j==0)
    sum=1000000
  if(i!=j && j<=n){
    for(y in as.numeric((i+1):j))  
    {
      sum=sum + h*r[y]*(y-i)
    }
  }
  sum=sum/(j-i+1)
  sum
}
stop<-FALSE
i=1
while(stop!=TRUE){
  for(j in 1:n){
    if(custo(i,j-1,n)>=custo(i,j,n) && custo(i,j+1,n)>=custo(i,j,n) && i!=j){
      i=j+1
      J[j]=1
    }
    if(i==n){
      stop=TRUE
      J[n]=1
    }
    if(i>n)
      stop=TRUE
  }
  stop=TRUE
}

