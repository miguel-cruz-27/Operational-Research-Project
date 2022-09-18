library(readxl)   #separamos os dados em 2 ficheiros: um com o mapa de povoamentos e o outro com as espécies por povoamento (em que retiramos o texto povoamentos espécies e original para facilitar a análise)
library(tidyverse)
especies<-read_excel("project/IO/Especies.xlsx")  #ler as espécies por povoamento no excel
mapa<-read_excel("project/IO/Mapa.xlsx")  #ler o mapa de povoamentos no excel
mapa[is.na(mapa)]<-0 #preencher os espaços que faltam com 0
vizinhanca<-function(x,y) #dá uma matriz em que 1 corresponde a vizinho e 0 corresponde a não vizinho (em todas as vizinhanças considera-se que ele próprio é vizinho dele próprio)
{
  P<-matrix(0,nrow=nrow(mapa),ncol=ncol(mapa))
  l<-nrow(mapa)
  c<-ncol(mapa)
  P[x,y]=1
  if(y-1>0){
    if(mapa[x,y-1]!=0)
      P[x,y-1]=1
    if(x-1>0){
      if(mapa[x-1,y-1]!=0)
        P[x-1,y-1]=1;}}
  if(x-1>0){
    if(mapa[x-1,y]!=0)
      P[x-1,y]=1
    if(y+1<=c){
      if(mapa[x-1,y+1]!=0)
        P[x-1,y+1]=1}}
  if(x+1<=l){
    if(mapa[x+1,y]!=0)
      P[x+1,y]=1
    if(y+1<=c){
      if(mapa[x+1,y+1]!=0)
      P[x+1,y+1]=1;}}
  if(x+1<=l){
    if(y-1>0){
      if(mapa[x+1,y-1]!=0)
        P[x+1,y-1]=1}}
  if(y+1<=c){
    if(mapa[x,y+1]!=0)
      P[x,y+1]=1}
  P
}
maximo<-function(especies)  #povoamento com maior número de espécies
{
  max<-0          
  for(i in 1:nrow(especies)){
    if(sum(especies[i, ])>max)
      max=i}
  max
}
especies_p<-function(x){  #encontra as espécias do povoamento
  A<-c(0)
  for(i in 1:ncol(especies))
  {
    if(especies[x,i]==1)
        A<-append(A,i)
  }
  A<-A[2:length(A)]
  A
}
vizinhos<-function(x,y){  #indica os vizinhos (por ordem numérica) a partir dos índices de coluna e linha de um determinado povoamento
  S<-c(0)
  for(n in 1:sum(vizinhanca(x,y)==1)){
    S<-append(S,mapa[which(vizinhanca(x,y)==1,arr.ind=TRUE)[n,1],which(vizinhanca(x,y)==1,arr.ind=TRUE)[n,2]])}
  S<-S[2:length(S)]
  S<-unlist(S)
  S<-sort(S)
  S
}
P<- 1:nrow(especies) #lista de povoamentos            #Passo 1
L<-1:ncol(especies) #lista de espécies
max<-maximo(especies)                                 #Passo 2
stop<-(2==3)
help<-length(L)
ind<-which(mapa==max,arr.ind=TRUE)
for(i in especies_p(max)){    #Passo 3
  for(j in vizinhos(ind[1],ind[2])[!vizinhos(ind[1],ind[2]) %in% as.numeric(mapa[ind[1],ind[2]])]) 
  {
    if(especies[j,i]==1){
      L=L[!L %in% i]
      P=P[!P %in% j]
      break}
  }}
if(length(L)<help){
  P=P[!P %in% max]}
if(length(L)==0)
  stop<-2==2
solucaotemp<-setdiff(1:nrow(especies),P)
for(j in L){               #Passo 4
  for(i in solucaotemp[!solucaotemp %in% max]){
    indu<-which(mapa==i,arr.ind=TRUE)
    if(especies[i,j]==1){
      for(z in vizinhos(indu[1],indu[2])[!vizinhos(indu[1],indu[2]) %in% as.numeric(mapa[indu[1],indu[2]])]){
        if(especies[z,j]==1){
          L=L[!L %in% j]
          P=P[!P %in% z]
          solucaotemp<-append(solucaotemp,z)
        }
      }
    }
  }
}
ex<-(2==3)
while(stop==FALSE){           #Passo 5
  for(j in L){
    for(i in 1:nrow(especies)){
      ind<-which(mapa==i,arr.ind=TRUE)
      if(especies[i,j]==1){
        for(z in vizinhos(ind[1],ind[2])[!vizinhos(ind[1],ind[2]) %in% as.numeric(mapa[ind[1],ind[2]])]){
          if(especies[z,j]==1){
            P=P[!P %in% z]
            P=P[!P %in% i]
            L=L[!L %in% j]
            if(length(L)==0)
              stop<-(2==2)
            ex<-(2==2)
            break
          }
        }
        if(ex==TRUE)
          break
      }
    }
  }}
solucao<-setdiff(1:nrow(especies),P)
solucao           #Passo 1
sair<-0
for(i in solucao){      #Passo 2    
  S=solucao[!solucao %in% i]
  for(j in 1:ncol(especies)){
      aux<-FALSE
      for(k in S)                                     
      {
        if(especies[k,j]==1){                           
          ind<-which(mapa==k,arr.ind=TRUE)
          for(z in vizinhos(ind[1],ind[2])[(vizinhos(ind[1],ind[2]) %in% S) & (!(vizinhos(ind[1],ind[2]) %in% as.numeric(mapa[ind[1],ind[2]])))]){
            if(especies[z,j]==1)
            {
              aux<-TRUE
              sair<-sair+1
              break
            }
          }
          if(aux==TRUE)                            
            break
        }
      }
      if(aux==FALSE){
        break
        sair=0
      }
  }
  if(sair==7)
    solucao<-solucao[!solucao %in% i]
  sair=0
  S=solucao
}
solucao
M<-matrix(0,nrow=nrow(especies),ncol=ncol(especies))    #Passo 3
solucao2<-solucao
for(i in solucao2){
  ind<-which(mapa==i,arr.ind=TRUE)
  for(z in vizinhos(ind[1],ind[2])[(vizinhos(ind[1],ind[2]) %in% solucao2) & (!(vizinhos(ind[1],ind[2]) %in% as.numeric(mapa[ind[1],ind[2]])))]){
    for(j in 1:ncol(especies)){
      if(especies[i,j]==1 & especies[z,j]==1){
        M[i,j]=z
      }
    }
  }
}
solucaoini<-solucao2
P<-1:nrow(especies)
for(i in solucaoini){
  for(z in P[!P %in% solucaoini]){
    trocar<-0
    for(j in 1:ncol(especies)){
      if(M[i,j]>0){
        inde<-which(mapa==M[i,j],arr.ind=TRUE)
        if(sum(vizinhos(inde[1],inde[2])==z)==1 & especies[z,j]==1)
          trocar<-trocar + 1
      }}
    if((sum(M[i, ]>0))==trocar){
      solucao2<-solucao2[!solucao %in% i]
      solucao2<-append(solucao2,z)
      solucaoini<-solucaoini[!solucaoini %in% i]
    }
  }
}
solucao2
if(!all(solucao2!=solucao)){
  solucao<-solucao2
  sair<-0
  for(i in solucao){           
    S=solucao[!solucao %in% i]
    for(j in 1:ncol(especies)){
      aux<-FALSE
      for(k in S)                                     
      {
        if(especies[k,j]==1){                           
          ind<-which(mapa==k,arr.ind=TRUE)
          for(z in vizinhos(ind[1],ind[2])[(vizinhos(ind[1],ind[2]) %in% S) & (!(vizinhos(ind[1],ind[2]) %in% as.numeric(mapa[ind[1],ind[2]])))]){
            if(especies[z,j]==1)
            {
              aux<-TRUE
              sair<-sair+1
              break
            }
          }
          if(aux==TRUE)                            
            break
        }
      }
      if(aux==FALSE){
        break
        sair=0
      }
    }
    if(sair==7)
      solucao<-solucao[!solucao %in% i]
    sair=0
    S=solucao
  }}
solucao