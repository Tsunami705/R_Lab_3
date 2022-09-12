dijkstra <-
function(graph,init_node){
  nodes_num=length(unique(graph[,1]))
  dist<-vector(mode="numeric",length=nodes_num)
  prev<-vector(mode="numeric",length=nodes_num)
  for(i in 1:nodes_num){
    dist[i]<-1024
    prev[i]<-NA
  }
  Q<- 1:nodes_num
  dist[init_node]<-0

##############################
  while(length(Q)!=0){
    u<-which.min(dist[Q])
    b<-Q[u]
    Q=Q[-u]
##############################
    
    rest1=which(graph[,1]==b)
    rest2=intersect(graph[rest1,2],Q)
    for(j in rest2){
      a=which(graph[,1]==b&graph[,2]==j)
      alt<-dist[b]+graph[a,3]
      if(alt<dist[j]){
        dist[j]<-alt
        prev[j]<-b
      }
    }
  }
  return(dist)
}
