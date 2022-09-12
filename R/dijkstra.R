#' Dijkstra Algorithm
#'
#' `dijkstra()`  takes a graph and an initial node and calculates the shortest path from the
#'initial node to every other node in the graph.
#'
#' @param graph should be a data.frame with three variables (v1, v2 and w) that contains the edges of the graph (from
#'v1 to v2) with the weight of the edge (w). 
#' @param init_node is a numeric scalar thatexist in the graph,which is the starting node.
#' @returns The shortest path to every other node from the starting node as a vector.

#' @seealso https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#' @examples
#' wiki_graph <-
#'data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'           v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'           w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#'dijkstra(wiki_graph, 1)
#' @export
#' 
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
