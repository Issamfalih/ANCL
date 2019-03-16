#' SA-Cluster
#'
#' @description Sa-cluster algorithm as describ in ....
#' @usage community.SACluster(graph,k, c, L, sigma, itermax)
#' @param graph : an attributed igraph graph
#' @param k : Number of clusters
#' @param L : length limit
#' @param c : restart probability
#' @param sigma : paramater of influence function
#' @return A set of cluster
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' graph = Polblogs
#' wt = community.SACluster(graph )
#' @export




community.SACluster <- function(graph,k = 10, c = 0.5, L = 20, sigma = 0.5, itermax = 10){

  if(!is.igraph(graph))
    stop("not an igraph graph")
  if(is.null(vertex_attr(graph)$name))
    stop("graph must have a name vertex attribute ")

  # the structural weight
  w0 = 1

  # Attribute weight vector initialization
  attr <- vector()
  for(attrName in vertex_attr_names(graph)){
    if(attrName=="name" || attrName=="id" ) next
      attr <- c(attr, attrName)
  }
  w <- rep(1,length(attr))
  names(w) <- attr

  # Creating the augmented graph
  AdGraph <- augmentedGraph(graph)

  # Structural Vertices and attribute vertices
  structuralVertices <- V(graph)$name
  attributeVertices <- V(AdGraph)$name[! V(AdGraph)$name %in% V(graph)$name]

  # The adjacency matrix of the augmented graph
  adj <- as.matrix(get.adjacency(AdGraph,names = TRUE,type = "both"))

  # The number of neighbors of each node in the augmented graph
  N <- as.vector(colSums(adj, na.rm = TRUE))
  names(N) <- rownames(adj)

  # Pv = |V|*|V| The transition probability from structural vertex to another one
  Pv <- adj[structuralVertices,structuralVertices]
  for(i in structuralVertices)
   Pv[i,which(Pv[i,]!=0)] = w0/((N[i]*w0) + sum(w))

  # A = |V|*|Va| The transition probability from structural vertex to attribute vertex
  A <- adj[structuralVertices,attributeVertices]
  for(i in rownames(A)){
    for( j in names(A[i,which(A[i,]!=0)]))
      A[i,j] = w[strsplit(j,"_")[[1]][1]]/((N[i]*w0) + sum(w))
  }

  # B = |Va|*|V| The transition probability from  attribute vertex to structural vertex
  B <- adj[attributeVertices,structuralVertices]
  for(i in rownames(B))
    B[i,which(B[i,]!=0)] = 1/N[i]

  # O = |Va|*|Va| The transition probability from  attribute vertex to another one
  O <- matrix(0,nrow = length(attributeVertices),ncol =  length(attributeVertices))

  # The random walk matrix
  Pa = rbind(cbind(Pv,A),cbind(B,O))
  Ra = matrix(0,nrow = vcount(AdGraph), ncol= vcount(AdGraph))
  for(l in seq(L))
    Ra = Ra + (c * ((1-c)^l) * (Pa^l))

  # The density influence function
  f = sapply(structuralVertices, function(i){ sum(1-exp((Ra[i,structuralVertices]^2)/(2 * (sigma^2))))})

  # Centroid initialization
  centers = names(sort(f,decreasing = TRUE)[1:k])

  Converge = FALSE
  iter = 1
  while((iter != itermax)|| Converge){

  cat(".")

    # Clusters attribution
    clusters = vector()
    for(i in structuralVertices){
      clusters = c(clusters, names(sampleOne(which(Ra[i,centers] == max(Ra[i,centers])))))
      #clusters = c(clusters, names(which.max(Ra[i,centers])))
    }
    names(clusters) <- structuralVertices


    #Convert membership to list of partition
    clusters <- memb2groups(clusters)

    # Computing average point centers and update centroids with the most centrally located point in each cluster
    centersUpdating <- vector()
    for( com in clusters){
      if(length(com)==1)
        centersUpdating <-  c(centersUpdating,com)
      else{
        centersUpdating <-  c(centersUpdating,names(which.min(sapply(com, function(i){abs(sum(Ra[i , com] - (rowSums(Ra[com,com])/length(com))))}))))
      }
    }
    names(clusters) = centersUpdating

    #Update attribute  weight
    deltaW = vector()
    for(i in attr){
      ss=0
      for( ctr in centersUpdating)
        ss = ss +length(which(get.vertex.attribute(graph,name=i,index = clusters[[ctr]])==get.vertex.attribute(graph,name=i,index =ctr)))
      deltaW = c(deltaW, ss)
    }
    deltaW = (length(attr) * deltaW)/sum(deltaW)
    w = (w+deltaW)/2

    # Re-computing Ra

          # A = |V|*|Va| The transition probability from structural vertex to attribute vertex
          A <- adj[V(graph)$name, V(AdGraph)$name[! V(AdGraph)$name %in% V(graph)$name]]
          for(i in rownames(A)){
            for( j in names(A[i,which(A[i,]!=0)]))
              A[i,j] = w[strsplit(j,"_")[[1]][1]]/((N[i]*w0) + sum(w))
          }

          # The random walk matrix
          Pa <- rbind(cbind(Pv,A),cbind(B,O))
          for(l in seq(L))
            Ra = Ra + (c * (1-c)^l * Pa^l)

    centers = centersUpdating

    iter = iter +1
  }
  # Clusters attribution
  clusters = vector()
  for(i in structuralVertices){
    #clusters = c(clusters, names(sampleOne(which(Ra[i,centers] == max(Ra[i,centers])))))
    clusters = c(clusters, names(which.max(Ra[i,centers])))
  }
  names(clusters) <- structuralVertices
  return(create.communities(graph = graph, membership = as.numeric(clusters),algorithm="SACluster"))
}

# for(i in seq(0.1,1,0.1)){
# cc = memb2groups(SACluster(DBLP10K,k = 10,c = 0.5,L = 20, sigma = i,itermax = 5))
# print(sapply(cc, length))
# print(density.clusters(DBLP10K,partition = cc))
# print(entropy.attribute(DBLP10K,partition = cc))
# print(mean(entropy.attribute(DBLP10K,partition = cc)))
# }


