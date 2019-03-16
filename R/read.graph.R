



read_attributedGraph <- function(fileName ="../../testGraph.graph"){

    file = readLines(fileName)

    # Read Nodes
    start = which(file=="# Vertices") + 1
    end = which(file=="# Edges") - 2
    nodes = do.call(rbind,lapply(file[start:end],function(x){as.numeric(unlist(strsplit(x,split = ";")))}))

    #Read edges
    start = which(file=="# Edges") + 1
    end = length(file)
    edges = as.vector(unlist(lapply(file[start:end],function(x){as.integer(unlist(strsplit(x,split = ";")))})))


    edges = edges + 1

    graph = make_empty_graph(n= nrow(nodes),directed = FALSE)%>%
      #set_vertex_attr("name",value = nodes[,1])%>%
      set_vertex_attr("attr_1",value = nodes[,2])%>%
      set_vertex_attr("attr_2",value = nodes[,3])%>%
      set_vertex_attr("id",value = nodes[,4])%>%
      add_edges(edges)


    # #create the graph and add attribute
    # graph = graph.empty(n=nrow(nodes),directed = FALSE)
    # for(attr in seq(ncol(nodes))){
    #   if(attr==1)
    #     graph = set.vertex.attribute(graph,name ="name",index = as.vector(V(graph)), value = as.character(nodes[,attr]))
    #   else if(attr==ncol(nodes))
    #       graph = set.vertex.attribute(graph,name = "label",index = as.character(nodes[,attr]),value = nodes[,ncol(nodes)])
    #   else
    #     graph = set.vertex.attribute(graph,name = paste0("attr_",attr-1),index =as.character(nodes[,attr]),value = nodes[,attr])
    # }

    graph = set.vertex.attribute(graph,name ="name",index = as.vector(V(graph)),value = as.character(as.vector(V(graph))))
    return(simplify(graph, remove.multiple = TRUE, remove.loops = TRUE))
}












