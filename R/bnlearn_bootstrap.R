require(bnlearn)
require(Rgraphviz)

plot_bn_strength <- function(to_analyze){
 # tmp <- boot.strength(to_analyze, R=50000, m=floor(nrow(to_analyze)/2), algorithm = "iamb")
  tmp <- boot.strength(to_analyze, R=10000, m=2000, algorithm = "iamb")
  n_nodes <- ncol(to_analyze)
  DAG_matrix <- matrix(rep(0,n_nodes*n_nodes), n_nodes, n_nodes)
  colnames(DAG_matrix) <- colnames(to_analyze)
  rownames(DAG_matrix) <- colnames(to_analyze)
  e = empty.graph(colnames(to_analyze))
  treshold <- 0.90
  res <- list()
 View(tmp$direction)
  for (i in seq(1,length(tmp$from))){
        DAG_matrix[tmp$from[i], tmp$to[i]] <- (tmp$strength[i] >treshold) * (tmp$direction[i] > 0.3)
        # if bothe directions > 0.3, there is an arraw in both directions, hence undirected 
  }

  amat(e) <- DAG_matrix
  return(list(DAG_matrix = DAG_matrix, strength_bn = tmp, DAG_graph = e))
}

colors_fun <- function(my_num){
  if (my_num == 1)
    return(1)
  if (my_num == 10)
    return(3)
  if (my_num == 11)
    return(5)
 return(1)
  }

plot_weigthed_amat <- function(amat){
  g <- as(amat,"graphNEL")
  nn <- nodes(g)
  p <- numNodes(g)
  n.edges <- numGedges(amat)
  ahs <- ats <- acol <- rep("none", n.edges)
  nms <- character(n.edges)
  fixedsize = rep(FALSE, times=length(nn))
  iE <- 0L
  for (i in seq_len(p-1)) {
    x <- nn[i]
    for (j in (i+1):p) {
      y <- nn[j]
      if ((amat[x,y] != 0)  ) {
        iE <- iE + 1L
        acol[[iE]] <- amat[x,y] 
        nms[[iE]] <- paste0(x,"~",y)
          
        
      }
    #  if ((amat[x,y])>amat[y,x]){
    #    ahs[[iE]] <- "normal"
    #    ats[[iE]] <- "none"
    #  }
    }
  }
  names(ahs) <- names(ats) <- names(acol) <- nms
  edgeRenderInfo(g) <- list(lwd = acol, lty=1, col="transparent") #arrowhead = ahs, arrowtail = ats, lwd=2)
  g
  nodeRenderInfo(g) <- list(shape =c("box"), fixedsize=fixedsize,fontsize = c("20"), col=c("transparent"))
  Rgraphviz::renderGraph(Rgraphviz::layoutGraph(g))

}





plot_amat <- function(amat){
  g <- as(amat,"graphNEL")
  nn <- nodes(g)
  p <- numNodes(g)
  #n.edges <- numEdges(g) -- is too large:
  ## rather count edges such that  "<-->" counts as 1 :
  n.edges <- numGedges(amat)
  ahs <- ats <- acol <- rep("none", n.edges)
  nms <- character(n.edges)
  fixedsize = rep(FALSE, times=length(nn))
  #cmat <- array(c("0" = "none",   "1" = "odot",
  #                "2" = "normal", "3" = "none")[as.character(amat)],
  #              dim = dim(amat), dimnames = dimnames(amat))
 # cmat <- array(c("0" = "none",   "1" = "normal",
 #                                 "10" = "normal", "11" = "normal")[as.character(amat)],
  #                              dim = dim(amat), dimnames = dimnames(amat))
  
  #colmat <- array(c( "1" = "red",
  #                "10" = "blue", "11" = "green")[as.character(amat)],
  #              dim = dim(amat), dimnames = dimnames(amat))
  
  iE <- 0L
  for (i in seq_len(p-1)) {
    x <- nn[i]
    for (j in (i+1):p) {
      y <- nn[j]
      if (amat[x,y] != 0) {
        iE <- iE + 1L
       # ahs[[iE]] <- cmat[x,y]
        #ats[[iE]] <- cmat[y,x]
        acol[[iE]] <- colors_fun(amat[x,y])
        #acol[[iE]] <- colmat[y,x]
        nms[[iE]] <- paste0(x,"~",y)
        #if (amat[x,y]==amat[y,x]-2){
        #  ahs[[iE]] <- "odot"
        #  ats[[iE]] <- "odot"
        #}
        if ((amat[x,y])>amat[y,x]-2){
          ahs[[iE]] <- "normal"
          ats[[iE]] <- "none"
        }
          
      }
    }
  }
  names(ahs) <- names(ats) <- names(acol) <- nms
  edgeRenderInfo(g) <- list(lty = acol, lwd=2) #arrowhead = ahs, arrowtail = ats, lwd=2)
  g
  #print(nodeRenderInfo(g)) <- list(shape ='box')
  nodeRenderInfo(g) <- list(shape =c("box"), fixedsize=fixedsize,fontsize = c("20"), col=c("red"))
  ## XXX Sep/Oct 2010  --- still current -- FIXME ??
  ## XXX undid change by MM, since edge marks didn't work anymore
  ## XXX "known bug in Rgraphviz, but not something they may fix soon"
  ## Rgraphviz::plot(g, main = main, ...)
  Rgraphviz::renderGraph(Rgraphviz::layoutGraph(g))
  #plot(g,attrs=list(node=list( fillcolor="white", 
  #                                 shape = "box",fixedsize=FALSE)))#,
  #                         edge=list(color="black"),   graph=list(rankdir="LR")) )    

}



