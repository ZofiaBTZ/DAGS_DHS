
plot_bn_strength <- function(to_analyze, output_file){
  tmp <- boot.strength(to_analyze, R=100, m=floor(nrow(to_analyze)/2), algorithm = "iamb")
  n_nodes <- ncol(to_analyze)
  output <- matrix(rep(0,n_nodes*n_nodes), n_nodes, n_nodes)
  colnames(output) <- colnames(to_analyze)
  rownames(output) <- colnames(to_analyze)
  e = empty.graph(colnames(to_analyze))
  treshold <- 0.9
  res <- list()

  for (i in seq(1,length(tmp$from))){
        output[tmp$from[i], tmp$to[i]] <- (tmp$strength[i] >treshold) * (tmp$direction[i] > 0.2)
        # if bothe directions > 0.3, there is an arraw in both directions, hence undirected 
  }

  amat(e) <- output
  png(paste(output_file, '.jpg', sep=""))
  graphviz.plot(e, shape="rectangle")
  dev.off()
  return(output)
}

colors_fun <- function(my_num){
  if (my_num == 1)
    return(3)
  if (my_num == 10)
    return(5)
  if (my_num == 11)
    return(1)
 return(2)
  }

plot_amat <- function(amat){
#  amat <- fci_output@amat
  #amat <- fci_output
 # amat <- f
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
      }
    }
  }
  names(ahs) <- names(ats) <- names(acol) <- nms
  edgeRenderInfo(g) <- list(lty = acol, lwd=2)
  g
  #print(nodeRenderInfo(g)) <- list(shape ='box')
  nodeRenderInfo(g) <- list(shape =c("box"), fixedsize=fixedsize,fontsize = c("28"), col=c("red"))
  ## XXX Sep/Oct 2010  --- still current -- FIXME ??
  ## XXX undid change by MM, since edge marks didn't work anymore
  ## XXX "known bug in Rgraphviz, but not something they may fix soon"
  ## Rgraphviz::plot(g, main = main, ...)
  Rgraphviz::renderGraph(Rgraphviz::layoutGraph(g))
  #plot(g,attrs=list(node=list( fillcolor="white", 
  #                                 shape = "box",fixedsize=FALSE)))#,
  #                         edge=list(color="black"),   graph=list(rankdir="LR")) )    
}



source("data_rfci_DHS2010_m_all_DHS_compiler.R")
output_file1 <- "./plots/bn_099_DHS2010_m"
m20101 <- to_analyze
m_output1 <- plot_bn_strength(to_analyze, output_file)

source("data_rfci_DHS2010_f_all_DHS_compiler.R")
output_file1 <- "./plots/bn_099_DHS2010_f"
f_output1 <- plot_bn_strength(to_analyze, output_file)
f20101 <- to_analyze

source("data_rfci_DHS2000_m_all_DHS_compiler.R")
output_file1 <- "./plots/bn_099_DHS2000_m"
m2000_output1 <- plot_bn_strength(to_analyze, output_file)
m20001 <- to_analyze

source("data_rfci_DHS2000_f_all_DHS_compiler.R")
output_file1 <- "./plots/bn_099_DHS2000_f"
f20001 <- to_analyze
f2000_output1 <- plot_bn_strength(to_analyze, output_file)



gg2000 <- 10*m2000_output1 + f2000_output1
plot_amat(gg2000)

gg2010 <- 10*m_output1 + f_output1
plot_amat(gg2010)