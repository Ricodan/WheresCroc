library(WheresCroc)

#F_t = F_(t-1) * T * O_t

#the transiion matrix function
transitionMatrix <-function (myPosition, edges){
  
  theMatrix = matrix(nrow=40, ncol = 40)
  for(i in 1:40){
    neighbors = getNeighbors(myPosition,edges)
    theMatrix[i,] = 0
    #no need fpr loops 
    #divide by the amount of neighbors plus itself since there's a possibility of no movement
    theMatrix[i,neighbors] = 1/(length(neighbors)+1)
    
  }
  return(theMatrix)
}

#traverse the edges matrix and get the nodes and all the edges that go from that node.
getNeighbors <- function(position, edges) {
     c(edges[which(edges[,1]==position),2],edges[which(edges[,2]==position),1])
}
  
  
  


myFunction <- function(movesAndMem, readings, tourists, edges, matrices){
  print(matrices)
  
  
  #Step 2: Create Fprev
  #Step 3: Create Omissions
  
  Fnew = matrix(0,ncol = 40)
  
  
}