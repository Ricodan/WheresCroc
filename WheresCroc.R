library(WheresCroc)

#F_t = F_(t-1) * T * O_t

#the transiion matrix function
transitionMatrix <-function (myPosition, edges){
  
  theMatrix = matrix(nrow=40, ncol = 40)
  for(i in 1:40){
    neighbors = getNeighbors(i,edges)
    
    #index[i,i] will remain with probability 0. Keep in mind for later.
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
  


#READINGS: A vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current location. 
#TOURISTS: A vector giving the positions of the two tourists (elements 1 and 2) and yourself (element 3). 
#If a tourist has just been eaten by Croc that turn, the position will be multiplied by -1. If a tourist was eaten by Croc in a 
#previous turn, then the position will be NA.
#EDGES: is edges
#MATRICES: contains the mean[1] and sd[2] of $salinity $phosphate and $nitrate
myFunction <- function(movesAndMem, readings, tourists, edges, matrices){
  tmatrix = transitionMatrix(1, edges)
 
  
 
  #Step 3: Create Omissions matrix
  
  Fprev = matrix(1/40,ncol = 40)
  
  #dnorm for each of the matrices, salinity and stuff like that
 
  #gotta fix this a little bit
  salinityColumn =  apply(matrices$salinity, 1, function(row) dnorm(readings[1],row[1], row[2]) )
  phosphateColumn = apply(matrices$phosphate,1, function(row) dnorm(readings[2],row[1], row[2]) )
  nitrateColumn =   apply(matrices$nitrogen,  1, function(row) dnorm(readings[3],row[1], row[2]) )
  print(salinityColumn)
  print(phosphateColumn)

  
  
  
  
  
  
  
  
  Fnew = Fprev %*% tmatrix
 
}