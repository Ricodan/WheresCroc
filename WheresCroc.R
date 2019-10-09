library(WheresCroc)

#F_t = F_(t-1) * T * O_t

#######################################################################################################################
#READINGS: A vector giving the salinity, phosphate and nitrogen reading from Croc sensors at his current location. 
#TOURISTS: A vector giving the positions of the two tourists (elements 1 and 2) and yourself (element 3). 
#If a tourist has just been eaten by Croc that turn, the position will be multiplied by -1. If a tourist was eaten by Croc in a 
#previous turn, then the position will be NA.
#EDGES: is edges
#MATRICES: contains the mean[1] and sd[2] of $salinity $phosphate and $nitrate
makeMoves <- function(movesAndMem, readings, touristsAndMe, edges, matrices){
  rangerNode = touristsAndMe[3]
  
  if(movesAndMem$mem$status == 0 | movesAndMem$mem$status == 1 ){
    tmatrix <-transitionMatrix(1, edges)
    movesAndMem$mem$transMatrix <-tmatrix
    Fprev <-matrix(1/39,ncol = 40)
    Fprev[rangerNode] = 0
  }else{
    tmatrix <- movesAndMem$mem$transMatrix
    Fprev<- movesAndMem$mem$Fprev
  }

  if(any(na.omit(touristsAndMe[1:2]) < 0 )){
    negNode = touristsAndMe[which(touristsAndMe[1:2]<0)]
    deathHole = negNode * -1
    Fprev <- matrix(0,ncol = 40)
    Fprev[deathHole]=1
    #cat("Tourist got eaten at: ", deathHole,"\n")
    
  }else{
    #If tourists not eaten then put those locations to 0
    if( !is.na(touristsAndMe[1]) ){
      Fprev[touristsAndMe[1]] = 0
      Fprev <- Fprev/sum(Fprev)
    }
    if( !is.na(touristsAndMe[2]) ){
      Fprev[touristsAndMe[2]] = 0
      Fprev = Fprev/sum(Fprev)
    }
  }
 
  salinityColumn =  apply(matrices$salinity, 1, function(row) dnorm(readings[1],row[1], row[2]) )
  phosphateColumn = apply(matrices$phosphate, 1, function(row) dnorm(readings[2],row[1], row[2]) )
  nitrogenColumn =   apply(matrices$nitrogen,  1, function(row) dnorm(readings[3],row[1], row[2]) )
  
  probabilityColumn = cbind(salinityColumn,phosphateColumn,nitrogenColumn)
  probabilityColumn = apply(probabilityColumn, 1, function(x) prod(x[1],x[2],x[3]))
  Omatrix = diag(probabilityColumn)
  

  Fnew = Fprev %*% tmatrix %*% Omatrix
  Fnew = Fnew/sum(Fnew)

  probCrocLocation = which.max(Fnew)

  routeToCroc=shortestPath(touristsAndMe[3], probCrocLocation, edges)

  nextStep1 = routeToCroc[2]
  nextStep2 = routeToCroc[3]

  if(probCrocLocation == rangerNode){
    #print("Same waterhole as crocodile, check here")
    movesAndMem$moves <-c(rangerNode,0)
    Fnew[probCrocLocation] = 0
    Fnew = Fnew/sum(Fnew)
 
   }else if(nextStep1 == probCrocLocation){
    movesAndMem$moves <-c(nextStep1,0)
    Fnew[nextStep1] = 0
    Fnew = Fnew/sum(Fnew)
    #cat("Move to ", nextStep1, " and check waterhole. \n")
    
  }else{
    movesAndMem$moves <-c(nextStep1,nextStep2)
    #cat("Move 2 steps: ", movesAndMem$moves, "\n")
  }
  
  if(movesAndMem$mem$status == 0 | movesAndMem$mem$status ==1){
     movesAndMem$mem$status = 2;  
  }
  movesAndMem$mem$Fprev <- Fnew
  movesAndMem$mem$prevCrocLoc <- probCrocLocation

  return(movesAndMem)
}

isAdjacent <- function(current, old, edges){
  
  print(any(which((edges[,1] == current) & (edges[,2] == old))))
  print(any(which((edges[,1] == old) & (edges[,2] == current))))
}

#the transiion matrix function
transitionMatrix <-function (myPosition, edges){
  
  theMatrix = matrix(nrow=40, ncol = 40)
  for(i in 1:40){
    neighbors = getNeighbors(i,edges)
    
    theMatrix[i,] = 0
    #no need for loops 
    #divide by the amount of neighbors plus itself since there's a possibility of no movement
    theMatrix[i,c(neighbors,i)] = 1/(length(neighbors)+1)
    
  }
  return(theMatrix)
}

#traverse the edges matrix and get the nodes and all the edges that go from that node.
getNeighbors <- function(position, edges) {
  c(edges[which(edges[,1]==position),2],edges[which(edges[,2]==position),1])
}

appendSorted = function (newNode,frontier){
  # We add the new node to the frontier, sorted with respect to the total cost of the path
  length = length(frontier);
  
  if(length == 0)
  {
    frontier[1] = list(newNode);
  }
  else{
    
    for(i in 1:length){
      if(frontier[[i]]$pos == newNode$pos ){
        if(frontier[[i]]$cost   >= newNode$cost ){
          frontier = frontier[-i];
          length = length -1;
          break
        }else{
          return(frontier)
        }
      }
    }
    for (i in 1:length){
      if(frontier[[i]]$cost  >= newNode$cost){
        frontier = append(frontier,list(newNode), i-1);
        return(frontier)
      }
    }
    frontier[length+1] = list(newNode);
  }
  return (frontier)
}

shortestPath = function(start,goal,edges){
  start_node = list(position=start, cost=0, path=start);
  frontier = list(start_node)
  expanded = NA
  flag = 1;
  counter = 1
  while (flag) {
    expanded = frontier[[1]]
    frontier = frontier[-1]
    if(expanded$position == goal ){
      flag = 0
      return(expanded$path)
      
    }
    for (i in 1:length(edges[,1])) {
      if(edges[i,1] == expanded$pos){
        newNode = list(position= edges[i,2], cost=expanded$cost +1, path=expanded$path)
        newNode$path[length(newNode$path)+1] = edges[i,2]
        frontier = appendSorted(newNode,frontier)
      }
      else if(edges[i,2] == expanded$position){
        newNode = list(position= edges[i,1], cost=expanded$cost +1, path=expanded$path)
        newNode$path[length(newNode$path)+1] = edges[i,1]
        frontier = appendSorted(newNode,frontier)
      }
    }
  }
  
}
