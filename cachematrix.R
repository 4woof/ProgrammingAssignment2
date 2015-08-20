## This function creates an object that includes a matrix 
## with a cache to store the invererse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setInverse <-function(SI) inv<<-SI
  getInverse <-function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function gets the object created by the first function. 
## If the object's cached inverse is not NULL, the function reports it and doesn't do any computation.
## However, if the cache is empty, the inverse is calculated using "solve()" function and cached into 
## the object in the end

cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setInverse(inv)
  inv
}

