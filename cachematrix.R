## Functions that cache the inverse of a matrix
## 

## This sets the matrix and an the inverse in an enviroment

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function() inv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## This compute and cache the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  if(!is.null(inv)){
    message("getting cached matrix inverse")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
}
