## This functions are used for caching the inverse
## of a matrix

## This function creates a matrix and caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m<<- solve
  getinverse <- function() m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## This function prints the inverse of the previous matrix if it has been already calculated,
## if not it computes the inverse of that matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m     ## Return a matrix that is the inverse of 'x'
}
