## Assignment to Cache the Inverse of a Matrix by Vaibhav C.


## As costly computation as matrix inversion is there is a benefit of caching the inverse of Matrix rather than computing it


## Our first function does the same, it creates cache of the Inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## In the solveTheCache() function we check if the iverse has been already calculated, if so then we retrieve it from out Cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
