##it's a pair of functions that cache the inverse of a matrix.


##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix  <- function(matx= matrix()) {
  invers<- NULL
  set <- function(t){
  matx <<- t
  invers<<- NULL
  }
  get <- function()matx
  setInverse <- function(inverse) invers <<- inverse
  getInverse <- function() invers
  
list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been
##computed (and the matrix has not changed), then the cachesolve 
##function must retrieve the inverse of the cache.

cacheSolve <- function(matx, ...) {
## Return a matrix that is the inverse of 'matx'
  invers <- matx$getInverse()
  if(!is.null(invers)){
  message("getting cached data")
  return(invers)
  }
  mat <- matx$get()
  invers <- solve(mat,...)
  matx$setInverse(invers)
  invers
}
