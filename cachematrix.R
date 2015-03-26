## This function invert a square matrix
## This following function creates a special "matrix" object that can cache its inverse.
## There are four functions created here: 1) Set the matrix; 2) Get the matrix; 3) Set the inversed matrix; 4) Get the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inv){
    inverse <<- inv
  }
  
  getInverse <- function() inverse
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## This following function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$get()
  inverse <- x$getInverse()
  if(!is.null(inverse)){
  message("getting cached data - Inverse of the matrix")
    return(inverse)
  }
  inverse <- solve(mat)
  x$setInverse(inverse)
  inverse
}
