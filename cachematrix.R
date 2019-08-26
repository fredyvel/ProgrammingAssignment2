## Put comments here that give an overall description of what your
## functions do

## a function saved the function with a matrix in cache

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInvMatrix <- function(invMatrix) inv<<- invMatrix
  getInvMatrix <- function() inv
  list(set = set, get = get,
       setInvMatrix = setInvMatrix,
       getInvMatrix = getInvMatrix)
}

## that function return the inverse of matrix, if the inverse have not calculated, that will be calculate

cacheSolve <- function(x, ...) {
  inv <- x$getInvMatrix()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInvMatrix(inv)
  inv
}