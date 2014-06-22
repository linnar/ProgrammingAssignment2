## Pair of functions for saving computation time on matrix inverse.
## makeCacheMatrix encapsulates matrix and caches it's inverse,
## cacheSolve returns inverse of matrix from previous function


## Object storing matrix and it's inverse
## Inverse must be computed by separate function, see cacheSolve.

makeCacheMatrix <- function(stored.matrix = matrix()) {
  inverse.matrix <- NULL
  set <- function(y) {
    stored.matrix <<- y
    inverse.matrix <<- NULL
  }
  get <- function() stored.matrix
  setinverse <- function(inverse) inverse.matrix <<- inverse
  getinverse <- function() inverse.matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return inverse of matrix in makeCacheMatrix object.
## Inverse is read from cache or computed and cached.

cacheSolve <- function(x) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse matrix")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
