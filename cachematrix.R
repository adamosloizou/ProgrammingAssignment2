## Function that allows for caching a matrix and its inverse.
## It takes x as the original matrix to cache.
## It exposes setter and getter functions for getting the
## cached matrix and its inverse as well as setting them.
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <<- NULL
  set <- function(incomingMatrix) {
    x <<- incomingMatrix
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(incomingInverseMatrix) 
    inverseMatrix <<- incomingInverseMatrix
  getinverse <- function() inverseMatrix
  list(
    set = set, 
    get = get, 
    setinverse = setinverse, 
    getinverse = getinverse
    )
}


## Given a cache object via makeCacheMatrix()
## The function returns the inverse matrix if found in the cache
## object, or otherwise calculates and and stores it in the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  cachedInverse <- x$getinverse()
  if (!is.null(cachedInverse)) {
    message("getting cached inverse matrix")
    return(cachedInverse)
  }
  originalMatrix <- x$get()
  inverseMatrix <- solve(originalMatrix, ...)
  x$setinverse(inverseMatrix)
  inverseMatrix
}
