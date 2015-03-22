## The following functions create a storage space to contain a matrix and its
## inverse after it has been calculated. This allows the inverse of a matrix to 
## be cached so that it can be recalled multiple times without having to re-
## calculate each time it is called.

## This function creates a storage matrix, sets the value of the matrix and
## creates a space to cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}

## This function first checks for a stored inverse in the cached matrix. If one
## already exists, it is retrieved and returned. If one does not exist in the 
## cache, the inverse of the matrix is calculated, returned, and stored in the
## cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  inverse <- x$get()
  m <- solve(inverse, ...)
  x$setinv(m)
  m
}

## Create matrix
x <- matrix(c(2,4,6,8), 2, 2)

## Cache matrix x
my_matrix <- makeCacheMatrix(x)

## Calculate inverse--none stored in cache so will calculate
cacheSolve(my_matrix)

## Calculate inverse--already stored in cache so will return (m)
cacheSolve(my_matrix)

## end comment
