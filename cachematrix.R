## The function makeCacheMatrix
## Sets the value of the matrix
## Gets the value of the matrix
## Sets the value of the inverse of the matrix
## Gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve calculates the value of the inverse of the matrix.
## the matrix must be invertible: dimensions n*n with det!=0
## det can be verified by running det(matrix)
## if the value has been calculated previously, the function returns the cached value.

## test:
## create a n*n matrix mt <- matrix(sample(100, 25), 5, 5)
## pass mt as argument to makeCacheMatrix mc <- makeCacheMatrix(mt)
## pass mc as argument to cacheSolve s <- cacheSolve(mc)
## executing cacheSolve(mc) for the first time returns the inverse of mc
## executing cacheSolve(mc) for the second time returns a message:
## "getting cached matrix"

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}