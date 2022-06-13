# Set of functions for caching inverse of a matrix

# Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # - set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # - get the value of the matrix
  get <- function() x
  # - set the value of the inverse matrix
  setinv <- function(solve) inv <<- solve
  # - get the value of the inverse matrix
  getinv <- function() inv
  
  list(set = set,
       get = get,
       setinv = setinv,
       getinv = getinv)
}


# Calculates the inverse matrix of the object created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message('getting cached data')
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}