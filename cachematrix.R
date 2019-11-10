## this R file contains 2 functions written to compute, store, and return the inverse of any square matrix (assuming it is invertible)
## you can test the functions by creating a random matrix, such as by
## x<-matrix(rexp(n^2, rate=.1), ncol=n)
## when n is any integer
## cacheSolve(makeCacheMatrix(x)) %*% x should return an identity matrix
## alternatively store the makeCacheMatrix(x) in y first, then try cacheSolve(y) %*% x

## makeCacheMatrix defines a few functions, converting the input matrix into a list which can store its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve searches the transformed matrix for its inverse, if the inverse already exists, it is retrieved, otherwise it is computed.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
