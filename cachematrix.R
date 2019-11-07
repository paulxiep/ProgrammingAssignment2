## this R file contains 2 functions written to compute, store, and return the inverse of any square matrix (assuming it is invertible)
## you can test the functions by creating a random matrix to apply the functions to, such as by
## x<-matrix(rexp(n^2, rate=.1), ncol=n)
## when n is any integer

## makeCacheMatrix transforms the square matrix into an array, adding a 3rd dimension (so x by x matrix becomes x by x by 2)

makeCacheMatrix <- function(x = matrix()) {
array(c(x,matrix(0/0,dim(x),dim(x))),dim=c(dim(x),2))
}


## cacheSolve computes the inverse of the matrix tranformed by makeCachematrix, and stores the inverse in [,,2] dimension
## If the inverse already exists in the [,,2] dimension, it simply retrieves the stored value, no computation required.
## output is of the same dimension as the original matrix (before being transformed by makeCacheMatrix)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if(is.nan(x[1,1,2])){
    x<-array(c(x[,,1],solve(x[,,1])),dim=dim(x))
  }
  x[,,2]
}
