## The aim of this assignment is to write a pair of functions that cache the 
## inverse of a matrix, the functions are "makeCacheMatrix" and "cacheSolve"

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      invs <- NULL
      set <- function(y) {
        x <<- y
        invs <<- NULL
      }
      get <- function() x
      setinvs <- function(inverse) invs <<- inverse
      getinvs <- function() invs
      list(set = set, get = get, 
          setinvs = setinvs,
          getinvs = getinvs)
}


## cacheSolve function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix
## has not changed), then the cashSolve should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getinvs()
        if(!is.null(invs)) {
          message("getting cached data")
          return(invs)
        }
        data <- x$get()
        invs <- solve(data, ...)
        x$setinvs(invs)
        invs
}
## ==================  testing the code  ===========================

## mtrx <- matrix(rnorm(9), 3, 3)
## mtrxy <- makeCacheMatrix(mtrx)
## cacheSolve(mtrxy)

##           [,1]       [,2]       [,3]
##   [1,]  0.6764578  0.1176006  0.2037385
##   [2,]  0.2296105  0.3438737 -0.4117841
##   [3,] -0.7582138 -1.3005773 -0.4798369
