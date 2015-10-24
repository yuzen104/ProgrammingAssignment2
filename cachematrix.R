## These functions calculate inverse of a sqaure matrix and cache the inverse


## makeCacheMatrix: creates a special "matrix" object that can 
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     mt  <- NULL
     set <- function(y) {
          x <<- y
          mt <<- NULL
     }
     get <- function() x
     setInverse <- function(inverse) mt <<- inverse
     getInverse <- function() mt
     
     list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve: computes the inverse of the special "matrix" returned by 
## makeCacheMatrix() function. If the inverse has already been calculated
## and the matrix has not changed, then the cacheSolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     mt <- x$getInverse()
     if(!is.null(mt)) {
          message("...getting cached data...")
          return(mt)
     }
     data <- x$get()
     mt <- solve(data,...)
     x$setInverse(mt)
     mt
}


# (e.g.) creating 3x3 matrix with random integer between 1 and 10
mat <- matrix(sample(1:10,9),3,3)

# and calling makeCacheMatrix() and then cacheSolve()
a <- makeCacheMatrix(mat)
cacheSolve(a)
