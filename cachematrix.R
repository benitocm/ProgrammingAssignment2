## Provide methods to create a matrix and calculate and
## cache the inverse of the matrix.
##


## 
## Create a "matrix" whose inverse will be cached.
##
##  e.g.    m <- makeCacheMatrix(matrix(1:4,2))

makeCacheMatrix <- function(x = matrix()) {
     cached <- NULL
     setMatrix <- function(y) {
          x <<- y
          theinverse <<- NULL
     }
     getMatrix <- function() x
     setInverse <- function(inverse) cached <<- inverse
     getInverse <- function() cached
     list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}

##
## Calculates the inverse of a matrix created with the previous method 
## makeCacheMatrix and caches the result. Returns the inverse of the matrix.
##
##  e.g.  inv <- cacheSolve(m)  

cacheSolve <- function(x, ...) {
        
     m <- x$getInverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     matrix <- x$getMatrix()
     inverse <- solve(matrix, ...)
     x$setInverse(inverse)
     inverse    
}
 