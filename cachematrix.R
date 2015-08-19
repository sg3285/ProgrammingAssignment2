## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix ather than compute it 
## repeatedly.  These functions will cache the inverse of a matrix. 

## Function "makeCacheMatrix" creates a matrix that can cache its inverse.
## This function will set the value of the matrix, get the value of the 
## matrix, set the value of the inverse of the matrix, and get the value
## of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
	  x <<- y
	  m <<- NULL
      }
	get <- function()x
	setInverse <- function(inverse) m <<- solve(x)
	getInverse <- function() m
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)

}


## This function "cacheSolve" computes the inverse of the matrix returned by 
## "makeCacheMatrix". If the inverse has already been calculated
## (and the matrix not changed), then the "cacheSolve" function should
## get the inverse from the cache.

##This function coomputes the inverse of the  matrix returned by "makeCacheMatrix".

cacheSolve <- function(x, ...) {
      m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	
	m <- solve(x$get())
	x$setInverse(m)
	m
	
}

## Test of functions:
## > x = rbind(c(5, -1/5), c(-1/5, 5))
## >  m = makeCacheMatrix(x)
## > m$get()
##     [,1] [,2]
## [1,]  5.0 -0.2
## [2,] -0.2  5.0
## > cacheSolve(m)
##            [,1]        [,2]
## [1,] 0.200320513 0.008012821
## [2,] 0.008012821 0.200320513
## > cacheSolve(m)
## getting cached data
##             [,1]        [,2]
## [1,] 0.200320513 0.008012821
## [2,] 0.008012821 0.200320513
