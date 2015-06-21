## These functions are to be used with each other. When calculating the inverse of a matrix, it is sometimes
## computationally expensive. So makeCacheMatrix will create a list that has functions allowing you to cache the solution
## of the inverse of a matrix. cacheSolve, will use that cache to solve the inverse of the matrix. Example usage:
## myMatrix <- makeCacheMatrix(cbind(c(2,2), c(3,2)))
## cacheSolve(myMatrix) ## calculates the inverse
## cacheSolve(myMatrix) ## uses the cache

## This function will make a list. The list contains functions for setting the matrix, getting the matrix back
## setting the inverse of the matrix and getting the inverse of the matrix back.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Check the cache for the inverse of the matrix. Return the cached value if it exists, otherwise calculate the inverse
## And set it on the cache
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
        	message("Getting cached inverse")
        	return (i)
        }
        
        matrix <- x$get()
        i <- solve(matrix)
        x$setinverse(i)
        i
}
