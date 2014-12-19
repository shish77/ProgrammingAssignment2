## This is part of Programming Assignment 2: Caching the Inverse of a Matrix.
## Matrix inversion is usually a costly computation.
## So the intent here is to cache and resuse it 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

	
	i <- NULL # variable to cache matrix inverse

	set <- function(y) {
		x <<- y
		i <<- NULL
	}

	get <- function() {
		x
	}

	setInverse <- function(inv) {
		i <<- inv
	}

	getInverse <- function() {
		i
	}

	list(set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)

}


## This function does one of the following:
## - Computes the inverse of the special "matrix" returned by makeCacheMatrix, or
## - If inverse was already calculated, it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
	i <- x$getInverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}