## Together, these two functions compute the inverse of a matrix and cache
## the result to potentially save time in future computations.

## This function creates four functions that help to cache the result

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	set_inverse <- function(inverse) m <<- inverse
	get_inverse <- function() m
	list(set = set, get = get,
		 set_inverse = set_inverse,
		 get_inverse = get_inverse)
}


## This function computes the inverse or returns the cache value if appropriate

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'z'
	m <- z$get_inverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- z$get()
	m <- solve(data, ...)
	z$set_inverse(m)
	m
}
