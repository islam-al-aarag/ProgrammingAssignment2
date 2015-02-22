## This function creates a special object that
## is capable of getting/setting a matrix inverse
## without recomputing it as long as the data
## is the same.

makeCacheMatrix <- function(x = matrix()) {
	cachedInv <- NULL
	set <- function(y) {
		x <<- y
		cachedInv <<- NULL
	}
	get <- function() x
	setInv <- function(inv) cachedInv <<- inv
	getInv <- function() cachedInv
	list(set = set, get = get,
		 setInv = setInv,
		 getInv = getInv)
}

## This function computes the inverse of a matrix
## if the data is new or gets it from the cache if
## computed already.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setInv(inv)
	inv
}