## Provide two functions: one, that will take in a matrix and store it in cache and
## second, will take the provided cached matrix and perform the inverse caluclation.

## This function creates a special matrix & caches the inverse of the matrix supplied.
## As stated in the assignment assuming the matrix is always invertable.
makeCacheMatrix <- function(x = matrix()) {
	mat <- NULL
	set <- function(y) {
		x <<- y
		mat <<- NULL
	}
	get <- function() x
	setsolve <- function(solvemat) mat <<- solvemat
	getsolve <- function() mat
	list(set = set, get = get,
		setsolve = setsolve,
		getsolve = getsolve)

}


## This function takes an invertable matrix stores the inverse in cache
## and returns the inverse matrix of the input.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		mat <- x$getsolve()
		if (!is.null(mat)){
			message("getting cached data")
			return(mat)
		}
		data <- x$get()
		mat <- solve(data, ...)
		x$setsolve(mat)
		mat
}
