## Put comments here that give an overall description of what your
## functions do

## The first function creates a special object, which contains a matrix 
## and stores a cache of the inversed matrix as well.

makeCacheMatrix <- function(x = matrix()) {
	inversed <- NULL
	set <- function(mat) {
		x <<- mat
		inversed <<- NULL
	}
	get <- function() x
	setinversed <- function(inverse) inversed <<- inverse
	getinversed <- function() inversed
	list(set = set, get = get, setinversed = setinversed, getinversed = getinversed)
}


## The second function first checked if there is a cached result.
## If there is one, then return it. 
## If there is none, caculate it first, store it and then return it.

cacheSolve <- function(x, ...) {
        inversed <- x$getinversed()
		if (!is.null(inversed)) {
			print("Cached result queried!")
			return(inversed)
		}
		mat <- x$get()
		inversed <- solve(mat, ...)
		x$setinversed(inversed)
		return(inversed)
}

## The following scripts checks if the the code above works.
mat <- matrix(rnorm(16),4,4)
mat
mat_with_cache <- makeCacheMatrix(mat)
cacheSolve(mat_with_cache)
