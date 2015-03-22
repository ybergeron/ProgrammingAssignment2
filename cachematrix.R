## Caching the Inverse of a Matrix

## "makeCacheMatrix" creates a special "matrix" object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
set <- function(y) {
	x <<- y
	i <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) i <<-solve
	getinverse <- function() i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## "cacheSolve" retrieves from the cache the inverse of the special "matrix" object returned by "makeCacheMatrix" above (message "cached inverse matrix"). If the matrix has changed or the inverse has not been calculated, then it calculates the inverse matrix using the "solve" function and returns it (message "newly calculated inverse matrix")
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
        	message("cached inverse matrix")
        	return(i)
        	}
        	message("newly calculated inverse matrix")
        	data <- x$get()
        	i <- solve(data, ...)
        	x$setinverse(i)
        	i
}