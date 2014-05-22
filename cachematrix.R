## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse.
	makeCacheMatrix <- function(x = matrix()) {
	## if an object is called without a method then set them to NULL
	## the superassignment operator <<- allows the change/writing to a variable just one level up in the environment hierarchy	
		m <- NULL
		## set change the matrix x outsite its function (up 1 level) with  superassignment operator <<- 
		set <- function(y) {
			x <<- y
			m <<- NULL
		}
		## get returns the matrix x
		get <- function() x
		## setinverse creates an inverse matrix m via the solve function and assigns that m back one level up (by the use of <<- operator)
		## note: only square matrix can be inversed
		setinverse <- function(solve) m <<- solve(x)
		## getinverse returns the inverse matrix m
		getinverse <- function() m
		## here are the returns
		list(set = set, get = get,
			setinverse = setinverse, 
			getinverse = getinverse)
	}


## Write a short comment describing this function

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

	cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		## calls the getinverse() method for the matrix x
		m <- x$getinverse()
		## check if m is not null, meaning it has cache then returns the cache	
		if(!is.null(m)) {
			message("getting cached data")
			return(m)
		 }
		## these codes below for the one that does not have cache
		data <- x$get()
		m <- solve(data, ...)
		x$setinverse(m)
		m
	}
