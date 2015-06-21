## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## start inverse property
	inv <- NULL

	## Set the Matrix
	set <- function(y) {
		matrix <<- y
		inv <<- NULL
}
	## Method to get the matrix
	get <- function(){matrix}
	
	## set the inverse of the matrix
	setInverse <- function(inverse) {inv <<- inverse}

	## Method to get the inverse of the matrix
	getInverse <- function() {inv}

	## Returns the list of methods
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	## getting the martix that in the inverse
	inv<- x$getInverse()

	##return if the inverrse has already been calculate
	if(!is.null(inv)) {message("getting cached data")
	return(inv)
}
	##if the inverse wasn't calculated
	
	## getting the matrix 
	data <- x$get()

	## calculating the inverse by using matrix multiplication
	m <- solve(data) %*% data

	## storing the inverse to the object
	x$setInverse(m)

	## returing a matrix that is the inverse
	m
}

