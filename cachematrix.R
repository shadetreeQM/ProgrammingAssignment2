## Put comments here that give an overall description of what your
## functions do

#Pass an invertible matrix, x, to makeCacheMatrix, which gets assigned to z
#use cacheSolve on z for further computations


## Write a short comment describing this function
#Create a list that will store the inverse of a matrix for future computations
#Acts a bit like an object in Java
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(solve) inv <<- solve
	getinv <- function() inv
	list(set = set, get = get,
			setinv = setinv,
			getinv = getinv)
}


## Write a short comment describing this function
#If an inverse has been cached, use it, otherwise compute the inverse and cache it for future use.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$getinv()
		if(!is.null(inv)){
			return(inv)
		}
		data <- x$get()
		inv <- solve(data, ...)
		x$setinv(inv)
		inv
}