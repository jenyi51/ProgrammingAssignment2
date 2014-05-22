## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# function name: makeCacheMatrix
#  - Takes a matrix object as input and create a cacheMatrix object which support 
#    caching calculated inverse matrix value for retrieval
makeCacheMatrix <- function(x = matrix()) {
	# initialize cache inverse to NULL
	inverse <- NULL
	# set matrix object from x
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	# get matrix object
	get <- function() x
	# set inverse of matrix object
	setInverse <- function(inv) inverse <<- inv
	# get inverse of matrix object
	getInverse <- function() inverse
	
	# return list of set, get, setInverse, getInverse for access
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

# function name: cacheSolve
#  - Takes a cacheMatrix object as input, returns the inverse of the input object
#  - The function first look for existing cached value of the matrix. If not found,
#    it calls solve() to solve for the inverse and cache it.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()	
	
	# if cached inverse matrix is found, return cached value
	if(!is.null(inverse)) {
		message("Getting cached matrix inverse")
		return(inverse)
	}
	
	# if not found, calculate it by calling solve()
	mat <- x$get()
	inverse <- solve(mat, ...)
	# set cache value
	x$setInverse(inverse)
	# return inverse
	inverse
}
