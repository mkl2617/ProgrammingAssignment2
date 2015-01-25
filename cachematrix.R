#makeCacheMatrix: This function creates a special "matrix" object that can store the 
#original matrix, and cache its inverse. This assumes that the original matrix is 
#inversible.

makeCacheMatrix <- function (x=matrix()) {
	m <- NULL			             #clears the matrix variable
	get <- function() x		             #allows retrieval of original matrix
	setCacheMatrix <- function(solve) m<<- solve #computes inverse matrix
	getCacheMatrix <- function() m               #stores/caches the inverse matrix
	list( get = get,                             #creates output list
		setCacheMatrix = setCacheMatrix ,
		getCacheMatrix = getCacheMatrix )
}

#cacheSolve: This function computes the inverse of the special "matrix" returned by 
#the makeCacheMatrix function. If the inverse has already been calculated (and the 
#matrix has not changed), and assuming that the original matrix was inversible, then 
#cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
	m <- x$getCacheMatrix ()                     #retrieves the inverse from the cache
	if(!is.null(m)) {		             #checks if cache contains inverse
		return(m)		             #stores the inverse from the cache
	}
	#if inverse matrix NOT available from cache
	data <- x$get()                              #retrieves the original matrix
	m <- solve(data, ...)                        #calculates in inverse matrix of x
	x$setCacheMatrix (m)                         #sets the inverse matrix into setCacheMatrix
	m
}