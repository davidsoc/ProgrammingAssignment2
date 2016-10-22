## cacheSolve returns a cached version of the inverted matrix (x) if it exists in the cache
## if the inverted matrix does not exist, it calls makeCacheMatrix to invert the matrix and 
## store it in the cache

## Caches the inverse of the matrix specified

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m

	list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## Returns the inverse of the matrix, returns inverse from cache if it already exists

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	m <- x$getmatrix()
	
	# if the matrix is not null return the cached data
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}

	message("creating inverse matrix")
	# matrix does not exist, create inverse and store it in the cache
	data <- x$get()
	m <- solve(data, ...)
	x$setmatrix(m)
	m
}
