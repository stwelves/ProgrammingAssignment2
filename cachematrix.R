## These functions allow a matrix to have its inverse cached and retrieved

## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	m_invmat <- NULL
	set <- function(y = matrix()) {
		x <<- y
		m_invmat <<- NULL
	}
	get <- function() x
	setinvmat <- function(solve) m_invmat <<- solve
	getinvmat <- function() m_invmat
	list(set = set, get = get, setinvmat = setinvmat, getinvmat = getinvmat)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  invmat <- x$getinvmat()
	  if (!is.null(invmat)) {
		message("getting cached inverse matrix")
		return(invmat)
	  }
	  mat <- x$get()
	  invmat <- solve(mat)
	  x$setinvmat(invmat)
	  invmat
}
