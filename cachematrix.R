## The two functions will firstly establish if an inverse of matrix (x) exists
## If not, the inverse will be calculated and returned (via cacheSolve) and
## then stored in the variable (in)
## If the matrix inverse already exists, then that will be returned saving the
## system from needing to do the calculation

## makeCacheMatrix will:
## 	set the value of the matrix
## 	get the value of the matrix
##	set the value of the inverse
##	get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    in <- NULL
	  ## set the matrix
    set <- function(y) {
            x <<- y
            in <<- NULL
    }
	  ## get the matrix
    get <- function() x
	  ## set a solved matrix inverse for future retrieval
    setinverse <- function(solve) in <<- solve
	  ## get/retrieve the matrix inverse
    getinverse <- function() in 
	  ## return a list of all the functions
    list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function returns the inverse of a special matrix (invertible)
## This is either calculated or retrieved from the global variable (in)

cacheSolve <- function(x, ...) {
    ## lookup the inverse
	  in <- x$getinverse()
    ## if it exists, return the cached value
	  if(!is.null(in)) {
            message("getting cached data")
            return(in)
    }
	  ## otherwise get the matrix
    data <- x$get()
	  ## calculate the inverse
    in <- solve(data, ...)
	  ## set the result for later use (cache it)
    x$setinverse(in)
	  ## return the result
    in
}
