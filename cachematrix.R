

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL  ## variable for storing cached value (the cache)
	
	# function for setting the matrix;
        set <- function(y) {
                x <<- y		##  saves the matrix
                m <<- NULL  ## clear the cache
        }
        get <- function() x  # function for getting the matrix
        setinv <- function(value) m <<- value   #saves value in the cache
        getinv <- function() ##m gets value from the cache
		# return the list of four functions
        list(set = set, 
		 get = get,
             setinverse = setinv,
             getinverse = getinv)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 m <- x$getinverse()  ## takes value from the cache;
		 ## if cache is not then empty return its value and quit this function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		## if cache is not empty ...
        data <- x$get()  ##not get the matrix 
        m <- solve(data, ...) ##calculate its inverse
        x$setinverse(m) ##save solution to the cache 
        m  ## return the result
}
