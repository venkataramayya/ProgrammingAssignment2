## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    get <- function() {
    	## Return the matrix
    	m
    }
    setInverse <- function(inverse) {
        i <<- inverse
    }
    getInverse <- function() {
        ## Return the inverse property
        i
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mat <- x$getInverse()

    ## Just return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    ## Get the matrix from our object
    data <- x$get()
    ## Calculate the inverse using matrix multiplication
    mat <- solve(data) %*% data
    ## Set the inverse to the object
    x$setInverse(m)
    ## Return the matrix
    mat
}
