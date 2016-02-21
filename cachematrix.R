###
#Assignment: Caching the Inverse of a Matrix
###

###
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
###
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x

	#solve function returns the inverse of a matrix

        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

###
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.
###
cacheSolve <- function(x, ...) {
        m <- x$getsolve()

	#if the inverse has been calculated, retrieve from the cache

        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()

	#if the inverse has not been calculated, then calculate it

        m <- solve(data, ...)
        x$solve(m)
        m
}
