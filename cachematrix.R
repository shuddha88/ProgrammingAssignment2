## Create functions to cache the inverse of a matrix

## The function makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to

## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## The function cacheSolve calculates the inverse of a matrix
## created from the function makeCacheMatrix such that

## if the inverse has already been calculated,
## it gets the inverse from the cache and skips the computation.
## Otherwise it calculated the inverse.


cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if (!is.null(i)) {
                message("Getting cached data")
                return(i)
        }

        else {  
                message("Not in cache. Calculating inverse")
                m <- x$get()
                i <- solve(m, ...)
                x$setinv(i)
                return(i)
        }
}
 
  
 
