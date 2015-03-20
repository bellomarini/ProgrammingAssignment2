## The file contains two functions
## makeCacheMatrix: returns a list with functions to cache a matrix and its inverse
## cacheSolve: calculates the inverse of a given matrix using, if possible, the cache

## The following function creates a special list containing functions to: 
## set a matrix, get the matrix, set the inverse, get the inverse
makeCacheMatrix <- function(x = matrix()) {
        yInverse <- NULL # at the beginning no inverse is calculated
        set <- function(y) {
                x <<- y # stores the matrix
                yInverse <<- NULL # resets the inverse
        }
        get <- function() x # returns the matrix
        setInverse <- function(inverse) yInverse<<-inverse # sets the inverse
        getInverse <- function() yInverse # returns the inverse
        # returns the list of functions
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## The following function calculates the inverse of the special
## matrix created with the function above, using
## the cached version, if possible.
cacheSolve <- function(x, ...) {
        # tries to retrieve the inverse
        xInverse <- x$getInverse()
        # if there is a cached version (which is absent if the matrix has changed)
        if(!is.null(xInverse)) {
                message("getting cached data")
                # returns it
                return(xInverse)
        }
        # else it retrieves the matrix
        m <- x$get()
        # calculates the inverse
        mInverse <- solve(m, ...)
        # stores it into the cache
        x$setInverse(mInverse)
        # returns that inverse
        mInverse
}
