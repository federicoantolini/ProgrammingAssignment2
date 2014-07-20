## These functions associate the value of a matrix to the value
## of its inverse within the same object. The inverse matrix is
## therefore cached together with the original matrix, and its value
## can be retrieved without the need to recalculate it after the
## first time.


## The first function takes a matrix as argument and returns a list
## of functions to set and get the value of the matrix and to set
## and get the value of its inverse.

makeCacheMatrix <- function(x = matrix()) {
	  inv <- matrix(NA, nrow(x), ncol(x))
        set <- function(y) {
                x <<- y
                inv <<- matrix(NA, nrow(x), ncol(x))
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## The second function returns the value of the inverse matrix
## if it has been already cached, otherwise it calculates it and
## caches it in the object created with makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.na(inv[1,1])) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setinv(inv)
        inv
}
