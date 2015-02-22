## A set of functions to save computation time when
## working with inverses of matrices, by caching
## inverse values and checking if they are available before
## going through with calculating the inverse of a matrix

## creates a list of functions to set and get 
## the values of the matrix and of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    set <- function(y) {
        x <<- y
        inv_mat <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv_mat <<- inverse
    getinverse <- function() inv_mat
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## gets the inverse of the matrix by, preferentially, getting the existing
## value if it's already been calculated, but if not available, goes through
## with standard inverse calculation

cacheSolve <- function(x, ...) {
    inv_mat <- x$getinverse()
    if(!is.null(inv_mat)) {
        message("getting cached data.")
        return(inv_mat)
    }
    data <- x$get()
    inv_mat <- solve(data)
    x$setinverse(inv_mat)
    inv_mat
}
