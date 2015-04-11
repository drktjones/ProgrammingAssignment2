## makeCacheMatrix creates a special "matrix" object that can cache its 
## inverse. cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache

## The first function in makeCacheMatrix creates the matrix and the second
## function retrieves matrix


makeCacheMatrix <- function(x = matrix()) {
        setinv <- function() {
                y <- matrix((solve(x)), nrow=nrow(x), ncol=ncol(x))
        }  
        get <- function () x
        list(setinv = setinv, get = get)
}

## Function points cached matrix to object and check to see if is null. 
## Function will return the inverse from memory if exists. Otherwise, it will
## calculate the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        y <- x$setinv()
        if(!is.null(y)) {
                message("getting cached data")
                return(y)
        }
        else matrix((solve(x)), nrow=nrow(x), ncol=ncol(x))
}
