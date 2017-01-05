

## set(m) - set the matrix m
## get() - return the matrix that was previously stored by set function
## setinv(invmat) - set a matrix (expected to be the inverse of m)
## getinv() - return the matrix that was previously stored by setinv function 


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        inv <<- NULL
        x <<- y
    }
    get <- function() x
    setinv <- function(invmat) inv <<- invmat
    getinv <- function() inv
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat,...)
    x$setinv(inv)
    inv
}
