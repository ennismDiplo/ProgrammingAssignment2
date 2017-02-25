## This function creates a special "matrix" object that can be used to cache its inverse.
##The function is really a list containing functions to: 1. set the matrix, 2. get the matrix, 3. set the inverse and 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
		invmtrix = NULL
        set = function(y) { 
                x <<- y
                invmtrix <<- NULL
        }
        get = function() x
        setinvmtrix = function(inverse) invmtrix <<- inverse 
        getinvmtrix = function() invmtrix
        list(set=set, get=get, setinvmtrix=setinvmtrix, getinvmtrix=getinvmtrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		        
        invmtrix = x$getinvmtrix()
        
        # if the inverse has already been calculated
        if (!is.null(invmtrix)){
                # get it from the cache, no need to compute again. 
                message("getting cached data")
                return(invmtrix)
        }
        
        # otherwise, calculates the inverse 
        mat.data = x$get()
        invmtrix = solve(mat.data, ...)
        
        # sets the value of the inverse in the cache via the setinvmtrix function.
        x$setinvmtrix(invmtrix)
        
        return(invmtrix)
}
