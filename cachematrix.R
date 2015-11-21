## These two functions augment the functionanalilty of the built-in matrix
## datatype. makeCacheMatrix creates the new object and stores: the 
## inverse (once calculated for the first time) and functions
## that operate on the matrix and its inverse.
## 
## Note that this object only works for invertible matrices.

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse.matrix <- NULL
    
    # The SetMatrix function would be used to change the base matrix after you've already
    # created the initial CacheMatrix object.
    SetMatrix <- function(y) { x <<- y                                        
                               inverse.matrix <<- NULL
                              }
    GetMatrix <- function() x
    SetInverseMatrix <- function(inv) inverse.matrix <<- inv # The inverse is saved in the "inverse.matrix" variable
                                                             # of the parent environment (makeCacheMatrix)                                                          
    GetInverseMatrix <- function() inverse.matrix
    list(SetMatrix = SetMatrix, GetMatrix = GetMatrix,
            SetInverseMatrix = SetInverseMatrix,
            GetInverseMatrix = GetInverseMatrix)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve 
## function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inverse.matrix <- x$GetInverseMatrix()
    if(!is.null(inverse.matrix)) {
        message("getting cached data")
        return(inverse.matrix)
    }
    data <- x$GetMatrix()
    inverse.matrix <- solve(data, ...)
    x$SetInverseMatrix(inverse.matrix) # This command saves the inverse
    inverse.matrix      
}
