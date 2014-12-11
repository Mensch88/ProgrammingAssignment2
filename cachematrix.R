## The following two functions create a special object that stores a square
## matrix and caches its inverse.

# `makeCacheMatrix` creates a special "matrix" that can cache its inverse.
#
# Args:
#   x: the square matrix to be stored, Default is a empty matrix
#
# Returns:
#   a list of functions, which can get the value of the matrix,
#   set a new matrix, get the inverse and set the inverse.

makeCacheMatrix <- function(x = matrix()) {    
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# `cacheSolve` retrieves the cached inverse in the special "matrix" created
# by `makeCahceMatrix` above, if the inverse has already been calculated, 
# or otherwise solves its inverse and caches it.
#
# Args:
#   x: the special "matrix" object, produced by `makeCahceMatrix`.
#   ...: optional arguments to `solve`.
#
# Returns:
#   the inverse of the matrix in 'x'

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
