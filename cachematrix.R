## Matrix inversion is usually a costly computation. The following functions
## compute and cache the inverse of a matrix, rather than computing it repeatedly.

## makeCacheMatirx() creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix())
{
    
    inv <- NULL # inv will be our 'inverse matrix' and it's reset to NULL every
                # time makeCacheMatrix is called
    
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() # This function returns the original matrix
    {
        x
    }
    
    setInverse <- function(inverse) # This is called by cacheSolve()
    {
        inv
    }
    
    getInverse <- function() # This will return the cached value
    {
        inv
    }
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    # This list is returned with the newly created object. It lists all the
    # functions that are part of the object.
}


## cacheSolve() calculates the inverse of the special "matrix" created with
## makeCacheMatrix(). But it first checks to see if the inverse has already
## been calculated.
## If so, cacheSolve() gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the
## inverse in the cache via setInverse() defined in makeCacheMatrix()

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv))
    {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
