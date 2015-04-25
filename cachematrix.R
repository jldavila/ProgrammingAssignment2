## Prog. Assigment 2: Caching the Inverse of a Matrix
## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix: Creates matrix "object" that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL # Initialize matrix
    
    set <- function(y){ # Setter method - initializes matrix 
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x # Accessor method - gets matrix
    
    # Setter method - sets matrix inverse value
    setinverse <- function(inverse) inv_x <<- inverse 
    
    # Getter method - gets matrix inverse
    getinverse <- function() inv_x
    
    list(set = set, 
         get = get,
         setinverse = setinverse,
         getinverse = getinverse) 
}


## cacheSolve: Get the inverse of the matrix from cache. 
##             Calculates inverse if not existing in memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mi <- x$getinverse() # Verify if inverse exists in cache
    
    if(!is.null(mi)){ # Get matrix inverse from cache if it exists
        message("Getting cache data")
        return(mi)
    }
    
    # Calculate Inverse - If inverse doesn't exist yet
    data <- x$get()
    mi <- solve(data, ...) # Inverse calculation
    x$setinverse(mi)
    mi

}

## Sample commands to test executing of makeCacheMatrix & cacheSolve
# mat_temp <- matrix(sample(1:10, 16, replace=T), 4, 4)
# mat_cache <- makeCacheMatrix(mat_temp)
# cacheSolve(mat_cache)

