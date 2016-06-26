## Data structure to store a matrix and its inverse, cached.
#
# Arguments: matrix - a matrix
# Returns: a list of functions to set and get a matrix and its (cached) inverse

makeCacheMatrix <- function(matrix = matrix()) {
    # Create empty object to store inversed matrix
    cachedInverse = NULL
    
    # Function to get the stored matrix
    getMatrix <- function() matrix;
    
    # Function to update the given matrix.
    # It also resets the cached value
    setMatrix <- function(newMatrix) {
        matrix <<- newMatrix
        cachedInverse <<- NULL
    } 
    
    # Function to get cached inverse
    getInverse <- function() cachedInverse
    
    # Function to update the cached inverse
    setInverse <- function(newInversed) cachedInverse <<- newInversed
    
    # Return a list of functions
    # Those functions will have the environment in which they
    # were defined associated with them, thus exposing the correct
    # objects "matrix" and "cachedInverse".
    #
    # Only possible due to lexical scoping in R. If R used dynamic
    # scoping, getInverse() would look for object cachedInverse in
    # environment in which it is called, and not in the one where
    # it was defined.
    list(getMatrix = getMatrix, setMatrix = setMatrix,
         getInverse = getInverse, setInverse = setInverse)
}


## Function to calculate the inverse of the matrix special
## data structure defined on the previous function.
## First it looks on its cache to see if it has already
## been calculated. Otherwise it calculates and stores it there.
# 
# Arguments: matrix - list returned from makeCacheMatrix()
# Returns: inverse of the matrix
# Side effects: update cached version on input data structure if
#  it was empty.

cacheSolve <- function(matrix, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse = matrix$getInverse();
    
    # Check if we got something
    if(!is.null(inverse)) {
        # If we did, return it
        message("Retrieving cached version")
        return(inverse)
    }
    
    # Otherwise, we got nothing. We have to:
    message("Cache not found. Calculating inverse")
    
    # Get the matrix
    data <- matrix$getMatrix()
    
    # Calculate the inverse
    inverse <- solve(data)
    
    # Store it on cache
    matrix$setInverse(inverse)
    
    # Return it
    inverse
}

## Function to test cacheMatrix() function
# Expected to print messages:
#   Cache not found. Calculating inverse
#   [1] TRUE
#   Retrieving cached version
#   [1] TRUE

testCacheMatrix <- function() {
    # Create matrix: 1 3
    #                 2 4   
    matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
    
    # Use our function to calculates its inverse
    inverse <- cacheSolve(matrix)
    
    # Check if inverse is correct by using the property that
    # states that: matrix * matrix_inverse = identity matrix
    print(identical(matrix$getMatrix() %*% inverse, diag(2)))
    
    # Use our function again. Now should print message
    # saying that there is a cached version
    inverse <- cacheSolve(matrix)
    
    # Perform check again
    print(identical(matrix$getMatrix() %*% inverse, diag(2)))
}

# Call function to test
testCacheMatrix()
