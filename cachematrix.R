## These functions demonstrate the creation and
## caching of the inverse of a matrix.

## The following function initializes a local variable
## "inverse" to NULL, and contains children functions that
## perform the following:
## (1) Set or modify a matrix
## (2) Get the value of the matrix
## (3) Set the inverse of the matrix and cache it
## (4) Get the inverse of the matrix

makeCacheMatrix <- function(mat = matrix()) {
  # Initialize the matrix inverse
  inverse = NULL
  
  # Super-assign the passed-in matrix value along with
  # the initial inverse value
  set <- function(y) {
    mat <<- y
    inverse <<- NULL
  }
  
  # Get the value of the matrix
  get <- function() mat
  
  # Set the inverse of the matrix
  setInv <- function(inv) inverse <<- inv
  
  # Get the inverse of the matrix
  getInv <- function() inverse
  
  # Return a list of the above functions
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## The following function computes the inverse of the matrix
## created by the makeCacheMatrix function. If the inverse had
## already been calculated, then the inverse should be retrieved
## from the cache.

cacheSolve <- function(mat, ...) {
  
  # Get the cached matrix inverse
  matInv <- mat$getInv()
  
  # If no matrix inverse were cached, then calculate the
  # matrix inverse and set it. Otherwise, indicate that we are
  # getting the cached data.
  if (!is.null(matInv)){
    message("Getting cached data")
    return(matInv)
  }
  
  data <- mat$get()
  matInv <- solve(data, ...)
  mat$setInv(matInv)
  matInv
}
