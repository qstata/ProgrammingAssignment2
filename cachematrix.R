####################################################################################
## Functions for creating and using a inverted matrix by caching the results.
## Functions list: (1) makeCacheMatrix, (2) cacheSolve
## -----------------
## Usage example:
## >  
## > m1 <- makeCacheMatrix()                ## Create an object
## > m1$set(rbind(c(1, -1/4), c(-1/4, 1)))  ## set the input matrix to be inverted
## > 
## > cacheSolve(m1)         ## Create the inverted matrix if it does not cached
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 
## > cacheSolve(m1)         ## Return the cached inverted matrix
## Getting cached inverse matrix
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 


## ---------------------------------------------------------------------------------
## Function (1): makeCacheMatrix -- creating a R object with caching fields 
##
makeCacheMatrix <- function(x = matrix()) {

  ## Checking the input data format. 
  if (!is.matrix(x)) stop("The input must be a matrix")

  ## initialize the return value: Return a matrix that is the inverse of 'x'.
  inverted.x <- NULL

  set <- function(y) {
    x <<- y
    inverted.x <<- NULL
  }
  
  # Functions for getting and setting cached inv. matrix value
  get <- function() x
  # Inversing the matrix using build in solve() function in R
  set.inverse <- function(solve) inverted.x <<- solve
  get.inverse <- function() inverted.x
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
  
}

##----------------------------------------------------------------------------------
## Function (2): cacheSolve -- return a cached inverted matrix, or create 
##                             the inverted matrix if it does not cached.
##
cacheSolve <- function(x, ...) {
  ## get the value from the cached field of the R object.
  inverted.x <- x$get.inverse()

  ## Check whether or not the inverted 'x' is cached.
  if(!is.null(inverted.x)) {
    message("Getting cached inverse matrix")
    return(inverted.x)
  }

  ## Create the inverted matrix if it does not cached
  A <- x$get()  ## get the input matrix
  inverted.x <- solve(A) ## Inverse of A where A is a square matrix.
  x$set.inverse(inverted.x)

  ## Return a matrix that is the inverse of 'x'
  return(inverted.x)
}

## END
