## The functions makeCacheMatrix and cacheSolve basically creates a matrix (makeCacheMatrix) based on user input and computes
## the inverse of the user defined matrix (cacheSolve function)
## This creates a new matrix based on the input the user gives. It is assumed that the input given 
## by user is an invertible matrix. No checks are done to see if the matrix is invertible or not.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve function computes the inverse of the matrix using R's solve function itself.
## and retrieves the inverse matrix from memory if it already exists or computes one and stores it in memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## Commands to execute to check the working of this program
# m<- makeCacheMatrix( )
# m$set( matrix( rnorm(4), 2, 2))         #Creates a new matrix
# m$get()                                 # Gets the new matrix created above
# cacheSolve( m )                         # Gets the inverse matrix not cached
# cacheSolve( m )                         # Gets the inverse matrix, cached though

