##################################################
## cache the calculation of inverse matrix
## first the matrix need to be created by a call to makeCacheMatrix
## then cacheSolve will solve the matrix
## 



#################################################
## Write a short comment describing this functiom
## makeCasheMatrix
## purpose : to create a matrix with metadata that can store the inverse of the matrix
## usage
##
## myMatrix <- makeCacheMatrix(matrix(sample(1:49,49,replace=FALSE),nrow=7,ncol=7))
##
## myMatrix$get()
## return the original matrix
##
## myMatrix$set()
## set a new matrix to solve, also clears the cache
##
## myMatrix$getmatrix()
## return the solved inverse of the original matrix (if already solved)
##
## myMatrix$setmatix(NULL)
## clear the inverse cache
##
## list
## no fucking clue what this does.
###
## example:
## create a 7x7 matrix with random numbers (integers) from 1 to 49
## cacheSolve(myMatrix)
## solve the matrix created above

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}
####################################################
## usage after successful call to makeCacheMatrix, cacheSolve can 
## solve the matrix, but cached.
## the commented lines proves that it works
## uncomment to see what actually happens
## usage 
## cachesolve(matrix)
cacheSolve <- function(x, ... ) {
  ## return the invese matrix
  m<-x$getmatrix()
  if(!is.null(m)) {
    #print("cached data")
    m
  } else {
    #print("calculate inverse")
    matrix <- x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
  }
}
