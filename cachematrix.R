## This file contains following functions
##    1. makeCacheMatrix
##    2. cacheSolve
## 
## 
##

## makeCacheMatrix function creates a matrix object with
## methods to get and set the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
   ## This function sets new matrix and resets the inverse matrix to NULL
   set <- function(y) {
    x <<- y
    invMatrix <<- NULL
   }
   ## This fuction returns the matrix
  get <- function() x
  
  ## This function sets the inverse matrix
  setinverse <- function(inv) invMatrix <<- inv
  
  ## This function returns the inverse matrix
  getinverse <- function() invMatrix
  
  ## returns the list of functions available in the matrix object
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function checks whether the matrix passed has 
## inverse matrix in cache using getinverse function. 
## If the inverse matrix is not available in cache, then 
## creates the inverse matrix using solve() method and sets
## the inverse matrix in the matrix object

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv<-x$getinverse()
  
    if( !is.null(inv)){
     message("getting cached data")
     return(inv)
    }
   ## Crete the inverse matrix using solve()
   inv<-solve(x$get())
   x$setinverse(inv)
   inv
}


