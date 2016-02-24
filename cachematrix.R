## This file contains following two functions 
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache.


## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of matrix
## 4. get the value of the inverse of matrix 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # initialize the variable for inverse matrix as NULL
  
  set <- function(y){
    x <<- y   #Set the new matrix to be set and 
    i <<- NULL #each time new matrix is to be stored then reset its inverse to NULL
  }
  get <- function()
  {
    x  #being a get function simply return x ( the matrix)
  }
  setinverse <- function(inv){
    i <<- inv # set and store the inverse matrix being passed in the set function
  } 
  getinverse <- function()
  {
    i #being a get function simply return i ( the inverse matrix)
  }
  #this makeCacheMatrix function will return the list with following elements
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse  of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()  # get the inverse using the getinverse function
  if(!is.null(i)) {    # if the returned inverse matrix is not null then already calculated inverse exists
    message("getting cached data")
    return(i)         # return the inverse matrix from cache
  }
  data <- x$get()     # code reaches here if inverse is not there in cache get the actual matrix for which inverse needs to be computed
  i <- solve(data, ...) # compute inverse using solve R function
  x$setinverse(i)     # set the calculated inverse matrix to cache
  i                   # return the inverse matrix
  
}