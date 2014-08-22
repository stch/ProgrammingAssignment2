## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function returns a matrix object that has getter/setter for both original and inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  #   i <- solve(x)
  #   get <- function() x
  #   set <- function(y){
  #     x <<- y
  #     i <<- solve(y)
  #   }
  #   getInv <- function() i
  #   setInv <- function(y){
  #     x <<- solve(y)
  #     i <<- y
  #   }
  i <- NULL
  get <- function() x
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  getInv <- function() i
  setInv <- function(y)     i <<- y
  return (list(get=get,set=set,getInv=getInv,setInv=setInv))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  if(is.null(x$getInv())){
    x$setInv(solve(x$get()))
  }else{
    message("cache was found.")
  }
  x$getInv()
}
