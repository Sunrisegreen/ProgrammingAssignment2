## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function takes a matrix as input value and returns a list of functions
## which can set or get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set<- function(n){
    x <<- n
    inv <<- NULL
  }
  
  get<- function() x
  
  setInverse <- function(minv) inv <<- minv
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function fetches the inverse value stored in x, and returns it if not null
## If it is null, then the inverse of the matrix is calculated using solve() and stored in x
## The inverse matrix is also returned from the function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv1 <- x$getInverse()
  if(!is.null(inv1)){
    message("Getting cached inverse")
    return(inv1)
  }
  a <- x$get()
  inv1 <- solve(a)
  x$setInverse(inv1)
  inv1
}
