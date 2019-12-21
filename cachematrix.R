## Caching the Inverse of a Matrix:

## cacheSolve() function which sets and gets the cached values

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y = matrix()){
    x <<- y
    inv <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setinv <- function(i){
    inv <<- i
  }
  
  getinv <- function(){
    inv
  }
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}


#### If the inverse has already been calculated and there's no change in the matrix
####then the cacheSolve() returns the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  
}
