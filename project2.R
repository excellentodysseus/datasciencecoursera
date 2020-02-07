

makeCacheMatrix <- function(x = matrix()){
  inverse <- NULL
  set <-  function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(the_inverse) inverse <<- the_inverse
  getInverse <- function(the_inverse) inverse
  list(set = set, get = get, 
        setInverse = setInverse,
        getInverse = getInverse)
}

cacheSolve <- function(x,...){
  
  inv = x$getInverse()
  if(!is.null(inverse)){
    message("geting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data,...)
  x$setInverse(inverse)
  return(inverse)
}
