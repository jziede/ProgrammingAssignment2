##I wrote this function to be as close to makeVector as possible while just 
##swapping out "mean" for "inverse" and "m" for "inv"
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() {
    inver <- ginv(x)  ## This part is different becaue when I used to getinv part,
    inver%*%x ## it gave me null as a response, so I put in ginv from the MASS
  }  ## package to get the inverse, and then it worked
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##I wrote this function to be as close to cachemean as possible while just 
##swapping out "mean" for "inverse" and "m" for "inv"
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inverse(data, ...)
  x$setinv(inv)
  inv
}
