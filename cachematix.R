# These functions is to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x=matrix()){
  inv <- NULL
  set <- function(y) {
      x <<-y
      inv <<-y
  }
  get <- function() x
  setinv <- function(invr) inv <<- invr
  getinv <- function () inv
  list (set=set,
        get=get,
        setinv=setinv,
        getinv=getinv
        )
}
# This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then `cacheSolve` should retrieve the inverse from the cache.
cacheSolve <- function(x,...){
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
