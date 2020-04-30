#setmx sets a matrix value.
#getmx returns a matrix value.
#setinv sets the inverse of matrix values.
#getinv returns the inverse of matrix values.

## The following function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  cache <- NULL
  setmx <- function(y) 
    {x <<- y
    cache <<- NULL}
  getmx <- function() 
    {x}
  setinv <- function(solve) 
    {cache <<- solve}
  getinv <- function() 
    {cache}
  
  list(
    setmax=setmx
    ,getmx=getmax
    ,setinv=setinv
    ,getinv=getinv
    )
}

## The following function 'cacheSolve' returns the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(y, ...) {

  inv <- y$getinv()

  if(!is.null(inv)) 
  {return(inv)}
  
  final <- y$getmx()
  inv <- solve(final, ...)
  y$setinv(inv)
  
  print(inv)
}
