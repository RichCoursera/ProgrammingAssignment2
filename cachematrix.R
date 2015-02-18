## The two functions below will take a matrix and return
## the inverse of that matix.
##There is also a function for an error message for invalid data.

## This function creates a special "matrix" object 
## that caches its inverse.
makeCacheMatrix <- function(x = matrix() )
{
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}  

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the 
## cacheSolve will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
  m <- x$getinverse()
  if(!is.null(m)) 
    {
    message("getting cached data")
    return(m)
    }
  data <- x$get()
  ##m <- solve(data, ...)
  ## credit for the tryCatch goes to the URL: http://stackoverflow.com/questions/3440373/functions-and-try-in-r
  tryCatch(m <- solve(data, ...), error=function(e)
    {print(errmsg()) ; return()} )
  x$setinverse(m)
  m
}
  
## Error message handling
errmsg<- function()
{
    message("The data entered was not valid. It was either")
    message("a singular matrix or not a squre matrix")
    message("PS. I cannot figure out how to stop the function")
    message("before the two NULLs show below :)")
    return()
}

