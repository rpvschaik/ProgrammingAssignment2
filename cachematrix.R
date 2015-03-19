## makeCacheMatrix creates a list with 4 named elements, each of which is a function to either
## set a matrix, get a matrix, set the inversed matrix or get the inversed matrix.
## these functions are called by the cacheResolve function. If the inversed matrix has already been calculated
## the results will be retreived from cache, instead of recalculating

makeCacheMatrix  <- function(x = matrix()) 
{
     i <- NULL ## initialise/reset variabe to store inverse matrix
     
     ## 1st: set the matrix    
     set <- function(y)
     {
          x <<- y ## to parent environment, because needed by get function
          i <<- NULL ## to parent environment because needed by setinverse function
     }
     
     ## 2nd:  get the matrix
     get <- function() {x} 
     
     ## 3rd: inverse matrix. 
     setinverse <-  function(solve) {i <<- solve} ## i to parent environment, because needed by getinverse
     
     ## 4th, get the inversed matrix
     getinverse <- function() {i} 
     
     ##return list of functions
     list
     ( 
          set = set, 
          get = get,
          setinverse = setinverse,
          getinverse = getinverse
     )
}

## cacheSolve inverts matrix and stores it in cache. 
## next time the function will be called, the cached matrix will be return,without recalculation
cacheSolve <- function(x, ...) 
{
     i <- x$getinverse()
     
     if(!is.null(i))  ##if i is not null (i.e. inverted matrix available), cached data will be returned 
     {
          message("getting cached data")
          return(i)
     }
     
     data <- x$get() ## if i is null, get matrix
     
     i <- solve(data, ...) ## and inverse the matrix
     
     x$setinverse(i)
     i ## return the inverted matrix
}