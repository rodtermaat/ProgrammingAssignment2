## makeCacheMatrix: This function stores the inverse of a matrix in cache in order 
##                  to speed up future requests.  Once set the process can access the 
##                  cached value and does not hav to recalculate the inverse.  When 
##                  a new matrix is passed the process regonizes the calculation 
##                  must be done again and recached for future calls to the function 


##  The inverse of a matrix is like taking the reciprocal of a number. You cannot 
##  divide a matrix, but you can multiply the reciprocal.  I am not going to pretend I 
##  know you to apply this function in a real life scenario. 


makeCacheMatrix <- function(x = matrix()) { 
  s <- NULL           ## this is the controller of the cache and contains the inverse value 
  set <- function(y) { 
    x <<- y 
    s <<- NULL 
  } 
  
  ## these functions are called by the cacheSolve function to set and get the inverse from 
  ## either the calculation itself of the stored cache - called object methods 
  get <- function() x 
  setsolve <- function(solve) s <<- solve 
  getsolve <- function() s 
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve) 
} 


## cacheSolve:Given a matrix as input this function either retreives the cached value or 
##            recalculates and sets a new cached value 
cacheSolve <- function(x, ...) { 
  ## Return a matrix that is the inverse of 'x' 
  s <- x$getsolve() 
  if(!is.null(s)) {               ##if the inverse has alrady been calculated use cached valued
    message("getting cached data") 
    return(s) 
  } 
  data <- x$get()                 ## value not cached or different matrix - recalculate
  message("calculating inverse") 
  s <- solve(data, ...) 
  x$setsolve(s) 
  s 
}

## Test matrix and code
##xy <- c(2,4,4,2)
##m_test <- matrix(data = xy, nrow=2, ncol=2)
##m_test
##z <- makeCacheMatrix(m_test)
##cacheSolve(z)

