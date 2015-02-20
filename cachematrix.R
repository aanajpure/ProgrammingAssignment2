## Put comments here that give an overall description of what your
## functions do

## This function creates a special 'matrix' object
## that can cache the inverse of this matrix

makeCacheMatrix <- function(x = matrix()) 
{
      
  #m <- NULL
  #set <- function(y) 
  #{
  #  x <<- y
  #  m <<- NULL
  #}
  #get <- function() x
  
  #setmean <- function(mean) m <<- mean
  
  #getmean <- function() m
  
  #list(set = set, get = get,
   #    setmean = setmean,
  #     getmean = getmean)
  
  i <- NULL # initialize inverse of the matrix to NULL
  set <- function(y) 
  {
    x <<- y
    i <<- NULL
  }
  get <- function() {x} #simply returns the matrix
  setinv <- function(solve) {i <<- solve} # sets i to inverse of the matrix
  getinv <- function() {i} #returns the valueof i - i.e. inverse of the matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # m <- x$getmean()
  # if(!is.null(m)) {
  #  message("getting cached data")
  #  return(m)
  # }
  # data <- x$get()
  # m <- mean(data, ...)
  # x$setmean(m)
  # m
  
  i <- x$getinv() # get the inverse of the matrix and store in i
  if(!is.null(i)) { # i.e. if i value is stored in the cache, then get it from there
    message("getting cached data")
    return(i)
  }
  #else inverse of the matrix is not stored in the cache, then do the below steps
  data <- x$get() #get the matrix as it is and store in the variable- data
  i <- solve(data, ...) #make inverse of the matrix and assign it to i
  x$setinv(i) 
  i
}
