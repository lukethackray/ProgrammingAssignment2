##ASSIGNMENT 2

##these are the functions from the exercise

# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }
# 
# cachemean <- function(x, ...) {
#   m <- x$getmean()
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m)
#   }
#   data <- x$get()
#   m <- mean(data, ...)
#   x$setmean(m)
#   m
# }



## Put comments here that give an overall description of what your
## functions do

## makeCachematrix creates a special type of matrix (a cacheMatrix), containing four functions which are used to interact with the matrix and its inverse 
## cacheSolve calculates the inverse of the matrix, storing it in the cache for quicker recall.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  # set: sets the values of the special matrix to an inputted matrix, while resetting any inverse that may be held in cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get: returns the values of the matrix
  get <- function() x
  
  # setInv: sets the inverse matrix m, that is then held in cache, as the variable inv. This value 'inv' is set when using the cacheSolve function to calculate the inverse.
  setInv <- function(inv) m <<- inv
  
  #getInv: returns the already held inverse matrix. This may be NULL if cacheSolve has not been used to calculate the inverse.
  getInv <- function() m
  
  #gives names to each of those functions so we can then use specialmatrix$functionname
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Requires x to be a cachematrix type as set by makeCacheMatrix
  
  #returns the inverse that is already held in cache.
   m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
   #If the inverse is not held in cache already (because it hasn't been calculated yet) then we need to use $get to get the values of the matrix and then use solve(x) to return the inverse of the matrix. 
  mat <- x$get()
  m <- solve(mat)
  
  #we then pass the calculated inverse back into the cache to be used elsewhere, rather than having to recalculate.
  x$setInv(m)
  m
}
