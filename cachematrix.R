## Put comments here that give an overall description of what your
## functions do

## This is a special vectorized function that will allow the storage
##of values from global environments and temporarily from local environments.
##The values stored are those that have not been calculated yet
## and those that have been calculated and are being cached. In the end a list is made
##to make accessing this information more direct in the next function. NOTE: THE ACTUAL CALCULATION IS NOT DONE HERE.

makeCacheMatrix <- function(x = matrix()) {
  x <- x # This is the matrix for which the inverse will one day be calculated for
  matrix_inverse <- NULL # This is the local inverse of the vector x, which may or may not be precalculated
  
  
  ## Creating a setter function that will store global values
  set <- function(y) {
    x <<- y
    matrix_inverse<<- NULL
  }
  ##creating a getter function to get the values and pass them on
  get <- function(){
    return(x)
  }
  setinverse <- function(inverse){
    matrix_inverse <<- inverse
  }
  getinverse <- function(){
    return(matrix_inverse)
  
  }
  list(
    set = set, 
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## This function accesses the cached data. If the inverse has not
## been calculated then it calculates the inverse and stores that new information
##into the global inverse information. If it already has been calculated
## this function simply access that and uses that information instead
##of generating it de novo.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrix_inverse <- x$getinverse()
        # If matrix_inverse is NULL, that means that the mean HAS NOT YET been calculated, so let's calculate it now
    if(is.null(matrix_inverse)){
      
      data <- x$get() # get and store a vector of numbers  in "data" from the special object that fulfill the "if" statement that they have not yet been calculated. ie they are nULL.
      
      matrix_inverse <- solve(data, ...) ##actually calculate the inverse for those that have not yet been calculated
      
      x$setinverse(matrix_inverse)
      return(matrix_inverse)
      
    }else{ # If matrix_inverse is NOT NULL, then that means that the mean has already been calculated, just return that precalculated inverse
      message("getting cached data")
      return(matrix_inverse)
    }
  }

