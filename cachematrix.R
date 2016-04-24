## The functions makeCacheMatrix and cacheSolve create a special "matrix" object (makeCacheMatrix) that can
##    cache its inverse which is then retrieved by cacheSolve.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mat=matrix()){
  #Description: 
  #  Argument: mat - a square, invertible matrix 
  #  Returns - a list of functions:
  #             a. setmat - sets the matrix.
  #             b. getmat - gets the matrix.
  #             c. setinvrs - sets the inverse matrix.
  #             d. getinvrs - gets the inverse matrix.
  #           the above list of functions serves as the argument to the 
  #           accompanying function for this assignment: cacheSolve()
  
  invrs <- NULL
  setmat <- function(newmat) {
    mat <<- newmat
    #assign mat to newmat in a different environment using <<-
    invrs <<- NULL
    #assign invrs to NULL in a different environment using <<-
  }
  
  getmat <- function() mat
  setinvrs <- function(inverse) invrs <<- inverse
  getinvrs <- function() invrs
  list(setmat <- setmat, getmat <- getmat, setinvrs <- setinvrs, getinvrs <- getinvrs)      
}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then 
#cachesolve retrieves the inverse from the cache.

cacheSolve <- function(mat,...) {
  #Description:
  # Argument: mat a matrix outputted from makeCacheMatrix()
  # Return: the inverse of the original matrix used as the
  #   argument for getCacheMatrix
  
  invrs = mat$getinvrs()
  
  #Search for the inverse to see if it already exists.
  if (!is.null(invrs)){
    #If it exists, retrieve it from the cache. 
    
    message("Searching for cached matrix.")
    return(invrs)
  }
  
  #If it does not exist, then calculate it.
  
  mat.data <- mat$getmat()
  invrs <- solve(mat.data, ...)
  
  #Set the value of the cache inverse matrix 
  mat$setinvrs(invrs)
  
  return(invrs)
}