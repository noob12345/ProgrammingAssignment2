# Below are two functions that are used to create a special object 
# that stores a numeric matrix and caches its inversion.

# This function  creates a list that contains 4 member functions.
# These are the: set, get, setInv and getInv. It uses <<- assignment 
# operator so that these internal variables are not exposed to the 
# external environment.
makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL                          # Inversion result storage
  
  set <- function(y) {
    x <<- y
    xinv <<- NULL                       # Initialize xinv to null
  }
  
  get <- function() x                   # Return the input matrix
  setInv <- function(inv) xinv <<- inv  # Set the inversed matrix
  getInv <- function() xinv             # Return the inversed matrix
  
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}
        
# This function calculates the inversed version of the special matrix
# already created with the above function. It first checks if the inversed
# matrix has already been calculated. If so, it gets the inversed matrix
# from the cache and skips the computation. Otherwise, it calculates the
# inversed matrix and sets the value in the cache via the setInv function.
cacheSolve <- function(x, ...) {
  m <- x$getInv()                   # Get the inversed matrix from object x
  
  if(!is.null(m)) {                 # If the inversion result is there
    message("getting cached data")
    return(m)                       # Return the calculated inversion
  }
  data <- x$get()                   # If not, we do x$get to get the matrix object
  m <- solve(data)                  # Solve it
  x$setInv(m)                       # Set it to the object
  m                                 # Return the solved result
}
