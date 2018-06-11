## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    n <- NULL #set value n to NULL
    set <- function(y){
            x <<- y
            n <<- NULL
    }
    get <- function() x #assigns matrix to get 
    setCAC <- function() n <<- solve(x) #performs inverse of matrix
    getCAC <- function() n #assigns inverse of matrix
    #creates list of closure functions
    list(set = set, get = get, setCAC = setCAC, getCAC = getCAC)
}

## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      n <- x$getCAC() #retrieves getCAC value from x if present
      if(!is.null(n)){ #confirms if n has a stored value
            #will print if inverse matrix cache is found
            message("retrieving cached data") 
            return(n)
      }
      info <- x$get() #performs get of matrix
      q <- solve(info) #performs an inversion on the matrix
      x$setCAC() #assigns inverse matrix to closure function setCAC
      return(q) #displays inverse matrix to ensure code has run
      
}