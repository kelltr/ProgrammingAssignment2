## Put comments here that give an overall description of what your
## functions do
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
  x1 <- NULL               ##will store the cached inverse matrix when calculated
 
  ##sets the matrix
  set <- function(y) {
    x <<- y                ## sets x in parent environment
    
  }
  # calls the matrix
  get <- function() x
  
  # Setter for the inverse
  setx1 <- function(inverse) x1 <<- inverse
  # Getter for the inverse
  getx1 <- function() x1
  
  ## list of new functions created
  list(set = set, get = get, setinv = setx1, getinv = getx1)
}


## Write a short comment describing this function
## Checks if an inverse for the matrix has been calculated if not it calculates the inverse
## it then returns the cached inverse
cacheSolve <- function(x, ...) {
  x2 <- x$getinv()
  
  if (!is.null(x2)) {                               ## Checks if the inverse if already calculated
    message("cached data exits .. retrieving")                   ## if calculated already tells you  -- not returning message look up --fixed now
    return(x2)
  }
  # Gets here when the if fails so we calculate the inverse of the matrix
  data <- x$get()                                    ## set data to the retrieved matrix x with the get function created before and passed through the ...
  message("calculating inverse")                     ## tells you why the delay
  x2 <- solve(data, ...)                            ## solve() is  used on data to calcalculate the inverse of data 
  
  # Cache the inverse using set inverse function from the previous list.  sets in the parent environment using <<-
  x$setinv(x2)
  
  
  ## Return a matrix that is the inverse of 'x'
  x2
}

## testing
## Example usage:
## > x <- matrix(rnorm(16), nrow = 4)                     // Create a matrix x as a 4by4 matrix
## > m = makeCacheMatrix(x)                               // sets m to matrix in function
## > m$get()                                              // returns the matrix
##            [,1]       [,2]       [,3]        [,4]
##[1,]  1.35834766  1.2821097  0.2214925 -1.19214767
##[2,]  0.10918052  0.6477042 -0.6379810  0.09546738
##[3,] -0.87158915 -0.2879350 -0.3510110 -1.02883196
##[4,]  0.06803428 -0.8219490  1.0126577 -0.81827332
##
## > cacheSolve(m)                            // No cache in the first run
## calculating inverse
## [,1]       [,2]       [,3]      [,4]
## [1,] -1.73557091  10.551172 -2.0370764  6.320821
## [2,]  2.34642768 -11.072020  1.7322144 -6.888239
## [3,]  2.09972588 -11.299541  1.3798059 -6.112272
## [4,]  0.09725518  -1.984777 -0.2017808 -1.341637

## > cacheSolve(m)                            //Retrieving from the cache in the second run
## cached data exits .. retrieving
## [,1]       [,2]       [,3]      [,4]
## [1,] -1.73557091  10.551172 -2.0370764  6.320821
## [2,]  2.34642768 -11.072020  1.7322144 -6.888239
## [3,]  2.09972588 -11.299541  1.3798059 -6.112272
## [4,]  0.09725518  -1.984777 -0.2017808 -1.341637