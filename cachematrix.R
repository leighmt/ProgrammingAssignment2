## This file defines two functions:
##  1. makeCacheMatrix - takes one argument (a matrix) and returns
## 			 a matrix object with four functions defined
##			 to get and set cached values for the matrix
##
##  2. cacheSolve -	 takes one argument (a matrix object as returned
##			 by makeCacheMatrix), checks the cache for the
##			 matrix's inverse and returns it if exists else
##			 generates the inverse and caches it.

##
## makeCacheMatrix
##
## This function defines a list of four functions to:
##  1. Set the value of the matrix 
##  2. Get the value of the matrix
##  3. Set the value of the inverse matrix
##  4. Get the vlaue of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
	## Initialise the value i to be used for the inverse
	i <- NULL

	## Generate a function to set the value of x.
	## This sets the value of x to the value used as an
	## argument to set when called and resets i to NULL
	set <- function(y) {
		x <<- y
                i <<- NULL
	}

	## Generate a function to get the value of x.
	## This simply returns the value x
	get <- function() x

	## Generate a function to set the value i and cache it.
	setinverse <- function(inverse) i <<- inverse

	## Generate a function to get the inverse.
	## This simply returns the value of i
        getinverse <- function() i

	## Return a list containing the four functions defined
	## above so that they can be called externally.
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##
## cacheSolve
##
## This function checks the cache for the inverse of x.
##
## If the value exists in the cache, it is returned.
##
## If the value doesn't exist in the cache the inverse of x is 
## calculated using the solve function and the solution is both
## added to the cache and returned.

cacheSolve <- function(x, ...) {
	# Check the cache for the inverse of x
        i <- x$getinverse()

	# If the inverse exists in the cache print a message
	# stating this and return the value as stored in the cache
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }

	# Get the value of x
        data <- x$get()

	# Calculate the inverse of x
        i <- solve(data, ...)

	# Cache the inverse of x
        x$setinverse(i)

	# Return the inverse of x
        i
}

## Usage examples
##
## > x <- matrix(1:4, 2)
## > x
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cx <- makeCacheMatrix(x)
## > cx$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > cx$getinverse()
## NULL
## > cacheSolve(cx)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(cx)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cx$getinverse()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
