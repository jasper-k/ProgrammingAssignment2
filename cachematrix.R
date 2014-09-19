## These functions create and use a special "matrix" object that can cache its inverse.
# 
#  

## makeCacheMatrix: This function creates a list of functions to:
# 1. set a new matrix x as the main matrix to work with
# 2. get the current matrix x
# 3. set_invert (calculate) the inverse matrix of x 
# 4. get_invert returns the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                #Reset the cached inverse to NULL to (re)trigger its new calculation
				inv <<- NULL
        }
        get <- function() x
        set_invert <- function(solve) inv <<- solve
        get_invert <- function() inv
        list(set = set, get = get,
             set_invert = set_invert,
             get_invert = get_invert)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
#  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inv <- x$get_invert()
        if(!is.null(inv)) {
				#If the inverse is not null, we serve it from the cache and exit
                return(inv)
        }
		#Otherwise calculate the inverse matrix
        data <- x$get()
        inv <- solve(data, ...)
        x$set_invert(inv)
        inv
}
