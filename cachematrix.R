# The first function, makeCacheMatrix creates a special "matrix", which is really 
# a list containing a function to
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse
# 4. get the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL    # initialize the inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL
        } # initialize the matrix and inverse
        get <- function() x #get the matrix
        setinverse <- function(inverse) inv <<- inverse #sets the inverse
        getinverse <- function() inv #gets the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) #list of functions
}


# The following function calculates the inverse of the special "matrix" created with 
# the above function. However, it first checks to see if the inverse has already been 
# calculated. If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the matrix of the inverse in 
# the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() #get the inverse
        # check if inverse has already been calculated and return cache if true
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get() #get the matrix
        inv <- solve(data, ...) #calculate the inverse
        x$setinverse(inv) #set the inverse in the cachec
        inv
}
