## makeCacheMatrix() creates a special "matrix" object that can cache its inverse
## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache. 

## makeCacheMatrix has 4 methods: set - to set a matrix, get - to get matrix, setinv and getinv to set and get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # matrix inverse
        set <- function(y) {    ## According to assignment description: "For this assignment, assume that the matrix supplied is always invertible.",
                x <<- y         ## so there is no check if y is matrix
                inv <<- NULL
                }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        }


## cacheSolve checks if the inverse of 'x' has already been calculated and if so, returns it from cache,
## otherwise it calculates the inverse and set it to 'x'


cacheSolve <- function(x, ...) {
        has_inv <- x$getinv()
        if (!is.null(has_inv)){
                message("getting cached data")
                
                has_inv
        }
        data <- solve(x$get())
        x$setinv(data)
        data
}

