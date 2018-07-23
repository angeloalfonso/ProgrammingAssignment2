## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

## x is initialized as a function argument to be a matrix object
## n is initialized to NULL in the makeCacheMatrix environment
makeCacheMatrix <- function(x = matrix()) {     
        n <- NULL
## Function set() is defined
## This function sets input y to object x in the parent environment
## Object n is initialized as NULL to ensure cached inverse is cleared
## whenever a new value for x has been defined by set() function
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
## Function get() is defined
## This is used to retrieve the matrix input
        get <- function() x
## Function setmatinv() is defined
## This is used to set and "store" the matrix inverse
        setmatinv <- function(minv) n <<- minv
## Function getmatinv() is defined
## This is used to retrieve the matrix inverse
        getmatinv <- function() n
## Creates a list containing each of the functions as elements
## Elements are named in the arguments so they can be used
## and called later downstream
        list(set = set, get = get,
             setmatinv = setmatinv,
             getmatinv = getmatinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
## Retrieves matrix inverse using getmatinv() on input x
        n <- x$getmatinv()
## n is checked whether it is NULL or not
## if n is not NULL, meaning makeCacheMatrix() or set() has not
## been executed again, n is a valid matrix inverse cache
## Message is displayed to indicate it is a cached value
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
## If n is NULL, matrix is retrieved from input object using get()
## and matrix inverse is computed using solve() and assigned to n
## Function setmatinv() is called on the input object to set/"cache" 
## the matrix inverse on the input
        mat <- x$get()
        n <- solve(mat, ...)
        x$setmatinv(n)
## Returns the matrix inverse
        n
}
