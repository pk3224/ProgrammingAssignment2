## Put comments here that give an overall description of what your
## functions do
## The following is written as part of 'Coursera/Johns Hopkins/Data Science/-
## R Programming' unit for week 3 assignment; by Pavan Khimesra (Github user: pk3224)
## to demonstrate my understanding of lexical scoping.

## Write a short comment describing this function
## Below function creates an invertible matrix and caches/stores it in memory via the initialization object 'x' and 'inv'
makeCacheMatrix <- function(x = matrix()) { ## Specifies argument as matrix class
        inv <- NULL                         ## Initialize 'inv' as NULL that will store the value of matrix inverse.
        set <- function(y) {                ## 'Set' function use the super assignment operator to point the value-
                x <<- y                     ## of argument 'y' into object 'x' (i.e. the matrix). Object 'x' will be accessible in the parent environment.
                inv <<- NULL                ## Object 'ínv' value is reset to NULL.
        }
        get <- function() {x}               ## Define 'get' function retrieves 'x' (i.e the matrix object)
        setInverse <- function(inverse) {inv <<- inverse}  ## Defines 'setInverse' function to super assign the inverse matrix to object 'inv', which is accessible in the parent environment.
        getInverse <- function() {inv}      ## Defines 'getInverse' function to retrieve the 'inv' object 
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) ## The 'list' function allows for the return of functions above to the parent environment so they can- 
        ## used with the $ extraction operator in the following function.
}


## Write a short comment describing this function
## This function calculate the inverse of the above matrix if it has not already been computed and cached.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("Getting cached data") ## Exits function and returns 'inv' to parent environment-
                return(inv)                    ## and notifies user that 'inv' object exists.
        }
        ## The following section calculates the inverse using the 'solve' function because it did not exist previously in cache.
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        inv ## Returns this last line's object (i.e. the recently calculated inverse of matrix 'x') to the parent environment.
}
