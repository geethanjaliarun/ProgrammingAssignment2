## This R file contains two functions.
## makeCacheMatrix which caches the matrix and its invers
## cacheMatrix which returns the cached inverse - if the inverse has been cached,
## else calculates the inverse and caches it.

## makeCacheMatrix : This function caches the values of the matrix and its inverse.
## Returns a list of four functions:
##           i) set() - Sets the value of the matrix
##          ii) get() - Gets the value of the matrix
##         iii) getinverse() - Gets the value of the inverse
##          iv) setinverse() - Sets the value of the inverse
## Arguments - Matrix - x whose inverse is to be cached

makeCacheMatrix <- function(x = matrix())
{
        #Sets the inverse to NULL
        inverse <- NULL
        
        #Set function which sets the value of x and changes the cached inverse value to NULL.
        set <- function(y = matrix())
        {
                x <<- y
                inverse <<- NULL
        }
        
        #Returns the value of x
        get <- function()
        {
                x
        }
        #Sets the inverse
        setinverse <- function(inv = matrix())
        {
                inverse <<- inv 
        }
        #Returns the inverse
        getinverse <<- function() 
        {
                inverse
        }
        
        list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## cacheSolve - This function returns the inverse of the matrix if its inverse is cached,
##              else calculates the inverse and stores it.
##              Returns the inverse
##              Parameters - Takes in the list returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) 
{
        ## Gets the inverse
        inverse = x$getinverse()
        
        ##If the returned inverse is not null, returns it.
        if(!is.null(inverse))
        {
                message("Fetching inverse")
                return(inverse)
        }
        
        ##The returned inverse is null calulates it and saves it.
        message("Calculating and saving inverse")
        
        ##Gets the matrix
        x_matrix = x$get()
        
        ##Finds the inverse
        inverse = solve(x_matrix)
        
        ##Sets the inverse
        x$setinverse(inverse)
        
        ##Returns the same
        return(inverse)
}
