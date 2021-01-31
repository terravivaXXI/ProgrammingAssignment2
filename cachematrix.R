## In general, this pair of functions will take a matrix x as an input and return the inverse of 
# such matrix with the condition that if you had already calculated the inverse previously
# it will not be calculated again but retrieved from memory

## To do this the program creates two functions, one (makeCacheMatrix) that is quite unnatural to understand, and 
# and another one (cacheSolve) that is based on the first one

## The first function makeCacheMatrix takes a matrix (if you do not feed anything to it, will take a void matrix)
# First of all we create the variable inv (empty for now) that will store the inverse of the matrix
# The function set takes its argument and sets it to replace the matrix that was stored before
# Set also restores the variable (the memorized inverted matrix) to null
# Then we create three functions: 
 # get returns the matrix that is currently in memory
 # setinverse stores its argument in the value inv (the idea is that its argument is actually the inverse of the matrix currently in memory)
 # getinverse will retrieve the value of inv
## We will store all these functions in an unnamed list that can be accesed through the name makeCacheMatrix
## On top of the functions in the list, makeCacheMatrix initiates to memory the variable inv and a matrix (that can be called using get)

makeCacheMatrix <- function(x = matrix()) {# Initiating of the element x which is a void matrix by default
        inv <- NULL             # Setting the value of the inverse matrix to NULL
        set <- function (newmatrix) { # Function makeCacheMatrix$set will reset the value of x to whatever "newmatrix" is
                x <<- newmatrix # We are changing the value of x, note that our cache is no longer valid
                inv <<- NULL # Reset the value of inv, as x has changed and we no longer have a valid inverse matrix
        }
        get <- function() x # whenever we call the makeCacheMatrix$get we load the value of x that is stored in immediate environment
        setinverse <- function(inverse) inv <<- inverse # this function will take whatever you give it and store it in immediate env
        getinverse <- function() inv # retrieves inv from immediate environment
        list(set = set, get = get, # forms a list with all the functions 
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function depends on makeCacheMatrix being initiated
# First thing we do is loading the variable inv that was defined by makeCacheMatrix
# a) If inv is not null, we print it, as it must have been calculated before
# b) If inv is null, we proceed to calculating it by applying function solve to our current matrix and update the value of inv inside 
# the list created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() 
        if (!is.null(inv)){
                message("the inverse is in cache, it is being retrieved...")
                return(inv)
        }
        currentMatrix <- x$get()
        inv <- solve(currentMatrix)
        x$setinverse(inv)
        inv
       
}

## Testing
firstMatrix <- makeCacheMatrix(matrix(1:16, nrow = 4, ncol=4))
cacheSolve(firstMatrix)
