## makeCacheMatrix 

## Defines a cache memory able to contain a matrix and it's inverse matrix,
## apart from the cache four functions are also provided that permit the
## initialization and recovery of the stored data.

## Further details:

## This function requires a single argument:
##   'x' is the matrix to be cached.  

## The cache is able to retain a matrix and it's inverse matrix using two 
## internal variables.
##  'x' - used to store the matrix
##  'i' - used to store de inverse matrix of x 

## Returns a list containing the following four functions:

## 1. setMatrix - Stores into the cache the given matrix x
## 2. getMatrix - Returns the cached matrix x
## 3. setInverse - stores into the cache the inverse matrix  
## 4. getInverse - Returns the inverse matrix stored in the cache

## Author: Jose Maria Echevarria
## Date: 20150726

makeCacheMatrix <- function(x = matrix()) {
        
        ## Inverse matrix is set to NULL 
        i <- NULL
        
        ## setMatrix allows to initialize the cache with the given matrix.
        ## the value of the inverse matrix is set to NULL as it hasn't been
        ## previously computed.
        setMatrix <- function (y) {
               x <<- y 
               i <<- NULL
        }
        
        ## getMatrix returns the matrix stored in cache
        getMatrix <- function () x
        
        ## setInverse sets the inverse matrix passed by argument to the function
        setInverse <- function (inverseMatrix) i <<- inverseMatrix
        
        ## getInverse returns the inverse matrix or NULL if it hasn't been 
        ## previously computed and stored.
        getInverse <- function () i
        
        # Definition of the functions to construct
        list(setMatrix = setMatrix, getMatrix = getMatrix,
            setInverse = setInverse,
            getInverse = getInverse)
}

##cacheSolve

## This function computes the inverse matrix of the one stored in the cache
## element provided by argument. If the inverse matrix has already been
## computed this will automatically be returned, otherwise, it will be computed
## and returned afterwards.

## Further details:

## This function takes a single argument:
##   'x' is a cache object created using the function makeCacheMatrix.  

## The function returns the inverse matrix of the matrix provided by argument

## Author: Jose Maria Echevarria
## Date: 20150726

cacheSolve <- function(x, ...) {
        
        ## The inverse matrix is read form cache
        i <- x$getInverse()
        
        ## if the inverse matrix has already been computed, the variable i will
        ## contain it's value and this will be the return value of the function
        if(!is.null(i)){
                message("Reading cached Inverse Matrix")
                return(i)
        }
        
        ## the stored matrix is read from the provided cache and it's inverse
        ## matrix is computed using the function solve.
        matrix <- x$getMatrix()
        i <- solve(matrix)
        
        ## sets into cache the value of the computed inverse matrix 
        x$setInverse(i)
        
        ## Returns a matrix that is the inverse of 'x'
        i
}
