## **************************************************************************
## The makeCacheMatrix and cacheSolve functions work in conjunction 
## to compute, given a suitable data matrix, the inverted matrix and to store
## the result in a cache. If an inverted matrix is to be computed again
## on the same data matrix, the cached result will be returned and not be
## recomputed.
##***************************************************************************

## makeCacheMatrix:
## Description: stores the data matrix and the inverted matrix
## Inputs: a data matrix, defaulted to an empty matrix
## Outputs: a list of functions/methods to do the following
##              1) setDataMatrix: set a new data matrix
##              2) getDataMatrix: retrieves the stored data matrix 
##              3) setInvertedMatrix: set the inverted matrix
##              4) getInvertedMatrix: retrieves the cached inverted matrix

makeCacheMatrix <- function(x = matrix()) {
        
        invertedMatrix <- NULL
        
        # setMatrix function: set new matrix  object to be inverted.
        setDataMatrix <- function(y) {
                
                x <<- y # assign the new matrix 'y' to the existing matrix 'x'
                        # in the PARENT environment.
                
                invertedMatrix <<- NULL # assigning a new matrix means the 
                                        # inverted matrix is reset to NULL.
        }
        
        #getMatrix function: returns the matrix object to be inverted.
        getDataMatrix <- function() x
        
        #setInvertedMatrix function: sets the invertedMatrix variable to be 
        #                            inverted matrix computed.
        setInvertedMatrix <- 
                function(extInvertedMatrix) invertedMatrix <<- extInvertedMatrix
        
        #getInvertedMatrix function: returns the cached invertedMatrix.
        getInvertedMatrix <- function() invertedMatrix
        
        #return the list comprising the above functions
        
        list(setDataMatrix = setDataMatrix,
             getDataMatrix = getDataMatrix,
             setInvertedMatrix = setInvertedMatrix,
             getInvertedMatrix = getInvertedMatrix)
        
}


## cacheSolve:
## Description: Calculates the inverted matrix and caches the result if an 
##              inverted matrix is not already cached. Otherwise returns
##              the cached inverted matrix.
## Inputs : list of methods from makeCacheMatrix and arguments list for 
##          solve function.
## Outputs: the inverted matrix

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        invertedMatrix <- x$getInvertedMatrix() # assign the inverted matrix.
        
        #if the inverted matrix exists then return it as the cached data.
        if(!is.null(invertedMatrix)){
                
                message ("Getting cached inverted matrix")
                return(invertedMatrix) # returns the cached data and exist 
                                       # the cacheSolve function
        }
        
        #if the inverted matrix is not cached then need to retrieve data and 
        #compute the inverted matrix
        
        dataMatrix <- x$getDataMatrix() # assign the matrix data to use
        
        invertedMatrix <- solve(dataMatrix, ...) # compute inverted matrix
        x$setInvertedMatrix(invertedMatrix) # set the computed inverted matrix
                                            # to the cache
        
        invertedMatrix # return the inverted matrix
}
