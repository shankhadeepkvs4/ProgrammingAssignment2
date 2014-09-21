# Programming Assignment 2

# This function takes a Matrix only as a Input and returns if the inverse exist in the cache memory
# or sets the inverse in the cache memory if the inverse doesnot exist.

makeCacheMatrix <- function(x = matrix()) 
{                                                      
        inverse <- NULL                  
        
        set <- function(y)                      #assigns the matrix to the variable "x".
        {
                x <<- y
                inverse <<- NULL                #initialise the variable "inverse"
        }
  
        get <- function()       x               #returns the given matrix
  
        set_inverse <- function(answer)
        {         
                inverse <<- answer              #Sets the inverse returned from func "cacheSolve" in the cache memory. 
        }
  
        get_inverse <- function() 
        {                
            inverse                             #Returns the Inverse of the matrix stored in Cache.
        }
}


# This function pulls the Inverse from the above function if Inverse is in the Cache memory
# or else Calculates the Inverse and returns it to the above function to store in the cache memory.

cacheSolve <- function(x, ...) 
{        
        answer <- x$get_inverse()               #Pulls the Inverse form the Cache memory      
  
        if(!is.null(answer))                    #Checks if the Inverse of the matrix already exists in the Cache
        {                   
                message("getting cached data")     
                return(answer)                  #returns the Inverse if it exists      
        }
  
        data <- x$get()                         #Gets the original matrix for further Computation          
        
        if(nrow(data) != ncol(data))            #Checks whether its a Square Matrix.
        {
                error <- message("Sorry! Error Calculating Inverse. \n
                                Given matrix is not a SQUARE matrix.")
                return(error)                   #Terminates the code; returns error msg if the its not a Square matrix
        }
        
        determinant <- det(data)                #This calculates the determinant of the Matrix
        
        if(determinant == 0)                    #Checks whether the matrix is Non-Singular i.e Invertible.
        {
                error <- message("Sorry! Error Calculating Inverse. \n
                                Given matrix is Not Invertible (Singular).")
                return(error)                   #Terminates the code;return error msg if its Not Invertible matrix.
        }
        
        answer <- solve(data, ...)              #Calculates the Inverse of the Matrix in the variable "answer"
        
        x$set_inverse(answer)                   #Sets the inverse matrix in the cache memory for further use.
        
        inverse                                  
}
