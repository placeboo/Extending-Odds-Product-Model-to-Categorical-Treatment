# two inputs:
# x is a vector with increasing number, 
# the first number is smallest number; 
# the last number is the largest number;

# y is a number of a vector
# output: which category
makeGroup = function(x, y, category.name = "group"){
        
        nx = length(x)
        group = paste(category.name, 0:(nx-1), sep = "")
        
        out = rep(0, length(y))
        
        for (i in 1:length(y)){
                y_i = y[i]
                
                if (y_i == x[1]){ 
                        out[i] = group[2]
                }else if (y_i == x[nx]){
                        out[i] = group[nx]
                }else{
                        out[i] = group[y_i < x][1] # first truth
                }
        }
        return(out)
}
