
install.packages("lpSolve")
library(lpSolve)



#Setting the coefficients of decision variables
objective.in=c(169.99, 359.99, 289.99, 0.00)

#Constraint Matrix
const.mat=matrix(c(330,370,410,635,0.3,-0.7,-0.7,0.3,25,40,25,25/4),nrow = 3,byrow = T)
print(const.mat)

#defining constraints
const_money=170000
const_space= 82
const_size = 12300

#RHS for constraints
const.rhs=c(const_size, const_space, const_money)

#Direction for constraints
const.dir=c("<=","<=", "<=")


#Finding the optimum solution
opt=lp(direction = "max",objective.in,const.mat,const.dir,const.rhs)
summary(opt)

print(opt$status)

#Objective values of the items
opt$solution

#Value of objective function at optimal point
opt$objval
