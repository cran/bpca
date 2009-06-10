##
## Computes: vector variable lengths, angles between vector variables and
## variable correlations from dataframe or matrix objects (n x p)
## n = rows (objects)
## p = columns (variables)
##

dt <- dt.tools(iris[-5], 2)

# Exploring the object 'bp' created by the function 'var.tools'
class(dt)
names(dt)
str(dt)

dt$length
dt$angle
dt$r
dt

# Checking the determinations
iris.tools <- round(dt.tools(iris[-5], var.center=TRUE)$r, 5); iris.tools
iris.obsv  <- round(cor(iris[-5]), 5); iris.obsv
all(iris.tools == iris.obsv)
