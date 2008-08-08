##
## Computing and ploting a bpca object with 'rgl' package - 3d
##

library(bpca)
plot(bpca(gabriel1971, lambda.end=3),
     rgl.use=TRUE, var.factor=2)

# Suggestion: Interact with the graphic with the mouse
# left button: press, maintain and movement it to interactive rotation;
# right button: press, maintain and movement it to interactive zoom.
# Enjoy it!

