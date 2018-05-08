
library(datasets)

names(infert)

#  *------------------------------------------------------------------*
#  | train the network
#  *------------------------------------------------------------------* 

library(neuralnet)

nn <- neuralnet(
  case~age+parity+induced+spontaneous,
  data=infert, hidden=2, err.fct="ce",
  linear.output=FALSE)

#  *------------------------------------------------------------------*
#  | output training results
#  *------------------------------------------------------------------*  

# basic
nn

# reults options
names(nn)


# result matrix

nn$result.matrix

# The given data is saved in nn$covariate and
# nn$response as well as in nn$data for the whole data
# set inclusive non-used variables. The output of the
# neural network, i.e. the fitted values o(x), is provided
# by nn$net.result:

out <- cbind(nn$covariate,nn$net.result[[1]])

dimnames(out) <- list(NULL, c("age", "parity","induced","spontaneous","nn-output"))

head(out)

# generalized weights

# The generalized weight expresses the effect of each
# ovariate xi and thus has an analogous interpretation
# as the ith regression parameter in regression models.
# However, the generalized weight depends on all
# other covariates. Its distribution indicates whether
# the effect of the covariate is linear since a small variance
# suggests a linear effect

# The columns refer to the four covariates age (j =
# 1), parity (j = 2), induced (j = 3), and spontaneous (j=4)

head(nn$generalized.weights[[1]])

# visualization

plot(nn)