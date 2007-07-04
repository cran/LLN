pause <- function(){readline(prompt="Press <Enter> to continue...");invisible()}

########### GraphClass package demonstration ###########

# Loading the data ##################################################

# Load the data
data(Monks)
Y <- Monks$Y; cls <- Monks$cls

# Random permutation of the data
ind <- sample(seq(1,nrow(Y)))
Y <- Y[ind,ind]; cls <- cls[ind]

# Split the data into a learning and a test set
learn <- seq(1,17)
test <- c(18)

pause()

# Learning ##################################################

# Learn the latent space associated to the network
pos <- latpos(Y[learn,learn],p=2,alpha=0)

# Build the supervised classifier (here LDA). The classifier can be any supervised classifier. You can also use QDA (MASS package) and SVM (e1071 package).
require('MASS')
c <- lda(pos$Z,cls[learn])

# Compute the correct classification rate on the learning dataset (should be good!)
tx1 <- sum( predict(c,pos$Z)$class == cls[learn]) / length(learn)
cat('* Classification result on the learning dataset:	',tx1,'\n')

pause()

# Classification ############################################

# Project the new nodes in the learned latent space
proj <- latproj(pos,Y,ind=test)

# Classify the new nodes using the classifier
res <- predict(c,proj$Z[test,])$class 

# Compute the correct classification rate on the learning dataset
tx2 <- sum(res == cls[test]) / length(test)
cat('* Classification resulton the test dataset:	',tx2,'\n')

pause()

# Visualization ############################################
split.screen(figs = c(2,2))

# Plot the network in the learned latent space
screen(1)
plot(x=pos$Z,prms=pos,cls=cls[learn])
title(main='Learned latent space',cex.main = 1)

# Plot the network and the supervised classifier
screen(2)
plot(x=pos$Z,prms=pos,cls=cls[learn],classifier=c)
title(main='Learned classifier',cex.main = 1)

# Project the new nodes in the latent space
screen(3)
plot(x=proj$Z,prms=proj,cls=cls,classifier=c,unlab=TRUE)
title(main='Projection of the new nodes',cex.main = 1)

# Final classification of the new nodes
screen(4)
plot(x=proj$Z,prms=proj,cls=cls,classifier=c,unlab=FALSE)
title(main='Classification of the new nodes',cex.main = 1)