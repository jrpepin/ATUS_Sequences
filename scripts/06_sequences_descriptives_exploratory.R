# subset data by cluster

## Cluster 1 -----------------------------------------------------------------------------------------------------------------------
cl1<-(seqdata.seq[seqdata$hcm ==  "A",])

# plot the selected cluster 
par(mfrow=c(1,1))
seqdplot(cl1, main = "",
         cex.main = 1.7, 
         with.legend = FALSE, 
         yaxis = FALSE, 
         ylab = "",
         border = NA)

gentime4_seq <- seqgranularity(cl1,
                               tspan=4, method="mostfreq")

seqdplot(gentime4_seq, border = NA, with.legend = "right", legend.prop=0.4) # Plot Cluster 1
seqplot(gentime4_seq, type="ms", with.legend = "right", legend.prop=0.4, main="Modal Sequences") # Plot Modal Sequences of Cluster 1


## Cluster 2 -----------------------------------------------------------------------------------------------------------------------
cl2<-(seqdata.seq[seqdata$hcm ==  "B",])

# plot the selected cluster 
par(mfrow=c(1,1))
seqdplot(cl2, main = "",
         cex.main = 1.7, 
         with.legend = FALSE, 
         yaxis = FALSE, 
         ylab = "",
         border = NA)

gentime4_seq <- seqgranularity(cl2,
                               tspan=4, method="mostfreq")

seqdplot(gentime4_seq, border = NA, with.legend = "right", legend.prop=0.4) # Plot Cluster 1
seqplot(gentime4_seq, type="ms", with.legend = "right", legend.prop=0.4, main="Modal Sequences") # Plot Modal Sequences of Cluster 1


