
require(plyr)

# 2013
loadScans("E:/cg2013scans")
batchecks <- aaply(levels(scans$batch)[-1], .margin = 1, .fun = check.batch, harvestFile=hh.2013)
batchecks <- cbind(batchecks, levels(scans$batch)[-1])
colnames(batchecks)[6] <- "batchName"
batchecks <- batchecks[,c(6,1,2,3,4,5)]

write.csv(batchecks, file="C:/Users/dhanson/Documents/2013scanSummary.csv")

# 2014
loadScans("E:/cg2014scans")
batchecks <- aaply(levels(scans$batch)[-1], .margin = 1, .fun = check.batch, harvestFile=hh.2014)
batchecks <- cbind(batchecks, levels(scans$batch)[-1])
colnames(batchecks)[6] <- "batchName"
batchecks <- batchecks[,c(6,1,2,3,4,5)]

write.csv(batchecks, file="C:/Users/dhanson/Documents/2014scanSummary.csv")
