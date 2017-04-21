load("crashes/crash1.RData")
set.seed(1234)
library(hte)
(x = x)
(y = y)
(ntree = r_old)
(mtry = allConfigs$mtry[j])
(nodesizeSpl = allConfigs$min_node_size_spl[j])
(nodesizeAvg = allConfigs$min_node_size_ave[j])
(splitratio = allConfigs$splitratio[j])
(replace = allConfigs$replace[j])
(sampsize = sampsize)
(nthread = nthread)
(middleSplit = allConfigs$middleSplit[j])

honestRF(
  x = x,
  y = y,
  ntree = r_old,
  mtry = allConfigs$mtry[j],
  nodesizeSpl = allConfigs$min_node_size_spl[j],
  nodesizeAvg = allConfigs$min_node_size_ave[j],
  splitratio = allConfigs$splitratio[j],
  replace = allConfigs$replace[j],
  sampsize = sampsize,
  nthread = nthread,
  middleSplit = allConfigs$middleSplit[j]
)
