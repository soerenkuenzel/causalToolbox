library("OpenML")
## temporarily set API key to read only key
setOMLConfig(apikey = "6e7606dcedb2a6810d88dfaa550f7f07") # https://www.openml.org/u/3454#!api

datasets = listOMLDataSets()
dim(datasets)
head(datasets)
datasets = listOMLDataSets(limit = 10000)
dim(datasets)
datasets[datasets$name=='iris',]
# look at the dataset atn http://openml.org/d/61
# contains features about those data sets


tasks = listOMLTasks(limit = 100000)
dim(tasks)
head(tasks)
# contains a list of all those tasks.
table(tasks$task.type)
# 1864 regression problems


task = getOMLTask(task.id = 1L)

iris.data = getOMLDataSet(data.id = 61L)
iris.data$data
iris.data$colnames.old
iris.data$colnames.new
iris.data$target.features
