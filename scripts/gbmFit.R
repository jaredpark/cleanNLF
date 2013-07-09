# fitWindowSize = 30
# validationWeek = 100
if (firstRun){
  betDelta = settings$delta
  
  colExclude = settings$excludedColumns  
  
  runName = settings$name
  rootDir = '~/GitHub/NFL/nflProject'
  require('gbm')
  source(paste(rootDir, '/funcs.R', sep = ''))
  
  load(paste(rootDir, '/', runName, '/settings.Rout', sep = ''))
  load(paste(rootDir, '/', runName, '/MM.Rout', sep = ''))
  configurations = read.table(paste(rootDir, '/', runName, '/configMatrix.txt', sep = ''), header = T)
}
# load(paste(rootDir, '/', runName, '/foldIndexFitWindow', fitWindowSize, '.Rout', sep = ''))

trainIndex = folds[[1]][[validationWeek]]
trainData = MM[trainIndex, -colExclude]
## added 5/15
trainData = trainData[, -which(is.element(colnames(trainData), c('Season', 'Week')))]
## end add 5/15
trainData = dropColWithNA(trainData, 'all')
response = trainData$Response
trainData = trainData[,-which(colnames(trainData) == 'Response')]

for (configNum in 1:nrow(configurations)){
  intDepth = configurations$intDepth[configNum]
  minObs = configurations$minObs[configNum]
  fit = gbm.fit(trainData, response, distribution = 'gaussian', shrinkage = settings$shrinkage, verbose = F,
                interaction.depth = intDepth, n.minobsinnode = minObs, n.trees = max(settings$numTrees),
                keep.data = F)
#   fit = gbm(Response ~ ., data = trainData, distribution = 'gaussian', shrinkage = settings$shrinkage, verbose = T,
#               interaction.depth = intDepth, n.minobsinnode = minObs, n.trees = max(settings$numTrees))
  save(fit, file = paste(rootDir, '/', runName, '/fits', '/gbmFit', 'FW', fitWindowSize, 'week', validationWeek, 
                         'config', configNum, '.Rout', sep = ''))
}