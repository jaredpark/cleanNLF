# fitWindowSize = 30
# validationWeek = 100

if (firstRun){
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
  
validateIndex = folds[[2]][[validationWeek]]
validateData = MM[validateIndex, -colExclude]
validateData = dropColWithNA(validateData, 'all')
homeLine = validateData$HomePtSpread
response = validateData$Response
validateData = validateData[, -which(colnames(validateData) == 'Response')]

for (config in 1:nrow(configurations)){
  intDepth = configurations$intDepth[config]
  minObs = configurations$minObs[config]
  
  load(paste(rootDir, '/', runName, '/fits', '/gbmFit', 'FW', fitWindowSize, 
             'week', validationWeek, 'config', config, '.Rout', sep = ''))
  pred = predict.gbm(fit, newdata = validateData, n.trees = settings$numTrees)
  
  treeCounter = 1 
  
  for (trees in settings$numTrees){
    out = cbind(pred = pred[,treeCounter], line = homeLine, actual = response)
    write.table(out, file = paste(rootDir, '/', runName, '/predictions', '/gbmPred', 'FW', fitWindowSize, 'week', validationWeek, 
                                  'config', config, 'nTrees', trees, '.txt', sep = ''))
    treeCounter = treeCounter + 1
  }
}