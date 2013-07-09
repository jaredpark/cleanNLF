runName = 't2'
rootDir = '~/GitHub/NFL/nflProject'
source(paste(rootDir, '/funcs.R', sep = ''))
gameInfoDate = 'Mar18'
gameInfoDir = paste(rootDir, '/data_objects', sep = '')
mmDate = 'Mar18'
mmDir = paste(rootDir, '/data_objects', sep = '')

if (!file.exists(paste(rootDir, '/', runName, sep = ''))){
  dir.create(paste(rootDir, '/', runName, sep = ''))
}

if (!file.exists(paste(rootDir, '/', runName, '/gameInfo.Rout', sep = ''))){
  file.copy(from = paste(gameInfoDir, '/gameInfo_', gameInfoDate, '.Rout', sep = ''), 
            to = paste(rootDir, '/', runName, '/gameInfo.Rout', sep = ''))
}

if (!file.exists(paste(rootDir, '/', runName, '/MM.Rout', sep = ''))){
  file.copy(from = paste(mmDir, '/MM_', mmDate, '.Rout', sep = ''), 
            to = paste(rootDir, '/', runName, '/MM.Rout', sep = ''))
}

fitWindowSizes = seq(10, 30, by = 10)
validateWindowSizes = seq(4, 6, by = 2)

validateWeeks = 5:15
validateSeas = 2003:2009

load(paste(rootDir, '/', runName, '/MM.Rout', sep = ''))
for (size in fitWindowSizes){
  folds = MMFoldIndexList(MM, validateWeeks, validateSeas, numWeeks = size)
  save(folds, file = paste(rootDir, '/', runName, '/', 'foldIndexFitWindow', size, '.Rout', sep = ''))
}

finalPredWeeks = 5:15
finalPredSeas = 2006:2009

numTrees = seq(100, 1000, by = 300)

betDeltas = seq(.5, 2, .5)

settings = list('fitWindowSizes' = fitWindowSizes, 'vWeeks' = validateWeeks, 'vSeas' = validateSeas, 
                'validateWindowSizes' = validateWindowSizes, 'finalPredWeeks' = finalPredWeeks, 'finalPredSeas' = finalPredSeas,
                'numTrees' = numTrees, 'betDeltas' = betDeltas)

if (!file.exists(paste(rootDir, '/', runName, '/settings.Rout', sep = ''))){
  save(settings, file = paste(rootDir, '/', runName, '/settings.Rout', sep = ''))
}

maxNumTrees = max(numTrees)
interactionDepth = seq(1, 2, by = 1)
minObsPerNode = seq(8, 16, by = 8)

if (!file.exists(paste(rootDir, '/', runName, '/configMatrix', sep = ''))){
  gbmConfigMatrix = expand.grid('intDepth' = interactionDepth, 'minObs' = minObsPerNode)
  write.table(gbmConfigMatrix, file = paste(rootDir, '/', runName, '/configMatrix.txt', sep = ''), row.names = F)
}

if (!file.exists(paste(rootDir, '/', runName, '/fits', sep = ''))){
  dir.create(paste(rootDir, '/', runName, '/fits', sep = ''))
}

if (!file.exists(paste(rootDir, '/', runName, '/predictions', sep = ''))){
  dir.create(paste(rootDir, '/', runName, '/predictions', sep = ''))
}

if (!file.exists(paste(rootDir, '/', runName, '/validation', sep = ''))){
  dir.create(paste(rootDir, '/', runName, '/validation', sep = ''))
}

if (!file.exists(paste(rootDir, '/', runName, '/finalRules', sep = ''))){
  dir.create(paste(rootDir, '/', runName, '/finalRules', sep = ''))
}