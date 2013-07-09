runName = 't11'
rootDir = '~/GitHub/NFL/nflProject'
setwd(rootDir)
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

fitWindowSizes = c(84,96)#seq(48, 60, by = 24)
validateWindowSizes = c(6,12)#seq(12, 36, by = 12)

betDeltas = c(1)

validateWeeks = 5:16
validateSeas = 2001:2010

load(paste(rootDir, '/', runName, '/MM.Rout', sep = ''))
for (size in fitWindowSizes){
  folds = MMFoldIndexList(MM, validateWeeks, validateSeas, numWeeks = size)
  save(folds, file = paste(rootDir, '/', runName, '/', 'foldIndexFitWindow', size, '.Rout', sep = ''))
}

numTrees = seq(300, 700, by = 100)
maxNumTrees = max(numTrees)
interactionDepth = seq(1,2, by = 1)
minObsPerNode = 10

shrinkage = .01

excludedColumns = unique(c(#seq(5, 3484, by = 15), # all
                         #seq(6, 3484, by = 15), # 1
                         #seq(7, 3484, by = 15), # 2
                         #seq(8, 3484, by = 15), # 3
                         #seq(9, 3484, by = 15), # 4    
                         seq(10, 3484, by = 15),
                         #seq(11, 3484, by = 15), 
                         #seq(12, 3484, by = 15),
                         #seq(13, 3484, by = 15), #8
                         seq(14, 3484, by = 15),
                         seq(15, 3484, by = 15),
                         seq(16, 3484, by = 15),
                         seq(17, 3484, by = 15),
                         seq(18, 3484, by = 15),
                         seq(19, 3484, by = 15)))


settings = list('name' = runName, 'fitWindowSizes' = fitWindowSizes, 'vWeeks' = validateWeeks, 'vSeas' = validateSeas, 
                'validateWindowSizes' = validateWindowSizes, 'numTrees' = numTrees, 'betDeltas' = betDeltas,
                'shrinkage' = shrinkage, 'excludedColumns' = excludedColumns)

if (!file.exists(paste(rootDir, '/', runName, '/settings.Rout', sep = ''))){
  save(settings, file = paste(rootDir, '/', runName, '/settings.Rout', sep = ''))
}

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