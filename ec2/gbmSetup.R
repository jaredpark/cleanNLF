runName = 'clusterTest1'
rootDir = '/home/jared'
mmDate = 'Mar18'
fitWindowSizes = seq(24, 48, by = 24)
validateWindowSizes = seq(12, 36, by = 12)

betDeltas = c(1)

validateWeeks = 5:16
validateSeas = 1994:2003

numTrees = seq(300, 700, by = 100)
maxNumTrees = max(numTrees)
interactionDepth = seq(1,2, by = 1)
minObsPerNode = 10

shrinkage = .01

excludedColumns = unique(c(#seq(5, 3484, by = 15), # all
                         #seq(6, 3484, by = 15), # 1
                         #seq(7, 3484, by = 15), # 2
                         seq(8, 3484, by = 15), # 3
                         #seq(9, 3484, by = 15), # 4    
                         seq(10, 3484, by = 15),
                         seq(11, 3484, by = 15), 
                         seq(12, 3484, by = 15),
                         #seq(13, 3484, by = 15), #8
                         seq(14, 3484, by = 15),
                         seq(15, 3484, by = 15),
                         seq(16, 3484, by = 15),
                         seq(17, 3484, by = 15),
                         seq(18, 3484, by = 15),
                         seq(19, 3484, by = 15)))

source(paste(rootDir, '/funcs.R', sep = ''))
gameInfoDate = mmDate
gameInfoDir = rootDir
mmDir = rootDir

if (!file.exists(paste(rootDir, '/', runName, sep = ''))){
  dir.create(paste(rootDir, '/', runName, sep = ''))
}

if (!file.exists(paste(rootDir, '/', runName, '/gameInfo.Rout', sep = ''))){
  file.copy(from = paste(gameInfoDir, '/gameInfo_', gameInfoDate, '.Rout', sep = ''), 
            to = paste(rootDir, '/', runName, '/gameInfo.Rout', sep = ''))
}

if (!file.exists(paste(rootDir, '/', runName, '/submitPredJobs.sh', sep = ''))){
  file.copy(from = paste(rootDir, '/submitPredJobs.sh', sep = ''), 
            to = paste(rootDir, '/', runName, '/submitPredJobs.sh', sep = ''))
}

if (!file.exists(paste(rootDir, '/', runName, '/submitFitJobs.sh', sep = ''))){
  file.copy(from = paste(rootDir, '/submitFitJobs.sh', sep = ''), 
            to = paste(rootDir, '/', runName, '/submitFitJobs.sh', sep = ''))
}

if (!file.exists(paste(rootDir, '/', runName, '/MM.Rout', sep = ''))){
  file.copy(from = paste(mmDir, '/MM_', mmDate, '.Rout', sep = ''), 
            to = paste(rootDir, '/', runName, '/MM.Rout', sep = ''))
}

load(paste(rootDir, '/', runName, '/MM.Rout', sep = ''))
for (size in fitWindowSizes){
  folds = MMFoldIndexList(MM, validateWeeks, validateSeas, numWeeks = size)
  save(folds, file = paste(rootDir, '/', runName, '/', 'foldIndexFitWindow', size, '.Rout', sep = ''))
}

settings = list('name' = runName, 'fitWindowSizes' = fitWindowSizes, 'vWeeks' = validateWeeks, 'vSeas' = validateSeas, 
                'validateWindowSizes' = validateWindowSizes, 'numTrees' = numTrees, 'betDeltas' = betDeltas,
                'shrinkage' = shrinkage, 'excludedColumns' = excludedColumns)

write.table(c(fitWindowSizes), file = paste(rootDir, '/', runName, '/fwSizes.txt', sep = ''), col.names = F, row.names = F)
write.table(c(validateWindowSizes), file = paste(rootDir, '/', runName, '/vwSizes.txt', sep = ''), col.names = F, row.names = F)
fitWeekRange = (max(settings$fitWindowSizes) + 1):(length(settings$vWeeks)*length(settings$vSeas))
write.table(c(fitWeekRange), file = paste(rootDir, '/', runName, '/fitWeekRange.txt', sep = ''), col.names = F, row.names = F)
finalPredWeekRange = fpWeeks = (max(settings$validateWindowSizes) + max(settings$fitWindowSizes)+ 1):(length(settings$vSeas)*length(settings$vWeeks))
write.table(c(finalPredWeekRange), file = paste(rootDir, '/', runName, '/finalPredWeekRange.txt', sep = ''), col.names = F, row.names = F)
write.table(runName, file = paste(rootDir, '/', runName, '/runName.txt', sep = ''), col.names = F, row.names = F)
write.table(rootDir, file = paste(rootDir, '/', runName, '/rootDir.txt', sep = ''), col.names = F, row.names = F)

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

if (!file.exists(paste(rootDir, '/', runName, '/jobScripts', sep = ''))){
  dir.create(paste(rootDir, '/', runName, '/jobScripts', sep = ''))
}