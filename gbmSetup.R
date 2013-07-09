runName = ''
rootDir = '~/GitHub/VM_share'
mmDate = 'May16'
fitWindowSizes = c(36, 60, 96, 120, 144)
validateWindowSizes = c(30) #seq(5, 30, by = 5)

betDeltas = c(.5)

validateWeeks = 2:16
validateSeas = 1986:2009

numTrees = seq(200, 800, by = 50)
maxNumTrees = max(numTrees)
interactionDepth = seq(1, 3, by = 1)
minObsPerNode = 10

shrinkage = .01

excludedColumns = c()

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

# file.copy(from = paste(rootDir, '/submitValidation1Jobs.sh', sep = ''), 
#             to = paste(rootDir, '/', runName, '/submitValidation1Jobs.sh', sep = ''))
# file.copy(from = paste(rootDir, '/validation1.R', sep = ''), 
#             to = paste(rootDir, '/', runName, '/validation1.R', sep = ''))
# file.copy(from = paste(rootDir, '/validation2.R', sep = ''), 
#             to = paste(rootDir, '/', runName, '/validation2.R', sep = ''))
# file.copy(from = paste(rootDir, '/gbmSetup.R', sep = ''), 
#             to = paste(rootDir, '/', runName, '/gbmSetup.R', sep = ''))

if (!file.exists(paste(rootDir, '/', runName, '/MM.Rout', sep = ''))){
  file.copy(from = paste(mmDir, '/MM_', mmDate, '.Rout', sep = ''), 
            to = paste(rootDir, '/', runName, '/MM.Rout', sep = ''))
}

# load(paste(rootDir, '/', runName, '/MM.Rout', sep = ''))
# for (size in fitWindowSizes){
#   folds = MMFoldIndexList(MM, validateWeeks, validateSeas, numWeeks = size)
#   save(folds, file = paste(rootDir, '/', runName, '/', 'foldIndexFitWindow', size, '.Rout', sep = ''))
# }

settings = list('name' = runName, 'fitWindowSizes' = fitWindowSizes, 'vWeeks' = validateWeeks, 'vSeas' = validateSeas, 
                'validateWindowSizes' = validateWindowSizes, 'numTrees' = numTrees, 'betDeltas' = betDeltas,
                'shrinkage' = shrinkage, 'excludedColumns' = excludedColumns)

# write.table(c(fitWindowSizes), file = paste(rootDir, '/', runName, '/fwSizes.txt', sep = ''), col.names = F, row.names = F)
# write.table(c(validateWindowSizes), file = paste(rootDir, '/', runName, '/vwSizes.txt', sep = ''), col.names = F, row.names = F)
# 
# lastFitWeek = length(settings$vSeas)*length(settings$vWeeks)
# write.table(lastFitWeek, file = paste(rootDir, '/', runName, '/fitWeekRange.txt', sep = ''), col.names = F, row.names = F)
# 
# write.table(runName, file = paste(rootDir, '/', runName, '/runName.txt', sep = ''), col.names = F, row.names = F)
# write.table(rootDir, file = paste(rootDir, '/', runName, '/rootDir.txt', sep = ''), col.names = F, row.names = F)

save(settings, file = paste(rootDir, '/', runName, '/settings.Rout', sep = ''))

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