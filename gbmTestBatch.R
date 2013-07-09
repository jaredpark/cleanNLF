
decisionRule = c('abs')
chooseMin = F
chooseParsimonious = F
chooseParsOnlyByTrees = F # ignored if chooseParsimonious == T
runName = ''
rootDir = '~/GitHub/VM_share'
load(paste(rootDir, '/', runName, '/settings.Rout', sep = ''))
require('gbm')
source(paste(rootDir, '/funcs.R', sep = ''))
configurations = read.table(paste(rootDir, '/', runName, '/configMatrix.txt', sep = ''), header = T)

betDelta = settings$betDeltas
fpWeeks = 181:345
for (validateWindowSize in settings$validateWindowSizes){
  
  print(paste(validateWindowSize, 'validation weeks'))
  for (finalPredWeek in fpWeeks){
    print(paste('week', finalPredWeek, 'of', max(fpWeeks)))
    source(paste(rootDir, '/validation1.R', sep = ''))
  }
}

source('validation2.R')