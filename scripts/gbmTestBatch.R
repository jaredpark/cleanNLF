source('gbmSetup.R')

firstRun = T
for (fitWindowSize in settings$fitWindowSizes){
  print(paste('beginning making rules for fit window size ', fitWindowSize, sep = ''))
  fitRange = (max(settings$fitWindowSizes) + 1):(length(settings$vWeeks)*length(settings$vSeas))
  load(paste(rootDir, '/', runName, '/foldIndexFitWindow', fitWindowSize, '.Rout', sep = ''))
  for (validationWeek in fitRange){
    print(paste('week ', validationWeek, ' of ', max(fitRange), sep = ''))
    source('gbmFit.R')
    firstRun = F
  }
}

firstRun = T
for (fitWindowSize in settings$fitWindowSizes){
  print(paste('beginning making predictions for fit window size ', fitWindowSize, sep = ''))
  fitRange = (max(settings$fitWindowSizes) + 1):(length(settings$vWeeks)*length(settings$vSeas))
  load(paste(rootDir, '/', runName, '/foldIndexFitWindow', fitWindowSize, '.Rout', sep = ''))
  for (validationWeek in fitRange){
    print(paste('week ', validationWeek, ' of ', max(fitRange), sep = ''))
    source('gbmPredict.R')
    firstRun = F
  }
}

decisionRule = c('classErr', 'rmse')
runName = ''
rootDir = '~/GitHub/VM_share'
load(paste(rootDir, '/', runName, '/settings.Rout', sep = ''))
require('gbm')
source(paste(rootDir, '/funcs.R', sep = ''))
configurations = read.table(paste(rootDir, '/', runName, '/configMatrix.txt', sep = ''), header = T)

for (validateWindowSize in settings$validateWindowSizes){
  
  fpWeeks = (max(settings$validateWindowSizes) + max(settings$fitWindowSizes)+ 1):(length(settings$vSeas)*length(settings$vWeeks))
  print(paste(validateWindowSize, 'validation weeks'))
  for (finalPredWeek in fpWeeks){
    print(paste('week', finalPredWeek, 'of', max(fpWeeks)))
    source(paste(rootDir, '/validation1.R', sep = ''))
  }
}

source('validation2.R')