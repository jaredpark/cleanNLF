plotIt = T
rootDir = '~/GitHub/NFL/nflProject'
runName = settings$name
source(paste(rootDir, '/funcs.R', sep = ''))
decisionRule = 'rmse'

load(paste(rootDir, '/', runName, '/settings.Rout', sep = ''))
numValidateWeeks = length(settings$vWeeks)*length(settings$vSeas)
fpWeeks = (max(settings$validateWindowSizes) + max(settings$fitWindowSizes) + 1):(length(settings$vSeas)*length(settings$vWeeks))

vWindowSizesSummStats = NULL

for (vWindowSize in settings$validateWindowSizes){
  
  vWindowSizePerf = NULL
  for (fpWeek in fpWeeks){
    FPweekPerf = t(read.table(file = paste(rootDir, '/', runName, '/validation/rulePerfFinalPredWeek', fpWeek,
                                           'vWindow', vWindowSize, '.txt', sep = '')))
    vWindowSizePerf = rbind(vWindowSizePerf, FPweekPerf)
  }
  vWindowSizeSummStat = makeSummStats(vWindowSizePerf)
  vWindowSizesSummStats = rbind(vWindowSizesSummStats, vWindowSizeSummStat)
}
print(vWindowSizesSummStats)
whichBest = pickRule(vWindowSizesSummStats, decisionRule)
bestVWindowSize = settings$validateWindowSizes[whichBest]

counter = 1
err = c(); adjErr = c(); rmse = c(); lineRmse = c()
for (fpWeek in fpWeeks){
  fpWeekPerf = read.table(paste(rootDir, '/', runName, '/validation/rulePerfFinalPredWeek', fpWeek,
                                'vWindow', bestVWindowSize, '.txt', sep = ''))
  err[counter] = fpWeekPerf['classErr',]
  adjErr[counter] = fpWeekPerf['adjClassErr',]
  rmse[counter] = fpWeekPerf['rmse',]
  lineRmse[counter] = fpWeekPerf['lineRmse',]
  counter = counter + 1  
  file.copy(paste(rootDir, '/', runName, '/validation/rulePerfFinalPredWeek', fpWeek,
                  'vWindow', bestVWindowSize, '.txt', sep = ''),
            paste(rootDir, '/', runName, '/finalRules/FPweek', fpWeek,
                  'vWindow', bestVWindowSize, 'performance.txt', sep = ''))
  file.copy(paste(rootDir, '/', runName, '/validation/ruleConfigFinalPredWeek', fpWeek,
                  'vWindow', bestVWindowSize, '.txt', sep = ''),
            paste(rootDir, '/', runName, '/finalRules/FPweek', fpWeek,
                  'vWindow', bestVWindowSize, 'config.txt', sep = ''))
}