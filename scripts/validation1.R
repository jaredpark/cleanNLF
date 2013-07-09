# validateWindowSize = 1
# finalPredWeek = 9
# betDelta = settings$betDelta
# betDelta = 1.5
vWindow = (finalPredWeek - validateWindowSize):(finalPredWeek - 1)

# decisionRule = 'rmse'
# runName = settings$name
# rootDir = '~/GitHub/VM_share'
# require('gbm')
# source(paste(rootDir, '/funcs.R', sep = ''))
# configurations = read.table(paste(rootDir, '/', runName, '/configMatrix.txt', sep = ''), header = T)
# load(paste(rootDir, '/', runName, '/settings.Rout', sep = ''))

finalPredWeekSummStats = NULL
lookupTable = NULL
vWeekRmseDiffTable = NULL

for (fitWindowSize in settings$fitWindowSizes){
  
  for (config in 1:nrow(configurations)){
    
    for (trees in settings$numTrees){
            
      for (betDelta in settings$betDeltas){
        errTable = NULL
        
        for (vWeek in vWindow){
          weekPredData = read.table(paste(rootDir, '/', runName, '/predictions', '/gbmPred', 'FW', fitWindowSize, 
                                          'week', vWeek, 'config', config, 'nTrees', trees, '.txt', sep = ''))
          pred = weekPredData$pred; line = weekPredData$line; actual = weekPredData$actual
          weekErrData = makeErrData(pred, line, actual, betDelta)
          errTable = rbind(errTable, weekErrData)
        }
        # plot(errTable[,2], pch = 20, ylim = c(0,1)); points(errTable[,1], pch = 20, col = 2)
        #
        summStats = makeSummStats(errTable, inclVars = T)
        weekByWeekRmseDiff = errTable[, 'rmseLineRmseDiff']
        vWeekRmseDiffTable = rbind(vWeekRmseDiffTable, weekByWeekRmseDiff)
        # This section needs to implement a procedure for analyzing the week to week performance of
        # a given configuration; variance, time dependence of performance, etc. 
        # Currently only the mean of the columns is considered, through makeSummStats() function.
        # Currently only one of the column means is considered, through pickRule() function.
        #
        finalPredWeekSummStats = rbind(finalPredWeekSummStats, summStats)
        lookupTable = rbind(lookupTable, c('fw' = fitWindowSize, 'config' = config, 'trees' = trees, 'delta' = betDelta))
      }
    }
  }
}
# plot(scale(apply(vWeekRmseDiffTable,1,var)), ylab = 'mean (black) & variance (red) of RMSE', col = 2, pch = 20, ylim = c(-3, 4))
# points(scale(apply(vWeekRmseDiffTable,1,mean)), col = rgb(.1,.1,.1,alpha=.6), pch = 20)
# apply(cbind(round(scale(apply(vWeekRmseDiffTable,1,mean)),3), round(scale(apply(vWeekRmseDiffTable,1,var)),3)), 1, sum)


# candidates = which(finalPredWeekSummStats[,decisionRule] < quantile(finalPredWeekSummStats[,decisionRule], .2))
# matplot(t(vWeekRmseTable[candidates,]), type = 'l', lty = 1, col = c(1:3, 1:3))
# round(finalPredWeekSummStats, 3)
whichBest = pickRule(finalPredWeekSummStats, decisionRule)
# candidates = which(finalPredWeekSummStats[, decisionRule] < quantile(finalPredWeekSummStats[, decisionRule], quant))
if (length(whichBest > 1)){
  candidatesClassErr = finalPredWeekSummStats[whichBest,'adjClassErr']
  whichBest = whichBest[which.min(candidatesClassErr)]
#   whichBest = whichBest[which.max(finalPredWeekSummStats[whichBest, 'adjUnitsWon'])]
}
best = lookupTable[whichBest, ]
best = c(best, configurations[best['config'], ])
write.table(best, file = paste(rootDir, '/', runName, '/validation/ruleConfigFinalPredWeek', finalPredWeek,
                               'vWindow', validateWindowSize, '.txt', sep = ''))

# bestPerf = finalPredWeekSummStats[whichBest, ]
weekPredData = read.table(paste(rootDir, '/', runName, '/predictions', '/gbmPred', 'FW', best$fw, 'week', 
                                finalPredWeek, 'config', best$config, 'nTrees', best$trees, '.txt', sep = ''))
pred = weekPredData$pred; line = weekPredData$line; actual = weekPredData$actual
bestPerf = makeErrData(pred, line, actual, betDelta)

write.table(bestPerf, file = paste(rootDir, '/', runName, '/validation/rulePerfFinalPredWeek', finalPredWeek,
                                   'vWindow', validateWindowSize, '.txt', sep = ''))