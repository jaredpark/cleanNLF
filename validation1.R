vWindow = (finalPredWeek - validateWindowSize):(finalPredWeek - 1)

finalPredWeekSummStats = NULL
lookupTable = NULL
actualPerf = NULL
allConfigStat = NULL

for (fitWindowSize in settings$fitWindowSizes){
  
  for (config in 1:nrow(configurations)){
    
    for (trees in settings$numTrees){
      
      allPredData = NULL
      configStats = NULL
      
      for (vWeek in vWindow){
        weekPredData = read.table(paste(rootDir, '/', runName, '/predictions', '/gbmPred', 'FW', fitWindowSize, 
                                        'week', vWeek, 'config', config, 'nTrees', trees, '.txt', sep = ''))
        allPredData = rbind(allPredData, weekPredData)
        weekStat = makeErrData(weekPredData$pred, weekPredData$line, weekPredData$actual, betDelta, fast = T)[decisionRule]
        configStats = c(configStats, weekStat)
      }
      allConfigStat = rbind(allConfigStat, configStats)
      summStats = makeErrData(allPredData$pred, allPredData$line, allPredData$actual, betDelta, fast = T)
      finalPredWeekSummStats = rbind(finalPredWeekSummStats, summStats)
      lookupTable = rbind(lookupTable, c('fw' = fitWindowSize, 'config' = config, 'trees' = trees, 'delta' = betDelta))
#       print(nrow(lookupTable))
    }
  }
}

column = decisionRule
weekSE = apply(allConfigStat, 1, sd)/vWindowSize

if (chooseMin){
  minRow = which.min(finalPredWeekSummStats[,column])
} else{
  minRow = which.min(scale(finalPredWeekSummStats[,column]) + .25*scale(weekSE))
}

# minPlus1 = finalPredWeekSummStats[minRow,column] + weekSE[minRow]
# abline(h = minPlus1)


if (chooseParsimonious){
  candidates = which(finalPredWeekSummStats[, column] < minPlus1)
  if (length(candidates) == 1){
    bestModel = candidates
  } else {
    candidateInfo = cbind(candidates, lookupTable[candidates,], mean = finalPredWeekSummStats[candidates, column], SE_mean = colSE[candidates])
    first = 'fw'
    second = 'config'
    third = 'trees'
    # min of neg fw is same as max of fw
    candidateInfo = rbind(NULL, candidateInfo[-candidateInfo[,first] == min(-candidateInfo[,first]),])
    candidateInfo = rbind(NULL, candidateInfo[candidateInfo[,second] == min(candidateInfo[,second]),])
    candidateInfo = rbind(NULL, candidateInfo[candidateInfo[,third] == min(candidateInfo[,third]),])
    bestModel = candidateInfo[,'candidates']
  }
} else if (chooseParsOnlyByTrees) {
  candidates = which(lookupTable[,'config'] == lookupTable[minRow, 'config'] & lookupTable[,'fw'] == lookupTable[minRow, 'fw'] & finalPredWeekSummStats[, column] < minPlus1)
  bestModel = candidates[which.min(lookupTable[candidates, 'trees'])]
} else {
  bestModel = minRow
}

best = lookupTable[bestModel, ]
best = c(best, configurations[best['config'], ])

write.table(best, file = paste(rootDir, '/', runName, '/validation/ruleConfigFinalPredWeek', finalPredWeek,
                               'vWindow', validateWindowSize, '.txt', sep = ''))

file.copy(paste(rootDir, '/', runName, '/predictions/gbmPred', 'FW', best$fw, 'week', 
                finalPredWeek, 'config', best$config, 'nTrees', best$trees, '.txt', sep = ''),
          paste(rootDir, '/', runName, '/validation/predFinalPredWeek', 
                finalPredWeek, 'vWindow', validateWindowSize, '.txt', sep = ''),
          overwrite = T)

if (F){
  column = decisionRule
  
  maxSize = 1
  par(mfrow = c(1,1))
  #   colVarRank = rank(colVar)
  #   colCex = colVarRank/max(colVarRank)
  colCex = rank(apply(allConfigStat, 1, var))/nrow(allConfigStat)
  
  dat = cbind(lookupTable, finalPredWeekSummStats)
  colRange = range(dat[,column])
  #   colRange = range(scale(dat[,column]) + .5*scale(colSE))
  c = 1
  for (fws in unique(dat[,'fw'])){
    for (intDepth in unique(configurations$intDepth)){
      intIndex = lookupTable[,'config'] == intDepth & dat[,'fw'] == fws
      toPlot = dat[intIndex ,column]
      #       toPlot = (scale(dat[,column]) + .5*scale(colSE))[intIndex] # scale(dat[intIndex,column]) + .5*scale(colSE[intIndex])
      if (fws == min(dat[,'fw']) & intDepth == min(configurations$intDepth)){
        plot(dat[intIndex,'trees'], toPlot, type = 'o', pch = 20, cex = maxSize*colCex[intIndex], col = c,
             lty = intDepth, main = paste(column, 'vs trees:', finalPredWeek), ylab = column, xlab = 'trees', ylim = colRange)
      } else {
        lines(dat[intIndex,'trees'], toPlot, type = 'o', pch = 20, cex = maxSize*colCex[intIndex],
              col = c, lty = intDepth)
      }
    }
    c = c+1
  }
  text(500, colRange[1], paste('line abs err = ', round(finalPredWeekSummStats[1,'lineAbs'], 2), sep = ''), cex = .4)
  legend(200, colRange[2], legend = unique(dat[,'fw']), fill = 1:length(settings$fitWindowSizes), lty = 1, cex = .5, bg = rgb(1,1,1,alpha = 0))
  legend(400, colRange[2], legend = unique(configurations[,'intDepth']), col = 1, lty = 1:length(unique(configurations$intDepth)), cex = .5, bg = rgb(1,1,1,alpha = 0))
  
   c = 1
  for (nTr in unique(dat[,'trees'])){
    for (intDepth in unique(configurations$intDepth)){
      intIndex = lookupTable[,'config'] == intDepth & dat[,'trees'] == nTr
      toPlot = dat[intIndex ,column]
      #       toPlot = (scale(dat[,column]) + .5*scale(colSE))[intIndex] # scale(dat[intIndex,column]) + .5*scale(colSE[intIndex])
      if (intDepth == min(configurations$intDepth)){
        plot(dat[intIndex,'fw'], toPlot, type = 'o', pch = 20, cex = maxSize*colCex[intIndex], col = c,
             lty = intDepth, main = paste(column, 'vs fws:', finalPredWeek), ylab = column, xlab = 'fws', ylim = colRange)
      } else {
        lines(dat[intIndex,'fw'], toPlot, type = 'o', pch = 20, cex = maxSize*colCex[intIndex],
              col = c, lty = intDepth)
      }
    }
    c = c+1
  }
  
  text(300, colRange[1], paste('line abs err = ', round(finalPredWeekSummStats[1,'lineAbs'], 2), sep = ''), cex = .5)
  legend(60, colRange[2], legend = unique(dat[,'trees']), fill = 1:length(settings$numTrees), lty = 1, cex = .5, bg = rgb(1,1,1,alpha = 0))
  legend(100, colRange[2], legend = unique(configurations[,'intDepth']), col = 1, lty = 1:length(unique(configurations$intDepth)), cex = .5, bg = rgb(1,1,1,alpha = 0))
}