# rootDir = '/home/jared'
# runName = '3'
decisionRule = 'rmse'
delta = c(3)

# source(paste(rootDir, '/funcs.R', sep = ''))
# load(paste(rootDir, '/', runName, '/settings.Rout', sep = ''))
# configurations = read.table(paste(rootDir, '/', runName, '/configMatrix.txt', sep = ''), header = T)

# plotIt = T

# fpWeeks = (max(settings$validateWindowSizes) + max(settings$fitWindowSizes) + 1):(length(settings$vSeas)*length(settings$vWeeks))
fpWeeks = 181:345
fpWeeks = 181:270
fpWeeks = 271:345

vWindowSizesSummStats = NULL
allPredData = NULL

for (vWindowSize in 30){#settings$validateWindowSizes){
  vWindowSizeSummStats = NULL
  allPredData = NULL
  for (fpWeek in fpWeeks){
    predData = read.table(paste(rootDir, '/', runName, '/validation/predFinalPredWeek',
                                fpWeek, 'vWindow', vWindowSize, '.txt', sep = ''))
    allPredData = rbind(allPredData, predData)
  }
  vWindowSizeSummStats = makeErrData(allPredData$pred, allPredData$line, allPredData$actual, delta)
  vWindowSizesSummStats = rbind(vWindowSizesSummStats, vWindowSizeSummStats)
}

print(vWindowSizesSummStats)

whichBest = pickRule(vWindowSizesSummStats, decisionRule)
bestVWindowSize = settings$validateWindowSizes[whichBest]

allLine = allActual = allSeasons = allWeeks = allConfig = allPerf = allScoreDiff = allPred = allPredLineDiff = allPredActualDiff = allLineActualDiff = allGameAction = allBetResult = NULL
seasStart = seq(181, 331, 15)
seasons = seq(1998, 2008, 1)

for (fpWeek in fpWeeks){
  if (is.element(fpWeek, seasStart)){
    season = seasons[which(seasStart == fpWeek)]
  }
  
  weekConfig = read.table(paste(rootDir, '/', runName, '/validation/ruleConfigFinalPredWeek', fpWeek,
                          'vWindow', bestVWindowSize, '.txt', sep = ''))
  allConfig = rbind(allConfig, weekConfig)
  
  dat = read.table(paste(rootDir, '/', runName, '/validation/predFinalPredWeek', 
                         fpWeek, 'vWindow', bestVWindowSize, '.txt', sep = ''))
  weekPerf = makeErrData(dat$pred, dat$line, dat$actual, delta)
  allPerf = rbind(allPerf, weekPerf)
  
  weekActual = dat$actual
  allActual = c(allActual, weekActual)
  weekLine = dat$line
  allLine = c(allLine, weekLine)
  classification = ifelse(dat$pred < dat$line, 'home', 'away')
  actualClass = ifelse(dat$line > dat$actual, 'home', ifelse(dat$line == dat$actual, 'push', 'away'))
  scoreDiff = dat$actual
  allScoreDiff = c(allScoreDiff, scoreDiff)
  pred = dat$pred
  allPred = c(allPred, pred)
  week = rep(fpWeek, length(pred))
  allWeeks = c(allWeeks, week)
  weekSeason = rep(season, length(pred))
  allSeasons = c(allSeasons, weekSeason)
  predLineDiff = dat$pred - dat$line
  allPredLineDiff = c(allPredLineDiff, predLineDiff)
  predActualDiff = dat$pred - dat$actual
  allPredActualDiff = c(allPredActualDiff, predActualDiff)
  lineActualDiff = dat$line - dat$actual
  allLineActualDiff = c(allLineActualDiff, lineActualDiff)
  gameAction = actualClass != 'push'
  allGameAction = c(allGameAction, gameAction)
  betResult = (classification == actualClass)
  allBetResult = c(allBetResult, betResult)
}

naive = -3
r1 = 1 - sum(abs(allScoreDiff - allPred))/sum(abs(allScoreDiff - naive))
r2 = 1 - sum((allScoreDiff - allPred)^2)/sum((allScoreDiff - naive)^2)
r1; r2
cor(allPred, allActual)
cor(allLine, allActual)

maxDiff = max(allPredLineDiff[allGameAction])
minDiff = min(allPredLineDiff[allGameAction])
bin = .5
hist(abs(allPredLineDiff[allBetResult & allGameAction]), col = rgb(0, 0, 1, alpha = 1), breaks = seq(0,max(abs(c(minDiff, maxDiff))+1),bin), xlab = 'abs diff b/w game line and prediction')
hist(abs(allPredLineDiff[!allBetResult & allGameAction]), add = T, col = rgb(1, 0, 0, alpha = .7), breaks = seq(0,max(abs(c(minDiff, maxDiff))+1),bin))
legend(7, 80, legend = c('W', 'L'), col = c(4,2), lty = 1, cex = .5)
hist(allPredLineDiff[allBetResult & allGameAction], col = rgb(0, 0, 1, alpha = .8), breaks = seq(floor(minDiff-1*bin), maxDiff+2*bin, 2*bin), xlab = 'diff b/w game line and prediction')
hist(allPredLineDiff[!allBetResult & allGameAction], add = T, col = rgb(1, 0, 0, alpha = .7), breaks = seq(floor(minDiff-1*bin), maxDiff+2*bin, 2*bin))
legend(7, 30, legend = c('W', 'L'), col = c(4,2), lty = 1, cex = .5); abline(v = 0, lwd = 2)

bin = 1
for (season in unique(allSeasons)){
  hist(abs(allPredLineDiff[(allSeasons == season) & allBetResult & allGameAction]), col = rgb(0, 0, 1, alpha = 1), breaks = seq(0,max(abs(c(minDiff, maxDiff))+1),bin), xlab = 'abs diff b/w game line and prediction', main = season)
  hist(abs(allPredLineDiff[allSeasons == season & !allBetResult & allGameAction]), add = T, col = rgb(1, 0, 0, alpha = .7), breaks = seq(0,max(abs(c(minDiff, maxDiff))+1),bin))
  legend(7, 40, legend = c('W', 'L'), col = c(4,2), lty = 1, cex = .5)
  hist(allPredLineDiff[allSeasons == season & allBetResult & allGameAction], col = rgb(0, 0, 1, alpha = .8), breaks = seq(floor(minDiff-1*bin), maxDiff+2*bin, 2*bin), xlab = 'diff b/w game line and prediction', main = season)
  hist(allPredLineDiff[allSeasons == season & !allBetResult & allGameAction], add = T, col = rgb(1, 0, 0, alpha = .7), breaks = seq(floor(minDiff-1*bin), maxDiff+2*bin, 2*bin))
  legend(7, 20, legend = c('W', 'L'), col = c(4,2), lty = 1, cex = .5); abline(v = 0, lwd = 2)
}


unitsWon = portionBet = betErr = NULL
dRange = seq(0, 8, by = .05)
for (d in dRange){
  betErr = c(betErr, 1 - mean(allBetResult[allGameAction][abs(allPredLineDiff[allGameAction]) > d]))
  portionBet = c(portionBet, mean(abs(allPredLineDiff[allGameAction]) > d))
  unitsWon = c(unitsWon, sum(allBetResult[allGameAction][abs(allPredLineDiff[allGameAction]) > d])/1.1 - sum(!allBetResult[allGameAction][abs(allPredLineDiff[allGameAction]) > d]))
}
plot(dRange, portionBet, pch = 20)
plot(dRange, betErr, pch = 20)
plot(dRange, unitsWon, pch = 20)
plot(portionBet, betErr, pch = 20)

mean(allBetResult[allGameAction][(allPredLineDiff[allGameAction]) > 10 | (allPredLineDiff[allGameAction]) < -2])
sum(allBetResult[allGameAction][(allPredLineDiff[allGameAction]) > 10 | (allPredLineDiff[allGameAction]) < -3])/1.1 - sum(!allBetResult[allGameAction][(allPredLineDiff[allGameAction]) > 10 | (allPredLineDiff[allGameAction]) < -3])

plot(allPerf[,'adjClassErr'], pch = 20, ylim = c(0,1)); abline(h=1-.5238)
abline(v = seq(0+.5, 225, 15), lty = 2)
plot(allPerf[,'classErr'], pch = 20, ylim = c(0,1)); abline(h=1-.5238)
abline(v = seq(0+.5, 225, 15), lty = 2)

fpWeekSeasVector = unlist(lapply(settings$vSeas, rep, length(settings$vWeek)))[fpWeeks]
# fpWeekSeasVector = c(rep(2005, 12), rep(2006, 12), rep(2007, 12), rep(2008, 12), rep(2009, 12))
for (season in unique(fpWeekSeasVector)){
  print(season)
  seasIndex = fpWeekSeasVector == season
  seasErr = as.numeric(allPerf[seasIndex, 'classErr']%*%allPerf[seasIndex, 'maxNumBets'])/sum(allPerf[seasIndex, 'maxNumBets'])
  adjSeasErr = as.numeric(allPerf[seasIndex, 'adjClassErr']%*%allPerf[seasIndex, 'adjNumBets'])/sum(allPerf[seasIndex, 'adjNumBets'])
  seasAbs = t(allPerf[seasIndex,'abs'])%*%(allPerf[seasIndex,'maxNumBets'])/sum(allPerf[seasIndex,'maxNumBets'])
  seasAdjUnitsWon = sum(allPerf[seasIndex, 'adjUnitsWon'])
  seasUnitsWon = sum(allPerf[seasIndex, 'unitsWon'])
  print(c(adjSeasErr, seasErr, seasAbs, seasAdjUnitsWon, seasUnitsWon))#, seasRmse, seasLineRmse))
#   lines(c(min(which(seasIndex))-1, max(which(seasIndex))), 
#         c(seasErr, seasErr), col = 1, lty = 2)
  lines(c(min(which(seasIndex))-1, max(which(seasIndex))), 
        c(adjSeasErr, adjSeasErr), col = 2, lty = 2)
}

summary(apply(allConfig[,c('fw', 'trees', 'intDepth')], 2, factor))
plot(allConfig$fw); abline(v = seq(0,200, 15), lty = 2)
plot(allConfig$trees); abline(v = seq(0,200, 15), lty = 2)
plot(allConfig$intDepth); abline(v = seq(0,200, 15), lty = 2)

plot(rep(settings$vWeeks, length(fpWeeks)/length(settings$vWeeks)), allPerf[,'adjClassErr'], pch = 20, ylim = c(0,1))
abline(h = 1 - .5238)
for (week in settings$vWeeks){
  points(week, t(allPerf[week-min(settings$vWeeks)+1 + length(settings$vWeeks)*(0:(length(fpWeeks)/length(settings$vWeeks)-1)), 'adjClassErr'])%*%allPerf[week-min(settings$vWeeks)+1 + length(settings$vWeeks)*(0:(length(fpWeeks)/length(settings$vWeeks)-1)), 'adjNumBets']/sum(allPerf[week-min(settings$vWeeks)+1 + length(settings$vWeeks)*(0:(length(fpWeeks)/length(settings$vWeeks)-1)), 'adjNumBets']), pch = 20, col = 2)
}

plot(allConfig$fw, allPerf[,'abs'] - allPerf[,'lineAbs'], pch = 20, ylab = 'abs - line abs', xlab = 'fw')
abline(h = mean(allPerf[,'abs'] - allPerf[,'lineAbs']))
plot(allConfig$fw, allPerf[,'rmseLineRmseDiff'], pch = 20)
plot(allConfig$fw, allPerf[,'binnedAbs'] - allPerf[,'binnedLineAbs'], pch = 20, ylab = 'binned abs - binned line abs', xlab = 'fw')

plot(allConfig$fw, allPerf[,'adjClassErr'], pch = 20, ylim = c(0,1))
abline(h = 1-.5238)
plot(allConfig$fw, allPerf[,'classErr'], pch = 20, ylim = c(0,1))
abline(h = 1-.5238)

sum(allBetResult[allGameAction]/1.1 - !allBetResult[allGameAction])
sum(allBetResult[allGameAction][abs(allPredLineDiff[allGameAction])>delta]/1.1 - !allBetResult[allGameAction][abs(allPredLineDiff[allGameAction])>delta])

savePoint = 2
totalRoll = 100
weekRollPortion = 1
seasRollPortion = 1
startingRoll = seasRollPortion*totalRoll

endSeas = seq(15,200,15)
saving = 0
roll = startingRoll
seasEndTotal = c()
for (betWeek in 1:length(fpWeeks)){
  
  weekRoll = roll*weekRollPortion
  weekBet = weekRoll*.05
#   weekBet = weekRoll/allPerf[betWeek, 'adjNumBets']
#   weekBet = 5
  weekProfit = weekBet * allPerf[betWeek, 'adjUnitsWon']
  roll = roll + weekProfit
  weekSaving = ifelse(roll > startingRoll*savePoint, roll - startingRoll*savePoint, 0)
  saving = saving + weekSaving
  roll = roll - weekSaving
  if (roll < startingRoll){
    saving = saving - (startingRoll - roll)
    roll = startingRoll
  }
  if (is.element(betWeek, endSeas)){
    seasEndTotal = c(seasEndTotal, saving + roll)
#     startingRoll = .5*(saving + roll)
#     startingRoll = 
#     saving = saving - (startingRoll - roll)
#     roll = startingRoll
  }
   print(c(betWeek, saving, roll))
}
seasEndTotal
diff(c(seasRollPortion*totalRoll, seasEndTotal))
