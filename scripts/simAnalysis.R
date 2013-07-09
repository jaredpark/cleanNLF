mean(err)
mean(err < 1 - .5283)
plot(err, pch = 20, col = ifelse(err > 1-.5238, 2, 1))
# abline(v = seq(11-bestVWindowSize,74,11) + .5)
abline(h = c(1-.523))
plot(err, pch = 20, col = ifelse(adjErr > 1-.5238, 2, 1)); abline(h = c(1-.523))

fpWeekSeasVector = unlist(lapply(settings$vSeas, rep, length(settings$vWeek)))[fpWeeks]
#   labels = rep(settings$vWeeks, length(settings$vSeas))
#   labels = labels[-c(1:bestVWindowSize)]
for (season in unique(fpWeekSeasVector)){
  print(season)
  seasIndex = fpWeekSeasVector == season
  seasErr = mean(err[seasIndex])
  seasAdjErr = mean(adjErr[seasIndex])
  seasRmse = mean(rmse[seasIndex])
  seasLineRmse = mean(lineRmse[seasIndex])
  print(c(seasAdjErr, seasErr, seasRmse, seasLineRmse))
  lines(c(min(which(seasIndex))-1, max(which(seasIndex))), 
        c(seasErr, seasErr), col = 2)
}



config = NULL
err = NULL
for (fpWeek in fpWeeks){
  config = rbind(config, read.table(paste('./', runName, '/validation/ruleConfigFinalPredWeek', fpWeek, 'vWindow', bestVWindowSize, '.txt', sep = '')))
  err = rbind(err, t(read.table(paste('./', runName, '/validation/rulePerfFinalPredWeek', fpWeek, 'vWindow', bestVWindowSize, '.txt', sep = ''))))
}

endSeas = seq(12, 84, 12)

plot(err[, 'adjUnitsWon'], pch = 20); abline(h = 0)
p = mean(1-adjErr)
maxBet = ((1/1.1)*p - (1 - p))/(1/1.1)

savePoint = 2
startingRoll = 100
rollPortion = .7
betPortion = .02

saving = 0
roll = startingRoll
seasEndTotal = c()
for (betWeek in 1:length(adjErr)){
  
  weekRoll = roll*rollPortion
  weekBet = weekRoll/err[betWeek, 'maxNumBets']
#   weekBet = weekRoll*betPortion
  weekProfit = weekBet * err[betWeek, 'unitsWon']
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
  }
  print(c(betWeek, saving, roll, weekBet))
}
seasEndTotal
diff(c(startingRoll, seasEndTotal))

plot(config[, 'delta'], err[,'maxNumBets'] - err[,'adjNumBets'], ylab = 'num pred w/in delta', xlab = 'delta', pch = 20)
boxplot(err[, 'adjClassErr']~config[, 'delta'], xlab = 'delta', ylab = 'adjusted error')
points(factor(config[, 'delta']), err[, 'adjClassErr'], pch = 20)
plot(err[,'rmse']-err[,'lineRmse'], err[,'classErr'], pch = 20, ylab = 'classification err (all games)', xlab = 'Pred RMSE minus Line RMSE'); abline(h = 1-.5238); abline(v = 0)
plot(err[,'rmse']-err[,'lineRmse'], err[,'adjClassErr'], pch = 20); abline(h = 1-.5238); abline(v = 0)
plot(err[,'classErr'], err[,'adjClassErr'], pch = 20, xlab = 'classification err (all games)', ylab = 'classification error (using delta)'); abline(0,1)
plot(err[,'unitsWon'], err[,'adjUnitsWon'], pch = 20, xlab = 'units won (all games)', ylab = 'units won (using delta)'); abline(0,1)
for (param in colnames(config)[-2]){
  boxplot(err[,'classErr']~config[, param], main = param)
  points(factor(config[, param]), err[, 'classErr'], pch = 20)
}
summary(apply(config[,-2], 2, factor))

