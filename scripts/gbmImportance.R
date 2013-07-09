require(gbm)

rootDir = "~/GitHub/NFL/nflProject"
runName = "t11"

featImp = NULL
wkAvgImp = NULL
for (fpWeek in 271:345){
  
  weekConfig = read.table(paste(rootDir, '/', runName, '/validation/ruleConfigFinalPredWeek',
                                fpWeek, 'vWindow', bestVWindowSize, '.txt',
                                sep = ''))
  
  load(paste(rootDir, '/', runName, '/fits/gbmFitFW', weekConfig$fw, 'week', 
             fpWeek, 'config', weekConfig$config, '.Rout', sep = ''))

  imp = summary.gbm(fit, plotit = F)
  impFeat = gsub('^([[:alpha:]]+)[[:digit:]]+.*Wk', '\\1', imp[,1])
  impFeat = gsub('^(.*)AllWk', '\\1', impFeat)
  wkAvg = gsub('^([[:alpha:]]+)([[:digit:]]+.*)Wk', '\\2', imp[,1])
  wkAvg = gsub('^([[:alpha:]]+)(All)Wk', '\\2', wkAvg)
  wkAvg = gsub('.*All', 'All', wkAvg)
  featSum = c()
  print(paste(fpWeek, length(unique(impFeat))))
  for (feat in sort(unique(impFeat))){
    featSum = c(featSum, sum(imp[impFeat == feat,2]))
  }
  featImp = cbind(featImp, round(featSum,4))
  
  wkAvgSum = c()
  for (numWeeks in sort(unique(wkAvg))){
    wkAvgSum = c(wkAvgSum, sum(imp[wkAvg == numWeeks,2]))
  }
  wkAvgImp = cbind(wkAvgImp, round(wkAvgSum,4))
}
rownames(featImp) = sort(unique(impFeat))
rownames(wkAvgImp) = sort(unique(wkAvg))
sort(apply(featImp, 1, sum))
round(sort(apply(wkAvgImp, 1, sum))/sum(sort(apply(wkAvgImp, 1, sum))), 3)