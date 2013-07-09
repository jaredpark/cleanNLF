gameInfoDate = 'sim'
mmDate = 'sim'

nTargetRows = 1600
nGamesPerWeek = 14
firstWeek = 5
lastWeek = 16
nSeas = round(nTargetRows/(nGamesPerWeek*(lastWeek - firstWeek + 1)), 0)
weeks = firstWeek:lastWeek
gameWeeks = rep(weeks, rep(nGamesPerWeek, length(weeks)))
weekColumn = rep(gameWeeks, nSeas)
seasColumn = rep((2007-nSeas+1):2007, rep(nGamesPerWeek*(lastWeek-firstWeek+1), nSeas))
nRows = length(seasColumn)

feat1 = abs(rnorm(nRows, 10, 2))
feat2 = abs(rnorm(nRows, 200, 40))
feat3 = abs(rnorm(nRows, 2, .25))

nNoiseFeat = 20
genNoise = function(n){
  return(rnorm(n))
}
noiseFeat = replicate(nNoiseFeat, genNoise(nRows))

response = 2*sqrt(feat1)+5.1*feat3^2+10*log(feat2)

MM = data.frame(cbind('Season' = seasColumn, 'Week' = weekColumn, 'Home' = seq(1:nRows), 
                'Away' = seq(1:nRows), feat1, feat2, feat3, noiseFeat, 'Response' = response))
gameInfo = MM[,1:4]
save(gameInfo, file = paste(gameInfoDir, '/gameInfo_', gameInfoDate, '.Rout', sep = ''))
save(MM, file = paste(gameInfoDir, '/MM_', mmDate, '.Rout', sep = ''))