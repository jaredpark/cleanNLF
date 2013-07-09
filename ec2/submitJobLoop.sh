fitWindowSizes="cat fwSizes.txt"
fitWindowSizes=$($fitWindowSizes)

fitWeekRange="cat fitWeekRange.txt"
fitWeekRange=$($fitWeekRange)

runName="cat runName.txt"
runName=$($runName)

rootDir="cat rootDir.txt"
rootDir=$($rootDir)

for fitWindowSize in $fitWindowSizes; do
 for fitWeek in $fitWeekRange; do
  echo "runName=$runName; rootDir=$rootDir; fitWindowSize = $fitWindowSize; validationWeek = $fitWeek; source('gbmFit.R')" > ./jobScripts/jobScript.$fitWindowSize.$fitWeek.R
  qsub "R CMD BATCH --no-save ./jobScripts/jobScript.$fitWindowSize.$fitWeek.R 
 done
done

  
