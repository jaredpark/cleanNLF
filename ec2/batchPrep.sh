R CMD BATCH --no-save /home/jjp/algorithm/gbmSetup.R gbmSetupLog
starcluster put $clusterName /home/jared/algorithm /home/jared
R CMD BATCH --no-save 


for ((fwSize