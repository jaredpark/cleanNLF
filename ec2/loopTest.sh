for(( vWeek=1; vWeek<6; vWeek++)); do
 echo "vWeek = $vWeek; source('Rscript.R')" > toSource.R
done