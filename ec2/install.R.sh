starcluster put -u jjp mycluster install.R.base.sh .
cmd="source /home/jjp/install.R.base.sh >& /home/jjp/install.R.base.log.master"
starcluster sshmaster mycluster $cmd

for node in $nodeNames; do
 if [ "$node" != "master" ]; then
  cmd="source /home/jjp/install.R.base.sh >& /home/jjp/install.R.base.log.$node"
  starcluster sshmaster mycluster "ssh $node $cmd" &
 fi
done

starcluster put -u jjp mycluster install.Rpackages.R .
for node in $nodeNames; do
 cmd="sudo R CMD BATCH --no-save /home/jjp/install.Rpackages.R /home/jjp/install.Rpackages.log.$node"
 starcluster sshmaster mycluster "ssh $node $cmd" &
done