starcluster put -u jjp mycluster install.sh .
cmd="source /home/jjp/install.sh >& /home/jjp/install.log.master"
starcluster sshmaster mycluster $cmd

for node in $nodeNames; do
 if [ "$node" != "master" ]; then
  cmd="source /home/jjp/install.sh >& /home/jjp/install.log.$node"
  starcluster sshmaster mycluster "ssh $node $cmd" &
 fi
done

starcluster put -u jjp mycluster installRpackages.R .

for node in $nodeNames; do
 cmd="R CMD BATCH --no-save /home/jjp/install.Rpackages.R >& /home/jjp/install.Rpackages.log.$node"
 starcluster sshmaster mycluster "ssh $node $cmd" &
done