userName=$USER
numNodes=$(starcluster listclusters mycluster | grep "Total nodes" | cut -d' ' -f3)
nodeNames="master $(eval echo $(seq -f node%03g 1 $(($numNodes-1))))"
