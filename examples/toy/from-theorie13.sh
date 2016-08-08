#!/bin/sh -e
# generated 2016-08-04 17:47:25 GMT-4 by esy on "theorie13"
# To be excecuted on remote host

case  `hostname`  in
"mendeleev")
rootDir="/home/shalaev/tmp/esy-test/"
;;
*)
echo "this host was not allowed in theorie13:.esy/theorie13.conf, I refuse to change files here, stopping"
exit
;;
esac
tar xjfv ~/.esy/from-theorie13.tbz -C $rootDir
# moving directories:
mv -i "1/gg/" "1/nd/gg/"
# moving files:
mv -i "3/ff.txt" "3/ttt/ff.txt"
# deleting files an directories:
# adjusting ownership (GIDs might be different on different hosts):
chgrp shalaev "3/ttt/"
# adjusting dates:
touch -d "2016-08-04 17:47:10 GMT-4" "3/ttt/"
# adjusting permissions:
chmod 770 "3/ttt/"
cd -
