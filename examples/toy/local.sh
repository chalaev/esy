#!/bin/sh -e
# generated 2016-08-04 17:47:25 GMT-4 by esy on "theorie13"
echo "`date '+%Y-%m-%d %H:%M:%S'` @ `hostname` = `hostname -I`" > ~/.esy/host-id.dat
timeout 5 emacsclient -e "(save-some-buffers t)"
fetchmail --quit
cd "/home/shalaev/esy-test/"
tar jcfv ~/.esy/$(hostname).tbz ~/.esy/host-id.dat "/home/shalaev/.esy/from-theorie13.sh" "1/nd/22.txt"
rm ~/.esy/host-id.dat
# end
cd -
