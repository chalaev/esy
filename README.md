<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Motivation</a></li>
<li><a href="#sec-2">2. Quick start</a></li>
<li><a href="#sec-3">3. Resulting files</a></li>
<li><a href="#sec-4">4. Saving local changes to perform them on another host</a></li>
</ul>
</div>
</div>

Note: this program requires [cl-libconfig](https://github.com/chalaev/cl-libconfig) (lisp interface to the standard libconfig library).

# Motivation<a id="sec-1" name="sec-1"></a>

Though I use free cloud file storages, I do not trust them.
The minimal danger with clouds is that they would analyze my files and give information about my income/habits etc.
to those annoying people who call me (sometimes several times a day!) trying to sell something.
It is also possible that such sophisticated systems like Dropbox or Google Drive have digital or human vulnerabilities.
Because of these fears all my [cloud-stored (daily) backups](http://www.chalaev.com/pub/etc/backup2l.conf) are encrypted.

However, encryption is inconvenient if I want to share (with a daily syncronization) folders
between different computers. For example, I use three computers: office workstation, laptop, and
home server. It is very unprobable that all of them will be broken simultaneously, so
my files are secure if syncronized among all three.
(I use [wifi-switcher](https://github.com/chalaev/wifi-switcher) to move files between mobile devices and linux laptops.)

Only important files are syncronized. The importance can be encoded in the
(`libconfig`-compatible) configuration file (see `examples/*/*.conf`) according to name, group, age, and size of a
file. Group names are preserved so it is ok if they have different IDs on different
hosts. Permissions are preserved as well. If a file (or a directory) is renamed/moved/erased/chmod-ed
on one host, the same will hapen on the other ones.
Note however that an independent backup is necessary
(I use `backup2l` + `gpg` + [yandex-disk](https://disk.yandex.com/) to protect important data from accidental removal.)

# Quick start<a id="sec-2" name="sec-2"></a>

Copy all files from the [github archive](https://github.com/chalaev/esy) to some directory, say `~/esy-source/`:

    mkdir ~/esy-source ~/.esy
    cd ~/esy-source
    git clone https://github.com/chalaev/esy
    cd esy
    sed -e "s/myhostname/`hostname`/" -e "s,/home/user,$HOME," examples/toy/theorie13.conf >  ~/.esy/`hostname`.conf
    mkdir -p ~/esy-test/{mail,2,3,4}
    mkdir -p ~/esy-test/2/a/b/c
    echo "aaa" > ~/esy-test/2/a/b/1.txt
    echo "bbb" > ~/esy-test/3/2.txt
    echo "ccc" > ~/esy-test/3/c.txt
    mkdir ~/esy-test/mail/a
    echo "uuu" > ~/esy-test/mail/a/u.txt
    ~/local/bin/sbcl --load main.lisp

where `~/local/bin/sbcl` is the (common) lisp command.
The default configuration file ``~/.esy/`hostname`.conf`` tells `esy` to
(recursively) monitor changes in the directories `~/esy-test/{mail,2,3,4,5}`.
The program will refuse to monitor inexisting `~/esy-test/5`.
It is instructive to monitor the log file:

    tail -f ~/.esy/log.txt

Now let us (in another terminal window)
create/move/copy/chmod files and directories in the monitored directories, for example:

    chmod a+r ~/esy-test/3/2.txt
    mv ~/esy-test/2/a/b/1.txt ~/esy-test/1/
    echo "ddd" >> ~/esy-test/3/c.txt
    echo "eee" >> ~/esy-test/3/e.txt
    mv ~/esy-test/3/e.txt ~/esy-test/4/
    rm -r ~/esy-test/mail/a

File-manipulation events are logged into `~/.esy/log.txt`
   For now the  `logLevel=1` in `example.conf` is set, but if you think that
   `~/.esy/log.txt` is growing too fast, set `logLevel=2` or even `logLevel=4`.
Now stop the program:

    echo "quit" > ~/.esy/message.ctl
    mkdir ~/.esy/do-read.ctl

where the last (mkdir) command tells the program to read instruction(s) from `~/.esy/message.ctl`

# Resulting files<a id="sec-3" name="sec-3"></a>

Let us consider simple live example of playing in the `~/esy-test/` directory on host named `theorie13`:

    cd ~/esy-test/1
    echo aa >> nd/22.txt 
    mv gg nd/
    cd ~/esy-test/3
    mkdir ttt
    mv ff.txt ttt/
    echo "quit" > ~/.esy/message.ctl
    mkdir ~/.esy/do-read.ctl

The resulting `~/.esy/local.sh` is

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

In this las code block, there are two somewhat "personal" commands:
timeout 5 emacsclient -e "(save-some-buffers t)" tells emacs to save all buffers, and
`fetchmail --quit` stops the `fetchmail` daemon.
(In the future these commands will be sourced from a separate user-defined file, say `~/.esy/on-sync.sh`)

The resulting `~/.esy/from-theorie13.sh` is

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

# Saving local changes to perform them on another host<a id="sec-4" name="sec-4"></a>

The simplest way to use these files is off-line one.
I work on theorie13 in my office with `esy` monitoring all files I change.
At the end of the day, I 
run `~/.esy/local.sh` and copy `~/.esy/from-theorie13.*` to a usb drive.
At home I

    mv from-theorie13.* ~/.esy
    cd ~/.esy
    chmod +x from-theorie13.sh
    ./from-theorie13.sh

and then launch `esy` in order to save any home file-edits on my work computer (that is, `theorie13`).

In reality I use email to copy files between hosts (I have special gmail accounts for that):
the file `~/.procmailrc` can be instructed to automatically save attachments from emails arriving from a certain address to ~/.esy/ …