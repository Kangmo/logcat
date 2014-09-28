#
#   How to run :
#   1. Download archived log files for each day.
#   2. move archived log files into ../thelog folder.
#   3. execute gunzip * in the thelog folder.
#   4. execute run.sh on logcat folder.
#

export SBT_OPTS="-Xmx12G -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=12G -Xss2M  -Duser.timezone=GMT"
sbt 'run'
