#!/bin/bash

#Locked
mysql -u root -pcrmanaroot@2016  -e "show processlist" | grep -i "Sleep" >> locked_log.txt

for line in `cat locked_log.txt | awk '{print $1}'`
do 
   echo "kill $line;" >> kill_thread_id.sql
done
