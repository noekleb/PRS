/* Very simple backup script scheduler that executes a script 
   sometime right after midnight (time < 4000).
   To invoke:
   - name the backupscript dbbackup and place it where the scheduler (this file) resides
   - command line:
     bpro -p simpleScheduler.p > ./backuplog.txt &
      - on *nix the & will return to the command line so the program will continue after logg-off
      - the > ./backup.log will redirect output from the backup script to backup.log
  To stop:
    rename the script file (dbbackup) 
  To stop immediately:
    ps -ef |grep simpleScheduler
    which will return the process# something like this:
    root      4419     1  0 15:32 pts/0    00:00:00 /usr/local/progress/10.0b/bin/_progres -1 -b -p ./simpleScheduler.r
    to kill the process:
    kill -9 4419
----------------------------------------------------------*/

REPEAT:
  IF SEARCH("./dbbackup") = ? THEN DO:
    OUTPUT TO ./dbbackup.log APPEND.
    PUT UNFORMATTED "** " TODAY " " STRING(TIME,"HH:MM:SS") " Backup failed (or was stopped)!! Invalid script file. Restart scheduler with valid script (./dbbackup)" SKIP.
    OUTPUT CLOSE.
    QUIT.
  END.
  
/*   IF SUBSTR(STRING(TIME,"HH:MM"),1,2) = "15" THEN DO: */

  IF TIME < 4000 THEN DO:
    OS-COMMAND SILENT ./dbbackup.

    OUTPUT TO ./dbbackup.log APPEND.
    PUT UNFORMATTED "** " TODAY " " STRING(TIME,"HH:MM:SS") " Backup script executed" SKIP.
    OUTPUT CLOSE.
  END.
  PAUSE 3600.
END.

/* 
bpro -p simpleScheduler.p > ./backuplog.txt & 
*/
