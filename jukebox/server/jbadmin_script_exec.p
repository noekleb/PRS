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

DEF TEMP-TABLE ttScripts NO-UNDO
    FIELD cScript   AS CHAR
    FIELD fFreq     AS DEC /* minutes */
    FIELD dLastExec AS DATE
    FIELD iLastExec AS INT
    .

DEF TEMP-TABLE ttNewScripts NO-UNDO
    FIELD cScript   AS CHAR
    FIELD fFreq     AS DEC
    .
                                 
DEF VAR dScriptListDate AS DATE NO-UNDO INIT 01/01/1999.
DEF VAR dScriptListTime AS INT  NO-UNDO.
DEF VAR iMinFreq        AS INT  NO-UNDO INIT 3600.
DEF VAR iDeltaTime      AS INT  NO-UNDO.
DEF VAR iNow            AS INT  NO-UNDO.
DEF VAR cScriptList     AS CHAR NO-UNDO INIT "./scriptlist".
DEF VAR ix              AS INT  NO-UNDO.

IF SESSION:PARAMETER NE "" THEN
  cScriptList = SESSION:PARAMETER.

cScriptList = "c:\progress\myprogress\scriptlist".


FORM iNow
     ttScripts.dLastExec
     ttScripts.fFreq
     iDeltaTime
     ttScripts.cScript FORMAT "x(15)"
     iMinFreq
     WITH FRAME f1 DOWN.

REPEAT:
  ix = ix + 1.
  IF ix > 5 THEN QUIT.

  IF SEARCH(cScriptlist) = ? THEN DO:
    OUTPUT TO VALUE(cScriptList + ".log") APPEND.
    PUT UNFORMATTED "** " TODAY " " STRING(TIME,"HH:MM:SS") " Script failed (or was stopped)!! Invalid scriptlist file. Restart scheduler with valid scriptlist" SKIP.
    OUTPUT CLOSE.
    QUIT.
  END.

  FILE-INFO:FILE-NAME = SEARCH(cScriptlist).
  IF FILE-INFO:FILE-MOD-DATE GT dScriptListDate OR (FILE-INFO:FILE-MOD-DATE = dScriptListDate AND FILE-INFO:FILE-MOD-TIME GT dScriptListTime)  THEN DO:
    OUTPUT TO VALUE(cScriptList + ".log") APPEND.
    PUT UNFORMATTED "** " TODAY " " STRING(TIME,"HH:MM:SS") " Scriptlist loaded" SKIP.
    OUTPUT CLOSE.
    RUN ReloadScriptList.
  END.
  
  iNow = TIME.

  FOR EACH ttScripts:
    IF TODAY > ttScripts.dLastExec THEN
      iDeltaTime = (TODAY - ttScripts.dLastExec) * 24 * 3600 + (824 * 3600 - ttScripts.iLastExec) + iNow.
    ELSE
      iDeltaTime = TIME - ttScripts.iLastExec.
   
    DISP iNow
         ttScripts.dLastExec
         ttScripts.fFreq
         iDeltaTime
         ttScripts.cScript
         iMinFreq
         WITH FRAME f1.
    DOWN WITH FRAM f1.
/*
    PAUSE.
    QUIT.
    */
    IF iDeltaTime GE ttScripts.fFreq THEN DO:
      ASSIGN ttScripts.dLastExec = TODAY
             ttScripts.iLastExec = iNow
             .
      OS-COMMAND NO-WAIT /* NO-CONSOLE */ VALUE(ttScripts.cScript).
      OUTPUT TO VALUE(ttScripts.cScript + ".log") APPEND.
      PUT UNFORMATTED "** " TODAY " " STRING(TIME,"HH:MM:SS") " " ttScripts.cScript " executed" SKIP.
      OUTPUT CLOSE.
    END.
  END.
  PAUSE iMinFreq.
END.

PROCEDURE ReloadScriptList:
  EMPTY TEMP-TABLE ttNewScripts.
  INPUT FROM VALUE(cScriptList).
  REPEAT:
    CREATE ttNewScripts.
    IMPORT DELIMITER ";" ttNewScripts.
  END.
  INPUT CLOSE.
  FOR EACH ttNewScripts WHERE ttNewScripts.cScript = "":
    DELETE ttNewScripts.
  END.
  FOR EACH ttNewScripts:
    FIND FIRST ttScripts
         WHERE ttScripts.cScript = ttNewScripts.cScript
         NO-ERROR.
    IF NOT AVAIL ttScripts THEN DO:
      CREATE ttScripts.
      BUFFER-COPY ttNewScripts TO ttScripts.
      ASSIGN ttScripts.dLastExec = TODAY
             ttScripts.fFreq     = ttNewScripts.fFreq * 60
             .
    END.
    ELSE IF ttNewScripts.fFreq NE 0 THEN
      ttScripts.fFreq = ttNewScripts.fFreq * 60.
  END.

  FOR EACH ttScripts:
    IF ttScripts.fFreq > 0 AND ttScripts.fFreq < iMinFreq THEN
      iMinFreq = ttScripts.fFreq.
  END.
END PROCEDURE.

/* 
bpro -p simpleScheduler.p > ./backuplog.txt & 
*/
