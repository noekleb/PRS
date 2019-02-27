
OS-COMMAND NO-WAIT VALUE("start http://aia.appfarm.no/chemistry/jukebox/JukeBox_tmphtm/index.html").
/*
DEF VAR cHelpFile AS CHAR NO-UNDO.
        
cHelpFile = SEARCH("doc\jukebox.chm").

IF cHelpFile NE ? THEN DO:
  FILE-INFO:FILE-NAME = cHelpFile.  
  OS-COMMAND NO-WAIT VALUE(QUOTER(FILE-INFO:FULL-PATHNAME)).
END.
ELSE 
  MESSAGE PROGRAM-NAME(1) SKIP
          "Could not locate JukeBox.chm help file. Should be placed under JukeBox\doc"
          VIEW-AS ALERT-BOX.

*/
