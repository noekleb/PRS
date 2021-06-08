DEF VAR pcKatFil AS CHAR FORMAT "x(60)" NO-UNDO.
DEF VAR pcFileName AS CHAR FORMAT "x(50)" NO-UNDO. 
DEF VAR pcFil2Name AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR pcFilePath AS CHAR NO-UNDO. 
DEF VAR pcFileAttrib AS CHAR NO-UNDO.
DEF VAR piEntries AS INT NO-UNDO.

DEF STREAM InnFil.  

CURRENT-WINDOW:WIDTH = 350.

ASSIGN
    pcKatFil = 'C:\NSoft\Polygon\PRS\kom\in\Gant\Bonger\Alle02062021'
    .

INPUT STREAM InnFil FROM OS-DIR (pcKatFil) NO-ECHO.
FILINPUT:
REPEAT:

  IMPORT STREAM InnFil
    pcFileName  
    pcFilePath  
    pcFileAttrib
    .
  /* Bare filer skal opprettes */
  IF LOOKUP("F",pcFileAttrib) <> 0 THEN
  DO:
    /* Åpner for filinformasjonen */
    ASSIGN
      piEntries           = NUM-ENTRIES(pcFileName,".")
      FILE-INFO:FILE-NAME = pcFilePath
      . 
    /* Sletter tomme filer. */
    IF FILE-INFO:FILE-SIZE = 0 THEN DO:
        /*OS-DELETE VALUE(FILE-INFO:FILE-NAME).*/
        NEXT FILINPUT.
    END.
    /* Kun filer som oppfyller masken på filnavn skal inn. */
    IF pcFileName MATCHES 'PRSPOS*' THEN. /* Gjør ingenting. */
    ELSE
      NEXT FILINPUT. /* Hopp over denne */

    ASSIGN
        /*pcFil2Name = REPLACE(ENTRY(1,pcFileName,'.'),'Pos','') + '.' + ENTRY(2,pcFileName,'.').*/
        pcFil2Name = ENTRY(1,pcFileName,'.') + '.' + ENTRY(2,pcFileName,'.').
        pcFil2Name = REPLACE(pcFileName,'PRSPOS','PRS').

    DISPLAY
        SEARCH(pcKatFil + '\' + pcFileName) FORMAT "x(100)"
        pcKatFil + '\' + pcFil2Name  FORMAT "x(100)"
        /*pcKatFil + '\' + pcFileName FORMAT "x(100)"*/
        WITH WIDTH 350.

    
    
    PAUSE 0.
    IF SEARCH(pcKatFil + '\' + pcFileName) <> ? THEN 
    DO:
      /* Gir filen dens riktige navn og tar bort den temporære filen. */
      /*
      OS-RENAME value(pcKatFil + '\' + pcFileName) 
         value(pcKatFil + '\' + LEFT-TRIM(pcFileName,'_')).
      */
      OS-RENAME value(pcKatFil + '\' + pcFileName) 
         value(pcKatFil + '\' + pcFil2Name).
         
    END.
    


  END.

END. /* FILINPUT */
INPUT STREAM InnFil CLOSE.
