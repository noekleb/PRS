/* Sletter gamle loggfiler */

DEF VAR cKatalog      AS CHAR FORMAT "x(240)" NO-UNDO.
DEF VAR cKatalogLst   AS CHAR FORMAT "x(240)" NO-UNDO.
DEF VAR pcFileName    AS CHAR FORMAT "x(80)" NO-UNDO.
DEFINE VARIABLE pcEkstent AS CHARACTER NO-UNDO.
DEF VAR pcFilePath    AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR pcFileAttrib  AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR piEntries     AS INT                 NO-UNDO.
DEF VAR pcBegins      AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR plFilId       AS DEC                 NO-UNDO.
DEF VAR piFilType     AS DEC                 NO-UNDO.
DEF VAR cLogg    AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR cekspLinje    AS CHAR FORMAT "x(40)" NO-UNDO.
DEF VAR iButNr        AS INT NO-UNDO.
DEF VAR dSlettDato    AS DATE NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.
DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS     cls.StdFunk.StandardFunksjoner NO-UNDO.

DEF STREAM InnFil.

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.
 
/* Default verdier. */
cKatalogLst = 'c:\tmp,' +
              'LOG,' + 
              'konv,' +
              'C:\OpenEdge\WRK\konv,' + 
              'filer\2\bku,' + 
              'filer\13\bku,' + 
              'C:\OpenEdge\WRK\filer\2\bku,' + 
              'C:\OpenEdge\WRK\filer\13\bku' +
              'C:\home\lindbak\ankommet\petikett\bku'
              .

ASSIGN    
    cLogg = 'Slett_gamle_Logger' + REPLACE(STRING(TODAY),'/','')
/*    pcBegins   = '*'  */
/*    pcekstent  = 'log'*/
    piFilType  = 1
    dSlettDato = TODAY - 10
    .

DEF TEMP-TABLE tmpFiler NO-UNDO 
    FIELD FilId AS DEC FORMAT ">>>>>>>>>>>>9"    
    FIELD FilNavn AS CHAR FORMAT "x(40)"  
    FIELD Katalog AS CHAR FORMAT "x(40)"
    FIELD Ekstent AS CHARACTER FORMAT "x(10)"   
    FIELD Dato AS DATE FORMAT "99/99/9999"     
    FIELD Kl AS CHAR FORMAT "x(10)"        
    FIELD Storrelse AS INT
    FIELD AntLinjer AS INT
    FIELD FilType AS INT  
    FIELD KFilNavn AS CHAR FORMAT "x(40)"
    .
    
rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'Start sletting.'
    ). 
    
    
DO iLoop = 1 TO NUM-ENTRIES(cKatalogLst):
    cKatalog = ENTRY(iLoop,cKatalogLst).
    IF TRIM(cKatalog) = '' THEN 
        NEXT.

rStandardFunksjoner:SkrivTilLogg(cLogg, 
    '    Sletter katalog: ' + cKatalog 
    ). 
        
INPUT STREAM InnFil FROM OS-DIR (cKatalog) NO-ECHO.
FILINPUT:
REPEAT:
    IMPORT STREAM InnFil
        pcFileName  
        pcFilePath  
        pcFileAttrib
        .

    /* Bare filer skal opprettes */
    IF LOOKUP("F",pcFileAttrib) <> 0 THEN
    FILBLOKK:
    DO:
      /* Åpner for filinformasjonen */
      ASSIGN
        piEntries           = NUM-ENTRIES(pcFileName,".")
        FILE-INFO:FILE-NAME = pcFilePath
        pcFileName          =  FILE-INFO:FILE-NAME
        . 
      /* Bare gamle filer skal behandles. nye skal bli liggende */
      IF FILE-INFO:FILE-MOD-DATE >= dSlettDato THEN 
        NEXT FILINPUT.
      
      iAnt = iAnt + 1.
      OS-DELETE SILENT VALUE(FILE-INFO:FILE-NAME).
      
    END. /* FILBLOKK */
END. /* FILINPUT */
INPUT STREAM InnFil CLOSE.
END.

rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'Ferdig sletting (' + STRING(iAnt) + ' filer selttet).'
    ). 

QUIT.

