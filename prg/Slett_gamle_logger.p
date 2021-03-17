/* Sletter gamle loggfiler */

DEFINE VARIABLE cKatalog      AS CHARACTER FORMAT "x(240)" NO-UNDO.
DEFINE VARIABLE cKatalogLst   AS CHARACTER FORMAT "x(240)" NO-UNDO.
DEFINE VARIABLE pcFileName    AS CHARACTER FORMAT "x(80)" NO-UNDO.
DEFINE VARIABLE pcEkstent AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcFilePath    AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE pcFileAttrib  AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE piEntries     AS INTEGER                 NO-UNDO.
DEFINE VARIABLE pcBegins      AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE plFilId       AS DECIMAL                 NO-UNDO.
DEFINE VARIABLE piFilType     AS DECIMAL                 NO-UNDO.
DEFINE VARIABLE cLogg    AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE cekspLinje    AS CHARACTER FORMAT "x(40)" NO-UNDO.
DEFINE VARIABLE iButNr        AS INTEGER NO-UNDO.
DEFINE VARIABLE dSlettDato    AS DATE NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
DEFINE VARIABLE cTblKatalog AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS     cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE TEMP-TABLE ttKatalog
    FIELD Katalog AS CHARACTER FORMAT "x(50)"
    FIELD SlettDato AS INTEGER 
    .

DEFINE TEMP-TABLE tmpFiler NO-UNDO 
    FIELD FilId AS DECIMAL FORMAT ">>>>>>>>>>>>9"    
    FIELD FilNavn AS CHARACTER FORMAT "x(40)"  
    FIELD Katalog AS CHARACTER FORMAT "x(40)"
    FIELD Ekstent AS CHARACTER FORMAT "x(10)"   
    FIELD Dato AS DATE FORMAT "99/99/9999"     
    FIELD Kl AS CHARACTER FORMAT "x(10)"        
    FIELD Storrelse AS INTEGER
    FIELD AntLinjer AS INTEGER
    FIELD FilType AS INTEGER  
    FIELD KFilNavn AS CHARACTER FORMAT "x(40)"
    .   

DEFINE STREAM InnFil.

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner() NO-ERROR.

ASSIGN 
    /* Default verdier - Oppsett hos GantNorge prod server. */
    cKatalogLst = 'c:\tmp,' +
                  'LOG,' + 
                  'konv,' +
                  'C:\OpenEdge\WRK\konv,' + 
                  'filer\2\bku,' + 
                  'filer\13\bku,' + 
                  'C:\OpenEdge\WRK\filer\2\bku,' + 
                  'C:\OpenEdge\WRK\filer\13\bku' +
                  'C:\home\lindbak\ankommet\petikett\bku'
    cTblKatalog = 'konfig\KatalogerFilSletting.json' 
    .

/* Leser katalogoppsett fra konfigurasjonsfil. */
IF SEARCH(cTblKatalog) <> ? THEN  
    TEMP-TABLE ttKatalog:READ-JSON ("File",cTblKatalog,"empty"). 

IF NOT CAN-FIND(FIRST ttKatalog) THEN 
DO:
    DO iLoop = 1 TO NUM-ENTRIES(cKatalogLst):
        CREATE ttKatalog.
        ASSIGN 
            ttKatalog.Katalog   = ENTRY(iLoop,cKatalogLst)
            ttKatalog.SlettDato = 10
            .
    END.
    OS-COMMAND SILENT 'mkdir Konfig'.
    TEMP-TABLE ttKatalog:WRITE-JSON('file', cTblKatalog, TRUE).
END.

ASSIGN    
    cLogg = 'Slett_gamle_Logger' + REPLACE(STRING(TODAY),'/','')
/*    pcBegins   = '*'  */
/*    pcekstent  = 'log'*/
    piFilType  = 1
    dSlettDato = TODAY 
    .

rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'Start sletting.'
    ). 
    
/* Logger slettedato. */
FOR EACH ttKatalog:
    rStandardFunksjoner:SkrivTilLogg(cLogg, 
        '   Katalog og slettedato:' + ttKatalog.Katalog + ' / ' + STRING(TODAY - ttKatalog.SlettDato) + '.' 
        ). 
END.
    
iLoop = 0.    
KATALOGLOOP:
FOR EACH ttKataLog:
    
    iLoop = iLoop + 1.
    IF TRIM(ttKatalog.Katalog) = '' THEN 
        NEXT.

    rStandardFunksjoner:SkrivTilLogg(cLogg, 
        '    Sletter katalog: ' + cKatalog 
        ). 
            
    INPUT STREAM InnFil FROM OS-DIR (ttKatalog.Katalog) NO-ECHO.
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
          IF FILE-INFO:FILE-MOD-DATE > dSlettDato - ttKatalog.SlettDato THEN 
            NEXT FILINPUT.

          rStandardFunksjoner:SkrivTilLogg(cLogg, 
              '    Sletter fil: ' + FILE-INFO:FILE-NAME 
              ). 
          
          iAnt = iAnt + 1.
          OS-DELETE SILENT VALUE(FILE-INFO:FILE-NAME).
          
        END. /* FILBLOKK */
    END. /* FILINPUT */
    INPUT STREAM InnFil CLOSE.
END. /* KATALOGLOOP */

rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'Ferdig sletting (' + STRING(iAnt) + ' filer selttet).'
    ). 

QUIT.

