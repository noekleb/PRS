
/*------------------------------------------------------------------------
    File        : SendEDIFiler.p
    Purpose     : Sender alle EDI filer i angitte kataloger til mottager.

    Syntax      : run SendEDIFiler.p.

    Description : 

    Author(s)   : Tom N�kleby
    Created     : 22/1-2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cLogg  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEkstent            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKatalog            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKatalogLst         AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk                 AS LOG       NO-UNDO.
DEFINE VARIABLE cReturn             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTime               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTimeLst            AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest               AS LOG       NO-UNDO. 
DEFINE VARIABLE iLoop               AS INTEGER NO-UNDO.
DEFINE VARIABLE cBku                AS CHARACTER NO-UNDO.
DEFINE VARIABLE iErr-Status         AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendFTP            AS CLASS cls.sendFTP.SendFTP    NO-UNDO.

{ cls\StdFunk\filliste.i }
{ syspar2.i 55 10 7 cEkstent }
{ syspara.i 55 10 7 cKatalogLst }

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN 
    cLogg = 'SendFTP' + REPLACE(STRING(TODAY),'/','')
    cBku  = '\bku'
    .

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).
rSendFTP = NEW cls.sendFTP.SendFTP( INPUT cLogg ).

/* Ikke aktiv, avslutter. */
IF rSendFTP:iAktiv <> 1 THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg, 
        '    Feed Ikke aktiv. Avslutter.' 
        ).
    QUIT.
  END.

/* Oppretter kataloger hvis de mangler. */
cTekst = ''.
DO iLoop = 1 TO NUM-ENTRIES(cKatalogLst):
  cTekst = cTekst +
           (IF cTekst <> '' THEN '\' ELSE '') +  
           ENTRY(iLoop,cKatalogLst).
  OS-CREATE-DIR VALUE(cTekst).
  iErr-Status = OS-ERROR.
  IF iErr-Status <> 0 THEN
    rStandardFunksjoner:SkrivTilLogg(cLogg, 
        '    OS-ERROR: ' + STRING(iErr-Status) + ' ' + rStandardFunksjoner:Error-Status(iErr-Status) 
        ).
END.
cTekst = cTekst + cBku.
OS-CREATE-DIR VALUE(cTekst).

ASSIGN         
    cEkstent    = IF cEkstent = '' THEN  '.edi' ELSE cEkstent
    cEkstent    = IF NUM-ENTRIES(cEkstent,'.') <> 2 THEN '.' + cEkstent ELSE cEkstent
    cTimeLst    = ''
    bTest       = TRUE 
    .

/* Leser katalog med filer og sender filene. */
DO iLoop = 1 TO NUM-ENTRIES(cKatalogLst):
  cKatalog = ENTRY(iLoop,cKatalogLst).
  
  /* Sikrer at temp-tabellen er tom f�r den fylles p�. */
  EMPTY TEMP-TABLE  tmpFiler.
      
  /* Henter liste med filer som skal sendes for butikken. */
  rStandardFunksjoner:LagFillisteForKatalog(INPUT  cKatalog,
                                            INPUT  '' , 
                                            INPUT  cEkstent, 
                                            OUTPUT TABLE tmpFiler).
  IF bTest THEN 
      TEMP-TABLE tmpFiler:WRITE-JSON('file', 'log\EDIFilLst' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.JSon', TRUE).
  
  /* For hver fil, kj�res sending */
  IF CAN-FIND(FIRST tmpfiler) THEN 
    FOR EACH tmpFiler:
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '   Sender fil: ' + tmpfiler.Full-Path-Name
            ). 
        
        IF SEARCH(tmpfiler.Full-Path-Name) <> ? THEN 
            rSendFTP:SendFile (tmpfiler.Full-Path-Name,
                OUTPUT bOk,
                OUTPUT cReturn
                ).
        DELETE tmpfiler.
    END.
END.    

/* **********************  Internal Procedures  *********************** */
