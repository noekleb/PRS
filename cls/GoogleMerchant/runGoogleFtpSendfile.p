
/*------------------------------------------------------------------------
    File        : runGoogleFtpSendfile.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrefix             AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE rGoogleftpSendFile        AS CLASS cls.GoogleMerchant.GoogleftpSendFile    NO-UNDO.

{ cls\StdFunk\filliste.i }
{ syspar2.i 50 65 2 cKatalogLst }

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN 
    cLogg = 'GoogleFtpSendfile' + REPLACE(STRING(TODAY),'/','')
    cBku  = '\bku'
    .

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).
rGoogleftpSendFile = NEW cls.GoogleMerchant.GoogleftpSendFile( INPUT cLogg ).

/* Ikke aktiv, avslutter. */
IF rGoogleftpSendFile:iAktiv <> 1 THEN
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
    cEkstent    = '.xml'
    cPrefix     = 'GoogleMerchant'
/*    cTimeLst    = '23,01'*/
    cTimeLst    = ''
    bTest       = TRUE 
    .

/* Leser katalog med filer og sender filene. */
DO iLoop = 1 TO NUM-ENTRIES(cKatalogLst):
  cKatalog = ENTRY(iLoop,cKatalogLst).
  
  /* Sikrer at temp-tabellen er tom før den fylles på. */
  EMPTY TEMP-TABLE  tmpFiler.
      
  /* Henter liste med filer som skal sendes for butikken. */
  rStandardFunksjoner:LagFillisteForKatalog(INPUT  cKatalog,
                                            INPUT  cPrefix , 
                                            INPUT  cEkstent, 
                                            OUTPUT TABLE tmpFiler).
  IF bTest THEN 
      TEMP-TABLE tmpFiler:WRITE-JSON('file', 'log\FilLst' + cPrefix + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.JSon', TRUE).
  
  /* For hver fil, kjøres sending */
  IF CAN-FIND(FIRST tmpfiler) THEN 
    FOR EACH tmpFiler:
        rStandardFunksjoner:SkrivTilLogg(cLogg, 
            '   Sender fil: ' + tmpfiler.Full-Path-Name
            ). 
        
        IF SEARCH(tmpfiler.Full-Path-Name) <> ? THEN 
            rGoogleftpSendFile:SendFile (tmpfiler.Full-Path-Name,
                OUTPUT bOk,
                OUTPUT cReturn
                ).
        DELETE tmpfiler.
    END.
END.    

QUIT.

/* **********************  Internal Procedures  *********************** */

