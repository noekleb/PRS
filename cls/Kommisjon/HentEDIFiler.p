
/*------------------------------------------------------------------------
    File        : HentEDIFiler.p
    Purpose     : Henter alle EDI filer i angitte kataloger.

    Syntax      : run HentEDIFiler.p.

    Description : 

    Author(s)   : Tom Nøkleby
    Created     : 4/4-2021
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
DEFINE VARIABLE cFilListe           AS LONGCHAR NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rHentFTP            AS CLASS cls.sendFTP.HentFTP    NO-UNDO.

{ cls\StdFunk\filliste.i }
{ syspar2.i 55 10 7 cEkstent }
{ syspara.i 55 10 7 cKatalogLst } 

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN 
    cLogg = 'HentEDIFiler' + REPLACE(STRING(TODAY),'/','')
    cBku  = '\bku'
    .

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).
rHentFTP = NEW cls.sendFTP.HentFTP( INPUT cLogg ).

/* Ikke aktiv, avslutter. */
IF rHentFTP:iAktiv <> 1 THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg, 
        '    Feed Ikke aktiv. Avslutter.' 
        ).
    QUIT.
  END.

/* Oppretter kataloger hvis de mangler. */
rStandardFunksjoner:prepKatalog(cKatalogLst, cBku).

ASSIGN         
    cEkstent    = IF cEkstent = '' THEN  '.edi' ELSE cEkstent
    cEkstent    = IF NUM-ENTRIES(cEkstent,'.') <> 2 THEN '.' + cEkstent ELSE cEkstent
    cTimeLst    = ''
    bTest       = TRUE 
    .

/* Sikrer at temp-tabellen er tom før den fylles på. */
EMPTY TEMP-TABLE  tmpFiler.

/* Henter filliste. */
IF rhentFTP:HentFileListe( OUTPUT cFilListe) THEN 
    rhentFTP:ByggHostFilListe(cFilListe, FALSE, OUTPUT TABLE tmpFiler).

/* Leser fillisten og henter filene. */
IF CAN-FIND(FIRST tmpFiler) THEN
DO:
  IF bTest THEN 
    TEMP-TABLE tmpFiler:WRITE-JSON ('file', 'konv\tmpFiler' + STRING(TODAY,"99999999") + '.json', TRUE).
      
  FOR EACH tmpFiler 
    BY tmpfiler.FilId:
    rStandardFunksjoner:SkrivTilLogg(cLogg, 
        '   Henter fil: ' + tmpfiler.File-Name
        ). 
    
    IF SEARCH(tmpfiler.File-Name) <> '' THEN 
        rHentFTP:HentFil (tmpfiler.File-Name,
            OUTPUT bOk,
            OUTPUT cReturn
            ).
  END.
END.    
ELSE DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg, 
      '   Ingen filer å hente.'
      ). 
END.

/* **********************  Internal Procedures  *********************** */

