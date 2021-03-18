
/*------------------------------------------------------------------------
    File        : importSLSRPT.p
    Purpose     : Importerer alle SLSRPT filene som ligger i inn katalog.

    Syntax      : run importSLSRPT.p.

    Description : 

    Author(s)   : Tom Nøkleby
    Created     : 6/3-2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cLogg  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
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
DEFINE VARIABLE rSLSRPT             AS CLASS cls.Kommisjon.SLSRPT NO-UNDO.

{ cls\StdFunk\filliste.i }

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN 
    cLogg = 'importSLSRPT' + REPLACE(STRING(TODAY),'/','')
    cBku  = '\bku'
    .

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).
rSLSRPT             = NEW cls.Kommisjon.SLSRPT( INPUT cLogg ).

/* Ikke aktiv, avslutter. */
IF rSLSRPT:cbAktiv = FALSE THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg, 
        '    Import ikke aktiv. Avslutter.' 
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
/*    ccEkstent   = IF ccEkstent = '' THEN  '.edi' ELSE ccEkstent                         */
/*    ccEkstent   = IF NUM-ENTRIES(ccEkstent,'.') <> 2 THEN '.' + ccEkstent ELSE ccEkstent*/
    cTimeLst    = ''
    bTest       = TRUE 
    .

/* Leser katalog med filer og sender importerer. */
DO iLoop = 1 TO NUM-ENTRIES(rSLSRPT:ccKatalogLst):
  rSLSRPT:ccKatalog = ENTRY(iLoop,rSLSRPT:ccKatalogLst).
  
  /* Sikrer at temp-tabellen er tom før den fylles på. */
  EMPTY TEMP-TABLE  tmpFiler.
      
  /* Henter liste med filer som skal sendes for butikken. */
  rStandardFunksjoner:LagFillisteForKatalog(INPUT  rSLSRPT:ccKatalog,
                                            INPUT  '' , 
                                            INPUT  rSLSRPT:ccEkstent, 
                                            OUTPUT TABLE tmpFiler).
  IF bTest THEN 
      TEMP-TABLE tmpFiler:WRITE-JSON('file', 'log\EDIFilLst' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.JSon', TRUE).
  
  /* For hver fil, kjøres sending */
  IF CAN-FIND(FIRST tmpfiler) THEN 
    DO: 
      FIND FIRST tmpfiler.
      
      /* Oppretter bonghode. */
      rSLSRPT:opprettFil (INPUT  tmpFiler.PathName, INPUT tmpFiler.File-Name).
      rSLSRPT:opprettDatasett().
      
      /* Import av salgsfilene. */
      FOR EACH tmpFiler:
          rStandardFunksjoner:SkrivTilLogg(cLogg, 
              '   Importerer fil: ' + tmpfiler.Full-Path-Name
              ). 
          
          IF SEARCH(tmpfiler.Full-Path-Name) <> ? THEN 
              rSLSRPT:importerFil (tmpfiler.Full-Path-Name,
                  OUTPUT bOk,
                  OUTPUT cReturn
                  ).
          rStandardFunksjoner:SkrivTilLogg(cLogg, 
              '     Importert: ' + tmpFiler.File-Name + ' ' + (IF bOk THEN 'OK' ELSE 'Feil ') + cReturn
              ). 
          DELETE tmpfiler.
      END.
    END.
END.    

/* **********************  Internal Procedures  *********************** */

