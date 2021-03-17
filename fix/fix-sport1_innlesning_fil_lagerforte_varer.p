/* Innlesning Avdeling fil med liste over lagerførte varer. */

DEFINE VARIABLE cFilNavn    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLinje      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lArtikkelNr AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cEAN        AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lDec        AS DECIMAL   NO-UNDO.

DEFINE STREAM Inn.

CURRENT-WINDOW:WIDTH = 250.

ASSIGN
  cFilNavn = 'C:\Home\Lindbak\ANKOMMET\Lagerartikler_August2009.csv'.

INPUT STREAM inn FROM VALUE(cFilNavn) NO-ECHO.

LOOPEN:
REPEAT:
  IMPORT STREAM Inn UNFORMATTED cLinje.
  
  ASSIGN
    cTekst = TRIM(ENTRY(2,cLinje,';'))
    cTekst = SUBSTRING(cTekst,1,length(cTekst) - 3)
    cEAN   = TRIM(ENTRY(3,cLinje,';'))
    .
  IF cTekst = '' THEN NEXT LOOPEN.
  ASSIGN
    lDec = DECIMAL(cTekst) NO-ERROR.
  IF ERROR-STATUS:ERROR THEN NEXT LOOPEN.
  
  /*
  DISPLAY
    cTekst FORMAT "x(20)" COLUMN-LABEL 'SE Art.nr'
    CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = DECIMAL(cTekst)) COLUMN-LABEL 'ARTBAS'
    CAN-FIND(StrekKode WHERE StrekKode.Kode = cEAN) AND cEAN <> '' COLUMN-LABEL 'EAN'
    ENTRY(1,clinje,';') FORMAT "x(20)"
    ENTRY(2,clinje,';') FORMAT "x(20)"
    ENTRY(3,clinje,';') FORMAT "x(20)"
    cLinje FORMAT 'x(60)'
    WITH WIDTH 250.
  */ 
  
  FIND ArtBas WHERE 
    ArtBas.ArtikkelNr = DECIMAL(cTekst) NO-ERROR.
  IF AVAILABLE ArtBas THEN ArtBas.KjedeVare = TRUE.

  IF cEAN <> '' THEN 
  DO:
    IF AVAILABLE ArtBas THEN RELEASE ArtBas.
    FIND StrekKode WHERE StrekKode.Kode = cEAN NO-ERROR.
    IF AVAILABLE StrekKode THEN FIND ArtBas OF StrekKode NO-ERROR.
    IF AVAILABLE ArtBas THEN ArtBas.KjedeVare = TRUE.
  END.
    
END. /* LOOPEN Repeat */  