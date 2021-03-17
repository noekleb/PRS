/* IMPORT av seqnr i strkonv tabellen. */

DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLinje AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.

DEFINE VARIABLE cStorl AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSeqNr AS INTEGER NO-UNDO.

ASSIGN
  cFilNavn = 'C:\Polygon\PRS\StorrelseSortering.csv'
  .
  
DEFINE STREAM Inn.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.
REPEAT:
  IMPORT STREAM inn UNFORMATTED cLinje.

  iAnt = iAnt + 1.
  IF iAnt = 1 THEN NEXT.

  ASSIGN
    cStorl = ENTRY(2,cLinje,';')
    iSeqNr = INTEGER(ENTRY(1,cLinje,';'))
    .
  
  FIND StrKonv EXCLUSIVE-LOCK WHERE
    TRIM(strKonv.Storl) = cStorl NO-ERROR.
  IF AVAILABLE StrKonv THEN 
    StrKonv.SeqNr = iSeqNr.
  
  DISPLAY 
    iSeqNr
    cStorl
    StrKonv.Storl WHEN AVAILABLE StrKonv
    StrKonv.SeqNr WHEN AVAILABLE StrKonv
    .
    
    
END.
INPUT STREAM Inn CLOSE.

/* **********************  Internal Procedures  *********************** */

PROCEDURE RefreshRecord:

/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN SUPER.

END PROCEDURE.

