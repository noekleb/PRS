
/*------------------------------------------------------------------------
    File        : batch_regen_stlager_run.p 
    Purpose     : 

    Syntax      :

    Description : Regenerering av stlager tabellen.

    Author(s)   : tomn
    Created     : Mon Apr 06 17:32:28 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*ROUTINE-LEVEL ON ERROR UNDO, THROW.*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE obOk      AS LOG       NO-UNDO.
DEFINE VARIABLE ocMelding AS CHAR      NO-UNDO.
DEFINE VARIABLE cButLst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTekst    AS CHARACTER NO-UNDO.

/*
/* Henter oppkoblingsinfo fra oppstartsicon. */
IF SESSION:PARAMETER <> "" THEN 
DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
    IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "BUTLST" AND 
        NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN 
    DO:
        ASSIGN 
            cButNr = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").
    END.
END.
*/

FOR EACH Butiker NO-LOCK WHERE
  Butiker.harButikksystem = TRUE AND
  Butiker.ApningsDato    <= TODAY AND
  Butiker.NedlagtDato     = ?:
  
  cButLst = cButLst + (IF cButLst <> '' THEN ',' ELSE '') + STRING(Butiker.Butik). 
             
END.
cButLst = TRIM(cButLst,',').

RUN byggomstlager.w (cButLst).

QUIT.