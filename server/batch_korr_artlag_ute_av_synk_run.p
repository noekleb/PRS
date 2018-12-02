
/*------------------------------------------------------------------------
    File        : batch_korr_artlag_ute_av_synk_run.p.p 
    Purpose     : 

    Syntax      :

    Description : Korreksjon av artlag hvor vg/lopnr er ute av synk ed artikkelnr.

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
DEFINE VARIABLE cButNr    AS CHARACTER NO-UNDO.
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

cTekst = REPLACE(cButNr,';',',') + "|" + 
         STRING(TODAY - 5) + "|" + 
         STRING(TODAY) + "|"  
         + "|" 
         + "|" 
         + "|Ja".

IF cButNr = '' THEN RETURN.
obOk =  DYNAMIC-FUNCTION('runProc','pllisteordre_linje_generer.p',cTekst,?).
*/

RUN regenartlag_m_ulikeartnr.p.

QUIT.