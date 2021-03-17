
/*------------------------------------------------------------------------
    File        : abc_excelrapport_run.p 
    Purpose     : 

    Syntax      :

    Description : Starter abc rapport 1

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
DEFINE VARIABLE iTid      AS INTEGER   NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

{adecomm/appserv.i}
RUN initjukebox.p.

ASSIGN
  iTid  = TIME
  cLogg = 'Suppleringsordre' + REPLACE(STRING(TODAY),'/','')
  .

/* Henter oppkoblingsinfo fra oppstartsicon. "BUTLST=176;177;178;179" */
IF SESSION:PARAMETER <> "" THEN 
DO iCount = 1 TO NUM-ENTRIES(SESSION:PARAMETER):
    IF ENTRY(iCount,SESSION:PARAMETER) BEGINS "BUTLST" AND 
        NUM-ENTRIES(ENTRY(iCount,SESSION:PARAMETER),"=") = 2 THEN 
    DO:
        ASSIGN 
            cButNr = ENTRY(2,ENTRY(iCount,SESSION:PARAMETER),"=").
    END.
END.

IF SEARCH('tnc.txt') <> ? THEN 
    DO:
        bTest = TRUE.
        IF cButNr = '' THEN cButNr = '16'.
    END.
ELSE bTest = TRUE.

IF bTest THEN 
DO:
    RUN bibl_loggDbFri.p (cLogg,' ' ).
    RUN bibl_loggDbFri.p (cLogg,'Starter. Butikker: ' + cButNr ).
END.
cTekst = /* 1 */ REPLACE(cButNr,';',',') + "|" + 
         /* 2 */ STRING(TODAY - 5) + "|" + 
         /* 3 */ STRING(TODAY) + "|" +   
         /* 4 */ "|" + 
         /* 5 */ "|" +
         /* 6 */ "|" +
         /* 7 */ "|" +
         /* 8 */ "Ja"
         .

IF cButNr = '' THEN 
DO:
    IF bTest THEN 
        RUN bibl_loggDbFri.p (cLogg,'  AVBRUTT - Butikknrliste er blank. ').  
  RETURN.
END.

/* TEST - Dette skal ikke gjøes i drift. */
/*obOk =  DYNAMIC-FUNCTION('runProc','pllisteordre_linje_nullstill_translogg.p',cTekst,?).*/

/* Kjører generering av ordreforslag. */
obOk =  DYNAMIC-FUNCTION('runProc','pllisteordre_linje_generer.p',cTekst,?).

IF bTest THEN 
    RUN bibl_loggDbFri.p (cLogg,'Ferdig. Tidsbruk: ' + string(TIME - iTid,"HH:MM:SS") + ' Status: ' + STRING(obOk)).

QUIT.