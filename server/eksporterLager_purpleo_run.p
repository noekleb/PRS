
/*------------------------------------------------------------------------
    File        : eksporterkontant_holin_visma_business_run.p 
    Purpose     : 

    Syntax      :

    Description : Starter eksport

    Author(s)   : tomn
    Created     : Mon Apr  17:32:28 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*ROUTINE-LEVEL ON ERROR UNDO, THROW.*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE ocReturn  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDatoTid  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lTid      AS INTEGER   NO-UNDO.

/* Henter oppkoblingsinfo fra oppstartsicon. */
/*
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
*/

ASSIGN lTid = TIME.
RUN bibl_logg.p ('eksporter_lager_purple', 'eksporterLager_purpleo_run.p: WinCheduler Starter eksport av lager. ' + string(TIME,"HH:MM:SS")).

RUN eksportlager_purpleo.p (OUTPUT ocReturn).

ASSIGN lTid = TIME - lTid.
RUN bibl_logg.p ('eksporter_lager_purpleo', 'eksporterLager_purpleo_run.p: WinCheduler Stoppet eksport av lager. ' + string(TIME,"HH:MM:SS") + ' Tidsbruk: ' + string(lTid,"HH:MM:SS")).

QUIT.
