
/*------------------------------------------------------------------------
    File        : medrabsjekkposter_run.p 
    Purpose     : 

    Syntax      :

    Description : Starter eksport

    Author(s)   : tomn
    Created     : 7/4-11
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*ROUTINE-LEVEL ON ERROR UNDO, THROW.*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE ocReturn  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDatoTid  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lTid      AS INTEGER   NO-UNDO.

DEF VAR bOk      AS LOG    NO-UNDO.
DEF VAR h_Prisko AS HANDLE NO-UNDO.
DEF VAR cTekst   AS CHAR   NO-UNDO.
DEF VAR hParent  AS HANDLE NO-UNDO.


ASSIGN lTid = TIME.
RUN bibl_logg.p ('medrabsjekkposter', 'medrabsjekkposter_run.p: WinCheduler Starter postering av rabattsjekker. ' + string(TIME,"HH:MM:SS")).

RUN posterRabattsjekker.

ASSIGN lTid = TIME - lTid.
RUN bibl_logg.p ('medrabsjekkposter', 'medrabsjekkposter_run.p: WinCheduler Stoppet postering av rabattsjekker. ' + string(TIME,"HH:MM:SS") + ' Tidsbruk: ' + string(lTid,"HH:MM:SS")).

QUIT.

PROCEDURE posterRabattsjekker:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAntPostert  AS INT     NO-UNDO.
  DEFINE VARIABLE iAntLest     AS INT     NO-UNDO.
  DEFINE VARIABLE dFraDato     AS DATE    NO-UNDO.
  DEFINE VARIABLE dTilDato     AS DATE    NO-UNDO.
  DEFINE VARIABLE lFraMedlemNr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lTilMedlemNr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE cMsgs        AS CHARACTER NO-UNDO.

  ASSIGN
    dFraDato     = 01/01/2000
    dTilDato     = TODAY 
    lFraMedlemNr = 1
    lTilMedlemNr = 9999999999999
    .
    
  RUN genererrabsjekker.p (dFraDato,
                           dTilDato,
                           lFraMedlemNr,
                           lTilMedlemNr,
                           INPUT-OUTPUT iAntLest,
                           INPUT-OUTPUT iAntPostert,
                           OUTPUT cMsgs).
END PROCEDURE.


