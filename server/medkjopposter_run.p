
/*------------------------------------------------------------------------
    File        : medkjopposter_run.p 
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
RUN bibl_logg.p ('medkjopposter', 'medkjopposter_run.p: WinCheduler Starter postering av medlemskjøp. ' + string(TIME,"HH:MM:SS")).

RUN lesMedlemsKjop.

ASSIGN lTid = TIME - lTid.
RUN bibl_logg.p ('medkjopposter', 'medkjopposter_run.p: WinCheduler Stoppet postering av medlemskjøp. ' + string(TIME,"HH:MM:SS") + ' Tidsbruk: ' + string(lTid,"HH:MM:SS")).

QUIT.

PROCEDURE lesMedlemsKjop:
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

  IF NOT VALID-HANDLE(h_PrisKo) THEN
      RUN PrisKo.p PERSISTENT SET h_PrisKo.

  ASSIGN
    dFraDato = TODAY - 2
    dTilDato = TODAY 
    lFraMedlemNr = 0
    lTilMedlemNr = 9999999999999
    .
    
  BLOKKEN:
  FOR EACH BongHode WHERE 
    BongHode.Dato      >= dFraDato     AND 
    BongHode.Dato      <= dTilDato     AND 
    BongHode.MedlemsNr >= lFraMedlemNr AND
    BongHode.MedlemsNr <= lTilMedlemNr AND
    BongHode.Makulert < 2:

    iAntLest = iAntLest + 1.

    /* posterer kjøpet */
    RUN posterMedlemsKjop.p (BongHode.B_Id, h_Prisko, INPUT-OUTPUT iAntPostert).

    STATUS DEFAULT 'Antall leste bonger: ' + STRING(iAntlest) + 
                   '. Antall postert: ' + STRING(iAntPostert) + '.'.

  END. /* BLOKKEN */

  IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.
END PROCEDURE.


