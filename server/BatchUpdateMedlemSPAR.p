&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :  Batchuppdatering av medlemmar mot SPAR

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER ipLCdata AS LONGCHAR     NO-UNDO.
DEFINE INPUT  PARAMETER hCaller AS HANDLE      NO-UNDO.

DEFINE VARIABLE ii AS INTEGER     NO-UNDO.

/* SPAR oppslag */
DEFINE VARIABLE Persondetaljer_Fornamn AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Efternamn AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Fodelsetid AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Kon AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_Utdelningsadress2 AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_PostNr AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_Postort AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordLanKod AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordKommunKod AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordForsamlingKod AS CHAR NO-UNDO. 
DEFINE VARIABLE cErrorMessage AS CHAR NO-UNDO. 
DEFINE VARIABLE lPersonFunnet AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO. 

DEFINE BUFFER bufMedlem FOR medlem.
DEFINE VARIABLE cMedlemsNr AS CHARACTER   NO-UNDO.

DEFINE VARIABLE dFgRun   AS DATE        NO-UNDO.
DEFINE VARIABLE cDatoStr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cInfoStr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lRunUpdate AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cInfoDate AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

cInfoDate = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99").
DO  ii = 1 TO NUM-ENTRIES(ipLCdata):
    cMedlemsNr = ENTRY(ii,ipLCdata).
    FIND medlem WHERE medlem.medlemsnr = DECI(cMedlemsNr) NO-LOCK NO-ERROR.
    IF AVAIL medlem AND LENGTH(Medlem.PersonNr) = 10 THEN DO:
        IF ENTRY(1,Medlem.MedlemInfo) = "SPAR" THEN DO:
            cDatoStr = ENTRY(2,Medlem.MedlemInfo) NO-ERROR.
            IF LENGTH(cDatoStr) = 6 THEN DO:
                dFgRun = DATE(INT(SUBSTR(cDatoStr,5,2)),INT(SUBSTR(cDatoStr,7,2)),INT(SUBSTR(cDatoStr,1,4))) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR AND TODAY - dFgRun < 30 THEN
                    lRunUpdate = FALSE.
                ELSE
                    lRunUpdate = TRUE.
            END.
        END.
        ELSE
            lRunUpdate = TRUE.
        IF lRunUpdate THEN DO:
            cInfoStr  = "SPAR," + cInfoDate + ",00000000".
            RUN UpdateMedlem.
        END.
    END.
    RUN UpdateDone IN hCaller (ii) NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-UpdateMedlem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateMedlem Procedure 
PROCEDURE UpdateMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cPrefix AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPersonNr AS CHARACTER   NO-UNDO.
    cPrefix = IF YEAR(TODAY) - INT('19' + SUBSTRING(Medlem.PersonNr,1,2)) > 100
                     THEN "20"
                     ELSE "19".

      ASSIGN
          cErrorMessage = ''
          cPersonNr    = cPrefix + Medlem.PersonNr
          lPersonFunnet = FALSE
          lSuccess      = FALSE 
          .
      {sww.i}
      RUN SPAR_Personsokninfraga.p (cPersonNr, /* personnr */
        OUTPUT Persondetaljer_Fornamn,
        OUTPUT Persondetaljer_Efternamn,
        OUTPUT Persondetaljer_Fodelsetid,
        OUTPUT Persondetaljer_Kon,
        OUTPUT Folkbokforingsadress_Utdelningsadress2,
        OUTPUT Folkbokforingsadress_PostNr,
        OUTPUT Folkbokforingsadress_Postort,
        OUTPUT Folkbokforingsadress_FolkbokfordLanKod,
        OUTPUT Folkbokforingsadress_FolkbokfordKommunKod,
        OUTPUT Folkbokforingsadress_FolkbokfordForsamlingKod,
        OUTPUT cErrorMessage,
        OUTPUT lPersonFunnet,
        OUTPUT lSuccess).
      {swn.i}
      IF lSuccess AND lPersonFunnet THEN
      DO:
          FIND bufMedlem WHERE ROWID(bufMedlem) = ROWID(Medlem) EXCLUSIVE NO-WAIT NO-ERROR.
          IF AVAIL bufMedlem THEN DO:
              IF bufMedlem.ForNavn   <> Persondetaljer_Fornamn         OR
                 bufMedlem.Etternavn <> Persondetaljer_Efternamn       OR
                 bufMedlem.PostNr    <> Folkbokforingsadress_PostNr    OR
                 bufMedlem.Adresse1  <> Folkbokforingsadress_Utdelningsadress2 THEN DO:
                 ENTRY(3,cInfoStr) = cInfoDate.
                 ASSIGN
                     bufMedlem.ForNavn   = Persondetaljer_Fornamn
                     bufMedlem.Etternavn = Persondetaljer_Efternamn
                     bufMedlem.PostNr    = Folkbokforingsadress_PostNr
                     bufMedlem.Adresse1  = Folkbokforingsadress_Utdelningsadress2
                     .
              END.
          END.
          bufMedlem.MedlemInfo = cInfoStr.
          RELEASE bufMedlem.
      END.
      ELSE DO:
          FIND bufMedlem WHERE ROWID(bufMedlem) = ROWID(Medlem) EXCLUSIVE NO-WAIT NO-ERROR.
          bufMedlem.MedlemInfo = cInfoStr + "," + STRING(lPersonFunnet,"SPARfel/EJ hittad").
      END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

