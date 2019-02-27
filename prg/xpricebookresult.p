&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEFINE VARIABLE cFilNavn AS CHARACTER  NO-UNDO.

DEFINE VARIABLE iButik AS INTEGER    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
/* DEF VAR cFilNavn      AS CHAR NO-UNDO. */

DEF VAR cType         AS CHAR NO-UNDO.
DEF VAR iRecType      AS INT  NO-UNDO. 
DEF VAR cTabellNavn   AS CHAR NO-UNDO. 
DEF VAR cRecVersion   AS CHAR NO-UNDO.
DEF VAR iAntRecord    AS INT  NO-UNDO.
DEF VAR cTblLst       AS CHAR NO-UNDO.
DEF VAR cTekst        AS CHAR NO-UNDO.

DEF STREAM InnFil.
DEFINE VARIABLE cPricebokkResultat AS CHARACTER  NO-UNDO.
DEFINE TEMP-TABLE TT_Exception NO-UNDO
    FIELD iRadnr AS INTE
    FIELD cDetail AS CHAR
    FIELD cId     AS CHAR
    FIELD cErrorCode AS CHAR
    FIELD cReason  AS CHAR
        INDEX iRadnr iRadnr.

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
DEFINE VARIABLE cRet_val AS CHARACTER  NO-UNDO.

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

IF SEARCH(cFilnavn) = ? THEN
    RETURN " ** Finner ikke filen på disk (" + STRING(lFilId) + ").".
IF ENTRY(NUM-ENTRIES(cFilNavn,"."),cFilNavn,".") = "xml" THEN DO:
    RUN LesInnFil.
    cRet_Val = RETURN-VALUE.
END.
ELSE
    cRet_val = "ERROR".

IF cRet_val = "ERROR" THEN 
    RETURN "ERROR".
ELSE 
    RETURN "".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE cDate AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTime AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSeq  AS CHARACTER  NO-UNDO.

DEFINE VARIABLE c2Detail  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c2Id      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE c2ErrCode AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFields    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dCampaignId AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iRad# AS INTEGER    NO-UNDO.
DEFINE VARIABLE hDoc AS HANDLE NO-UNDO.
DEFINE VARIABLE hRoot AS HANDLE NO-UNDO.
DEFINE VARIABLE hTable AS HANDLE NO-UNDO.
DEFINE VARIABLE hReceipt AS HANDLE NO-UNDO.
DEFINE VARIABLE hReceiptRows AS HANDLE NO-UNDO.
DEFINE VARIABLE hItemLine AS HANDLE NO-UNDO.
DEFINE VARIABLE hItemLineValues AS HANDLE NO-UNDO.
DEFINE VARIABLE hTenderLine AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTenderAmount AS HANDLE     NO-UNDO.
DEFINE VARIABLE hTenderAmountValue AS HANDLE     NO-UNDO.
DEFINE VARIABLE hField2 AS HANDLE NO-UNDO.
DEFINE VARIABLE hText2 AS HANDLE NO-UNDO.
DEFINE VARIABLE hBuf AS HANDLE NO-UNDO.
DEFINE VARIABLE hDBFld AS HANDLE NO-UNDO.
DEFINE VARIABLE hState AS HANDLE NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.
DEFINE VARIABLE k AS INTEGER NO-UNDO.
DEFINE VARIABLE hPricebook AS HANDLE     NO-UNDO.
DEFINE VARIABLE cName AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iButik AS INTEGER    NO-UNDO.
DEFINE VARIABLE cFilPrefix AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iSeqNr AS INTEGER    NO-UNDO.
DEFINE VARIABLE dKampId AS DECIMAL    NO-UNDO.
DEFINE VARIABLE lViHarData AS LOGICAL    NO-UNDO.
DEF VAR lOk AS LOGICAL NO-UNDO.

ASSIGN iButik = INT(ENTRY(2,VPIFilHode.FilNavn,"_")) NO-ERROR.

IF ERROR-STATUS:ERROR THEN DO:
    RETURN "OK".
END.
FIND butiker WHERE butiker.butik = iButik NO-LOCK NO-ERROR.
IF NOT AVAIL butiker THEN
    RETURN "OK".
cFilPrefix = ENTRY(1,VPIFilHode.FilNavn,".").
iSeqNr = INT(ENTRY(NUM-ENTRIES(cFilPrefix,"_"),cFilPrefix,"_")) NO-ERROR.

IF ERROR-STATUS:ERROR THEN
    RETURN "OK".
/* MESSAGE iSeqNr                         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/* RETURN "OK".                           */

FIND KampanjeButMottak WHERE KampanjeButMottak.Butik = iButik AND KampanjeButMottak.SekvNr = iSeqNr NO-LOCK NO-ERROR.
IF AVAIL KampanjeButMottak THEN
    dKampid = KampanjeButMottak.KampId.
IF dKampid > 0 AND CAN-FIND(KampanjeButikker WHERE KampanjeButikker.KampId = dKampId AND KampanjeButikker.Butik = iButik) THEN DO:
    lViHarData = TRUE.
    CREATE X-DOCUMENT hDoc.
    CREATE X-NODEREF hRoot.
    CREATE X-NODEREF hTable.
    /* CREATE X-NODEREF hReceipt.     */
    /* CREATE X-NODEREF hReceiptRows. */
    CREATE X-NODEREF hPricebook.
    /* CREATE X-NODEREF hItemLine.          */
    /* CREATE X-NODEREF hItemLineValues.    */
    /* CREATE X-NODEREF hTenderLine.        */
    /* CREATE X-NODEREF hTenderAmount.      */
    /* CREATE X-NODEREF hTenderAmountValue. */
    CREATE X-NODEREF hField2.
    CREATE X-NODEREF hText2.
    /* CREATE X-NODEREF hState.             */
    lOk = hDoc:LOAD ("file",cFilNavn, FALSE) NO-ERROR.
    IF lOK = FALSE THEN DO:
        RETURN "ERROR".
    END.
    IF NOT hDoc:GET-DOCUMENT-ELEMENT (hRoot) THEN
        RETURN "ERROR".
    IF hRoot:NAME <> "POSBOFile" THEN
        RETURN "ERROR".
    REPEAT i = 1 TO hRoot:NUM-CHILDREN:
        hRoot:GET-CHILD (hPricebook,i) NO-ERROR. /* Alla element */
        IF hPricebook:NAME = "Pricebook" THEN DO:
            cNAME = hPricebook:ATTRIBUTE-NAMES.
            IF CAN-DO(cName,"date") THEN
                cDate = hPricebook:GET-ATTRIBUTE ("date").
            IF CAN-DO(cName,"date") THEN
                cTime = hPricebook:GET-ATTRIBUTE ("time").
            IF CAN-DO(cName,"date") THEN
                cSeq = hPricebook:GET-ATTRIBUTE ("sequenceNumber").
            REPEAT j = 1 TO hPricebook:NUM-CHILDREN:
                hPricebook:GET-CHILD (hTable,j).
                IF CAN-DO("Result,Exception,Reason",hTable:NAME) THEN DO:
                    IF hTable:NAME = "Result" THEN DO:
                        hTable:GET-CHILD (hText2,1).
                        cPricebokkResultat = "Resultat: " + hText2:NODE-VALUE + " - Sekvensnummer: " + cSeq + " Datum: " + cDate + " Tid: " + cTime.
                    END.
                    ELSE IF hTable:NAME  = "Exception" THEN DO:
                        DO k = 1 TO hTable:NUM-CHILDREN:
                            hTable:GET-CHILD (hField2,k).
                            cFields = hTable:ATTRIBUTE-NAMES.
                            ASSIGN c2Detail  = IF CAN-DO(cFields,"detail") THEN hTable:GET-ATTRIBUTE ("detail") ELSE ""
                                   c2Id      = IF CAN-DO(cFields,"id") THEN hTable:GET-ATTRIBUTE ("id") ELSE ""
                                   c2ErrCode = IF CAN-DO(cFields,"errorCode") THEN hTable:GET-ATTRIBUTE ("errorCode") ELSE "".
                            IF hField2:NAME = "Reason" THEN DO:
                                hField2:GET-CHILD (hText2,1).
                                iRad# = iRad# + 1.
                                CREATE TT_Exception.
                                ASSIGN TT_Exception.iRadnr = iRad#
                                       TT_Exception.cDetail = c2Detail 
                                    TT_Exception.cId        = c2Id     
                                    TT_Exception.cErrorCode = c2ErrCode
                                    TT_Exception.cReason    = hText2:NODE-VALUE.
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.

        DELETE OBJECT hDoc NO-ERROR.
        DELETE OBJECT hRoot NO-ERROR.
        DELETE OBJECT hTable NO-ERROR.
        DELETE OBJECT hPricebook NO-ERROR.
    /*     DELETE OBJECT hReceipt NO-ERROR.           */
    /*     DELETE OBJECT hReceiptRows NO-ERROR.       */
    /*     DELETE OBJECT hItemLine NO-ERROR.          */
    /*     DELETE OBJECT hItemLineValues NO-ERROR.    */
    /*     DELETE OBJECT hTenderLine NO-ERROR.        */
    /*     DELETE OBJECT hTenderAmount NO-ERROR.      */
    /*     DELETE OBJECT hTenderAmountValue NO-ERROR. */
        DELETE OBJECT hField2 NO-ERROR.
        DELETE OBJECT hText2 NO-ERROR.
    /*     DELETE OBJECT hState NO-ERROR.             */
END.
  DO TRANSACTION:
      IF lViHarData THEN DO:
          FIND CURRENT KampanjeButMottak EXCLUSIVE.
          ASSIGN KampanjeButMottak.MottattDato = DATE(INT(ENTRY(2,cDate,"-")),INT(ENTRY(3,cDate,"-")),INT(ENTRY(1,cDate,"-")))
                 KampanjeButMottak.MottattTid  = (INT(ENTRY(1,cTime,":")) * 3600) + (INT(ENTRY(2,cTime,":")) * 60) + INT(ENTRY(3,cTime,":"))
                 KampanjeButMottak.OkMottatt   = NOT CAN-FIND(FIRST TT_Exception)
                 KampanjeButMottak.Resultat    = cPricebokkResultat.       

          IF KampanjeButMottak.Filnavn = "Kampanj" THEN DO:
              FIND KampanjeButikker WHERE KampanjeButikker.KampId = dKampId AND KampanjeButikker.Butik = iButik.
              ASSIGN KampanjeButikker.MottattDato = KampanjeButMottak.MottattDato
                     KampanjeButikker.MottattKl   = cTime
                     KampanjeButikker.Resultat    = IF CAN-FIND(FIRST TT_Exception) THEN cPricebokkResultat ELSE "OK".
          END.

      END.
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 5
          .
/*       IF pbOk = FALSE THEN                                                     */
/*       DO:                                                                      */
/*         ASSIGN                                                                 */
/*           cTekst = "** Feil ved oppdatering av fil."                           */
/*           .                                                                    */
/*         PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40").                         */
/*       END.                                                                     */
/*       ASSIGN                                                                   */
/*         cTekst = "Fil oppdatert (Antall linjer: " + STRING(iAntLinjer) + ").". */
/*         .                                                                      */
/*       PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").                            */
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.
/* OUTPUT TO "CLIPBOARD".                                                                                         */
/* PUT UNFORMATTED cPricebokkResultat SKIP.                                                                       */
/* FOR EACH TT_Exception:                                                                                         */
/*     PUT UNFORMATTED "  - Detail: " TT_Exception.cDetail " Id: " TT_Exception.cId " Errorcode:" cErrorCode SKIP */
/*                     "  - Reason: " TT_Exception.cReason SKIP(1).                                               */
/* END.                                                                                                           */
/* OUTPUT CLOSE.                                                                                                  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

