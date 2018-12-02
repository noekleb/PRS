&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xinnMAGOrder.p
    Purpose     :

    Syntax      :

    Description : Leser inn ordre fra BITS nettbutikk ePages.

    Author(s)   : Tom Nøkleby
    Created     : 19/5-09
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR pcLinje       AS CHAR NO-UNDO.
DEFINE VARIABLE cReturnStatus AS CHARACTER NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtVPI.
DEF STREAM UtVre.

DEFINE TEMP-TABLE tt_Error
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR.

DEFINE VARIABLE cInrec AS CHARACTER FORMAT "X(50)" NO-UNDO.
DEFINE VARIABLE cInrec2 AS CHARACTER FORMAT "X(50)"  NO-UNDO.
DEFINE VARIABLE cInrec3 AS CHARACTER FORMAT "X(50)"  NO-UNDO.
DEFINE VARIABLE ix AS INTEGER     NO-UNDO.

DEFINE TEMP-TABLE ReturnOrder NO-UNDO
    FIELD orderID AS CHAR FORMAT "X(15)"
    FIELD returnID AS CHAR
    FIELD note AS CHAR.
DEFINE TEMP-TABLE ReturnItem NO-UNDO
    FIELD orderID AS CHAR FORMAT "X(15)"
    FIELD returnID AS CHAR
    FIELD rownumber AS INT
    FIELD itemID AS CHAR
    FIELD sku AS CHAR FORMAT "X(15)"
    FIELD qty AS CHAR.


{windows.i}

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
         HEIGHT             = 13.95
         WIDTH              = 37.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
FOR EACH tt_Error:
  DELETE tt_Error.
END.
FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

RUN LesInnFil.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ErrorLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLogg Procedure 
PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF CAN-FIND(FIRST tt_Error) THEN 
DO:
  OUTPUT TO VALUE("Error.Txt").
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn SKIP
      .
    FOR EACH tt_Error:
      PUT UNFORMATTED tt_Error.Tekst SKIP.
    END.
  OUTPUT CLOSE.
  IF SEARCH("Error.Txt") <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH("Error.Txt"),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       

------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR piAntFeil AS INT  NO-UNDO.
  DEF VAR pcBkuFil  AS CHAR NO-UNDO.
  DEF VAR piLoop AS INT NO-UNDO.
  
  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
      
  /* Leser inn hele xml filen DOM. */
/*  RUN xmlReadBITSOrder.p (INPUT cFilNavn).
  ASSIGN cReturnStatus = RETURN-VALUE.*/
      
  IF cReturnStatus = 'ERROR' THEN
    DO:
      CREATE tt_Error.
      ASSIGN
        piAntFeil = piAntFeil + 1
        tt_Error.LinjeNr = piAntFeil
        tt_Error.Tekst   = "** Feil ved innlesning av fil " + cFilNavn + ".".
    END.

  INPUT STREAM Innfil FROM VALUE(cFilNavn) NO-ECHO.

  REPEAT:
    IMPORT STREAM Innfil UNFORMATTED cInrec.
  
    ASSIGN cInrec2 = ENTRY(1,cInrec,">").
    ASSIGN cInrec2 = TRIM(cInrec2).
    IF cInrec2 = "<returnRow" THEN
    DO:
       CREATE ReturnOrder.
       ASSIGN ix = 0.
    END.
    ELSE
      RUN Inlasning.
    END.
  
    INPUT STREAM Innfil CLOSE.
  
    FOR EACH ReturnOrder:
      RUN UpdateOrder.
  END.

  /* Stempler posten som innlest. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 5
          .
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

  IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

  ASSIGN
  pcBkuFil = VPIFilHode.Katalog + "~\bku" + "\" + 
             VPIFilHode.FilNavn
  .

  /* Sikrer at backup katalog finnes. */
  OS-CREATE-DIR value(VPIFilHode.Katalog + "~\bku").
  /* Flytter filen til backup katalog. */
  OS-COPY value(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) 
          value(pcBkuFil).
  /* Renser bort fil */
  IF SEARCH(pcBkuFil) <> ? THEN
  DO:
      /* Filen tas bort fra katalogen. */
      IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
          OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
  END.

END PROCEDURE.

PROCEDURE Inlasning:

    CASE cInrec2:
        WHEN "<orderID" THEN
            ASSIGN cInrec3 = ENTRY(2,cInrec,">")
                   cInrec2 = ENTRY(1,cInrec3,"<")
                   ReturnOrder.orderID = cInrec2.
        WHEN "<returnID" THEN
            ASSIGN cInrec3 = ENTRY(2,cInrec,">")
                   cInrec2 = ENTRY(1,cInrec3,"<")
                   ReturnOrder.returnID = cInrec2.
        WHEN "<note" THEN
            ASSIGN cInrec3 = ENTRY(2,cInrec,">")
                   cInrec2 = ENTRY(1,cInrec3,"<")
                   ReturnOrder.note = cInrec2.
        WHEN "<itemRow" THEN
        DO:
          CREATE ReturnItem.
          ASSIGN returnItem.orderID = ReturnOrder.orderID
                 returnItem.returnID = ReturnOrder.returnID.
          ASSIGN ix = ix + 1.
          ASSIGN returnItem.Rownumber = ix.
        END.
        WHEN "<itemID" THEN
            ASSIGN cInrec3 = ENTRY(2,cInrec,">")
                   cInrec2 = ENTRY(1,cInrec3,"<")
                   ReturnItem.itemID = cInrec2.
        WHEN "<sku" THEN
            ASSIGN cInrec3 = ENTRY(2,cInrec,">")
                   cInrec2 = ENTRY(1,cInrec3,"<")
                   ReturnItem.sku = cInrec2.
        WHEN "<qty" THEN
            ASSIGN cInrec3 = ENTRY(2,cInrec,">")
                   cInrec2 = ENTRY(1,cInrec3,"<")
                   ReturnItem.qty = cInrec2.
    END CASE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UpdateOrder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateOrder Procedure 
PROCEDURE UpdateOrder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


/*DEFINE TEMP-TABLE ReturnOrder NO-UNDO
    FIELD orderID AS CHAR FORMAT "X(15)"
    FIELD returnID AS CHAR
    FIELD note AS CHAR.
DEFINE TEMP-TABLE ReturnItem NO-UNDO
    FIELD orderID AS CHAR FORMAT "X(15)"
    FIELD returnID AS CHAR
    FIELD rownumber AS INT
    FIELD itemID AS CHAR
    FIELD sku AS CHAR FORMAT "X(15)"
    FIELD qty AS CHAR.*/


FIND FIRST KOrdreHode WHERE KOrdreHode.EkstOrdreNr = ReturnOrder.OrderID.
  IF AVAILABLE KOrdreHode THEN
  DO:
    FOR EACH ReturnItem WHERE ReturnItem.orderID = ReturnOrder.orderID:
      FIND FIRST KOrdreLinje WHERE KOrdreLinje.KOrdre_Id = KOrdreHode.KOrdre_Id AND
                                   KOrdreLinje.VareNr = ReturnItem.sku.
      IF AVAILABLE KOrdreLinje THEN
      DO:
        ASSIGN KOrdreLinje.Antall = KOrdreLinje.Antall - DEC(ReturnItem.qty).
      END.
    END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

