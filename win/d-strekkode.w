&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      cEndreStrekKode = om blank -> NYREG annars ændring av storlekskoppling.

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VARIABLE hParentHandle    AS HANDLE     NO-UNDO.
  DEFINE VARIABLE dArtikkelNr     LIKE ArtBas.ArtikkelNr INIT 14053 NO-UNDO.
  DEFINE VARIABLE iStrTypeId      LIKE StrType.StrTypeId INIT 12    NO-UNDO.
  DEFINE VARIABLE cEndreStrekKode AS CHARACTER           INIT ""  NO-UNDO.
  DEFINE VARIABLE iKodeType       LIKE StrekKode.KodeType    NO-UNDO.
/*   DEFINE VARIABLE cEndreStrekKode AS CHARACTER           INIT "0312312312314"  NO-UNDO. */
&ELSE
  DEFINE INPUT  PARAMETER hParentHandle AS HANDLE     NO-UNDO.
  DEFINE INPUT  PARAMETER dArtikkelNr   LIKE ArtBas.ArtikkelNr NO-UNDO.
  DEFINE INPUT  PARAMETER iStrTypeId    LIKE StrType.StrTypeId NO-UNDO.
  DEFINE INPUT  PARAMETER cEndreStrekKode AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER iKodeType     LIKE StrekKode.KodeType NO-UNDO.
/*   DEFINE INPUT  PARAMETER lPLU AS LOGICAL    NO-UNDO. */
&ENDIF
DEFINE VARIABLE cEnStr        AS CHARACTER  NO-UNDO. /* vid 1 storlek */
/* Local Variable Definitions ---                                       */
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  {runlib.i}

DEF VAR iLoop AS INT NO-UNDO.
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE obOk     AS LOG       NO-UNDO.
DEFINE VARIABLE cTidskrift    AS CHARACTER   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-55 B-OK RS-Type BtnCancel 
&Scoped-Define DISPLAYED-OBJECTS RS-Type FI-PLU FI-StrekKode FI-Storl 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-LagreNy 
     LABEL "&Lagre/Ny" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-OK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Avslutt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-PLU AS INTEGER FORMAT ">>>>>>z":U INITIAL 0 
     LABEL "PLU" 
     VIEW-AS FILL-IN 
     SIZE 12.4 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Storl AS CHARACTER FORMAT "X(10)":U 
     LABEL "Angi størrelse" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE FI-StrekKode AS CHARACTER FORMAT "x(13)":U 
     LABEL "EAN/UPC" 
     VIEW-AS FILL-IN 
     SIZE 26 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Type AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "PLU", "0",
"EAN", "1",
"UPC", "2"
     SIZE 40 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 6.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-OK AT ROW 1.48 COL 50.2
     RS-Type AT ROW 1.71 COL 6 NO-LABEL
     B-LagreNy AT ROW 2.71 COL 50.2
     FI-PLU AT ROW 2.91 COL 18.6 COLON-ALIGNED
     FI-StrekKode AT ROW 4.33 COL 18.6 COLON-ALIGNED
     FI-Storl AT ROW 5.76 COL 18.6 COLON-ALIGNED
     BtnCancel AT ROW 6.19 COL 50.6
     RECT-55 AT ROW 1.43 COL 2
     SPACE(18.19) SKIP(0.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registrer strekkode/størrelse"
         DEFAULT-BUTTON B-OK CANCEL-BUTTON BtnCancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-LagreNy IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-PLU IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Storl IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-StrekKode IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Registrer strekkode/størrelse */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LagreNy
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LagreNy Dialog-Frame
ON CHOOSE OF B-LagreNy IN FRAME Dialog-Frame /* Lagre/Ny */
DO:
  IF TRIM(FI-Storl:SCREEN-VALUE) = '' OR
     FI-Storl:SCREEN-VALUE = ? OR
     TRIM(FI-Storl:SCREEN-VALUE) = '0' THEN 
  DO:
    MESSAGE 'Ugyldig størrelse angitt.'
    VIEW-AS ALERT-BOX.
    RETURN NO-APPLY.
  END. 

  RUN Lagre.
  ASSIGN 
/*       FI-StrekKode:FORMAT       = IF RS-Type:SCREEN-VALUE = "2" THEN "x(12)" ELSE "x(13)" */
         FI-StrekKode:SCREEN-VALUE = ""
         FI-StrekKode:SENSITIVE = TRUE
         B-OK:SENSITIVE = FALSE
         SELF:SENSITIVE = FALSE.
/*   APPLY "ENTRY" TO FI-StrekKode. */
      APPLY "VALUE-CHANGED" TO FI-StrekKode.
      APPLY "VALUE-CHANGED" TO RS-Type.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-OK Dialog-Frame
ON CHOOSE OF B-OK IN FRAME Dialog-Frame /* OK */
DO:
    IF TRIM(FI-Storl:SCREEN-VALUE) = '' OR
       FI-Storl:SCREEN-VALUE = ? OR
       TRIM(FI-Storl:SCREEN-VALUE) = '0' THEN 
    DO:
      MESSAGE 'Ugyldig størrelse angitt.'
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
    END. 
    /* knappen skall ha no-focus */
    IF FOCUS:NAME = "FI-PLU" THEN DO:
        RUN KontrollerInput.
        IF RETURN-VALUE = "AVBRYT" THEN
            RETURN NO-APPLY.
    END.
    RUN Lagre.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-PLU
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-PLU Dialog-Frame
ON RETURN OF FI-PLU IN FRAME Dialog-Frame /* PLU */
DO:
  IF B-OK:SENSITIVE = TRUE THEN DO:
      RUN KontrollerInput.
      IF RETURN-VALUE <> "AVBRYT" THEN
      DO:
          RUN InputPLUOk.
      END.
      /*APPLY "CHOOSE" TO B-OK.*/
      IF SELF:SCREEN-VALUE <> "" THEN
          RETURN NO-APPLY.
      ELSE
          RETURN.
  END.
  ELSE
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-PLU Dialog-Frame
ON VALUE-CHANGED OF FI-PLU IN FRAME Dialog-Frame /* PLU */
DO:
    IF SELF:SCREEN-VALUE BEGINS "0" THEN DO:
        MESSAGE "PLU-kode kann ikke starte med '0'"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        SELF:SCREEN-VALUE = "".
        RETURN NO-APPLY.
    END.
    ASSIGN B-OK:SENSITIVE    = LENGTH(SELF:SCREEN-VALUE) > 0
           RS-Type:SENSITIVE = LENGTH(SELF:SCREEN-VALUE) = 0.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Storl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Storl Dialog-Frame
ON LEAVE OF FI-Storl IN FRAME Dialog-Frame /* Angi størrelse */
OR "TAB" OF FI-Storl OR "RETURN" OF FI-Storl
DO:
    IF SELF:SCREEN-VALUE = ? OR TRIM(SELF:SCREEN-VALUE) = '' THEN
    DO:
        MESSAGE 'Ugyldig størrelse.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    
    SELF:SCREEN-VALUE = REPLACE(CAPS(SELF:SCREEN-VALUE),',','.').  
    
    RUN bibl_fixstorl.p (FI-Storl:SCREEN-VALUE,?,'',OUTPUT ocReturn,OUTPUT obOk).
    FI-Storl:SCREEN-VALUE = ocReturn.
    FIND StrKonv NO-LOCK WHERE
      StrKonv.Storl = ocReturn NO-ERROR.

    IF NOT AVAILABLE StrKonv THEN 
    DO:
      MESSAGE 'Ugyldig størrelse. Ikke definert i størrelsestabellen.'
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
    END.
    /*
    ELSE DO:
      IF iStrTypeId > 2 THEN
          RUN opprett_str_i_StrType.p (StrKonv.Storl,iStrTypeId).
    END. 
    */
    IF cEndreStrekKode = "" THEN DO:
        ASSIGN B-OK:SENSITIVE      = TRUE
               B-LagreNy:SENSITIVE = TRUE.
        APPLY "CHOOSE" TO B-LagreNy.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        ASSIGN B-OK:SENSITIVE = (SELF:SCREEN-VALUE) <> ''.
        APPLY "CHOOSE" TO B-OK.
        RETURN NO-APPLY.
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-StrekKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-StrekKode Dialog-Frame
ON ANY-PRINTABLE OF FI-StrekKode IN FRAME Dialog-Frame /* EAN/UPC */
DO:
        IF NOT CAN-DO("13,48,49,50,51,52,53,54,55,56,57,58,59",STRING(KEYCODE(KEYFUNCTION(LASTKEY)))) THEN
        RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-StrekKode Dialog-Frame
ON LEAVE OF FI-StrekKode IN FRAME Dialog-Frame /* EAN/UPC */
OR "RETURN" OF FI-StrekKode OR "TAB" OF FI-StrekKode DO:
  IF (RS-Type:SCREEN-VALUE = "1" AND (LENGTH(SELF:SCREEN-VALUE) = 13 OR LENGTH(SELF:SCREEN-VALUE) = 8)) OR 
     (RS-Type:SCREEN-VALUE = "2" AND (LENGTH(SELF:SCREEN-VALUE) = 12 OR LENGTH(SELF:SCREEN-VALUE) = 6)) THEN DO:
      RUN KontrollerInput.
      IF RETURN-VALUE <> "AVBRYT" THEN DO:
          RUN InputOk.
      END.
      ELSE .
  END.
  IF SELF:SCREEN-VALUE <> "" THEN
      RETURN NO-APPLY.
  ELSE
      RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-StrekKode Dialog-Frame
ON VALUE-CHANGED OF FI-StrekKode IN FRAME Dialog-Frame /* EAN/UPC */
DO:
    RS-Type:SENSITIVE = LENGTH(SELF:SCREEN-VALUE) = 0.
END.

/* 
     IF LENGTH(CLIPBOARD:VALUE) = 13 THEN
        ASSIGN SELF:SCREEN-VALUE = CLIPBOARD:VALUE.
    APPLY "VALUE-CHANGED" TO SELF.
  RETURN NO-APPLY.
 */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Type
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Type Dialog-Frame
ON VALUE-CHANGED OF RS-Type IN FRAME Dialog-Frame
DO:
    ASSIGN FI-PLU:SENSITIVE       = RS-Type:SCREEN-VALUE = "0"
           FI-StrekKode:SENSITIVE = RS-Type:SCREEN-VALUE <> "0".
    IF FI-StrekKode:SENSITIVE THEN
        ASSIGN FI-StrekKode:FORMAT = IF RS-Type:SCREEN-VALUE = "1" THEN "x(13)" ELSE "x(12)"
               FI-StrekKode:LABEL  = IF RS-Type:SCREEN-VALUE = "1" THEN "EAN" ELSE "UPC".
               .
    IF FI-PLU:SENSITIVE = TRUE THEN
        APPLY "ENTRY" TO FI-PLU.
    ELSE
        APPLY "ENTRY" TO FI-StrekKode.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */

ON ALT-O OF FRAME Dialog-Frame ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO B-OK IN FRAME Dialog-Frame.
  END.
ON ALT-L OF FRAME Dialog-Frame ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO B-LagreNy IN FRAME Dialog-Frame.
  END.

/* Sjekker om tidskrift koder skal konverteres. */
{syspara.i 2 4 23 cTidskrift}
IF cTidskrift = "0" THEN
    cTidskrift = "".

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  RUN InitStrType.
  ASSIGN FI-StrekKode = cEndreStrekKode
/*          cKodeType = IF iKodeType = 0 THEN "PLU" ELSE IF iKodeType = 1 THEN "EAN" */
/*                         ELSE IF iKodeType = 2 THEN "UPC" ELSE "ERROR"             */
         RS-Type   = "1".

  ASSIGN FI-StrekKode:LABEL   = "EAN".

    /* NYREG */
  IF cEndreStrekKode <> "" THEN DO:
      FIND StrekKode WHERE StrekKode.Kode = cEndreStrekKode NO-LOCK NO-ERROR.
  END.
  ELSE
      ASSIGN B-LagreNy:DEFAULT = TRUE
      FRAME {&FRAME-NAME}:DEFAULT-BUTTON = B-LagreNy:HANDLE.

  RUN enable_UI.
  IF cEndreStrekKode <> "" THEN DO: 
      ASSIGN RS-Type:SENSITIVE = FALSE.
  END.
  ELSE
      APPLY "VALUE-CHANGED" TO RS-Type.

  ASSIGN FI-Storl:SENSITIVE  = TRUE. 
  IF cEndreStrekKode <> "" THEN APPLY 'ENTRY' TO FI-Storl IN FRAME {&FRAME-NAME}.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultVerdier Dialog-Frame 
PROCEDURE DefaultVerdier :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EkstVPINrListe Dialog-Frame 
PROCEDURE EkstVPINrListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcEkstVPILEvNrListe AS CHAR NO-UNDO.

  FOR EACH EkstVPILev NO-LOCK WHERE
      EkstVPILev.AktivLev = TRUE
      BREAK BY EkstVPILev.Prio:

      pcEkstVPILEvNrListe = pcEkstVPILEvNrListe + 
                            (IF pcEkstVPILEvNrListe = ""
                               THEN ""
                               ELSE ",") +
                            STRING(EkstVPILev.EkstVPILevNr).
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY RS-Type FI-PLU FI-StrekKode FI-Storl 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-55 B-OK RS-Type BtnCancel 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitStrType Dialog-Frame 
PROCEDURE InitStrType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   IF iStrTypeId > 1 /* AND NOT lPLU */ THEN DO:
       FOR EACH StrTstr WHERE StrTstr.StrTypeId = iStrTypeId NO-LOCK:
           FIND StrKonv WHERE StrKonv.Storl = StrTstr.SoStorl NO-LOCK NO-ERROR.
           IF AVAIL StrKonv THEN
               ASSIGN cListItemPairs = cListItemPairs +
                                 (IF cListItemPairs = "" THEN "" ELSE ",") +
                          TRIM(StrKonv.Storl) + "," + STRING(StrKonv.StrKode).
       END.
       IF NUM-ENTRIES(cListItemPairs) = 2 THEN
           ASSIGN cEnStr = ENTRY(2,cListItemPairs).
       ASSIGN cListItemPairs = "(0),0," + cListItemPairs.
   END.
   ELSE
       ASSIGN cListItemPairs = ",".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InputOk Dialog-Frame 
PROCEDURE InputOk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN FI-StrekKode:SENSITIVE  = FALSE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InputPLUOk Dialog-Frame 
PROCEDURE InputPLUOk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN FI-PLU:SENSITIVE     = FALSE.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerInput Dialog-Frame 
PROCEDURE KontrollerInput :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE cEANfromUPC AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cEAN13      AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cEAN13Tmp   AS CHARACTER  NO-UNDO.

   DEF VAR piLoop  AS INT  NO-UNDO.
   DEF VAR pcListe AS CHAR NO-UNDO.
   DEF VAR pbOk    AS LOG  NO-UNDO.
   
   RUN EkstVPINrListe (OUTPUT pcListe).
   
   DO WITH FRAME {&FRAME-NAME}:
     /* 29-koder används som 'medlemsnummerEAN' */
     IF RS-Type:SCREEN-VALUE = "1" AND FI-StrekKode:SCREEN-VALUE BEGINS "29" AND
                              LENGTH(FI-StrekKode:SCREEN-VALUE) = 13 THEN DO:
         MESSAGE "Strekkode som begynner med '29' kan ikke registreres manuellt." SKIP
                 "Denne nummerserien er avsatt til medlemsnummer. "
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN "AVBRYT".
     END.
     ELSE IF RS-Type:SCREEN-VALUE = "2" AND FI-StrekKode:SCREEN-VALUE BEGINS "2" AND 
                                   LENGTH(FI-StrekKode:SCREEN-VALUE) = 6 THEN DO:
         MESSAGE "UPC-kode som begynner med '2' kan ikke registreres."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN "AVBRYT".
     END.
     
     ELSE IF RS-Type:SCREEN-VALUE = "1" THEN 
     DO:
       ASSIGN cEAN13 = FILL("0",13 - LENGTH(FI-StrekKode:SCREEN-VALUE)) + FI-StrekKode:SCREEN-VALUE.
       RUN bibl_chkean.p (INPUT-OUTPUT cEAN13).
       IF RETURN-VALUE <> '' THEN 
         DO:
           MESSAGE RETURN-VALUE
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
           RETURN "AVBRYT".
         END.
       ELSE FI-StrekKode:SCREEN-VALUE = cEAN13.
     END.
     
     ELSE IF RS-Type:SCREEN-VALUE = "2" THEN 
     DO:
         ASSIGN cEANfromUPC = FI-StrekKode:SCREEN-VALUE.
         IF LENGTH(cEANfromUPC) = 12 THEN 
         DO:
             ASSIGN cEANfromUPC = "0" + cEANfromUPC.
             IF DYNAMIC-FUNCTION('fixChkEAN':U IN h_dproclib,
                INPUT SUBSTR(cEANfromUPC,1,12)) <> cEANfromUPC THEN 
             DO:
                 MESSAGE "Feil sjekksiffre"
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
                 RETURN "AVBRYT".
             END.
         END.
         ELSE DO:
             ASSIGN cEANfromUPC = DYNAMIC-FUNCTION('EkspanderUPC':U IN h_dproclib, INPUT cEANfromUPC)
                    cEANfromUPC = DYNAMIC-FUNCTION('fixChkEAN':U IN h_dproclib, INPUT cEANfromUPC).
         END.
         ASSIGN FI-StrekKode:FORMAT       = "x(13)"
                FI-StrekKode:SCREEN-VALUE = cEANfromUPC.
     END.

     FIND StrekKode WHERE StrekKode.Kode = IF RS-Type:SCREEN-VALUE = "2" THEN 
                                               FI-PLU:SCREEN-VALUE 
                                           ELSE
                                               FI-StrekKode:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAIL StrekKode THEN 
     DO:
          FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
          MESSAGE "Koden er brukt fra før på artikkel:" SKIP
                  "ArtikkelNr.....:" StrekKode.ArtikkelNr SKIP
                  "Lev.artNr......:" IF AVAILABLE ArtBAs THEN ArtBas.LevKod ELSE '' SKIP
                  "Varetekst......:" IF AVAILABLE ArtBAs THEN ArtBas.Beskr ELSE '' SKIP
                  "Lev.fargekode..:" IF AVAILABLE ArtBAs THEN ArtBas.LevFargKod ELSE '' SKIP
                  "Strekkode......:" Strekkode.Kode SKIP
                  "Bestillingsnr..:" Strekkode.Bestillingsnummer SKIP
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
     END.
     /* Kontrollerer mot VPI registeret. */
     IF RS-Type:SCREEN-VALUE <> "0" AND RS-Type:SCREEN-VALUE <> "2" AND pcListe <> "" THEN 
     DO piLoop = 1 TO NUM-ENTRIES(pcListe):
         FIND FIRST VPIStrekKode WHERE 
                    VPIStrekkode.EkstVPILevNr = INT(ENTRY(piLoop,pcListe)) AND
                    VPIStrekkOde.Kode = FI-StrekKode:SCREEN-VALUE NO-LOCK NO-ERROR.
/*          IF AVAIL vpistrekkode  THEN                     */
/*              MESSAGE PROGRAM-NAME(1) SKIP                */
/*              VPIStrekkode.EkstVPILevNr SKIP "SKITT" SKIP */
/*              VPIStrekkOde.Kode                           */
/*                  VIEW-AS ALERT-BOX INFO BUTTONS OK.      */
         IF AVAILABLE VPISTrekkode THEN
             FIND VPIArtBas OF VPIStrekkode NO-LOCK NO-ERROR.

         IF NOT AVAILABLE VPISTrekkode THEN
             FIND FIRST VPIArtbas WHERE 
                        VPIArtBas.EkstVPILevNr = INT(ENTRY(piLoop,pcListe)) AND
                        VPIArtbas.VareNr    = FI-StrekKode:SCREEN-VALUE NO-LOCK NO-ERROR.
         IF AVAIL VPIArtbas THEN 
         DO:   /* tidigare stod det bara DO: */
           pbOk = FALSE.
           MESSAGE "Koden finnes i VPI-registeret til VPI leverandør " + entry(piLoop,pcListe) + "." SKIP
                   "Varetekst: " VPIArtBas.Beskr " StrType: " VPIArtBas.StrType SKIP(1)
                   "Vil du alikevel legge den opp på denne artikkelen?"
               VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL 
               UPDATE pbOk.
           IF pbOk <> TRUE THEN
               RETURN "AVBRYT".
           ELSE
               RETURN "".
         END.
     END.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Lagre Dialog-Frame 
PROCEDURE Lagre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE bOk         AS LOG NO-UNDO.
  DEFINE VARIABLE iOldStrKode AS INTEGER NO-UNDO.
  DEFINE VARIABLE iNyStrKode  AS INTEGER NO-UNDO.

  DEF VAR cTekst AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    FIND StrKonv NO-LOCK WHERE
         StrKonv.Storl = FI-Storl:SCREEN-VALUE NO-ERROR.
    IF NOT AVAILABLE StrKonv THEN
    DO:
        MESSAGE 'Ugyldig størrelse. Finnes ikke i størrelsestabell.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    IF AVAILABLE StrKonv THEN 
        iNyStrKode = StrKonv.StrKode.

    IF cEndreStrekKode = "" THEN DO:
        CREATE StrekKode.
        ASSIGN StrekKode.ArtikkelNr = dArtikkelNr
               StrekKode.Kode       = IF RS-Type:SCREEN-VALUE = "0" THEN FI-PLU:SCREEN-VALUE ELSE FI-StrekKode:SCREEN-VALUE
               StrekKode.KodeType   = IF RS-Type:SCREEN-VALUE = "0" THEN 0 ELSE 1
               Strekkode.StrKode    = iNyStrKode
               /*
               StrekKode.StrKode    = IF iStrTypeId > 1 THEN INT(CB-Storl:SCREEN-VALUE)
                                         ELSE StrekKode.StrKode
               */
               StrekKode.StrKode    = IF /* lPLU AND */ StrekKode.StrKode = ? THEN 0 ELSE StrekKode.StrKode.
        ASSIGN StrekKode.VareId     = dArtikkelNr NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            ASSIGN StrekKode.VareId = 0.
        IF VALID-HANDLE(hParentHandle) THEN
            RUN OpenQueryRep IN hParentHandle (ROWID(StrekKode)).
        FI-Storl:SCREEN-VALUE = ''.
    END.
    ELSE DO:
        /* Det er endret størrelse på strekkoden. Hvis det ligger lager, skal det spørres om */
        /* lager skal flyttes med til den nye størrelsen.                                    */
        IF StrekKode.StrKode <> iNyStrKode /*INT(CB-Storl:SCREEN-VALUE)*/ THEN 
        BEKREFT_FLYTT_LAGER:
        DO:
          bOk = FALSE.
          /* Positivt lager ? */
          FIND FIRST ArtLag NO-LOCK WHERE
            ArtLag.ArtikkelNr = StrekKode.ArtikkelNr AND
            ArtLag.butik      > 0 AND 
            ArtLag.StrKode    = StrekKode.StrKode AND 
            ArtLag.lagant     > 0 NO-ERROR.
          IF NOT AVAILABLE ArtLag THEN 
          /* Positivt lager ? */
          FIND FIRST ArtLag NO-LOCK WHERE
            ArtLag.ArtikkelNr = StrekKode.ArtikkelNr AND
            ArtLag.butik      > 0 AND 
            ArtLag.StrKode    = StrekKode.StrKode AND 
            ArtLag.lagant     < 0 NO-ERROR.
          IF AVAILABLE ArtLag THEN
          DO:
            MESSAGE 'Strekkode er flyttet til en annen størrelse. ' SKIP
                    'Det ligger et lager <> 0 på den gamle størrelsen på en eller flere butikker.' SKIP 
                    'Skal lageret flyttes med til den nye størrelsen (Vil gjelde for alle butikker)?'
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE bOk .
            
          END. 
        END. /* BEKREFT_FLYTT_LAGER */
        
        ASSIGN iOldStrKode = StrekKode.StrKode
               /*iNyStrKode  = INT(CB-Storl:SCREEN-VALUE)*/.        

        IF bOk AND (iOldStrKode <> iNyStrKode) THEN 
            RUN bibl_flytt_lager_str.p (Strekkode.Kode, iNyStrKode).
        
        FIND CURRENT StrekKode EXCLUSIVE-LOCK.
        ASSIGN StrekKode.StrKode = iNyStrKode /*INT(CB-Storl:SCREEN-VALUE)*/.
        FIND CURRENT StrekKode NO-LOCK.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

