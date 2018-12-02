&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
DEFINE INPUT  PARAMETER dKordreId AS DECIMAL  NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE wCL              AS INTEGER    NO-UNDO.
DEFINE VARIABLE cStartEtiPrinter AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLAYOUT  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSKRIVER AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iKampanjeId     LIKE Kampanjehode.KampanjeId  NO-UNDO.

{etikettlogg.i &NEW=NEW}

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-51 Btn_OK FI-KOrdreId FI-Navn ~
Btn_Cancel FI-Notat FI-Telefon CB-Printer FI-AntEti 
&Scoped-Define DISPLAYED-OBJECTS FI-KOrdreId FI-Navn FI-Notat FI-Telefon ~
CB-Printer FI-AntEti 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE CB-Printer AS INTEGER FORMAT ">>9":U INITIAL 0 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FI-AntEti AS INTEGER FORMAT ">>9":U INITIAL 1 
     LABEL "Antall etiketter" 
     VIEW-AS FILL-IN 
     SIZE 5.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KOrdreId AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Kundeordre" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Navn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Notat AS CHARACTER FORMAT "X(25)":U 
     LABEL "Notat" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Telefon AS CHARACTER FORMAT "X(14)":U 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64.4 BY 7.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 1.38 COL 50
     FI-KOrdreId AT ROW 1.43 COL 14.4 COLON-ALIGNED
     FI-Navn AT ROW 2.52 COL 14.4 COLON-ALIGNED
     Btn_Cancel AT ROW 2.62 COL 50
     FI-Notat AT ROW 3.62 COL 14.2 COLON-ALIGNED
     FI-Telefon AT ROW 4.71 COL 14.4 COLON-ALIGNED
     CB-Printer AT ROW 6.19 COL 14.4 COLON-ALIGNED NO-LABEL
     FI-AntEti AT ROW 7.29 COL 40.8 COLON-ALIGNED
     RECT-51 AT ROW 1 COL 2
     SPACE(0.00) SKIP(0.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kundeordreetikett"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       FI-KOrdreId:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Kundeordreetikett */
DO:
  RUN SaveSettings.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Kundeordreetikett */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  IF FI-AntEti:SENSITIVE THEN DO:
    IF NOT INT(FI-AntEti:SCREEN-VALUE) > 0 THEN DO:
        MESSAGE "Registrer antall"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO FI-AntEti.
        RETURN NO-APPLY.
    END.
  END.
  RUN SkapaEtikettLogg.
  IF RETURN-VALUE <> "AVBRYT" THEN
      RUN x-etikettsend.w (INT(CB-Printer:SCREEN-VALUE)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{syspara.i 5 1 1 wCl INT}
FIND butiker NO-LOCK WHERE
  Butiker.Butik = wCl NO-ERROR.
IF NOT AVAILABLE Butiker THEN
  DO:
    MESSAGE "Sentrallager er ikke lagt opp!"
      VIEW-AS ALERT-BOX MESSAGE TITLE "Melding".
    RETURN NO-APPLY.
  END.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    FIND KOrdreHode WHERE KOrdreHode.KOrdre_Id = dKordreId NO-LOCK NO-ERROR.
    IF NOT AVAIL KOrdreHode THEN DO:
        MESSAGE "Finner ikke kundeordre"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    ASSIGN FI-KOrdreId = dKordreId
           FI-Navn     = KordreHode.Navn
           FI-Telefon  = Kordrehode.Telefon.
  RUN GetLastPrinter.
  RUN InitCombo.
  {lng.i}
  RUN enable_UI.
  APPLY "VALUE-CHANGED" TO CB-Printer.
  APPLY "ENTRY" TO FI-AntEti.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
  DISPLAY FI-KOrdreId FI-Navn FI-Notat FI-Telefon CB-Printer FI-AntEti 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-51 Btn_OK FI-KOrdreId FI-Navn Btn_Cancel FI-Notat FI-Telefon 
         CB-Printer FI-AntEti 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetLastPrinter Dialog-Frame 
PROCEDURE GetLastPrinter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(wLibHandle) THEN
  DO WITH FRAME default-frame:
    RUN HentParametre IN wLibHandle ("ETIKETTER", "LAYOUT",  OUTPUT cLAYOUT).
    RUN HentParametre IN wLibHandle ("ETIKETTER", "SKRIVER", OUTPUT cSKRIVER).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombo Dialog-Frame 
PROCEDURE InitCombo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE BUFFER bSysPara FOR SysPara.
    
    ASSIGN cListItemPairs = "".
    FOR EACH SysPara WHERE SysPara.SysHId = 5 AND 
                       SysPara.SysGr = 21 NO-LOCK:
/*         IF CAN-DO(REPLACE(SysPara.Parameter2,";",","),"START") THEN */
/*             NEXT.                                                   */
        IF CAN-DO(REPLACE(SysPara.Parameter2,";",","),"AVAIL") THEN DO:
            FIND bSysPara WHERE bSysPara.SysHId = 5 AND 
                                bSysPara.SysGr  = 20 AND
                                bSysPara.ParaNr = SysPara.ParaNr NO-LOCK NO-ERROR.
            IF AVAIL bSysPara THEN DO:
                ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
                       bSysPara.Parameter2 + "," + TRIM(STRING(bSysPara.ParaNr)).
            END.
        END.
    END.
    ASSIGN CB-Printer:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME}= cListItemPairs.
    IF cSKRIVER <> ? AND CAN-DO(cListItemPairs,cSKRIVER) THEN
        CB-Printer = INT(cSKRIVER).
    ELSE
        CB-Printer = INT(ENTRY(2,cListItemPairs)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveSettings Dialog-Frame 
PROCEDURE SaveSettings :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(wLibHandle) THEN
  DO WITH FRAME {&FRAME-NAME}:
    RUN LagreParametre IN wLibHandle ("ETIKETTER", "LAYOUT", ENTRY(LOOKUP(CB-Printer:SCREEN-VALUE,cListItemPairs) - 1,CB-Printer:LIST-ITEM-PAIRS)).
    RUN LagreParametre IN wLibHandle ("ETIKETTER", "SKRIVER", CB-Printer:SCREEN-VALUE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaEtikettLogg Dialog-Frame 
PROCEDURE SkapaEtikettLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iCount                 AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cInfoRad1              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoRad2              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoRad3              AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cInfoRad4              AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:

    ASSIGN cInfoRad1 = "K-ORDRE: " + STRING(dKordreId)
           cInfoRad2 = INPUT FI-Navn
           cInfoRad3 = INPUT FI-Notat
           cInfoRad4 = "TLF:" + INPUT FI-Telefon.
    DO iCount = 1 TO INPUT FI-AntEti:
          CREATE EtikettLogg.
          ASSIGN
            EtikettLogg.Butik     = iCount /* Det skal skrives ut i seqnr ordning. */
            EtikettLogg.Vg        = 0   
            EtikettLogg.LopNr     = 0
            EtikettLogg.Ant       = 1
            EtikettLogg.Storl     = "INFO"
            EtikettLogg.Bongtekst = cInfoRad1 + CHR(1) + cInfoRad2 + CHR(1) + cInfoRad3 + CHR(1) + cInfoRad4 + " " + STRING(iCount) + "(" + STRING(INPUT FI-AntEti) + ")"
            EtikettLogg.Pris      = 0
            EtikettLogg.Pris2     = 0
            EtikettLogg.SeqNr     = iCount.
    END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

