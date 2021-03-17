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
DEFINE VARIABLE cReturnValue AS CHARACTER NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.

DEF VAR iIntegrasjon AS INT NO-UNDO.

{etikettlogg.i &NEW=NEW}

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-51 FI-KOrdreId Btn_OK FI-Navn ~
Btn_Cancel CB-Printer 
&Scoped-Define DISPLAYED-OBJECTS FI-KOrdreId FI-Navn CB-Printer FI-Txt1 

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

DEFINE VARIABLE CB-Printer AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Skriver" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEMS "Item" 
     DROP-DOWN-LIST
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KOrdreId AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Kundeordre" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Navn AS CHARACTER FORMAT "X(256)":U 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Txt1 AS CHARACTER FORMAT "X(256)":U INITIAL "Skriv ut postpakke etikett for kundeordre" 
      VIEW-AS TEXT 
     SIZE 59 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-51
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 64.4 BY 7.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     FI-KOrdreId AT ROW 1.43 COL 14.4 COLON-ALIGNED
     Btn_OK AT ROW 1.52 COL 50
     FI-Navn AT ROW 2.52 COL 14.4 COLON-ALIGNED
     Btn_Cancel AT ROW 2.62 COL 50
     CB-Printer AT ROW 4.67 COL 15 COLON-ALIGNED
     FI-Txt1 AT ROW 6.24 COL 3 COLON-ALIGNED NO-LABEL
     RECT-51 AT ROW 1.14 COL 2
     SPACE(0.03) SKIP(0.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Postpakke etikett"
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       FI-KOrdreId:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN FI-Txt1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Postpakke etikett */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  CASE iIntegrasjon:
      WHEN 1 THEN DO:
          RUN ekspWinEDI.p(FI-KOrdreId:SCREEN-VALUE + '|WinEDI' + '|' + CB-Printer:SCREEN-VALUE).
      END.
      WHEN 2 THEN DO:
          RUN ekspUniFaun.p(FI-KOrdreId:SCREEN-VALUE + '|UniFaun' + '|' + CB-Printer:SCREEN-VALUE).
      END.
      OTHERWISE DO:
          MESSAGE 'Ingen integrasjon er satt opp for postpakke etikettskriver.'
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
      END.
  END CASE.
  RUN SaveSettings.
  cReturnValue = 'OK'.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Integrasjonstype mot postpakke etikettskriver. */
{syspara.i 19 9 1 iIntegrasjon INT}
IF iIntegrasjon = 0 THEN
DO:
    MESSAGE "Det er ikke satt opp integrasjon mot postpakke etikett utskriftssystem." SKIP 
            "Sjekk systemparameter 19 9 1."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

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
           FI-Navn     = KordreHode.Navn.
  RUN GetLastPrinter.
  RUN InitCombo.
  {lng.i}
  RUN enable_UI.
  APPLY "VALUE-CHANGED" TO CB-Printer.  
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.
RETURN cReturnValue.

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
  DISPLAY FI-KOrdreId FI-Navn CB-Printer FI-Txt1 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-51 FI-KOrdreId Btn_OK FI-Navn Btn_Cancel CB-Printer 
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
    /*RUN HentParametre IN wLibHandle ("ETIKETTER", "SKRIVER", OUTPUT cSKRIVER).*/
    {syspara.i 210 100 7 cSkriver}

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
    ASSIGN cListItemPairs = SESSION:GET-PRINTERS( ).
    ASSIGN CB-Printer:LIST-ITEMS IN FRAME {&FRAME-NAME}= cListItemPairs.
    IF cSKRIVER <> ? AND CAN-DO(cListItemPairs,cSKRIVER) THEN
        CB-Printer = cSKRIVER.
    ELSE
        CB-Printer = ENTRY(1,cListItemPairs).
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
    RUN LagreParametre IN wLibHandle ("POSTETIKETTER", "LAYOUT", CB-Printer:SCREEN-VALUE).
    RUN LagreParametre IN wLibHandle ("POSTETIKETTER", "SKRIVER", CB-Printer:SCREEN-VALUE).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

