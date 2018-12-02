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
DEFINE INPUT  PARAMETER ipStrTypeId LIKE StrType.StrTypeId NO-UNDO.
DEFINE OUTPUT PARAMETER cStorrelser AS CHARACTER           NO-UNDO.
DEFINE OUTPUT PARAMETER iLandkode   AS INT                 NO-UNDO.
/* Local Variable Definitions ---                                       */
/* DEFINE VARIABLE ipStrTypeId AS INTEGER INIT 22   NO-UNDO. */
DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisToggle000 AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-Angre RS-Nummerserie CB-Fra CB-Til ~
TG-Kun000 Btn_OK Btn_Cancel RECT-55 
&Scoped-Define DISPLAYED-OBJECTS RS-Nummerserie CB-Fra CB-Til TG-Kun000 

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

DEFINE BUTTON BUTTON-Angre 
     IMAGE-UP FILE "icon/reset.bmp":U NO-FOCUS
     LABEL "&Angre" 
     SIZE 4.6 BY 1.05 TOOLTIP "Angre".

DEFINE VARIABLE CB-Fra AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fra" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Til AS CHARACTER FORMAT "X(256)":U 
     LABEL "Til" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Nummerserie AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Bedriftsintern EAN13 nr.serie", 1,
"Egen EAN13 nr. serie", 10,
"Bedriftsintern EAN8 nr. serie", 11,
"Egen EAN8 nr. serie", 12
     SIZE 112.2 BY .95 TOOLTIP "Styrer om EAN koder skal hentes fra Eegen serie eller serie med bedriftsinterne" NO-UNDO.

DEFINE RECTANGLE RECT-55
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 123 BY 4.05.

DEFINE VARIABLE TG-Kun000 AS LOGICAL INITIAL no 
     LABEL "Kun '000'" 
     VIEW-AS TOGGLE-BOX
     SIZE 15.8 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BUTTON-Angre AT ROW 2.43 COL 59.8 NO-TAB-STOP 
     RS-Nummerserie AT ROW 1.29 COL 10.8 NO-LABEL
     CB-Fra AT ROW 2.43 COL 8.6 COLON-ALIGNED
     CB-Til AT ROW 2.43 COL 36.2 COLON-ALIGNED
     TG-Kun000 AT ROW 3.67 COL 38.4
     Btn_OK AT ROW 2.48 COL 107.8
     Btn_Cancel AT ROW 3.71 COL 107.8
     RECT-55 AT ROW 1 COL 1
     SPACE(0.79) SKIP(0.04)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Størrelser til Ean"
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Størrelser til Ean */
DO:
    DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
    IF TG-Kun000:CHECKED THEN
        cStorrelser = "000".
    ELSE DO iCount = LOOKUP(CB-Fra:SCREEN-VALUE,CB-Fra:LIST-ITEM-PAIRS) TO 
                LOOKUP(CB-Til:SCREEN-VALUE,CB-Fra:LIST-ITEM-PAIRS) BY 2:
      ASSIGN cStorrelser = cStorrelser + 
                  (IF cStorrelser = "" THEN "" ELSE ",") +
             ENTRY(iCount,CB-Fra:LIST-ITEM-PAIRS).
    END.
    ASSIGN cStorrelser = REPLACE(cStorrelser,"<","").
    /* Här trimmar vi bort '<' */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Størrelser til Ean */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
    DO WITH FRAME Dialog-Frame:
        ASSIGN
            iLandKode = INPUT RS-Nummerserie.

        CASE iLandKode:
            WHEN 1 THEN
            DO:
                IF NOT CAN-FIND(FIRST EANNrSerie WHERE
                                EANNrSerie.EANType = 13 AND
                                EANNrSerie.EANLandKode = 2 AND
                                EANNrSerie.EANSerieAktiv) THEN
                DO:
                    MESSAGE "Det finnes ingen aktive EAN13 nummerserie med bedriftsinterne EAN koder." SKIP
                            "Eventuelt er nummerserien full." SKIP(1)
                            "Bruk EAN nummerserieregisteret for å legge opp en ny serie."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    RETURN NO-APPLY.
                END.
            END.
            WHEN 10 THEN
            DO:
                IF NOT CAN-FIND(FIRST EANNrSerie WHERE
                                EANNrSerie.EANType = 13 AND
                                EANNrSerie.EANLandKode > 2 AND
                                EANNrSerie.EANSerieAktiv) THEN
                DO:
                    MESSAGE "Det finnes ingen aktive EAN13 nummerserie med egne EAN koder." SKIP
                            "Eventuelt er nummerserien full." SKIP(1)
                            "Bruk EAN nummerserieregisteret for å legge opp en ny serie."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    RETURN NO-APPLY.
                END.
            END.
            WHEN 11 THEN
            DO:
                IF NOT CAN-FIND(FIRST EANNrSerie WHERE
                                EANNrSerie.EANType = 8 AND
                                EANNrSerie.EANLandKode > 2 AND
                                EANNrSerie.EANSerieAktiv) THEN
                DO:
                    MESSAGE "Det finnes ingen aktive EAN13 nummerserie med bedritsinterne EAN koder." SKIP
                            "Eventuelt er nummerserien full." SKIP(1)
                            "Bruk EAN nummerserieregisteret for å legge opp en ny serie."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    RETURN NO-APPLY.
                END.
            END.
            WHEN 12 THEN
            DO:
                IF NOT CAN-FIND(FIRST EANNrSerie WHERE
                                EANNrSerie.EANType = 8 AND
                                EANNrSerie.EANLandKode = 2 AND
                                EANNrSerie.EANSerieAktiv) THEN
                DO:
                    MESSAGE "Det finnes ingen aktive EAN13 nummerserie med egne EAN koder." SKIP
                            "Eventuelt er nummerserien full." SKIP(1)
                            "Bruk EAN nummerserieregisteret for å legge opp en ny serie."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    RETURN NO-APPLY.
                END.
            END.
        END CASE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Angre Dialog-Frame
ON CHOOSE OF BUTTON-Angre IN FRAME Dialog-Frame /* Angre */
DO:
    ASSIGN CB-Fra:LIST-ITEM-PAIRS = cListItemPairs
           CB-Til:LIST-ITEM-PAIRS = cListItemPairs
           CB-Fra:SCREEN-VALUE = ENTRY(2,CB-Fra:LIST-ITEM-PAIRS)
           CB-Til:SCREEN-VALUE = ENTRY(2,CB-Til:LIST-ITEM-PAIRS).
           
    OUTPUT TO "CLIPBOARD".
    PUT UNFORMATTED cListItemPairs SKIP.
    OUTPUT CLOSE.
    APPLY "ENTRY" TO CB-Fra.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Fra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Fra Dialog-Frame
ON VALUE-CHANGED OF CB-Fra IN FRAME Dialog-Frame /* Fra */
DO:
  DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
  DO iCount = LOOKUP(SELF:SCREEN-VALUE,SELF:LIST-ITEM-PAIRS) - 1
                         TO NUM-ENTRIES(SELF:LIST-ITEM-PAIRS):
      ASSIGN cListItemPairs = cListItemPairs + 
                  (IF cListItemPairs = "" THEN "" ELSE ",") +
             ENTRY(iCount,SELF:LIST-ITEM-PAIRS).
  END.
  ASSIGN CB-Til:LIST-ITEM-PAIRS = cListItemPairs
         CB-Til:SCREEN-VALUE = ENTRY(2,CB-Til:LIST-ITEM-PAIRS).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{syspara.i 2 4 20 cVisToggle000}

ASSIGN
    iLandKode = 1.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    FIND StrType WHERE StrType.StrTypeId = ipStrTypeId NO-LOCK NO-ERROR.
    IF NOT AVAIL StrType THEN DO:
        MESSAGE "Størrelsetype mangler"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    RUN InitCombos.
    RUN enable_UI.
    TG-Kun000:HIDDEN = NOT cVisToggle000 = "1".
    {lng.i}
    APPLY "CHOOSE" TO BUTTON-Angre IN FRAME {&FRAME-NAME}.
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
  DISPLAY RS-Nummerserie CB-Fra CB-Til TG-Kun000 
      WITH FRAME Dialog-Frame.
  ENABLE BUTTON-Angre RS-Nummerserie CB-Fra CB-Til TG-Kun000 Btn_OK Btn_Cancel 
         RECT-55 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombos Dialog-Frame 
PROCEDURE InitCombos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount         AS INTEGER    NO-UNDO.

    DO iCount = 1 TO NUM-ENTRIES(StrType.AlfaFordeling):
        ASSIGN cListItemPairs = cListItemPairs + (IF cListItemPairs = "" THEN "" ELSE ",") +
            ENTRY(iCount,StrType.AlfaFordeling) + ",<" + ENTRY(iCount,StrType.Fordeling).
        /* vi lägger in '<' temporärt för att lookup kan többla om alfastorlek finns samtidigt som
           samma strkod finns, ex. Alfa 10 och strkode 10 gäller vid 5.5 */
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

