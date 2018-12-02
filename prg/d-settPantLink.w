&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
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
DEF INPUT  PARAM icMessage     AS CHAR NO-UNDO.
DEF INPUT  PARAM iiNumSelected AS INT  NO-UNDO.
DEF INPUT  PARAM iiNumTotal    AS INT  NO-UNDO.
DEF OUTPUT PARAM oiReturn      AS INT  NO-UNDO.
DEF OUTPUT PARAM cValues       AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
/* DEF VAR icMessage     AS CHAR NO-UNDO. */
/* DEF VAR iiNumSelected AS INT  NO-UNDO. */
/* DEF VAR iiNumTotal    AS INT  NO-UNDO. */
/* DEF VAR oiReturn      AS INT  NO-UNDO. */
/* DEF VAR cValues       AS char NO-UNDO. */
DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk     AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ArtBas

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH ArtBas SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH ArtBas SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame ArtBas


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-SokLinkvare rsAlle FI-LinkVareNr Btn_OK ~
Btn_Cancel fi-cMessage fi-cNumTotal fi-cNumSelected 
&Scoped-Define DISPLAYED-OBJECTS rsAlle FI-LinkVareNr FI-LinkVareTekst ~
fi-cMessage fi-cNumTotal fi-cNumSelected 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokLinkvare 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi-cMessage AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 69 BY .95
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-cNumSelected AS CHARACTER FORMAT "X(256)":U 
     LABEL "Antall valgt" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fi-cNumTotal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Totalt antall" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE FI-LinkVareNr AS DECIMAL FORMAT "zzzzzzzzzzzz9":U INITIAL 0 
     LABEL "Link til pantevare" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 20.2 BY 1 TOOLTIP "Artikkelnr. på artikkel som representerer pant på varen" NO-UNDO.

DEFINE VARIABLE FI-LinkVareTekst AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 23 BY 1 NO-UNDO.

DEFINE VARIABLE rsAlle AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alle", 1,
"Valgte poster", 2
     SIZE 21 BY 1.57 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      ArtBas SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-SokLinkvare AT ROW 4.57 COL 66.2 NO-TAB-STOP 
     rsAlle AT ROW 2.38 COL 32 NO-LABEL
     FI-LinkVareNr AT ROW 4.57 COL 21 COLON-ALIGNED HELP
          "Artikkelnr. på artikkel som representerer pant på varen (F10)"
     FI-LinkVareTekst AT ROW 4.57 COL 41.2 COLON-ALIGNED NO-LABEL
     Btn_OK AT ROW 5.86 COL 39
     Btn_Cancel AT ROW 5.86 COL 55
     fi-cMessage AT ROW 1.19 COL 2 NO-LABEL
     fi-cNumTotal AT ROW 2.52 COL 16 COLON-ALIGNED
     fi-cNumSelected AT ROW 3.33 COL 16 COLON-ALIGNED
     SPACE(39.59) SKIP(3.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Endre link til pantvare"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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

{incl/DevMode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-cMessage IN FRAME Dialog-Frame
   ALIGN-L                                                              */
ASSIGN 
       fi-cMessage:RESIZABLE IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN FI-LinkVareTekst IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FI-LinkVareTekst:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "skotex.ArtBas"
     _Options          = "SHARE-LOCK"
     _Query            is OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Endre link til pantvare */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokLinkvare
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokLinkvare Dialog-Frame
ON CHOOSE OF B-SokLinkvare IN FRAME Dialog-Frame /* ... */
DO:
  
  DO WITH FRAME Dialog-Frame:
  /*
  RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                      "ArtBas;ArtikkelNr;Beskr",
                      "where ArtBas.Pant = true",
                      INPUT-OUTPUT pcTekst,
                      "ArtikkelNr",
                      INPUT-OUTPUT pcTekst,
                      "","",
                      OUTPUT bOK).
  */
  pcTekst = "ArtikkelNr".
  RUN JBoxDLookup.w ("ArtBas;ArtikkelNr;LevKod;Beskr;LevFargKod", "where ArtBas.Pant = TRUE", INPUT-OUTPUT pcTekst).

  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = dec(pcTekst) NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN
          FI-LinkVareNr:SCREEN-VALUE IN FRAME Dialog-Frame = pcTekst
          FI-LinkVareTekst:SCREEN-VALUE  = ArtBas.Beskr
          .
  END.
  ELSE DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN
          FI-LinkVareNr:SCREEN-VALUE IN FRAME Dialog-Frame = ''
          FI-LinkVareTekst:SCREEN-VALUE  = ''
          .
  END.
  END.
    APPLY "ENTRY" TO FI-LinkVareNr IN FRAME Dialog-Frame.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DO WITH FRAME Dialog-Frame:
    ASSIGN FI-LinkVareNr rsAlle.
  
    ASSIGN cValues  = FI-LinkVareNr:SCREEN-VALUE
           oiReturn = rsAlle
           .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-LinkVareNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-LinkVareNr Dialog-Frame
ON LEAVE OF FI-LinkVareNr IN FRAME Dialog-Frame /* Link til pantevare */
DO:
  IF INPUT FI-LinkVareNr > 0 AND
    NOT CAN-FIND(ArtBas WHERE
                 ArtBas.ArtikkelNr = INPUT FI-LinkVareNr AND
                 ArtBas.Pant = TRUE) THEN
    DO:
      MESSAGE 'Link kan bare legges inn på varer som er satt opp som pantvare.'
        VIEW-AS ALERT-BOX WARNING.
      RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{incl/frametrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   
  RUN enable_UI.
  RUN InitWindow.
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

  {&OPEN-QUERY-Dialog-Frame}
  GET FIRST Dialog-Frame.
  DISPLAY rsAlle FI-LinkVareNr FI-LinkVareTekst fi-cMessage fi-cNumTotal 
          fi-cNumSelected 
      WITH FRAME Dialog-Frame.
  ENABLE B-SokLinkvare rsAlle FI-LinkVareNr Btn_OK Btn_Cancel fi-cMessage 
         fi-cNumTotal fi-cNumSelected 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow Dialog-Frame 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iBlanks AS INT NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN iBlanks                      = IF LENGTH(icMessage) < 90 THEN 90 - LENGTH(icMessage) ELSE 2
         iBlanks                      = iBlanks / 2
         fi-cMessage:SCREEN-VALUE     = FILL(" ",iBlanks) + icMessage 
         fi-cNumSelected:SCREEN-VALUE = STRING(iiNumSelected)
         fi-cNumTotal:SCREEN-VALUE    = STRING(iiNumTotal)
         .
/*   IF iiNumSelected > 1 THEN    */
/*     rsAlle:SCREEN-VALUE = "2". */
/*   ELSE                         */
/*     rsAlle:SCREEN-VALUE = "1". */
  rsAlle:SCREEN-VALUE = "2".

  IF iiNumSelected = 0 THEN 
    rsAlle:SENSITIVE = FALSE.

  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

