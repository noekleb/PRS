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

DEF VAR cButikerRowIdList AS CHAR NO-UNDO.
DEF VAR cButikerIdList    AS CHAR NO-UNDO.

DEF VAR bUndertrykkFilter AS LOG NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCalFraDato fdDato rsAlle ~
T-OpprettArtikkel T-OppdArtInfo T-OverfUtpris Btn_OK Btn_Cancel fi-cMessage ~
fi-cNumTotal fi-cNumSelected 
&Scoped-Define DISPLAYED-OBJECTS fdDato rsAlle T-OpprettArtikkel ~
T-OppdArtInfo T-OverfInnpris T-OverfUtpris fi-cMessage fi-cNumTotal ~
fi-cNumSelected 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fdDato AS DATE FORMAT "99/99/99" 
     LABEL "Aktiveringsdato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Dato for aktivering av pris. Tidspunkt er 00:00.".

DEFINE VARIABLE fi-cMessage AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 93 BY .95
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-cNumSelected AS CHARACTER FORMAT "X(256)":U 
     LABEL "Antall valgt" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE fi-cNumTotal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Totalt antall" 
      VIEW-AS TEXT 
     SIZE 10 BY .62 NO-UNDO.

DEFINE VARIABLE rsAlle AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alle", 1,
"Valgte poster", 2
     SIZE 19 BY 1.57 NO-UNDO.

DEFINE VARIABLE T-OppdArtInfo AS LOGICAL INITIAL yes 
     LABEL "Oppd. art.info på eksisterende artikler" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.

DEFINE VARIABLE T-OpprettArtikkel AS LOGICAL INITIAL yes 
     LABEL "Opprett ukjente artikler" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.

DEFINE VARIABLE T-OverfInnpris AS LOGICAL INITIAL yes 
     LABEL "Innpris og rabatt overføres alltid" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.

DEFINE VARIABLE T-OverfUtpris AS LOGICAL INITIAL no 
     LABEL "Overfør utpris" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnCalFraDato AT ROW 3.14 COL 82
     fdDato AT ROW 3.14 COL 64 COLON-ALIGNED HELP
          "Produsent"
     rsAlle AT ROW 3.38 COL 28.8 NO-LABEL
     T-OpprettArtikkel AT ROW 4.33 COL 66
     T-OppdArtInfo AT ROW 5.29 COL 66
     T-OverfInnpris AT ROW 6.24 COL 66
     T-OverfUtpris AT ROW 7.43 COL 66
     Btn_OK AT ROW 8.86 COL 66.2
     Btn_Cancel AT ROW 8.86 COL 92.8
     fi-cMessage AT ROW 1.95 COL 2 NO-LABEL
     fi-cNumTotal AT ROW 3.52 COL 16 COLON-ALIGNED
     fi-cNumSelected AT ROW 4.33 COL 16 COLON-ALIGNED
     SPACE(80.39) SKIP(5.14)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Overfør til priskø"
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
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN fi-cMessage IN FRAME Dialog-Frame
   ALIGN-L                                                              */
ASSIGN 
       fi-cMessage:RESIZABLE IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR TOGGLE-BOX T-OverfInnpris IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Overfør til priskø */
DO:
  APPLY "END-ERROR":U TO SELF. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato Dialog-Frame
ON CHOOSE OF btnCalFraDato IN FRAME Dialog-Frame /* ... */
OR F10 OF fdDato
DO:
  RUN dCal.w (fdDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  ASSIGN rsAlle fdDato T-OpprettArtikkel T-OverfInnpris T-OverfUtpris T-OppdArtInfo.
  IF fdDato < TODAY OR fdDato = ? THEN
    DO:
      MESSAGE 'Ugyldig dato eller dato ikke angitt. Dato må være større eller lik dagens dato.'
      VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
    END.
  ASSIGN cValues  = (IF fdDato:SCREEN-VALUE <> ? THEN fdDato:SCREEN-VALUE ELSE '') + "|" + 
                    T-OpprettArtikkel:SCREEN-VALUE + "|" +
                    T-OverfInnpris:SCREEN-VALUE + "|" +
                    T-OverfUtpris:SCREEN-VALUE + "|" +
                    T-OppdArtInfo:SCREEN-VALUE
         oiReturn = rsAlle
         .
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
  
  IF bUndertrykkFilter THEN
    T-OppdArtInfo:HIDDEN IN FRAME {&FRAME-NAME} = TRUE.
  
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
  DISPLAY fdDato rsAlle T-OpprettArtikkel T-OppdArtInfo T-OverfInnpris 
          T-OverfUtpris fi-cMessage fi-cNumTotal fi-cNumSelected 
      WITH FRAME Dialog-Frame.
  ENABLE btnCalFraDato fdDato rsAlle T-OpprettArtikkel T-OppdArtInfo 
         T-OverfUtpris Btn_OK Btn_Cancel fi-cMessage fi-cNumTotal 
         fi-cNumSelected 
      WITH FRAME Dialog-Frame.
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
DEF VAR cTekst  AS CHAR NO-UNDO.

cTekst      = DYNAMIC-FUNCTION("getFieldValues","SysPara","WHERE SysHId = 50 and SysGr =  25 and ParaNr = 6","Parameter1").
IF CAN-DO('1,Ja,True,Yes,J,Y',cTekst) THEN 
    bUndertrykkFilter = TRUE.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN iBlanks                      = IF LENGTH(icMessage) < 90 THEN 90 - LENGTH(icMessage) ELSE 2
         iBlanks                      = iBlanks / 2
         fi-cMessage:SCREEN-VALUE     = FILL(" ",iBlanks) + entry(1,icMessage,"|") 
         fi-cNumSelected:SCREEN-VALUE = STRING(iiNumSelected)
         fi-cNumTotal:SCREEN-VALUE    = STRING(iiNumTotal)
         fdDato:SCREEN-VALUE          = STRING(TODAY)
         T-OverfUtpris:CHECKED        = TRUE
         .
  rsAlle:SCREEN-VALUE = "2".

  IF iiNumSelected = 0 THEN 
    rsAlle:SENSITIVE = FALSE.


  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).
  VIEW FRAME {&FRAME-NAME}.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

