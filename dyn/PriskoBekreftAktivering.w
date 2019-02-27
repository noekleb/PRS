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
DEF INPUT  PARAM icMessage            AS CHAR   NO-UNDO.
DEF INPUT  PARAM iiNumSelected        AS INT    NO-UNDO.
DEF INPUT  PARAM iiNumTotal           AS INT    NO-UNDO.
DEF INPUT  PARAM ihBrowse             AS HANDLE NO-UNDO.
DEF INPUT  PARAM iAntProf             AS INT    NO-UNDO.
DEF OUTPUT PARAM oiReturn             AS INT    NO-UNDO.
DEF OUTPUT PARAM ocParam              AS CHAR   NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR cSprak AS CHAR NO-UNDO.
DEF VAR cChar  AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rsAlle tbAktiverUtenLokPris ~
tbAktiverMedLokPris Btn_OK Btn_Cancel fi-cMessage fi-cNumTotal ~
fi-cNumSelected fiAntUtenLokPris fiAntMedLokPris 
&Scoped-Define DISPLAYED-OBJECTS rsAlle tbAktiverUtenLokPris ~
tbAktiverMedLokPris fi-cMessage fi-cNumTotal fi-cNumSelected ~
fiAntUtenLokPris fiAntMedLokPris 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SjekkPrisFordeling Dialog-Frame 
FUNCTION SjekkPrisFordeling RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE fi-cMessage AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 93 BY .95
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-cNumSelected AS CHARACTER FORMAT "X(256)":U 
     LABEL "Antal valda" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fi-cNumTotal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Totalt antal" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiAntMedLokPris AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Antal" 
      VIEW-AS TEXT 
     SIZE 12.6 BY .62 NO-UNDO.

DEFINE VARIABLE fiAntUtenLokPris AS INTEGER FORMAT "->>>>>>9":U INITIAL 0 
     LABEL "Antal" 
      VIEW-AS TEXT 
     SIZE 12.6 BY .62 NO-UNDO.

DEFINE VARIABLE rsAlle AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alla", 1,
"Valda poster", 2
     SIZE 21 BY 1.57 NO-UNDO.

DEFINE VARIABLE tbAktiverMedLokPris AS LOGICAL INITIAL no 
     LABEL "Aktivera varor där lokalt pris finns" 
     VIEW-AS TOGGLE-BOX
     SIZE 47 BY .81 NO-UNDO.

DEFINE VARIABLE tbAktiverUtenLokPris AS LOGICAL INITIAL yes 
     LABEL "Aktivera varor där lokalt pris inte finns" 
     VIEW-AS TOGGLE-BOX
     SIZE 51 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     rsAlle AT ROW 3.52 COL 40 NO-LABEL
     tbAktiverUtenLokPris AT ROW 6 COL 12
     tbAktiverMedLokPris AT ROW 7.05 COL 12
     Btn_OK AT ROW 8.62 COL 62.8
     Btn_Cancel AT ROW 8.62 COL 78.8
     fi-cMessage AT ROW 1.95 COL 2 NO-LABEL
     fi-cNumTotal AT ROW 3.57 COL 21 COLON-ALIGNED
     fi-cNumSelected AT ROW 4.38 COL 21 COLON-ALIGNED
     fiAntUtenLokPris AT ROW 6.14 COL 69.4 COLON-ALIGNED
     fiAntMedLokPris AT ROW 7.14 COL 69.4 COLON-ALIGNED
     SPACE(11.79) SKIP(2.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Aktivering av priskö"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Aktivering av priskø */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  IF CAN-DO("SE,SVE",cSprak) THEN
      cChar = "Är du säker på att du önskar att uppdatera lokala priser med poster i priskön?".
  ELSE 
      cCHar = "Er du sikker på at du vil oppdatere lokale priser med poster i priskøen?".

  IF tbAktiverMedLokPris:SENSITIVE AND tbAktiverMedLokPris:CHECKED THEN
      IF DYNAMIC-FUNCTION("DoMessage",0,1,
                          cCHar,
                          "","") = 2 THEN RETURN NO-APPLY.
  IF (tbAktiverUtenLokPris:CHECKED AND tbAktiverUtenLokPris:SENSITIVE) OR 
     (tbAktiverMedLokPris:CHECKED AND tbAktiverMedLokPris:SENSITIVE) THEN
    ASSIGN oiReturn = rsAlle
           ocParam  = STRING(tbAktiverUtenLokPris:CHECKED AND tbAktiverUtenLokPris:SENSITIVE) + "|"
                    + STRING(tbAktiverMedLokPris:CHECKED AND tbAktiverMedLokPris:SENSITIVE)
           .
  ELSE DO:
      IF CAN-DO("SE,SVE",cSprak) THEN
          DYNAMIC-FUNCTION("DoMessage",0,0,"Inga varor aktiveras","","").
      ELSE
          DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen varer vil bli aktivert","","").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsAlle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsAlle Dialog-Frame
ON VALUE-CHANGED OF rsAlle IN FRAME Dialog-Frame
DO:
  ASSIGN rsAlle.
  SjekkPrisFordeling().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{incl/frametrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
    IF AVAIL bruker THEN
        cSprak = TRIM(Bruker.Lng).

  RUN enable_UI.
  {lng.i}

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
  DISPLAY rsAlle tbAktiverUtenLokPris tbAktiverMedLokPris fi-cMessage 
          fi-cNumTotal fi-cNumSelected fiAntUtenLokPris fiAntMedLokPris 
      WITH FRAME Dialog-Frame.
  ENABLE rsAlle tbAktiverUtenLokPris tbAktiverMedLokPris Btn_OK Btn_Cancel 
         fi-cMessage fi-cNumTotal fi-cNumSelected fiAntUtenLokPris 
         fiAntMedLokPris 
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
  rsAlle:SCREEN-VALUE = "2".

  IF iiNumSelected = 0 THEN 
    ASSIGN rsAlle:SCREEN-VALUE = "1"
           rsAlle:SENSITIVE = FALSE.
  IF iAntProf = 1 THEN
      tbAktiverMedLokPris:CHECKED = TRUE.

  APPLY "value-changed" TO rsAlle.

  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SjekkPrisFordeling Dialog-Frame 
FUNCTION SjekkPrisFordeling RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR iAntMedLokPris  AS INT NO-UNDO.
DEF VAR iAntUtenLokPris AS INT NO-UNDO.

DEF VAR hQuery  AS HANDLE NO-UNDO.
DEF VAR hBuffer AS HANDLE NO-UNDO.
DEF VAR ix      AS INT    NO-UNDO.

IF rsAlle = 1 THEN DO:
  CREATE BUFFER hBuffer FOR TABLE ihBrowse:QUERY:GET-BUFFER-HANDLE(1).
  
  CREATE QUERY hQuery.
  hQuery:SET-BUFFERS(hBuffer).
  hQuery:QUERY-PREPARE("FOR EACH " + hBuffer:NAME).
  hQuery:QUERY-OPEN().
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    IF hBuffer:BUFFER-FIELD("LokalPris"):BUFFER-VALUE NE hBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE THEN
      iAntMedLokPris = iAntMedLokPris + 1.
    ELSE
      iAntUtenLokPris = iAntUtenLokPris + 1.
    hQuery:GET-NEXT().
  END.
  
  DELETE OBJECT hQuery  NO-ERROR.
  DELETE OBJECT hBuffer NO-ERROR.
END.
ELSE DO:
  hBuffer = ihBrowse:QUERY:GET-BUFFER-HANDLE(1).  
  DO ix = 1 TO ihBrowse:NUM-SELECTED-ROWS:
    IF ihBrowse:FETCH-SELECTED-ROW(ix) THEN DO:
      IF hBuffer:BUFFER-FIELD("LokalPris"):BUFFER-VALUE NE hBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE THEN
        iAntMedLokPris = iAntMedLokPris + 1.
      ELSE
        iAntUtenLokPris = iAntUtenLokPris + 1.
    END.
  END.
END.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN fiAntUtenLokPris               = iAntUtenLokPris
         fiAntMedLokPris                = iAntMedLokPris
         tbAktiverUtenLokPris:SENSITIVE = iAntUtenLokPris > 0
         tbAktiverUtenLokPris:CHECKED   = iAntUtenLokPris > 0
         tbAktiverMedLokPris:SENSITIVE  = iAntMedLokPris > 0
         .
  IF NOT tbAktiverMedLokPris:SENSITIVE THEN
    tbAktiverMedLokPris:CHECKED = NO.

  DISP fiAntUtenLokPris fiAntMedLokPris.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

