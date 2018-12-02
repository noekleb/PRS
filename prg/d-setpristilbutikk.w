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
DEF VAR cTeamIdList       AS CHAR NO-UNDO.
DEF VAR cTekst            AS CHAR NO-UNDO.
DEF VAR piLoop            AS INT  NO-UNDO.
DEF VAR cAlleButikker     AS CHAR NO-UNDO.

DEF VAR cUser             AS CHAR NO-UNDO.
DEF VAR iBrGrpNr          AS INT  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS CB-ButikkTeam rsAlle B-SokButikk Btn_OK ~
Btn_Cancel fi-cMessage fi-cNumTotal fi-cNumSelected 
&Scoped-Define DISPLAYED-OBJECTS CB-ButikkTeam rsAlle fcButikkliste ~
FI-Antall fi-cMessage fi-cNumTotal fi-cNumSelected 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokButikk 
     LABEL "..." 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE CB-ButikkTeam AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikkteam" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE fcButikkliste AS CHARACTER FORMAT "X(256)" 
     LABEL "Butikkliste" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1.

DEFINE VARIABLE FI-Antall AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Antall valgte butikker" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE fi-cMessage AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 93 BY .95
     FONT 6 NO-UNDO.

DEFINE VARIABLE fi-cNumSelected AS CHARACTER FORMAT "X(256)":U 
     LABEL "Antall valgt" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fi-cNumTotal AS CHARACTER FORMAT "X(256)":U 
     LABEL "Totalt antall" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE rsAlle AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alle", 1,
"Valgte poster", 2
     SIZE 21 BY 1.57 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     CB-ButikkTeam AT ROW 3.29 COL 64 COLON-ALIGNED
     rsAlle AT ROW 3.38 COL 32 NO-LABEL
     B-SokButikk AT ROW 4.33 COL 90.2
     fcButikkliste AT ROW 4.38 COL 63.8 COLON-ALIGNED HELP
          "Produsent"
     FI-Antall AT ROW 5.43 COL 64 COLON-ALIGNED WIDGET-ID 2 NO-TAB-STOP 
     Btn_OK AT ROW 8.14 COL 64
     Btn_Cancel AT ROW 8.14 COL 80
     fi-cMessage AT ROW 1.95 COL 2 NO-LABEL
     fi-cNumTotal AT ROW 3.52 COL 16 COLON-ALIGNED
     fi-cNumSelected AT ROW 4.33 COL 16 COLON-ALIGNED
     SPACE(64.39) SKIP(4.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Overf. til vpi/send til butikk"
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

/* SETTINGS FOR FILL-IN fcButikkliste IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Antall IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fi-cMessage IN FRAME Dialog-Frame
   ALIGN-L                                                              */
ASSIGN 
       fi-cMessage:RESIZABLE IN FRAME Dialog-Frame        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Overf. til vpi/send til butikk */
DO:
  APPLY "END-ERROR":U TO SELF. 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokButikk Dialog-Frame
ON CHOOSE OF B-SokButikk IN FRAME Dialog-Frame /* ... */
OR "MOUSE-SELECT-DBLCLICK":U OF B-SokButikk
OR "F10" OF B-SokButikk
DO:

  DEF VAR bOK               AS LOG  NO-UNDO.

  IF cAlleButikker <> '' THEN
  DO:
      ASSIGN
          cButikerRowIdList = cAlleButikker
          cButikerIdList    = ''.

      DO piLoop = 1 TO NUM-ENTRIES(cAlleButikker):
          FIND Butiker NO-LOCK WHERE Butiker.Butik = INT(ENTRY(piLoop,cAlleButikker)).
          IF AVAILABLE Butiker THEN
              ENTRY(piLoop,cButikerRowIdList) = STRING(ROWID(Butiker)).
      END.
      RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                          "Butiker;Butik;ButNamn",
                          "where CAN-DO('" + cAlleButikker + "',STRING(Butiker.Butik))",
                          INPUT-OUTPUT cButikerRowIdList,
                          "Butik",
                          INPUT-OUTPUT cButikerIdList,
                          "","",
                          OUTPUT bOK).
      IF NUM-ENTRIES(cAlleButikker) <> NUM-ENTRIES(cButikerIdList,'|') THEN
      DO:
          CB-ButikkTeam:SCREEN-VALUE = '0'.
          cAlleButikker = ''.
      END.
  END.
  ELSE DO:
      RUN JBoxDSelector.w (THIS-PROCEDURE,0,
                          "Butiker;Butik;ButNamn",
                          "where Butiker.ApningsDato <> ? and Butiker.NedlagtDato = ? and Butiker.harButikksystem = true",
                          INPUT-OUTPUT cButikerRowIdList,
                          "Butik",
                          INPUT-OUTPUT cButikerIdList,
                          "","",
                          OUTPUT bOK).
  END.

  IF bOk THEN DO: 
    fcButikkListe:SCREEN-VALUE = REPLACE(cButikerIdList,"|",",").
    ASSIGN FI-Antall:SCREEN-VALUE IN FRAME Dialog-Frame = STRING(NUM-ENTRIES(REPLACE(cButikerIdList,"|",","))).
    APPLY "return" TO fcButikkListe.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  IF INT(CB-ButikkTeam:SCREEN-VALUE) > 0 THEN
  DO:
      ASSIGN
          cTekst        = ''
          cAlleButikker = ''.
      FOR EACH ButikkKobling NO-LOCK WHERE
          ButikkKobling.TeamNr = INT(CB-ButikkTeam:SCREEN-VALUE) AND
          ButikkKobling.TeamTypeId = 3,
          EACH Butiker OF ButikkKobling NO-LOCK WHERE
            Butiker.ApningsDato     <> ? AND  
            Butiker.NedlagtDato     =  ? AND 
            Butiker.harButikksystem =  TRUE: 

            IF NOT CAN-DO(cTekst,STRING(Butiker.Butik)) THEN
                cTekst = cTekst + ',' + STRING(Butiker.Butik).  
      END.
      cTekst = TRIM(cTekst,',').
      ASSIGN
          fcButikkliste:SCREEN-VALUE = cTekst
          cAlleButikker              = cTekst
          rsAlle.
      
      ASSIGN cValues  = cAlleButikker                       
             oiReturn = rsAlle
             .
  END.
  ELSE DO:
      ASSIGN fcButikkListe rsAlle.
      ASSIGN cValues  = (IF cButikerIdList <> '' 
                          THEN REPLACE(cButikerIdList,"|",",") 
                          ELSE STRING(cAllebutikker))                       
             oiReturn = rsAlle.
  END.
  /*
  MESSAGE NUM-ENTRIES(cAlleButikker) 'cAllebutikker' cAlleButikker SKIP
          NUM-ENTRIES(REPLACE(cButikerIdList,"|",",")) 'cButikerIdList' REPLACE(cButikerIdList,"|",",") SKIP
          NUM-ENTRIES(fcButikkListe) 'fcButikkListe' fcButikkListe SKIP
          NUM-ENTRIES(cValues) 'cValues' cValues 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-ButikkTeam
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-ButikkTeam Dialog-Frame
ON VALUE-CHANGED OF CB-ButikkTeam IN FRAME Dialog-Frame /* Butikkteam */
DO:
  IF CB-ButikkTeam:SCREEN-VALUE = '0' THEN
      ASSIGN      
      fcButikkliste:SCREEN-VALUE = ''
      cAlleButikker              = ''.
  ELSE DO:
      cTekst = ''.
      FOR EACH ButikkKobling NO-LOCK WHERE
          ButikkKobling.TeamNr = INT(CB-ButikkTeam:SCREEN-VALUE) AND 
          ButikkKobling.TeamTypeId = 3,
          EACH Butiker OF ButikkKobling NO-LOCK WHERE
            Butiker.ApningsDato     <> ? AND  
            Butiker.NedlagtDato     =  ? AND 
            Butiker.harButikksystem =  TRUE: 

            IF NOT CAN-DO(cTekst,STRING(Butiker.Butik)) THEN
                cTekst = cTekst + ',' + STRING(Butiker.Butik).  
      END.
      cTekst = TRIM(cTekst,',').
      ASSIGN
          fcButikkliste:SCREEN-VALUE = cTekst
          cAlleButikker              = cTekst.
      ASSIGN FI-Antall:SCREEN-VALUE IN FRAME Dialog-Frame = STRING(NUM-ENTRIES(cAlleButikker)).

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fcButikkliste
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fcButikkliste Dialog-Frame
ON VALUE-CHANGED OF fcButikkliste IN FRAME Dialog-Frame /* Butikkliste */
DO:
  ASSIGN FI-Antall:SCREEN-VALUE = STRING(cAlleButikker,',').
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
  DISPLAY CB-ButikkTeam rsAlle fcButikkliste FI-Antall fi-cMessage fi-cNumTotal 
          fi-cNumSelected 
      WITH FRAME Dialog-Frame.
  ENABLE CB-ButikkTeam rsAlle B-SokButikk Btn_OK Btn_Cancel fi-cMessage 
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

cUser = DYNAMIC-FUNCTION('getASUserId').
iBrGrpNr = INT(DYNAMIC-FUNCTION('getFieldValues','Bruker','WHERE BrukerId = ' + QUOTER(cUser),'BrGrpNr')) NO-ERROR.

DO WITH FRAME {&FRAME-NAME}:
  
  cTeamIdList = '<Alle>,0'.
  FOR EACH ButikkTeam NO-LOCK WHERE
      ButikkTeam.TeamTypeId = 3 AND
      ButikkTeam.BrGrpNr    = iBrGrpNr:
      cTeamIdList = cTeamIdList + 
                    (IF cTeamIdList = '' THEN '' ELSE ',') + 
                    ButikkTeam.Beskrivelse + ',' + STRING(TeamNr).
  END.

  IF NUM-ENTRIES(icMessage,"|") > 1 THEN
  DO:
      /*
      IF ENTRY(2,icMessage,"|") = "TMPARTBAS" THEN
          ASSIGN
          T-HKVpi:HIDDEN = TRUE.
      */
  END.

  ASSIGN iBlanks                       = IF LENGTH(icMessage) < 90 THEN 90 - LENGTH(icMessage) ELSE 2
         iBlanks                       = iBlanks / 2
         fi-cMessage:SCREEN-VALUE      = FILL(" ",iBlanks) + entry(1,icMessage,"|") 
         fi-cNumSelected:SCREEN-VALUE  = STRING(iiNumSelected)
         fi-cNumTotal:SCREEN-VALUE     = STRING(iiNumTotal)
         CB-ButikkTeam:LIST-ITEM-PAIRS = cTeamIdList
         CB-ButikkTeam:SCREEN-VALUE    = '0'
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

