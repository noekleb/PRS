&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-ButikKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-ButikKort 
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEF VAR wInputRecid AS RECID NO-UNDO.
  DEF VAR wModus       AS CHAR  NO-UNDO. /* NY, ENDRE, SLETT */
  ASSIGN 
    wModus = "ENDRE". /* Default */
 FIND FIRST Ekstbutiker NO-LOCK NO-ERROR.
  IF AVAILABLE Ekstbutiker THEN
    ASSIGN wInputRecid = RECID(Ekstbutiker).
&ELSE
  DEF INPUT PARAMETER wInputRecid AS RECID NO-UNDO.
  DEF INPUT PARAMETER wModus       AS CHAR  NO-UNDO. /* NY, ENDRE, SLETT */
&ENDIF

/* Local Variable Definitions ---                                       */
DEF VAR wOk                AS LOG    NO-UNDO.
DEF VAR wOldRecid          AS RECID  NO-UNDO.
DEF VAR hHandle            AS HANDLE NO-UNDO.
DEF VAR hLabel             AS HANDLE NO-UNDO.
DEF VAR wFilNavn           AS CHAR   NO-UNDO.
DEF VAR wFilExt            AS CHAR   NO-UNDO.
DEF VAR wSjekkStreng       AS CHAR   NO-UNDO.
DEF VAR wRS-Vis            AS RECID  NO-UNDO.
DEF VAR wDataObjekt        AS CHAR   NO-UNDO.
DEF VAR wSubWin            AS HANDLE NO-UNDO.
DEF VAR wBekreft           AS LOG    NO-UNDO.
DEF VAR wFeil              AS LOGI   NO-UNDO.
DEF VAR cTekst             AS CHAR   NO-UNDO.
/* Variabler for håndtering av Tab-Stip. */
DEFINE VAR wBrowseHandle     AS HANDLE NO-UNDO.
/* Buffere */
DEF BUFFER bEkstbutiker FOR Ekstbutiker.
DEF TEMP-TABLE tmpChild 
  FIELD wChild AS HANDLE.
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ekstbutiker.ButNamn ekstbutiker.KortNavn ~
ekstbutiker.BuAdr ekstbutiker.BuPonr ekstbutiker.BuPadr ekstbutiker.BuKon ~
ekstbutiker.BuTel ekstbutiker.ePostAdresse ekstbutiker.OrganisasjonsNr 
&Scoped-define ENABLED-TABLES ekstbutiker
&Scoped-define FIRST-ENABLED-TABLE ekstbutiker
&Scoped-Define ENABLED-OBJECTS BUTTON-SokBut B-KundeNr RECT-27 RECT-28 ~
BUTTON-Angre BUTTON-Kopier BUTTON-Lagre BUTTON-Next BUTTON-Ny BUTTON-Prev ~
BUTTON-Slett Btn_Help BUTTON-Ok 
&Scoped-Define DISPLAYED-FIELDS ekstbutiker.Butik ekstbutiker.ButNamn ~
ekstbutiker.KortNavn ekstbutiker.BuAdr ekstbutiker.BuPonr ~
ekstbutiker.BuPadr ekstbutiker.BuKon ekstbutiker.BuTel ~
ekstbutiker.ePostAdresse ekstbutiker.OrganisasjonsNr ekstbutiker.KundeNr 
&Scoped-define DISPLAYED-TABLES ekstbutiker
&Scoped-define FIRST-DISPLAYED-TABLE ekstbutiker
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-Butik FILL-IN-ButNamn ~
FILL-IN-Poststed FI-Kundenavn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Modifierad C-ButikKort 
FUNCTION Modifierad RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD VisTid C-ButikKort 
FUNCTION VisTid RETURNS CHARACTER
  ( INPUT iTid AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-ButikKort AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE SUB-MENU m_Fil 
       MENU-ITEM m_Avslutt      LABEL "&Avslutt"       ACCELERATOR "ALT-F4".

DEFINE MENU MENU-BAR-C-ArtKort MENUBAR
       SUB-MENU  m_Fil          LABEL "&Fil"          .


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-KundeNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon\e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Help" 
     SIZE 4.6 BY 1.05
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Angre 
     IMAGE-UP FILE "icon\e-undo":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ångra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Angre (Alt-A)".

DEFINE BUTTON BUTTON-Kopier 
     IMAGE-UP FILE "icon\e-copy":U NO-FOCUS FLAT-BUTTON
     LABEL "&Kopiera" 
     SIZE 4.6 BY 1.05 TOOLTIP "Kopier post  (Alt-K)".

DEFINE BUTTON BUTTON-Lagre 
     IMAGE-UP FILE "icon\e-save":U NO-FOCUS FLAT-BUTTON
     LABEL "&Lagra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre post (Alt-L)".

DEFINE BUTTON BUTTON-Next 
     IMAGE-UP FILE "icon\e-pilned":U NO-FOCUS FLAT-BUTTON
     LABEL "Neste" 
     SIZE 4.6 BY 1.05 TOOLTIP "Neste post (Alt-PilNed)".

DEFINE BUTTON BUTTON-Ny 
     IMAGE-UP FILE "icon\e-ny":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ny" 
     SIZE 4.6 BY 1.05 TOOLTIP "Ny post (Alt-N)".

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagre og avslutt".

DEFINE BUTTON BUTTON-Prev 
     IMAGE-UP FILE "icon\e-pilopp":U NO-FOCUS FLAT-BUTTON
     LABEL "Forrige" 
     SIZE 4.6 BY 1.05 TOOLTIP "Forrige post (Alt-PilOpp)".

DEFINE BUTTON BUTTON-Slett 
     IMAGE-UP FILE "icon\e-del":U NO-FOCUS FLAT-BUTTON
     LABEL "Radera" 
     SIZE 4.6 BY 1.05 TOOLTIP "Slett post (Alt-D)".

DEFINE BUTTON BUTTON-SokBut 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk i leverandørregister".

DEFINE VARIABLE FI-Kundenavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-Butik AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknummer" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE FILL-IN-ButNamn AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1.

DEFINE VARIABLE FILL-IN-Poststed AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 38 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-27
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 117 BY .1.

DEFINE RECTANGLE RECT-28
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 117 BY .1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-SokBut AT ROW 3 COL 33.2 NO-TAB-STOP 
     B-KundeNr AT ROW 16.71 COL 43 NO-TAB-STOP 
     FILL-IN-Butik AT ROW 3 COL 20 COLON-ALIGNED HELP
          "Butikknummer"
     FILL-IN-ButNamn AT ROW 3 COL 36 COLON-ALIGNED HELP
          "Butikkens navn" NO-LABEL
     ekstbutiker.Butik AT ROW 4.91 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     ekstbutiker.ButNamn AT ROW 6.05 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ekstbutiker.KortNavn AT ROW 7.05 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     ekstbutiker.BuAdr AT ROW 8.14 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ekstbutiker.BuPonr AT ROW 9.33 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     ekstbutiker.BuPadr AT ROW 10.52 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     FILL-IN-Poststed AT ROW 10.52 COL 44.4 COLON-ALIGNED NO-LABEL
     ekstbutiker.BuKon AT ROW 11.71 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     ekstbutiker.BuTel AT ROW 13 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     ekstbutiker.ePostAdresse AT ROW 14.1 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     ekstbutiker.OrganisasjonsNr AT ROW 15.52 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     ekstbutiker.KundeNr AT ROW 16.71 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     FI-Kundenavn AT ROW 16.81 COL 47.4 COLON-ALIGNED NO-LABEL
     BUTTON-Angre AT ROW 1.33 COL 20.8 NO-TAB-STOP 
     BUTTON-Kopier AT ROW 1.33 COL 11.4 NO-TAB-STOP 
     BUTTON-Lagre AT ROW 1.33 COL 6.8 NO-TAB-STOP 
     BUTTON-Next AT ROW 1.33 COL 30.6 NO-TAB-STOP 
     BUTTON-Ny AT ROW 1.33 COL 2 NO-TAB-STOP 
     BUTTON-Prev AT ROW 1.33 COL 26 NO-TAB-STOP 
     BUTTON-Slett AT ROW 1.33 COL 16 NO-TAB-STOP 
     Btn_Help AT ROW 1.24 COL 107.4 NO-TAB-STOP 
     BUTTON-Ok AT ROW 1.24 COL 112.4 NO-TAB-STOP 
     RECT-27 AT ROW 1.1 COL 1.4
     RECT-28 AT ROW 2.43 COL 1.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 117.8 BY 17.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-ButikKort ASSIGN
         HIDDEN             = YES
         TITLE              = "Butik"
         HEIGHT             = 17.95
         WIDTH              = 117.8
         MAX-HEIGHT         = 33.71
         MAX-WIDTH          = 204.4
         VIRTUAL-HEIGHT     = 33.71
         VIRTUAL-WIDTH      = 204.4
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

ASSIGN {&WINDOW-NAME}:MENUBAR    = MENU MENU-BAR-C-ArtKort:HANDLE.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-ButikKort 
/* ************************* Included-Libraries *********************** */

{incl/custdevmode.i}
{incl/devmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-ButikKort
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ekstbutiker.Butik IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-SokBut:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FI-Kundenavn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Butik IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-ButNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Poststed IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ekstbutiker.KundeNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-ButikKort)
THEN C-ButikKort:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-ButikKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-ButikKort C-ButikKort
ON END-ERROR OF C-ButikKort /* Butik */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-ButikKort C-ButikKort
ON WINDOW-CLOSE OF C-ButikKort /* Butik */
DO:

  IF CAN-FIND(FIRST tmpChild WHERE
               VALID-HANDLE(tmpChild.wChild)) THEN
    DO:
      wBekreft = FALSE.
      MESSAGE 'Det er startet andre programmer fra dette vinduet.' SKIP
              'Avsluttes dette vinduet, vil alle underliggende programmer' SKIP
              'også bli avsluttet.'
              VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO TITLE 'Bekreft avsluttning'
              UPDATE wBekreft
              .
    END.
  ELSE wBekreft = TRUE.
  IF wBekreft <> TRUE THEN 
  RETURN NO-APPLY.
  
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  /* RETURN NO-APPLY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KundeNr C-ButikKort
ON CHOOSE OF B-KundeNr IN FRAME DEFAULT-FRAME /* ... */
DO:
    DEFINE VARIABLE cLookupValue AS CHARACTER  NO-UNDO.
/*     IF wModus <> "NY" THEN                          */
/*         cLookupValue = STRING(Ekstbutiker.kundenr). */
/*     ELSE                                            */
/*         cLookupValue = "0".                         */
    cLookupValue = "KundeNr;Navn".
    RUN JBoxDLookup.w ("Kunde;KundeNr;Navn","where true",INPUT-OUTPUT cLookupValue).
    IF cLookupValue NE "" THEN DO:
        Ekstbutiker.KundeNr:SCREEN-VALUE = entry(1,cLookupValue,"|").
        FI-Kundenavn:SCREEN-VALUE = entry(2,cLookupValue,"|").
/*         APPLY "TAB" TO KundeNr. */
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-ButikKort
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Help */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  RUN WinHlp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Angre C-ButikKort
ON CHOOSE OF BUTTON-Angre IN FRAME DEFAULT-FRAME /* Ångra */
DO:
  IF wModus = "NY" THEN
     ASSIGN Ekstbutiker.Butik:SENSITIVE = NO.
            wModus = "ENDRE".
  RUN VisPost.
  RUN BUTTONEnaDis.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Kopier
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kopier C-ButikKort
ON CHOOSE OF BUTTON-Kopier IN FRAME DEFAULT-FRAME /* Kopiera */
DO:
  DO WITH FRAME DEFAULT-FRAME:
    ASSIGN 
      wModus   = "NY"
      Ekstbutiker.Butik:SENSITIVE = TRUE
      Ekstbutiker.Butik:SCREEN-VALUE = "0".
    RUN BUTTONEnaDis.
    APPLY "ENTRY":U TO Ekstbutiker.Butik.
  END.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Lagre C-ButikKort
ON CHOOSE OF BUTTON-Lagre IN FRAME DEFAULT-FRAME /* Lagra */
DO:
  RUN LagrePost (0).
  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  RUN BUTTONEnaDis.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Next C-ButikKort
ON CHOOSE OF BUTTON-Next IN FRAME DEFAULT-FRAME /* Neste */
DO:
  RUN Bytpost("Next").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-ButikKort
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
  RUN BUTTON-Ny.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-ButikKort
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
  IF CAN-FIND(FIRST tmpChild WHERE
               VALID-HANDLE(tmpChild.wChild)) THEN
    DO:
      wBekreft = FALSE.
      MESSAGE 'Det er startet andre programmer fra dette vinduet.' SKIP
              'Avsluttes dette vinduet, vil alle underliggende programmer' SKIP
              'også bli avsluttet.'
              VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO TITLE 'Bekreft avsluttning'
              UPDATE wBekreft
              .
    END.
  ELSE wBekreft = TRUE.
  IF wBekreft <> TRUE THEN
    RETURN NO-APPLY.
  IF Modifierad() THEN DO: 
    RUN LagrePost (0).
    IF RETURN-VALUE <> "OK" THEN
     DO:
       READKEY PAUSE 0.
       RETURN NO-APPLY.
    END.
  END.
  APPLY "CLOSE":U TO THIS-PROCEDURE.  
  RETURN wModus + "," + string(wInputRecid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Prev C-ButikKort
ON CHOOSE OF BUTTON-Prev IN FRAME DEFAULT-FRAME /* Forrige */
DO:
  RUN Bytpost("Prev").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-ButikKort
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Radera */
DO:
  RUN BUTTON-Slett.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokBut C-ButikKort
ON CHOOSE OF BUTTON-SokBut IN FRAME DEFAULT-FRAME /* ... */
DO:
    MESSAGE "ev sok"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-ButikKort 


/* ***************************  Main Block  *************************** */
/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* Biblotek, inn/utmelding og ON CLOSE OF THIS... */
{genlib.i 
  &NoLibCall      = "Nei"
  &WindowName     = "EkstButikkkort"
  &PostIClose    = "if valid-handle(wSubWin) then
                       do:
                         /* RUN SaveBrowseSettings in wHistorikk. */
                         delete procedure wSubWin no-error.
                       end.
                     RUN DelTmpChild.
                        "                     
  &PostDisable_ui = "wModus = 'AVBRYT'." 
}


/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* HotKeySøk - DYYYYRT */
ON ALT-N OF {&WINDOW-NAME} ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Ny IN FRAME DEFAULT-FRAME.
  END.
ON ALT-L OF {&WINDOW-NAME} ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Lagre IN FRAME DEFAULT-FRAME.
  END.
ON ALT-K OF {&WINDOW-NAME} ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Kopier IN FRAME DEFAULT-FRAME.
  END.
ON ALT-D OF {&WINDOW-NAME} ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Slett IN FRAME DEFAULT-FRAME.
  END.
ON ALT-A OF {&WINDOW-NAME} ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-angre IN FRAME DEFAULT-FRAME.
  END.
ON ALT-CURSOR-UP OF {&WINDOW-NAME} ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Prev IN FRAME DEFAULT-FRAME.
  END.
ON ALT-CURSOR-DOWN OF {&WINDOW-NAME} ANYWHERE 
  DO:
    APPLY "CHOOSE":U TO BUTTON-Next IN FRAME DEFAULT-FRAME.
  END.




/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  IF wInputRecid = ? AND wModus = "ENDRE" THEN DO:
      MESSAGE "Recid = ? och wModus = 'ENDRE' --> FEL !!" VIEW-AS ALERT-BOX ERROR.
      RETURN "AVBRYT".
  END.
  RUN enable_UI.
  
  {lng.i} /* Oversettelse */
  
  ASSIGN {&WINDOW-NAME}:HIDDEN = NO.


/*   assign chTab1Side = chCtrlFrame:TabStrip. /* COM-Handl 1. side */ */
  

  IF wModus <> "NY" THEN DO:
      FIND Ekstbutiker WHERE RECID(Ekstbutiker) = wInputRecid NO-LOCK NO-ERROR.
      RUN VisPost.
  END.

  
  IF wModus = "NY" THEN 
    DO:
      APPLY "CHOOSE":U TO BUTTON-Ny.
    END.
  ELSE IF wModus = "SLETT" THEN DO:
     APPLY "CHOOSE" TO BUTTON-Slett.
     RETURN RETURN-VALUE.
  END.
  /*  */
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
    
  IF wModus <> "NY" THEN
    FIND Ekstbutiker NO-LOCK WHERE 
      RECID(Ekstbutiker) = wInputRecid NO-ERROR.
      
  /* Retur verdi */  
  IF AVAILABLE Ekstbutiker AND wModus <> "NY" THEN
    RETURN STRING(RECID(Ekstbutiker)).
  ELSE 
    RETURN "AVBRYT".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BUTTON-Ny C-ButikKort 
PROCEDURE BUTTON-Ny :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wSort AS INT NO-UNDO.
  DEFINE VARIABLE iButnr AS INTEGER     NO-UNDO.
  DO WITH FRAME DEFAULT-FRAME:
     IF Modifierad() THEN DO:
         RUN LagrePost(0).
         IF RETURN-VALUE = "AVBRYT" THEN
             RETURN NO-APPLY.
     END.
     FIND LAST bEkstbutiker NO-LOCK NO-ERROR.
     IF NOT AVAIL bEkstbutiker THEN DO:
         FIND LAST butiker NO-LOCK.
         iButnr = TRUNC((butiker.butik + 2000) / 1000,0) * 1000.
     END.
     ELSE DO:
         iButnr = bEkstbutiker.butik + 1.
         IF CAN-FIND(butiker WHERE butiker.butik = iButnr) THEN DO:
             FIND LAST butiker NO-LOCK.
             iButnr = TRUNC((butiker.butik + 2000) / 1000,0) * 1000.
         END.
     END.
     ASSIGN wModus = "NY"
         FILL-IN-Butik:SCREEN-VALUE = "" 
         FILL-IN-ButNamn:SCREEN-VALUE = ""
         Ekstbutiker.Butik:SCREEN-VALUE = STRING(iButnr)
         Ekstbutiker.ButNamn:SCREEN-VALUE = ""
         Ekstbutiker.KortNavn:SCREEN-VALUE = ""
         Ekstbutiker.BuKon:SCREEN-VALUE = ""
         Ekstbutiker.BuAdr:SCREEN-VALUE = ""
         Ekstbutiker.BuPadr:SCREEN-VALUE = ""
         Ekstbutiker.BuPoNr:SCREEN-VALUE = ""
         Ekstbutiker.BuTel:SCREEN-VALUE = ""
         Ekstbutiker.ePostAdresse:SCREEN-VALUE = ""
         .
    RUN BUTTONEnaDis.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BUTTON-Slett C-ButikKort 
PROCEDURE BUTTON-Slett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wOk AS LOG FORMAT "Ja/Nei" NO-UNDO.
  DEF VAR wNesteRecid AS RECID NO-UNDO.
  
  FIND bEkstbutiker NO-LOCK WHERE
    RECID(bEkstbutiker) = recid(Butiker) NO-ERROR.
  FIND NEXT bEkstbutiker NO-LOCK NO-ERROR.
  IF NOT AVAILABLE bEkstbutiker THEN
    FIND FIRST bEkstbutiker.
  IF AVAILABLE bEkstbutiker THEN
    ASSIGN
      wNesteRecid = RECID(bEkstbutiker).
MESSAGE "vid slett, kolla tilgode för butiken"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*   IF CAN-FIND(FIRST ArtLag OF Butiker) THEN ~                            */
/*     DO: ~                                                                */
/*       MESSAGE "Det finnes artikkler med lager på denne butikken!" SKIP ~ */
/*               "Posten kan ikke slettes." ~                               */
/*               VIEW-AS ALERT-BOX MESSAGE TITLE "Melding". ~               */
/*       IF wModus = "SLETT" THEN                                           */
/*           RETURN "AVBRYT".                                               */
/*        ELSE                                                              */
/*           RETURN NO-APPLY.                                               */
/*     END.                                                                 */
  ASSIGN wOk = FALSE.
  MESSAGE "Skal butikken slettes?" VIEW-AS ALERT-BOX 
    QUESTION BUTTONS YES-NO
    TITLE "Bekreftelse"
    UPDATE wOk.
  IF wOk = FALSE THEN
    RETURN NO-APPLY "AVBRYT".  
  ELSE DO:
    FIND CURRENT Ekstbutiker EXCLUSIVE.
    DELETE Ekstbutiker.
    IF wModus = "SLETT" THEN
        RETURN "OK".
    ASSIGN 
          wInputRecid   = wNesteRecid
          wModus         = "ENDRE".
    FIND Ekstbutiker NO-LOCK WHERE RECID(Ekstbutiker) = wInputRecid.
    RUN VisPost.
    RUN BUTTONEnaDis.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButtonEnaDis C-ButikKort 
PROCEDURE ButtonEnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:  
  ASSIGN
    BUTTON-Ny:sensitive     = NOT wModus = "Ny"
    BUTTON-Kopier:sensitive = NOT wModus = "Ny"
    BUTTON-Slett:sensitive  = NOT wModus = "Ny"
    BUTTON-Prev:sensitive   = NOT wModus = "Ny"
    BUTTON-Next:sensitive   = NOT wModus = "Ny"
    BUTTON-Ok:sensitive     = NOT wModus = "Ny"
    BUTTON-Lagre:sensitive  = TRUE
    BUTTON-Angre:sensitive  = TRUE AND wInputRecid <> ?
    BUTTON-SokBut:SENSITIVE = TRUE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bytpost C-ButikKort 
PROCEDURE Bytpost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wRettning AS CHAR NO-UNDO.
  DEF VAR wSort AS INT NO-UNDO.
  IF Modifierad() THEN
    RUN LagrePost (0).

  IF RETURN-VALUE = "AVBRYT" THEN
    RETURN NO-APPLY.
  ELSE DO:
    ASSIGN wModus = "ENDRE".
    
    CASE wSort:
      WHEN 1 THEN
        DO:
          IF wRettning = "Next" THEN DO:
              FIND NEXT Butiker NO-LOCK USE-INDEX ButikIn NO-ERROR.    
              IF NOT AVAILABLE Butiker THEN
                 FIND LAST Butiker NO-LOCK USE-INDEX ButikIn  NO-ERROR.
          END.
          ELSE DO:
              FIND PREV Butiker NO-LOCK USE-INDEX ButikIn NO-ERROR.    
              IF NOT AVAILABLE Butiker THEN
                 FIND FIRST Butiker NO-LOCK USE-INDEX ButikIn  NO-ERROR.
          END.
        END.
      WHEN 2 THEN
        DO:
          IF wRettning = "Next" THEN DO:
              FIND NEXT Butiker NO-LOCK USE-INDEX ButNamn NO-ERROR.    
              IF NOT AVAILABLE Butiker THEN
                 FIND LAST Butiker NO-LOCK USE-INDEX ButNamn  NO-ERROR.
          END.
          ELSE DO:
              FIND PREV Butiker NO-LOCK USE-INDEX ButNamn NO-ERROR.    
              IF NOT AVAILABLE Butiker THEN
                 FIND FIRST Butiker NO-LOCK USE-INDEX ButNamn  NO-ERROR.
          END.
        END.
    END CASE.  
    IF AVAILABLE Butiker THEN
      DO:
        ASSIGN
          wInputRecid = RECID(Butiker).
        RUN InitCB2.
        RUN VisPost.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DelTmpChild C-ButikKort 
PROCEDURE DelTmpChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH tmpChild:
        IF VALID-HANDLE(tmpChild.wChild) THEN DO:
            RUN DelTmpChild IN tmpChild.wChild NO-ERROR.
            DELETE PROCEDURE tmpChild.wChild.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-ButikKort  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-ButikKort)
  THEN DELETE WIDGET C-ButikKort.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-ButikKort  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-Butik FILL-IN-ButNamn FILL-IN-Poststed FI-Kundenavn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ButikKort.
  IF AVAILABLE ekstbutiker THEN 
    DISPLAY ekstbutiker.Butik ekstbutiker.ButNamn ekstbutiker.KortNavn 
          ekstbutiker.BuAdr ekstbutiker.BuPonr ekstbutiker.BuPadr 
          ekstbutiker.BuKon ekstbutiker.BuTel ekstbutiker.ePostAdresse 
          ekstbutiker.OrganisasjonsNr ekstbutiker.KundeNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ButikKort.
  ENABLE BUTTON-SokBut B-KundeNr RECT-27 RECT-28 ekstbutiker.ButNamn 
         ekstbutiker.KortNavn ekstbutiker.BuAdr ekstbutiker.BuPonr 
         ekstbutiker.BuPadr ekstbutiker.BuKon ekstbutiker.BuTel 
         ekstbutiker.ePostAdresse ekstbutiker.OrganisasjonsNr BUTTON-Angre 
         BUTTON-Kopier BUTTON-Lagre BUTTON-Next BUTTON-Ny BUTTON-Prev 
         BUTTON-Slett Btn_Help BUTTON-Ok 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-ButikKort.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-ButikKort.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePost C-ButikKort 
PROCEDURE LagrePost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wBekreft AS INT NO-UNDO.
  DEF VAR ipStatus AS CHAR INIT "AVBRYT" NO-UNDO.
  DEF VAR bSvar    AS LOG NO-UNDO.

  DO WITH FRAME DEFAULT-FRAME:
    IF wModus = "NY" THEN DO:
      /* Sjekker input */
      IF INPUT Ekstbutiker.Butik = 0 THEN
        DO:
          MESSAGE "Butikknummer må være større enn 0"
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".
          APPLY "ENTRY":U TO Ekstbutiker.Butik.
          RETURN ipStatus.
        END.
      IF CAN-FIND(Ekstbutiker WHERE
                  Ekstbutiker.Butik = int(Ekstbutiker.Butik:screen-value)) THEN
        DO:
          MESSAGE "Butikk finnes allerede med nr:" Ekstbutiker.Butik:screen-value
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".        
          APPLY "ENTRY":U TO Ekstbutiker.Butik.
          RETURN ipStatus.
        END.
    END.
    IF NOT CAN-FIND(Post NO-LOCK WHERE 
         Post.PostNr = INPUT Ekstbutiker.BuPoNr) THEN
        DO:
          MESSAGE "Postnr finnes ikke"
          VIEW-AS ALERT-BOX TITLE "Lagringsfeil".        
          APPLY "ENTRY":U TO Ekstbutiker.BuPoNr.
          RETURN ipStatus.
        END.

    LAGRE_Ekstbutiker:
      DO TRANSACTION:
        IF wModus = "NY" THEN
          DO:
            RELEASE Ekstbutiker. /* Slipper gammel post. */
            CREATE Ekstbutiker.
            ASSIGN
               wInputRecid = RECID(Ekstbutiker)
               Ekstbutiker.Butik.
          END.
        ELSE DO:
          FIND CURRENT Ekstbutiker EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
          IF LOCKED Ekstbutiker THEN
            DO:
                MESSAGE "Butikken oppdateres fra en annen terminal" SKIP
                  "Forsök å lagre en gang til" VIEW-AS ALERT-BOX 
               WARNING TITLE "Lagringsfeil".
           RETURN NO-APPLY ipStatus.
          END.
        END.
        ASSIGN
             wModus       = "ENDRE"
             Ekstbutiker.Butik:SENSITIVE = FALSE
             Ekstbutiker.ButNamn
             Ekstbutiker.KortNavn
             Ekstbutiker.BuKon
             Ekstbutiker.BuAdr
             Ekstbutiker.BuPadr
             Ekstbutiker.BuPonr
             Ekstbutiker.BuTel
             Ekstbutiker.OrganisasjonsNr
             Ekstbutiker.ePostAdresse
             Ekstbutiker.KundeNr
             .
         FIND CURRENT Ekstbutiker NO-LOCK.
         RUN VisPost.
         RUN BUTTONEnaDis.
      END.
  END.
  RETURN "OK".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettModified C-ButikKort 
PROCEDURE SettModified :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER wModified AS LOGI NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN Ekstbutiker.Butik:MODIFIED = wModified
           Ekstbutiker.ButNamn:MODIFIED = wModified
           Ekstbutiker.BuKon:MODIFIED = wModified
           Ekstbutiker.BuAdr:MODIFIED = wModified
           Ekstbutiker.BuPadr:MODIFIED = wModified
           Ekstbutiker.BuPonr:MODIFIED = wModified
           Ekstbutiker.BuTel:MODIFIED = wModified
           Ekstbutiker.OrganisasjonsNr:MODIFIED = wModified.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTmpChild C-ButikKort 
PROCEDURE SkapaTmpChild :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipChild AS HANDLE NO-UNDO.
    IF VALID-HANDLE(ipChild) THEN DO:
        CREATE tmpChild.
        ASSIGN tmpChild.wChild = ipChild.
        RELEASE tmpChild.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettProg C-ButikKort 
PROCEDURE SlettProg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF VALID-HANDLE(wSubWin) THEN
    DELETE PROCEDURE wSubWin NO-ERROR.
  APPLY "CLOSE":U TO THIS-PROCEDURE. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPost C-ButikKort 
PROCEDURE VisPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wButPoststed AS CHAR NO-UNDO.
  DO WITH FRAME DEFAULT-FRAME:
    ASSIGN FILL-IN-Butik:SCREEN-VALUE = STRING(Ekstbutiker.Butik)
           FILL-IN-ButNamn:SCREEN-VALUE = Ekstbutiker.ButNamn.
  FIND Post WHERE Post.PostNr = Ekstbutiker.BuPoNr NO-LOCK NO-ERROR.
  ASSIGN wButPoststed = IF AVAIL Post THEN Post.Beskrivelse ELSE "".
  IF ekstButiker.kundenr <> 0 THEN DO:
      FIND kunde OF ekstButiker NO-LOCK NO-ERROR.
      IF AVAIL kunde THEN
          FI-Kundenavn = kunde.navn.
  END.
     DISPLAY 
         Ekstbutiker.Butik 
         Ekstbutiker.ButNamn
         Ekstbutiker.BuKon
         Ekstbutiker.BuAdr
         Ekstbutiker.BuPadr
         wButPoststed @ FILL-IN-Poststed
         Ekstbutiker.BuPonr
         Ekstbutiker.BuTel
         Ekstbutiker.KortNavn
         Ekstbutiker.OrganisasjonsNr
         Ekstbutiker.ePostAdresse
         Ekstbutiker.KundeNr
         FI-Kundenavn
         .
  RUN SettModified(FALSE).
 APPLY "ENTRY" TO Ekstbutiker.ButNamn.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WinHlp C-ButikKort 
PROCEDURE WinHlp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  {winhlp.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Modifierad C-ButikKort 
FUNCTION Modifierad RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    RETURN IF Ekstbutiker.Butik:MODIFIED = TRUE OR
              Ekstbutiker.ButNamn:MODIFIED = TRUE OR
              Ekstbutiker.BuKon:MODIFIED = TRUE OR
              Ekstbutiker.BuAdr:MODIFIED = TRUE OR
              Ekstbutiker.BuPadr:MODIFIED = TRUE OR
              Ekstbutiker.BuPonr:MODIFIED = TRUE OR
              Ekstbutiker.BuTel:MODIFIED = TRUE OR
              Ekstbutiker.OrganisasjonsNr:MODIFIED = TRUE THEN TRUE ELSE FALSE.
  END.
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION VisTid C-ButikKort 
FUNCTION VisTid RETURNS CHARACTER
  ( INPUT iTid AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN STRING(iTid,"HH:MM").   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

