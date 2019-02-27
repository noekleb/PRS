&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
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
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

DEF VAR bOk               AS LOG  NO-UNDO.
DEF VAR ix                AS INT  NO-UNDO.
DEF VAR iReturn           AS INT  NO-UNDO.
DEF VAR hParent           AS HANDLE NO-UNDO.

DEF VAR hParentBuffer     AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hTabFolder        AS HANDLE NO-UNDO.

DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame     AS HANDLE NO-UNDO.
DEF VAR iCurrTab          AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnLeveringsDato KOrdre_Id LeveringsDato ~
DeresRef Telefon VaarRef Referanse LevAdresse1 LevAdresse2 LevPostNr ~
LevLand FaktAdresse1 FaktAdresse2 FaktPostNr FaktLand LevPostSted ~
FaktPostSted btnLevPostnr RegistrertDato btnFaktPostnr RegistrertAv BetBet ~
rectOrdreHode rectTBordreHode rectFolder 
&Scoped-Define DISPLAYED-OBJECTS KOrdre_Id LeveringsDato DeresRef Telefon ~
VaarRef Referanse LevAdresse1 LevAdresse2 LevPostNr LevLand FaktAdresse1 ~
FaktAdresse2 FaktPostNr FaktLand LevPostSted FaktPostSted RegistrertDato ~
RegistrertAv BetBet 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnFaktPostnr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnLeveringsDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnLevPostnr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE BetBet AS CHARACTER FORMAT "X(256)":U 
     LABEL "Bet.bet" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 32.6 BY 1 NO-UNDO.

DEFINE VARIABLE DeresRef AS CHARACTER FORMAT "X(30)" 
     LABEL "Deres ref" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FaktAdresse1 AS CHARACTER FORMAT "X(30)" 
     LABEL "Fakt.adr" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FaktAdresse2 AS CHARACTER FORMAT "X(30)" 
     LABEL "Fakt.Adr" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FaktLand AS CHARACTER FORMAT "X(30)" 
     LABEL "Fakt.land" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FaktPostNr AS CHARACTER FORMAT "X(15)" 
     LABEL "Fakt.pnr" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE FaktPostSted AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1 NO-UNDO.

DEFINE VARIABLE KOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Ordrenr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE LevAdresse1 AS CHARACTER FORMAT "X(40)" 
     LABEL "Lev.adr" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE LevAdresse2 AS CHARACTER FORMAT "X(40)" 
     LABEL "Lev.adr" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE LeveringsDato AS DATE FORMAT "99/99/9999":U 
     LABEL "Lev.dato" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1 NO-UNDO.

DEFINE VARIABLE LevLand AS CHARACTER FORMAT "X(30)" 
     LABEL "Lev. Land" 
     VIEW-AS FILL-IN 
     SIZE 32.2 BY 1.

DEFINE VARIABLE LevPostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "Lev.postnr" 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.

DEFINE VARIABLE LevPostSted AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE Referanse AS CHARACTER FORMAT "X(30)" 
     LABEL "Referanse" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1 TOOLTIP "Evt. bestillingsnummer".

DEFINE VARIABLE RegistrertAv AS CHARACTER FORMAT "X(10)" 
     LABEL "av" 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1.

DEFINE VARIABLE RegistrertDato AS DATE FORMAT "99/99/9999" 
     LABEL "Reg" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE Telefon AS CHARACTER FORMAT "X(15)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE VaarRef AS CHARACTER FORMAT "X(30)" 
     LABEL "Vår ref" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE RECTANGLE rectFolder
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 142 BY 10.48.

DEFINE RECTANGLE rectOrdreHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 55.4 BY 12.38.

DEFINE RECTANGLE rectTBordreHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnLeveringsDato AT ROW 2.67 COL 128.4
     KOrdre_Id AT ROW 2.67 COL 67.4 COLON-ALIGNED HELP
          "Internt faktura id. Tildeles autmatisk av systemet."
     LeveringsDato AT ROW 2.67 COL 109.4 COLON-ALIGNED
     DeresRef AT ROW 3.71 COL 67.4 COLON-ALIGNED HELP
          "Deres referanse"
     Telefon AT ROW 3.71 COL 109.4 COLON-ALIGNED HELP
          "Telefon"
     VaarRef AT ROW 4.76 COL 67.4 COLON-ALIGNED HELP
          "Vår referanse"
     Referanse AT ROW 5.81 COL 67.4 COLON-ALIGNED HELP
          "Referanse til kundens ordre id."
     LevAdresse1 AT ROW 11 COL 67.8 COLON-ALIGNED HELP
          "Kundens adresse"
     LevAdresse2 AT ROW 12 COL 67.8 COLON-ALIGNED HELP
          "Kundens adresse"
     LevPostNr AT ROW 13 COL 67.8 COLON-ALIGNED HELP
          "Postnummer"
     LevLand AT ROW 14.05 COL 67.8 COLON-ALIGNED HELP
          "Land"
     FaktAdresse1 AT ROW 11 COL 109.6 COLON-ALIGNED HELP
          "Fakturaadresse"
     FaktAdresse2 AT ROW 12 COL 109.6 COLON-ALIGNED HELP
          "Fakturaadresse"
     FaktPostNr AT ROW 13.05 COL 109.6 COLON-ALIGNED HELP
          "Postnr. fakturaadresse."
     FaktLand AT ROW 14.1 COL 109.6 COLON-ALIGNED HELP
          "Land"
     LevPostSted AT ROW 13.05 COL 84.2 COLON-ALIGNED HELP
          "Poststed" NO-LABEL
     FaktPostSted AT ROW 13.05 COL 126 COLON-ALIGNED NO-LABEL
     btnLevPostnr AT ROW 13.05 COL 82 NO-TAB-STOP 
     RegistrertDato AT ROW 1.29 COL 114 COLON-ALIGNED HELP
          "Dato da posten ble registrert i registeret"
     btnFaktPostnr AT ROW 13.05 COL 123.8 NO-TAB-STOP 
     RegistrertAv AT ROW 1.24 COL 133.6 COLON-ALIGNED HELP
          "Brukerid på den som registrerte posten"
     BetBet AT ROW 6.91 COL 67.4 COLON-ALIGNED
     rectOrdreHode AT ROW 2.67 COL 1.6
     rectTBordreHode AT ROW 1.24 COL 1.8
     rectFolder AT ROW 15.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 143.6 BY 24.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 24.95
         WIDTH              = 143.8
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
                                                                        */
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME
ASSIGN C-Win = CURRENT-WINDOW.


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   Custom                                                               */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 24.95
       FRAME DEFAULT-FRAME:WIDTH            = 143.6.

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

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFaktPostnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFaktPostnr C-Win
ON CHOOSE OF btnFaktPostnr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cZipCodeFieldList    AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Post"
                     + ";Beskrivelse"
                     + ";Postnr"
                     ,
                   "WHERE false"
                    ,""
                    ,"Beskrivelse,Postnr",
                    OUTPUT cZipCodeFieldList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cZipCodeFieldList NE "" THEN DO:
    ASSIGN 
       FaktPostSted:SCREEN-VALUE   = ENTRY(1,cZipCodeFieldList,"|")
       FaktPostNr:SCREEN-VALUE     = ENTRY(2,cZipCodeFieldList,"|")
       .
    APPLY "any-printable" TO FaktPostnr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLeveringsDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLeveringsDato C-Win
ON CHOOSE OF btnLeveringsDato IN FRAME DEFAULT-FRAME /* ... */
DO:
  RUN Cal.w (LeveringsDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLevPostnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLevPostnr C-Win
ON CHOOSE OF btnLevPostnr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cZipCodeFieldList    AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Post"
                     + ";Beskrivelse"
                     + ";Postnr"
                     ,
                   "WHERE false"
                    ,""
                    ,"Beskrivelse,Postnr",
                    OUTPUT cZipCodeFieldList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cZipCodeFieldList NE "" THEN DO:
    ASSIGN 
       LevPostSted:SCREEN-VALUE  = ENTRY(1,cZipCodeFieldList,"|")
       LevPostNr:SCREEN-VALUE    = ENTRY(2,cZipCodeFieldList,"|")
       .
    APPLY "any-printable" TO LevPostnr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FaktPostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FaktPostNr C-Win
ON F10 OF FaktPostNr IN FRAME DEFAULT-FRAME /* Fakt.pnr */
DO:
  APPLY "choose" TO btnFaktPostNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevPostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevPostNr C-Win
ON F10 OF LevPostNr IN FRAME DEFAULT-FRAME /* Lev.postnr */
DO:
  APPLY "choose" TO btnLevPostNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  PUBLISH "InvalidateHandle".
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/supptrigg.i hFieldMap}

ON 'alt-s':U OF FRAME {&FRAME-NAME} ANYWHERE
  PUBLISH "AltSKundeOrdre" (THIS-PROCEDURE:FILE-NAME).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  HIDE FRAME DEFAULT-FRAME.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY KOrdre_Id LeveringsDato DeresRef Telefon VaarRef Referanse LevAdresse1 
          LevAdresse2 LevPostNr LevLand FaktAdresse1 FaktAdresse2 FaktPostNr 
          FaktLand LevPostSted FaktPostSted RegistrertDato RegistrertAv BetBet 
      WITH FRAME DEFAULT-FRAME.
  ENABLE btnLeveringsDato KOrdre_Id LeveringsDato DeresRef Telefon VaarRef 
         Referanse LevAdresse1 LevAdresse2 LevPostNr LevLand FaktAdresse1 
         FaktAdresse2 FaktPostNr FaktLand LevPostSted FaktPostSted btnLevPostnr 
         RegistrertDato btnFaktPostnr RegistrertAv BetBet rectOrdreHode 
         rectTBordreHode rectFolder 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cTabList    AS CHAR   NO-UNDO.
DEF VAR hPageObject AS HANDLE NO-UNDO.
DEF VAR iy          AS INT    NO-UNDO.
DEF VAR hTabFrame   AS HANDLE NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

  ASSIGN BetBet:DELIMITER = "|"
         BetBet:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList",
                                                     "Betalingsbetingelser;BetTekst;BetBet",
                                                     "WHERE true")
         .

  /* Create the browse: */
   hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                           rectOrdreHode:HANDLE,        /* Coordinates */
                           100,                      /* Batchsize */
                           "",                       /* Attributes that must be set at creation time for the browse */
                           "KOrdreHode"              /* Buffers and fields (and calculated fields) for browse */
                           + ";KOrdre_Id|Ordrenr"
                           + ";Leveringsdato"
                           + ";DeresRef"
                           + ";VaarRef"
                           + ";Referanse"
                           + ";Telefon"
                           + ";LevAdresse1"
                           + ";LevAdresse2"
                           + ";LevPostNr"
                           + ";LevPostSted"
                           + ";LevLand"
                           + ";FaktAdresse1"
                           + ";FaktAdresse2"
                           + ";FaktPostNr"
                           + ";FaktPostSted"
                           + ";FaktLand"
                           + ";!BetBet"
                           + ";!KundeNr"
                           + ";!RegistrertDato"
                           + ";!RegistrertAv"
                           , 
                           "WHERE false"
                           ,"sort|Leveringsdato desc").         
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  hBrowse:NAME = "brwOrdreHode".

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                           hBrowse:QUERY,
                           FRAME {&FRAME-NAME}:HANDLE,
                           "LeveringsDato,VaarRef,Telefon,DeresRef,Referanse,BetBet,LevAdresse1,LevAdresse2,LevPostNr,LevLand,LevPostSted,FaktAdresse1,FaktAdresse2,FaktPostNr,FaktPostSted,FaktLand","",
                           "KOrdre_Id,RegistrertAv,RegistrertDato","",
                           "btnLeveringsDato,btnLevPostnr,btnFaktPostnr").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextrafields","Kundenr").
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hBrowse).

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                            rectTBOrdreHode:HANDLE,
                            "",
                            "new;Ny,undo;Angre,delete;Slett,save;Lagre,Print,excel",
                            "maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).


  hTabFolder = DYNAMIC-FUNCTION("NewTabFolder",rectFolder:HANDLE).
  
  IF cTabList = "" THEN
    cTabList = "Linjer|KOrdreLinje.w".

  DO ix = 1 TO NUM-ENTRIES(cTabList,"|") BY 2:
    IF ix MOD 2 = 1 THEN iy = iy + 1.
    hPageObject = DYNAMIC-FUNCTION("addFolder" IN hTabFolder,iy,ENTRY(ix,cTabList,"|"),ENTRY(ix + 1,cTabList,"|"),"").
    DYNAMIC-FUNCTION("setQuery" IN hPageObject,hBrowse).
    RUN InitializeObject IN hPageObject.
    DYNAMIC-FUNCTION("setAttribute",hTabFolder,"pageframe" + STRING(iy),STRING(DYNAMIC-FUNCTION("getFrameHandle" IN hPageObject))).
  END.

  DYNAMIC-FUNCTION("buildFolder" IN hTabFolder).
  hTabFrame = DYNAMIC-FUNCTION("getTabFrame" IN hTabFolder).

  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectTBordreHode,rectOrdreHode,brwOrdreHode").
  DYNAMIC-FUNCTION("setAnchor",THIS-PROCEDURE:CURRENT-WINDOW,hBrowse,
                    STRING(RegistrertAv:HANDLE) + "," 
                  + STRING(RegistrertDato:HANDLE) + "," 
                  + STRING(LeveringsDato:HANDLE) + "," 
                  + STRING(KOrdre_Id:HANDLE) + "," 
                  + STRING(Telefon:HANDLE) + "," 
                  + STRING(VaarRef:HANDLE) + "," 
                  + STRING(DeresRef:HANDLE) + "," 
                  + STRING(Referanse:HANDLE) + "," 
                  + STRING(LevAdresse1:HANDLE) + "," 
                  + STRING(LevAdresse2:HANDLE) + "," 
                  + STRING(LevPostNr:HANDLE) + "," 
                  + STRING(LevPostSted:HANDLE) + "," 
                  + STRING(LevLand:HANDLE) + "," 
                  + STRING(FaktAdresse1:HANDLE) + "," 
                  + STRING(FaktAdresse2:HANDLE) + "," 
                  + STRING(FaktPostNr:HANDLE) + "," 
                  + STRING(FaktPostSted:HANDLE) + "," 
                  + STRING(FaktLand:HANDLE) + "," 
                  + STRING(btnLevPostnr:HANDLE) + ","
                  + STRING(btnFaktPostnr:HANDLE) + ","
                  + STRING(btnLeveringsDato:HANDLE) + ","
                  + STRING(BetBet:HANDLE)
                   ,"").

END.

DYNAMIC-FUNCTION("TabFolderChanged" IN hTabFolder,1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfField C-Win 
PROCEDURE LeaveOfField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DEF VAR cZipCodeFieldList    AS CHAR NO-UNDO.

CASE icFieldName:
  WHEN 'FaktPostNr' THEN DO WITH FRAME {&FRAME-NAME}:
    cZipCodeFieldList = DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + FaktPostNr:SCREEN-VALUE + "'"
                                         ,"Beskrivelse,PostNr").
    IF cZipCodeFieldList NE ? THEN
      ASSIGN 
         FaktPostSted:SCREEN-VALUE   = ENTRY(1,cZipCodeFieldList,"|")
         FaktPostNr:SCREEN-VALUE     = ENTRY(2,cZipCodeFieldList,"|")
         .
    ELSE
      ASSIGN 
         FaktPostSted:SCREEN-VALUE   = ""
         .
  END.
  WHEN 'LevPostNr' THEN DO WITH FRAME {&FRAME-NAME}:
    cZipCodeFieldList = DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + LevPostNr:SCREEN-VALUE + "'"
                                         ,"Beskrivelse,PostNr").
    IF cZipCodeFieldList NE ? THEN
      ASSIGN 
         LevPostSted:SCREEN-VALUE   = ENTRY(1,cZipCodeFieldList,"|")
         LevPostNr:SCREEN-VALUE     = ENTRY(2,cZipCodeFieldList,"|")
         .
    ELSE
      ASSIGN 
         LevPostSted:SCREEN-VALUE   = ""
         .
  END.
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hWidget AS HANDLE NO-UNDO.
hWidget = hBrowse:GET-BROWSE-COLUMN(1).
APPLY "end-resize" TO hWidget.
FRAME {&FRAME-NAME}:MOVE-TO-TOP().

DYNAMIC-FUNCTION("MoveTabToTop" IN hTabFolder,rectFolder:HANDLE IN FRAME {&FRAME-NAME}).

APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iSelection AS INT NO-UNDO.
RUN JBoxDSelectFunction.w ("Ordrebekreftelse|Faktura",OUTPUT iSelection).
IF iSelection NE 0 THEN DO:
  DYNAMIC-FUNCTION("runproc","kordre_produksjon.p",STRING(hFieldMap:BUFFER-FIELD("KOrdreId"):BUFFER-VALUE),?).
  IF iSelection = 2 THEN
    DYNAMIC-FUNCTION("runproc","faktura_produksjon.p",STRING(hFieldMap:BUFFER-FIELD("KOrdreId"):BUFFER-VALUE),?).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferextravalues",STRING(hParentBuffer:BUFFER-FIELD("Kundenr"):BUFFER-VALUE)).
RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setLookupAttributes C-Win 
PROCEDURE setLookupAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihBrowse AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy1 AS HANDLE NO-UNDO.
DEF INPUT PARAM ihDummy2 AS HANDLE NO-UNDO.

IF CAN-DO("Post",ihBrowse:QUERY:GET-BUFFER-HANDLE(1):NAME) THEN
  DYNAMIC-FUNCTION("setAttribute",ihBrowse,"basequery","").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSearch C-Win 
PROCEDURE setSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icKOrdre_id AS CHAR NO-UNDO.

bOk = hFieldMap:FIND-FIRST("WHERE KOrdre_id = " + icKOrdre_id) NO-ERROR.
IF NOT bOK THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"querywhere","WHERE KOrdre_id = " + icKOrdre_id).
  RUN OpenQuery.
END.
ELSE DO:
  hBrowse:QUERY:REPOSITION-TO-ROWID(hFieldMap:ROWID).
  RUN DisplayRecord.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFrameHandle C-Win 
FUNCTION getFrameHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN FRAME {&FRAME-NAME}:HANDLE. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParentBrowse C-Win 
FUNCTION getParentBrowse RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBrowse.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParent C-Win 
FUNCTION setParent RETURNS LOGICAL
  ( INPUT ihParent AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  hParent = ihParent.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setQuery C-Win 
FUNCTION setQuery RETURNS LOGICAL
  (INPUT ihQuery AS HANDLE) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hParentBuffer = ihQuery:QUERY:GET-BUFFER-HANDLE(1).
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION TabChanged C-Win 
FUNCTION TabChanged RETURNS LOGICAL
  ( INPUT iiTab AS INT ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF iCurrTab > 0 THEN
  DYNAMIC-FUNCTION("DeleteObjectLink",hBrowse,DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iCurrTab)).

DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledevents","").
DYNAMIC-FUNCTION("CreateParentLink",DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,iiTab),hBrowse,"KOrdre_Id").

ASSIGN hCurrTabProc  = DYNAMIC-FUNCTION("getPageHandle" IN hTabFolder,iiTab)
       hCurrTabFrame = DYNAMIC-FUNCTION("getPageFrame" IN hTabFolder,iiTab)
       iCurrTab      = iiTab
       .

RUN MoveToTop.

DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
RUN DisplayRecord.

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

