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
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hTabFolder        AS HANDLE NO-UNDO.

DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame     AS HANDLE NO-UNDO.
DEF VAR iCurrTab          AS INT NO-UNDO.


DEF VAR hLinjeNrOverlay   AS HANDLE NO-UNDO.
DEF VAR hVareNrOverlay    AS HANDLE NO-UNDO.
DEF VAR hVareTekstOverlay AS HANDLE NO-UNDO.
DEF VAR hStorlOverlay     AS HANDLE NO-UNDO.
DEF VAR hAntallOverlay    AS HANDLE NO-UNDO.
DEF VAR hLinjeRabOverlay  AS HANDLE NO-UNDO.

DEF VAR hArtBasSok        AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmFaktLinje

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FirmaNavn FirmaAdresse1 FirmaAdresse2 ~
Adresse1 Adresse2 PostNr PostSted Telefon Telefaks MobilTlf ePostAdresse ~
KontNavn FaktAdresse1 btnFaktPostnr FaktAdresse2 FaktPostNr FaktPoststed ~
FaktLand LevAdresse1 LevAdresse2 LevPostNr LevPostSted LevLand btnLevPostnr ~
btnPostnr 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-20 FirmaNavn FILL-IN-21 ~
FirmaAdresse1 FILL-IN-22 FirmaAdresse2 FILL-IN-23 Adresse1 Adresse2 PostNr ~
PostSted Telefon Telefaks MobilTlf ePostAdresse KontNavn FaktAdresse1 ~
FaktAdresse2 FaktPostNr FaktPoststed FaktLand LevAdresse1 LevAdresse2 ~
LevPostNr LevPostSted LevLand 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getKundeInfo C-Win 
FUNCTION getKundeInfo RETURNS LOGICAL
  ( OUTPUT icKunde AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setKunde C-Win 
FUNCTION setKunde RETURNS LOGICAL
  ( INPUT icKunde AS CHAR )  FORWARD.

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


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnFaktPostnr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnLevPostnr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnPostnr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE Adresse1 AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 41.6 BY 1.

DEFINE VARIABLE Adresse2 AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 41.6 BY 1.

DEFINE VARIABLE ePostAdresse AS CHARACTER FORMAT "X(50)" 
     LABEL "Epost" 
     VIEW-AS FILL-IN 
     SIZE 41.6 BY 1.

DEFINE VARIABLE FaktAdresse1 AS CHARACTER FORMAT "X(40)" 
     LABEL "Fakt.adr" 
     VIEW-AS FILL-IN 
     SIZE 41.6 BY 1.

DEFINE VARIABLE FaktAdresse2 AS CHARACTER FORMAT "X(40)" 
     LABEL "Fakt.adr" 
     VIEW-AS FILL-IN 
     SIZE 41.6 BY 1.

DEFINE VARIABLE FaktLand AS CHARACTER FORMAT "X(30)" 
     LABEL "Land" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE FaktPostNr AS CHARACTER FORMAT "X(15)" 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE FaktPoststed AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1.

DEFINE VARIABLE FILL-IN-20 AS CHARACTER FORMAT "X(256)":U INITIAL "Eget firmanavn og adresse" 
      VIEW-AS TEXT 
     SIZE 42 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-21 AS CHARACTER FORMAT "X(256)":U INITIAL "Kundens adresse" 
      VIEW-AS TEXT 
     SIZE 42 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-22 AS CHARACTER FORMAT "X(256)":U INITIAL "Kundens leveringsadresse" 
      VIEW-AS TEXT 
     SIZE 41 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FILL-IN-23 AS CHARACTER FORMAT "X(256)":U INITIAL "Kundens fakturaadresse" 
      VIEW-AS TEXT 
     SIZE 41 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FirmaAdresse1 AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse 1" 
     VIEW-AS FILL-IN 
     SIZE 41.6 BY 1.

DEFINE VARIABLE FirmaAdresse2 AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse 2" 
     VIEW-AS FILL-IN 
     SIZE 41.6 BY 1.

DEFINE VARIABLE FirmaNavn AS CHARACTER FORMAT "X(40)" 
     LABEL "Eget firmanavn" 
     VIEW-AS FILL-IN 
     SIZE 41.6 BY 1.

DEFINE VARIABLE KontNavn AS CHARACTER FORMAT "X(40)" 
     LABEL "Kont.pers" 
     VIEW-AS FILL-IN 
     SIZE 41.6 BY 1.

DEFINE VARIABLE LevAdresse1 AS CHARACTER FORMAT "X(40)" 
     LABEL "Lev.adr" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE LevAdresse2 AS CHARACTER FORMAT "X(40)" 
     LABEL "Lev.adr" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE LevLand AS CHARACTER FORMAT "X(30)" 
     LABEL "Lev. Land" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE LevPostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "PostNr" 
     VIEW-AS FILL-IN 
     SIZE 13 BY 1.

DEFINE VARIABLE LevPostSted AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 24.6 BY 1.

DEFINE VARIABLE MobilTlf AS CHARACTER FORMAT "X(15)" 
     LABEL "MobilTlf" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Mobiltelefon".

DEFINE VARIABLE PostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "PostNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE PostSted AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 24.4 BY 1.

DEFINE VARIABLE Telefaks AS CHARACTER FORMAT "X(15)" 
     LABEL "Fax" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE Telefon AS CHARACTER FORMAT "X(15)" 
     LABEL "Tlf" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmFaktLinje
     FILL-IN-20 AT ROW 1 COL 22 COLON-ALIGNED NO-LABEL
     FirmaNavn AT ROW 1.67 COL 22 COLON-ALIGNED
     FILL-IN-21 AT ROW 4.67 COL 22 COLON-ALIGNED NO-LABEL
     FirmaAdresse1 AT ROW 2.67 COL 22 COLON-ALIGNED
     FILL-IN-22 AT ROW 6.76 COL 91.4 COLON-ALIGNED NO-LABEL
     FirmaAdresse2 AT ROW 3.67 COL 22 COLON-ALIGNED
     FILL-IN-23 AT ROW 1.14 COL 91 COLON-ALIGNED NO-LABEL
     Adresse1 AT ROW 5.33 COL 22 COLON-ALIGNED HELP
          "Kundens adresse"
     Adresse2 AT ROW 6.33 COL 22 COLON-ALIGNED HELP
          "Kundens adresse"
     PostNr AT ROW 7.33 COL 22 COLON-ALIGNED HELP
          "Postnummer"
     PostSted AT ROW 7.33 COL 39.2 COLON-ALIGNED HELP
          "Poststed" NO-LABEL
     Telefon AT ROW 8.33 COL 22 COLON-ALIGNED HELP
          "Telefon"
     Telefaks AT ROW 8.33 COL 46.6 COLON-ALIGNED HELP
          "Telefaks"
     MobilTlf AT ROW 9.33 COL 22 COLON-ALIGNED HELP
          "Mobiltelefon"
     ePostAdresse AT ROW 10.33 COL 22 COLON-ALIGNED HELP
          "E-Post adresse"
     KontNavn AT ROW 11.33 COL 22 COLON-ALIGNED HELP
          "Navn på kontaktperson"
     FaktAdresse1 AT ROW 1.81 COL 91 COLON-ALIGNED HELP
          "Fakturaadresse"
     btnFaktPostnr AT ROW 3.76 COL 106.2 NO-TAB-STOP 
     FaktAdresse2 AT ROW 2.81 COL 91 COLON-ALIGNED HELP
          "Fakturaadresse"
     FaktPostNr AT ROW 3.81 COL 91 COLON-ALIGNED HELP
          "Postnr. fakturaadresse."
     FaktPoststed AT ROW 3.81 COL 108.6 COLON-ALIGNED HELP
          "Poststed fakturaadresse." NO-LABEL
     FaktLand AT ROW 4.81 COL 91 COLON-ALIGNED HELP
          "Land"
     LevAdresse1 AT ROW 7.43 COL 91 COLON-ALIGNED HELP
          "Kundens adresse"
     LevAdresse2 AT ROW 8.43 COL 91 COLON-ALIGNED HELP
          "Kundens adresse"
     LevPostNr AT ROW 9.43 COL 91 COLON-ALIGNED HELP
          "Postnummer"
     LevPostSted AT ROW 9.43 COL 108.4 COLON-ALIGNED HELP
          "Poststed" NO-LABEL
     LevLand AT ROW 10.43 COL 91 COLON-ALIGNED HELP
          "Land"
     btnLevPostnr AT ROW 9.43 COL 106.2 NO-TAB-STOP 
     btnPostnr AT ROW 7.33 COL 37 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 136 BY 11.67.


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
         HEIGHT             = 11.67
         WIDTH              = 136.4
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
/* SETTINGS FOR FRAME frmFaktLinje
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME frmFaktLinje:HEIGHT           = 11.67
       FRAME frmFaktLinje:WIDTH            = 136.

/* SETTINGS FOR FILL-IN FILL-IN-20 IN FRAME frmFaktLinje
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-20:READ-ONLY IN FRAME frmFaktLinje        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-21 IN FRAME frmFaktLinje
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-21:READ-ONLY IN FRAME frmFaktLinje        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-22 IN FRAME frmFaktLinje
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-22:READ-ONLY IN FRAME frmFaktLinje        = TRUE.

/* SETTINGS FOR FILL-IN FILL-IN-23 IN FRAME frmFaktLinje
   NO-ENABLE                                                            */
ASSIGN 
       FILL-IN-23:READ-ONLY IN FRAME frmFaktLinje        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmFaktLinje
/* Query rebuild information for FRAME frmFaktLinje
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frmFaktLinje */
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
ON CHOOSE OF btnFaktPostnr IN FRAME frmFaktLinje /* ... */
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
    APPLY "entry" TO FaktLand.
  END.
  ELSE APPLY "entry" TO FaktPostnr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLevPostnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLevPostnr C-Win
ON CHOOSE OF btnLevPostnr IN FRAME frmFaktLinje /* ... */
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
       LevPostSted:SCREEN-VALUE   = ENTRY(1,cZipCodeFieldList,"|")
       LevPostNr:SCREEN-VALUE     = ENTRY(2,cZipCodeFieldList,"|")
       .
    APPLY "any-printable" TO LevPostnr.
    APPLY "entry" TO LevLand.
  END.
  ELSE APPLY "entry" TO LevPostnr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPostnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPostnr C-Win
ON CHOOSE OF btnPostnr IN FRAME frmFaktLinje /* ... */
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
       PostSted:SCREEN-VALUE   = ENTRY(1,cZipCodeFieldList,"|")
       PostNr:SCREEN-VALUE     = ENTRY(2,cZipCodeFieldList,"|")
       .
    APPLY "any-printable" TO Postnr.
    APPLY "entry" TO Telefon.
  END.
  ELSE APPLY "entry" TO PostNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FaktPostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FaktPostNr C-Win
ON F10 OF FaktPostNr IN FRAME frmFaktLinje /* Postnr */
OR F3 OF FaktPostNr DO:
  APPLY "choose" TO btnFaktPostnr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevPostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevPostNr C-Win
ON F10 OF LevPostNr IN FRAME frmFaktLinje /* PostNr */
OR F3 OF LevPostNr DO:
  APPLY "choose" TO btnLevPostnr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PostNr C-Win
ON F10 OF PostNr IN FRAME frmFaktLinje /* PostNr */
DO:
  APPLY "choose" TO btnPostnr.
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
  IF VALID-HANDLE(hArtBasSok) THEN APPLY "close" TO hArtBasSok.
  PUBLISH "InvalidateHandle".
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
{incl/supptrigg.i hParentBuffer}

/* ON 'alt-s':U OF FRAME {&FRAME-NAME} ANYWHERE DO: */
/*   MESSAGE PROGRAM-NAME(1) SKIP                   */
/*            SKIP                                  */
/*           VIEW-AS ALERT-BOX.                     */
/*   PUBLISH "AltSKundeOrdre" ("kundeordre").       */
/* END.                                             */
/*                                                  */

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
  HIDE FRAME frmFaktLinje.
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
  DISPLAY FILL-IN-20 FirmaNavn FILL-IN-21 FirmaAdresse1 FILL-IN-22 FirmaAdresse2 
          FILL-IN-23 Adresse1 Adresse2 PostNr PostSted Telefon Telefaks MobilTlf 
          ePostAdresse KontNavn FaktAdresse1 FaktAdresse2 FaktPostNr 
          FaktPoststed FaktLand LevAdresse1 LevAdresse2 LevPostNr LevPostSted 
          LevLand 
      WITH FRAME frmFaktLinje.
  ENABLE FirmaNavn FirmaAdresse1 FirmaAdresse2 Adresse1 Adresse2 PostNr 
         PostSted Telefon Telefaks MobilTlf ePostAdresse KontNavn FaktAdresse1 
         btnFaktPostnr FaktAdresse2 FaktPostNr FaktPoststed FaktLand 
         LevAdresse1 LevAdresse2 LevPostNr LevPostSted LevLand btnLevPostnr 
         btnPostnr 
      WITH FRAME frmFaktLinje.
  {&OPEN-BROWSERS-IN-QUERY-frmFaktLinje}
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
DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").


  DYNAMIC-FUNCTION("AppendFieldMap",
                   hParentBuffer,
                   FRAME {&FRAME-NAME}:HANDLE,
                   "FirmaNavn,FirmaAdresse1,firmaAdresse2,Adresse1,Adresse2,PostNr,PostSted,ePostAdresse,KontNavn,FaktAdresse1,FaktAdresse2,FaktLand,FaktPostNr,FaktPoststed,LevAdresse1,LevAdresse2,LevLand,LevPostNr,LevPostSted,Telefaks,Telefon,MobilTlf","",
                   "","",
                   "btnPostnr,btnFaktPostnr,btnLevPostnr").

  DYNAMIC-FUNCTION("setAdresseProc" IN hParent).
END.
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

DEF VAR cValueList    AS CHAR NO-UNDO.
DEF VAR hCurrWidget   AS HANDLE NO-UNDO.

hCurrWidget = DYNAMIC-FUNCTION("getCurrentWidget").
IF NOT hCurrWidget:MODIFIED THEN RETURN.

CASE icFieldName:
  WHEN 'PostNr' THEN DO WITH FRAME {&FRAME-NAME}:
    cValueList = DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + PostNr:SCREEN-VALUE + "'"
                                         ,"Beskrivelse,PostNr").
    IF cValueList NE ? THEN
      ASSIGN 
         PostSted:SCREEN-VALUE   = ENTRY(1,cValueList,"|")
         PostNr:SCREEN-VALUE     = ENTRY(2,cValueList,"|")
         .
    ELSE
      ASSIGN 
         PostSted:SCREEN-VALUE   = ""
         .
  END.
  WHEN 'FaktPostNr' THEN DO WITH FRAME {&FRAME-NAME}:
    cValueList = DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + FaktPostNr:SCREEN-VALUE + "'"
                                         ,"Beskrivelse,PostNr").
    IF cValueList NE ? THEN
      ASSIGN 
         FaktPostSted:SCREEN-VALUE   = ENTRY(1,cValueList,"|")
         FaktPostNr:SCREEN-VALUE     = ENTRY(2,cValueList,"|")
         .
    ELSE
      ASSIGN 
         FaktPostSted:SCREEN-VALUE   = ""
         .
  END.
END CASE.

CASE icFieldName:
  WHEN 'LevPostNr' THEN DO WITH FRAME {&FRAME-NAME}:
    cValueList = DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + LevPostNr:SCREEN-VALUE + "'"
                                         ,"Beskrivelse,PostNr").
    IF cValueList NE ? THEN
      ASSIGN 
         LevPostSted:SCREEN-VALUE   = ENTRY(1,cValueList,"|")
         LevPostNr:SCREEN-VALUE     = ENTRY(2,cValueList,"|")
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
FRAME {&FRAME-NAME}:MOVE-TO-TOP().
APPLY "entry" TO Adresse1.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN NewRecord IN hParent.

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
RUN SaveRecord IN hParent.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getKundeInfo C-Win 
FUNCTION getKundeInfo RETURNS LOGICAL
  ( OUTPUT icKunde AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:


  ASSIGN icKunde = '||||||||||||||||||||'
         ENTRY(2,icKunde,"|") = Adresse1:SCREEN-VALUE     
         ENTRY(3,icKunde,"|") = Adresse2:SCREEN-VALUE     
         ENTRY(4,icKunde,"|") = PostNr:SCREEN-VALUE       
         ENTRY(7,icKunde,"|") = ePostAdresse:SCREEN-VALUE 
         ENTRY(8,icKunde,"|") = Telefon:SCREEN-VALUE      
         ENTRY(9,icKunde,"|") = Telefaks:SCREEN-VALUE     
         ENTRY(10,icKunde,"|") = KontNavn:SCREEN-VALUE     
         ENTRY(11,icKunde,"|") = FaktAdresse1:SCREEN-VALUE 
         ENTRY(12,icKunde,"|") = FaktAdresse2:SCREEN-VALUE 
         ENTRY(13,icKunde,"|") = FaktPostNr:SCREEN-VALUE   
         ENTRY(14,icKunde,"|") = FaktLand:SCREEN-VALUE     
         ENTRY(15,icKunde,"|") = LevAdresse1:SCREEN-VALUE  
         ENTRY(16,icKunde,"|") = LevAdresse2:SCREEN-VALUE  
         ENTRY(17,icKunde,"|") = LevPostNr:SCREEN-VALUE    
         ENTRY(18,icKunde,"|") = LevLand:SCREEN-VALUE      
         ENTRY(19,icKunde,"|") = MobilTlf:SCREEN-VALUE     
         .

END.
RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setKunde C-Win 
FUNCTION setKunde RETURNS LOGICAL
  ( INPUT icKunde AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:


  ASSIGN Adresse1:SCREEN-VALUE     = ENTRY(2,icKunde,"|")
         Adresse2:SCREEN-VALUE     = ENTRY(3,icKunde,"|")
         PostNr:SCREEN-VALUE       = ENTRY(4,icKunde,"|")
         ePostAdresse:SCREEN-VALUE = ENTRY(7,icKunde,"|")
         Telefon:SCREEN-VALUE      = ENTRY(8,icKunde,"|")
         Telefaks:SCREEN-VALUE     = ENTRY(9,icKunde,"|")
         KontNavn:SCREEN-VALUE     = ENTRY(10,icKunde,"|")
         FaktAdresse1:SCREEN-VALUE = ENTRY(11,icKunde,"|")
         FaktAdresse2:SCREEN-VALUE = ENTRY(12,icKunde,"|")
         FaktPostNr:SCREEN-VALUE   = ENTRY(13,icKunde,"|")
         FaktLand:SCREEN-VALUE     = ENTRY(14,icKunde,"|")
         LevAdresse1:SCREEN-VALUE  = ENTRY(15,icKunde,"|")
         LevAdresse2:SCREEN-VALUE  = ENTRY(16,icKunde,"|")
         LevPostNr:SCREEN-VALUE    = ENTRY(17,icKunde,"|")
         LevLand:SCREEN-VALUE      = ENTRY(18,icKunde,"|")
         MobilTlf:SCREEN-VALUE     = ENTRY(19,icKunde,"|")
         .

  APPLY "leave" TO PostNr.
  APPLY "leave" TO FaktPostNr.
  APPLY "leave" TO LevPostNr.

END.
RETURN TRUE.   /* Function return value. */

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
IF ihQuery:TYPE = "browse" THEN
  hParentBuffer = ihQuery:QUERY:GET-BUFFER-HANDLE(1).
ELSE
  hParentBuffer = ihQuery:GET-BUFFER-HANDLE(1).

RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

