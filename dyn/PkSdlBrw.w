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
DEF VAR hSearchField      AS HANDLE NO-UNDO.

DEF VAR hParentBuffer     AS HANDLE NO-UNDO.
DEF VAR hParentBrowse     AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hBuffer           AS HANDLE NO-UNDO.
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hToolbarPris      AS HANDLE NO-UNDO.
DEF VAR hViewer           AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hGyldigKodeCol    AS HANDLE NO-UNDO.
DEF VAR hArtBilde         AS HANDLE NO-UNDO.
DEF VAR hArtBildeFrame    AS HANDLE NO-UNDO.

DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame     AS HANDLE NO-UNDO.
DEF VAR iCurrTab          AS INT    NO-UNDO.
DEF VAR iCurrBildNr       AS INT    NO-UNDO.


DEF VAR hLevAnt           AS HANDLE NO-UNDO.
DEF VAR hAntall           AS HANDLE NO-UNDO.
DEF VAR hLinjenr          AS HANDLE NO-UNDO.
DEF VAR hStrekkode        AS HANDLE NO-UNDO.
DEF VAR fCurrPkSdlId      AS DEC    NO-UNDO.
DEF VAR hPrisTekst        AS HANDLE NO-UNDO.
DEF VAR hOrdreSok         AS HANDLE NO-UNDO.
DEF VAR bOverstyrKol      AS LOG    NO-UNDO.
DEF VAR bHKinst           AS LOG    NO-UNDO.
DEF VAR iWebBut           AS INT NO-UNDO.
DEF VAR dKundeNr          AS DEC FORMAT ">>>>>>>>>>>>9" NO-UNDO.

DEF VAR iFontWingdings    AS INT    NO-UNDO.
iFontWingdings = DYNAMIC-FUNCTION("setAppFont","Wingdings, size=11 Script=symbol","").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmPage1

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tbSjekkPrisavvik sokEkstId cmbStatus ~
Strekkode sokLevKod sokLevNr sokLevNamn sokBeskr btnBlankFilter btnLev ~
tbVisFilter sokPkSdlNr sokButikk sokCL sokSendtDato pklevnr pklevnamn ~
btnPkLev sokRegistrertDato BrwPkSdlHode searchField PkSdlToolbar ~
rectPksdlDet 
&Scoped-Define DISPLAYED-OBJECTS tbSjekkPrisavvik sokEkstId cmbStatus ~
Strekkode sokLevKod sokLevNr sokLevNamn sokBeskr tbVisFilter sokPkSdlNr ~
sokButikk sokCL sokSendtDato pklevnr pklevnamn sokRegistrertDato 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 btnLev btnPkLev 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddToToolbar C-Win 
FUNCTION AddToToolbar RETURNS LOGICAL
  ( INPUT ihToolbar AS HANDLE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtNr C-Win 
FUNCTION getArtNr RETURNS DECIMAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setButikkListe C-Win 
FUNCTION setButikkListe RETURNS LOGICAL
  ( INPUT ifPkSldId AS DEC )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setFilterFields C-Win 
FUNCTION setFilterFields RETURNS LOGICAL
  ( INPUT ibView AS LOG )  FORWARD.

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
DEFINE BUTTON btnBlankFilter 
     LABEL "Blank filter" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnLev 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk hele leverandør-register".

DEFINE BUTTON btnPkLev 
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk hele leverandør-register".

DEFINE VARIABLE cmbStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 47.6 BY 1 NO-UNDO.

DEFINE VARIABLE pklevnamn AS CHARACTER FORMAT "x(30)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.

DEFINE VARIABLE pklevnr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Lev.pakksdl" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1.

DEFINE VARIABLE sokBeskr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Beskr" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE sokButikk AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE sokCL AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Sentrallager" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE sokEkstId AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ekstern id" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevKod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lev.art.nr" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevNamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1 NO-UNDO.

DEFINE VARIABLE sokLevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Lev" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 TOOLTIP "Artikkel-leverandør".

DEFINE VARIABLE sokPkSdlNr AS CHARACTER FORMAT "X(15)" 
     LABEL "Pakkseddelnr" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE sokRegistrertDato AS DATE FORMAT "99/99/9999" 
     LABEL "Registrert dato" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE sokSendtDato AS DATE FORMAT "99/99/9999" 
     LABEL "Sendt dato" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1.

DEFINE VARIABLE Strekkode AS CHARACTER FORMAT "X(256)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE BrwPkSdlHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 128 BY 17.86.

DEFINE RECTANGLE PkSdlToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 15.4 BY .95.

DEFINE RECTANGLE rectPksdlDet
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 62 BY 17.86.

DEFINE RECTANGLE searchField
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.

DEFINE VARIABLE tbSjekkPrisavvik AS LOGICAL INITIAL NO 
     LABEL "Sjekk for prisavvik" 
     VIEW-AS TOGGLE-BOX
     SIZE 22.2 BY .81 NO-UNDO.

DEFINE VARIABLE tbVisFilter AS LOGICAL INITIAL NO 
     LABEL "Vis filter" 
     VIEW-AS TOGGLE-BOX
     SIZE 15 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmPage1
     tbSjekkPrisavvik AT ROW 5.71 COL 95.8
     sokEkstId AT ROW 2.48 COL 82 COLON-ALIGNED
     cmbStatus AT ROW 2.48 COL 13.4 COLON-ALIGNED
     Strekkode AT ROW 2.48 COL 120.8 COLON-ALIGNED
     sokLevKod AT ROW 4.57 COL 13.4 COLON-ALIGNED
     sokLevNr AT ROW 3.52 COL 13.4 COLON-ALIGNED HELP
          "Leverandør - trykk F3 for å søke blant alle leverandører"
     sokLevNamn AT ROW 3.52 COL 29 COLON-ALIGNED NO-LABEL
     sokBeskr AT ROW 4.57 COL 35.8 COLON-ALIGNED
     btnBlankFilter AT ROW 4.57 COL 128
     btnLev AT ROW 3.52 COL 62.8 NO-TAB-STOP 
     tbVisFilter AT ROW 5.81 COL 128
     sokPkSdlNr AT ROW 3.52 COL 82 COLON-ALIGNED HELP
          "Pakkseddelnummer"
     sokButikk AT ROW 4.52 COL 82 COLON-ALIGNED
     sokCL AT ROW 5.62 COL 82 COLON-ALIGNED HELP
          "Sentrallager pakkseddelen er sendt til."
     sokSendtDato AT ROW 3.52 COL 120.8 COLON-ALIGNED HELP
          "Dato da varene er sendt fra leverandør."
     pklevnr AT ROW 5.62 COL 13.4 COLON-ALIGNED
     pklevnamn AT ROW 5.62 COL 35.8 COLON-ALIGNED
     btnPkLev AT ROW 5.62 COL 62.8 NO-TAB-STOP 
     sokRegistrertDato AT ROW 3.52 COL 156.4 COLON-ALIGNED HELP
          "Dato da varene er sendt fra leverandør."
     BrwPkSdlHode AT ROW 7.91 COL 1
     searchField AT ROW 6.76 COL 1.2
     PkSdlToolbar AT ROW 1.14 COL 1.6
     rectPksdlDet AT ROW 7.91 COL 130
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 191.6 BY 24.95.


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
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "<insert window title>"
         HEIGHT             = 25
         WIDTH              = 191.8
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
/* SETTINGS FOR FRAME frmPage1
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME frmPage1:HEIGHT           = 24.95
       FRAME frmPage1:WIDTH            = 191.6.

/* SETTINGS FOR BUTTON btnLev IN FRAME frmPage1
   1                                                                    */
/* SETTINGS FOR BUTTON btnPkLev IN FRAME frmPage1
   1                                                                    */
ASSIGN 
       sokLevNamn:READ-ONLY IN FRAME frmPage1        = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frmPage1
/* Query rebuild information for FRAME frmPage1
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME frmPage1 */
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


&Scoped-define SELF-NAME btnBlankFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnBlankFilter C-Win
ON CHOOSE OF btnBlankFilter IN FRAME frmPage1 /* Blank filter */
DO:
  RUN BlankFilter (YES).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev C-Win
ON CHOOSE OF btnLev IN FRAME frmPage1 /* ... */
DO:
  DEF VAR cLevBasId AS CHAR NO-UNDO.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn",
                      "where true",
                      "",
                      "Levnr",
                      OUTPUT cLevBasId,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    sokLevNr:SCREEN-VALUE   = cLevBasId.
    APPLY "Return" TO sokLevnr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPkLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPkLev C-Win
ON CHOOSE OF btnPkLev IN FRAME frmPage1 /* ... */
DO:
  DEF VAR cLevBasId AS CHAR NO-UNDO.
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,0,
                      "LevBas;levnr;levnamn",
                      "where true",
                      "",
                      "Levnr,Levnamn",
                      OUTPUT cLevBasId,
                      OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk THEN DO:
    ASSIGN pkLevNr:SCREEN-VALUE   = ENTRY(1,cLevBasId,"|")
           pkLevNamn:SCREEN-VALUE = ENTRY(2,cLevBasId,"|").
    APPLY "Return" TO pkLevnr.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbStatus
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbStatus C-Win
ON VALUE-CHANGED OF cmbStatus IN FRAME frmPage1 /* Status */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pklevnamn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pklevnamn C-Win
ON RETURN OF pklevnamn IN FRAME frmPage1 /* Navn */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME pklevnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL pklevnr C-Win
ON RETURN OF pklevnr IN FRAME frmPage1 /* Lev.pakksdl */
DO:
  IF SELF:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokBeskr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokBeskr C-Win
ON RETURN OF sokBeskr IN FRAME frmPage1 /* Beskr */
DO:
  IF sokBeskr:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokButikk C-Win
ON LEAVE OF sokButikk IN FRAME frmPage1 /* Butikk */
DO:
    IF sokButikk:MODIFIED THEN RUN StartQuery. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokCL
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokCL C-Win
ON RETURN OF sokCL IN FRAME frmPage1 /* Sentrallager */
DO:
  IF sokCL:MODIFIED THEN RUN StartQuery.     
/*   IF sokCL:MODIFIED THEN DO:                                                                            */
/*     IF sokCL:SCREEN-VALUE NE "" THEN DO:                                                                */
/*       DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","WHERE CL = '" + sokCL:SCREEN-VALUE + "'"). */
/*       DYNAMIC-FUNCTION("setPreScanQuery","").                                                           */
/*       RUN InvokeMethod(hBrowse,"OpenQuery").                                                            */
/*                                                                                                         */
/*       IF hBrowse:QUERY:NUM-RESULTS = 1 THEN                                                             */
/*         DYNAMIC-FUNCTION("setTab" IN hParent,2).                                                        */
/*     END.                                                                                                */
/*     ELSE RUN StartQuery.                                                                                */
/*   END.                                                                                                  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokEkstId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokEkstId C-Win
ON RETURN OF sokEkstId IN FRAME frmPage1 /* Ekstern id */
DO:
  IF sokEkstId:MODIFIED THEN RUN StartQuery.      
/*   IF sokEkstId:MODIFIED THEN DO:                                                                                */
/*     IF sokEkstId:SCREEN-VALUE NE "" THEN DO:                                                                    */
/*       DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","WHERE EkstId = '" + sokEkstId:SCREEN-VALUE + "'"). */
/*       DYNAMIC-FUNCTION("setPreScanQuery","").                                                                   */
/*       RUN InvokeMethod(hBrowse,"OpenQuery").                                                                    */
/*                                                                                                                 */
/*       IF hBrowse:QUERY:NUM-RESULTS = 1 THEN                                                                     */
/*         DYNAMIC-FUNCTION("setTab" IN hParent,2).                                                                */
/*     END.                                                                                                        */
/*     ELSE RUN StartQuery.                                                                                        */
/*   END.                                                                                                          */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevKod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevKod C-Win
ON RETURN OF sokLevKod IN FRAME frmPage1 /* Lev.art.nr */
DO:
  IF sokLevKod:MODIFIED THEN RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokLevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON F3 OF sokLevNr IN FRAME frmPage1 /* Lev */
DO:
  APPLY "choose" TO btnLev.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokLevNr C-Win
ON RETURN OF sokLevNr IN FRAME frmPage1 /* Lev */
DO:
  IF sokLevNr:MODIFIED THEN DO:
    sokLevNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldList","LevBas;LevNamn","WHERE LevNr = " + sokLevNr:SCREEN-VALUE).
    RUN StartQuery.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokPkSdlNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokPkSdlNr C-Win
ON RETURN OF sokPkSdlNr IN FRAME frmPage1 /* Pakkseddelnr */
DO:
  IF sokPkSdlNr:MODIFIED THEN RUN StartQuery.      
/*   IF sokPkSdlNr:MODIFIED THEN DO:                                                                                 */
/*     IF sokPkSdlNr:SCREEN-VALUE NE "" THEN DO:                                                                     */
/*       DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","WHERE PkSdlNr = '" + sokPkSdlNr:SCREEN-VALUE + "'"). */
/*       DYNAMIC-FUNCTION("setPreScanQuery","").                                                                     */
/*       RUN InvokeMethod(hBrowse,"OpenQuery").                                                                      */
/*                                                                                                                   */
/*       IF hBrowse:QUERY:NUM-RESULTS = 1 THEN                                                                       */
/*         DYNAMIC-FUNCTION("setTab" IN hParent,2).                                                                  */
/*     END.                                                                                                          */
/*     ELSE RUN StartQuery.                                                                                          */
/*   END.                                                                                                            */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokRegistrertDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokRegistrertDato C-Win
ON RETURN OF sokRegistrertDato IN FRAME frmPage1 /* Registrert dato */
DO:
  IF sokRegistrertDato:MODIFIED THEN RUN StartQuery.
/*   IF sokSendtDato:MODIFIED THEN DO:                                                                                   */
/*     IF sokSendtDato:SCREEN-VALUE NE "" THEN DO:                                                                       */
/*       DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","WHERE SendtDato = '" + sokSendtDato:SCREEN-VALUE + "'"). */
/*       DYNAMIC-FUNCTION("setPreScanQuery","").                                                                         */
/*       RUN InvokeMethod(hBrowse,"OpenQuery").                                                                          */
/*                                                                                                                       */
/*       IF hBrowse:QUERY:NUM-RESULTS = 1 THEN                                                                           */
/*         DYNAMIC-FUNCTION("setTab" IN hParent,2).                                                                      */
/*     END.                                                                                                              */
/*     ELSE RUN StartQuery.                                                                                              */
/*   END.                                                                                                                */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sokSendtDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sokSendtDato C-Win
ON RETURN OF sokSendtDato IN FRAME frmPage1 /* Sendt dato */
DO:
  IF sokSendtDato:MODIFIED THEN RUN StartQuery.
/*   IF sokSendtDato:MODIFIED THEN DO:                                                                                   */
/*     IF sokSendtDato:SCREEN-VALUE NE "" THEN DO:                                                                       */
/*       DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","WHERE SendtDato = '" + sokSendtDato:SCREEN-VALUE + "'"). */
/*       DYNAMIC-FUNCTION("setPreScanQuery","").                                                                         */
/*       RUN InvokeMethod(hBrowse,"OpenQuery").                                                                          */
/*                                                                                                                       */
/*       IF hBrowse:QUERY:NUM-RESULTS = 1 THEN                                                                           */
/*         DYNAMIC-FUNCTION("setTab" IN hParent,2).                                                                      */
/*     END.                                                                                                              */
/*     ELSE RUN StartQuery.                                                                                              */
/*   END.                                                                                                                */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Strekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Strekkode C-Win
ON DELETE-CHARACTER OF Strekkode IN FRAME frmPage1 /* Strekkode */
DO:
  APPLY LASTKEY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Strekkode C-Win
ON RETURN OF Strekkode IN FRAME frmPage1 /* Strekkode */
DO:
  IF Strekkode:SCREEN-VALUE NE "" THEN DO:
    DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","").  
    DYNAMIC-FUNCTION("setPreScanQuery",
                     "Strekkode WHERE kode = '" + Strekkode:SCREEN-VALUE + "'"
                   + ",FIRST ArtBas NO-LOCK OF Strekkode"
                   + ",EACH PkSdlLinje WHERE PkSdlLinje.ArtikkelNr = ArtBas.ArtikkelNr NO-LOCK"
                   + ",FIRST PkSdlHode NO-LOCK OF PkSdlLinje").
  END.
  ELSE
    DYNAMIC-FUNCTION("setPreScanQuery","").

  RUN InvokeMethod(hBrowse,"OpenQuery").

  IF hBrowse:QUERY:NUM-RESULTS = 1 THEN
    DYNAMIC-FUNCTION("setTab" IN hParent,2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbSjekkPrisavvik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbSjekkPrisavvik C-Win
ON VALUE-CHANGED OF tbSjekkPrisavvik IN FRAME frmPage1 /* Sjekk for prisavvik */
DO:
  RUN StartQuery.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tbVisFilter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tbVisFilter C-Win
ON VALUE-CHANGED OF tbVisFilter IN FRAME frmPage1 /* Vis filter */
DO:
  setFilterFields(tbVisFilter:CHECKED).
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
  IF VALID-HANDLE(hStrekkode)    THEN APPLY "close" TO hStrekkode.
  IF VALID-HANDLE(hOrdreSok)     THEN APPLY "close" TO hOrdreSok.
  IF VALID-HANDLE(hViewer)       THEN APPLY "close" TO hViewer.
  RUN disable_UI.
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
{incl/supptrigg.i hBrowse}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AngreOverforRecord C-Win 
PROCEDURE AngreOverforRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR plPkSdlId  AS DEC  NO-UNDO.
    DEF VAR plFlag     AS LOG  NO-UNDO.
    DEF VAR pcReturn   AS CHAR NO-UNDO.
    DEF VAR piButNr    AS INT  NO-UNDO.
    DEF VAR piTilbut   AS INT  NO-UNDO.
    DEF VAR pcTekst    AS CHAR NO-UNDO.
    DEF VAR dKundeNr   AS DEC  NO-UNDO.

    plFlag = FALSE.
    MESSAGE 'Denne funksjonen angrer en overføring som er gjort.' SKIP
        'Ved overføring opprettes en pakkseddel i mottagende butikk.' SKIP
        'Det er bare pakksedler som er opprette som følge av en overføring' SKIP
        'som kan angres. Det gjøres kontroll på dette ved aktivering av funksjonen.' SKIP
        'Faktura som ble opprettet som følge av overføringen, blir kreditert.' SKIP(1)
        'Skal overføring angres (Hvis ja, må ny butikk det skal overføres til velges)?' SKIP(1)
        'NB: I feltet "Merknad fra lev" står det hvor varene er overført fra, hvis' SKIP
        'pakkseddelen er opprettet fra en overføring.'
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO  TITLE 'Angre overføring'   UPDATE plFlag.
    IF plflag = FALSE  THEN
        RETURN.

    IF hBuffer:AVAIL THEN
    ANGRE:
    DO:
        pcTekst = "Sasong".
        /*RUN JBoxDLookup.w ("Sasong;SasBeskr;Sasong", "where true", INPUT-OUTPUT cTekst).*/
        THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
        RUN JBoxLookup.w (THIS-PROCEDURE,50,
                          "Butiker;Butik;ButNamn;KortNavn;HarButikkSystem;Apningsdato;NedlagtDato"
                         ,"WHERE HarButikkSystem = 'TRUE' and Apningsdato <> '?' and nedlagtDato = '?'"
                          ,""                                                  
                          ,"Butik",   /* <- return values for these fields */
                            OUTPUT pcTekst,
                            OUTPUT plFlag).
        THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

        IF RETURN-VALUE = "AVBRYT" THEN
            RETURN NO-APPLY.
        FIND Butiker NO-LOCK WHERE
          Butiker.Butik = INT(pcTekst) NO-ERROR.
        IF AVAILABLE Butiker THEN
            piTilbut = Butiker.Butik.
        ELSE
            RETURN.

        RUN setFakturaKobling (OUTPUT dKundeNr).

        ASSIGN
            plPkSdlId = DEC(hBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE)
            .
        IF plPkSdlId > 0 THEN
        DO:
            FIND PkSdlHode NO-LOCK WHERE 
                PkSdlHode.PksdlId = plPkSdlId NO-ERROR.
            IF AVAILABLE PkSdlHode THEN
                FIND FIRST PkSdlLinje OF PkSdlHode NO-ERROR.
            IF NOT AVAILABLE PkSdlLinje THEN
                LEAVE ANGRE.

            FIND LAST FakturaHode NO-LOCK WHERE
                FakturaHode.KundeNr   = dKundeNr AND
                FakturaHode.FakturaNr = DEC(PkSdlHode.PkSdlNr) AND 
                FakturaHode.Bilagstype = 1 NO-ERROR.
            IF AVAILABLE FakturaHode THEN
                piButNr = FakturaHode.ButikkNr.
            ELSE 
                LEAVE ANGRE.

            IF piTilBut = PkSdlLinje.ButikkNr THEN
            DO:
                MESSAGE 'Du kan ikke overføre til samme butikk som den opprinnelige overføringen er gjort til.' SKIP
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN.
            END.

            plFlag = FALSE.
            MESSAGE 'Du har valgt å angre overføringen som ligger til grunn for denne pakkseddeelen og gjøre ny overføring ' + 
                'til butikk ' + STRING(Butiker.Butik) + ' ' + Butiker.ButNamn + '.' SKIP(1)
                'Skal angring av overføring utføres?'
                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO  TITLE 'Angre overføring'   UPDATE plFlag.
            IF plflag = FALSE  THEN
                RETURN.

            /* Kjører uten AppServer. */
            RUN asPkSdl_AngreOverforing.p 
                (plPkSdlId,
                 piButNr,
                 piTilbut,
                 OUTPUT plFlag,
                 OUTPUT pcReturn
                 ) 
                .
        END.
    END. /* ANGRE */

    IF plFlag = FALSE  THEN
        MESSAGE pcReturn SKIP
            pibutNr
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE DO:
        DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
        /*
        ASSIGN
          iTellenr = hBuffer:BUFFER-FIELD('tellenr'):BUFFER-VALUE
          rRowid = TO-ROWID(hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE)
        .
        DYNAMIC-FUNCTION("runproc","tellehode_oppdatersum.p",STRING(rRowid),?).
        RUN InvokeMethod(hBrowse,"OpenQuery").
        hBuffer:FIND-FIRST('WHERE tellenr = ' + STRING(iTellenr),NO-LOCK) NO-ERROR.
        hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID).
        */

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankFilter C-Win 
PROCEDURE BlankFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibStartQuery AS LOG NO-UNDO.
DO WITH FRAME {&FRAME-NAME}:

  ASSIGN cmbStatus:SCREEN-VALUE    = "0"
         sokLevNr:SCREEN-VALUE     = ""
         sokLevKod:SCREEN-VALUE    = ""
         sokLevKod:SCREEN-VALUE    = ""
         sokBeskr:SCREEN-VALUE     = ""
         Strekkode:SCREEN-VALUE    = ""
         sokEkstId:SCREEN-VALUE    = ""
         sokPkSdlNr:SCREEN-VALUE   = ""
         sokCL:SCREEN-VALUE        = ""
         sokSendtDato:SCREEN-VALUE = ?
         sokRegistrertDato:SCREEN-VALUE = ?
         pkLevnr:SCREEN-VALUE      = ""
         pkLevnamn:SCREEN-VALUE    = ""
         sokbutikk:SCREEN-VALUE    = ""
         tbSjekkPrisavvik:CHECKED  = NO
         .
  IF ibStartQuery THEN RUN StartQuery.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BytButNrRecord C-Win 
PROCEDURE BytButNrRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.
DEF VAR ocValue AS INT NO-UNDO.
DEF VAR cRowIdList AS CHAR NO-UNDO.

IF hBuffer:AVAILABLE THEN
NY_KODE:    
DO:    
    
    IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('PkSdlStatus'):BUFFER-VALUE <> 10 THEN  
        DO:
            MESSAGE 'Butikknr. kan bare byttes på ikke innleverte pakskedler.'
            VIEW-AS ALERT-BOX.        
            RETURN.
        END.
    iReturn = 0.
    RUN JBoxBrowseMsgUpdateVal.w ("Angi nytt butikknr. for pakkseddelen.",
                              hBrowse:NUM-SELECTED-ROWS,
                              IF INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount")) LT INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"rowstobatch")) THEN
                                INT(DYNAMIC-FUNCTION("getAttribute",hBrowse,"Totalcount"))
                              ELSE 99999
                              ,'INTEGER|>>>>>9'
                              ,OUTPUT ocValue
                              ,OUTPUT iReturn).  /*1=Alle,2=Valgte*/

    IF INT(ocValue) = 0 THEN
        MESSAGE 'Butikknr. er ikke angitt.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    IF NOT CAN-FIND(Butiker WHERE 
                    Butiker.Butik = INT(ocValue)) THEN
    DO:
        MESSAGE 'Ugyldig butikknr.'
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.

    IF iReturn = 0 OR INT(ocValue) = 0 THEN RETURN NO-APPLY.


    IF iReturn = 1 OR hBrowse:NUM-SELECTED-ROWS = 0 THEN /* Alle poster */
      DO:
        DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"pksdl_ByttButNr.p",ocValue).
        RUN InvokeMethod (hBrowse,'OpenQuery').
      END.
    ELSE DO: /* Valgte poster */
        DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
            IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
                cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
        END.
        DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"pksdl_ByttButNr.p",ocValue).
        DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(cRowIdList,",")).
    END.
END. /* NY_KODE */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setTab" IN hParent,2).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  HIDE FRAME frmPage1.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("DoLockWindow",THIS-PROCEDURE:CURRENT-WINDOW).
DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","yes").

IF hBuffer:AVAIL AND hBuffer:BUFFER-FIELD("PkSdlStatus"):BUFFER-VALUE > 10 THEN
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","Delete").
ELSE IF hBuffer:AVAIL AND hBuffer:BUFFER-FIELD("PkSdlStatus"):BUFFER-VALUE > 15 THEN
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","Innlev,Delete").
ELSE
  DYNAMIC-FUNCTION("setAttribute",hToolbar,"disabledEvents","").

PUBLISH "setPkslButikkListe" (IF hBuffer:AVAIL THEN hBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE ELSE 0).

RUN SUPER.

IF NOT hBrowse:QUERY:GET-BUFFER-HANDLE(1):AVAIL THEN
  PUBLISH "HideOverlays" (THIS-PROCEDURE:CURRENT-WINDOW).

IF Strekkode:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE "" THEN
  PUBLISH "FinnStrekkode" (Strekkode:SCREEN-VALUE).


DYNAMIC-FUNCTION("setAttribute",SESSION,"userkeepswindowlocked","").
DYNAMIC-FUNCTION("DoLockWindow",?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE editRecord C-Win 
PROCEDURE editRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN ArtikkelKort IN hViewer.

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
  DISPLAY tbSjekkPrisavvik sokEkstId cmbStatus Strekkode sokLevKod sokLevNr 
          sokLevNamn sokBeskr tbVisFilter sokPkSdlNr sokButikk sokCL 
          sokSendtDato pklevnr pklevnamn sokRegistrertDato 
      WITH FRAME frmPage1.
  ENABLE tbSjekkPrisavvik sokEkstId cmbStatus Strekkode sokLevKod sokLevNr 
         sokLevNamn sokBeskr btnBlankFilter btnLev tbVisFilter sokPkSdlNr 
         sokButikk sokCL sokSendtDato pklevnr pklevnamn btnPkLev 
         sokRegistrertDato BrwPkSdlHode searchField PkSdlToolbar rectPksdlDet 
      WITH FRAME frmPage1.
  {&OPEN-BROWSERS-IN-QUERY-frmPage1}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FinnStrekkode C-Win 
PROCEDURE FinnStrekkode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icStrekkode AS CHAR NO-UNDO.

DEF VAR cArtNrStr AS CHAR NO-UNDO.

cArtNrStr = DYNAMIC-FUNCTION("getFieldValues","Strekkode","WHERE Kode = '" + icStrekkode + "'","ArtikkelNr,StrKode").
IF cArtNrStr NE "" THEN DO:
  bOK = hBuffer:FIND-FIRST("WHERE ArtikkelNr = " + ENTRY(1,cArtNrStr,"|") + " AND StrKode = " + ENTRY(2,cArtNrStr,"|")) NO-ERROR.
  IF bOk THEN DO:
    hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID) NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
      APPLY "value-changed" TO hBrowse.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlatViewRecord C-Win 
PROCEDURE FlatViewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR hTabFolder AS HANDLE NO-UNDO.
DEF VAR hVareBrw   AS HANDLE NO-UNDO.
DEF VAR hVareQry   AS HANDLE NO-UNDO.

hTabFolder = DYNAMIC-FUNCTION("getTabFolderHandle" IN hParent).
hVareBrw = DYNAMIC-FUNCTION("getPageQuery" IN hTabFolder,2).
hVareQry = DYNAMIC-FUNCTION("getLinkedObject",hVareBrw,"parent","to").

/* DYNAMIC-FUNCTION("deleteObjectLink",hVareBrw,hVareQry).  */


DYNAMIC-FUNCTION("CreateParentLink",hVareBrw,hBrowse,"PkSdlId").

RUN SUPER.

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
DEF VAR cTabList     AS CHAR   NO-UNDO.
DEF VAR hPageObject  AS HANDLE NO-UNDO.
DEF VAR iy           AS INT    NO-UNDO.
DEF VAR hTabFrame    AS HANDLE NO-UNDO.
DEF VAR hViewerFrame AS HANDLE NO-UNDO.
DEF VAR cHKinst      AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

/*   PUBLISH "removeFollowSplitBar" (THIS-PROCEDURE:CURRENT-WINDOW,  */
/*                                   STRING(PrisViewer:HANDLE)).     */

  ASSIGN cmbStatus:DELIMITER = "|"
         cmbStatus:LIST-ITEM-PAIRS = "|0|" + DYNAMIC-FUNCTION("getFieldList",
                                                      "SysPara;Paranr|Beskrivelse;Paranr",
                                                      "WHERE SysHid = 5 AND SysGr = 25")
         cmbStatus:SCREEN-VALUE = "10"
         bOverstyrKol = DYNAMIC-FUNCTION("getOverstyrKol" IN hParent)
         cHKinst      = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                                         "WHERE SysHId = 1 and SysGr = 1 and ParaNr = 18","Parameter1")
         bHKinst      = IF CAN-DO("1,y,yes,j,ja,true",cHKinst) THEN TRUE ELSE FALSE
         iWebBut      = INT(
                            DYNAMIC-FUNCTION("getFieldValues","SysPara",
                                         "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 2","Parameter1")                           
                            )
         .

  APPLY "value-changed" TO tbVisFilter.

  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                    BrwPkSdlHode:HANDLE,   
                    100,                 
                    "MULTIPLE",                  
                    "PkSdlHode"
                      + ";SendtDato"
                      + ";+PkSdlInnlevDato|DATE|99/99/9999|pksdl_InnlevDato|Innlev.dato"
                      + ";EkstId|Ekst.ordrenr"
                      + ";PkSdlNr|Pakkseddel"
                      + ";!PkSdlStatus|St"
                      + ";+PkSdlOrdreType|CHARACTER|x(3)|pksdl_OrdreType(ROWID)|OTyp"  
                      + ";+PkSdlLevVerdi|DECIMAL|->>><>>><>>9.99|pksdl_levverdi|Sum levert"
                      + ";!+PkSdlPrisAvvik|CHARACTER|xx|pksdl_prisavvik(ROWID)|prisavvk - setter flagg for tekst Sport1"
                      + ";!+PkSdlTotRest|DECIMAL|->>>>9|pksdl_totrest|Rest" 
                      + ";+PkSdlTotBest|DECIMAL|->>>>9|pksdl_totbest|Bestilt" 
                      + ";+PkSdlPrisTekst|CHARACTER|x(26)|pksdl_avvikstekst|Prisavvik"
                      + ";CL|CL"
                      + ";+PkSdlButLst|CHARACTER|x(30)|pksdl_butlst(ROWID)|Butikker"  
                      + ";FakturaNr|FakturaNr"  
                      + ";+PkSdlFakturabelop|DECIMAL|->>><>>><>>9.99|pksdl_fakturaBelop(ROWID)|Fakturabeløp"  
                      + ";PkSdlId"
                      + ";+!PkSdlTotLevAvvik|DECIMAL|->>>>>9|pksdl_totlev_avvik|Tot.lev.avvik"  
                      + ";LevNr"
                      + ";LevNamn"
                      + ";!MeldingFraLev"
                      + ";Merknad"
                      + ";+PkSdlLandedCost|CHARACTER|x(20)|pksdl_LandedCost(ROWID)|LandedCost"  
                      + ";+PkSdlSesong|CHARACTER|x(20)|pksdl_Sesong(ROWID)|Sesong"  
                      + ";!RegistrertAv"
                      + ";PkSdlOpphav"
                      + ";+Rab1|DECIMAL|-><>>9.9|pksdl_Rab1|Rab%" 
                  + ",SysPara"
                      + ";Parameter1|Status|x(10)" + (IF FALSE /* bOverstyrKol */ THEN "@6" ELSE "@5")
                    ,"WHERE false"
                  + ",FIRST SysPara NO-LOCK WHERE SysHId = 5 and SysGr = 25 AND ParaNr = PkSdlStatus" 
                    ,"").
/*                      ,"sort|SendtDato;DESC"). */

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","").

  hBuffer = hBrowse:QUERY:GET-BUFFER-HANDLE(1).

  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).

  DYNAMIC-FUNCTION("setSortString",hBrowse,"SendtDato;desc").  



/*  DYNAMIC-FUNCTION("setAttribute",hBrowse,"getrecordcount","yes"). */
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"CustomDeleteValProc","=pksdlhode_delete.p").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"filterExcludeFields","Parameter1").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"noColumnSearch","Parameter1").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"searchdefault","filter").
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","pksdl_brwcalc.p").

  DYNAMIC-FUNCTION("NewMenuBand",hBrowse
                  ,"MultiSortBrowse;Sorter på flere kolonner"
                  ,"").

  hSearchField = DYNAMIC-FUNCTION("NewBrowseSearchField",SearchField:HANDLE,hBrowse,1).
  DYNAMIC-FUNCTION("CreateObjectLink",hSearchField,hBrowse).

  hViewer = DYNAMIC-FUNCTION("NewViewer"
            ,rectPksdlDet:HANDLE
            ,hBrowse
            ,"PkSdlSmallView.w").

  DYNAMIC-FUNCTION("CreateOneToOneLink",
                   DYNAMIC-FUNCTION("GetLinkedObject",hViewer,"query","from")
                   ,hBrowse,"PkSdlId").

  SUBSCRIBE TO "InvalidateHandle" IN hViewer.

  hViewerFrame = DYNAMIC-FUNCTION("getFrameHandle" IN hViewer).

  DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, hViewerFrame,hViewerFrame:NAME).
/*           DYNAMIC-FUNCTION("getSiblingNames",hViewerFrame:FIRST-CHILD:FIRST-CHILD,"","","") + "," + hViewerFrame:NAME). */
  DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, hViewerFrame,"Merknad,MeldingFraLev," + hViewerFrame:NAME).
  DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, hViewerFrame,"Merknad,MeldingFraLev," + hViewerFrame:NAME).

  RUN MoveToTop IN hViewer.
END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initOrdreStatusSearch C-Win 
PROCEDURE initOrdreStatusSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF OUTPUT PARAM ocFraStatus  AS CHAR NO-UNDO INIT "4".
DEF OUTPUT PARAM ocTilStatus  AS CHAR NO-UNDO INIT "5".
DEF OUTPUT PARAM obStartQuery AS LOG  NO-UNDO INIT YES.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle C-Win 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF SOURCE-PROCEDURE = HVIEWER THEN 
  APPLY "close" TO THIS-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KopierRecord C-Win 
PROCEDURE KopierRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR plPkSdlId  AS DEC  NO-UNDO.
    DEF VAR plFlag     AS LOG  NO-UNDO.
    DEF VAR iReturn AS INT NO-UNDO.
    DEF VAR pocValue AS CHARACTER NO-UNDO.
    DEF VAR pcRowIdList AS CHAR NO-UNDO.
    DEFINE VARIABLE pcPkSdlNr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcRowId AS CHARACTER NO-UNDO.

    plFlag = FALSE.
    MESSAGE 'Denne funksjonen kopierer pakkseddelen til en ny pakkseddel med samme ordre og pakkseddelnr.' SKIP
        'Kopien som opprettes vi ha status 10_Ny.' SKIP
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO  TITLE 'Kopier pakkseddel'   UPDATE plFlag.
    IF plflag = FALSE  THEN
        RETURN.

    IF hBrowse:NUM-SELECTED-ROWS <> 1 THEN /* Alle poster */
      DO:
          MESSAGE 'Kopier funksjonen kan bare benyttes på enkeltrader. Marker raden som skal kopieres.'
          VIEW-AS ALERT-BOX.
          RETURN.
      END.
    ELSE DO: /* Valgte poster */
        pcRowIdList = ''.
        DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
            IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
                pcRowIdList = pcRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
        END.
        pocValue = ''.
/*        DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"pksdl_kopier.p",pocValue).*/
/*        pocValue =  DYNAMIC-FUNCTION("getServerTransReturnParam").                */
        
        RUN pksdl_kopier.p('',hBuffer,'',OUTPUT pocValue,OUTPUT bOk).        
        
        IF NUM-ENTRIES(pocValue,'|') > 1 THEN 
            ASSIGN 
                pcRowId   = ENTRY(1,pocValue,'|')
                pcPksdlNr = ENTRY(2,pocValue,'|')
                . 
        ELSE 
            ASSIGN 
                pcRowId   = ''
                pcPksdlNr = ''
                . 
        IF pocValue <> '' THEN 
        DO:
            pcRowIdList = pcRowIdList + pcRowId + ','.
            DYNAMIC-FUNCTION("RefreshRowids",hBrowse,TRIM(pcRowIdList,",")).
        END.
        IF pcPkSdlNr <> '' THEN 
            sokPkSdlNr:SCREEN-VALUE IN FRAME frmPage1 = pcPkSdlNr.      
        RUN StartQuery.
        
/*        RUN InvokeMethod (hBrowse,'OpenQuery').*/
/*
hBrowse:QUERY:REPOSITION-TO-ROWID(hBuffer:ROWID) NO-ERROR.
*/
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MotposterRecord C-Win 
PROCEDURE MotposterRecord :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR plPkSdlId  AS DEC  NO-UNDO.
    DEF VAR plFlag     AS LOG  NO-UNDO.
    DEF VAR iReturn AS INT NO-UNDO.
    DEF VAR pocValue AS CHARACTER NO-UNDO.
    DEF VAR pcRowIdList AS CHAR NO-UNDO.
    DEFINE VARIABLE pcPkSdlNr AS CHARACTER NO-UNDO.
    DEFINE VARIABLE pcRowId AS CHARACTER NO-UNDO.

    IF hBuffer:AVAILABLE THEN
    BEHANDLE: 
    DO:
        IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('PkSdlStatus'):BUFFER-VALUE <> 20 THEN  
            DO:
                MESSAGE 'Pakkseddelen er ikke innlevert, og kan ikke motposteres.'
                VIEW-AS ALERT-BOX.        
                RETURN.
            END.
        IF STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('Merknad'):BUFFER-VALUE) BEGINS 'MOTPOSTERT' THEN  
            DO:
                MESSAGE 'Pakkseddelen er motpostert tidlgiere og kan ikke motposteres flere ganger.'
                VIEW-AS ALERT-BOX.        
                RETURN.
            END.

        plFlag = FALSE.
        MESSAGE 'Denne funksjonen motposterer innlevert av pakkseddel.' SKIP
            "Skal pakkseddelen motposteres?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO  TITLE 'Motposter pakkseddel'   UPDATE plFlag.
        IF plflag = FALSE  THEN
            RETURN.
    
        IF hBrowse:NUM-SELECTED-ROWS <> 1 THEN /* Alle poster */
          DO:
              MESSAGE 'Funksjonen kan bare benyttes på enkeltrader. Marker raden som skal motposteres.'
              VIEW-AS ALERT-BOX.
              RETURN.
          END.
        ELSE DO: /* Valgte poster */
            pcRowIdList = ''.
            DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:
                IF hBrowse:FETCH-SELECTED-ROW(ix) THEN
                    pcRowIdList = pcRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('RowIdent1'):BUFFER-VALUE + ','.
            END.
            pocValue = ''.            
            RUN PkSdl_motposter.p(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('PkSdlId'):BUFFER-VALUE,OUTPUT bOk,OUTPUT pocValue).        
            DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
        END.
    END. /* BEHANDLE */
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

hBrowse:MOVE-TO-TOP().
FRAME {&FRAME-NAME}:MOVE-TO-TOP().

RUN MoveToTop IN hViewer.

APPLY "entry" TO sokEkstId.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextRow C-Win 
PROCEDURE nextRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
  hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).
IF hBrowse:SELECT-NEXT-ROW() THEN
  APPLY "value-changed" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext C-Win 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER icDir AS CHARACTER  NO-UNDO.

IF CAN-DO("Prev,Next",icDir) THEN DO:
  IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
    hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).

  IF icDir = "Prev" THEN
    hBrowse:SELECT-PREV-ROW().
  ELSE
    hBrowse:SELECT-NEXT-ROW().
  APPLY "value-changed" TO hBrowse.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prevRow C-Win 
PROCEDURE prevRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hBrowse:IS-ROW-SELECTED(hBrowse:FOCUSED-ROW) THEN
  hBrowse:SELECT-ROW(hBrowse:FOCUSED-ROW).
IF hBrowse:SELECT-PREV-ROW() THEN
  APPLY "value-changed" TO hBrowse.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resendOrdreRecord C-Win 
PROCEDURE resendOrdreRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.

RUN JboxBrowseSelectMsg.w ("Send pakksedler på nytt",
                           hBrowse:NUM-SELECTED-ROWS,
                           INTEGER(DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordCount")),
                           OUTPUT iReturn).
IF iReturn = 1 THEN DO:
  IF DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"pksdl_resend.p","") THEN
    RUN InvokeMethod(hBrowse,"OpenQuery").
END.
ELSE IF iReturn = 2 THEN
  DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"pksdl_resend.p","").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE resendWebRecord C-Win 
PROCEDURE resendWebRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iReturn AS INT NO-UNDO.

RUN JboxBrowseSelectMsg.w ("Send pakkseddel informasjon til nettbutikk.",
                           hBrowse:NUM-SELECTED-ROWS,
                           INTEGER(DYNAMIC-FUNCTION("getAttribute",hBrowse,"recordCount")),
                           OUTPUT iReturn).
IF iReturn = 1 THEN
  DYNAMIC-FUNCTION("ProcessQuery",hBrowse,"pksdl_resend_til_web.p","").
ELSE IF iReturn = 2 THEN
  DYNAMIC-FUNCTION("ProcessSelectedRows",hBrowse,"pksdl_resend_til_web.p","").

MESSAGE 'Artikler og lager er sendt til nettbutikk.'
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFakturaKobling C-Win 
PROCEDURE setFakturaKobling :
/*------------------------------------------------------------------------------
  Purpose:     Henter kundenr. som denne pakkseddelen er fakturert fra.
               Pakkseddelen er bare koblet mot kunde, hvis den er en konsekvens 
               av en overføring. 
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF OUTPUT PARAMETER odKundeNr AS DEC NO-UNDO.

    IF hBuffer:AVAIL THEN
    DO:
        FIND FIRST PkSdlLinje NO-LOCK WHERE
            PkSdlLinje.PkSdlId = DEC(hBuffer:BUFFER-FIELD("PkSdlId"):BUFFER-VALUE) NO-ERROR.
        IF AVAILABLE pkSdlLinje THEN
        DO:
            FIND Butiker NO-LOCK WHERE
                butiker.butik = pkSdlLinje.ButikkNr NO-ERROR.
            IF AVAILABLE Butiker THEN
                odKundeNr = butiker.KundeNr.
            IF NOT CAN-FIND(Kunde WHERE 
                            Kunde.KundeNr = odKundeNr) THEN
                odKundeNr = 0.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setNyPkSdlQuery C-Win 
PROCEDURE setNyPkSdlQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icCrit         AS CHAR NO-UNDO.
DEF INPUT PARAM ihSourceBrowse AS HANDLE NO-UNDO.

/*
DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","").
RUN BlankFilter(NO).
RUN setQuery IN hParent (icCrit,ihSourceBrowse).
*/
DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter","").
DYNAMIC-FUNCTION("setSortString",hBrowse,"SendtDato;DESC").

RUN BlankFilter(NO).
cmbStatus:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "10".
RUN StartQuery.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE skrivFakturaRecord C-Win 
PROCEDURE skrivFakturaRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR pcPkSdlNr AS CHAR NO-UNDO.
    DEF VAR pcPrinter AS CHAR NO-UNDO.
    DEF VAR piAntEks  AS INT  NO-UNDO.
    DEF VAR piformat AS INT NO-UNDO.
    DEF VAR plFlag AS LOG NO-UNDO.
    DEF VAR pcRowIdlist AS CHAR NO-UNDO.


    IF hBrowse:NUM-SELECTED-ROWS <> 1 THEN /* Alle poster */
      DO:
          MESSAGE 'Fakturautskrift funksjonen kan bare benyttes på enkeltrader. Marker raden som skal skrives ut.'
          VIEW-AS ALERT-BOX.
          RETURN.
      END.
    ELSE DO: /* Valgte poster */
        IF hBuffer:AVAIL THEN
        DO:
            RUN setFakturaKobling (OUTPUT dKundeNr).
            FIND LAST FakturaHode NO-LOCK WHERE 
                FakturaHode.FakturaNr = hBuffer:BUFFER-FIELD("FakturaNr"):BUFFER-VALUE NO-ERROR.
            IF AVAILABLE FakturaHode THEN
            DO:
                RUN DSelectPrinter.w (INPUT-OUTPUT pcPrinter,INPUT-OUTPUT piAntEks,INPUT-OUTPUT piFormat,"FakturaSkriver,FakturaKopi",OUTPUT bOk).
                IF bOk THEN
                    RUN skrivfaktura.p (STRING(FakturaHode.Faktura_Id) + "|",
                                        TRUE,
                                        pcPrinter,
                                        piAntEks,
                                        '',
                                        piFormat). 
                plFlag = TRUE.
            END.
        END.
    END.

    IF plFlag = FALSE  THEN
        MESSAGE 'Pakkseddelen er ikke koblet til faktura.' SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE sokOrdreRecord C-Win 
PROCEDURE sokOrdreRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hOrdreSok) THEN DO:    
  RUN OrdreSok.w PERSIST SET hOrdreSok.
  RUN InitializeObject IN hOrdreSok.
END.

RUN MoveToTop IN hOrdreSok.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartQuery C-Win 
PROCEDURE StartQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamPkSdlPrisAvvik",STRING(tbSjekkPrisavvik:CHECKED)).

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryFilter","WHERE true"
                 + (IF cmbStatus:SCREEN-VALUE NE "0" AND cmbStatus:SCREEN-VALUE NE ? THEN
                      " AND PkSdlHode.PkSdlStatus = " + cmbStatus:SCREEN-VALUE
                    ELSE "")
                 + (IF sokEkstId:SCREEN-VALUE NE "" THEN
                      " AND EkstId BEGINS '" + sokEkstId:SCREEN-VALUE + "'"
                    ELSE "")
                 + (IF sokPkSdlNr:SCREEN-VALUE NE "" THEN
                      " AND PkSdlNr BEGINS '" + sokPkSdlNr:SCREEN-VALUE + "'"
                    ELSE "")
                 + (IF INT(sokCL:SCREEN-VALUE) NE 0 THEN
                      " AND CL = '" + sokCL:SCREEN-VALUE + "'"
                    ELSE "")
                 + (IF DATE(sokSendtDato:SCREEN-VALUE) NE ? THEN
                      " AND SendtDato = DATE('" + sokSendtDato:SCREEN-VALUE + "')"
                    ELSE "")
                 + (IF DATE(sokRegistrertDato:SCREEN-VALUE) NE ? THEN
                      " AND RegistrertDato = DATE('" + sokRegistrertDato:SCREEN-VALUE + "')"
                    ELSE "")
                 + (IF INT(pkLevnr:SCREEN-VALUE) NE 0 THEN
                      " AND levnr = " + pkLevnr:SCREEN-VALUE
                    ELSE "")
                 + (IF pkLevnamn:SCREEN-VALUE NE "" THEN
                      " AND levnamn " +
                      (IF INDEX(pkLevnamn:SCREEN-VALUE,"*") > 0 THEN
                        "MATCHES '"
                       ELSE "BEGINS '") +
                      pkLevnamn:SCREEN-VALUE + "'"
                    ELSE "")
                   ).


  DYNAMIC-FUNCTION("setPreScanQuery",TRIM(
                  (IF sokLevKod:SCREEN-VALUE NE "" OR sokLevNr:SCREEN-VALUE NE "0" THEN 
                     (IF sokLevKod:SCREEN-VALUE NE "" THEN
                       "PkSdlLinje WHERE PkSdlLinje.LevKod = '" + sokLevKod:SCREEN-VALUE + "'"
                      ELSE "")
                   + (IF sokLevNr:SCREEN-VALUE NE "0" THEN
                       (IF sokLevKod:SCREEN-VALUE NE "" THEN " AND " ELSE "")
                     + "PkSdlLinje WHERE PkSdlLinje.LevNr = " + sokLevNr:SCREEN-VALUE
                      ELSE "")
                   + ",FIRST PkSdlHode NO-LOCK OF PkSdlLinje"
                   ELSE "") + "|"
                + (IF sokBeskr:SCREEN-VALUE NE "" THEN 
                     "PkSdlLinje WHERE PkSdlLinje.beskr " 
                   + (IF INDEX(sokBeskr:SCREEN-VALUE,"*") > 0 THEN "MATCHES '" ELSE "BEGINS '")
                   + sokBeskr:SCREEN-VALUE + "'"
                   + ",FIRST PkSdlHode NO-LOCK OF PkSdlLinje"
                   ELSE "") + "|"
                + (IF INT(sokButikk:SCREEN-VALUE) NE 0 THEN
                    "PkSdlLinje WHERE PkSdlLinje.ButikkNr = " + sokButikk:SCREEN-VALUE
                  + ",FIRST PkSdlHode NO-LOCK OF PkSdlLinje"
                   ELSE "")
                  ,"|")).
  /*
  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcParamPkSdlButLst",
                   IF sokButikk:SCREEN-VALUE NE "" AND sokButikk:SCREEN-VALUE NE "0" THEN 
                     sokButikk:SCREEN-VALUE
                   ELSE "").
  */
END.

RUN InvokeMethod(hBrowse,"OpenQuery").

IF hBrowse:QUERY:NUM-RESULTS = 1 THEN
  DYNAMIC-FUNCTION("setTab" IN hParent,2).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSortSearch C-Win 
PROCEDURE StartSortSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN SUPER.

IF hBrowse:QUERY:NUM-RESULTS = 1 THEN
  DYNAMIC-FUNCTION("setTab" IN hParent,2).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StrekkodeRecord C-Win 
PROCEDURE StrekkodeRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hStrekkode) THEN
  RUN Strekkode.w PERSIST SET hStrekkode (THIS-PROCEDURE).

RUN MoveToTop IN hStrekkode.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE xxRowDisplayBrowse C-Win 
PROCEDURE xxRowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBuffer:BUFFER-FIELD("PkSdlPrisTekst"):BUFFER-VALUE BEGINS "Pris" THEN
  hPrisTekst:BGCOLOR = 16.
ELSE IF hBuffer:BUFFER-FIELD("PkSdlPrisTekst"):BUFFER-VALUE NE "" THEN
  hPrisTekst:BGCOLOR = 18.
ELSE
  hPrisTekst:BGCOLOR = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddToToolbar C-Win 
FUNCTION AddToToolbar RETURNS LOGICAL
  ( INPUT ihToolbar AS HANDLE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hToolbar = ihToolbar.

IF bOverstyrKol THEN DO:
  DYNAMIC-FUNCTION("AppendToolbar",ihToolbar,
                    PkSdlToolbar:HANDLE IN FRAME {&FRAME-NAME},
                    "",
                    "Delete;Slett"
  /*                 + ",BrowseConfig;Kolonneoppsett" */
                  + ",Filter,FlatView,Excel"
                  + ",Refresh,sokOrdre;Søk ordre for nytt varemottak¤enable"
                  + ",BytButNr;Bytt butikknr. på pakkseddel"
                  + (IF iWebBut > 0 THEN ",resendWeb;Send pakksedl til nettbutikk" ELSE "")
                  + (IF bHKinst THEN ",resendOrdre;Send pakksedler på nytt" ELSE "")
                  + ",skrivFaktura;Skriv faktura fra avsender"
                  + ",AngreOverfor;Angre overføring"
                  + ",Kopier;Kopier"
                  + ",Motposter;Motposter"
                   ,"maxborder").
END.
ELSE DO:
    DYNAMIC-FUNCTION("AppendToolbar",ihToolbar,
                      PkSdlToolbar:HANDLE IN FRAME {&FRAME-NAME},
                      "",
                      "Delete;Slett"
    /*                 + ",BrowseConfig;Kolonneoppsett" */
                    + ",Refresh,sokOrdre;Søk ordre for nytt varemottak¤enable"
                    + (IF iWebBut > 0 THEN ",resendWeb;Send pakksedl til nettbutikk" ELSE "")
                    + (IF bHKinst THEN ",resendOrdre;Send pakksedler på nytt" ELSE "")
                     ,"maxborder").
END.
DYNAMIC-FUNCTION("setAttribute",hToolbar,"enabledevents","sokOrdre").    

/*ELSE PkSdlToolbar:HIDDEN IN FRAME {&FRAME-NAME} = YES.*/

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
hPrisTekst = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"PkSdlPrisTekst").

IF bOverstyrKol THEN
    ASSIGN
    ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 60
    ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 60
    ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 35
    ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = 27
    ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS = 80
    ihBrowse:GET-BROWSE-COLUMN(8):WIDTH-PIXELS = 60
    ihBrowse:GET-BROWSE-COLUMN(9):WIDTH-PIXELS = 30
    ihBrowse:GET-BROWSE-COLUMN(10):WIDTH-PIXELS = 60
    . 
ELSE 
  ASSIGN 
     ihBrowse:GET-BROWSE-COLUMN(3):WIDTH-PIXELS = 60
     ihBrowse:GET-BROWSE-COLUMN(4):WIDTH-PIXELS = 60
     ihBrowse:GET-BROWSE-COLUMN(5):WIDTH-PIXELS = 35
     ihBrowse:GET-BROWSE-COLUMN(6):WIDTH-PIXELS = 27
     ihBrowse:GET-BROWSE-COLUMN(7):WIDTH-PIXELS = 80
     ihBrowse:GET-BROWSE-COLUMN(8):WIDTH-PIXELS = 30
     ihBrowse:GET-BROWSE-COLUMN(9):WIDTH-PIXELS = 60
     . 

RETURN YES.

END FUNCTION.

/*
                      + ";SendtDato"
                      + ";+PkSdlInnlevDato|DATE|99/99/9999|pksdl_InnlevDato|Innlev.dato"
                      + ";EkstId|Ekst.ordrenr"
                      + ";PkSdlNr|Pakkseddel"
                      + ";!PkSdlStatus|St"
                      + ";+PkSdlOrdreType|CHARACTER|x(3)|pksdl_OrdreType(ROWID)|OTyp"  
                      + ";+PkSdlLevVerdi|DECIMAL|->>><>>><>>9.99|pksdl_levverdi|Sum levert"
                      + ";!+PkSdlPrisAvvik|CHARACTER|xx|pksdl_prisavvik(ROWID)|prisavvk - setter flagg for tekst Sport1"
                      + ";!+PkSdlTotRest|DECIMAL|->>>>9|pksdl_totrest|Rest" 
                      + ";+PkSdlTotBest|DECIMAL|->>>>9|pksdl_totbest|Bestilt" 
                      + ";+PkSdlPrisTekst|CHARACTER|x(26)|pksdl_avvikstekst|Prisavvik"
                    + (IF FALSE /* bOverstyrKol */ THEN
                       ";+PkSdlTotLevAvvik|DECIMAL|->>>>>9|pksdl_totlev_avvik|Tot.lev.avvik"
                      + ";CL|Cl"
                      + ";+PkSdlButLst|CHARACTER|x(30)|pksdl_butlst|Butikker"  
                      + ";+PkSdlFakturaNr|CHARACTER|x(12)|pksdl_FakturaNr(ROWID)|FakturaNr"  
                      + ";+PkSdlFakturabelop|CHARACTER|x(12)|pksdl_fakturaBelop(ROWID)|Fakturabeløp"  
                      + ";PkSdlHode.LevNr"
                      + ";PkSdlHode.LevNamn"
                      + ";MeldingFraLev"
  /*                     + ";PkSdlOpphav"  */
                      + ";Merknad"
                      + ";RegistrertDato|Registrert"
                      + ";RegistrertAv"
                      + ";!PkSdlId"
                      + ";PkSdlOpphav"
                      + ";!LevFNr"
                      ELSE 
                         ";CL|CL"
                       + ";+PkSdlButLst|CHARACTER|x(30)|pksdl_butlst(ROWID)|Butikker"  
                       + ";+PkSdlFakturaNr|CHARACTER|x(12)|pksdl_FakturaNr(ROWID)|FakturaNr"  
                       + ";+PkSdlFakturabelop|CHARACTER|x(12)|pksdl_fakturaBelop(ROWID)|Fakturabeløp"  
                       + ";+!PkSdlTotLevAvvik|DECIMAL|->>>>>9|pksdl_totlev_avvik|Tot.lev.avvik"  
                       + ";LevNr"
                       + ";LevNamn"
                       + ";!MeldingFraLev"
   /*                     + ";PkSdlOpphav"  */
                       + ";Merknad"
                             
  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtNr C-Win 
FUNCTION getArtNr RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

RETURN hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStorl C-Win 
FUNCTION getStorl RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
RETURN REPLACE(
       REPLACE(
       DYNAMIC-FUNCTION("getFieldList","PkSdlLinje;,StrKonv;Storl",
                        "WHERE ArtikkelNr = " + STRING(hBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
                      + ",FIRST StrKonv NO-LOCK WHERE StrKonv.StrKode = PkSdlLinje.StrKode")
      ,"|",",")
      ," ","").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION InitializeResize C-Win 
FUNCTION InitializeResize RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"searchField,PkSdlToolbar,rectPksdlDet").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"rectPksdlDet").
DYNAMIC-FUNCTION("setAddMoveX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"rectPksdlDet").


RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setButikkListe C-Win 
FUNCTION setButikkListe RETURNS LOGICAL
  ( INPUT ifPkSldId AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
IF ifPkSldId NE fCurrPkSdlId THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cmbStatus:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" +
            DYNAMIC-FUNCTION("getFieldList","Butiker;Butik|Butnamn;Butik,PkSdlLinje;",
                             "WHERE true"
                           + ",FIRST PkSdlLinje WHERE PkSdlLinje.ButikkNr = Butiker.Butik AND PkSdlLinje.PkSdlId = " + STRING(ifPkSldId))
                             ,"|")
         cmbStatus:SCREEN-VALUE = cmbStatus:ENTRY(1).
  RUN SetQueryFilter(NO).
END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setFilterFields C-Win 
FUNCTION setFilterFields RETURNS LOGICAL
  ( INPUT ibView AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN cmbStatus:HIDDEN      = NOT ibView
         Strekkode:HIDDEN      = NOT ibView
         sokLevKod:HIDDEN      = NOT ibView
         sokLevNr:HIDDEN       = NOT ibView
         sokLevNamn:HIDDEN     = NOT ibView
         sokBeskr:HIDDEN       = NOT ibView
         btnBlankFilter:HIDDEN = NOT ibView
         btnLev:HIDDEN         = NOT ibView
         sokCL:HIDDEN          = NOT ibView
         pkLevnr:HIDDEN        = NOT ibView
         pkLevnamn:HIDDEN      = NOT ibView
         btnPkLev:HIDDEN       = NOT ibView
         sokRegistrertDato:HIDDEN = NOT ibView
         tbSjekkPrisavvik:HIDDEN = NOT ibView
         .
END.

RETURN YES.

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
ASSIGN hParentBrowse = ihQuery
       hParentBuffer = ihQuery:QUERY:GET-BUFFER-HANDLE(1).
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

