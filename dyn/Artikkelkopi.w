&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
DEF INPUT PARAM ihParent AS HANDLE NO-UNDO.

/* Local Variable Definitions ---                                       */

DEF VAR ix               AS INT NO-UNDO.
DEF VAR bOk              AS LOG NO-UNDO.

DEF VAR hWinTB           AS HANDLE NO-UNDO.

DEF VAR fArtikkelNr      AS DEC NO-UNDO.

DEF TEMP-TABLE ttVerdier
    FIELD cNavn   AS CHAR
    FIELD cVerdi  AS CHAR
    .
DEF VAR httVerdier  AS HANDLE NO-UNDO.
httVerdier = BUFFER ttVerdier:TABLE-HANDLE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectWinTB LevNr btnLev LevKod LevFargKod ~
Beskr Vg btnVarGr StrTypeID btnStr matkod sasong Pris VareKost btnSave ~
levnamn vgbeskr Beskrivelse 
&Scoped-Define DISPLAYED-OBJECTS LevNr LevKod LevFargKod Beskr Vg StrTypeID ~
matkod sasong Pris VareKost levnamn vgbeskr Beskrivelse 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HentVerdier C-Win 
FUNCTION HentVerdier RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnLev 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSave 
     LABEL "&Lagre og avslutt" 
     SIZE 27 BY 1.14.

DEFINE BUTTON btnStr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnVarGr 
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE matkod AS CHARACTER FORMAT "X(256)":U 
     LABEL "Materiale" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE sasong AS CHARACTER FORMAT "X(256)" 
     LABEL "Sesong" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 22 BY 1 NO-UNDO.

DEFINE VARIABLE Beskr AS CHARACTER FORMAT "x(20)" 
     LABEL "Beskrivelse" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 17 BY .62 NO-UNDO.

DEFINE VARIABLE LevFargKod AS CHARACTER FORMAT "X(15)" 
     LABEL "Lev.fargekode" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE LevKod AS CHARACTER FORMAT "x(20)" 
     LABEL "Lev.artikkelnr" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE levnamn AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 17 BY .62 NO-UNDO.

DEFINE VARIABLE LevNr AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Lev.nr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE Pris AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE StrTypeID AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Størrelsestype" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE VareKost AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "VareKost" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE Vg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Varegruppe" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1.

DEFINE VARIABLE vgbeskr AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 17 BY .62 NO-UNDO.

DEFINE RECTANGLE rectWinTB
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 17 BY 1.19.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     LevNr AT ROW 3 COL 17 COLON-ALIGNED HELP
          "Leverandørnummer"
     btnLev AT ROW 3 COL 29.6 NO-TAB-STOP 
     LevKod AT ROW 4 COL 17 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer"
     LevFargKod AT ROW 5 COL 17 COLON-ALIGNED HELP
          "Leverandørens fargekode"
     Beskr AT ROW 6 COL 17 COLON-ALIGNED HELP
          "Kort beskrivelse av artikkelen"
     Vg AT ROW 7 COL 17 COLON-ALIGNED HELP
          "'varegruppenummer"
     btnVarGr AT ROW 7.05 COL 29.6 NO-TAB-STOP 
     StrTypeID AT ROW 8 COL 17 COLON-ALIGNED HELP
          "Størrelsestype"
     btnStr AT ROW 8.05 COL 29.6 NO-TAB-STOP 
     matkod AT ROW 9.05 COL 17 COLON-ALIGNED
     sasong AT ROW 10.05 COL 17 COLON-ALIGNED
     Pris AT ROW 11.1 COL 17 COLON-ALIGNED HELP
          "Pris inkl. mva."
     VareKost AT ROW 12.19 COL 17 COLON-ALIGNED HELP
          "Kalkulert varekost"
     btnSave AT ROW 13.81 COL 15
     levnamn AT ROW 3.24 COL 33 COLON-ALIGNED NO-LABEL
     vgbeskr AT ROW 7.24 COL 33 COLON-ALIGNED NO-LABEL
     Beskrivelse AT ROW 8.24 COL 33 COLON-ALIGNED NO-LABEL
     rectWinTB AT ROW 1.24 COL 35
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 52.2 BY 14.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Kopier artikkel"
         HEIGHT             = 14.38
         WIDTH              = 52
         MAX-HEIGHT         = 57.14
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 57.14
         VIRTUAL-WIDTH      = 320
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
ASSIGN 
       Beskrivelse:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       levnamn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       vgbeskr:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

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
ON END-ERROR OF C-Win /* Kopier artikkel */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kopier artikkel */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLev C-Win
ON CHOOSE OF btnLev IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "levnr;levnamn".

  RUN JBoxDLookup.w ("LevBas;Levnr;levnamn", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN     
    ASSIGN levnr:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")
           levnamn:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")
           .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSave
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSave C-Win
ON CHOOSE OF btnSave IN FRAME DEFAULT-FRAME /* Lagre og avslutt */
DO:
  IF DYNAMIC-FUNCTION("runproc","artbas_lagkopi.p",STRING(fArtikkelNr),HentVerdier()) THEN DO:
    RUN "NyArtBas" IN ihParent (DEC(DYNAMIC-FUNCTION("getTransactionMessage"))).
    APPLY "close" TO THIS-PROCEDURE.
  END.
  ELSE
    DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("GetTransactionMessage"),"Feil","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStr C-Win
ON CHOOSE OF btnStr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "strtypeid;Beskrivelse".

  RUN JBoxDLookup.w ("StrType;strtypeid;Beskrivelse;KortNavn;Alfafordeling", 
                     "WHERE strtypeid > 1",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN     
    ASSIGN strtypeid:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")
           Beskrivelse:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")
           .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVarGr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVarGr C-Win
ON CHOOSE OF btnVarGr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "vg;vgbeskr".

  RUN JBoxDLookup.w ("VarGr;vg;vgBeskr", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN     
    ASSIGN vg:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")
           vgBeskr:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")
           .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME matkod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL matkod C-Win
ON RETURN OF matkod IN FRAME DEFAULT-FRAME /* Materiale */
DO:
  APPLY "tab" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME sasong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL sasong C-Win
ON RETURN OF sasong IN FRAME DEFAULT-FRAME /* Sesong */
DO:
  APPLY "tab" TO SELF.
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
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
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

  RUN InitWindow.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

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
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
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
  DISPLAY LevNr LevKod LevFargKod Beskr Vg StrTypeID matkod sasong Pris VareKost 
          levnamn vgbeskr Beskrivelse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectWinTB LevNr btnLev LevKod LevFargKod Beskr Vg btnVarGr StrTypeID 
         btnStr matkod sasong Pris VareKost btnSave levnamn vgbeskr Beskrivelse 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow C-Win 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFieldValues AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN fArtikkelNr = DYNAMIC-FUNCTION("getArtNr" IN ihParent)
         matkod:DELIMITER = "|"
         sasong:DELIMITER = "|"
         matkod:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","Material;Matkod|MatBeskr;Matkod","WHERE true")
         sasong:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","Sasong;SasBeskr;Sasong","WHERE true")
         cFieldValues = DYNAMIC-FUNCTION("getFieldValues","Artbas","WHERE Artikkelnr = " + STRING(fArtikkelnr),
                                         "Levnr,LevKod,LevFargKod,Beskr,Vg,StrTypeID,MatKod,Sasong")
         Levnr:SCREEN-VALUE = ENTRY(1,cFieldValues,"|")
         LevKod:SCREEN-VALUE = ENTRY(2,cFieldValues,"|")
         LevFargKod:SCREEN-VALUE = ENTRY(3,cFieldValues,"|")
         Beskr:SCREEN-VALUE = ENTRY(4,cFieldValues,"|")
         Vg:SCREEN-VALUE = ENTRY(5,cFieldValues,"|")
         StrTypeId:SCREEN-VALUE = ENTRY(6,cFieldValues,"|")
         MatKod:SCREEN-VALUE = ENTRY(7,cFieldValues,"|")
         Sasong:SCREEN-VALUE = ENTRY(8,cFieldValues,"|")
         cFieldValues = DYNAMIC-FUNCTION("getFieldValues","ArtPris","WHERE Artikkelnr = " + STRING(fArtikkelnr),
                                         "Pris;1,Varekost;1")
         Pris:SCREEN-VALUE = ENTRY(1,cFieldValues,"|")
         Varekost:SCREEN-VALUE = ENTRY(2,cFieldValues,"|")
         levnamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","LevBas","WHERE levnr = " + Levnr:SCREEN-VALUE,"levnamn")
         vgbeskr:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","VarGr","WHERE Vg = " + Vg:SCREEN-VALUE,"VgBeskr")
         Beskrivelse:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","StrType","WHERE StrTypeId = " + StrTypeId:SCREEN-VALUE,"Beskrivelse")
         .

  hWinTB = DYNAMIC-FUNCTION("NewToolbar",
                 rectWinTB:HANDLE,
                 "Fil",
                 "close;Avslutt",
                 "maxborder,right,enable").

END.
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
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HentVerdier C-Win 
FUNCTION HentVerdier RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hField AS HANDLE NO-UNDO.

EMPTY TEMP-TABLE ttVerdier.
hField = FRAME {&FRAME-NAME}:FIRST-CHILD:FIRST-CHILD.

REPEAT WHILE VALID-HANDLE(hField):
  IF CAN-DO("fill-in,combo-box",hField:TYPE) THEN DO:
    CREATE ttVerdier.
    ASSIGN ttVerdier.cNavn  = hField:NAME
           ttVerdier.cVerdi = hField:INPUT-VALUE
           .
  END.
  hField = hField:NEXT-SIBLING.
END.

RETURN httVerdier.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

