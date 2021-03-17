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

/* Local Variable Definitions ---                                       */

DEF VAR cTabProg     AS CHAR   NO-UNDO.
DEF VAR bOK          AS LOG    NO-UNDO.

DEF VAR hWinToolbar  AS HANDLE NO-UNDO.
DEF VAR cKunde       AS CHAR   NO-UNDO.
DEF VAR cFaktura     AS CHAR   NO-UNDO.
DEF VAR cOrdre       AS CHAR   NO-UNDO.
DEF VAR hPinButton   AS HANDLE NO-UNDO.
DEF VAR hParent      AS HANDLE NO-UNDO.
DEF VAR bFind        AS LOG    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-64 RECT-65 RECT-66 rectWinToolbar ~
KundeNr Navn KortNr Telefon MobilTlf EksterntKundeNr FakturaNr Bilagstype ~
Faktura_Id KOrdre_Id KProsjektNr btnOk btnCancel 
&Scoped-Define DISPLAYED-OBJECTS KundeNr Navn KortNr Telefon MobilTlf ~
EksterntKundeNr FakturaNr Bilagstype Faktura_Id KOrdre_Id KProsjektNr 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD DoSearch C-Win 
FUNCTION DoSearch RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCancel 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnOk 
     LABEL "OK" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE Bilagstype AS INTEGER FORMAT "99":U INITIAL 1 
     LABEL "Bilagstype" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Faktura",1,
                     "Kreditnota",2,
                     "Purring",10
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE EksterntKundeNr AS CHARACTER FORMAT "X(20)" 
     LABEL "Ekst.kundenr" 
     VIEW-AS FILL-IN 
     SIZE 22 BY 1.

DEFINE VARIABLE FakturaNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Fakturanummer" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE Faktura_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Faktura Id" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE KOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Ordrenr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE KortNr AS CHARACTER FORMAT "X(22)" 
     LABEL "Kundekort" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1.

DEFINE VARIABLE KProsjektNr AS INTEGER FORMAT ">>>9" INITIAL 0 
     LABEL "Kundeprosjekt" 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1.

DEFINE VARIABLE KundeNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Kundenummer" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1.

DEFINE VARIABLE MobilTlf AS CHARACTER FORMAT "X(15)" 
     LABEL "Mobiltelefon" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE Navn AS CHARACTER FORMAT "X(40)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.

DEFINE VARIABLE Telefon AS CHARACTER FORMAT "X(15)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE RECTANGLE RECT-64
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 7.33.

DEFINE RECTANGLE RECT-65
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 2.71.

DEFINE RECTANGLE RECT-66
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 66 BY 2.71.

DEFINE RECTANGLE rectWinToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14.6 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     KundeNr AT ROW 2.76 COL 17 COLON-ALIGNED HELP
          "Kundenummer"
     Navn AT ROW 3.76 COL 17 COLON-ALIGNED HELP
          "Navn eller firmanavn"
     KortNr AT ROW 4.81 COL 17 COLON-ALIGNED HELP
          "Kortnummer"
     Telefon AT ROW 5.86 COL 17 COLON-ALIGNED HELP
          "Telefon"
     MobilTlf AT ROW 6.91 COL 17 COLON-ALIGNED HELP
          "Mobiltelefon"
     EksterntKundeNr AT ROW 7.91 COL 17 COLON-ALIGNED HELP
          "Eksternt kundenummer (Fra f.eks fakturasystem)"
     FakturaNr AT ROW 10.24 COL 16.8 COLON-ALIGNED HELP
          "Fakturanummer"
     Bilagstype AT ROW 10.24 COL 49 COLON-ALIGNED
     Faktura_Id AT ROW 11.24 COL 16.8 COLON-ALIGNED HELP
          "Internt faktura id. Tildeles autmatisk av systemet."
     KOrdre_Id AT ROW 12.86 COL 16.8 COLON-ALIGNED HELP
          "Internt faktura id. Tildeles autmatisk av systemet."
     KProsjektNr AT ROW 13.86 COL 16.8 COLON-ALIGNED HELP
          "Kundeprosjekt"
     btnOk AT ROW 15.48 COL 37
     btnCancel AT ROW 15.48 COL 52.4
     RECT-64 AT ROW 2.48 COL 2
     RECT-65 AT ROW 9.86 COL 2
     RECT-66 AT ROW 12.48 COL 2
     rectWinToolbar AT ROW 1.29 COL 49
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 67.8 BY 15.71
         DEFAULT-BUTTON btnOk.


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
         TITLE              = "Søk kunde, ordre eller faktura"
         HEIGHT             = 15.71
         WIDTH              = 67.4
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Søk kunde, ordre eller faktura */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Søk kunde, ordre eller faktura */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY "close" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* OK */
DO:
  IF DoSearch() THEN DO:
    RUN setSearch IN hParent (cKunde,bFind,cTabProg).
    IF NOT DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"keepwindowopen") = "yes" THEN
      APPLY "close" TO THIS-PROCEDURE.
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Finnes ikke","","").
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
  DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW).
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
  hParent = SOURCE-PROCEDURE.
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

{incl/wintrigg.i}

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
  DISPLAY KundeNr Navn KortNr Telefon MobilTlf EksterntKundeNr FakturaNr 
          Bilagstype Faktura_Id KOrdre_Id KProsjektNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-64 RECT-65 RECT-66 rectWinToolbar KundeNr Navn KortNr Telefon 
         MobilTlf EksterntKundeNr FakturaNr Bilagstype Faktura_Id KOrdre_Id 
         KProsjektNr btnOk btnCancel 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
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
DEF INPUT PARAM icTabProg     AS CHAR NO-UNDO.

cTabProg     = icTabProg.


DO WITH FRAME {&FRAME-NAME}:
  hWinToolbar = DYNAMIC-FUNCTION("NewToolbar",
                             rectWinToolbar:HANDLE,
                             "Fil",
                             "Close;Exit,Pin;;;pinWindow;gif/pushout.gif",
                             "right,enable").  

  hPinButton = WIDGET-HANDLE(DYNAMIC-FUNCTION("getAttribute",hWinToolbar,"ButtonPin")).
  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).

  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
  THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
  
  CASE cTabProg:
    WHEN "Kunde_brw.w" THEN APPLY "entry" TO Navn.
    WHEN "KundeView.w" THEN APPLY "entry" TO Navn.
    WHEN "KOrdre.w"    THEN APPLY "entry" TO KOrdre_Id.
    WHEN "Faktura.w"   THEN APPLY "entry" TO FakturaNr.
  END CASE.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PinWindow C-Win 
PROCEDURE PinWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("getAttribute",THIS-PROCEDURE,"keepwindowopen") = "yes" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",THIS-PROCEDURE,"keepwindowopen","").
  hPinButton:LOAD-IMAGE("gif/pushout.gif").
END.
ELSE DO:
  DYNAMIC-FUNCTION("setAttribute",THIS-PROCEDURE,"keepwindowopen","yes").
  hPinButton:LOAD-IMAGE("gif/pushin.gif").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION DoSearch C-Win 
FUNCTION DoSearch RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN KundeNr KortNr MobilTlf Navn Telefon Faktura_Id FakturaNr KOrdre_Id KProsjektNr Bilagstype EksterntKundeNr
         bFind = TRUE.

  IF FakturaNr    NE 0
    OR Faktura_id NE 0 THEN DO:
    cKunde = DYNAMIC-FUNCTION("getFieldValues","FakturaHode",
                        "WHERE true" 
                      + (IF Bilagstype NE 0 THEN 
                          " AND Bilagstype = " + STRING(Bilagstype)
                         ELSE "")
                      + (IF FakturaNr NE 0 THEN 
                          " AND FakturaNr = " + STRING(FakturaNr)
                         ELSE "")
                      + (IF Faktura_id NE 0 THEN 
                          " AND Faktura_id = " + STRING(Faktura_id)
                         ELSE "")
                        ,"KundeNr,Faktura_id").
    IF cKunde NE ? THEN cTabProg = "Faktura.w".
  END.
  ELSE IF KOrdre_id NE 0 
     OR KProsjektNr NE 0 THEN DO:
    cKunde = DYNAMIC-FUNCTION("getFieldValues","KOrdreHode",
                              "WHERE true" 
                            + (IF KOrdre_id NE 0 THEN 
                                " AND KOrdre_id = " + STRING(KOrdre_id)
                               ELSE "")
                            + (IF KProsjektNr NE 0 THEN 
                                " AND KProsjektNr = " + STRING(KProsjektNr)
                               ELSE "")
                              ,"KundeNr,KOrdre_id").
    IF cKunde NE ? THEN cTabProg = "KOrdre.w".
  END.
  ELSE IF KundeNr NE 0
     OR MobilTlf  NE "" 
     OR Telefon   NE ""
     OR EksterntKundeNr NE "" THEN DO:
    cKunde = DYNAMIC-FUNCTION("getFieldValues","Kunde",
                              "WHERE true" 
                            + (IF Kundenr NE 0 THEN 
                                " AND Kundenr = " + STRING(Kundenr)
                               ELSE "")
                            + (IF Navn NE "" THEN
                                " AND Navn " + (IF INDEX(Navn,"*") > 0 THEN "MATCHES '" ELSE "BEGINS '") + Navn + "'"
                               ELSE "")
                            + (IF Telefon NE "" THEN
                                " AND Telefon " + (IF INDEX(Telefon,"*") > 0 THEN "MATCHES '" ELSE "BEGINS '") + Telefon + "'"
                               ELSE "")
                            + (IF MobilTlf NE "" THEN
                                " AND MobilTlf " + (IF INDEX(MobilTlf,"*") > 0 THEN "MATCHES '" ELSE "BEGINS '") + MobilTlf + "'"
                               ELSE "")
                            + (IF EksterntKundeNr NE "" THEN
                                " AND EksterntKundeNr " + (IF INDEX(EksterntKundeNr,"*") > 0 THEN "MATCHES '" ELSE "BEGINS '") + EksterntKundeNr + "'"
                               ELSE "")
                              ,"Kundenr").
    IF cKunde NE "" THEN cTabProg = "KundeView.w".
  END.
  ELSE IF KortNr NE "" THEN DO:
    cKunde = DYNAMIC-FUNCTION("getFieldValues","KundeKort",
                              "WHERE KortNr = '" + KortNr + "'" 
                              ,"Kundenr").
    IF cKunde = ? THEN
      cKunde = DYNAMIC-FUNCTION("getFieldList","MedlemsKort;,Medlem;KundeNr",
                                "WHERE KortNr = '" + KortNr + "'" 
                              + ",FIRST Medlem OF MedlemsKort").
    IF cKunde NE "" AND cKunde NE ? THEN 
      cTabProg = "KundeView.w".
    ELSE cKunde = ?.
  END.
  ELSE IF Navn NE "" THEN DO:
    ASSIGN cKunde   = "WHERE Navn " + (IF INDEX(Navn,"*") > 0 THEN "MATCHES '" ELSE "BEGINS '") + Navn + "'"
           cTabProg = "KundeView.w"
           bFind    = FALSE.
  END.
END.

IF cKunde NE ? THEN RETURN TRUE.
ELSE RETURN FALSE.  

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

