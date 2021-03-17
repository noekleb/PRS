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
DEF VAR hParentBrowse     AS HANDLE NO-UNDO.
DEF VAR hBrowse           AS HANDLE NO-UNDO.
DEF VAR hFieldMap         AS HANDLE NO-UNDO.
DEF VAR hToolbar          AS HANDLE NO-UNDO.
DEF VAR hTabFolder        AS HANDLE NO-UNDO.

DEF VAR hCurrTabProc      AS HANDLE NO-UNDO.
DEF VAR hCurrTabFrame     AS HANDLE NO-UNDO.
DEF VAR iCurrTab          AS INT NO-UNDO.
DEF VAR cKundekortJoin    AS CHAR   NO-UNDO.

{dproclibstart.i}
{etikettlogg.i &NEW=NEW}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectMedlem rectTBMedlem 

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


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE rectMedlem
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 141.4 BY 23.1.

DEFINE RECTANGLE rectTBMedlem
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rectMedlem AT ROW 2.67 COL 2
     rectTBMedlem AT ROW 1.24 COL 1.8
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
                                                                        */
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
{incl/supptrigg.i hBrowse}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditMedlem C-Win 
PROCEDURE EditMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM irMedlem AS RECID NO-UNDO.

THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
RUN w-vmedlem.w (irMedlem,"ENDRE").
RUN RefreshCurrentRow IN hParent.
THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EditRecord C-Win 
PROCEDURE EditRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN EditMedlem (DYNAMIC-FUNCTION("getRecId","Medlem",hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE)).
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
  ENABLE rectMedlem rectTBMedlem 
      WITH FRAME DEFAULT-FRAME.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EtikettRecord C-Win 
PROCEDURE EtikettRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR iPrinterValg AS INTEGER    NO-UNDO.
  DEFINE VAR iAntall      AS INTEGER    NO-UNDO.
  DEFINE VAR dKortNummer  AS DECIMAL DECIMALS 0 NO-UNDO.
  DEFINE VAR cEANprefix        AS CHAR INIT "29"  NO-UNDO.
  DEFINE VAR cEAN         AS CHAR FORMAT "x(13)" NO-UNDO. 
  
  DEF VAR lMedlemsNr AS DEC NO-UNDO.

  ASSIGN
      lMedlemsNr  = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("MedlemsNr"):BUFFER-VALUE
      dKortNummer = hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("KortNr"):BUFFER-VALUE
      .

  IF lMedlemsNr = 0 OR
      lMedlemsnr = ? THEN
  DO:
      MESSAGE "Ukjent medlem"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.

  ASSIGN 
      cEAN = DYNAMIC-FUNCTION('fixChkEAN':U IN h_dproclib,
               cEANprefix + STRING(dKortNummer,"9999999999")).

  RUN d-skrivervalgmedlem.w (OUTPUT iPrinterValg,INPUT-OUTPUT iAntall).
  IF iPrinterValg > 0 AND iAntall > 0 THEN DO:
      EMPTY TEMP-TABLE EtikettLogg.
      FIND Butiker WHERE 
          Butiker.Butik = int(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("ButikkNr"):BUFFER-VALUE) NO-LOCK NO-ERROR.
      CREATE EtikettLogg.
      ASSIGN EtikettLogg.storl     = cEan
             EtikettLogg.bongtekst = "Medlem|" + REPLACE(SUBSTR(TRIM(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Innehaver"):BUFFER-VALUE),1,25),"|"," ") + "|" +
                          (IF AVAIL Butiker THEN REPLACE(SUBSTR(TRIM(Butiker.butnamn),1,25),"|"," ") ELSE "") + "|" +
                          IF hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("UtgarDato"):BUFFER-VALUE = ? THEN "" ELSE STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("UtgarDato"):BUFFER-VALUE,"99/99/99")
             EtikettLogg.Ant       = iAntall.
      RUN x-etikettsend.w (iPrinterValg).
      EMPTY TEMP-TABLE EtikettLogg.
  END.

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

  cKundekortJoin   =  ",FIRST MedlemsKort of Medlem NO-LOCK WHERE MedlemsKort.InterntKKortId > 0 OUTER-JOIN"
                       + ",FIRST KundeKort NO-LOCK where KundeKort.InterntKKortId = MedlemsKort.InterntKKortId OUTER-JOIN".

  /* Create the browse: */
   hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                           rectMedlem:HANDLE,        /* Coordinates */
                           100,                      /* Batchsize */
                           "",                       /* Attributes that must be set at creation time for the browse */
                           "Medlem"             /* Buffers and fields (and calculated fields) for browse */
                           + ";MedlemsNr"
                           + ";EtterNavn"
                           + ";ForNavn"
                           + ";FodselsDato|Født|99/99/9999"
                           + ";ButikkNr"
                           + ";ePostAdresse"
                           + ";Telefon"
                           + ";MobilTlf"
                           + ";Adresse1"
                           + ";Adresse2"
                           + ";PostNr"
                           + ";MedGruppe"
                           + ";MedType"
                           + ";!KundeNr"
                           + ";ButikkNr"
                           + ";Land"
                           + ";Opphort"
                           + ";HovedMedlemsNr"
                           + ";Kjonn"
                           + ";Aktiv"
                           + ";AktivertFraWeb"      
                           + ";WebBrukerId"         
                           + ";WebPassord"          
                           + ";Kilde"               
                           + ";TilgKilde"           
                           + ";Rabatt"              
                           + ";EksterntMedlemsNr"   
                           + ";RegistrertDato"
                           + ";RegistrertAv"
                           + ",Post"
                           + ";Beskrivelse"
                           + ",MedlemsKort;KortNr|Medlemskort@2;AktivertDato|Aktivert@3;UtgarDato|Utgår@4;!Innehaver"
                           + ",KundeKort;KortNr|Kundekort@5"
                           ,"WHERE false"
                          + ",FIRST Post OF Medlem NO-LOCK OUTER-JOIN"
                          + cKundekortJoin
                           ,"sort|EtterNavn").         
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,THIS-PROCEDURE).
  hBrowse:NAME = "brwMedlem".
  hBrowse:MOVE-COLUMN(14,11).

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                            rectTBMedlem:HANDLE,
                            "",
                            "New;Ny,Edit;Rediger,excel,Etikett;Medlemsetikett",
                            "maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hBrowse,hToolbar).



  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectTBMedlem").

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
DEF VAR hWidget AS HANDLE NO-UNDO.
hWidget = hBrowse:GET-BROWSE-COLUMN(1).
APPLY "end-resize" TO hWidget.
FRAME {&FRAME-NAME}:MOVE-TO-TOP().


APPLY "entry" TO hBrowse.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:      Dersom medlem blir opprettet på serveren så returneres rowid til medlem i Trans.msg
              Så gjør vi en ekstra tur for å få tak i recid'en. 
------------------------------------------------------------------------------*/
IF DYNAMIC-FUNCTION("runproc","medlem_opprett.p",STRING(DYNAMIC-FUNCTION("getKundeNr" IN hParent)),?) THEN 
  RUN EditMedlem (DYNAMIC-FUNCTION("getRecId","Medlem",DYNAMIC-FUNCTION("getTransactionMessage"))).
ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
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
ASSIGN hParentBrowse = ihQuery
       hParentBuffer = ihQuery:QUERY:GET-BUFFER-HANDLE(1).
RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

