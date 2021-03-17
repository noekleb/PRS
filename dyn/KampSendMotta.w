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

DEF VAR bOK           AS LOG    NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.

DEF VAR hBrowse       AS HANDLE NO-UNDO.
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hBuffer       AS HANDLE NO-UNDO.
DEF VAR hToolbar      AS HANDLE NO-UNDO.
DEF VAR hParent       AS HANDLE NO-UNDO.

DEF VAR cQueryJoin    AS CHAR   NO-UNDO.

DEFINE VARIABLE dKampId AS DECIMAL  INIT 9000003  NO-UNDO.

/*Browse stuff*/
DEF VAR hbcButik          AS HANDLE NO-UNDO.
DEF VAR hbfOkMottatt      AS HANDLE NO-UNDO.
DEF VAR hbfMottattDato    AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectFaktHode rectToolbar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE rsResultat AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Alla", 1,
"OK", 2,
"Fel", 3,
"Inte bekräftade", 4
     SIZE 55 BY .71 NO-UNDO.

DEFINE RECTANGLE rectFaktHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 150 BY 27.48.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14 BY 1.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     rsResultat AT ROW 1.52 COL 51 NO-LABEL
     rectFaktHode AT ROW 2.81 COL 2
     rectToolbar AT ROW 1.43 COL 137.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 152 BY 29.43.


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
         TITLE              = "Kontroll sända mottagna kampanjer"
         HEIGHT             = 29.43
         WIDTH              = 152
         MAX-HEIGHT         = 50
         MAX-WIDTH          = 320
         VIRTUAL-HEIGHT     = 50
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

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
   FRAME-NAME                                                           */
/* SETTINGS FOR RADIO-SET rsResultat IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       rsResultat:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Kontroll sända mottagna kampanjer */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Kontroll sända mottagna kampanjer */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Kontroll sända mottagna kampanjer */
DO:
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsResultat
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsResultat C-Win
ON VALUE-CHANGED OF rsResultat IN FRAME DEFAULT-FRAME
DO:
  RUN SetQueryFilter(YES).
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
{incl/wintrigg.i}
ON CLOSE OF THIS-PROCEDURE DO:
/*   IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN */
/*     RETURN NO-APPLY.                                                             */
/*   PUBLISH "InvalidateHandle" (THIS-PROCEDURE).                                   */
/*   DYNAMIC-FUNCTION("setCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).            */
  RUN disable_UI.
END.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  hParent = SOURCE-PROCEDURE.
  IF VALID-HANDLE(hParent) AND CAN-DO(hParent:INTERNAL-ENTRIES,"GetKampid") THEN
      dKampId = DYNAMIC-FUNCTION("GetKampid" IN hParent).
  {&WINDOW-NAME}:TITLE = {&WINDOW-NAME}:TITLE + " - Kampanj:" + STRING(dKampId).
  RUN enable_UI.
  RUN InitWindow.
  THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DefaultActionBrowse C-Win 
PROCEDURE DefaultActionBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN setSearch IN hParent (STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("KundeNr"):BUFFER-VALUE)
                          ,YES
                          ,"").
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
  ENABLE rectFaktHode rectToolbar 
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
DO WITH FRAME {&FRAME-NAME}:

/*   RUN SetQueryFilter(YES). */
  hBrowse = DYNAMIC-FUNCTION("NewBrowse",
                          rectFaktHode:HANDLE,        /* Coordinates */
                          10000,                      /* Batchsize */
                          "",                 /* Attributes that must be set at creation time for the browse */
                          "KampanjeButMottak"             /* Buffers and fields (and calculated fields) for browse */
                          + ";Butik"
                          + ";!KampId"
                          + ";SekvNr"
                          + ";OkMottatt|Status|OK/" 
                          + ";RegistrertDato|Sendt"
/*                           + ";RegistrertTid|Tid" */
                          + ";+cRegTime|CHARACTER|x(5)|jbserv_int_to_hhmm_time.p (RegistrertTid)|Tid"
                          + ";MottattDato|Mottatt"
/*                           + ";MottattTid|Tid" */
                          + ";+cMotTime|CHARACTER|x(5)|jbserv_int_to_hhmm_time.p (MottattTid)|Tid"
                          + ";Filnavn"
                          + ";Resultat|Resultat|x(70)"
                          ,"WHERE Kampid = " + STRING(dKampid)
                          ,"sort|Butik desc").         
  /* 
   + ";+cFromTime|CHARACTER|x(5)|jbserv_int_to_hhmm_time.p (CalenderTimeFrom)|From"
   */


  hBrowse:NAME = "brwKampanjeButMottak".
  hQuery = hBrowse:QUERY.
  hBuffer = hQuery:GET-BUFFER-HANDLE(1).

/*   hBrowse:MOVE-COLUMN(11,1). */
/*   hBrowse:MOVE-COLUMN(12,2). */
/*   hBrowse:MOVE-COLUMN(13,5). */
/*   hBrowse:MOVE-COLUMN(14,6). */


  hToolbar = DYNAMIC-FUNCTION("NewToolBar",
                    rectToolBar:HANDLE,             
                    "Fil",                          
                    "Reject;&Slett;Fjern markerte poster fra kjøring;RejectRecord;bmp/rejec16e.bmp"
                  + ",excel"
/*                   + ",PrintPreview;Forhvisn;Forhåndsvisning;PrintPreviewRecord;gif/afprintpre.gif"  */
                  + ",print"
                    ,"").
  DYNAMIC-FUNCTION("createObjectLink",hBrowse,hToolbar).
APPLY "value-changed" TO hBrowse.
  DYNAMIC-FUNCTION("InitTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
  DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,400,500,500,550).
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
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
/* APPLY "entry" TO cbPurreTrinn IN FRAME {&FRAME-NAME}. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpenQuery C-Win 
PROCEDURE OpenQuery :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"querywhere", */
/*                    "WHERE Kampid = " + STRING(dKampId)  */
/*                    ).                                   */
END.

/* CLIPBOARD:VALUE =  DYNAMIC-FUNCTION("getAttribute",hBrowse,"querywhere").  */
        
DYNAMIC-FUNCTION("setCurrentObject",hBrowse).

RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintPreviewRecord C-Win 
PROCEDURE PrintPreviewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* DEF VAR cIdList  AS CHAR NO-UNDO.                                                                                                                                                          */
/*                                                                                                                                                                                            */
/* DO ix = 1 TO hBrowse:NUM-SELECTED-ROWS:                                                                                                                                                    */
/*   IF hBrowse:FETCH-SELECTED-ROW(ix) THEN                                                                                                                                                   */
/*     cIdList = cIdList + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("Reskontro_id"):BUFFER-VALUE) + ",".                                                                        */
/* END.                                                                                                                                                                                       */
/* IF cIdList NE "" THEN DO WITH FRAME {&FRAME-NAME}:                                                                                                                                         */
/*   cIdList = TRIM(cIdList,",").                                                                                                                                                             */
/*   IF NOT DYNAMIC-FUNCTION("runproc","faktura_purring.p","idlist|" + cIdList + "|" + cbPurreTrinn:SCREEN-VALUE,?) THEN                                                                      */
/*     DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").                                                                                                     */
/*   ELSE DO:                                                                                                                                                                                 */
/*     /* Får tilbake liste over genererte purrefaktuaer: */                                                                                                                                  */
/*     cIdList = DYNAMIC-FUNCTION("getTransactionMessage").                                                                                                                                   */
/*                                                                                                                                                                                            */
/*     RUN skrivfaktura.p (cIdList + "|",FALSE,"",1,                                                                                                                                          */
/*                         DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE KundeNr = " + STRING(hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("KundeNr"):BUFFER-VALUE),"ePostAdresse"),1). */
/*     RUN OpenQuery.                                                                                                                                                                         */
/*   END.                                                                                                                                                                                     */
/* END.                                                                                                                                                                                       */
/* ELSE DYNAMIC-FUNCTION("DoMessage",0,0,"Ingen faktura er valgt","","").                                                                                                                     */

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
/* DEF VAR cIdList  AS CHAR NO-UNDO.                                                                                                                                          */
/* DEF VAR cPrinter AS CHAR NO-UNDO.                                                                                                                                          */
/* DEF VAR iAntEks  AS INT  NO-UNDO INIT 1.                                                                                                                                   */
/* DEF VAR iFormat  AS INT  NO-UNDO INIT 1.                                                                                                                                   */
/*                                                                                                                                                                            */
/* hQuery:GET-FIRST().                                                                                                                                                        */
/* REPEAT WHILE NOT hQuery:QUERY-OFF-END:                                                                                                                                     */
/*   cIdList = cIdList + STRING(hBuffer:BUFFER-FIELD("Reskontro_id"):BUFFER-VALUE) + ",".                                                                                     */
/*   hQuery:GET-NEXT().                                                                                                                                                       */
/* END.                                                                                                                                                                       */
/*                                                                                                                                                                            */
/* IF cIdList NE "" THEN DO WITH FRAME {&FRAME-NAME}:                                                                                                                         */
/*   cIdList = TRIM(cIdList,",").                                                                                                                                             */
/*   RUN DSelectPrinter.w (INPUT-OUTPUT cPrinter,INPUT-OUTPUT iAntEks,INPUT-OUTPUT iFormat,"Fakturaskriver",OUTPUT bOk).                                                      */
/*   IF NOT bOk THEN RETURN.                                                                                                                                                  */
/*                                                                                                                                                                            */
/*   IF NOT DYNAMIC-FUNCTION("runproc","faktura_purring.p","idlist|" + cIdList + "|" + cbPurreTrinn:SCREEN-VALUE,?) THEN                                                      */
/*     DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").                                                                                     */
/*   ELSE DO:                                                                                                                                                                 */
/*     /* Får tilbake liste over genererte purrefaktuaer: */                                                                                                                  */
/*     cIdList = DYNAMIC-FUNCTION("getTransactionMessage").                                                                                                                   */
/*                                                                                                                                                                            */
/* /*       RUN skrivfaktura.p (cIdList + "|",TRUE,cPrinter,iAntEks,"",iFormat). */                                                                                           */
/*       RUN skrivfaktura.p (cIdList + "|",TRUE,cPrinter,iAntEks,"",iFormat).                                                                                                 */
/*                                                                                                                                                                            */
/* /*       RUN skrivfaktura.p (cIdList + "|",FALSE,"",1,                                                                                                                  */ */
/* /*                           DYNAMIC-FUNCTION("getFieldValues","Kunde","WHERE KundeNr = " + STRING(hFieldMap:BUFFER-FIELD("KundeNr"):BUFFER-VALUE),"ePostAdresse"),1).  */ */
/*   END.                                                                                                                                                                     */
/* END.                                                                                                                                                                       */
/*                                                                                                                                                                            */
/* RUN OpenQuery.                                                                                                                                                             */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RejectRecord C-Win 
PROCEDURE RejectRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF hBrowse:NUM-SELECTED-ROWS > 0 THEN 
  hBrowse:DELETE-SELECTED-ROWS().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplayBrowse C-Win 
PROCEDURE RowDisplayBrowse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR iGreen  AS INT INIT 10 NO-UNDO.
  DEF VAR iRed    AS INT INIT 12 NO-UNDO.
  DEF VAR iDefault AS INT INIT 14 NO-UNDO.

  hbcButik:BGCOLOR =  IF hbfOkMottatt:BUFFER-VALUE = TRUE THEN iGreen 
                           ELSE IF hbfMottattDato:BUFFER-VALUE <> ? THEN iRed
                           ELSE ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetQueryFilter C-Win 
PROCEDURE SetQueryFilter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ibExecute AS LOG NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  ASSIGN rsResultat.

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"queryfilter",
                  (IF rsResultat = 4 THEN
                     " AND KampanjeButMottak.Resultat = ''"
              ELSE IF rsResultat = 3 THEN
                     " AND KampanjeButMottak.OkMottatt = TRUE"
              ELSE IF rsResultat = 2 THEN
                     " AND KampanjeButMottak.OkMottatt = FALSE AND KampanjeButMottak.Resultat <> ''"
              ELSE "")
                   ).

  IF ibExecute THEN DO:
    DYNAMIC-FUNCTION("setCurrentObject",hBrowse).
    RUN OpenQuery.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSearch C-Win 
PROCEDURE StartSearch :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
/*   IF cbPurreTrinn:SCREEN-VALUE = ? THEN RETURN. */
  RUN SUPER.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse     AS HANDLE,
    INPUT icBrowseName AS CHAR):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    IF icBrowseName = 'rectFaktHode' THEN
    DO ix = 1 TO ihBrowse:NUM-COLUMNS:
      CASE ihBrowse:GET-BROWSE-COLUMN(ix):NAME:
        WHEN 'OkMottatt' THEN 
          ASSIGN
            hbfOkMottatt = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('OkMottatt')
          .
        WHEN 'MottattDato' THEN 
          ASSIGN
            hbfMottattDato = ihBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD('MottattDato')
          .
          WHEN 'Butik' THEN 
            ASSIGN
              hbcButik = ihBrowse:GET-BROWSE-COLUMN(ix)
            .
      END CASE.
    END.

    RETURN TRUE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

