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
DEFINE VARIABLE iWidthPix  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iHeightPix AS INTEGER   NO-UNDO.
DEFINE VARIABLE hdlParent  AS HANDLE NO-UNDO. 
DEFINE VARIABLE glCancelImport AS LOGICAL NO-UNDO. 
DEF VAR hToolbar    AS HANDLE NO-UNDO.

hdlParent = SOURCE-PROCEDURE. 

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
  /*                    
 {lesexcelvpifil.i "new" "shared" } 
*/

     DEFINE TEMP-TABLE TT_Prisfil NO-UNDO
        FIELD Radnr           AS INTE FORMAT ">>>>9"  LABEL "Radnr"
        FIELD lNy             AS LOG  FORMAT "J/"     LABEL "Ny"
        FIELD cTandem         AS CHAR FORMAT "x(15)"  LABEL "Tandem"
        FIELD cStatus         AS CHAR FORMAT "x(4)"  LABEL "Anm."
        FIELD artbas_levnr    AS INTE FORMAT ">>>>>>>>>>>9" LABEL "Levnr"
        FIELD artbas_artnr    AS DECI FORMAT ">>>>>>>>>>>>9" LABEL "Varunr"
        FIELD strekkode_kode  AS CHAR FORMAT "x(15)" LABEL "EAN"
        FIELD pris_bestnr     AS INTE FORMAT ">>>>>>>9"      LABEL "Bestnr"
        FIELD vare_varetekst  AS CHAR FORMAT "x(40)"         LABEL "Varutext"
        FIELD vare_mengde     AS DECI FORMAT ">>,>>9.999"    LABEL "Mängd"
        FIELD vare_enhet      AS INTE FORMAT ">9"            LABEL "Enhet"
        FIELD vare_Cenhet     AS CHAR FORMAT "x(4)" LABEL  "Text" /* = 1 THEN "st" ELSE IF vare.enhet = 2 THEN "kg" ELSE IF vare.enhet = 3 THEN "l" ELSE "m") */
        FIELD vare_hgr        AS INTE FORMAT ">>>9" LABEL "Varugr"
        FIELD pris_engrosn    AS DECI FORMAT ">>,>>9.99"   LABEL "Inpris"
        FIELD rabatt          AS DECI FORMAT "9"           LABEL "Rab"
        FIELD pris_utprisn    AS DECI FORMAT ">>,>>9.99"   LABEL "Utpris"
        FIELD dbpris_utprisn  AS DECI FORMAT ">>,>>9.99" 
        FIELD pris_veilpris   AS DECI FORMAT ">>,>>9.99"   LABEL "Riktpris"
        FIELD vare_mva        AS DECI FORMAT ">9.99"       LABEL "Moms%"
        FIELD vare_mvagr      AS INTE FORMAT "9" LABEL "Momsgr" /* konv vid inläsning */
        FIELD vare_antpkn     AS DECI FORMAT ">,>>9.999" LABEL "Förp"
        FIELD vare_link       AS DECI FORMAT ">>>>>>>>>>>>9" LABEL "Link"
        FIELD Vare_Produsent  AS CHAR FORMAT "x(40)" LABEL "Producent"
        FIELD ProdNr          AS INT  FORMAT ">>>>>>9" LABEL "ProdNr"
        FIELD Vare_Varemerke  AS CHAR FORMAT "x(40)" LABEL "Varumärke"
        FIELD VmId            AS INT  FORMAT ">>>>>9" LABEL "VmId"
        FIELD Butikk_ButNr    AS INT  FORMAT ">>>>>9" LABEL "KundNr"
        FIELD ButNamn         AS CHAR FORMAT "x(30)" LABEL "ButNamn"
        FIELD Butik           AS INT  FORMAT ">>>>>9" LABEL "Butikk"
        FIELD fil_tbproc      AS DECI FORMAT "->>9.99" LABEL "TB%"
        FIELD hgrprof_brutto% AS DECI FORMAT "->>9.99" LABEL "VG TB%"
        FIELD orgdata         AS CHAR 
        FIELD cDummy          AS CHAR FORMAT "x(2)" LABEL ""
        INDEX Radnr IS PRIMARY UNIQUE Radnr
        INDEX Butik Butik vare_varetekst pris_bestnr.

 DEFINE TEMP-TABLE ttImportFields NO-UNDO
         FIELD FieldNumber AS INT FORMAT "99" LABEL "Felt Nr." 
         FIELD FieldLabel AS CHAR FORMAT "X(30)" LABEL "Name"
         FIELD FieldName AS CHAR FORMAT "X(30)" LABEL "Kolonne" 
         FIELD FieldSource AS CHAR FORMAT "x(8)" LABEL "Kilde"
         FIELD RowIdent1 AS CHAR .

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-3 BUTTON-2 CB-VPILev 
&Scoped-Define DISPLAYED-OBJECTS CB-VPILev 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-2 
     LABEL "Neste >>" 
     SIZE 24 BY 1.43.

DEFINE BUTTON BUTTON-3 
     LABEL "Avbryt" 
     SIZE 24 BY 1.43.

DEFINE VARIABLE CB-VPILev AS CHARACTER FORMAT "X(256)":U 
     LABEL "VPI leverandør" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","1"
     DROP-DOWN-LIST
     SIZE 55 BY 1 TOOLTIP "VPI leverandør er bestemmende for hvilken mappingtabell som brukes." NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-3 AT ROW 1.95 COL 78 
     BUTTON-2 AT ROW 3.62 COL 78 
     CB-VPILev AT ROW 3.86 COL 18 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 104.8 BY 5.95.


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
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "VPI Leverandør"
         HEIGHT             = 5.95
         WIDTH              = 105.8
         MAX-HEIGHT         = 48.95
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 48.95
         VIRTUAL-WIDTH      = 384
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 5.95
       FRAME DEFAULT-FRAME:WIDTH            = 104.8.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* VPI Leverandør */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* VPI Leverandør */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* VPI Leverandør */
DO:
    IF {&WINDOW-NAME}:WIDTH-PIXELS < iWidthPix THEN
          {&WINDOW-NAME}:WIDTH-PIXELS = iWidthPix.
  DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Neste >> */
DO:
   /*
   IF VALID-HANDLE(hdlParent) AND 
     CAN-DO(hdlParent:INTERNAL-ENTRIES,'SetContinue') THEN
     RUN SetContinue IN hdlParent (TRUE) NO-ERROR.
   APPLY "CLOSE" TO THIS-PROCEDURE.  */

   THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = TRUE. 
   RUN wVpiSelectImportColumns02.w NO-ERROR.
   IF ERROR-STATUS:ERROR THEN
   DO:
       IF RETURN-VALUE = "back" THEN
       DO:                        /*
           C-Win:HIDDEN = FALSE.*/ 
           ASSIGN CURRENT-WINDOW         = {&WINDOW-NAME} 
           THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.
           CURRENT-WINDOW:MOVE-TO-TOP().
           C-Win:HIDDEN = FALSE.
       END. 
       ELSE IF RETURN-VALUE = "cancel" THEN 
       DO:
           glCancelImport = true.  
           APPLY "CLOSE" TO THIS-PROCEDURE.
       END. 
   END.

   ELSE APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 C-Win
ON CHOOSE OF BUTTON-3 IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
    glCancelImport = true. 
    APPLY "CLOSE" TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-VPILev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-VPILev C-Win
ON VALUE-CHANGED OF CB-VPILev IN FRAME DEFAULT-FRAME /* VPI leverandør */
DO:
    IF VALID-HANDLE(hdlParent) THEN
    RUN SetVPILeverandor IN hdlParent (CB-VPILev:SCREEN-VALUE) NO-ERROR. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
/* Old menu code for center on top */ 
ASSIGN                                
    C-Win:X = ACTIVE-WINDOW:X + 300
    C-Win:Y = ACTIVE-WINDOW:Y + 160 NO-ERROR.

/* new menu for center based on desktop */ 
IF ERROR-STATUS:ERROR THEN
   ASSIGN {&WINDOW-NAME}:X = (SESSION:WIDTH-PIXELS  - {&WINDOW-NAME}:WIDTH-PIXELS) / 2
          {&WINDOW-NAME}:Y = (SESSION:HEIGHT-PIXELS - {&WINDOW-NAME}:HEIGHT-PIXELS) / 2. 


/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
DO:
   RUN disable_UI. 
END. 

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN InitializeObject. 
  RUN SetDivResize. 
  RUN enable_UI.
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").
  DYNAMIC-FUNCTION("setAttribute",THIS-PROCEDURE:CURRENT-WINDOW,"keepInitialSize","yes").
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.

END.

IF glCancelImport THEN RETURN ERROR "Cancel".

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
  DISPLAY CB-VPILev 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-3 BUTTON-2 CB-VPILev 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getFieldList C-Win 
PROCEDURE getFieldList :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opcFieldList AS CHAR NO-UNDO. 
    DEFINE OUTPUT PARAMETER opcFieldList2 AS CHAR NO-UNDO. 
    
    DEFINE VARIABLE cFieldList AS CHAR EXTENT 3 NO-UNDO. 
    DEFINE VARIABLE iCnt AS INT NO-UNDO. 
    DEFINE VARIABLE cFieldName AS CHAR NO-UNDO. 
    DEFINE VARIABLE cFieldLabel AS CHAR NO-UNDO. 
    DEFINE VARIABLE hPrisFil AS HANDLE NO-UNDO. 

    hPrisFil = BUFFER tt_prisfil:HANDLE.
    cFieldList = FILL(",",hPrisFil:NUM-FIELDS).
    
    DO iCnt = 1 TO hPrisFil:NUM-FIELDS:
        cFieldLabel = hPrisFil:BUFFER-FIELD(icnt):LABEL.
        cFieldName  = hPrisFil:BUFFER-FIELD(icnt):NAME.

        ENTRY(icnt,cFieldList[1]) = cFieldName.
        IF cFieldLabel NE "" THEN
          ENTRY(icnt,cFieldList[2]) = cFieldLabel.
        ELSE ENTRY(icnt,cFieldList[2]) = ENTRY(icnt,cFieldList[1]).
    END.       

    opcFieldList2 = cFieldList[1].
    opcFieldList  = cFieldList[2].
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getParentSourceProcedure C-Win 
PROCEDURE getParentSourceProcedure :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAMETER hdl AS HANDLE NO-UNDO. 
   hdl = hdlParent. 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getVPILeverandor C-Win 
PROCEDURE getVPILeverandor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE OUTPUT PARAMETER cOutput AS CHAR NO-UNDO. 
      DO WITH FRAME {&FRAME-NAME}:      

   cOutput = CB-VPILev:SCREEN-VALUE.
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
    DEFINE VARIABLE cFieldList AS CHAR NO-UNDO.
    DEFINE VARIABLE cFieldName AS CHAR NO-UNDO. 
    DEFINE VARIABLE iCnt AS INT NO-UNDO.
    
    DEFINE VARIABLE cVPILevLst AS CHAR NO-UNDO.
    DEFINE VARIABLE piLoop AS INT NO-UNDO. 
    DEFINE VARIABLE cTekst AS CHAR NO-UNDO. 
        
    /* Set VPI Leverandør CB -- Copy From wvpiPRS01.w*/ 
    {syspara.i 50 17 1 cVPILevLst}
    IF cVPILevLst = '' THEN
    cVPILevLst = '889'.
    
    DO piLoop = 1 TO NUM-ENTRIES(cVPILevLst):
    FIND EkstVPILev NO-LOCK WHERE
        EkstVPILev.EkstVPILevNr = INT(ENTRY(piLoop,cVPILevLst)) NO-ERROR.
    IF AVAILABLE EkstVPILev THEN
       cTekst = cTekst + STRING(EkstVPILev.EkstVPILevNr) + ' ' + EkstVPILev.KortNavn + ','  + STRING(EkstVPILev.EkstVPILevNr) + ','.
    END.
    cTekst = TRIM(cTekst,',').

   DO WITH FRAME DEFAULT-FRAME:
   ASSIGN
       CB-VPILev:LIST-ITEM-PAIRS = cTekst
       CB-VPILev:SCREEN-VALUE    = ENTRY(2,cTekst) NO-ERROR.   
   END. 

   IF VALID-HANDLE(hdlParent) AND 
      CAN-DO(hdlParent:INTERNAL-ENTRIES,'SetVPILeverandor') THEN
      RUN SetVPILeverandor IN hdlParent (CB-VPILev:SCREEN-VALUE) NO-ERROR. 


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDivResize C-Win 
PROCEDURE SetDivResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME DEFAULT-FRAME:HANDLE,
                                "RECT-71").


DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME DEFAULT-FRAME:HANDLE,
                                "RECT-71,BROWSE-TT_Error,BROWSE-TT_VareOrgInfo,Rect-1,Rect-2").


DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW, 
                               FRAME DEFAULT-FRAME:HANDLE,   
                        "FI-Filnamn,BUTTON-3,FI-Rigalnr,BUTTON-4,RECT-71,BROWSE-TT_VareOrgInfo").          



/* DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW, */
/*                                 FRAME DEFAULT-FRAME:HANDLE,   */
/*                                  "Btn_Ok,Btn_Help").          */
/* DYNAMIC-FUNCTION("SetResizeADM2panel",TRUE). */

*/
    DYNAMIC-FUNCTION("setnoMoveX",THIS-PROCEDURE:CURRENT-WINDOW, 
                                   FRAME DEFAULT-FRAME:HANDLE,   
                            "BROWSE-2,TEXT-1,CB-VPILev").          
    
    DYNAMIC-FUNCTION("setnoMovey",THIS-PROCEDURE:CURRENT-WINDOW, 
                                   FRAME DEFAULT-FRAME:HANDLE,   
                            "BROWSE-2,BUTTON-2,text-1,btnselectvalues,TEXT-1,CB-VPILev").          
    
    DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW, 
                            FRAME DEFAULT-FRAME:HANDLE,   
                            "BUTTON-2").          
    
    DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectToolbar").
    DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,iWidthPix,iHeightPix,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setSelectorAttributes C-Win 
PROCEDURE setSelectorAttributes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihSourceBrw   AS HANDLE NO-UNDO.
DEF INPUT PARAM ihTargetBrw   AS HANDLE NO-UNDO.

DEF VAR hSourceBuffer  AS HANDLE NO-UNDO.
DEF VAR hTargetBuffer  AS HANDLE NO-UNDO.
DEF VAR ix             AS INT    NO-UNDO.
DEF VAR cSelectedRows  AS CHAR   NO-UNDO.
DEF VAR cFieldList AS CHAR NO-UNDO.
hSourceBuffer = ihSourceBrw:QUERY:GET-BUFFER-HANDLE(1).
hTargetBuffer = ihTargetBrw:QUERY:GET-BUFFER-HANDLE(1).

DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"basequery","where true").
DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"uselocaldata","yes").
DYNAMIC-FUNCTION("setAttribute",ihTargetBrw,"uselocaldata","yes").

FOR EACH ttImportFields WHERE 
         ttImportFields.FieldSource = "File" AND 
         ttImportFields.FieldNumber NE 3 AND 
         ttImportFields.FieldLabel NE "" NO-LOCK :
    hSourceBuffer:BUFFER-CREATE().
    hSourceBuffer:BUFFER-COPY(BUFFER ttImportFields:HANDLE ).
END.

FOR EACH ttImportFields WHERE 
         ttImportFields.FieldSource = "db" AND 
         ttImportFields.FieldLabel NE ""  NO-LOCK:
    hTargetBuffer:BUFFER-CREATE().
    hTargetBuffer:BUFFER-COPY(BUFFER ttImportFields:HANDLE ).
    cSelectedRows = cSelectedRows + ttImportFields.RowIdent1 + ",".
END.


    /*
    /* Fill temp-table: */
    DO ix = 1 TO NUM-ENTRIES(cFieldList):
      hSourceBuffer:BUFFER-CREATE().
      ASSIGN hSourceBuffer:BUFFER-FIELD("FieldNumber"):BUFFER-VALUE = ix
             hSourceBuffer:BUFFER-FIELD("FieldName"):BUFFER-VALUE = ENTRY(ix,cFieldList)
             hSourceBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
             .
    
      /* If any records should be pre-selected: 
      IF ix > 6 THEN DO:
        hTargetBuffer:BUFFER-CREATE().
        ASSIGN hTargetBuffer:BUFFER-FIELD("FeltNummer"):BUFFER-VALUE = ix
               hTargetBuffer:BUFFER-FIELD("Beskrivelse"):BUFFER-VALUE = ENTRY(ix,cFieldList[2])
               hTargetBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = "rowid" + STRING(ix)
               cSelectedRows = cSelectedRows + "rowid" + STRING(ix) + ","
               .
      END.
      */
    END.
    */



    DYNAMIC-FUNCTION("setSelectedRowids" IN SOURCE-PROCEDURE,cSelectedRows).
    
    /* DYNAMIC-FUNCTION("setAttribute",ihSourceBrw,"filterdropdownvaluelist_Description","A|A|B|B").  */
    
    DYNAMIC-FUNCTION("setCurrentObject",ihSourceBrw).
    RUN OpenQuerySource IN SOURCE-PROCEDURE.
    
    DYNAMIC-FUNCTION("setCurrentObject",ihTargetBrw).
    RUN OpenQueryTarget IN SOURCE-PROCEDURE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

