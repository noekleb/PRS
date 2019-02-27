&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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

DEF VAR bOk         AS LOG    NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR hBrowse     AS HANDLE NO-UNDO.
DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR hToolbar    AS HANDLE NO-UNDO.
DEF VAR hFieldMap   AS HANDLE NO-UNDO.
DEF VAR hParent     AS HANDLE NO-UNDO.

DEF VAR wlibhandle  AS HANDLE NO-UNDO.
DEF VAR cUserId     AS CHAR   NO-UNDO.
DEF VAR iBrGrpNr    AS INT    NO-UNDO.
DEF VAR iTellenr    AS INT    NO-UNDO.
DEF VAR lTypeTransliste      AS LOG    NO-UNDO.
DEF VAR ipOldTelleType AS INT NO-UNDO.
DEF VAR ipOld2TelleType AS INT NO-UNDO.

DEF VAR iDefaultButikkNr AS INT NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS btnCalFraDato Tellenr Beskrivelse ttid ~
startdato butikkliste Notat BtnOK BtnCancel TelleType 
&Scoped-Define DISPLAYED-OBJECTS Tellenr Beskrivelse ttid startdato ~
butikkliste Notat TelleType 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQuery C-Win 
FUNCTION getQuery RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCalFraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON BtnCancel AUTO-END-KEY DEFAULT 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BtnOK AUTO-GO DEFAULT 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE butikkliste AS CHARACTER FORMAT "X(256)":U 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE TelleType AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Telletype" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE ttid AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Trans.typer" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1",0
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE Notat AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 83 BY 6.91 NO-UNDO.

DEFINE VARIABLE Beskrivelse AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 69 BY 1 NO-UNDO.

DEFINE VARIABLE startdato AS DATE FORMAT "99/99/99":U 
     LABEL "Start dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Tellenr AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Tellenr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Bygg AS CHARACTER FORMAT "X(256)":U INITIAL "Bygging av telleliste pågår, vent......." 
      VIEW-AS TEXT 
     SIZE 41 BY .95
     BGCOLOR 10  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnCalFraDato AT ROW 4.33 COL 29.8
     Tellenr AT ROW 1.95 COL 13 COLON-ALIGNED
     Beskrivelse AT ROW 1.95 COL 27 COLON-ALIGNED NO-LABEL
     ttid AT ROW 3.14 COL 13 COLON-ALIGNED
     startdato AT ROW 4.33 COL 13 COLON-ALIGNED
     butikkliste AT ROW 5.52 COL 13 COLON-ALIGNED
     Notat AT ROW 8.38 COL 15 NO-LABEL
     BtnOK AT ROW 15.76 COL 2
     BtnCancel AT ROW 15.76 COL 85
     TelleType AT ROW 6.71 COL 13 COLON-ALIGNED
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SCROLLABLE SIZE 100.2 BY 16.05
         DEFAULT-BUTTON BtnOK CANCEL-BUTTON BtnCancel.

DEFINE FRAME FRAME-Bygg
     FI-Bygg AT ROW 1.95 COL 3 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 33.8 ROW 7.76
         SIZE 49 BY 2.76.


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
         TITLE              = "Detaljer telleliste"
         HEIGHT             = 16.05
         WIDTH              = 100.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 114.2
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 114.2
         MIN-BUTTON         = no
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
/* REPARENT FRAME */
ASSIGN FRAME FRAME-Bygg:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 16.05
       FRAME DEFAULT-FRAME:WIDTH            = 100.2.

/* SETTINGS FOR FRAME FRAME-Bygg
                                                                        */
/* SETTINGS FOR FILL-IN FI-Bygg IN FRAME FRAME-Bygg
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Detaljer telleliste */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Detaljer telleliste */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalFraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalFraDato C-Win
ON CHOOSE OF btnCalFraDato IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF Startdato
DO:
  RUN Cal.w (StartDato:HANDLE).
  IF StartDato:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnCancel C-Win
ON CHOOSE OF BtnCancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
   APPLY "CLOSE" TO THIS-PROCEDURE.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnOK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnOK C-Win
ON CHOOSE OF BtnOK IN FRAME DEFAULT-FRAME /* OK */
DO:
  DEF VAR hParentBrowse AS HANDLE NO-UNDO.
  
  IF Beskrivelse:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Angi beskrivelse"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO Beskrivelse.
      RETURN NO-APPLY.
  END.
  ASSIGN TelleType:SCREEN-VALUE IN FRAME DEFAULT-FRAME = STRING(ipOldTelleType).
  RUN InvokeMethod(hFieldMap,"SaveRecord").
  IF DYNAMIC-FUNCTION("getTransactionMessage") NE '' THEN
  DO:
/*     MESSAGE DYNAMIC-FUNCTION("getTransactionMessage") */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK.             */
    RETURN.
  END.

/*   hParentBrowse = DYNAMIC-FUNCTION("getParentBrowse" IN hParent).                                         */
/*   IF VALID-HANDLE(hParentBrowse) THEN                                                                     */
/*   DO:                                                                                                     */
/*     MESSAGE program-name(1)                                                                               */
/*        VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                 */
/*     IF DYNAMIC-FUNCTION("getObjectState",hFieldMap) = 'New' THEN                                          */
/*     DO:                                                                                                   */
/*       RUN InvokeMethod(hParentBrowse,"OpenQuery").                                                        */
/*       hParentBrowse:QUERY:REPOSITION-TO-ROWID(hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE) NO-ERROR. */
/*     END.                                                                                                  */
/*                                                                                                           */
/*     DYNAMIC-FUNCTION("RefreshRowids",hParentBrowse,hFieldMap:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).     */
/*                                                                                                           */
/*   END.    
                                                                                                  */
  IF ipOld2TelleType > 0 THEN
  DO:
      IF ipOld2TelleType = 1 THEN
          RUN FullListe IN hParent.

      IF ipOld2TelleType = 2 THEN
          RUN Rullerende IN hParent.

      IF ipOld2TelleType = 3 THEN
          RUN Kontroll IN hParent.

      IF ipOld2TelleType = 4 THEN
          RUN Lokasjon IN hParent.

      IF ipOld2TelleType = 5 THEN
          RUN Transaksjon IN hParent.
  END.
  
  IF lTypeTransliste =  TRUE THEN 
  DO:
      RUN FyllTransliste.
  END.
  bOK = TRUE.
  APPLY "CLOSE" TO THIS-PROCEDURE.
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
  DYNAMIC-FUNCTION("SetWindowSensitive" IN hParent,TRUE) NO-ERROR.
  RUN OppdaterSumRecord IN hParent NO-ERROR.
  DYNAMIC-FUNCTION("friskOppRad" IN hParent,bOK) NO-ERROR.
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
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
  RUN enable_UI.
  FRAME FRAME-Bygg:MOVE-TO-BOTTOM().
  hParent = SOURCE-PROCEDURE.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    RUN SUPER.
    DO WITH FRAME {&FRAME-NAME}:
        /*ED-FullTxt:HIDDEN = TelleType:SCREEN-VALUE <> "1".*/
    END.
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
  DISPLAY Tellenr Beskrivelse ttid startdato butikkliste Notat TelleType 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE btnCalFraDato Tellenr Beskrivelse ttid startdato butikkliste Notat 
         BtnOK BtnCancel TelleType 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY FI-Bygg 
      WITH FRAME FRAME-Bygg IN WINDOW C-Win.
  VIEW FRAME FRAME-Bygg IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Bygg}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FyllTransliste C-Win 
PROCEDURE FyllTransliste :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FRAME FRAME-Bygg:MOVE-TO-TOP().
  bOK = DYNAMIC-FUNCTION("RunProc","trans_to_tellelinje.p",TelleNr:SCREEN-VALUE IN FRAME DEFAULT-FRAME,?).
  FRAME FRAME-Bygg:MOVE-TO-BOTTOM().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
DEF VAR cButikkLst AS CHAR NO-UNDO.
DEF VAR iCl        AS INT  NO-UNDO.

iCl = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                       "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1").

DO WITH FRAME {&FRAME-NAME}:
    
  hQuery = DYNAMIC-FUNCTION("NewQuery",1,
                            "",
                            "tellehode"
                           + ";TelleNr"
                           + ";TTId"
                           + ";Beskrivelse"
                           + ";ButikkListe"
                           + ";Notat"
                           + ";PkSdlNr"
                           + ";OrdreNr"
                           + ";StartDato"
                           + ";TelleType"
                            ,"where false"
                            ,"").
  ASSIGN 
    ttid:DELIMITER = "|"
    ttid:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","TransType;Beskrivelse|ttid;ttid","WHERE CAN-DO('1,2,5,6,8,9,11',string(ttid))")
    ttid:SCREEN-VALUE    = ttid:ENTRY(ttid:LOOKUP('9'))
  .
  ASSIGN 
    iBrGrpNr         = INT(DYNAMIC-FUNCTION("getFieldValues","bruker","WHERE brukerid = " + QUOTER(DYNAMIC-FUNCTION("getASuserId")),"BrGrpNr"))
/*     iDefaultButikkNr = INT(DYNAMIC-FUNCTION("getFieldValues","bruker","WHERE brukerid = " + QUOTER(DYNAMIC-FUNCTION("getASuserId")),"ButikkNr")) */
/*     iDefaultButikkNr = IF iDefaultButikkNr = 0 THEN 1 ELSE iDefaultButikkNr                                                                      */
    iDefaultButikkNr    = int(DYNAMIC-FUNCTION("getButikkNr" IN hParent)).
  .
  ASSIGN
  cButikkLst = DYNAMIC-FUNCTION("getFieldList","butiker;butnamn|butik;butik,butikktilgang"
                                               ,"WHERE true, first butikktilgang where brgrpnr = " + STRING(iBrGrpNr)).
      
  IF cButikkLst = '' OR cButikkLst = ? THEN
  cButikkLst = DYNAMIC-FUNCTION("getFieldList","butiker;butnamn|butik;butik,butikktilgang"
                                               ,"WHERE true, first butikktilgang where true").
  /* Er bruker ikke satt opp mot butikk, skal sentrallager benyttes */
  IF cButikkLst = '' OR cButikkLst = ? THEN
  cButikkLst = DYNAMIC-FUNCTION("getFieldList","butiker;butnamn|butik;butik"
                                               ,"WHERE Butiker.Butik = " + STRING(iCl)).
  ASSIGN 
    butikkliste:DELIMITER = "|"
    butikkliste:LIST-ITEM-PAIRS = cButikkLst
    butikkliste:SCREEN-VALUE    = butikkliste:ENTRY(butikkliste:LOOKUP(STRING(iDefaultButikkNr)))
  .
  
  ASSIGN 
    TelleType:DELIMITER = ","
    TelleType:LIST-ITEM-PAIRS = 'Full telling,1,Rullerende telling,2,Kontrolltelling,3,Lokasjonsliste,4,Transaksjonsliste,5'
    TelleType:SCREEN-VALUE    = TelleType:ENTRY(TelleType:LOOKUP('1'))
  .
  
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap",
                             hQuery,                    
                             FRAME {&FRAME-NAME}:HANDLE,
                             "TelleNr,Beskrivelse,StartDato,ButikkListe,Notat",""
                             ,"TTId,TelleType",""
                             ,"").
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).

END.

DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,200,200,0,0).
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
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
APPLY 'entry' TO tellenr IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE myNewRecord C-Win 
PROCEDURE myNewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ipTelleType AS INT NO-UNDO.
  DEF INPUT PARAM iTTId       AS INT NO-UNDO.
  DEF INPUT PARAM iButikkNr   AS INT NO-UNDO.
  DEFINE VARIABLE cDatotidFra AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cDatotidTil AS CHARACTER   NO-UNDO.

  DEF VAR cTelleNr AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN ipOld2TelleType = ipTelleType.

    DYNAMIC-FUNCTION("setObjectState",hFieldMap,'New').
    RUN InvokeMethod(hFieldMap,"NewRecord").

    IF DYNAMIC-FUNCTION("runproc","get_tellenr.p",'',?) THEN.
    cTelleNr = DYNAMIC-FUNCTION("getTransactionMessage").

    ASSIGN
        TTId:SCREEN-VALUE        = STRING(iTTId)
        TelleNr:SCREEN-VALUE     = cTelleNr
        TelleNr:SENSITIVE        = FALSE
        StartDato:SCREEN-VALUE   = STRING(TODAY)
        StartDato:SENSITIVE      = FALSE
        butikkliste:SCREEN-VALUE = butikkliste:ENTRY(butikkliste:LOOKUP(STRING(iButikkNr)))
        butikkliste:SENSITIVE    = FALSE
        TelleType:SCREEN-VALUE   = TelleType:ENTRY(TelleType:LOOKUP(STRING(ipTelleType)))
        Beskrivelse:SCREEN-VALUE = ENTRY((ipTelleType * 2) - 1,TelleType:LIST-ITEM-PAIRS) + 
                                   ' ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + ' ' + USERID('skotex') 
        .


    IF ipTelleType = 5 THEN lTypeTransliste = TRUE.
    IF lTypeTransliste = TRUE THEN 
    DO:
        RUN dvelgperiode.w (OUTPUT cDatotidFra,OUTPUT cDatotidTil).
        IF RETURN-VALUE <> "OK" THEN DO:
            RETURN "AVBRYT".
        END.
    END.
    IF cDatotidFra <> "" THEN DO:
        ASSIGN Beskrivelse:SCREEN-VALUE = "Korreksjon transaksjoner"
               Beskrivelse:SENSITIVE = FALSE
               Notat:SCREEN-VALUE = IF cDatotidFra <> "" THEN cDatotidFra + "-" + cDatotidTil ELSE ""
               Notat:SENSITIVE = FALSE.
        APPLY "ENTRY" TO BtnOK.
    END.
    ELSE
        APPLY "ENTRY" TO Beskrivelse.
  
    IF can-do('4,5',STRING(ipTelletype))THEN 
        ipTelletype = 2.
    /* Rullerende og kontrolltelling skal håndteres som tellinng. */
    ELSE IF CAN-DO("1,2,3",STRING(ipTelletype)) THEN
        ipTelletype = 1.
    ASSIGN
        ipOldTelleType = ipTelleType.
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
  DEF VAR iTellenr AS INT NO-UNDO.
  
  IF DYNAMIC-FUNCTION("getObjectState",hFieldMap) = 'New' THEN 
  DO:
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","ttid,TelleType").
    DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues","9|" + STRING(TelleType:SCREEN-VALUE IN FRAME {&FRAME-NAME})).
  END.
  RUN SUPER.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQuery C-Win 
FUNCTION getQuery RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN hQuery.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

