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

DEF VAR ix           AS INT    NO-UNDO.
DEF VAR bOk          AS LOG    NO-UNDO.
DEF VAR hParent      AS HANDLE NO-UNDO.
DEF VAR hSourceProc  AS HANDLE NO-UNDO.
DEF VAR hBrowse      AS HANDLE NO-UNDO.
DEF VAR iPakkeIdx    AS INT    NO-UNDO.
DEF VAR httCalc      AS HANDLE NO-UNDO.
DEF VAR hBuffCalc    AS HANDLE NO-UNDO.
DEF VAR fKOrdre_id   AS DEC    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME frmPakkePris

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fiPakkePris fiSumOrdreEksMva ~
fiSumOrdreEksMva-2 btnVisKalkyle fiSumOrdreDbLabel fiSumOrdreDb%Label ~
fiSumRabattKrLabel fiSumOrdreLabel fiSumOrdreEksMvaLabel 
&Scoped-Define DISPLAYED-OBJECTS fiPakkePris fiSumOrdreDb fiSumOrdreDb% ~
fiSumRabattKr fiSumOrdre fiSumOrdreEksMva fiSumOrdreDb-2 fiSumOrdreDb%-2 ~
fiSumRabattKr-2 fiSumOrdre-2 fiSumOrdreEksMva-2 fiSumOrdreDbLabel ~
fiSumOrdreDb%Label fiSumRabattKrLabel fiSumOrdreLabel fiSumOrdreEksMvaLabel 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getPakkePris C-Win 
FUNCTION getPakkePris RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD VisKalkyle C-Win 
FUNCTION VisKalkyle RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnVisKalkyle 
     LABEL "Vis kalkyle" 
     SIZE 15.4 BY .91.

DEFINE VARIABLE fiPakkePris AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Ny pakkepris" 
     VIEW-AS FILL-IN 
     SIZE 15 BY 1 NO-UNDO.

DEFINE VARIABLE fiSumOrdre AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumOrdre-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumOrdreDb AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Gjeldende" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumOrdreDb% AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumOrdreDb%-2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 8.8 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumOrdreDb%Label AS CHARACTER FORMAT "X(256)":U INITIAL "Db%:" 
      VIEW-AS TEXT 
     SIZE 7.4 BY .62 NO-UNDO.

DEFINE VARIABLE fiSumOrdreDb-2 AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Ny" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumOrdreDbLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Sum DbKr:" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiSumOrdreEksMva AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumOrdreEksMva-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18.2 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumOrdreEksMvaLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Sum eks.mva:" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiSumOrdreLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Totalsum:" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.

DEFINE VARIABLE fiSumRabattKr AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumRabattKr-2 AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 17.8 BY .86 NO-UNDO.

DEFINE VARIABLE fiSumRabattKrLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Sum rabatt:" 
      VIEW-AS TEXT 
     SIZE 14 BY .62 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME frmPakkePris
     fiPakkePris AT ROW 2.05 COL 13.2 COLON-ALIGNED
     fiSumOrdreDb AT ROW 2.1 COL 40 COLON-ALIGNED NO-TAB-STOP 
     fiSumOrdreDb% AT ROW 2.1 COL 55.6 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiSumRabattKr AT ROW 2.1 COL 64.8 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiSumOrdre AT ROW 2.1 COL 83.2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiSumOrdreEksMva AT ROW 2.1 COL 101.8 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiSumOrdreDb-2 AT ROW 3.05 COL 40 COLON-ALIGNED NO-TAB-STOP 
     fiSumOrdreDb%-2 AT ROW 3.05 COL 55.6 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiSumRabattKr-2 AT ROW 3.05 COL 64.8 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiSumOrdre-2 AT ROW 3.05 COL 83.2 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     fiSumOrdreEksMva-2 AT ROW 3.05 COL 101.8 COLON-ALIGNED NO-LABEL NO-TAB-STOP 
     btnVisKalkyle AT ROW 3.1 COL 14.8
     fiSumOrdreDbLabel AT ROW 1.48 COL 40 COLON-ALIGNED NO-LABEL
     fiSumOrdreDb%Label AT ROW 1.48 COL 55.8 COLON-ALIGNED NO-LABEL
     fiSumRabattKrLabel AT ROW 1.48 COL 64.8 COLON-ALIGNED NO-LABEL
     fiSumOrdreLabel AT ROW 1.48 COL 83.2 COLON-ALIGNED NO-LABEL
     fiSumOrdreEksMvaLabel AT ROW 1.48 COL 102 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 121.8 BY 3.33.


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
         HEIGHT             = 3.48
         WIDTH              = 121.8
         MAX-HEIGHT         = 23.33
         MAX-WIDTH          = 192.6
         VIRTUAL-HEIGHT     = 23.33
         VIRTUAL-WIDTH      = 192.6
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




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME frmPakkePris
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN fiSumOrdre IN FRAME frmPakkePris
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSumOrdre-2 IN FRAME frmPakkePris
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSumOrdreDb IN FRAME frmPakkePris
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSumOrdreDb% IN FRAME frmPakkePris
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSumOrdreDb%-2 IN FRAME frmPakkePris
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSumOrdreDb-2 IN FRAME frmPakkePris
   NO-ENABLE                                                            */
ASSIGN 
       fiSumOrdreEksMva:READ-ONLY IN FRAME frmPakkePris        = TRUE.

ASSIGN 
       fiSumOrdreEksMva-2:READ-ONLY IN FRAME frmPakkePris        = TRUE.

/* SETTINGS FOR FILL-IN fiSumRabattKr IN FRAME frmPakkePris
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN fiSumRabattKr-2 IN FRAME frmPakkePris
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
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


&Scoped-define SELF-NAME btnVisKalkyle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVisKalkyle C-Win
ON CHOOSE OF btnVisKalkyle IN FRAME frmPakkePris /* Vis kalkyle */
DO:
  VisKalkyle().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME fiPakkePris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL fiPakkePris C-Win
ON RETURN OF fiPakkePris IN FRAME frmPakkePris /* Ny pakkepris */
DO:
  APPLY "choose" TO btnVisKalkyle.
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
  IF VALID-HANDLE(httCalc) THEN DO:
    DELETE OBJECT httCalc NO-ERROR.
    DELETE OBJECT hBuffCalc NO-ERROR.
  END.
  APPLY "close" TO hParent.
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

  DYNAMIC-FUNCTION("setNoMoveX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"fiSumOrdre,fiSumOrdre-2,fiSumOrdreDb,fiSumOrdreDb%,fiSumOrdreDb%-2,fiSumOrdreDb%Label,fiSumOrdreDb-2,fiSumOrdreDbLabel,fiSumOrdreEksMva,fiSumOrdreEksMva-2,fiSumOrdreEksMvaLabel,fiSumOrdreLabel,fiSumRabattKr,fiSumRabattKr-2,fiSumRabattKrLabel").
  DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"frmPakkePris").

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
  /* Hide all frames. */
  HIDE FRAME frmPakkePris.
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
  DISPLAY fiPakkePris fiSumOrdreDb fiSumOrdreDb% fiSumRabattKr fiSumOrdre 
          fiSumOrdreEksMva fiSumOrdreDb-2 fiSumOrdreDb%-2 fiSumRabattKr-2 
          fiSumOrdre-2 fiSumOrdreEksMva-2 fiSumOrdreDbLabel fiSumOrdreDb%Label 
          fiSumRabattKrLabel fiSumOrdreLabel fiSumOrdreEksMvaLabel 
      WITH FRAME frmPakkePris.
  ENABLE fiPakkePris fiSumOrdreEksMva fiSumOrdreEksMva-2 btnVisKalkyle 
         fiSumOrdreDbLabel fiSumOrdreDb%Label fiSumRabattKrLabel 
         fiSumOrdreLabel fiSumOrdreEksMvaLabel 
      WITH FRAME frmPakkePris.
  {&OPEN-BROWSERS-IN-QUERY-frmPakkePris}
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
DEF INPUT PARAM ihBrowse     AS HANDLE NO-UNDO.
DEF INPUT PARAM ihSourceProc AS HANDLE NO-UNDO.
DEF INPUT PARAM iiPakkeIdx   AS INT    NO-UNDO.
DEF INPUT PARAM ifKOrdre_id  AS DEC    NO-UNDO.

ASSIGN hBrowse     = ihBrowse
       hSourceProc = ihSourceProc
       iPakkeIdx   = iiPakkeIdx
       fKOrdre_id  = ifKOrdre_id
       .

VisKalkyle().

DYNAMIC-FUNCTION("setNoMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmPakkePris:HANDLE,"btnVisKalkyle,fiPakkePris,fiSumOrdre,fiSumOrdre-2,fiSumOrdreDb,fiSumOrdreDb%,fiSumOrdreDb%-2,fiSumOrdreDb%Label,fiSumOrdreDb-2,fiSumOrdreDbLabel,fiSumOrdreEksMva,fiSumOrdreEksMva-2,fiSumOrdreEksMvaLabel,fiSumOrdreLabel,fiSumRabattKr,fiSumRabattKr-2,fiSumRabattKrLabel").
DYNAMIC-FUNCTION("setAddMoveY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME frmPakkePris:HANDLE,"frmPakkePris").

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getPakkePris C-Win 
FUNCTION getPakkePris RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN fiPakkePris:SCREEN-VALUE IN FRAME {&FRAME-NAME}.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION VisKalkyle C-Win 
FUNCTION VisKalkyle RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cQueryStat AS CHAR   NO-UNDO.
DEF VAR hQuery     AS HANDLE NO-UNDO.
DEF VAR cRowIdList AS CHAR   NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  /* Viser først opprinnelig kalkyle for valgte poster: */
  cQueryStat = DYNAMIC-FUNCTION("getMyQueryStat" IN hSourceProc,hBrowse,"NettoLinjeSum,SumEksMvaKr,DbKr,LinjeRabattKr,OrdreRabattKr,KundeRabattKr","").
  
  DO ix = 2 TO NUM-ENTRIES(cQueryStat,";"):
    CASE ix:
      WHEN 2 THEN fiSumOrdre:SCREEN-VALUE    = ENTRY(2,ENTRY(ix,cQueryStat,";"),"|").
      WHEN 3 THEN fiSumOrdreEksMva:SCREEN-VALUE = ENTRY(2,ENTRY(ix,cQueryStat,";"),"|").
      WHEN 4 THEN fiSumOrdreDb:SCREEN-VALUE   = ENTRY(2,ENTRY(ix,cQueryStat,";"),"|").
      WHEN 5 THEN fiSumRabattKr:SCREEN-VALUE  = ENTRY(2,ENTRY(ix,cQueryStat,";"),"|").
      WHEN 6 THEN fiSumRabattKr:SCREEN-VALUE  = STRING(DEC(fiSumRabattKr:SCREEN-VALUE) + DEC(ENTRY(2,ENTRY(ix,cQueryStat,";"),"|"))).
      WHEN 7 THEN fiSumRabattKr:SCREEN-VALUE  = STRING(DEC(fiSumRabattKr:SCREEN-VALUE) + DEC(ENTRY(2,ENTRY(ix,cQueryStat,";"),"|"))).
    END CASE.
  END.
  fiSumOrdreDb%:SCREEN-VALUE  = STRING(DEC(fiSumOrdreDb:SCREEN-VALUE) / DEC(fiSumOrdreEksMva:SCREEN-VALUE) * 100).

  IF NOT hBrowse:QUERY:IS-OPEN THEN RETURN NO.

  /* Sender de valgte radene til serverprosedyre for prøvekalkulasjon: */
  hBrowse:QUERY:GET-FIRST().
  REPEAT WHILE NOT hBrowse:QUERY:QUERY-OFF-END:
    cRowIdList = cRowIdList + hBrowse:QUERY:GET-BUFFER-HANDLE(1):BUFFER-FIELD("RowIdent1"):BUFFER-VALUE + "|".
    hBrowse:QUERY:GET-NEXT().
  END.
  cRowIdList = TRIM(cRowIdList,"|").

  /* Resultatet av prøvekalkulasjonen returneres i en temp-tabell som må lages her hvis den ikke eksisterer: */
  IF NOT VALID-HANDLE(httCalc) THEN DO:
    CREATE TEMP-TABLE httCalc.
    httCalc:CREATE-LIKE(hBrowse:QUERY:GET-BUFFER-HANDLE(1)).
    httCalc:TEMP-TABLE-PREPARE("ttCalc").
    hBuffCalc = httCalc:DEFAULT-BUFFER-HANDLE.
  END.
  
  hBuffCalc:EMPTY-TEMP-TABLE().

  IF DEC(fiPakkePris:SCREEN-VALUE) = 0 THEN DO:
    ASSIGN fiSumOrdre-2:SCREEN-VALUE = "" 
           fiSumOrdreDb%-2:SCREEN-VALUE = "" 
           fiSumOrdreDb-2:SCREEN-VALUE = "" 
           fiSumOrdreEksMva-2:SCREEN-VALUE = "" 
           fiSumRabattKr-2:SCREEN-VALUE = "".
    RETURN NO.
  END.

  IF DYNAMIC-FUNCTION("runproc","kordrelinje_kalkpakke.p","test;" + fiPakkePris:SCREEN-VALUE + ";" + cRowIdList + ";1;" + STRING(iPakkeIdx) + ";" + STRING(fKOrdre_id),httCalc) THEN DO:

    DYNAMIC-FUNCTION("getRunProcReturnTable",hBuffCalc).
    hBuffCalc:FIND-FIRST().    /* <- må til pga noe som må være en bug i 4GL. Finner ellers ikke alle poster i neste query (getMyQueryStat) */
    cQueryStat = DYNAMIC-FUNCTION("getMyQueryStat" IN hSourceProc,hBuffCalc,"NettoLinjeSum,SumEksMvaKr,DbKr,LinjeRabattKr,OrdreRabattKr,KundeRabattKr","").
    DO ix = 2 TO NUM-ENTRIES(cQueryStat,";"):
      CASE ix:
        WHEN 2 THEN fiSumOrdre-2:SCREEN-VALUE    = ENTRY(2,ENTRY(ix,cQueryStat,";"),"|").
        WHEN 3 THEN fiSumOrdreEksMva-2:SCREEN-VALUE = ENTRY(2,ENTRY(ix,cQueryStat,";"),"|").
        WHEN 4 THEN fiSumOrdreDb-2:SCREEN-VALUE   = ENTRY(2,ENTRY(ix,cQueryStat,";"),"|").
        WHEN 5 THEN fiSumRabattKr-2:SCREEN-VALUE  = ENTRY(2,ENTRY(ix,cQueryStat,";"),"|").
        WHEN 6 THEN fiSumRabattKr-2:SCREEN-VALUE  = STRING(DEC(fiSumRabattKr-2:SCREEN-VALUE) + DEC(ENTRY(2,ENTRY(ix,cQueryStat,";"),"|"))).
        WHEN 7 THEN fiSumRabattKr-2:SCREEN-VALUE  = STRING(DEC(fiSumRabattKr-2:SCREEN-VALUE) + DEC(ENTRY(2,ENTRY(ix,cQueryStat,";"),"|"))).
      END CASE.
    END.
    fiSumOrdreDb%-2:SCREEN-VALUE  = STRING(DEC(fiSumOrdreDb-2:SCREEN-VALUE) / DEC(fiSumOrdreEksMva-2:SCREEN-VALUE) * 100).
  END.
  ELSE DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").

END.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

