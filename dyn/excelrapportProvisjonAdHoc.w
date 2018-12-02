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

DEFINE STREAM out-str. 

  DEFINE VARIABLE bOk         AS LOG    NO-UNDO.
  DEFINE VARIABLE ix          AS INT    NO-UNDO.
  DEFINE VARIABLE hBrowse     AS HANDLE NO-UNDO.
  DEFINE VARIABLE hQuery      AS HANDLE NO-UNDO.
  DEFINE VARIABLE hToolbar    AS HANDLE NO-UNDO.
  DEFINE VARIABLE hFieldMap   AS HANDLE NO-UNDO.
  DEFINE VARIABLE hParent     AS HANDLE NO-UNDO.

  DEFINE VARIABLE tth         AS HANDLE NO-UNDO.
  DEFINE VARIABLE giSysHid    AS INT    NO-UNDO.
  DEFINE VARIABLE giSysGr     AS INT    NO-UNDO.
  DEFINE VARIABLE bh          AS HANDLE NO-UNDO.
  DEFINE VARIABLE iInstance   AS INT    NO-UNDO.
  DEFINE VARIABLE cReportFileName AS CHAR   NO-UNDO.

  DEFINE TEMP-TABLE tt_prov NO-UNDO
      FIELD butik  AS INTE
      FIELD datum  AS DATE
      FIELD fsg    AS DECI
      FIELD reklam AS DECI
      FIELD ret_fr_andra AS DECI
      FIELD ret_i_andra  AS DECI
      INDEX dabu IS PRIMARY datum butik.


{windows.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS fDato tDato BtnSkriv btnfraDato btnTilDato 
&Scoped-Define DISPLAYED-OBJECTS fDato tDato 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFieldHandle C-Win 
FUNCTION getFieldHandle RETURNS HANDLE
  (INPUT icFieldName AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWeekNum C-Win 
FUNCTION getWeekNum RETURNS INTEGER
  ( INPUT idSomeDate     AS DATE,
    INPUT iiOutputLength AS INT)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnfraDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON BtnSkriv DEFAULT 
     LABEL "Skriv rapport" 
     SIZE 20 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON btnTilDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE VARIABLE fDato AS DATE FORMAT "99/99/99":U 
     LABEL "Fra Dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE tDato AS DATE FORMAT "99/99/99":U 
     LABEL "Til Dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     fDato AT ROW 2.67 COL 12 COLON-ALIGNED
     tDato AT ROW 3.86 COL 12 COLON-ALIGNED
     BtnSkriv AT ROW 6.48 COL 13
     btnfraDato AT ROW 2.67 COL 28 NO-TAB-STOP 
     btnTilDato AT ROW 3.86 COL 28 NO-TAB-STOP 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 62.4 BY 10.38.


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
         TITLE              = "Ad-Hoc Rapport"
         HEIGHT             = 7.71
         WIDTH              = 44.2
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 118.6
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 118.6
         RESIZE             = yes
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
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Ad-Hoc Rapport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Ad-Hoc Rapport */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnfraDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnfraDato C-Win
ON CHOOSE OF btnfraDato IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN Cal.w (fDato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnSkriv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnSkriv C-Win
ON CHOOSE OF BtnSkriv IN FRAME DEFAULT-FRAME /* Skriv rapport */
DO:
    RUN excelRapportProvisjon.p 
      (fDato:SCREEN-VALUE,
       tDato:SCREEN-VALUE,
       OUTPUT TABLE tt_prov). 

    /* RUN GenRapport (fDato:SCREEN-VALUE,tDato:SCREEN-VALUE). 
    TEMP-TABLE tt_prov:WRITE-XML('file','E:\slettme.xml').    */
    
    RUN SkrivAdHocRapport.
    APPLY 'close' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnTilDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnTilDato C-Win
ON CHOOSE OF btnTilDato IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN Cal.w (tDato:HANDLE).
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

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  
  hParent = SOURCE-PROCEDURE.
  RUN enable_UI.

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
  DISPLAY fDato tDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE fDato tDato BtnSkriv btnfraDato btnTilDato 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GenRapport C-Win 
PROCEDURE GenRapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* Internal testing procedure not used in production env 
   called excelrapportProvision.p */ 

DEFINE INPUT PARAMETER  dFraDag AS DATE NO-UNDO.
DEFINE INPUT PARAMETER  dTilDag AS DATE NO-UNDO.

DEFINE VARIABLE dDatumLoop AS DATE NO-UNDO.
  
    FOR EACH butiker NO-LOCK WHERE 
        CAN-FIND(FIRST kasse WHERE kasse.butikknr = butiker.butik AND 
                                   kasse.aktiv = TRUE):
    
        DO dDatumLoop = dFraDag TO dTilDag:
            CREATE tt_prov.
            ASSIGN tt_prov.butik = butiker.butik
                   tt_prov.datum = dDatumLoop.
            
            FIND Dags_Rap WHERE dags_rap.butikk = butiker.butik AND 
                                dags_rap.dato = dDatumLoop NO-LOCK NO-ERROR.
    
            IF AVAIL dags_rap THEN 
                ASSIGN tt_prov.fsg   = hg1_oms + hg2_oms + hg3_oms + hg4_oms + hg5_oms + hg6_oms + hg7_oms + hg8_oms + hg9_oms.
        END.
    END.
    
    
    FOR EACH butiker NO-LOCK:
        FOR EACH kasse WHERE kasse.butikknr = butiker.butik NO-LOCK:
    
            DO dDatumLoop = dFraDag TO dTilDag:
    
                FOR EACH bonghode WHERE bonghode.butikknr = butiker.butik  AND
                                        bonghode.gruppenr = kasse.gruppenr AND
                                        bonghode.kassenr  = kasse.kassenr  AND
                                        bonghode.dato     = dDatumLoop     AND
                                        bonghode.makulert <> 2             NO-LOCK:
                    FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id NO-LOCK.
                        IF bonglinje.makulert THEN
                            NEXT.
    
                        /* kundreklamation */
                        IF bonglinje.ttid = 3 THEN 
                        DO:
                            FIND tt_prov WHERE tt_prov.butik = butiker.butik AND
                                               tt_prov.datum = dDatumLoop.
    
                            ASSIGN tt_prov.reklam = tt_prov.reklam + BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab.
                                   tt_prov.fsg    = tt_prov.fsg - (BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab).
                        END.
                        /* returer */
                        ELSE IF bonglinje.ttid = 10 THEN 
                        DO:
                            IF bonglinje.ReturButikk > 0 AND bonglinje.ReturButikk <> bonglinje.butikknr THEN 
                            DO:
                                FIND tt_prov WHERE tt_prov.butik = butiker.butik AND
                                                   tt_prov.datum = dDatumLoop.
                                ASSIGN tt_prov.ret_fr_andra = tt_prov.ret_fr_andra + BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab.
                                FIND tt_prov WHERE tt_prov.butik = bonglinje.ReturButikk AND
                                                   tt_prov.datum = dDatumLoop NO-ERROR.
                                IF NOT AVAIL tt_prov THEN DO:
                                    CREATE tt_prov.
                                    ASSIGN tt_prov.butik = bonglinje.ReturButikk
                                           tt_prov.datum = dDatumLoop.
                                END.
                                ASSIGN tt_prov.ret_i_andra = tt_prov.ret_i_andra + BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab.
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.

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
    APPLY 'entry' TO fDato IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivAdHocRapport C-Win 
PROCEDURE SkrivAdHocRapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    cReportFileName = SESSION:TEMP-DIR + 
                     STRING(YEAR(TODAY),"9999") + 
                     STRING(MONTH(TODAY),"99") + 
                     STRING(DAY(TODAY),"99") + 
                     "_T" + STRING(TIME,"999999") + 
                     "Provisjon.xls".
    

    DEFINE VARIABLE hExcel AS COM-HANDLE NO-UNDO. 
    DEFINE VARIABLE lData AS LOGICAL INIT FALSE NO-UNDO. 
    
    OUTPUT STREAM out-str TO VALUE(cReportFileName).
    FOR EACH tt_Prov BY tt_prov.butik BY tt_prov.datum:
        PUT STREAM out-str 
            UNFORMATTED tt_prov.butik        CHR(9)
                        tt_prov.datum        CHR(9)
                        tt_prov.fsg          CHR(9)
                        tt_prov.reklam       CHR(9)
                        tt_prov.ret_fr_andra CHR(9)
                        tt_prov.ret_i_andra SKIP.
        lData = TRUE. 
    END.
    OUTPUT STREAM out-str CLOSE.

    FILE-INFO:FILENAME = cReportFileName.
    cReportFileName = FILE-INFO:FULL-PATHNAME.

    IF lData AND cReportFileName NE ? THEN
    DO:
        CREATE "Excel.Application" hExcel.
        hExcel:visible = FALSE.
        hExcel:Workbooks:OPEN(cReportFileName,1,FALSE,,,,,,2,,,,,,).
        hExcel:visible = TRUE. 
        RELEASE OBJECT hExcel NO-ERROR.
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WeekNum C-Win 
PROCEDURE WeekNum :
/************************************************************************************
        PROCEDURE: weeknum.p

        PURPOSE:   Calculates the week-number for a given date

        SYNTAX:    RUN samples/weeknum.p (INPUT in, OUTPUT out).

        REMARKS:   This code calculates the week-number for the date given.
                   The format is YYYYWW

        PARAMETERS:
            INPUT:  date
            OUTPUT: week number

        AUTHORS:   Judy Rothermal
        DATE:      February 1993

        LAST INSPECTED:
        INSPECTED BY:

 ************************************************************************************/
 /* Copyright(c) PROGRESS SOFTWARE CORPORATION, 1993 - All Rights Reserved.         */
 
/*Code_Start*/

/* Assumptions:                                                     */
/* 1. Weeks start on MONDAYS                                        */
/* 2. If January 1st falls on Friday, Saturday, Sunday or Monday    */
/*    then week 1 for this year will start on the first Monday      */
/*    the same year. If not, week 1 will start on the last Monday   */
/*    previous year.                                                */
/*    (In other words: At least 4 of the seven days of week 1 for   */
/*     a given year must fall into this year)                       */


  DEFINE INPUT  PARAMETER indate   AS DATE.  /* Input date , eg 10/17/90 */
  DEFINE OUTPUT PARAMETER yyyyww   AS INT.   /* Output week, eg 9042     */
  
  DEFINE VARIABLE yr   AS INT.  /* Year of indate, eg 1990      */
  DEFINE VARIABLE d1   AS INT.  /* Weekday of 1/1 current year, eg 2  */
                                /* (01/01/90 is a Monday)      */
  DEFINE VARIABLE dat1 AS DATE. /* Starting date of week 1     */
  DEFINE VARIABLE wn   AS INT.  /* Week number , eg 45         */
  
  ASSIGN
    yr   = YEAR(indate)
    d1   = WEEKDAY(DATE( 1 , 1 , yr))
    dat1 = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
                            DATE(1, 10, yr) - d1 )
    wn   = TRUNCATE((indate - dat1 + 7) / 7 , 0)
    yyyyww = yr * 100 + wn.
  
  IF wn < 1 THEN       /* Week 52 or 53 previous year ? */
  ASSIGN
    yr     = yr - 1
    d1     = WEEKDAY(DATE( 1 , 1 , yr))
    dat1   = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
                              DATE(1, 10, yr) - d1 )
    wn     = TRUNCATE((indate - dat1 + 7) / 7 , 0)
    yyyyww = yr * 100 + wn.
  
  ELSE IF wn > 52 THEN  /* Week 53 this year or week 1 next year ? */
  ASSIGN
    yr     = yr + 1
    d1     = WEEKDAY(DATE( 1 , 1 , yr))
    yyyyww = IF d1 EQ 6 OR d1 EQ 7 OR d1 EQ 1
                THEN (yr - 1) * 100 + 53 ELSE yr * 100 + 1.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFieldHandle C-Win 
FUNCTION getFieldHandle RETURNS HANDLE
  (INPUT icFieldName AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR hObj AS HANDLE NO-UNDO.

  hObj = FRAME {&FRAME-NAME}:HANDLE:FIRST-CHILD:FIRST-CHILD.

  DO WHILE VALID-HANDLE(hObj):
    IF hObj:NAME = icFieldName THEN RETURN hObj.
    hObj = hObj:NEXT-SIBLING.
  END.
  RETURN ?. 

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWeekNum C-Win 
FUNCTION getWeekNum RETURNS INTEGER
  ( INPUT idSomeDate     AS DATE,
    INPUT iiOutputLength AS INT) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iWeekNum AS INT NO-UNDO.
  
  RUN WeekNum (idSomeDate, OUTPUT iWeekNum).
  
  IF iWeekNum NE ? THEN
    CASE iiOutputLength:
      WHEN 2 THEN RETURN INT(SUBSTR(STRING(iWeekNum),5)).
      WHEN 4 THEN RETURN INT(SUBSTR(STRING(iWeekNum),3)).
      OTHERWISE RETURN iWeekNum.
    END CASE.
  ELSE RETURN 0.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

