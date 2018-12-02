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

DEFINE VARIABLE dFirstDate     AS DATE NO-UNDO.
DEFINE VARIABLE dLastDate      AS DATE NO-UNDO.
DEFINE VARIABLE dFirstDateW1   AS DATE NO-UNDO.
DEFINE VARIABLE dLastDateW5#   AS DATE NO-UNDO.
DEFINE VARIABLE iTotInnLev     AS INTEGER EXTENT 53 NO-UNDO.

DEFINE VAR cVindusTittel AS CHAR INIT "Innleveransestatistikk" NO-UNDO.

DEFINE TEMP-TABLE TT_InnLev
    FIELD Butik     LIKE Butiker.Butik
    FIELD ButInnlev AS INTE EXTENT 53
    INDEX ButMan Butik.
{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Rapport RS-Periode B-Exit TG-Jmf CB-Aar ~
CB-Fra CB-Til Btn_Help RECT-53 RECT-7 RECT-8 
&Scoped-Define DISPLAYED-OBJECTS RS-Periode TG-Jmf CB-Aar CB-Fra CB-Til 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getFirstDayW1 C-Win 
FUNCTION getFirstDayW1 RETURNS DATE
  ( INPUT ipDate AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Exit 
     IMAGE-UP FILE "icon/e-exit":U NO-FOCUS FLAT-BUTTON
     LABEL "Button 10" 
     SIZE 4.6 BY 1.1 TOOLTIP "Avslutt".

DEFINE BUTTON B-Rapport 
     IMAGE-UP FILE "icon/e-chart.bmp":U NO-FOCUS
     LABEL "Rapport" 
     SIZE 4.6 BY 1.1.

DEFINE BUTTON Btn_Help DEFAULT 
     IMAGE-UP FILE "icon/e-help":U NO-FOCUS FLAT-BUTTON
     LABEL "&Hjelp" 
     SIZE 4.6 BY 1.1 TOOLTIP "Hjelp"
     BGCOLOR 8 .

DEFINE VARIABLE CB-Aar AS CHARACTER FORMAT "X(4)":U 
     LABEL "År" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CB-AarTil AS CHARACTER FORMAT "X(4)":U 
     LABEL "Til år" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Fra AS CHARACTER FORMAT "X(256)":U 
     LABEL "Fra" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Til AS CHARACTER FORMAT "X(256)":U 
     LABEL "Til" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 16 BY 1 NO-UNDO.

DEFINE VARIABLE RS-Periode AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Måned", 1,
"Uke", 2
     SIZE 37 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65 BY 13.1.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65.8 BY .1.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 65.8 BY .1.

DEFINE VARIABLE TG-Jmf AS LOGICAL INITIAL no 
     LABEL "Årsstatistikk" 
     VIEW-AS TOGGLE-BOX
     SIZE 36.6 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Rapport AT ROW 1.29 COL 1.8
     RS-Periode AT ROW 5.52 COL 16.2 NO-LABEL
     B-Exit AT ROW 1.29 COL 62
     TG-Jmf AT ROW 7.1 COL 16.4
     CB-Aar AT ROW 8.86 COL 14 COLON-ALIGNED
     CB-AarTil AT ROW 8.86 COL 38.2 COLON-ALIGNED
     CB-Fra AT ROW 10 COL 14 COLON-ALIGNED
     CB-Til AT ROW 11.19 COL 14 COLON-ALIGNED
     Btn_Help AT ROW 1.29 COL 57
     RECT-53 AT ROW 2.67 COL 1.2
     RECT-7 AT ROW 1.1 COL 1
     RECT-8 AT ROW 2.43 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 65.8 BY 14.76.


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
         TITLE              = "Innleveransestatistikk"
         HEIGHT             = 14.76
         WIDTH              = 65.8
         MAX-HEIGHT         = 24.62
         MAX-WIDTH          = 143.4
         VIRTUAL-HEIGHT     = 24.62
         VIRTUAL-WIDTH      = 143.4
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
                                                                        */
/* SETTINGS FOR COMBO-BOX CB-AarTil IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       CB-AarTil:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Innleveransestatistikk */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Innleveransestatistikk */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Exit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Exit C-Win
ON CHOOSE OF B-Exit IN FRAME DEFAULT-FRAME /* Button 10 */
DO:
  apply "close":U to this-procedure.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapport C-Win
ON CHOOSE OF B-Rapport IN FRAME DEFAULT-FRAME /* Rapport */
DO:
     DEFINE VARIABLE dFraDato    AS DATE       NO-UNDO.
     DEFINE VARIABLE dTilDato    AS DATE       NO-UNDO.
     DEFINE VARIABLE dLoopDato   AS DATE       NO-UNDO.
     DEFINE VARIABLE cColValues  AS CHARACTER  NO-UNDO.
     DEFINE VARIABLE cTmpVerdier AS CHARACTER  NO-UNDO.
     DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
     DEFINE VARIABLE iWeekNum    AS INTEGER    NO-UNDO.

     DEFINE VAR cTitle        AS CHAR NO-UNDO.
     DEFINE VAR cRowLabel     AS CHAR NO-UNDO.
     DEFINE VAR cColLabel     AS CHAR NO-UNDO.
     DEFINE VAR cColEnable    AS CHAR NO-UNDO.      
     {sww.i}
     CASE RS-Periode:SCREEN-VALUE:
         WHEN "1" THEN DO:
             IF TG-Jmf:CHECKED THEN
                 ASSIGN dFraDato = DATE(INT(CB-Fra:SCREEN-VALUE),01,INT(CB-Aar:SCREEN-VALUE))
                        dTilDato = DATE(12,31,INT(CB-AartIL:SCREEN-VALUE)).
             ELSE
                 ASSIGN dFraDato = DATE(INT(CB-Fra:SCREEN-VALUE),01,INT(CB-Aar:SCREEN-VALUE))
                        dTilDato = IF CB-Til:SCREEN-VALUE = "12" THEN DATE(12,31,INT(CB-Aar:SCREEN-VALUE))
                            ELSE DATE(INT(CB-Til:SCREEN-VALUE) + 1,01,INT(CB-Aar:SCREEN-VALUE)) - 1.
         END.
         WHEN "2" THEN DO:
             ASSIGN dFraDato = dFirstDateW1 + (INT(CB-Fra:SCREEN-VALUE) - 1) * 7
                    dTilDato = dFirstDateW1 +  INT(CB-Til:SCREEN-VALUE) * 7 - 1.
         END.
     END CASE.
     ASSIGN iTotInnLev = 0.
     EMPTY TEMP-TABLE TT_InnLev NO-ERROR.
     IF ERROR-STATUS:ERROR THEN DO:
         FOR EACH TT_InnLev:
             DELETE TT_Innlev.
         END.
     END.
     IF RS-Periode:SCREEN-VALUE = "1" THEN
     DO dLoopDato = dFraDato TO dTilDato:
       FOR EACH BestLevert WHERE BestLevert.LevertDato = dLoopDato NO-LOCK:
         FIND BestHode OF BestLevert NO-LOCK NO-ERROR.
         IF NOT AVAIL BestHode THEN
             NEXT.
         FIND TT_InnLev WHERE TT_InnLev.Butik = IF TG-Jmf:CHECKED THEN YEAR(BestLevert.LevertDato) ELSE BestLevert.Butik NO-ERROR.
         IF NOT AVAIL TT_InnLev THEN DO:
           CREATE TT_InnLev.
           ASSIGN TT_InnLev.Butik = IF TG-Jmf:CHECKED THEN YEAR(BestLevert.LevertDato) ELSE BestLevert.Butik.
         END.
         ASSIGN iTotInnLev[MONTH(BestLevert.LevertDato)] = iTotInnLev[MONTH(BestLevert.LevertDato)] + 
                          (BestLevert.Levert * IF BestHode.BestStat <> 7 THEN 1 ELSE -1)
                TT_InnLev.ButInnlev[MONTH(BestLevert.LevertDato)] = TT_InnLev.ButInnlev[MONTH(BestLevert.LevertDato)] +
                          (BestLevert.Levert * IF BestHode.BestStat <> 7 THEN 1 ELSE -1).
       END.
     END.
     ELSE IF RS-Periode:SCREEN-VALUE = "2" THEN 
     DO dLoopDato = dFraDato TO dTilDato:
       FOR EACH BestLevert WHERE BestLevert.LevertDato = dLoopDato NO-LOCK:
         FIND BestHode OF BestLevert NO-LOCK NO-ERROR.
         IF NOT AVAIL BestHode THEN
             NEXT.
         FIND TT_InnLev WHERE TT_InnLev.Butik = BestLevert.Butik NO-ERROR.
         IF NOT AVAIL TT_InnLev THEN DO:
           CREATE TT_InnLev.
           ASSIGN TT_InnLev.Butik = BestLevert.Butik.
         END.
         ASSIGN iWeekNum = TRUNC((BestLevert.LevertDato - dFirstDateW1) / 7,0) + 1.
         ASSIGN iTotInnLev[iWeekNum] = iTotInnLev[iWeekNum] + 
                          (BestLevert.Levert * IF BestHode.BestStat <> 7 THEN 1 ELSE -1)
                TT_InnLev.ButInnlev[iWeekNum] = TT_InnLev.ButInnlev[iWeekNum] +
                          (BestLevert.Levert * IF BestHode.BestStat <> 7 THEN 1 ELSE -1).
       END.
     END.
     IF NOT TG-Jmf:CHECKED THEN DO:
       ASSIGN cColLabel = "SUM"
              cColEnable = "1".
       DO iCount = INT(CB-Fra:SCREEN-VALUE) TO INT(CB-Til:SCREEN-VALUE):
           ASSIGN cColValues  = cColValues + (IF cColValues  = "" THEN "" ELSE ",") + STRING(iTotInnLev[iCount]).
       END.
     END.
     DO iCount = INT(CB-Fra:SCREEN-VALUE) TO INT(CB-Til:SCREEN-VALUE):
         ASSIGN cRowLabel   = cRowLabel  + (IF cRowLabel   = "" THEN "" ELSE ",") + STRING(iCount,"99").
     END.
     FOR EACH TT_InnLev:
         ASSIGN cColLabel  = cColLabel  + (IF cColLabel  = "" THEN "" ELSE ",") + STRING(TT_InnLev.Butik)
                cColEnable = cColEnable + (IF cColEnable = "" THEN "" ELSE ",") + "1"
                cTmpVerdier = "".
         DO iCount = INT(CB-Fra:SCREEN-VALUE) TO INT(CB-Til:SCREEN-VALUE):
             cTmpVerdier = cTmpVerdier + (IF cTmpVerdier = "" THEN "" ELSE ",") + STRING(TT_InnLev.ButInnlev[iCount]).
         END.
         ASSIGN cColValues = cColValues + (IF cColValues = "" THEN "" ELSE ";") + cTmpVerdier.
     END.
     ASSIGN cTitle = (IF RS-Periode:SCREEN-VALUE = "1" THEN "Måned" ELSE "Uke") + " " + CB-Fra:SCREEN-VALUE + " - " + CB-Til:SCREEN-VALUE
                     + (IF TG-Jmf:CHECKED THEN "  ( År " + CB-Aar:SCREEN-VALUE + 
                          (IF CB-Aar:SCREEN-VALUE <> CB-AarTil:SCREEN-VALUE THEN " - " + CB-AarTil:SCREEN-VALUE ELSE "") + " )" 
                        ELSE "  ( Butikker " + CB-Aar:SCREEN-VALUE + " )").
     {swn.i}
            
     RUN d-mschart.w (cVindusTittel,cTitle,cRowLabel,cColLabel,cColValues,cColEnable).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help C-Win
ON CHOOSE OF Btn_Help IN FRAME DEFAULT-FRAME /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {winhlp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Aar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Aar C-Win
ON VALUE-CHANGED OF CB-Aar IN FRAME DEFAULT-FRAME /* År */
DO:
    DEFINE VARIABLE iAr AS INTEGER NO-UNDO.
    DEFINE VARIABLE cListItems AS CHARACTER  NO-UNDO.
    IF TG-Jmf:CHECKED THEN DO:
        DO iAr = INT(SELF:SCREEN-VALUE) TO YEAR(TODAY).
            ASSIGN cListItems = cListItems + (IF cListItems = "" THEN "" ELSE ",") + STRING(iAr).
        END.
        ASSIGN CB-AarTil:LIST-ITEMS = cListItems
               CB-AarTil:SCREEN-VALUE = ENTRY(1,cListItems).
    END.
    ELSE
        RUN PopulateCB_FraTil.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Fra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Fra C-Win
ON VALUE-CHANGED OF CB-Fra IN FRAME DEFAULT-FRAME /* Fra */
DO:
  RUN RePopulateCB_Til (SELF:SCREEN-VALUE,ENTRY(NUM-ENTRIES(SELF:LIST-ITEMS),SELF:LIST-ITEMS)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Periode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Periode C-Win
ON VALUE-CHANGED OF RS-Periode IN FRAME DEFAULT-FRAME
DO:
    RUN PopulateCB_FraTil.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-Jmf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-Jmf C-Win
ON VALUE-CHANGED OF TG-Jmf IN FRAME DEFAULT-FRAME /* Årsstatistikk */
DO:
  IF SELF:CHECKED THEN DO:
      ASSIGN RS-Periode:SCREEN-VALUE = "1".
      APPLY "VALUE-CHANGED" TO RS-Periode.
      ASSIGN CB-Aar:SCREEN-VALUE    = ENTRY(1,CB-Aar:LIST-ITEMS)
             CB-AarTil:LIST-ITEMS   = CB-Aar:LIST-ITEMS
             CB-AarTil:SCREEN-VALUE = ENTRY(1,CB-Aar:LIST-ITEMS)
             CB-AarTil:SENSITIVE    = TRUE
             CB-AarTil:HIDDEN       = FALSE
             CB-Fra:SENSITIVE       = FALSE
             CB-Til:SENSITIVE       = FALSE
             CB-Til:SCREEN-VALUE    = "12"
             RS-Periode:SENSITIVE   = FALSE.
  END.
  ELSE DO:
      ASSIGN CB-Aar:SCREEN-VALUE  = STRING(YEAR(TODAY))
             CB-AarTil:HIDDEN     = TRUE
             CB-Fra:SENSITIVE     = TRUE
             CB-Til:SENSITIVE     = TRUE
             RS-Periode:SENSITIVE = TRUE.
      APPLY "VALUE-CHANGED" TO RS-Periode.
  END.
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
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FOR FIRST bestlevert NO-LOCK BY BestLevert.LevertDato.
      ASSIGN dFirstDate = BestLevert.LevertDato.
  END.
  FOR LAST bestlevert NO-LOCK BY BestLevert.LevertDato.
      ASSIGN dLastDate = BestLevert.LevertDato.
  END.
  ASSIGN RS-Periode:SCREEN-VALUE = "1".  
  RUN  InitCB_Aar.
  RUN enable_UI.
  {lng.i} 
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
  DISPLAY RS-Periode TG-Jmf CB-Aar CB-Fra CB-Til 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE B-Rapport RS-Periode B-Exit TG-Jmf CB-Aar CB-Fra CB-Til Btn_Help 
         RECT-53 RECT-7 RECT-8 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB_Aar C-Win 
PROCEDURE InitCB_Aar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount     AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cListItems AS CHARACTER  NO-UNDO.

    IF dFirstDate <> ? AND dLastDate <> ? THEN
    DO iCount = YEAR(dFirstDate) TO YEAR(dLastDate):
        ASSIGN cListItems = cListItems + (IF cListItems = "" THEN "" ELSE ",") +
            STRING(iCount).
    END.
    ELSE
        ASSIGN cListItems = cListItems + (IF cListItems = "" THEN "" ELSE ",") +
            STRING(year(today)).

    DO WITH FRAME {&FRAME-NAME}:
    ASSIGN CB-Aar:LIST-ITEMS    = cListItems
           CB-Aar:SCREEN-VALUE  = STRING(YEAR(TODAY)).
/*            CB-AarTil:LIST-ITEMS = cListItems. */
    END.
    APPLY "VALUE-CHANGED" TO CB-Aar.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateCB_FraTil C-Win 
PROCEDURE PopulateCB_FraTil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListItems  AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iCount      AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLastWeek   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iLastMonth  AS INTEGER    NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        CASE RS-Periode:SCREEN-VALUE:
            WHEN "1" THEN DO: /* måned */
                ASSIGN iLastMonth = IF TG-Jmf:CHECKED THEN 12 ELSE
                       IF INT(CB-Aar:SCREEN-VALUE) = YEAR(TODAY) THEN MONTH(TODAY) ELSE 12.
                DO iCount = 1 TO iLastMonth:
                    ASSIGN cListItems = cListItems + (IF cListItems = "" THEN "" ELSE ",") +
                        STRING(iCount,"99").
                END.
            END.
            WHEN "2" THEN DO: /* uke */
                ASSIGN dFirstDateW1 = getFirstDayW1(DATE(01,01,INT(CB-Aar:SCREEN-VALUE))).
                IF INT(CB-Aar:SCREEN-VALUE) = YEAR(TODAY) THEN
                    ASSIGN dLastDateW5# = TODAY + IF WEEKDAY(TODAY) = 1 THEN 0 ELSE
                               8 - WEEKDAY(TODAY).
                ELSE 
                    ASSIGN dLastDateW5# = getFirstDayW1(DATE(01,01,INT(CB-Aar:SCREEN-VALUE) + 1)) - 1.
                RUN weeknum.p (dLastDateW5#,OUTPUT iLastWeek).
                ASSIGN iLastweek = INT(SUBSTR(STRING(iLastWeek),5,2)).
                DO iCount = 1 TO iLastweek:
                    ASSIGN cListItems = cListItems + (IF cListItems = "" THEN "" ELSE ",") +
                        STRING(iCount,"99").
                END.
            END.
        END CASE.
        ASSIGN CB-Fra:LIST-ITEMS   = cListItems
               CB-Til:LIST-ITEMS   = cListItems
               CB-Fra:SCREEN-VALUE = ENTRY(1,cListItems)
               CB-Til:SCREEN-VALUE = ENTRY(1,cListItems).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RePopulateCB_Til C-Win 
PROCEDURE RePopulateCB_Til :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcFra     AS CHARACTER  NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTil     AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE         cHuskTil   AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE         cListItems AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE         iCount     AS INTEGER    NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        IF ipcFra < CB-Til:SCREEN-VALUE THEN
            ASSIGN cHuskTil = CB-Til:SCREEN-VALUE.
        DO iCount = INT(ipcFra) TO INT(ipcTil):
            ASSIGN cListItems = cListItems + (IF cListItems = "" THEN "" ELSE ",") +
                      STRING(iCount,"99").
        END.
        ASSIGN CB-Til:LIST-ITEMS   = cListItems
               CB-Til:SCREEN-VALUE = IF cHuskTil = "" THEN ENTRY(1,cListItems)
                                         ELSE cHuskTil.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getFirstDayW1 C-Win 
FUNCTION getFirstDayW1 RETURNS DATE
  ( INPUT ipDate AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN IF WEEKDAY(ipDate) < 6 THEN ipDate - (WEEKDAY(ipDate) - 2)
                   ELSE ipDate + 9 - WEEKDAY(ipDate).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

