&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress AppBuilder.       */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DEFINE VARIABLE ihDate AS HANDLE NO-UNDO.
&ELSE
  DEF INPUT PARAM ihDate AS HANDLE NO-UNDO.
&ENDIF

/* Local Variable Definitions ---                                       */
DEF VAR hParent AS HANDLE NO-UNDO.

DEFINE VARIABLE COLOR-Hilight      AS INTEGER NO-UNDO INITIAL 15.
DEFINE VARIABLE COLOR-Hilight-Text AS INTEGER NO-UNDO INITIAL 0.
DEFINE VARIABLE COLOR-Title        AS INTEGER NO-UNDO INITIAL 1.

DEFINE VARIABLE lResult  AS LOGICAL NO-UNDO.
DEFINE VARIABLE lInFrame AS LOGICAL NO-UNDO.

DEFINE VARIABLE CalendarDate AS DATE NO-UNDO INITIAL TODAY.
DEFINE VARIABLE iDay       AS INTEGER NO-UNDO.
DEFINE VARIABLE iMonth     AS INTEGER NO-UNDO.
DEFINE VARIABLE iYear      AS INTEGER NO-UNDO.
DEFINE VARIABLE iDayOfWeek AS INTEGER NO-UNDO.
DEFINE VARIABLE iFormatNo  AS INTEGER NO-UNDO INITIAL 8.

DEFINE VARIABLE cDateValue AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemList  AS CHARACTER NO-UNDO.

DEFINE VARIABLE hHandle    AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hLastDay   AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE hSelection AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE haViewDay  AS WIDGET-HANDLE NO-UNDO EXTENT 42.

DEFINE VARIABLE hContainer AS HANDLE NO-UNDO.

DEFINE VARIABLE iaViewDay AS INTEGER FORMAT "ZZ" INITIAL 0 EXTENT 42 NO-UNDO VIEW-AS TEXT.

DEFINE VARIABLE clDayName AS CHARACTER NO-UNDO EXTENT 7 INITIAL 
["Søndag","Mandag","Tirsdag","Onsdag","Torsdag","Fredag","Lørdag"].

DEFINE VARIABLE clMonthName AS CHARACTER NO-UNDO EXTENT 12 INITIAL 
["Januar","Februar","Mars","April","Mai","Juni","Juli",
 "August","September","Oktober","November","Desember"].


/* DEFINE VARIABLE clDayName AS CHARACTER NO-UNDO EXTENT 7 INITIAL           */
/* ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]. */
/*                                                                           */
/* DEFINE VARIABLE clMonthName AS CHARACTER NO-UNDO EXTENT 12 INITIAL        */
/* ["January","February","March","April","May","June","July",                */
/*  "August","September","October","November","December"].                   */

DEFINE VARIABLE clDayText AS CHARACTER NO-UNDO EXTENT 31 INITIAL 
["st","nd","rd","th","th","th","th","th","th","th",
 "th","th","th","th","th","th","th","th","th","th",
 "st","nd","rd","th","th","th","th","th","th","th",
 "st"
].

DEFINE VARIABLE x AS INTEGER NO-UNDO INITIAL 1.
DEFINE VARIABLE y AS INTEGER NO-UNDO.

DEFINE RECTANGLE RECT-S EDGE-PIXELS 1 SIZE-PIXELS 1 BY 1.

DEFINE BUTTON BUTTON-1 SIZE-PIXELS 1 BY 1.

/*
   Date Formats. Field codes that are not recognised appear literally in the 
   Resulting format (including blanks). You can add more formats as required or
   reduce the number of formats to speed things up a little. Format codes are
   as follows: 
   
   "fd"   FullDay (Monday, Tuesday, etc)
   "ad"   Abbreviated Day (Mon, Tue, etc)
   "fm"   Full Month (January, February, etc)
   "am"   Abbreviated Month (Jan, Feb, etc)
   "n"    Numeric day (1,2, etc)
   "nx"   Numeric day extended (1st, 2nd, etc)
   "d"    Single Digit day (1,2,7,30,)
   "dd"   Double Digit day (01, 02, 07, 30, etc)
   "m"    Single digit Month (1,2,7,12, etc)
   "mm"   Double digit Month (01,02,07,12, etc)
   "yy"   Decade Only
   "yyyy" Full Year
   */

&SCOPED-DEFINE MAX-EXTENTS 9 

/* This is the main array - At run-time, one of the formats below is copied
   into this array, based upon the SESSION date format. You can modify or
   add any series of formats that you like, or add your own selection logic.
*/
DEFINE VARIABLE caFormat AS CHARACTER NO-UNDO EXTENT {&MAX-EXTENTS} INITIAL
[  "fd, ,nx, ,fm, ,yyyy",
   ""
].

/* This is a smaller, European version */
DEFINE VARIABLE caEuroFormat AS CHARACTER NO-UNDO EXTENT {&MAX-EXTENTS} INITIAL
[  "fd, ,nx, ,fm, ,yyyy",    /* Monday 6th July 2000 */
   "fd, ,fm, ,nx, ,yyyy",    /* Monday July 6th 2000 */
   "fm, ,nx, ,yyyy",         /* July 6th 2000 */
   "nx, ,fm, ,yyyy",         /* 6th July 2000 */
   "d,/,m,/,yy",             /* 6/7/00 */
   "dd,/,mm,/,yy",           /* 6/7/00 */
   "d,/,m,/,yyyy",           /* 6/7/2000 */
   "dd,/,mm,/,yyyy",         /* 06/07/2000 */
   ""
].

/* A smaller American version */
DEFINE VARIABLE caUSAFormat AS CHARACTER NO-UNDO EXTENT {&MAX-EXTENTS} INITIAL
[  "fd, ,nx, ,fm, ,yyyy",    /* Monday 6th July 2000 */
   "fd, ,fm, ,nx, ,yyyy",    /* Monday July 6th 2000 */
   "fm, ,nx, ,yyyy",         /* July 6th 2000 */
   "nx, ,fm, ,yyyy",         /* 6th July 2000 */
   "m,/,d,/,yy",             /* 7/6/00 */
   "mm,/,dd,/,yy",           /* 07/06/00 */
   "m,/,d,/,yyyy",           /* 7/6/2000 */
   "mm,/,dd,/,yyyy",         /* 07/06/2000 */
   ""
].

/* This large list is commented out. Smaller versions are defined above.
   
DEFINE VARIABLE caFormat AS CHARACTER NO-UNDO EXTENT {&MAX-EXTENTS} INITIAL
[  "fd, ,nx, ,fm, ,yyyy",    /* Monday 6th July 2000 */
   "fd, ,n, ,fm, ,yyyy",     /* Monday 6 July 2000  */
   "fd, ,fm, ,nx, ,yyyy",    /* Monday July 6th 2000 */
   "fd, ,fm, ,n, ,yyyy",     /* Monday July 6 2000  */
   
   "ad, ,nx, ,fm, ,yyyy",    /* Mon 6th July 2000 */
   "ad, ,n, ,fm, ,yyyy",     /* Mon 6 July 2000  */
   "ad, ,fm, ,nx, ,yyyy",    /* Mon July 6th 2000 */
   "ad, ,fm, ,n, ,yyyy",     /* Mon July 6 2000  */
   
   "nx, ,fm, ,yyyy",        /* 6th July 2000 */
   "n, ,fm, ,yyyy",         /* 6 July 2000  */
   "fm, ,nx, ,yyyy",        /* July 6th 2000 */
   "fm, ,n, ,yyyy",         /* July 6 2000  */
   
   "nx, ,am, ,yyyy",        /* 6th Jul 2000 */
   "n, ,am, ,yyyy",         /* 6 Jul 2000  */
   "am, ,nx, ,yyyy",        /* Jul 6th 2000 */
   "am, ,n, ,yyyy",         /* Jul 6 2000  */
   
   "d,/,am,/,yyyy",         /* 6/Jul/2000  */
   "am,/,d,/,yyyy",         /* Jul/6/2000  */
   
   "d,-,am,-,yyyy",         /* 6-Jul-2000  */
   "am,-,d,-,yyyy",         /* Jul-6-2000  */
   
   "d,/,m,/,yy",          /* dd/mm/yy */
   "d,/,m,/,yyyy",        /* dd/mm/yyyy */
   "dd,/,mm,/,yy",        /* dd/mm/yy */
   "dd,/,mm,/,yyyy",      /* dd/mm/yyyy */
   
   "d,-,m,-,yy",          /* dd-mm-yy */
   "d,-,m,-,yyyy",        /* dd-mm-yyyy */
   "dd,-,mm,-,yy",        /* dd-mm-yy */
   "dd,-,mm,-,yyyy",      /* dd-mm-yyyy */
   
   "m,/,d,/,yy",          /* dd/mm/yy */
   "m,/,d,/,yyyy",        /* dd/mm/yyyy */
   "mm,/,dd,/,yy",        /* dd/mm/yy */
   "mm,/,dd,/,yyyy",      /* dd/mm/yyyy */
   
   "m,-,d,-,yy",          /* dd-mm-yy */
   "m,-,d,-,yyyy",        /* dd-mm-yyyy */
   "mm,-,dd,-,yy",        /* dd-mm-yy */
   "mm,-,dd,-,yyyy",      /* dd-mm-yyyy */
   
   ""
].
*/

FORM RECT-S AT ROW 1 COLUMN 1 BUTTON-1 SKIP(.1)
     SPACE iaViewDay[1 for 7] VIEW-AS TEXT SIZE 3.6 BY .63 SKIP(.3)
     SPACE iaViewDay[8 for 7] VIEW-AS TEXT SIZE 3.6 BY .63 SKIP(.3)
     SPACE iaViewDay[15 for 7] VIEW-AS TEXT SIZE 3.6 BY .63 SKIP(.3)
     SPACE iaViewDay[22 for 7] VIEW-AS TEXT SIZE 3.6 BY .63 SKIP(.3)
     SPACE iaViewDay[29 for 7] VIEW-AS TEXT SIZE 3.6 BY .63 SKIP(.3)
     SPACE iaViewDay[36 for 7] VIEW-AS TEXT SIZE 3.6 BY .63 
  WITH FRAME FRAME-A.
  ASSIGN RECT-S:HIDDEN = TRUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-44 CB-MONTH CB-YEAR fiWeek 
&Scoped-Define DISPLAYED-OBJECTS CB-MONTH CB-YEAR fiWeek TEXT-6 TEXT-7 ~
TEXT-1 TEXT-2 TEXT-3 TEXT-4 TEXT-5 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getWeekNumChar Dialog-Frame 
FUNCTION getWeekNumChar RETURNS CHARACTER
  ( INPUT dSomeDate AS DATE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD LocalTranslation Dialog-Frame 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE VARIABLE CB-FORMAT AS CHARACTER FORMAT "X(256)":U INITIAL "<Return Format>" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "<Return Format>" 
     DROP-DOWN-LIST
     SIZE 33.6 BY 1 NO-UNDO.

DEFINE VARIABLE CB-MONTH AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 12
     LIST-ITEMS "Januar","Februar","Mars","April","Mai","Juni","Juli","August","September","Oktober","November","Desember" 
     DROP-DOWN-LIST
     SIZE 24.2 BY 1 NO-UNDO.

DEFINE VARIABLE CB-YEAR AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 14
     DROP-DOWN-LIST
     SIZE 9.6 BY 1 NO-UNDO.

DEFINE VARIABLE fiWeek AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 34 BY 1 NO-UNDO.

DEFINE VARIABLE TEXT-1 AS CHARACTER FORMAT "X(256)":U INITIAL "O" 
      VIEW-AS TEXT 
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE TEXT-2 AS CHARACTER FORMAT "X(256)":U INITIAL "T" 
      VIEW-AS TEXT 
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE TEXT-3 AS CHARACTER FORMAT "X(256)":U INITIAL "F" 
      VIEW-AS TEXT 
     SIZE 4 BY .62 NO-UNDO.

DEFINE VARIABLE TEXT-4 AS CHARACTER FORMAT "X(256)":U INITIAL "L" 
      VIEW-AS TEXT 
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE TEXT-5 AS CHARACTER FORMAT "X(256)":U INITIAL "S" 
      VIEW-AS TEXT 
     SIZE 3 BY .62 NO-UNDO.

DEFINE VARIABLE TEXT-6 AS CHARACTER FORMAT "X(256)":U INITIAL "M" 
      VIEW-AS TEXT 
     SIZE 3.4 BY .62 NO-UNDO.

DEFINE VARIABLE TEXT-7 AS CHARACTER FORMAT "X(256)":U INITIAL "T" 
      VIEW-AS TEXT 
     SIZE 3 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-44
     EDGE-PIXELS 999  NO-FILL 
     SIZE 33.6 BY 7.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     CB-MONTH AT ROW 1.19 COL 1.8 NO-LABEL
     CB-YEAR AT ROW 1.19 COL 24 COLON-ALIGNED NO-LABEL
     fiWeek AT ROW 9.57 COL 1.8 NO-LABEL
     CB-FORMAT AT ROW 9.57 COL 1.8 NO-LABEL
     TEXT-6 AT ROW 2.62 COL 3.4 NO-LABEL
     TEXT-7 AT ROW 2.62 COL 7.6 NO-LABEL
     TEXT-1 AT ROW 2.62 COL 12.2 NO-LABEL
     TEXT-2 AT ROW 2.62 COL 16.6 NO-LABEL
     TEXT-3 AT ROW 2.62 COL 21.2 NO-LABEL
     TEXT-4 AT ROW 2.62 COL 26.2 NO-LABEL
     TEXT-5 AT ROW 2.62 COL 31 NO-LABEL
     RECT-44 AT ROW 2.38 COL 1.8
     SPACE(1.19) SKIP(1.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kalender".

DEFINE FRAME FRAME-A
    WITH 1 DOWN NO-BOX NO-HIDE KEEP-TAB-ORDER OVERLAY NO-HELP 
         NO-LABELS NO-UNDERLINE NO-VALIDATE 
         AT COL 2.2 ROW 3.43
         SIZE 32.8 BY 5.86
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME Dialog-Frame:HANDLE.

/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX CB-FORMAT IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE ALIGN-L                                         */
ASSIGN 
       CB-FORMAT:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR COMBO-BOX CB-MONTH IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN fiWeek IN FRAME Dialog-Frame
   ALIGN-L                                                              */
ASSIGN 
       fiWeek:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN TEXT-1 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN TEXT-2 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN TEXT-3 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN TEXT-4 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN TEXT-5 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN TEXT-6 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN TEXT-7 IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME FRAME-A
                                                                        */
ASSIGN 
       FRAME FRAME-A:SENSITIVE        = FALSE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-A
/* Query rebuild information for FRAME FRAME-A
     _Query            is NOT OPENED
*/  /* FRAME FRAME-A */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Kalender */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FRAME-A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A Dialog-Frame
ON ENTRY OF FRAME FRAME-A
ANYWHERE
DO:
  ASSIGN lInFrame = TRUE. 
  
  IF VALID-HANDLE(hLastDay) THEN 
      ASSIGN hLastDay:BGCOLOR = COLOR-Hilight
             hLastDay:FGCOLOR = COLOR-Hilight-Text.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A Dialog-Frame
ON LEAVE OF FRAME FRAME-A
DO:
  ASSIGN lInFrame = FALSE. 
  
  IF VALID-HANDLE(hLastDay) THEN 
      ASSIGN hLastDay:BGCOLOR = ?
             hLastDay:FGCOLOR = ?.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A Dialog-Frame
ON MOUSE-SELECT-DBLCLICK OF FRAME FRAME-A
ANYWHERE
DO:
  ihDate:SCREEN-VALUE = STRING(CalendarDate) NO-ERROR.
  APPLY "any-printable" TO ihDate.

  IF VALID-HANDLE(ihDate) THEN DO:
    ihDate:SCREEN-VALUE = STRING(CalendarDate).
    ihDate:MODIFIED = TRUE.
    APPLY "any-printable" TO ihDate.
    IF CAN-DO(hParent:INTERNAL-ENTRIES,"MyCalenderAction") THEN
      RUN MyCalenderAction IN hParent (ihDate:NAME).
    ELSE IF CAN-DO(hParent:INTERNAL-ENTRIES,"CalenderAction") THEN
      RUN CalenderAction IN hParent.
  END.

  APPLY "go" TO FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A Dialog-Frame
ON MOUSE-SELECT-DOWN OF FRAME FRAME-A
ANYWHERE
DO:
    IF SELF:TYPE = "FRAME":U THEN RETURN.
    
    /* Selects a day by giving it "focus". Focus is mimiced by making the
       rectangle surrounding the day a bit wider. The rectangle is moved
       over the selected day-text widget. The format string is updated to
       reflect the selected date. (The full list of formats is updated only
       when the user selects a new format).
    */
    IF SELF:NAME = "iaViewDay" AND SELF:SCREEN-VALUE <> ""
    THEN DO:
        IF VALID-HANDLE(hLastDay) 
        THEN 
            ASSIGN hLastDay:BGCOLOR = ?
                   hLastDay:FGCOLOR = ?. 
                   
        ASSIGN SELF:WIDTH-PIXELS = FONT-TABLE:GET-TEXT-WIDTH-PIXELS("99",4) + 1
               SELF:BGCOLOR = IF lInFrame THEN COLOR-Hilight ELSE ?
               SELF:FGCOLOR = IF lInFrame THEN COLOR-Hilight-Text ELSE ?
               hLastDay = SELF:HANDLE
               hSelection:HIDDEN = TRUE
               hSelection:WIDTH-PIXELS = SELF:WIDTH-PIXELS + 4
               hSelection:X = SELF:X - 2
               hSelection:Y = SELF:Y - 2
               hSelection:HIDDEN = FALSE
               iDay = INT(SELF:SCREEN-VALUE) 
               CalendarDate = DATE(iMonth,iDay,iYear)
               x = iFormatNo
               NO-ERROR.
        
        Run _BuildString.

        CB-FORMAT:REPLACE(cDateValue,iFormatNo) IN FRAME {&FRAME-NAME}.

    END.
END.

/* When focus is within frame-a, these triggers will move the day selection in the
   same way as clicking with a mouse pointer.
*/
ON "CURSOR-LEFT" OF FRAME FRAME-A ANYWHERE
DO: 
    Run _SelectDay(INT(hLastDay:SCREEN-VALUE) - 1).    
END.

ON "CURSOR-RIGHT"  OF FRAME FRAME-A ANYWHERE
DO: 
    Run _SelectDay(INT(hLastDay:SCREEN-VALUE) + 1).    
END.

ON "CURSOR-UP" OF FRAME FRAME-A ANYWHERE
DO: 
    Run _SelectDay(INT(hLastDay:SCREEN-VALUE) - 7).    
END.

ON "CURSOR-DOWN" OF FRAME FRAME-A ANYWHERE
DO: 
    Run _SelectDay(INT(hLastDay:SCREEN-VALUE) + 7).    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FRAME-A Dialog-Frame
ON RETURN OF FRAME FRAME-A
ANYWHERE
DO:
    RUN _AssignDay IN THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-FORMAT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-FORMAT Dialog-Frame
ON ENTRY OF CB-FORMAT IN FRAME Dialog-Frame
DO:
  RUN _SetFormat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-FORMAT Dialog-Frame
ON VALUE-CHANGED OF CB-FORMAT IN FRAME Dialog-Frame
DO:
  ASSIGN iFormatNo = LOOKUP(SELF:SCREEN-VALUE,SELF:LIST-ITEMS).
  RUN _SetFormat. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-MONTH
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-MONTH Dialog-Frame
ON VALUE-CHANGED OF CB-MONTH IN FRAME Dialog-Frame
OR VALUE-CHANGED OF CB-YEAR
DO:
  RUN _SetDay.

  RUN _SetFormat.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

{incl/frametrigg.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  hParent = SOURCE-PROCEDURE.
  RUN InitWindow.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
  HIDE FRAME FRAME-A.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  DISPLAY CB-MONTH CB-YEAR fiWeek TEXT-6 TEXT-7 TEXT-1 TEXT-2 TEXT-3 TEXT-4 
          TEXT-5 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-44 CB-MONTH CB-YEAR fiWeek 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
  VIEW FRAME FRAME-A.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  FRAME FRAME-A:SENSITIVE = NO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitWindow Dialog-Frame 
PROCEDURE InitWindow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR ix AS INT NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  LocalTranslation().
  /* Default the date format according to the session format */
  DO x = 1 TO {&MAX-EXTENTS}:
     ASSIGN caFormat[x] = IF SESSION:DATE-FORMAT = "mdy" 
                          THEN caUSAFormat[x]
                          ELSE caEuroFormat[x].
  END.
  DO ix = YEAR(TODAY) - 100 TO YEAR(TODAY) + 100:
    cb-year:ADD-LAST(STRING(ix)).
  END.
  ASSIGN 
     iYear  = YEAR(TODAY)
     iMonth = MONTH(TODAY)
     iDay   = DAY(TODAY) 
     hSelection = RECT-S:HANDLE IN FRAME FRAME-A
     hSelection:WIDTH-PIXELS  = iaViewDay[1]:WIDTH-PIXELS + 4
     hSelection:HEIGHT-PIXELS = iaViewDay[1]:HEIGHT-PIXELS + 4
     CalendarDate = TODAY
     cb-year:SCREEN-VALUE     = STRING(YEAR(TODAY))
/*      CB-YEAR:LIST-ITEMS IN FRAME {&FRAME-NAME} = STRING(YEAR(TODAY) - 20) */
/*      COLOR-Hilight = COLOR-OF("Hilight":U)           */
/*      COLOR-Hilight-Text = COLOR-OF("HilightText":U)  */
/*      COLOR-Title = COLOR-OF("ActiveTitle":U)         */
/*      RECT-99:BGCOLOR = COLOR-Title */
/*      TEXT-1:BGCOLOR = COLOR-Title  */
/*      TEXT-2:BGCOLOR = COLOR-Title  */
/*      TEXT-3:BGCOLOR = COLOR-Title  */
/*      TEXT-4:BGCOLOR = COLOR-Title  */
/*      TEXT-5:BGCOLOR = COLOR-Title  */
/*      TEXT-6:BGCOLOR = COLOR-Title  */
/*      TEXT-7:BGCOLOR = COLOR-Title  */
     TEXT-1:FGCOLOR = COLOR-Hilight-Text
     TEXT-2:FGCOLOR = COLOR-Hilight-Text
     TEXT-3:FGCOLOR = COLOR-Hilight-Text
     TEXT-4:FGCOLOR = COLOR-Hilight-Text
     TEXT-5:FGCOLOR = COLOR-Hilight-Text
     TEXT-6:FGCOLOR = COLOR-Hilight-Text
     TEXT-7:FGCOLOR = COLOR-Hilight-Text
/*      CB-YEAR:BGCOLOR = COLOR-Hilight-Text  */
  NO-ERROR.
  
  RUN _SetHandles.
     
  RUN setNewDate.
  
  ENABLE ALL WITH FRAME FRAME-A.
  
  APPLY "LEAVE" TO FRAME FRAME-A.

  DYNAMIC-FUNCTION("InitTranslation",FRAME {&FRAME-NAME}:HANDLE).

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setFieldHandle Dialog-Frame 
PROCEDURE setFieldHandle :
/*------------------------------------------------------------------------------
  Purpose:     Set handle for update widget.
  Parameters:  Widget Handle
  Notes:       After a valid handle is passed, updates the widget screen value
               with the selected date, whenever the date changes.
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ip-handle AS HANDLE NO-UNDO.

IF VALID-HANDLE(ip-handle) 
AND CAN-SET(ip-handle,"SCREEN-VALUE":U) 
AND CAN-DO("CHARACTER,DATE",ip-handle:DATA-TYPE) 
THEN DO:
    ASSIGN hHandle = ip-handle 
    
           CalendarDate = DATE(hHandle:SCREEN-VALUE) NO-ERROR.
           
    IF CalendarDate = ? THEN CalendarDate = TODAY.
    
    ELSE RUN setNewDate.
END.    

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE setNewDate Dialog-Frame 
PROCEDURE setNewDate :
/*------------------------------------------------------------------------------
  Purpose:     Initialise to a new date
  Parameters:  <none>
  Notes:       CalendarDate holds the date.
------------------------------------------------------------------------------*/

  ASSIGN CB-YEAR = STRING(YEAR(CalendarDate)) 
         CB-YEAR:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(YEAR(CalendarDate))
         CB-MONTH:SCREEN-VALUE = ENTRY(MONTH(CalendarDate),CB-MONTH:LIST-ITEMS)
         iDay = DAY(CalendarDate).

  APPLY "VALUE-CHANGED" TO CB-MONTH IN FRAME {&FRAME-NAME}.

/*   RUN _SetFormat.  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE viewObject Dialog-Frame 
PROCEDURE viewObject :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       The frame containing the days is hidden first, and viewed when
               the main frame is in view. This makes the redisplay much cleaner.
------------------------------------------------------------------------------*/
  
  /* Code placed here will execute PRIOR to standard behavior. */

  ASSIGN FRAME FRAME-A:HIDDEN = TRUE.
  MESSAGE PROGRAM-NAME(1) SKIP 
           
          VIEW-AS ALERT-BOX.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _AssignDay Dialog-Frame 
PROCEDURE _AssignDay PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Set the Linked Field Date, if specified
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF VALID-HANDLE(hHandle) AND hHandle:SENSITIVE 
THEN DO:
    
    CASE hHandle:TYPE:
    
        WHEN "DATE"
        THEN
            ASSIGN hHandle:SCREEN-VALUE = STRING(CalendarDate,hHandle:FORMAT) NO-ERROR.
        
        WHEN "EDITOR"
        THEN DO:
            IF hHandle:TEXT-SELECTED
            THEN
                hHandle:REPLACE-SELECTION-TEXT(cDateValue).
            ELSE
                hHandle:INSERT-STRING(cDateValue).
        END.
        
        OTHERWISE            
            ASSIGN hHandle:SCREEN-VALUE = cDateValue NO-ERROR.
    
    END CASE.
    
    APPLY "VALUE-CHANGED" TO hHandle.           
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _BuildString Dialog-Frame 
PROCEDURE _BuildString PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Build date string from format entry (current value x)
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN cDateValue = "".

DO y = 1 TO NUM-ENTRIES(caFormat[x]):
    CASE ENTRY(y,caFormat[x]):
        WHEN "fd"   THEN ASSIGN cDateValue = cDateValue + clDayName[WEEKDAY(CalendarDate)].
        WHEN "ad"   THEN ASSIGN cDateValue = cDateValue + SUBSTRING(clDayName[WEEKDAY(CalendarDate)],1,3).
        WHEN "fm"   THEN ASSIGN cDateValue = cDateValue + clMonthName[iMonth].
        WHEN "am"   THEN ASSIGN cDateValue = cDateValue + SUBSTRING(clMonthName[iMonth],1,3).
        WHEN "n"    THEN ASSIGN cDateValue = cDateValue + TRIM(STRING(iDay,"Z9")).
        WHEN "nx"   THEN ASSIGN cDateValue = cDateValue + TRIM(STRING(iDay,"Z9")) + clDayText[iDay].
        WHEN "d"    THEN ASSIGN cDateValue = cDateValue + TRIM(STRING(iDay,"Z9")).
        WHEN "dd"   THEN ASSIGN cDateValue = cDateValue + STRING(iDay,"99").
        WHEN "m"    THEN ASSIGN cDateValue = cDateValue + TRIM(STRING(iMonth,"Z9")).
        WHEN "mm"   THEN ASSIGN cDateValue = cDateValue + STRING(iMonth,"99").
        WHEN "yy"   THEN ASSIGN cDateValue = cDateValue + SUBSTRING(STRING(iYear,"9999"),3,2).
        WHEN "yyyy" THEN ASSIGN cDateValue = cDateValue + STRING(iYear,"9999").
        OTHERWISE ASSIGN cDateValue = cDateValue + ENTRY(y,caFormat[x]).
    END CASE.
END.

cDateValue = cDateValue + getWeekNumChar(CalendarDate).
        
fiWeek:SCREEN-VALUE IN FRAME {&FRAME-NAME} = cDateValue.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _SelectDay Dialog-Frame 
PROCEDURE _SelectDay PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Select the appropriate array element for a particular day.
  Parameters:  <none>
  Notes:       Scans each array element looking for a match.
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER sel_day AS INTEGER NO-UNDO.

DO x = 1 to 42:
    IF VALID-HANDLE(haViewDay[x]) AND INT(haViewDay[x]:SCREEN-VALUE) = sel_day 
    THEN DO:
        APPLY "MOUSE-SELECT-DOWN" TO haViewDay[x].
        LEAVE.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _SetDay Dialog-Frame 
PROCEDURE _SetDay PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     After choosing a new date, this assigns days into array elements
               so that they appear under the correct weekday.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN iYear  = INT(CB-YEAR:SCREEN-VALUE IN FRAME {&FRAME-NAME})
         iMonth = LOOKUP(CB-MONTH:SCREEN-VALUE,CB-MONTH:LIST-ITEMS)
         iDay   = IF iDay = 0 THEN DAY(TODAY) ELSE iDay
         CalendarDate  = DATE(iMonth,1,iYear)
         iaViewDay = 0 
         
         /* Progress returns 1-7 (Sun->Sat)from the WEEKDAY function. However, the
            week is formatted in Sat->Fri on screen (Saturday has been moved to the
            front). So we handle that difference here.
         */
         iDayOfWeek = WEEKDAY(CalendarDate) - 1
/*            iDayOfWeek = IF WEEKDAY(CalendarDate) = 7 THEN 0 ELSE WEEKDAY(CalendarDate) - 1 */
/*            iDayOfWeek = IF WEEKDAY(CalendarDate) = 7 THEN 1 ELSE WEEKDAY(CalendarDate) + 1 */
         
         NO-ERROR.

  /* Starting from day 1, we just keep adding 1 until the month changes */
  DO X = 1 TO 31:
    IF iDayOfWeek LE 0 THEN iDayOfWeek = 7 - iDayOfWeek.
    ASSIGN iaViewDay[iDayOfWeek] = X
           iDayOfWeek = iDayOfWeek + 1 
           CalendarDate = CalendarDate + 1.
    IF MONTH(CalendarDate) <> iMonth THEN LEAVE.
  END.
  
  /* Ensure iDay <= number of days in the month */ 
  IF iDay > x THEN iDay = x.
  
  DISPLAY iaViewDay WITH FRAME FRAME-A. 
  
  Run _SelectDay(iDay).
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _SetFormat Dialog-Frame 
PROCEDURE _SetFormat PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Build All Format Lists
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

ASSIGN cItemList = "". 

DO x = 1 TO {&MAX-EXTENTS}:
    
    IF caFormat[x] = "" THEN LEAVE.
    
    Run _BuildString.
  
    ASSIGN cItemList = cItemList + IF cItemList = "" THEN cDateValue ELSE "," + cDateValue. 
END.

ASSIGN CB-FORMAT:LIST-ITEMS IN FRAME {&FRAME-NAME} = cItemList
       CB-FORMAT:SCREEN-VALUE = ENTRY(iFormatNo,CB-FORMAT:LIST-ITEMS). 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE _SetHandles Dialog-Frame 
PROCEDURE _SetHandles PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Moves handles of static "day" widgets into array
  Parameters:  <none>
  Notes:       For Static array widgets, the expression 
                  widget-name[n]:<attribute|method>
               is only valid when "n" is a numeric constant. e.g. not a variable.
               This assignment process means that we can process the array easily
               in other parts of this procedure.
------------------------------------------------------------------------------*/
ASSIGN haViewDay[1] = iaViewDay[1]:HANDLE IN FRAME FRAME-A
       haViewDay[2] = iaViewDay[2]:HANDLE
       haViewDay[3] = iaViewDay[3]:HANDLE
       haViewDay[4] = iaViewDay[4]:HANDLE
       haViewDay[5] = iaViewDay[5]:HANDLE
       haViewDay[6] = iaViewDay[6]:HANDLE
       haViewDay[7] = iaViewDay[7]:HANDLE
       haViewDay[8] = iaViewDay[8]:HANDLE
       haViewDay[9] = iaViewDay[9]:HANDLE
       haViewDay[10] = iaViewDay[10]:HANDLE
       haViewDay[11] = iaViewDay[11]:HANDLE
       haViewDay[12] = iaViewDay[12]:HANDLE
       haViewDay[13] = iaViewDay[13]:HANDLE
       haViewDay[14] = iaViewDay[14]:HANDLE
       haViewDay[15] = iaViewDay[15]:HANDLE
       haViewDay[16] = iaViewDay[16]:HANDLE
       haViewDay[17] = iaViewDay[17]:HANDLE
       haViewDay[18] = iaViewDay[18]:HANDLE
       haViewDay[19] = iaViewDay[19]:HANDLE
       haViewDay[20] = iaViewDay[20]:HANDLE
       haViewDay[21] = iaViewDay[21]:HANDLE
       haViewDay[22] = iaViewDay[22]:HANDLE
       haViewDay[23] = iaViewDay[23]:HANDLE
       haViewDay[24] = iaViewDay[24]:HANDLE
       haViewDay[25] = iaViewDay[25]:HANDLE
       haViewDay[26] = iaViewDay[26]:HANDLE
       haViewDay[27] = iaViewDay[27]:HANDLE
       haViewDay[28] = iaViewDay[28]:HANDLE
       haViewDay[29] = iaViewDay[29]:HANDLE
       haViewDay[30] = iaViewDay[30]:HANDLE
       haViewDay[31] = iaViewDay[31]:HANDLE
       haViewDay[32] = iaViewDay[32]:HANDLE
       haViewDay[33] = iaViewDay[33]:HANDLE
       haViewDay[34] = iaViewDay[34]:HANDLE
       haViewDay[35] = iaViewDay[35]:HANDLE
       haViewDay[36] = iaViewDay[36]:HANDLE
       haViewDay[37] = iaViewDay[37]:HANDLE
       haViewDay[38] = iaViewDay[38]:HANDLE
       haViewDay[39] = iaViewDay[39]:HANDLE
       haViewDay[40] = iaViewDay[40]:HANDLE
       haViewDay[41] = iaViewDay[41]:HANDLE
       haViewDay[42] = iaViewDay[42]:HANDLE.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getWeekNumChar Dialog-Frame 
FUNCTION getWeekNumChar RETURNS CHARACTER
  ( INPUT dSomeDate AS DATE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VAR yyyyww    AS INT NO-UNDO.   /* Output week, eg 9042     */

DEFINE VARIABLE yr   AS INT NO-UNDO.  /* Year of dSomeDate, eg 1990      */
DEFINE VARIABLE d1   AS INT NO-UNDO.  /* Weekday of 1/1 current year, eg 2  */
                              /* (01/01/90 is a Monday)      */
DEFINE VARIABLE dat1 AS DATE NO-UNDO. /* Starting date of week 1     */
DEFINE VARIABLE wn   AS INT NO-UNDO.  /* Week number , eg 45         */

ASSIGN
  yr   = YEAR(dSomeDate)
  d1   = WEEKDAY(DATE( 1 , 1 , yr))
  dat1 = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
                          DATE(1, 10, yr) - d1 )
  wn   = TRUNCATE((dSomeDate - dat1 + 7) / 7 , 0)
  yyyyww = yr * 100 + wn.

IF wn < 1 THEN       /* Week 52 or 53 previous year ? */
ASSIGN
  yr     = yr - 1
  d1     = WEEKDAY(DATE( 1 , 1 , yr))
  dat1   = (IF d1 LE 5 THEN DATE(1,  3, yr) - d1 ELSE
                            DATE(1, 10, yr) - d1 )
  wn     = TRUNCATE((dSomeDate - dat1 + 7) / 7 , 0)
  yyyyww = yr * 100 + wn.

ELSE IF wn > 52 THEN  /* Week 53 this year or week 1 next year ? */
ASSIGN
  yr     = yr + 1
  d1     = WEEKDAY(DATE( 1 , 1 , yr))
  yyyyww = IF d1 EQ 6 OR d1 EQ 7 OR d1 EQ 1
              THEN (yr - 1) * 100 + 53 ELSE yr * 100 + 1.

RETURN (IF DYNAMIC-FUNCTION("Scandinavian") THEN " - uke " ELSE " - week ") + 
       SUBSTR(STRING(yyyyww),5).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION LocalTranslation Dialog-Frame 
FUNCTION LocalTranslation RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose: Set english labels
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
  IF DYNAMIC-FUNCTION("Scandinavian") THEN RETURN FALSE.
  ELSE 
    ASSIGN CB-MONTH:LIST-ITEMS      = "January,February,March,April,May,June,July,August,September,October,November,December" 
           CB-FORMAT:LIST-ITEMS     = REPLACE(CB-FORMAT:LIST-ITEMS,"uke","week")
           TEXT-1:SCREEN-VALUE      = "W" 
           TEXT-4:SCREEN-VALUE      = "S" 
           FRAME Dialog-Frame:TITLE = "Calendar"
           .
  
  RETURN TRUE.  
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

