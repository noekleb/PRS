&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tStrKonv NO-UNDO LIKE StrKonv.
DEFINE TEMP-TABLE tStrTStr NO-UNDO LIKE StrTStr
       FIELD StrKode LIKE StrKonv.StrKode.



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

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEF VAR iStrTypeID LIKE StrType.StrTypeID INIT 4 FORMAT '>>>>>>9' NO-UNDO.
  DEFINE VARIABLE         cBeskrivelse AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE         cStrListe    AS CHARACTER  NO-UNDO.
&ELSE
  DEFINE INPUT PARAMETER  iStrTypeID LIKE StrType.StrTypeID NO-UNDO.
  DEFINE INPUT  PARAMETER cBeskrivelse AS CHARACTER  NO-UNDO.
  DEFINE OUTPUT PARAMETER cStrListe  AS CHARACTER           NO-UNDO.
&ENDIF
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE iAntStr AS INTEGER    NO-UNDO.
DEFINE VARIABLE wAktivCol AS INTEGER    NO-UNDO.
DEFINE BUFFER   btStrTStr FOR tStrTStr.

DEFINE VAR wOrgBgCol         AS INT         NO-UNDO.
DEFINE VAR wSortBgCol        AS INT INIT 15 NO-UNDO.

&scoped-define sortby-phrase1 tStrKonv.StrKode /* DESCENDING */
&scoped-define sortby-phrase2 tStrkonv.Storl /* DESCENDING */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Dialog-Box
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-Fra

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tStrKonv tStrTStr

/* Definitions for BROWSE BROWSE-Fra                                    */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Fra tStrKonv.StrKode tStrKonv.Storl 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Fra tStrKonv.StrKode ~
tStrKonv.Storl 
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-Fra tStrKonv
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-Fra tStrKonv
&Scoped-define QUERY-STRING-BROWSE-Fra FOR EACH tStrKonv NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Fra OPEN QUERY BROWSE-Fra FOR EACH tStrKonv NO-LOCK ~
    ~{&SORTBY-PHRASE} INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Fra tStrKonv
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Fra tStrKonv


/* Definitions for BROWSE BROWSE-Til                                    */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Til tStrTStr.SoStorl 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Til 
&Scoped-define QUERY-STRING-BROWSE-Til FOR EACH tStrTStr NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Til OPEN QUERY BROWSE-Til FOR EACH tStrTStr NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Til tStrTStr
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Til tStrTStr


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-Fra}~
    ~{&OPEN-QUERY-BROWSE-Til}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BROWSE-Fra BROWSE-Til B-LeggTil B-TrekkFra ~
B-FlyttOpp B-FlyttNer Btn_OK Btn_Cancel Btn_Help FI-Tilgjenglige FI-Valgte 
&Scoped-Define DISPLAYED-OBJECTS FI-Tilgjenglige FI-Valgte 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-FlyttNer 
     LABEL "Flytt ner" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-FlyttOpp 
     LABEL "Flytt opp" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-LeggTil 
     LABEL "Legg til >" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-TrekkFra 
     LABEL "< Trekk fra" 
     SIZE 15 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE VARIABLE FI-Tilgjenglige AS CHARACTER FORMAT "X(256)":U INITIAL "Tilgjenglige" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Valgte AS CHARACTER FORMAT "X(256)":U INITIAL "Valgte" 
      VIEW-AS TEXT 
     SIZE 17 BY .62
     FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Fra FOR 
      tStrKonv SCROLLING.

DEFINE QUERY BROWSE-Til FOR 
      tStrTStr SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Fra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Fra Dialog-Frame _STRUCTURED
  QUERY BROWSE-Fra NO-LOCK DISPLAY
      tStrKonv.StrKode FORMAT ">>>>9":U
      tStrKonv.Storl FORMAT "x(10)":U WIDTH 12.8
  ENABLE
      tStrKonv.StrKode
      tStrKonv.Storl
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 26 BY 17.1 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-Til
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Til Dialog-Frame _STRUCTURED
  QUERY BROWSE-Til NO-LOCK DISPLAY
      tStrTStr.SoStorl COLUMN-LABEL "Størrelse" FORMAT "x(10)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 17.6 BY 17.1 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BROWSE-Fra AT ROW 2.86 COL 2
     BROWSE-Til AT ROW 2.86 COL 46.4
     B-LeggTil AT ROW 8.76 COL 30
     B-TrekkFra AT ROW 10.14 COL 30
     B-FlyttOpp AT ROW 11.52 COL 30
     B-FlyttNer AT ROW 12.91 COL 30
     Btn_OK AT ROW 20.33 COL 2
     Btn_Cancel AT ROW 20.33 COL 18.2
     Btn_Help AT ROW 20.33 COL 49
     FI-Tilgjenglige AT ROW 1.71 COL 2.4 NO-LABEL
     FI-Valgte AT ROW 1.71 COL 45 COLON-ALIGNED NO-LABEL
     SPACE(0.99) SKIP(19.37)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Størrelsesvalg"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Dialog-Box
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tStrKonv T "?" NO-UNDO skotex StrKonv
      TABLE: tStrTStr T "?" NO-UNDO skotex StrTStr
      ADDITIONAL-FIELDS:
          FIELD StrKode LIKE StrKonv.StrKode
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-Fra 1 Dialog-Frame */
/* BROWSE-TAB BROWSE-Til BROWSE-Fra Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

ASSIGN 
       tStrKonv.StrKode:COLUMN-READ-ONLY IN BROWSE BROWSE-Fra = TRUE
       tStrKonv.Storl:COLUMN-READ-ONLY IN BROWSE BROWSE-Fra = TRUE.

/* SETTINGS FOR FILL-IN FI-Tilgjenglige IN FRAME Dialog-Frame
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Fra
/* Query rebuild information for BROWSE BROWSE-Fra
     _TblList          = "Temp-Tables.tStrKonv"
     _Options          = "NO-LOCK INDEXED-REPOSITION SORTBY-PHRASE"
     _FldNameList[1]   > Temp-Tables.tStrKonv.StrKode
"tStrKonv.StrKode" ? ">>>>9" "integer" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.tStrKonv.Storl
"tStrKonv.Storl" ? ? "character" ? ? ? ? ? ? yes ? no no "12.8" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-Fra */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Til
/* Query rebuild information for BROWSE BROWSE-Til
     _TblList          = "Temp-Tables.tStrTStr"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.tStrTStr.SoStorl
"tStrTStr.SoStorl" "Størrelse" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BROWSE-Til */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Størrelsesvalg */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-FlyttNer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-FlyttNer Dialog-Frame
ON CHOOSE OF B-FlyttNer IN FRAME Dialog-Frame /* Flytt ner */
DO:
  DEFINE VARIABLE rFlyttObj AS ROWID      NO-UNDO.
  DEFINE VARIABLE iFlyttSeq AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFgSeq    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iRepRow   AS INTEGER    NO-UNDO.
  FIND btStrTStr WHERE ROWID(btStrTStr) = ROWID(tStrTStr).
  ASSIGN iFlyttSeq       = btStrTStr.SeqNr.
  IF BROWSE BROWSE-Til:SELECT-NEXT-ROW() THEN DO:
    ASSIGN btStrTStr.SeqNr = ?
           iRepRow         = BROWSE BROWSE-Til:FOCUSED-ROW
           iFgSeq          = tStrTStr.SeqNr
           tStrTStr.SeqNr  = iFlyttSeq
           btStrTStr.SeqNr = iFgSeq.
    FIND tStrTStr WHERE ROWID(tStrTStr) = ROWID(btStrTStr).
    BROWSE-TIL:SET-REPOSITIONED-ROW(iRepRow,"CONDITIONAL").
    {&OPEN-QUERY-BROWSE-Til}
    REPOSITION BROWSE-Til TO ROWID ROWID(btStrTStr).
  END.
  RUN ButtonEnaDis("TIL").
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-FlyttOpp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-FlyttOpp Dialog-Frame
ON CHOOSE OF B-FlyttOpp IN FRAME Dialog-Frame /* Flytt opp */
DO:
  DEFINE VARIABLE rFlyttObj AS ROWID      NO-UNDO.
  DEFINE VARIABLE iFlyttSeq AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iFgSeq    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iRepRow   AS INTEGER    NO-UNDO.
  FIND btStrTStr WHERE ROWID(btStrTStr) = ROWID(tStrTStr).
  ASSIGN iFlyttSeq       = btStrTStr.SeqNr
         btStrTStr.SeqNr = ?.
  ASSIGN iRepRow         = BROWSE BROWSE-Til:FOCUSED-ROW.
  BROWSE BROWSE-Til:SELECT-PREV-ROW().
  ASSIGN iFgSeq          = tStrTStr.SeqNr
         tStrTStr.SeqNr  = iFlyttSeq
         btStrTStr.SeqNr = iFgSeq.
  FIND tStrTStr WHERE ROWID(tStrTStr) = ROWID(btStrTStr).
  BROWSE-TIL:SET-REPOSITIONED-ROW(iRepRow,"CONDITIONAL").
  {&OPEN-QUERY-BROWSE-Til}
  REPOSITION BROWSE-Til TO ROWID ROWID(btStrTStr).
  RUN ButtonEnaDis("TIL"). 
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-LeggTil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-LeggTil Dialog-Frame
ON CHOOSE OF B-LeggTil IN FRAME Dialog-Frame /* Legg til > */
DO:
    DEFINE VARIABLE rStrTStr AS ROWID      NO-UNDO.
    DEFINE VARIABLE iNextSeq AS INTEGER    NO-UNDO.
    FIND LAST tStrTStr NO-ERROR.
    ASSIGN iNextSeq = IF AVAIL tStrTStr THEN tStrTStr.SeqNr + 1 ELSE 1.
    RELEASE tStrTStr.
    BUFFER-COPY tStrKonv USING StrKode TO tStrTStr.
    ASSIGN tStrTStr.StrTypeID = iStrTypeID
           tStrTStr.SoStorl   = tStrKonv.Storl
           tStrTStr.SeqNr     = iNextSeq.
    ASSIGN rStrTStr = ROWID(tStrTStr).
    RELEASE tStrTStr.
    DELETE tStrKonv.
    BROWSE BROWSE-Fra:DELETE-SELECTED-ROWS().
    IF BROWSE BROWSE-Fra:FOCUSED-ROW <> ? THEN
        BROWSE BROWSE-Fra:SELECT-FOCUSED-ROW().
    {&OPEN-QUERY-BROWSE-Til}
    BROWSE-TIL:SET-REPOSITIONED-ROW(BROWSE-Til:DOWN,"ALWAYS").
    REPOSITION BROWSE-Til TO ROWID rStrTStr.
    ASSIGN iAntStr = iAntStr + 1.
    IF iAntStr = 48 THEN
        ASSIGN SELF:SENSITIVE = FALSE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-TrekkFra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-TrekkFra Dialog-Frame
ON CHOOSE OF B-TrekkFra IN FRAME Dialog-Frame /* < Trekk fra */
DO:
  DEFINE VARIABLE rStrKonv AS ROWID      NO-UNDO.
  FIND StrKonv WHERE StrKonv.StrKode = tStrTstr.StrKode NO-LOCK.
  RELEASE tStrKonv.
  BUFFER-COPY StrKonv TO tStrKonv.
  ASSIGN rStrKonv = ROWID(tStrKonv).
  DELETE tStrTstr.
  BROWSE BROWSE-Til:DELETE-SELECTED-ROWS().
  IF BROWSE BROWSE-Til:FOCUSED-ROW <> ? THEN
      BROWSE BROWSE-Til:SELECT-FOCUSED-ROW().
  {&OPEN-QUERY-BROWSE-Fra}
  REPOSITION BROWSE-Fra TO ROWID rStrKonv.
  ASSIGN iAntStr = iAntStr - 1.
  RUN ButtonEnaDis("TIL").
  IF BROWSE BROWSE-Til:FOCUSED-ROW = ? THEN
      APPLY "ENTRY" TO BROWSE BROWSE-Fra.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Fra
&Scoped-define SELF-NAME BROWSE-Fra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Fra Dialog-Frame
ON ENTRY OF BROWSE-Fra IN FRAME Dialog-Frame
DO:
  RUN ButtonEnaDis("FRA").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Fra Dialog-Frame
ON START-SEARCH OF BROWSE-Fra IN FRAME Dialog-Frame
DO:
  DEFINE VAR h-Curr-Col AS widget.
  h-Curr-Col = BROWSE {&BROWSE-NAME}:CURRENT-COLUMN.
  APPLY "END-SEARCH" TO SELF.
  CASE h-Curr-Col:NAME:
      WHEN "StrKode" THEN
          IF wAktivCol <> 1 THEN DO:
              RUN SortNyCol(1).
              RETURN NO-APPLY.
          END.
      WHEN "Storl" THEN
          IF wAktivCol <> 2 THEN DO:
              RUN SortNyCol(2).
              RETURN NO-APPLY.
          END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Til
&Scoped-define SELF-NAME BROWSE-Til
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Til Dialog-Frame
ON ENTRY OF BROWSE-Til IN FRAME Dialog-Frame
DO:
    RUN ButtonEnaDis("TIL").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Til Dialog-Frame
ON VALUE-CHANGED OF BROWSE-Til IN FRAME Dialog-Frame
DO:
   RUN ButtonEnaDis("TIL").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  MESSAGE "Help for File: {&FILE-NAME}" VIEW-AS ALERT-BOX INFORMATION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  DEFINE VARIABLE iCount AS INTEGER    NO-UNDO.
  ASSIGN cStrListe = FILL(CHR(1),150).
  FOR EACH tStrTStr:
      ASSIGN iCount = iCount + 1.
             ENTRY(iCount,cStrListe,CHR(1)) = tStrTStr.SoStorl.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Fra
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT EQ ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.


/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   wOrgBgCol = tStrKonv.StrKode:Label-bgcolor IN BROWSE {&BROWSE-NAME}.
  RUN InitTempTables.
  {lng.i}
  ASSIGN FRAME {&FRAME-NAME}:TITLE = FRAME {&FRAME-NAME}:TITLE + " " + cBeskrivelse.
  
  ASSIGN
  tStrKonv.StrKode:LABEL IN BROWSE {&BROWSE-NAME} = tStrKonv.StrKode:LABEL + " * "
  tStrKonv.Storl:LABEL IN BROWSE {&BROWSE-NAME}  = tStrKonv.Storl:LABEL + " * ".

  RUN enable_UI.
  RUN Init-Read-Only.
  RUN SortNyCol(1).
/*   BROWSE-Fra:SET-REPOSITIONED-ROW(INT(BROWSE-Fra:DOWN / 2),"ALWAYS").  */
/*   BROWSE-TIL:SET-REPOSITIONED-ROW(BROWSE-Til:DOWN,"ALWAYS").           */

  APPLY "ENTRY" TO BROWSE-Fra.
  APPLY "VALUE-CHANGED" TO BROWSE-Til.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
END.
RUN disable_UI.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButtonEnaDis Dialog-Frame 
PROCEDURE ButtonEnaDis :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cBrowse AS CHARACTER  NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
    /*
    IF cBrowse = "TIL" THEN DO:
        ASSIGN B-LeggTil:SENSITIVE  = FALSE
               B-TrekkFra:SENSITIVE = BROWSE-Til:FOCUSED-ROW <> ?
               B-FlyttOpp:SENSITIVE = 
                 CAN-FIND(FIRST btStrTStr WHERE bTstrTStr.SeqNr < tStrTStr.SeqNr)
               B-FlyttNer:SENSITIVE = 
                 CAN-FIND(FIRST btStrTStr WHERE bTstrTStr.SeqNr > tStrTStr.SeqNr).
    END.
    ELSE IF cBrowse = "FRA" THEN DO:
        ASSIGN B-LeggTil:SENSITIVE  = BROWSE-Fra:FOCUSED-ROW <> ? AND 
                                      iAntStr < 48
               B-TrekkFra:SENSITIVE = FALSE
               B-FlyttOpp:SENSITIVE = FALSE
               B-FlyttNer:SENSITIVE = FALSE.
    END.
    */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  DISPLAY FI-Tilgjenglige FI-Valgte 
      WITH FRAME Dialog-Frame.
  ENABLE BROWSE-Fra BROWSE-Til B-LeggTil B-TrekkFra B-FlyttOpp B-FlyttNer 
         Btn_OK Btn_Cancel Btn_Help FI-Tilgjenglige FI-Valgte 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init-Read-Only Dialog-Frame 
PROCEDURE Init-Read-Only :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
    tStrKonv.StrKode:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
    tStrKonv.Storl:READ-ONLY IN BROWSE {&BROWSE-NAME} = YES
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitTempTables Dialog-Frame 
PROCEDURE InitTempTables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR iAnt1 AS INT NO-UNDO.
DEF VAR iAnt2 AS INT NO-UNDO.

   FOR EACH StrKonv NO-LOCK.
        CREATE tStrKonv.
        BUFFER-COPY StrKonv TO tStrKonv.
        iAnt1 = iAnt1 + 1.
    END.

    FOR EACH StrTStr WHERE StrTStr.StrTypeID = iStrTypeID NO-LOCK:
        FIND FIRST tStrKonv WHERE tStrKonv.Storl = StrTstr.SoStorl NO-ERROR.
        IF NOT AVAILABLE tStrKonv THEN
        DO:
            MESSAGE "Størrelse som er ukjent i StrKonv, men som ligger i størrelsestypen: " "'" + StrTstr.SoStorl + "'" SKIP
                CAN-FIND(FIRST StrKonv WHERE StrKonv.Storl = StrTStr.SoStorl) SKIP
                CAN-FIND(FIRST tStrKonv WHERE tStrKonv.Storl = StrTStr.SoStorl)
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            NEXT.
        END.
        BUFFER-COPY StrTStr TO tStrTStr
            ASSIGN tStrTStr.StrKode = tStrKonv.StrKode.
        DELETE tStrKonv.
        RELEASE tStrTStr.
        ASSIGN iAntStr = iAntStr + 1.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SortNyCol Dialog-Frame 
PROCEDURE SortNyCol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER wNyCol AS INT NO-UNDO.
  DEFINE VAR             wDataType AS CHAR NO-UNDO.
  IF wNyCol = wAktivCol THEN
      RETURN NO-APPLY.
  ASSIGN wAktivCol = wNyCol.
  CASE wNyCol:
    WHEN 1 THEN DO:
        &scope SORTBY-PHRASE BY {&sortby-phrase1}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
    END.
    &IF DEFINED(sortby-phrase2) &THEN
    WHEN 2 THEN DO:  
        &scope SORTBY-PHRASE BY {&sortby-phrase2}
        {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
        .
     END.
     &ENDIF
  END CASE.
  ASSIGN
   tStrKonv.StrKode:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF wAktivCol = 1 THEN wSortBgCol ELSE wOrgBgCol
   tStrKonv.Storl:Label-bgcolor IN BROWSE {&BROWSE-NAME} = 
                                   IF wAktivCol = 2 THEN wSortBgCol ELSE wOrgBgCol.
  &scope SORTBY-PHRASE BY {&sortby-phrase1}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

