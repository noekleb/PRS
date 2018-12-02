&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE TT_BongLinje NO-UNDO LIKE BongLinje
       FIELD Levkod as char
       FIELD DBandel as deci
       FIELD DBkr as deci
       FIELD strekrowid as char
       FIELD ttRowid as ROWID
       .



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


DEFINE INPUT  PARAMETER ipcKOrdreID AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER iButikkNr AS INTEGER     NO-UNDO.
/* DEFINE VARIABLE iButikkNr AS INTEGER     NO-UNDO. */
/* Parameters Definitions ---                                           */
DEFINE VAR iSelgernr AS INTEGER     NO-UNDO.
DEFINE VAR lOK           AS LOGICAL     NO-UNDO.

/* Local Variable Definitions ---                                       */
DEFINE VARIABLE cSprak      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hSourceProc AS HANDLE      NO-UNDO.
DEFINE VARIABLE iAvbryt AS INTEGER     NO-UNDO.
DEFINE VARIABLE cStrekkode AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLinjenr  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iKassenr AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMBBekreft AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMBAvbryt  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMBAntal   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMBSlett   AS INTEGER     NO-UNDO.

DEFINE VARIABLE lwaitforKO AS LOGICAL     NO-UNDO.

DEFINE VARIABLE cMenuLabels AS CHARACTER   NO-UNDO.


DEFINE BUFFER bufTT FOR TT_Bonglinje.

DEFINE TEMP-TABLE tt_linjer NO-UNDO SERIALIZE-NAME "kolinjer"
    FIELD artikkelnr AS CHAR
    FIELD linjenr AS INTE
    FIELD ean AS CHAR
    FIELD varetekst AS CHAR
    FIELD antall AS INTE
    FIELD levfargkod AS CHAR
    FIELD storl AS CHAR
    FIELD kundpris AS DECI
    FIELD feilkode AS INTE
    FIELD used AS LOG. /* används på kassasidan */

DEFINE TEMP-TABLE tt_koder NO-UNDO SERIALIZE-NAME "returkoder" 
    FIELD ReturKodeId    AS INTE
    FIELD ReturKodeTekst AS CHAR
    INDEX ReturKodeId IS PRIMARY UNIQUE ReturKodeId.

DEF VAR cSingleHW     AS CHAR                NO-UNDO. /* se längre ner för initiering */
DEF VAR cDoubleH      AS CHAR                NO-UNDO. /*            "                 */
DEF VAR cDoubleW      AS CHAR                NO-UNDO. /*            "                 */
DEF VAR cDoubleHW     AS CHAR                NO-UNDO. /*            "                 */

ASSIGN cSingleHW = CHR(27) + "|1C" /* dessa sätts till blankt om printer inte stöder */
       cDoubleW  = CHR(27) + "|2C"
       cDoubleH  = CHR(27) + "|3C"
       cDoubleHW = CHR(27) + "|4C".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BR-TT

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_BongLinje

/* Definitions for BROWSE BR-TT                                         */
&Scoped-define FIELDS-IN-QUERY-BR-TT TT_BongLinje.ArtikkelNr ~
TT_BongLinje.Strekkode TT_BongLinje.BongTekst TT_BongLinje.Storrelse ~
TT_BongLinje.Antall TT_BongLinje.FeilKode TT_BongLinje.LinjeSum " " 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-TT 
&Scoped-define QUERY-STRING-BR-TT FOR EACH TT_BongLinje SHARE-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-TT OPEN QUERY BR-TT FOR EACH TT_BongLinje SHARE-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-TT TT_BongLinje
&Scoped-define FIRST-TABLE-IN-QUERY-BR-TT TT_BongLinje


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BR-TT}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-KOrdreId BR-TT B-Select B-Returnera ~
B-Avsluta 
&Scoped-Define DISPLAYED-OBJECTS FI-KOrdreId FI-Kundnamn FI-EAN ~
FI-Varetekst FI-Felkod FI-InfoText 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getParameter C-Win 
FUNCTION getParameter RETURNS CHAR
      (INPUT  piSysHId AS INT ,
       INPUT  piSysGr  AS INT  ,
       INPUT  piParaNr AS INT ) FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ResetBtnState C-Win 
FUNCTION ResetBtnState RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Avsluta 
     LABEL "Avbryt" 
     SIZE 31 BY 2.14
     FONT 8.

DEFINE BUTTON B-Returnera 
     LABEL "Returnera" 
     SIZE 31 BY 2.14
     FONT 8.

DEFINE BUTTON B-Select 
     LABEL "Välj/Välj bort" 
     SIZE 31 BY 2.14
     FONT 8.

DEFINE VARIABLE FI-EAN AS CHARACTER FORMAT "X(22)":U 
     VIEW-AS FILL-IN 
     SIZE 66 BY 1.76
     FONT 8 NO-UNDO.

DEFINE VARIABLE FI-Felkod AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY 1.76
     FONT 8 NO-UNDO.

DEFINE VARIABLE FI-InfoText AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 146 BY 1.71
     FONT 8 NO-UNDO.

DEFINE VARIABLE FI-KOrdreId AS CHARACTER FORMAT "X(22)":U 
     VIEW-AS FILL-IN 
     SIZE 66 BY 1.76
     FONT 8 NO-UNDO.

DEFINE VARIABLE FI-Kundnamn AS CHARACTER FORMAT "X(100)":U 
     VIEW-AS FILL-IN 
     SIZE 88 BY 1.76
     FONT 12 NO-UNDO.

DEFINE VARIABLE FI-Varetekst AS CHARACTER FORMAT "X(100)":U 
     VIEW-AS FILL-IN 
     SIZE 88 BY 1.76
     FONT 22 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR-TT FOR 
      TT_BongLinje SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR-TT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-TT C-Win _STRUCTURED
  QUERY BR-TT SHARE-LOCK NO-WAIT DISPLAY
      TT_BongLinje.ArtikkelNr COLUMN-LABEL "Artikkelnr" FORMAT "X(16)":U
      TT_BongLinje.Strekkode COLUMN-LABEL "EAN" FORMAT "X(16)":U
            WIDTH 29
      TT_BongLinje.BongTekst COLUMN-LABEL "Varetekst" FORMAT "X(30)":U
            WIDTH 43.6
      TT_BongLinje.Storrelse FORMAT "X(4)":U WIDTH 5.8
      TT_BongLinje.Antall COLUMN-LABEL "Antal" FORMAT "->>9":U
      TT_BongLinje.FeilKode FORMAT ">9":U WIDTH 5.6
      TT_BongLinje.LinjeSum COLUMN-LABEL "Pris" FORMAT "->>>,>>9.99":U
      " "
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 156 BY 12.62
         FONT 12 ROW-HEIGHT-CHARS 1.05 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-KOrdreId AT ROW 1.29 COL 2 NO-LABEL WIDGET-ID 38
     FI-Kundnamn AT ROW 1.29 COL 70 NO-LABEL WIDGET-ID 42
     FI-EAN AT ROW 3.29 COL 2 NO-LABEL WIDGET-ID 2
     FI-Varetekst AT ROW 3.29 COL 70 NO-LABEL WIDGET-ID 4
     FI-Felkod AT ROW 5.29 COL 2 NO-LABEL WIDGET-ID 44
     BR-TT AT ROW 7.33 COL 2 WIDGET-ID 200
     B-Select AT ROW 7.43 COL 165
     B-Returnera AT ROW 10.29 COL 165
     B-Avsluta AT ROW 13.62 COL 165
     FI-InfoText AT ROW 20.43 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 28
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 202 BY 22.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: TT_BongLinje T "?" NO-UNDO data BongLinje
      ADDITIONAL-FIELDS:
          FIELD Levkod as char
          FIELD DBandel as deci
          FIELD DBkr as deci
          FIELD strekrowid as char
          FIELD ttRowid as ROWID
          
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Retur webbutik"
         HEIGHT             = 22.1
         WIDTH              = 202
         MAX-HEIGHT         = 40.24
         MAX-WIDTH          = 216.4
         VIRTUAL-HEIGHT     = 40.24
         VIRTUAL-WIDTH      = 216.4
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
/* BROWSE-TAB BR-TT FI-Felkod DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN FI-EAN IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Felkod IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-InfoText IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-KOrdreId IN FRAME DEFAULT-FRAME
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN FI-Kundnamn IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Varetekst IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-TT
/* Query rebuild information for BROWSE BR-TT
     _TblList          = "Temp-Tables.TT_BongLinje"
     _Options          = "SHARE-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > Temp-Tables.TT_BongLinje.ArtikkelNr
"TT_BongLinje.ArtikkelNr" "Artikkelnr" "X(16)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > Temp-Tables.TT_BongLinje.Strekkode
"TT_BongLinje.Strekkode" "EAN" "X(16)" "character" ? ? ? ? ? ? no ? no no "29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > Temp-Tables.TT_BongLinje.BongTekst
"TT_BongLinje.BongTekst" "Varetekst" ? "character" ? ? ? ? ? ? no ? no no "43.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > Temp-Tables.TT_BongLinje.Storrelse
"TT_BongLinje.Storrelse" ? ? "character" ? ? ? ? ? ? no ? no no "5.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > Temp-Tables.TT_BongLinje.Antall
"TT_BongLinje.Antall" "Antal" "->>9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > Temp-Tables.TT_BongLinje.FeilKode
"TT_BongLinje.FeilKode" ? ? "integer" ? ? ? ? ? ? no ? no no "5.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > Temp-Tables.TT_BongLinje.LinjeSum
"TT_BongLinje.LinjeSum" "Pris" "->>>,>>9.99" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > "_<CALC>"
""" """ ? ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE BR-TT */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Retur webbutik */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Retur webbutik */
DO:
  /* This event will close the window and terminate the procedure.  */
    APPLY "CHOOSE" TO B-Avsluta IN FRAME {&FRAME-NAME}.
/*   APPLY "CLOSE":U TO THIS-PROCEDURE. */
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Avsluta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Avsluta C-Win
ON CHOOSE OF B-Avsluta IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  RUN funkPopUpBtn7Action.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Returnera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Returnera C-Win
ON CHOOSE OF B-Returnera IN FRAME DEFAULT-FRAME /* Returnera */
DO:

    RUN funkPopUpBtn3Action.
    RETURN NO-APPLY.
/*     OUTPUT TO "CLIPBOARD".                       */
/*                                                  */
/*   FOR EACH tt_Linjer WHERE tt_linjer.antall = 1: */
/*       EXPORT tt_Linjer.                          */
/*   END.                                           */
/*   OUTPUT CLOSE.                                  */
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Select
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Select C-Win
ON CHOOSE OF B-Select IN FRAME DEFAULT-FRAME /* Välj/Välj bort */
DO:
    DEFINE VARIABLE dBelopp AS DECIMAL     NO-UNDO.
    IF tt_bonglinje.originaldata = "MANUELL" THEN DO:
        IF tt_bonglinje.antal = 0 THEN DO:
            RUN d-ReturKostnad.w (INPUT tt_Bonglinje.bongtekst, OUTPUT dBelopp,OUTPUT lOK).
            IF lOK THEN
                tt_bonglinje.linjesum = dBelopp.
            ELSE
                RETURN NO-APPLY.
        END.
        ELSE
            tt_bonglinje.linjesum = dBelopp.
    END.
    tt_bonglinje.antal = IF tt_bonglinje.antal = 0 THEN (1 * IF tt_bonglinje.DivInfo = "DEBITERA" THEN -1 ELSE 1) ELSE 0.
    FIND tt_linjer WHERE ROWID(tt_linjer) = tt_bonglinje.TTrowid.
    tt_linjer.used = tt_bonglinje.antal <> 0.
    IF tt_bonglinje.originaldata = "MANUELL" THEN
        tt_linjer.kundpris = dBelopp.
    BROWSE {&BROWSE-NAME}:REFRESH().
    DYNAMIC-FUNCTION('ResetBtnState':U).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-TT
&Scoped-define SELF-NAME BR-TT
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-TT C-Win
ON CURSOR-DOWN OF BR-TT IN FRAME DEFAULT-FRAME
OR CURSOR-UP OF BR-TT DO:
    IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN
  BROWSE {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-TT C-Win
ON ENTRY OF BR-TT IN FRAME DEFAULT-FRAME
DO:
/*   APPLY "ENTRY" TO FI-EAN. */
/*   RETURN NO-APPLY.              */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-TT C-Win
ON VALUE-CHANGED OF BR-TT IN FRAME DEFAULT-FRAME
DO:
/*     ASSIGN FI-Bruttopris  = ROUND(tt_bonglinje.linjesum / tt_bonglinje.antall,2)    */
/*            FI-Styckrabatt = ROUND(tt_bonglinje.linjerab / tt_bonglinje.antall,2)    */
/*            FI-Subrabatt   = ROUND(tt_bonglinje.SubtotalRab / tt_bonglinje.antall,2) */
/*            FI-Returpris   = FI-Bruttopris - FI-Styckrabatt - FI-Subrabatt.          */
/*                                                                                     */
/*     DISPLAY FI-Bruttopris                                                           */
/*             FI-Styckrabatt                                                          */
/*             FI-Subrabatt                                                            */
/*             FI-Returpris WITH FRAME {&FRAME-NAME}.                                  */
    resetBtnState().

/*   DISPLAY TT_Bonglinje.antall WITH FRAME {&FRAME-NAME}. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-EAN
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-EAN C-Win
ON ANY-KEY OF FI-EAN IN FRAME DEFAULT-FRAME
DO:
    
    /* cKeyboard innehåller en lista med tangenter som i tur och ordning betyder: */
    /* 0,1,2,3,4,5,6,7,8,9,00,',',clear,bekreft (',' används inte här)            */
    DEF VAR iIdx    AS INTE NO-UNDO.
    DEF VAR iIdx2   AS INTE NO-UNDO.
    DEF VAR iFunkNr AS INTE NO-UNDO.
    /* 13 = Return = Bekreft */
    IF LASTKEY = 27 OR LASTKEY = iAvbryt OR LASTKEY = iMBAvbryt THEN DO:
            IF SELF:SCREEN-VALUE <> "" THEN
                SELF:SCREEN-VALUE = "".
        RETURN NO-APPLY.
    END.
/*     ELSE IF LASTKEY = iMBNyEan AND DYNAMIC-FUNCTION('GetButtonSensitive',1) THEN DO: */
/*         RUN funkPopUpBtn1Action.                                                     */
/*         RETURN NO-APPLY.                                                             */
/*     END.                                                                             */
    ELSE IF LASTKEY = iMBBekreft AND DYNAMIC-FUNCTION('GetButtonSensitive',3) THEN DO:
        RUN funkPopUpBtn3Action.
        RETURN NO-APPLY.
    END.
    ELSE IF LASTKEY = 13 THEN DO:
        IF FI-EAN:SCREEN-VALUE = "" THEN
            RETURN NO-APPLY.
        ELSE DO:
            FI-Infotext:SCREEN-VALUE = "".
            PUBLISH "EAN" (FI-EAN:SCREEN-VALUE,"").
/*             RUN EAN (FI-EAN:SCREEN-VALUE,""). */
            RETURN NO-APPLY.
        END.
    END.
    APPLY LASTKEY.
    ASSIGN SELF:CURSOR-OFFSET = LENGTH(SELF:SCREEN-VALUE) + 1.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-EAN C-Win
ON ANY-PRINTABLE OF FI-EAN IN FRAME DEFAULT-FRAME
DO:
   IF LASTKEY < 48 AND LASTKEY > 57 THEN
       RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-EAN C-Win
ON CURSOR-DOWN OF FI-EAN IN FRAME DEFAULT-FRAME
DO:
/*   run funk46. */
/*   IF CAN-FIND(FIRST TT_BongLinje) THEN APPLY "ENTRY" TO br-tt. */
/*   IF br-tt:FOCUSED-ROW <> ? THEN                               */
/*       br-tt:SELECT-FOCUSED-ROW().                              */
/*   APPLY "CURSOR-DOWN" TO br-tt.                                */
/*   IF br-tt:FOCUSED-ROW <> ? THEN                               */
/*       br-tt:SELECT-FOCUSED-ROW().                              */
/*   ResetBtnState().                                             */
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-EAN C-Win
ON CURSOR-UP OF FI-EAN IN FRAME DEFAULT-FRAME
DO:
/*   run funk47. */
/*   IF CAN-FIND(FIRST TT_BongLinje) THEN APPLY "ENTRY" TO br-tt. */
/*   APPLY "CURSOR-UP" TO br-tt.                                  */
/*   IF br-tt:FOCUSED-ROW <> ? THEN                               */
/*       br-tt:SELECT-FOCUSED-ROW().                              */
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Felkod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Felkod C-Win
ON ANY-KEY OF FI-Felkod IN FRAME DEFAULT-FRAME
DO:
    
    /* cKeyboard innehåller en lista med tangenter som i tur och ordning betyder: */
    /* 0,1,2,3,4,5,6,7,8,9,00,',',clear,bekreft (',' används inte här)            */
    DEF VAR iIdx    AS INTE NO-UNDO.
    DEF VAR iIdx2   AS INTE NO-UNDO.
    DEF VAR iFunkNr AS INTE NO-UNDO.
    /* 13 = Return = Bekreft */
    IF LASTKEY = 27 OR LASTKEY = iAvbryt OR LASTKEY = iMBAvbryt THEN DO:
            IF SELF:SCREEN-VALUE <> "" THEN
                SELF:SCREEN-VALUE = "".
        RETURN NO-APPLY.
    END.
/*     ELSE IF LASTKEY = iMBNyEan AND DYNAMIC-FUNCTION('GetButtonSensitive',1) THEN DO: */
/*         RUN funkPopUpBtn1Action.                                                     */
/*         RETURN NO-APPLY.                                                             */
/*     END.                                                                             */
    ELSE IF LASTKEY = iMBBekreft AND DYNAMIC-FUNCTION('GetButtonSensitive',3) THEN DO:
        RUN funkPopUpBtn3Action.
        RETURN NO-APPLY.
    END.
    ELSE IF LASTKEY = 13 THEN DO:
        IF FI-EAN:SCREEN-VALUE = "" THEN
            RETURN NO-APPLY.
        ELSE DO:
            FI-Infotext:SCREEN-VALUE = "".
            RUN EAN (FI-Felkod:SCREEN-VALUE,"FK").
/*             RUN EAN (FI-EAN:SCREEN-VALUE,""). */
            RETURN NO-APPLY.
        END.
    END.
    APPLY LASTKEY.
    ASSIGN SELF:CURSOR-OFFSET = LENGTH(SELF:SCREEN-VALUE) + 1.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Felkod C-Win
ON ANY-PRINTABLE OF FI-Felkod IN FRAME DEFAULT-FRAME
DO:
   IF LASTKEY < 48 AND LASTKEY > 57 THEN
       RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-KOrdreId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-KOrdreId C-Win
ON ANY-KEY OF FI-KOrdreId IN FRAME DEFAULT-FRAME
DO:
    
    /* cKeyboard innehåller en lista med tangenter som i tur och ordning betyder: */
    /* 0,1,2,3,4,5,6,7,8,9,00,',',clear,bekreft (',' används inte här)            */
    DEF VAR iIdx    AS INTE NO-UNDO.
    DEF VAR iIdx2   AS INTE NO-UNDO.
    DEF VAR iFunkNr AS INTE NO-UNDO.
    /* 13 = Return = Bekreft */
    IF LASTKEY = 27 OR LASTKEY = iAvbryt OR LASTKEY = iMBAvbryt THEN DO:
        IF SELF:SCREEN-VALUE <> "" THEN
            SELF:SCREEN-VALUE = "".
        RETURN NO-APPLY.
    END.
/*     ELSE IF LASTKEY = iMBNyEan AND DYNAMIC-FUNCTION('GetButtonSensitive',1) THEN DO: */
/*         RUN funkPopUpBtn1Action.                                                     */
/*         RETURN NO-APPLY.                                                             */
/*     END.                                                                             */
    ELSE IF LASTKEY = iMBBekreft AND DYNAMIC-FUNCTION('GetButtonSensitive',3) THEN DO:
        RUN funkPopUpBtn3Action.
        RETURN NO-APPLY.
    END.
    ELSE IF LASTKEY = iMBAvbryt AND DYNAMIC-FUNCTION('GetButtonSensitive',7) THEN DO:
        RUN funkPopUpBtn7Action .  /* OBS Knapp6 ger Btn7Action */
        RETURN NO-APPLY.
    END.
    ELSE IF LASTKEY = 13 THEN DO:
        IF SELF:SCREEN-VALUE = "" THEN
            RETURN NO-APPLY.
        ELSE DO:
            FI-Infotext:SCREEN-VALUE = "".
            RUN EAN (SELF:SCREEN-VALUE,"").
/*             RUN EAN (FI-EAN:SCREEN-VALUE,""). */
            RETURN NO-APPLY.
        END.
    END.
    APPLY LASTKEY.
    ASSIGN SELF:CURSOR-OFFSET = LENGTH(SELF:SCREEN-VALUE) + 1.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */
/* {osk\oskpopupform.i} */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.
/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.
hSourceProc = SOURCE-PROCEDURE.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
hSourceProc = SOURCE-PROCEDURE.
cSprak = "SE". /*  DYNAMIC-FUNCTION("LANGUAGE"). */

EMPTY TEMP-TABLE TT_Bonglinje.
RUN initParameter.

/* först skall vi ange ett kordreid */
lwaitforKO = TRUE.
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    iMBBekreft = KEYCODE("F4") NO-ERROR.
    iMBAvbryt  = KEYCODE("F7") NO-ERROR.
/*     iMBAntal   = INT(ENTRY(5,cMenuButtons)) NO-ERROR. */

/*     RUN initPopUpFrame IN SOURCE-PROCEDURE (FRAME {&FRAME-NAME}:HANDLE) . */
FI-KOrdreId = ipcKOrdreID.
    cMenuLabels = ",,Bekreft,,,,Avbryt,".
/* IF CAN-DO(hSourceProc:INTERNAL-ENTRIES,"GetKordreId") THEN */
/*     RUN GetKordreId IN hSourceProc(OUTPUT FI-KOrdreId).    */
  RUN enable_UI.
/*   RUN ResetMenu ("CONFIRMMENU_CUSTOM") . */
  RUN showwidget.
  lOK = TRUE.
  IF lOK THEN DO: /* lOK sätts till TRUE för att komma hit om vi inte har överföring, lOK används även till programmets returvärde */
      lOK = FALSE.
      APPLY "ENTRY" TO FI-KOrdreId.
      IF FI-KOrdreId <> "" THEN
          RUN EAN (FI-KOrdreId,"").
      DYNAMIC-FUNCTION('ResetBtnState':U).
      WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  END.
END.
RUN disable_UI.
/* RUN InitPopUpFrameExit IN SOURCE-PROCEDURE NO-ERROR. */
/* DELETE OBJECT oskPopUpForm NO-ERROR.                 */

/* OUTPUT TO "CLIPBOARD".                       */
/* FOR EACH tt_bonglinje:                       */
/*     EXPORT artikkelnr strekkode antall tbid. */
/* END.                                         */
OUTPUT CLOSE.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EAN C-Win 
PROCEDURE EAN :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER scanned    AS CHAR.
  DEFINE INPUT PARAMETER cAnroptFra AS CHAR.
  DEFINE VARIABLE dPris      AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dVarekost  AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE iPrisType  AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dMedPris1  AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dNormalpris AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dMvaKr AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE RowIDttBongLinje AS ROWID NO-UNDO. 
  DEFINE VARIABLE lOK AS LOGICAL     NO-UNDO.
  DEFINE BUFFER dBufTT FOR tt_BongLinje.
/*       MESSAGE scanned SKIP LENGTH(scanned)   */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK. */
  IF FOCUS:NAME = "FI-KOrdreId" THEN DO:
      /* cAnroptFra = "MANUELL" THEN */

      IF NOT scanned BEGINS "KO" THEN DO:
              scanned = "KO" + scanned.
      END.
      ELSE IF NOT scanned BEGINS "KO" THEN DO:
          fi-infoText:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Feil kundeordre".
          RETURN.
      END.
      FI-KOrdreId:SCREEN-VALUE = scanned.
      RUN getKOrdredata ("GETKORDRE",scanned,OUTPUT lOK).
/*       lwaitforKO = FALSE. */
      RETURN.
  END.
  ELSE IF FOCUS:NAME = "FI-Felkod" THEN DO:
      FIND tt_koder WHERE tt_koder.ReturKodeId = INT(scanned) NO-ERROR.
      IF ERROR-STATUS:ERROR OR NOT AVAIL tt_koder THEN DO:
          BELL.
          FI-Felkod:SCREEN-VALUE = "".
          FI-InfoText:SCREEN-VALUE = "Feil kode".
          RETURN.
      END.
      FIND FIRST tt_linjer WHERE tt_linjer.ean = FI-EAN:SCREEN-VALUE AND tt_linjer.used = FALSE NO-ERROR.
      IF AVAIL tt_linjer THEN DO:
          FI-Varetekst = tt_linjer.varetekst.
          CREATE tt_bonglinje.
          ASSIGN tt_bonglinje.artikkelnr = tt_linjer.artikkelnr
                 tt_bonglinje.linjenr    = tt_linjer.linjenr
                 tt_bonglinje.strekkode  = tt_linjer.ean
                 tt_bonglinje.bongtekst  = tt_linjer.varetekst
                 tt_bonglinje.storrelse  = tt_linjer.storl
                 tt_bonglinje.antall     = tt_linjer.antall
                 tt_bonglinje.linjesum   = tt_linjer.kundpris
                 tt_bonglinje.feilkode   = tt_koder.ReturKodeId
                 tt_linjer.used = TRUE
                 tt_linjer.feilkode = tt_koder.ReturKodeId.
          resetBtnState().
          {&OPEN-QUERY-{&BROWSE-NAME}}
      END.
      FI-Varetekst:SCREEN-VALUE = "".
      FI-FELKOD:SCREEN-VALUE = "".
      FI-EAN:SCREEN-VALUE = "".
      FI-EAN:SENSITIVE = TRUE.
      APPLY "ENTRY" TO FI-EAN.
      FI-Felkod:SENSITIVE = FALSE.
      RETURN.
  END.

  ELSE IF FOCUS:NAME = "FI-EAN" THEN DO:
      FI-InfoText:SCREEN-VALUE = "".
      FI-InfoText:BGCOLOR      = ?.
      FIND FIRST tt_linjer WHERE tt_linjer.ean = scanned NO-ERROR.
      IF NOT AVAIL tt_linjer THEN DO:
          BELL.
          FI-InfoText:SCREEN-VALUE = scanned + "-Mangler på weborder".
          FI-Ean:SCREEN-VALUE = "".
          RETURN.
      END.
      FIND FIRST tt_linjer WHERE tt_linjer.ean = scanned AND tt_linjer.used = FALSE NO-ERROR.
      IF AVAIL tt_linjer THEN DO:
          FI-EAN:SCREEN-VALUE = scanned.
          FI-Varetekst:SCREEN-VALUE = tt_linjer.varetekst.
          FI-Felkod:SENSITIVE = TRUE.
          APPLY "ENTRY" TO FI-Felkod.
          FI-EAN:SENSITIVE = FALSE.
      END.
      ELSE DO:
          FIND FIRST tt_linjer WHERE tt_linjer.ean = scanned AND tt_linjer.used = TRUE NO-ERROR.
          BELL.
          IF tt_linjer.antall <> 1 THEN
              FI-InfoText:SCREEN-VALUE = scanned + "-Feil antall på ordre".
          ELSE
              FI-InfoText:SCREEN-VALUE = scanned + "-Tidligere registrert".
          FI-Ean:SCREEN-VALUE = "".
          RETURN.
      END.
  END.

  RETURN NO-APPLY.

/*   APPLY "GO" TO FRAME {&FRAME-NAME}. */

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
  DISPLAY FI-KOrdreId FI-Kundnamn FI-EAN FI-Varetekst FI-Felkod FI-InfoText 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FI-KOrdreId BR-TT B-Select B-Returnera B-Avsluta 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE funkPopUpBtn3Action C-Win 
PROCEDURE funkPopUpBtn3Action :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lOK AS LOGICAL     NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN INPUT FI-KOrdreId.
        FOR EACH tt_linjer WHERE tt_linjer.used = FALSE:
            DELETE tt_linjer.
        END.
/*         FOR EACH tt_linjer:                        */
/*             MESSAGE artikkelnr                     */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*         END.                                       */
/*         RETURN.                                    */
        RUN getKOrdredata ("RETURNER",FI-KOrdreId,OUTPUT lOK).
    END.

  lOK = TRUE.
  APPLY "GO" TO FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE funkPopUpBtn7Action C-Win 
PROCEDURE funkPopUpBtn7Action :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    
    EMPTY TEMP-TABLE TT_Bonglinje.
    APPLY "GO" TO FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getKOrdredata C-Win 
PROCEDURE getKOrdredata :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cTyp AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER cScannedId AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER lConnected AS LOGICAL NO-UNDO.
DEFINE VARIABLE lOK AS LOGICAL     NO-UNDO.
DEFINE VARIABLE cLayout AS CHAR NO-UNDO. 
DEFINE VARIABLE cBrukerId AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPingHost AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dSum AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lcLinjer AS LONGCHAR NO-UNDO.
DEFINE VARIABLE lcReturKoder AS LONGCHAR NO-UNDO.
DEFINE VARIABLE cKvitto   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dKordreId AS DECIMAL     NO-UNDO.
DEFINE VARIABLE cEksterntOrdrenr AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dReturKOrdre_Id AS DECIMAL     NO-UNDO.
/* "GETKORDRE" */
/* "RETURNER " */
/*     iButikkNr = 24. */
/* MESSAGE iButikkNr                      */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    iSelgernr = 1.
    DO: 
        IF cTyp = "RETURNER" THEN
            TEMP-TABLE tt_linjer:WRITE-JSON("longchar",lcLinjer,TRUE).
        RUN asReturWeborderJF.p (iButikknr,iSelgernr,INPUT cTyp,INPUT cScannedId,INPUT-OUTPUT lcLinjer,OUTPUT lcReturKoder,OUTPUT lOK,OUTPUT cKvitto,OUTPUT cEksterntOrdrenr,OUTPUT dReturKOrdre_Id,OUTPUT cMessage).
        IF lOK AND cTyp = "GETKORDRE" THEN DO:
            FI-Kundnamn:SCREEN-VALUE IN FRAME {&FRAME-NAME} = ENTRY(1,cMessage,CHR(1)).
            fi-infotext:SCREEN-VALUE = ENTRY(2,cMessage,CHR(1)) NO-ERROR.
            FI-EAN:SENSITIVE = TRUE.
            APPLY "ENTRY" TO FI-EAN.
            SELF:SENSITIVE = FALSE.
            IF length(STRING(lcLinjer)) > 0 THEN DO:
                TEMP-TABLE tt_linjer:READ-JSON ("longchar", lcLinjer,"EMPTY").
/*                 FOR EACH tt_linjer WHERE tt_linjer.antall <> 1: */
/*                     tt_linjer.used = TRUE.                      */
/*                 END.                                            */
                FOR EACH tt_linjer WHERE tt_linjer.used = FALSE.

                    CREATE tt_bonglinje.
                    ASSIGN tt_bonglinje.artikkelnr = tt_linjer.artikkelnr
                           tt_bonglinje.linjenr    = tt_linjer.linjenr
                           tt_bonglinje.strekkode  = tt_linjer.ean
                           tt_bonglinje.bongtekst  = tt_linjer.varetekst
                           tt_bonglinje.storrelse  = tt_linjer.storl
                           tt_bonglinje.DivInfo    = IF tt_linjer.antall < 1 THEN "DEBITERA" ELSE ""
                           tt_bonglinje.antall     = 0 /* tt_linjer.antall */
                           tt_bonglinje.linjesum      = tt_linjer.kundpris
                           tt_bonglinje.originaldata  = IF tt_linjer.kundpris = 0 AND tt_linjer.antall < 0 THEN "MANUELL" ELSE ""
                           tt_bonglinje.ttRowid    = ROWID(tt_linjer)
/*                            tt_bonglinje.feilkode   = tt_koder.ReturKodeId */
/*                            tt_linjer.feilkode = tt_koder.ReturKodeId. */
                        tt_linjer.used = FALSE.
                END.
                resetBtnState().
                {&OPEN-QUERY-{&BROWSE-NAME}}

            END.
            IF LENGTH(STRING(lcReturKoder)) > 0 THEN DO:
                TEMP-TABLE tt_koder:READ-JSON ("longchar", lcReturKoder,"EMPTY").
                OUTPUT TO "CLIPBOARD".
                FOR EACH tt_linjer:
                    EXPORT tt_linjer.
                END.
                OUTPUT CLOSE.
            END.
            IF NOT CAN-FIND(FIRST tt_linjer) THEN DO:
                FI-EAN:SENSITIVE = FALSE.
                BELL.
/*                 fi-infotext:SCREEN-VALUE = "Ordre mangler data". */
            END.
        END.
        ELSE IF cTyp = "GETKORDRE" THEN DO:
            BELL.
            fi-infotext:SCREEN-VALUE = ENTRY(2,cMessage,CHR(1)) NO-ERROR.
        END.
/*         ELSE IF cTyp = "RETURNER" THEN DO:                                     */
/*             RUN skrivkvitto (cEksterntOrdrenr,"KO" + STRING(dReturKOrdre_Id)). */
/*         END.                                                                   */
    END. 
    
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitParameter C-Win 
PROCEDURE InitParameter :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*     FIND SysPara WHERE SysPara.SysHId   = 100      AND                */
/*                        SysPara.SysGr    = 11       AND                */
/*                        SysPara.ParaNr   = 31        NO-LOCK NO-ERROR. */
/*     IF AVAIL SysPara THEN                                             */
/*         ASSIGN lVisBruttoDBpakke   = SysPara.Parameter1 = "yes".      */
/*                                                                       */
/*     FIND SysPara WHERE SysPara.SysHId   = 100      AND                */
/*                        SysPara.SysGr    = 11       AND                */
/*                        SysPara.ParaNr   = 32        NO-LOCK NO-ERROR. */
/*     IF AVAIL SysPara THEN                                             */
/*         ASSIGN lVisDB%Pakke   = SysPara.Parameter1 = "yes".           */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE showwidget C-Win 
PROCEDURE showwidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkrivKvitto C-Win 
PROCEDURE SkrivKvitto :
/*------------------------------------------------------------------------------
  Purpose:
  Parameters:  <none>
  Notes:  2 = bräckage
          4 = levreklamation
          5 = inleverans
          6 = överföring
         11 = internt förbruk
         24 = beställning av etiketter
         25 = beställning av etiketter pakkseddel

------------------------------------------------------------------------------*/
/*     DEFINE INPUT  PARAMETER cExterntOrderid AS CHARACTER   NO-UNDO.                                                           */
/*     DEFINE INPUT  PARAMETER cReturnKOrdreID AS CHARACTER   NO-UNDO.                                                           */
/*                                                                                                                               */
/*     DEFINE VARIABLE cVarerad2     AS CHARACTER   NO-UNDO.                                                                     */
/*     DEFINE VARIABLE cBongtekst    AS CHARACTER   NO-UNDO.                                                                     */
/*     DEFINE VARIABLE cKvittoinfo   AS CHARACTER   NO-UNDO.                                                                     */
/*     DEFINE VARIABLE cKassererinfo AS CHARACTER   NO-UNDO.                                                                     */
/*     DEFINE VARIABLE cDatoTid      AS CHARACTER   NO-UNDO.                                                                     */
/*     DEFINE VARIABLE cTitel        AS CHARACTER   NO-UNDO.                                                                     */
/*     DEFINE VARIABLE cSoldKOrdreidTxt AS CHARACTER   NO-UNDO.                                                                  */
/*     DEFINE VARIABLE cReturnKOrdreIDtxt AS CHARACTER   NO-UNDO.                                                                */
/*     DEFINE VARIABLE cExtraInfo    AS CHARACTER   NO-UNDO.                                                                     */
/*     DEFINE VARIABLE cLinjesum AS CHARACTER   NO-UNDO.                                                                         */
/*     DEFINE VARIABLE ii AS INTEGER     NO-UNDO.                                                                                */
/*     DEFINE VARIABLE cSelger AS CHARACTER   NO-UNDO.                                                                           */
/*     RUN TransactionPrint IN hSkriver (2,11,OUTPUT iPTR_Status).                                                               */
/*                                                                                                                               */
/*                                                                                                                               */
/*     FIND selger WHERE selger.selgernr = iSelgernr NO-LOCK NO-ERROR.                                                           */
/*     cSelger = STRING(iSelgernr) + " " + IF AVAIL selger THEN Selger.Fornavn ELSE "".                                          */
/*     cTitel = "RETUR WEBORDER". /* STRING(cSprak = "NO","** PAKKSEDDEL **!!** PACKSEDEL **"). */                               */
/*     cExtraInfo = "ORDERID     : " + cExterntOrderid.                                                                          */
/*                                                                                                                               */
/*     cSoldKOrdreidTxt = "ORDRENR     : " + FI-KOrdreId:SCREEN-VALUE IN FRAME {&FRAME-NAME}.                                    */
/*     cReturnKOrdreIDtxt  = "RETURORDRENR: " + cReturnKOrdreID.                                                                 */
/*                                                                                                                               */
/*     cKvittoInfo =  "Returnert i kasse: " + STRING(iKasseNr).                                                                  */
/*                                                                                                                               */
/*     cDatoTid    =  STRING("Dato    : ") + STRING(TODAY,"99999999") + " " + STRING(TIME,"HH:MM").                              */
/*                                                                                                                               */
/*     DO ii = 1 TO 2:                                                                                                           */
/*         IF lSkrivHeader = FALSE OR lHDRSkriven = FALSE THEN                                                                   */
/*             RUN KvittoHeader IN hSourceProc.                                                                                  */
/*         IF ii = 2 THEN                                                                                                        */
/*             RUN PrintNormal IN hSkriver (2,cDoubleHW + "*** BUTIKKENS EKS ***" +  CHR(10) + CHR(10)).                         */
/*         RUN PrintNormal IN hSkriver (2,cDoubleHW + cTitel +  CHR(10) + CHR(10)).                                              */
/*         RUN PrintNormal IN hSkriver (2,cExtraInfo +  CHR(10)).                                                                */
/*         RUN PrintNormal IN hSkriver (2,cSoldKOrdreidTxt +  CHR(10)).                                                          */
/*         RUN PrintNormal IN hSkriver (2,cReturnKOrdreIDTxt +  CHR(10) + CHR(10)).                                              */
/*                                                                                                                               */
/*         RUN PrintNormal IN hSkriver (2,cKvittoInfo +  CHR(10)).                                                               */
/*         RUN PrintNormal IN hSkriver (2,"Selger  : " + DYNAMIC-FUNCTION('chrConv',cSelger) + CHR(10)).                         */
/*                                                                                                                               */
/*     /*     RUN PrintNormal IN hSkriver (2,cKassererinfo +  CHR(10)). */                                                       */
/*         RUN PrintNormal IN hSkriver (2,cDatoTid +  CHR(10) + CHR(10)).                                                        */
/*                                                                                                                               */
/*         FOR EACH tt_bonglinje:                                                                                                */
/*             cLinjesum = STRING(tt_bonglinje.linjesum,">>9,999.99").                                                           */
/*             cLinjesum = TRIM(cLinjesum).                                                                                      */
/*             cLinjeSum = FILL(" ",10 - length(cLinjesum)) + cLinjesum.                                                         */
/*             RUN PrintNormal IN hSkriver (2,STRING(SUBSTR(tt_bonglinje.bongtekst,1,25),"x(25)") + "  " + cLinjesum + CHR(10)). */
/*         END.                                                                                                                  */
/*         RUN PrintNormal IN hSkriver (2, FILL(CHR(10),3)).                                                                     */
/*         RUN PrintNormal IN hSkriver (2, DYNAMIC-FUNCTION('chrConv',"Tilbakebetaling vil skje via nettbutikken") + CHR(10)).   */
/*         RUN PrintNormal IN hSkriver (2, DYNAMIC-FUNCTION('chrConv',"Det vil ta mellom 3 - 10 virkedager før") + CHR(10)).     */
/*         RUN PrintNormal IN hSkriver (2, DYNAMIC-FUNCTION('chrConv',"tilbakebetalingen er inne på kortet") + CHR(10)).         */
/*                                                                                                                               */
/*         RUN PrintNormal IN hSkriver (2, FILL(CHR(10),7)).                                                                     */
/*                                                                                                                               */
/*         RUN PaperCut IN hSkriver (100).                                                                                       */
/*         RUN PrintEnd IN hSkriver NO-ERROR.                                                                                    */
/*         IF lSkrivHeader = TRUE AND lHDRSkriven = TRUE THEN                                                                    */
/*             RUN KvittoHeader IN hSourceProc.                                                                                  */
/*     END.                                                                                                                      */
/*     RUN TransactionPrint IN hSkriver (2,12,OUTPUT iPTR_Status).                                                               */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getParameter C-Win 
FUNCTION getParameter RETURNS CHAR
      (INPUT  piSysHId AS INT ,
       INPUT  piSysGr  AS INT  ,
       INPUT  piParaNr AS INT ):

    DEFINE BUFFER pSysPara FOR SysPara. 

    FIND pSysPara NO-LOCK WHERE
         pSysPara.SysHId   = piSysHId   AND
         pSysPara.SysGr    = piSysGr    AND
         pSysPara.ParaNr   = piParaNr   NO-ERROR.
    IF AVAIL pSysPAra THEN
        RETURN  pSysPAra.Parameter1. 
    ELSE RETURN "". 

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ResetBtnState C-Win 
FUNCTION ResetBtnState RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:
    B-Select:SENSITIVE = BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ?.
    B-Returnera:SENSITIVE = CAN-FIND(FIRST tt_bonglinje WHERE tt_bonglinje.antall = 1).
END.
/*         RUN SetButtonSensitive(2,FALSE). */
/*     RUN SetButtonSensitive(3,CAN-FIND(FIRST tt_linjer WHERE tt_linjer.used = TRUE)). */

  RETURN FALSE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

