&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
          data             PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS bTableWin 
/*------------------------------------------------------------------------

  File: adm2\src\browser.w

  Description: SmartDataBrowser Object

  Input Parameters:
      <none>

  Output Parameters:
      <none>

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
DEF VAR wTittel  AS CHAR   NO-UNDO.
DEF VAR h_dfiler AS HANDLE NO-UNDO.
DEF VAR cTekst   AS CHAR   NO-UNDO.

DEF VAR pitest AS INT INITIAL 10.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Target,Data-Target,Update-Source

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dfiler.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table rowObject.FilId rowObject.Innlest ~
rowObject.Oppdatert rowObject.Overfort rowObject.FilNavn ~
rowObject.fuFilTypeTekst rowObject.Dato rowObject.Kl rowObject.Storrelse ~
rowObject.Katalog rowObject.Feil rowObject.Dobbel rowObject.Backup ~
rowObject.Slettet rowObject.InnlestDato rowObject.fuInnlestKl 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH rowObject NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH rowObject NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_table rowObject
&Scoped-define FIRST-TABLE-IN-QUERY-br_table rowObject


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-49 CB-Innlest FI-Dato CB-Feil ~
CB-Behandlet FI-Storrelse CB-Dobbel CB-Overfort CB-FilType B-BlankAllt ~
CB-Sort FI-Katalog B-Sok B-Blank2 br_table B-Blank B-SokDato 
&Scoped-Define DISPLAYED-OBJECTS CB-Innlest FI-Dato CB-Feil CB-Behandlet ~
FI-Storrelse CB-Dobbel CB-Overfort CB-FilType CB-Sort FI-Katalog FI-Label 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Blank  NO-FOCUS
     LABEL "Blank" 
     SIZE 10 BY 1.

DEFINE BUTTON B-Blank2  NO-FOCUS
     LABEL "Blank" 
     SIZE 10 BY 1.

DEFINE BUTTON B-BlankAllt 
     LABEL "Blank filter" 
     SIZE 35 BY 1.

DEFINE BUTTON B-Sok 
     LABEL "Aktiver filter" 
     SIZE 35 BY 1.

DEFINE BUTTON B-SokDato  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE CB-Behandlet AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Behandlet" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Dobbel AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Dobbel" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Feil AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Feil" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE CB-FilType AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Filtype" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Innlest AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Innlest" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Overfort AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Overført" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Sort AS CHARACTER FORMAT "X(256)":U INITIAL "FilId" 
     LABEL "Sortering" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Default","FilId"
     DROP-DOWN-LIST
     SIZE 35 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Katalog AS CHARACTER FORMAT "X(256)":U 
     LABEL "Katalog" 
     VIEW-AS FILL-IN 
     SIZE 37 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Label AS CHARACTER FORMAT "X(256)":U INITIAL "Filter" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Storrelse AS INTEGER FORMAT "->>>,>>>,>>9":U INITIAL 0 
     LABEL "Størrelse" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-49
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 139 BY 4.76.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE TEMP-TABLE RowObject NO-UNDO
    {{&DATA-FIELD-DEFS}}
    {src/adm2/robjflds.i}.

DEFINE QUERY br_table FOR 
      rowObject SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table bTableWin _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      rowObject.FilId FORMAT ">>>>>>>>>>>>9":U WIDTH 14
      rowObject.Innlest COLUMN-LABEL "Inn" FORMAT "*/":U WIDTH 3
      rowObject.Oppdatert COLUMN-LABEL "Opp" FORMAT "*/":U WIDTH 3
      rowObject.Overfort FORMAT "*/":U WIDTH 3
      rowObject.FilNavn FORMAT "X(40)":U
      rowObject.fuFilTypeTekst FORMAT "x(15)":U
      rowObject.Dato FORMAT "99/99/99":U WIDTH 9.4
      rowObject.Kl FORMAT "X(8)":U WIDTH 9
      rowObject.Storrelse FORMAT ">>>,>>>,>>9":U WIDTH 13.2
      rowObject.Katalog FORMAT "X(40)":U WIDTH 30.2
      rowObject.Feil FORMAT "*/":U WIDTH 4.2
      rowObject.Dobbel COLUMN-LABEL "Dob" FORMAT "*/":U WIDTH 3
      rowObject.Backup COLUMN-LABEL "Back" FORMAT "*/":U WIDTH 8
      rowObject.Slettet FORMAT "*/":U
      rowObject.InnlestDato FORMAT "99/99/99":U
      rowObject.fuInnlestKl FORMAT "x(8)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE SEPARATORS MULTIPLE SIZE 139 BY 16.43 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     CB-Innlest AT ROW 2.19 COL 10.6 COLON-ALIGNED
     FI-Dato AT ROW 2.19 COL 58 COLON-ALIGNED
     CB-Feil AT ROW 2.19 COL 102 COLON-ALIGNED
     CB-Behandlet AT ROW 3.14 COL 10.6 COLON-ALIGNED
     FI-Storrelse AT ROW 3.14 COL 58 COLON-ALIGNED
     CB-Dobbel AT ROW 3.14 COL 102 COLON-ALIGNED
     CB-Overfort AT ROW 4.1 COL 10.6 COLON-ALIGNED
     CB-FilType AT ROW 4.1 COL 58 COLON-ALIGNED
     B-BlankAllt AT ROW 4.19 COL 104
     CB-Sort AT ROW 5.05 COL 58 COLON-ALIGNED
     FI-Katalog AT ROW 5.1 COL 10.6 COLON-ALIGNED
     B-Sok AT ROW 5.29 COL 104
     B-Blank2 AT ROW 3.14 COL 84.6 NO-TAB-STOP 
     br_table AT ROW 6.48 COL 1
     B-Blank AT ROW 2.19 COL 84.6 NO-TAB-STOP 
     B-SokDato AT ROW 2.19 COL 80 NO-TAB-STOP 
     FI-Label AT ROW 1 COL 2.6 NO-LABEL
     RECT-49 AT ROW 1.71 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataBrowser
   Data Source: "dfiler.w"
   Allow: Basic,Browse
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW bTableWin ASSIGN
         HEIGHT             = 22.1
         WIDTH              = 139.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB bTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/browser.i}
{sdo/dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW bTableWin
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table B-Blank2 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1.

/* SETTINGS FOR FILL-IN FI-Label IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "rowObject"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > _<SDO>.rowObject.FilId
"rowObject.FilId" ? ? "decimal" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > _<SDO>.rowObject.Innlest
"rowObject.Innlest" "Inn" ? "logical" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > _<SDO>.rowObject.Oppdatert
"rowObject.Oppdatert" "Opp" ? "logical" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > _<SDO>.rowObject.Overfort
"rowObject.Overfort" ? ? "logical" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = _<SDO>.rowObject.FilNavn
     _FldNameList[6]   = _<SDO>.rowObject.fuFilTypeTekst
     _FldNameList[7]   > _<SDO>.rowObject.Dato
"rowObject.Dato" ? ? "date" ? ? ? ? ? ? no ? no no "9.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > _<SDO>.rowObject.Kl
"rowObject.Kl" ? ? "character" ? ? ? ? ? ? no ? no no "9" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > _<SDO>.rowObject.Storrelse
"rowObject.Storrelse" ? ? "integer" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > _<SDO>.rowObject.Katalog
"rowObject.Katalog" ? ? "character" ? ? ? ? ? ? no ? no no "30.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > _<SDO>.rowObject.Feil
"rowObject.Feil" ? ? "logical" ? ? ? ? ? ? no ? no no "4.2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > _<SDO>.rowObject.Dobbel
"rowObject.Dobbel" "Dob" ? "logical" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > _<SDO>.rowObject.Backup
"rowObject.Backup" "Back" ? "logical" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   = _<SDO>.rowObject.Slettet
     _FldNameList[15]   = _<SDO>.rowObject.InnlestDato
     _FldNameList[16]   = _<SDO>.rowObject.fuInnlestKl
     _Query            is NOT OPENED
*/  /* BROWSE br_table */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME B-Blank
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank bTableWin
ON CHOOSE OF B-Blank IN FRAME F-Main /* Blank */
DO:
  ASSIGN
      FI-Dato:SCREEN-VALUE = ""
      .
  APPLY "TAB":U TO FI-Dato.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Blank2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank2 bTableWin
ON CHOOSE OF B-Blank2 IN FRAME F-Main /* Blank */
DO:
  ASSIGN
      FI-Storrelse:SCREEN-VALUE = ""
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-BlankAllt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BlankAllt bTableWin
ON CHOOSE OF B-BlankAllt IN FRAME F-Main /* Blank filter */
DO:
  ASSIGN
      CB-Innlest:SCREEN-VALUE   = "0"
      CB-Behandlet:SCREEN-VALUE = "0"
      CB-Feil:SCREEN-VALUE      = "0"
      CB-Dobbel:SCREEN-VALUE    = "0"
      CB-Sort:SCREEN-VALUE      = "FilId"
      FI-Katalog:SCREEN-VALUE   = ""
      .
  APPLY "CHOOSE":U TO B-Sok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sok bTableWin
ON CHOOSE OF B-Sok IN FRAME F-Main /* Aktiver filter */
DO:
  RUN StartSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokDato bTableWin
ON CHOOSE OF B-SokDato IN FRAME F-Main /* ... */
OR F10 of FI-Dato
DO:
  assign 
    FI-Dato = date(FI-Dato:screen-value in frame {&FRAME-NAME})
    .

  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-Dato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   

  end. /* FRAME */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br_table
&Scoped-define SELF-NAME br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON CTRL-END OF br_table IN FRAME F-Main
DO:
  APPLY "END":U TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON CTRL-HOME OF br_table IN FRAME F-Main
DO:
  APPLY "HOME":U TO BROWSE {&BROWSE-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON END OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsend.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON HOME OF br_table IN FRAME F-Main
DO:
  {src/adm2/brshome.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON OFF-END OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsoffnd.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON OFF-HOME OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsoffhm.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON ROW-DISPLAY OF br_table IN FRAME F-Main
DO:
    IF RowObject.Slettet THEN
      ASSIGN 
          RowObject.FilNavn:FGCOLOR IN BROWSE Br_Table = 13
          .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON ROW-ENTRY OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsentry.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON ROW-LEAVE OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsleave.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON SCROLL-NOTIFY OF br_table IN FRAME F-Main
DO:
  {src/adm2/brsscrol.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON VALUE-CHANGED OF br_table IN FRAME F-Main
DO:
  {src/adm2/brschnge.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK bTableWin 


/* ***************************  Main Block  *************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
RUN initializeObject.        
&ENDIF

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI bTableWin  _DEFAULT-DISABLE
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
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetFocusedRow bTableWin 
PROCEDURE GetFocusedRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER piRowNr AS INT NO-UNDO.
  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          piRowNr = Br_Table:FOCUSED-ROW
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSelectedRows bTableWin 
PROCEDURE GetSelectedRows :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pcValgteDatasett AS CHAR NO-UNDO.

  DEF VAR piLoop1          AS INT    NO-UNDO.
  DEF VAR pcDataSettId     AS CHAR   NO-UNDO.
  DEF VAR hQuery           AS HANDLE NO-UNDO.
  DEF VAR hBuff            AS HANDLE NO-UNDO.
  DEF VAR hField           AS HANDLE NO-UNDO.

  ASSIGN
    hQuery = br_table:QUERY IN FRAME {&FRAME-NAME}
    hBuff  = hQuery:GET-BUFFER-HANDLE(1) /* Default antall poster er 200 */
                                         /* i RowsToBatch                */
    hField = hBuff:BUFFER-FIELD('FilId')
    .
  /* Satser på at bruker ikke velger så mange poster at variabelen sprekker */
  DO piLoop1 = 1 TO br_table:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
      br_table:FETCH-SELECTED-ROW(piLoop1).

    assign
        pcDataSettId      = hField:BUFFER-VALUE
        pcValgteDatasett  = pcValgteDatasett + 
                           (IF pcValgteDatasett = ""
                              THEN ""
                              ELSE chr(1)) + 
                           pcDataSettId
        .
  END.
  /*br_table:FETCH-SELECTED-ROW(1).*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject bTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  /* Code placed here will execute PRIOR to standard behavior. */
  cTekst = "1,11,5".
  RUN GetSysPara  IN h_dproclib (INPUT-OUTPUT cTekst).
  IF cTekst <> "" THEN
  ASSIGN
      CB-FilType:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = 
        "[Alle],0" + "," + cTekst.

  cTekst = "1,11,10".
  RUN GetSysPara  IN h_dproclib (INPUT-OUTPUT cTekst).
  IF cTekst <> "" THEN
  ASSIGN
      CB-Innlest:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cTekst.

  cTekst = "1,11,11".
  RUN GetSysPara  IN h_dproclib (INPUT-OUTPUT cTekst).
  IF cTekst <> "" THEN
  ASSIGN
      CB-Behandlet:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cTekst.

  cTekst = "1,11,12".
  RUN GetSysPara  IN h_dproclib (INPUT-OUTPUT cTekst).
  IF cTekst <> "" THEN
  ASSIGN
      CB-Overfort:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cTekst.

  cTekst = "1,11,13".
  RUN GetSysPara  IN h_dproclib (INPUT-OUTPUT cTekst).
  IF cTekst <> "" THEN
  ASSIGN
      CB-Sort:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cTekst.

  cTekst = "1,11,14".
  RUN GetSysPara  IN h_dproclib (INPUT-OUTPUT cTekst).
  IF cTekst <> "" THEN
  ASSIGN
      CB-Feil:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cTekst.

  cTekst = "1,11,15".
  RUN GetSysPara  IN h_dproclib (INPUT-OUTPUT cTekst).
  IF cTekst <> "" THEN
  ASSIGN
      CB-Dobbel:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cTekst.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  RUN SetBrowseFocus (?).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetBrowseFocus bTableWin 
PROCEDURE SetBrowseFocus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cRadListe AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE         iCount    AS INTEGER    NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    IF br_table:FOCUSED-ROW = ? THEN
        RETURN.

    IF cRadListe = ? THEN DO:            
        IF br_table:NUM-SELECTED-ROWS > 1 THEN
            br_table:DESELECT-ROWS().
        br_table:SELECT-FOCUSED-ROW().
    END.
    ELSE DO iCount = 1 TO NUM-ENTRIES(cRadListe,CHR(1)):
        /* Litt FY FY her... */
        FIND Filer NO-LOCK WHERE
            Filer.FilId = INT(ENTRY(iCount,cRadListe,CHR(1))) NO-ERROR.
        IF AVAILABLE Filer THEN
        DO:
            br_table:SELECT-ROW(INT(ENTRY(iCount,cRadListe,CHR(1)))) NO-ERROR.
        END.
    END.
    APPLY "ENTRY" TO br_table.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartSok bTableWin 
PROCEDURE StartSok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcFields    AS CHAR NO-UNDO.
  DEF VAR pcValues    AS CHAR NO-UNDO.
  DEF VAR pcSort      AS CHAR NO-UNDO.
  DEF VAR pcOperator  AS CHAR NO-UNDO.
  DEF VAR pcFeltListe AS CHAR NO-UNDO.

  ASSIGN
      pcFeltListe = "Innlest,Oppdatert,Overfort,Feil,Dobbel,Dato,Filtype,Storrelse,Katalog"
      .

  ASSIGN FRAME {&FRAME-NAME}
      FI-Dato
      FI-Storrelse
      CB-Innlest
      CB-Behandlet
      CB-Feil
      CB-Dobbel
      CB-Overfort
      CB-Sort
      CB-FilType
      FI-Katalog
      .

  ASSIGN
      h_dfiler = DYNAMIC-FUNCTION('getDataSource':U)
      .

  IF CB-Innlest <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "Innlest"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   (IF CB-Innlest = 1
                      THEN "no"
                      ELSE "yes")
        pcOperator = pcOperator + 
                    (IF pcOperator = ""
                       THEN ""
                       ELSE ",") + 
                    "="
        .
  IF CB-Behandlet <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "Oppdatert"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   (IF CB-Behandlet = 1
                      THEN "no"
                      ELSE "yes")
        pcOperator = pcOperator + 
                    (IF pcOperator = ""
                       THEN ""
                       ELSE ",") + 
                    "="
        .
  IF CB-Overfort <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "Overfort"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   (IF CB-Overfort = 1
                      THEN "no"
                      ELSE "yes")
        pcOperator = pcOperator + 
                    (IF pcOperator = ""
                       THEN ""
                       ELSE ",") + 
                    "="
        .
  IF CB-Feil <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "Feil"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   (IF CB-Feil = 1
                      THEN "no"
                      ELSE "yes")
        pcOperator = pcOperator + 
                    (IF pcOperator = ""
                       THEN ""
                       ELSE ",") + 
                    "="
        .
  IF CB-Dobbel <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "Dobbel"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   (IF CB-Dobbel = 1
                      THEN "no"
                      ELSE "yes")
        pcOperator = pcOperator + 
                    (IF pcOperator = ""
                       THEN ""
                       ELSE ",") + 
                    "="
       .
  IF FI-Dato <> ? THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "Dato"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   string(FI-Dato)
        pcOperator = pcOperator + 
                    (IF pcOperator = ""
                       THEN ""
                       ELSE ",") + 
                    "="
        .
  IF CB-FilType <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "Filtype"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   STRING(CB-FilType)
        pcOperator = pcOperator + 
                    (IF pcOperator = ""
                       THEN ""
                       ELSE ",") + 
                    "="
        .
  IF FI-Storrelse <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "Storrelse"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   string(FI-Storrelse)
        pcOperator = pcOperator + 
                    (IF pcOperator = ""
                       THEN ""
                       ELSE ",") + 
                    "="
       .
  IF FI-Katalog <> "" THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "Katalog"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   STRING(INPUT FI-Katalog)
        pcOperator = pcOperator + 
                    (IF pcOperator = ""
                       THEN ""
                       ELSE ",") + 
                    "begins"
        .
  ASSIGN
      pcSort = IF CB-Sort = "FilId"
                 THEN CB-Sort + " Descending"
               ELSE IF CB-Sort = "FilNavn" 
                 THEN "By FilNavn"
               ELSE IF CB-Sort = "Dato" 
                 THEN "By Dato"
               ELSE IF CB-Sort = "Størrelse" 
                 THEN "By Storrelse"
               ELSE
                   "FilId"
      .

/* MESSAGE pcFields SKIP                  */
/*         pcValues SKIP                  */
/*         pcOperator SKIP                */
/*         pcFeltListe SKIP               */
/*         pcSort                         */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */

  RUN SokSdo IN h_dfiler (pcFields,
                          pcValues,
                          pcSort,
                          pcOperator,
                          pcFeltListe).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

