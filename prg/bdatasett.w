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
DEF VAR h_ddatasett AS HANDLE NO-UNDO.
DEF VAR cTekst      AS CHAR   NO-UNDO.
DEF VAR wTittel     AS CHAR   NO-UNDO.
DEF VAR cOldValue   AS CHAR   NO-UNDO.
DEF VAR piInt       AS INT    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Target,Data-Target,Update-Source

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "ddatasett.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table rowObject.ButikkNr ~
rowObject.fuButikkKortNavn rowObject.KasseNr rowObject.DataSettId ~
rowObject.fuStatusTekst rowObject.fuFilTypeTekst ~
rowObject.fuBehandletStatus rowObject.Dato rowObject.AntallLinjer ~
rowObject.SettNr rowObject.SettStatus rowObject.FilId rowObject.FilType ~
rowObject.Behandlet rowObject.GruppeNr rowObject.fuKasseNavn ~
rowObject.pfFlagg 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH rowObject NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH rowObject NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_table rowObject
&Scoped-define FIRST-TABLE-IN-QUERY-br_table rowObject


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-48 br_table FI-Butikk B-SokButikk ~
CB-Kasse CB-MStatus CB-BehStatus FI-Dato CB-FilType CB-Sort B-SpinUp ~
B-SpinDown B-BlankAllt B-Sok T-AlleDatasett B-Blank B-Blank-2 B-SokDato 
&Scoped-Define DISPLAYED-OBJECTS FI-Butikk CB-Kasse CB-MStatus CB-BehStatus ~
FI-ButNavn FI-Dato CB-FilType CB-Sort T-AlleDatasett FI-Label 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Blank  NO-FOCUS
     LABEL "Blank" 
     SIZE 10 BY 1.

DEFINE BUTTON B-Blank-2  NO-FOCUS
     LABEL "Blank" 
     SIZE 10 BY 1.

DEFINE BUTTON B-BlankAllt 
     LABEL "Blank filter" 
     SIZE 26.2 BY 1.

DEFINE BUTTON B-Sok 
     LABEL "Aktiver filter" 
     SIZE 26.2 BY 1.

DEFINE BUTTON B-SokButikk  NO-FOCUS
     LABEL "..." 
     SIZE 4.6 BY 1.

DEFINE BUTTON B-SokDato  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SpinDown 
     LABEL "-" 
     SIZE 4.2 BY .52
     FONT 6.

DEFINE BUTTON B-SpinUp 
     LABEL "+" 
     SIZE 4.2 BY .52
     FONT 6.

DEFINE BUTTON BUTTON-1 
     LABEL "Button 1" 
     SIZE 8 BY 1.14.

DEFINE VARIABLE CB-BehStatus AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Behandlet status" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE CB-FilType AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Filtype" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Kasse AS CHARACTER FORMAT "X(256)":U INITIAL "0|0" 
     LABEL "Kasse" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "[Alle]","0|0"
     DROP-DOWN-LIST
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE CB-MStatus AS INTEGER FORMAT ">9":U INITIAL 0 
     LABEL "Mottak status" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 41 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Sort AS CHARACTER FORMAT "X(256)":U INITIAL "Default" 
     LABEL "Sortering" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "DataSettId","Default"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Butikk AS CHARACTER FORMAT "XXXXXX":U INITIAL "[Alle]" 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1 NO-UNDO.

DEFINE VARIABLE FI-ButNavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN NATIVE 
     SIZE 26.2 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Label AS CHARACTER FORMAT "X(256)":U INITIAL "Filter" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-48
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 141.6 BY 4.38.

DEFINE VARIABLE T-AlleDatasett AS LOGICAL INITIAL no 
     LABEL "Alle datasett" 
     VIEW-AS TOGGLE-BOX
     SIZE 17 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE TEMP-TABLE RowObject
    {{&DATA-FIELD-DEFS}}
    {src/adm2/robjflds.i}.

DEFINE QUERY br_table FOR 
      rowObject SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE br_table
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br_table bTableWin _STRUCTURED
  QUERY br_table NO-LOCK DISPLAY
      rowObject.ButikkNr FORMAT ">>>>>9":U
      rowObject.fuButikkKortNavn FORMAT "x(8)":U
      rowObject.KasseNr FORMAT ">>9":U
      rowObject.DataSettId FORMAT ">>>>>>>>>>>>>9":U WIDTH 15
      rowObject.fuStatusTekst COLUMN-LABEL "MottakStatus" FORMAT "x(12)":U
            WIDTH 17.2
      rowObject.fuFilTypeTekst COLUMN-LABEL "FilType" FORMAT "x(15)":U
            WIDTH 16.2
      rowObject.fuBehandletStatus FORMAT "x(10)":U WIDTH 15
      rowObject.Dato FORMAT "99/99/99":U WIDTH 9.4
      rowObject.AntallLinjer COLUMN-LABEL "AntLinjer" FORMAT "->,>>>,>>9":U
      rowObject.SettNr FORMAT ">>>9":U WIDTH 11.8
      rowObject.SettStatus FORMAT "9":U WIDTH 11.2
      rowObject.FilId FORMAT ">>>>>>>>>>>>9":U WIDTH 14
      rowObject.FilType FORMAT ">9":U
      rowObject.Behandlet FORMAT ">9":U
      rowObject.GruppeNr FORMAT ">9":U
      rowObject.fuKasseNavn FORMAT "x(30)":U WIDTH 32.6
      rowObject.pfFlagg FORMAT ">9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 141.6 BY 16.14 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 6.24 COL 1
     FI-Butikk AT ROW 1.91 COL 18 COLON-ALIGNED
     B-SokButikk AT ROW 1.91 COL 30.2
     CB-Kasse AT ROW 2.91 COL 18 COLON-ALIGNED
     CB-MStatus AT ROW 3.86 COL 18 COLON-ALIGNED
     CB-BehStatus AT ROW 4.81 COL 18 COLON-ALIGNED
     FI-ButNavn AT ROW 1.91 COL 32.8 COLON-ALIGNED NO-LABEL
     FI-Dato AT ROW 1.91 COL 76.2 COLON-ALIGNED
     CB-FilType AT ROW 2.91 COL 76.2 COLON-ALIGNED
     CB-Sort AT ROW 4.81 COL 76.2 COLON-ALIGNED
     B-SpinUp AT ROW 1.86 COL 110
     B-SpinDown AT ROW 2.43 COL 110
     B-BlankAllt AT ROW 1.91 COL 115
     BUTTON-1 AT ROW 3.38 COL 115
     B-Sok AT ROW 4.81 COL 115
     T-AlleDatasett AT ROW 3.62 COL 124
     B-Blank AT ROW 1.91 COL 100 NO-TAB-STOP 
     B-Blank-2 AT ROW 1.91 COL 61.2 NO-TAB-STOP 
     B-SokDato AT ROW 1.91 COL 95.2 NO-TAB-STOP 
     FI-Label AT ROW 1 COL 2.6 NO-LABEL
     RECT-48 AT ROW 1.62 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataBrowser
   Data Source: "ddatasett.w"
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
         HEIGHT             = 21.48
         WIDTH              = 142.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB bTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/browser.i}
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW bTableWin
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit L-To-R,COLUMNS                    */
/* BROWSE-TAB br_table RECT-48 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3.

/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-1:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR FILL-IN FI-ButNavn IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Label IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "rowObject"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   = _<SDO>.rowObject.ButikkNr
     _FldNameList[2]   = _<SDO>.rowObject.fuButikkKortNavn
     _FldNameList[3]   = _<SDO>.rowObject.KasseNr
     _FldNameList[4]   > _<SDO>.rowObject.DataSettId
"rowObject.DataSettId" ? ? "decimal" ? ? ? ? ? ? no ? no no "15" yes no no "U" "" ""
     _FldNameList[5]   > _<SDO>.rowObject.fuStatusTekst
"rowObject.fuStatusTekst" "MottakStatus" ? "character" ? ? ? ? ? ? no "?" no no "17.2" yes no no "U" "" ""
     _FldNameList[6]   > _<SDO>.rowObject.fuFilTypeTekst
"rowObject.fuFilTypeTekst" "FilType" ? "character" ? ? ? ? ? ? no "?" no no "16.2" yes no no "U" "" ""
     _FldNameList[7]   > _<SDO>.rowObject.fuBehandletStatus
"rowObject.fuBehandletStatus" ? ? "character" ? ? ? ? ? ? no "?" no no "15" yes no no "U" "" ""
     _FldNameList[8]   > _<SDO>.rowObject.Dato
"rowObject.Dato" ? ? "date" ? ? ? ? ? ? no ? no no "9.4" yes no no "U" "" ""
     _FldNameList[9]   > _<SDO>.rowObject.AntallLinjer
"rowObject.AntallLinjer" "AntLinjer" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > _<SDO>.rowObject.SettNr
"rowObject.SettNr" ? ? "integer" ? ? ? ? ? ? no ? no no "11.8" yes no no "U" "" ""
     _FldNameList[11]   > _<SDO>.rowObject.SettStatus
"rowObject.SettStatus" ? ? "integer" ? ? ? ? ? ? no ? no no "11.2" yes no no "U" "" ""
     _FldNameList[12]   > _<SDO>.rowObject.FilId
"rowObject.FilId" ? ? "decimal" ? ? ? ? ? ? no ? no no "14" yes no no "U" "" ""
     _FldNameList[13]   = _<SDO>.rowObject.FilType
     _FldNameList[14]   = _<SDO>.rowObject.Behandlet
     _FldNameList[15]   = _<SDO>.rowObject.GruppeNr
     _FldNameList[16]   > _<SDO>.rowObject.fuKasseNavn
"rowObject.fuKasseNavn" ? ? "character" ? ? ? ? ? ? no "?" no no "32.6" yes no no "U" "" ""
     _FldNameList[17]   = _<SDO>.rowObject.pfFlagg
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


&Scoped-define SELF-NAME B-Blank-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank-2 bTableWin
ON CHOOSE OF B-Blank-2 IN FRAME F-Main /* Blank */
DO:
  ASSIGN
      FI-Butikk:SCREEN-VALUE  = "[Alle]"
      FI-ButNavn:SCREEN-VALUE = ""
      .
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-BlankAllt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-BlankAllt bTableWin
ON CHOOSE OF B-BlankAllt IN FRAME F-Main /* Blank filter */
DO:
  ASSIGN
      FI-Butikk:SCREEN-VALUE    = "[Alle]"
      FI-ButNavn:SCREEN-VALUE   = ""
      CB-Kasse:SCREEN-VALUE     = "0|0"
      CB-MStatus:SCREEN-VALUE   = "0"
      CB-BehStatus:SCREEN-VALUE = "0"
      FI-Dato:SCREEN-VALUE      = ""
      CB-FilType:SCREEN-VALUE   = "0"
      CB-Sort:SCREEN-VALUE      = "Default"
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


&Scoped-define SELF-NAME B-SokButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokButikk bTableWin
ON CHOOSE OF B-SokButikk IN FRAME F-Main /* ... */
OR "F10":U OF FI-Butikk
DO:

   DO WITH FRAME {&FRAME-NAME}:
     IF FI-Butikk:SCREEN-VALUE = "[Alle]"
       THEN cTekst = "".
     ELSE cTekst = FI-Butikk:SCREEN-VALUE. 
   END.

  /* Kaller søkerutine */
  RUN gButiker.w (
    INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
    "", /* Feltliste avgrensningsfelt (kommaseparert) */
    "", /* Feltverdier (chr(1) sep) */ 
    FI-Butikk:SCREEN-VALUE /* Post markøren skal stå på */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
      /* Legger opp verdier I de aktuelle feltene */
      ASSIGN
        FI-Butikk:SCREEN-VALUE   = ENTRY(2,cTekst,CHR(1))
        FI-ButNavn:SCREEN-VALUE  = ENTRY(3,cTekst,CHR(1))
        .

        RETURN NO-APPLY.
  END.

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


&Scoped-define SELF-NAME B-SpinDown
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SpinDown bTableWin
ON CHOOSE OF B-SpinDown IN FRAME F-Main /* - */
DO:
    ASSIGN FRAME {&FRAME-NAME}
        FI-Dato 
        FI-Dato = FI-Dato - 1
        FI-Dato:SCREEN-VALUE = STRING(FI-Dato)
        .
    APPLY "choose":U TO B-Sok.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SpinUp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SpinUp bTableWin
ON CHOOSE OF B-SpinUp IN FRAME F-Main /* + */
DO:
    ASSIGN FRAME {&FRAME-NAME}
        FI-Dato 
        FI-Dato = FI-Dato + 1
        FI-Dato:SCREEN-VALUE = STRING(FI-Dato)
        .
    APPLY "choose":U TO B-Sok.
    RETURN NO-APPLY.
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
  ASSIGN
    RowObject.fuStatusTekst:BGCOLOR IN BROWSE br_table = 
                                    (IF RowObject.SettStatus = 2
                                       THEN 10
                                     ELSE IF RowObject.SettStatus = 3
                                       THEN 11
                                     ELSE IF RowObject.SettStatus = 8
                                       THEN 12
                                     ELSE IF RowObject.SettStatus = 9
                                       THEN 13
                                     ELSE ?)
    RowObject.fuBehandletStatus:BGCOLOR IN BROWSE br_table = 
                                    (IF RowObject.Behandlet = 2
                                       THEN 14
                                     ELSE IF RowObject.Behandlet = 3
                                       THEN 10
                                     ELSE IF RowObject.Behandlet = 4
                                           THEN 8
                                     ELSE IF RowObject.Behandlet = 5
                                               THEN 11
                                     ELSE IF RowObject.Behandlet = 9
                                       THEN 13
                                     ELSE ?)
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


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 bTableWin
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
      MESSAGE 
          DYNAMIC-FUNCTION('getQueryWhere':U IN h_ddatasett)
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Kasse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Kasse bTableWin
ON VALUE-CHANGED OF CB-Kasse IN FRAME F-Main /* Kasse */
DO:
  /* Endres kassen, skal butikken følge med */
  FI-Butikk:SCREEN-VALUE = TRIM(entry(1,ENTRY(1,CB-Kasse:SCREEN-VALUE," "),"|")).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Butikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Butikk bTableWin
ON VALUE-CHANGED OF FI-Butikk IN FRAME F-Main /* Butikk */
DO:
  ASSIGN
      piInt = INT(FI-Butikk:SCREEN-VALUE)
      NO-ERROR.
  IF ERROR-STATUS:ERROR  AND FI-Butikk:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "[Alle]" THEN
  DO:
      MESSAGE "Ugyldig butikknummer"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN
          FI-Butikk:SCREEN-VALUE    = "[Alle]"
          FI-ButNavn:SCREEN-VALUE   = ""
          .
      RETURN NO-APPLY.
  END.

  IF FI-Butikk:SCREEN-VALUE = "[Alle]"
    THEN cTekst = "".
  ELSE cTekst = FI-Butikk:SCREEN-VALUE. 

  /* Henter kasseliste for butikk, og setter skjermverdi til alle kasser. */
  RUN GetKasseListe IN h_dproclib (INPUT int(FI-Butikk:SCREEN-VALUE), OUTPUT cTekst).
  IF cTekst <> "" THEN
  assign
      CB-Kasse:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = "[Alle],0|0" + "," + cTekst.
  CB-Kasse:SCREEN-VALUE = "0|0".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Dato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato bTableWin
ON DELETE-CHARACTER OF FI-Dato IN FRAME F-Main /* Dato */
DO:
  ASSIGN
      SELF:SCREEN-VALUE = ""
      .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisableAlle bTableWin 
PROCEDURE DisableAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          T-AlleDataSett = FALSE
          .
      DISPLAY T-AlleDataSett.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
    hField = hBuff:BUFFER-FIELD('DataSettId')
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HideAlle bTableWin 
PROCEDURE HideAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          T-AlleDataSett:HIDDEN = TRUE 
          .
  END.

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
  
  ASSIGN
      h_ddatasett = DYNAMIC-FUNCTION('getDataSource':U).

  /*
  RUN GetButikkListe     IN h_dproclib (OUTPUT cTekst).
  IF cTekst <> "" THEN
  ASSIGN
      DB-Butikk:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = 
        DB-Butikk:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} + "," + cTekst.
  */      
  
  RUN GetKasseListe      IN h_dproclib (INPUT 0, OUTPUT cTekst).
  IF cTekst <> "" THEN
  assign
      CB-Kasse:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} =
        CB-Kasse:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} + "," + cTekst.

  RUN GetMottakStatus    IN h_dproclib (OUTPUT cTekst).
  IF cTekst <> "" THEN
  ASSIGN
      CB-MStatus:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = 
        CB-MStatus:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} + "," + cTekst.

  RUN GetBehandletStatus IN h_dproclib (OUTPUT cTekst).
  IF cTekst <> "" THEN
  ASSIGN
      CB-BehStatus:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = 
        CB-BehStatus:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} + "," + cTekst.

  RUN GetFilTypeTekst IN h_dproclib (OUTPUT cTekst).
  IF cTekst <> "" THEN
  ASSIGN
      CB-FilType:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = 
        CB-FilType:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} + "," + cTekst.

  cTekst = "1,11,16".
  RUN GetSysPara IN h_dproclib (INPUT-OUTPUT cTekst).
  IF cTekst <> "" THEN
  ASSIGN
      CB-Sort:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cTekst
      FI-Butikk:SCREEN-VALUE    = "[Alle]"
      FI-ButNavn:SCREEN-VALUE   = ""
      .
  
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  
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
  DEF VAR pcFields   AS CHAR NO-UNDO.
  DEF VAR pcValues   AS CHAR NO-UNDO.
  DEF VAR pcSort     AS CHAR NO-UNDO.
  DEF VAR pcOperator AS CHAR NO-UNDO.

  ASSIGN 
      piInt = INT(FI-Butikk:SCREEN-VALUE IN FRAME {&FRAME-NAME})
      NO-ERROR.
  IF ERROR-STATUS:ERROR AND FI-Butikk:SCREEN-VALUE IN FRAME {&FRAME-NAME} <> "[Alle]" THEN
  DO:
      MESSAGE "Ugyldig butikknummer"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN
          FI-Butikk:SCREEN-VALUE    = "[Alle]"
          FI-ButNavn:SCREEN-VALUE   = ""
          .
      RETURN NO-APPLY.
  END.

  ASSIGN FRAME {&FRAME-NAME}
      FI-Dato
      FI-Butikk
      CB-Kasse
      CB-MStatus
      CB-BehStatus
      CB-Sort
      CB-FilType
      T-AlleDataSett
      .

  IF FI-Butikk = "[Alle]"
    THEN cTekst = "".
  ELSE cTekst = FI-Butikk. 

  IF int(cTekst) <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "ButikkNr"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   string(cTekst)
        .
  IF CB-Kasse <> "0|0" THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "KasseNr"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   entry(2,(CB-Kasse),"|")
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
        .
  IF CB-MStatus <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "SettStatus"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   string(CB-MStatus)
        .
  IF CB-BehStatus <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "Behandlet"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   string(CB-BehStatus)
        .
  IF CB-FilType <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "FilType"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   string(CB-FilType)
        .
  ASSIGN
      pcSort = IF CB-Sort = "DataSettId"
                 THEN "By DataSettId" 
               ELSE IF CB-Sort = "ButikkNr" 
                 THEN "By FilId By ButikkNr By GruppeNr By KasseNr By Dato By DataSettId"
               ELSE IF CB-Sort = "Dato" 
                 THEN "By FilId By Dato By ButikkNr By GruppeNr By KasseNr By DataSettId"
               ELSE IF CB-Sort = "SettStatus" 
                 THEN "By FilId By SettStatus By ButikkNr By GruppeNr By KasseNr By Dato By DataSettId"
               ELSE IF CB-Sort = "Behandlet" 
                 THEN "By FilId By Behandlet By ButikkNr By GruppeNr By KasseNr By Dato By DataSettId"
               ELSE
                   "By DataSettId"
      pcOperator = ""
      .

  RUN SokSdo IN h_ddatasett (pcFields,
                             pcValues,
                             pcSort,
                             pcOperator,
                             T-AlleDataSett).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

