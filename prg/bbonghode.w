&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
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
DEF VAR h_dbonghode AS HANDLE NO-UNDO.
DEF VAR cTekst      AS CHAR   NO-UNDO.
DEF VAR wTittel     AS CHAR   NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Target,Data-Target,Update-Source

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dbonghode.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table rowObject.ButikkNr ~
rowObject.KasseNr rowObject.Dato rowObject.fuKl rowObject.BongNr ~
rowObject.BongStatus rowObject.Makulert rowObject.fuStatusTekst ~
rowObject.Gradering rowObject.OpdKvit rowObject.OpdUtskKopi ~
rowObject.Konvertert rowObject.Belop rowObject.KortType ~
rowObject.MedlemsKort rowObject.MedlemsNr rowObject.KundeKort ~
rowObject.KundeNr rowObject.OverforingsNr rowObject.KassererNr ~
rowObject.KassererNavn rowObject.SelgerNr rowObject.SelgerNavn ~
rowObject.MedlemNavn rowObject.DataSettId rowObject.KundeNavn 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define QUERY-STRING-br_table FOR EACH rowObject NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH rowObject NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_table rowObject
&Scoped-define FIRST-TABLE-IN-QUERY-br_table rowObject


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS T-KortSalg B-Bong FI-BongNr B-Blank-2 ~
B-SokDato T-Alle CB-KortType B-SokMedlem B-SokMedlemsKort CB-Status ~
FI-Kasserer B-SokKasserer FI-Selger FI-Kunde FI-Kundekort FI-Medlem ~
FI-Medlemskort FI-Overfor FI-FraBelop FI-TilBelop CB-Sort B-Blank br_table ~
B-Sok B-SokKunde B-SokKundekort B-SokSelger FI-Dato CB-Butikk CB-Kasse ~
RECT-50 RECT-57 RECT-58 
&Scoped-Define DISPLAYED-OBJECTS T-KortSalg FI-BongNr T-Alle CB-KortType ~
CB-Status FI-Kasserer FI-Selger FI-Kunde FI-Kundekort FI-Medlem ~
FI-Medlemskort FI-Overfor FI-FraBelop FI-TilBelop CB-Sort FI-Tekst FI-Dato ~
CB-Butikk CB-Kasse 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Blank 
     LABEL "Blank filter" 
     SIZE 24 BY 1.

DEFINE BUTTON B-Blank-2  NO-FOCUS
     LABEL "Blank" 
     SIZE 10 BY 1.

DEFINE BUTTON B-Bong 
     LABEL "Gå til bong" 
     SIZE 24 BY 1.14.

DEFINE BUTTON B-Sok 
     LABEL "Aktiver" 
     SIZE 24 BY 1.

DEFINE BUTTON B-SokDato  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokKasserer  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk mot kassererregisteret.".

DEFINE BUTTON B-SokKunde  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk mot kunderegisteret.".

DEFINE BUTTON B-SokKundekort  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk mot kundekortregisteret.".

DEFINE BUTTON B-SokMedlem  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk mot medlemsregisteret".

DEFINE BUTTON B-SokMedlemsKort  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk mot medlemsregisteret".

DEFINE BUTTON B-SokSelger  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk mot selgerregisteret.".

DEFINE VARIABLE CB-Butikk AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Kasse AS CHARACTER FORMAT "X(256)":U INITIAL "0|0" 
     LABEL "Kasse" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "[Alle]","0|0"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE CB-KortType AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "KortType" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "[Alle]",0,
                     "Kundekort",2,
                     "Medlemskort",3
     DROP-DOWN-LIST
     SIZE 24.8 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Sort AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Sortering" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Bongnummer",0,
                     "Status",1,
                     "Kasserer",2,
                     "Selger",3,
                     "KundeKort",4,
                     "MedlemsKort",5,
                     "MedlemsNummer",6,
                     "Beløp",7
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Status AS INTEGER FORMAT "9":U INITIAL 0 
     LABEL "Status" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "[Alle]",0
     DROP-DOWN-LIST
     SIZE 24.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-BongNr AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "BongNr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99":U 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 16.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FraBelop AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Beløp fra" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kasserer AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Kasserer" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kunde AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Kunde" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kundekort AS CHARACTER FORMAT "X(22)":U 
     LABEL "Kundekort" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Medlem AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Medlem" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Medlemskort AS CHARACTER FORMAT "X(22)":U 
     LABEL "Medlemskort" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Overfor AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Overføring" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Selger AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Selger" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Tekst AS CHARACTER FORMAT "X(256)":U INITIAL "Filter" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-TilBelop AS DECIMAL FORMAT "->,>>>,>>9.99":U INITIAL 0 
     LABEL "Beløp til" 
     VIEW-AS FILL-IN 
     SIZE 20 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-50
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 156 BY 6.43.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .2 BY 6.43.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE .2 BY 6.43.

DEFINE VARIABLE T-Alle AS LOGICAL INITIAL no 
     LABEL "Vis alle bonger" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE T-KortSalg AS LOGICAL INITIAL no 
     LABEL "Kortsalg" 
     VIEW-AS TOGGLE-BOX
     SIZE 24.8 BY .81 NO-UNDO.

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
      rowObject.ButikkNr COLUMN-LABEL "Butikk" FORMAT ">>>>>9":U
            WIDTH 8
      rowObject.KasseNr COLUMN-LABEL "Kasse" FORMAT ">>9":U WIDTH 6
      rowObject.Dato FORMAT "99/99/99":U WIDTH 10
      rowObject.fuKl FORMAT "x(8)":U
      rowObject.BongNr FORMAT ">>>>>>>9":U
      rowObject.BongStatus COLUMN-LABEL "St" FORMAT "9":U WIDTH 3
      rowObject.Makulert FORMAT "9":U
      rowObject.fuStatusTekst FORMAT "x(15)":U
      rowObject.Gradering COLUMN-LABEL "Feil" FORMAT ">9":U WIDTH 3
      rowObject.OpdKvit COLUMN-LABEL "OK" FORMAT "*/":U WIDTH 3
      rowObject.OpdUtskKopi COLUMN-LABEL "OU" FORMAT "*/":U WIDTH 3
      rowObject.Konvertert COLUMN-LABEL "Ko" FORMAT "*/":U WIDTH 3
      rowObject.Belop FORMAT "->,>>>,>>9.99":U
      rowObject.KortType FORMAT ">9":U
      rowObject.MedlemsKort FORMAT "X(16)":U
      rowObject.MedlemsNr FORMAT ">>>>>>>>>>>>9":U
      rowObject.KundeKort FORMAT "X(22)":U
      rowObject.KundeNr FORMAT ">>>>>>>>>>>>9":U
      rowObject.OverforingsNr COLUMN-LABEL "Overføring" FORMAT ">>>>>>>9":U
            WIDTH 13.2
      rowObject.KassererNr FORMAT ">>>>>>>>>>>>9":U
      rowObject.KassererNavn COLUMN-LABEL "Kasserer" FORMAT "X(30)":U
            WIDTH 23.6
      rowObject.SelgerNr FORMAT ">>>>>>>>>>>>9":U
      rowObject.SelgerNavn COLUMN-LABEL "Selger" FORMAT "X(30)":U
            WIDTH 25.2
      rowObject.MedlemNavn COLUMN-LABEL "Medlem" FORMAT "X(30)":U
            WIDTH 22.6
      rowObject.DataSettId FORMAT ">>>>>>>>>>>>>9":U WIDTH 12.2
      rowObject.KundeNavn FORMAT "X(30)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE NO-ROW-MARKERS SEPARATORS SIZE 156 BY 13.81 ROW-HEIGHT-CHARS .63 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     T-KortSalg AT ROW 7.1 COL 53.2
     B-Bong AT ROW 3.62 COL 133
     FI-BongNr AT ROW 2.43 COL 140 COLON-ALIGNED
     B-Blank-2 AT ROW 3 COL 115.2 NO-TAB-STOP 
     B-SokDato AT ROW 3 COL 110.4 NO-TAB-STOP 
     T-Alle AT ROW 2 COL 93.4
     CB-KortType AT ROW 4 COL 50.8 COLON-ALIGNED
     B-SokMedlem AT ROW 2 COL 73 NO-TAB-STOP 
     B-SokMedlemsKort AT ROW 3 COL 73 NO-TAB-STOP 
     CB-Status AT ROW 2 COL 12.2 COLON-ALIGNED
     FI-Kasserer AT ROW 3 COL 12.2 COLON-ALIGNED
     B-SokKasserer AT ROW 3 COL 34.4 NO-TAB-STOP 
     FI-Selger AT ROW 4 COL 12.2 COLON-ALIGNED
     FI-Kunde AT ROW 5 COL 12.2 COLON-ALIGNED
     FI-Kundekort AT ROW 6 COL 12.2 COLON-ALIGNED
     FI-Medlem AT ROW 2 COL 50.8 COLON-ALIGNED
     FI-Medlemskort AT ROW 3 COL 50.8 COLON-ALIGNED
     FI-Overfor AT ROW 7 COL 12.4 COLON-ALIGNED
     FI-FraBelop AT ROW 5 COL 50.8 COLON-ALIGNED
     FI-TilBelop AT ROW 6 COL 50.8 COLON-ALIGNED
     CB-Sort AT ROW 6.95 COL 91.4 COLON-ALIGNED
     B-Blank AT ROW 5.76 COL 133
     br_table AT ROW 8.38 COL 2
     B-Sok AT ROW 6.95 COL 133
     B-SokKunde AT ROW 5 COL 34.4 NO-TAB-STOP 
     B-SokKundekort AT ROW 6 COL 34.4 NO-TAB-STOP 
     B-SokSelger AT ROW 4 COL 34.4 NO-TAB-STOP 
     FI-Tekst AT ROW 1 COL 2.8 NO-LABEL
     FI-Dato AT ROW 3 COL 91.4 COLON-ALIGNED
     CB-Butikk AT ROW 4 COL 91.4 COLON-ALIGNED
     CB-Kasse AT ROW 5 COL 91.4 COLON-ALIGNED
     RECT-50 AT ROW 1.71 COL 2
     RECT-57 AT ROW 1.71 COL 81
     RECT-58 AT ROW 1.71 COL 131.2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataBrowser
   Data Source: "dbonghode.w"
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
         HEIGHT             = 21.57
         WIDTH              = 157.6.
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
   NOT-VISIBLE Size-to-Fit Custom                                       */
/* BROWSE-TAB br_table B-Blank F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 5.

/* SETTINGS FOR FILL-IN FI-Tekst IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "rowObject"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > _<SDO>.rowObject.ButikkNr
"rowObject.ButikkNr" "Butikk" ? "integer" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _FldNameList[2]   > _<SDO>.rowObject.KasseNr
"rowObject.KasseNr" "Kasse" ? "integer" ? ? ? ? ? ? no ? no no "6" yes no no "U" "" ""
     _FldNameList[3]   > _<SDO>.rowObject.Dato
"rowObject.Dato" ? ? "date" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[4]   = _<SDO>.rowObject.fuKl
     _FldNameList[5]   = _<SDO>.rowObject.BongNr
     _FldNameList[6]   > _<SDO>.rowObject.BongStatus
"rowObject.BongStatus" "St" ? "integer" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" ""
     _FldNameList[7]   = _<SDO>.rowObject.Makulert
     _FldNameList[8]   > _<SDO>.rowObject.fuStatusTekst
"rowObject.fuStatusTekst" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[9]   > _<SDO>.rowObject.Gradering
"rowObject.Gradering" "Feil" ? "integer" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" ""
     _FldNameList[10]   > _<SDO>.rowObject.OpdKvit
"rowObject.OpdKvit" "OK" ? "logical" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" ""
     _FldNameList[11]   > _<SDO>.rowObject.OpdUtskKopi
"rowObject.OpdUtskKopi" "OU" ? "logical" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" ""
     _FldNameList[12]   > _<SDO>.rowObject.Konvertert
"rowObject.Konvertert" "Ko" ? "logical" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" ""
     _FldNameList[13]   = _<SDO>.rowObject.Belop
     _FldNameList[14]   = _<SDO>.rowObject.KortType
     _FldNameList[15]   = _<SDO>.rowObject.MedlemsKort
     _FldNameList[16]   = _<SDO>.rowObject.MedlemsNr
     _FldNameList[17]   = _<SDO>.rowObject.KundeKort
     _FldNameList[18]   = _<SDO>.rowObject.KundeNr
     _FldNameList[19]   > _<SDO>.rowObject.OverforingsNr
"rowObject.OverforingsNr" "Overføring" ? "decimal" ? ? ? ? ? ? no ? no no "13.2" yes no no "U" "" ""
     _FldNameList[20]   = _<SDO>.rowObject.KassererNr
     _FldNameList[21]   > _<SDO>.rowObject.KassererNavn
"rowObject.KassererNavn" "Kasserer" ? "character" ? ? ? ? ? ? no ? no no "23.6" yes no no "U" "" ""
     _FldNameList[22]   = _<SDO>.rowObject.SelgerNr
     _FldNameList[23]   > _<SDO>.rowObject.SelgerNavn
"rowObject.SelgerNavn" "Selger" ? "character" ? ? ? ? ? ? no ? no no "25.2" yes no no "U" "" ""
     _FldNameList[24]   > _<SDO>.rowObject.MedlemNavn
"rowObject.MedlemNavn" "Medlem" ? "character" ? ? ? ? ? ? no ? no no "22.6" yes no no "U" "" ""
     _FldNameList[25]   > _<SDO>.rowObject.DataSettId
"rowObject.DataSettId" ? ? "decimal" ? ? ? ? ? ? no ? no no "12.2" yes no no "U" "" ""
     _FldNameList[26]   = _<SDO>.rowObject.KundeNavn
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
ON CHOOSE OF B-Blank IN FRAME F-Main /* Blank filter */
DO:
    ASSIGN
      CB-Status:SCREEN-VALUE      = "0"
      FI-Kasserer:SCREEN-VALUE    = "0"
      FI-Selger:SCREEN-VALUE      = "0"
      FI-Kunde:SCREEN-VALUE       = "0"
      FI-KundeKort:SCREEN-VALUE   = ""
      FI-Medlem:SCREEN-VALUE      = "0"
      FI-Medlemskort:SCREEN-VALUE = ""
      FI-Overfor:SCREEN-VALUE     = "0"
      FI-FraBelop:SCREEN-VALUE    = "0"
      FI-TilBelop:SCREEN-VALUE    = "0"
      CB-KortType:SCREEN-VALUE    = "0"
      CB-Sort:SCREEN-VALUE        = "0"
      FI-Dato:SCREEN-VALUE        = ""
      CB-Butikk:SCREEN-VALUE      = "0"
      CB-Kasse:SCREEN-VALUE       = "0|0"
      T-KortSalg:SCREEN-VALUE     = "no"
      .
  APPLY "CHOOSE":U TO B-Sok.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Blank-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Blank-2 bTableWin
ON CHOOSE OF B-Blank-2 IN FRAME F-Main /* Blank */
DO:
  ASSIGN
      FI-Dato:SCREEN-VALUE = ""
      .
  APPLY "TAB":U TO FI-Dato.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Bong
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Bong bTableWin
ON CHOOSE OF B-Bong IN FRAME F-Main /* Gå til bong */
DO:
  ASSIGN
      FI-BongNr
      .
   RUN SokBong.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Sok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Sok bTableWin
ON CHOOSE OF B-Sok IN FRAME F-Main /* Aktiver */
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


&Scoped-define SELF-NAME B-SokKasserer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokKasserer bTableWin
ON CHOOSE OF B-SokKasserer IN FRAME F-Main /* ... */
OR F10 OF FI-Kasserer
DO:
  ASSIGN
      cTekst = FI-Kasserer:SCREEN-VALUE
      .
  /* Kaller søkerutine */
  RUN gforsalj.w (
    INPUT-OUTPUT cTekst,      /* Returstreng - chr(1) separert */
    "",                       /* Feltliste   Komma separert */
    "",                       /* Feltverdier (chr(1) sep)   */ 
    "",                       /* Feltoperatorer komma separert */
    FI-Kasserer:SCREEN-VALUE  /* Post som markøren skal stille seg på. */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
    /* Legger opp verdier I de aktuelle feltene */
    ASSIGN
      FI-Kasserer:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
      .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokKunde bTableWin
ON CHOOSE OF B-SokKunde IN FRAME F-Main /* ... */
OR F10 OF FI-Kunde
DO:
  ASSIGN
      cTekst = FI-Kunde:SCREEN-VALUE
      .
  /* Kaller søkerutine */
  RUN gkunde.w (
    INPUT-OUTPUT cTekst,      /* Returstreng - chr(1) separert */
    "",                       /* Feltliste   Komma separert */
    "",                       /* Feltverdier (chr(1) sep)   */ 
    "",                       /* Feltoperatorer komma separert */
    FI-Kunde:SCREEN-VALUE  /* Post som markøren skal stille seg på. */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
    /* Legger opp verdier I de aktuelle feltene */
    ASSIGN
      FI-Kunde:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
      .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokKundekort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokKundekort bTableWin
ON CHOOSE OF B-SokKundekort IN FRAME F-Main /* ... */
OR F10 OF FI-KundeKort
DO:
  ASSIGN
      cTekst = FI-KundeKort:SCREEN-VALUE
      .
  /* Kaller søkerutine */
  RUN gkundekort.w (
    INPUT-OUTPUT cTekst,       /* Returstreng - chr(1) separert */
    "",                        /* Feltliste   Komma separert */
    "",                        /* Feltverdier (chr(1) sep)   */ 
    "EQ",                      /* Feltoperatorer komma separert */
    FI-kundekort:SCREEN-VALUE  /* Post som markøren skal stille seg på. */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
    /* Legger opp verdier I de aktuelle feltene */
    ASSIGN
      FI-KundeKort:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
      .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokMedlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokMedlem bTableWin
ON CHOOSE OF B-SokMedlem IN FRAME F-Main /* ... */
OR F10 OF FI-Medlem
DO:
  ASSIGN
      cTekst = FI-Medlem:SCREEN-VALUE
      .
  /* Kaller søkerutine */
  RUN gmedlem.w (
    INPUT-OUTPUT cTekst,       /* Returstreng - chr(1) separert */
    "",                        /* Feltliste   Komma separert */
    "",                        /* Feltverdier (chr(1) sep)   */ 
    "EQ",                      /* Feltoperatorer komma separert */
    FI-Medlem:SCREEN-VALUE  /* Post som markøren skal stille seg på. */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
    /* Legger opp verdier I de aktuelle feltene */
    ASSIGN
      FI-Medlem:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
      .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokMedlemsKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokMedlemsKort bTableWin
ON CHOOSE OF B-SokMedlemsKort IN FRAME F-Main /* ... */
OR F10 OF FI-MedlemsKort
DO:
  ASSIGN
      cTekst = FI-MedlemsKort:SCREEN-VALUE
      .
  /* Kaller søkerutine */
  RUN gmedlemskort.w (
    INPUT-OUTPUT cTekst,       /* Returstreng - chr(1) separert */
    "",                        /* Feltliste   Komma separert */
    "",                        /* Feltverdier (chr(1) sep)   */ 
    "EQ",                      /* Feltoperatorer komma separert */
    FI-Medlemskort:SCREEN-VALUE  /* Post som markøren skal stille seg på. */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
    /* Legger opp verdier I de aktuelle feltene */
    ASSIGN
      FI-Medlemskort:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
      .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokSelger
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokSelger bTableWin
ON CHOOSE OF B-SokSelger IN FRAME F-Main /* ... */
OR F10 OF FI-Selger
DO:
  ASSIGN
      cTekst = FI-Selger:SCREEN-VALUE
      .
  /* Kaller søkerutine */
  RUN gselger.w (
    INPUT-OUTPUT cTekst,      /* Returstreng - chr(1) separert */
    "",                       /* Feltliste   Komma separert */
    "",                       /* Feltverdier (chr(1) sep)   */ 
    "",                       /* Feltoperatorer komma separert */
    FI-Selger:SCREEN-VALUE  /* Post som markøren skal stille seg på. */
    ).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.
  IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
  DO:
    /* Legger opp verdier I de aktuelle feltene */
    ASSIGN
      FI-Selger:SCREEN-VALUE = ENTRY(2,cTekst,CHR(1))
      .
  END.
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


&Scoped-define SELF-NAME CB-Butikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Butikk bTableWin
ON VALUE-CHANGED OF CB-Butikk IN FRAME F-Main /* Butikk */
DO:
  /* Henter kasseliste for butikk, og setter skjermverdi til alle kasser. */
  RUN GetKasseListe IN h_dproclib (INPUT int(CB-Butikk:SCREEN-VALUE), OUTPUT cTekst).
  IF cTekst <> "" THEN
  assign
      CB-Kasse:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = "[Alle],0|0" + "," + cTekst.
  CB-Kasse:SCREEN-VALUE = "0|0".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Kasse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Kasse bTableWin
ON VALUE-CHANGED OF CB-Kasse IN FRAME F-Main /* Kasse */
DO:
  /* Endres kassen, skal butikken følge med */
  CB-Butikk:SCREEN-VALUE = TRIM(entry(1,ENTRY(1,CB-Kasse:SCREEN-VALUE," "),"|")).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-BongNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-BongNr bTableWin
ON RETURN OF FI-BongNr IN FRAME F-Main /* BongNr */
DO:
  APPLY "choose":U TO B-Bong.
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
          T-Alle = FALSE
          .
      DISPLAY T-Alle.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject bTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
  ASSIGN
      h_dbonghode = DYNAMIC-FUNCTION('getDataSource':U)
      .

  RUN GetButikkListe     IN h_dproclib (OUTPUT cTekst).
  IF cTekst <> "" THEN
  ASSIGN
      CB-Butikk:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = 
        CB-Butikk:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} + "," + cTekst.
  
  RUN GetKasseListe      IN h_dproclib (INPUT 0, OUTPUT cTekst).
  IF cTekst <> "" THEN
  assign
      CB-Kasse:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} =
        CB-Kasse:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} + "," + cTekst.

  RUN GetBongStatusListe IN h_dproclib (OUTPUT cTekst).
  IF cTekst <> "" THEN
    ASSIGN
      CB-Status:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = 
        CB-Status:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} + "," + cTekst.

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokBong bTableWin 
PROCEDURE SokBong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cFelt    AS CHAR NO-UNDO.
  DEF VAR cVerdier AS CHAR NO-UNDO.
           
  /* Posisjonerer qyery på ønsket post. */
  DYNAMIC-FUNCTION('findRowWhere':U IN h_dbonghode,
     INPUT cFelt    + (IF cFelt = "" THEN "" ELSE ",") + "BongNr" /* CHARACTER */,
     INPUT cVerdier + (IF cVerdier = "" 
                         THEN "" 
                         ELSE CHR(1)) + 
                      string(FI-BongNr) /* CHARACTER */,
     INPUT "EQ,EQ" /* CHARACTER */).
  /* Signalerer til de andre objektene. */
  RUN dataAvailable IN h_dbonghode
    ( INPUT "SAME" /* CHARACTER */).

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
      pcFeltListe = "BongStatus,KassererNr,SelgerNr,KundeNr,KundeKort,MedlemsNr,MedlemsKort,OverforingsNr,Belop,KortType,Dato,ButikkNr,KasseNr,flBetalingskort"
      .

  ASSIGN FRAME {&FRAME-NAME}
      CB-Status
      FI-Kasserer
      FI-Selger
      FI-Kunde
      FI-KundeKort
      FI-Medlem
      FI-Medlemskort
      FI-Overfor
      FI-FraBelop
      FI-TilBelop
      CB-KortType
      CB-Sort
      FI-Dato
      CB-Butikk
      CB-Kasse
      T-KortSalg
      T-Alle
      .

  IF CB-Status <> 0 THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "BongStatus"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + 
                   string(CB-Status)
        pcOperator = pcOperator + 
                     (IF pcOperator = ""
                         THEN ""
                         ELSE ",") + 
                     "=".
  
 IF FI-Kasserer <> 0 THEN
   ASSIGN
      pcFields = pcFields + 
                 (IF pcFields = ""
                    THEN ""
                    ELSE ",") + 
                 "KassererNr"
      pcValues = pcValues + 
                 (IF pcValues = ""
                    THEN ""
                    ELSE chr(1)) + 
                 string(FI-Kasserer)
     pcOperator = pcOperator + 
                  (IF pcOperator = ""
                      THEN ""
                      ELSE ",") + 
                  "=".
  
 IF FI-Selger <> 0 THEN
   ASSIGN
      pcFields = pcFields + 
                 (IF pcFields = ""
                    THEN ""
                    ELSE ",") + 
                 "SelgerNr"
      pcValues = pcValues + 
                 (IF pcValues = ""
                    THEN ""
                    ELSE chr(1)) + 
                 string(FI-Selger)
     pcOperator = pcOperator + 
                  (IF pcOperator = ""
                      THEN ""
                      ELSE ",") + 
                  "=".

 IF FI-Kunde <> 0 THEN
   ASSIGN
      pcFields = pcFields + 
                 (IF pcFields = ""
                    THEN ""
                    ELSE ",") + 
                 "KundeNr"
      pcValues = pcValues + 
                 (IF pcValues = ""
                    THEN ""
                    ELSE chr(1)) + 
                 string(FI-Kunde)
     pcOperator = pcOperator + 
                  (IF pcOperator = ""
                      THEN ""
                      ELSE ",") + 
                  "=".

 IF FI-KundeKort <> "" THEN
   ASSIGN
      pcFields = pcFields + 
                 (IF pcFields = ""
                    THEN ""
                    ELSE ",") + 
                 "KundeKort"
      pcValues = pcValues + 
                 (IF pcValues = ""
                    THEN ""
                    ELSE chr(1)) + 
                 "'" + string(FI-KundeKort) + "'"
     pcOperator = pcOperator + 
                  (IF pcOperator = ""
                      THEN ""
                      ELSE ",") + 
                  "=".

 IF FI-Medlem <> 0 THEN
   ASSIGN
      pcFields = pcFields + 
                 (IF pcFields = ""
                    THEN ""
                    ELSE ",") + 
                 "MedlemsNr"
      pcValues = pcValues + 
                 (IF pcValues = ""
                    THEN ""
                    ELSE chr(1)) + 
                 string(FI-Medlem)
     pcOperator = pcOperator + 
                  (IF pcOperator = ""
                      THEN ""
                      ELSE ",") + 
                  "=".

 IF FI-Medlemskort <> "" THEN
   ASSIGN
      pcFields = pcFields + 
                 (IF pcFields = ""
                    THEN ""
                    ELSE ",") + 
                 "MedlemsKort"
      pcValues = pcValues + 
                 (IF pcValues = ""
                    THEN ""
                    ELSE chr(1)) + 
                 "'" + string(FI-MedlemsKort) + "'"
     pcOperator = pcOperator + 
                  (IF pcOperator = ""
                      THEN ""
                      ELSE ",") + 
                  "=".

 IF FI-Overfor <> 0 THEN
   ASSIGN
      pcFields = pcFields + 
                 (IF pcFields = ""
                    THEN ""
                    ELSE ",") + 
                 "OverforingsNr"
      pcValues = pcValues + 
                 (IF pcValues = ""
                    THEN ""
                    ELSE chr(1)) + 
                 string(FI-Overfor)
     pcOperator = pcOperator + 
                  (IF pcOperator = ""
                      THEN ""
                      ELSE ",") + 
                  "=".

 IF FI-FraBelop <> 0 THEN
   ASSIGN
      pcFields = pcFields + 
                 (IF pcFields = ""
                    THEN ""
                    ELSE ",") + 
                 "Belop"
      pcValues = pcValues + 
                 (IF pcValues = ""
                    THEN ""
                    ELSE chr(1)) + 
                 string(FI-FraBelop)
     pcOperator = pcOperator + 
                  (IF pcOperator = ""
                      THEN ""
                      ELSE ",") + 
                  ">=".

 IF FI-TilBelop <> 0 THEN
   ASSIGN
      pcFields = pcFields + 
                 (IF pcFields = ""
                    THEN ""
                    ELSE ",") + 
                 "Belop"
      pcValues = pcValues + 
                 (IF pcValues = ""
                    THEN ""
                    ELSE chr(1)) + 
                 string(FI-TilBelop)
     pcOperator = pcOperator + 
                  (IF pcOperator = ""
                      THEN ""
                      ELSE ",") + 
                  "<=".

  IF CB-KortType <> 0 THEN
    ASSIGN
       pcFields = pcFields + 
                  (IF pcFields = ""
                     THEN ""
                     ELSE ",") + 
                  "KortType"
       pcValues = pcValues + 
                  (IF pcValues = ""
                     THEN ""
                     ELSE chr(1)) + 
                  string(CB-KortType)
      pcOperator = pcOperator + 
                   (IF pcOperator = ""
                       THEN ""
                       ELSE ",") + 
                   "=".

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
                   "=".

  IF CB-Butikk <> 0 THEN
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
                  string(CB-Butikk)
      pcOperator = pcOperator + 
                   (IF pcOperator = ""
                       THEN ""
                       ELSE ",") + 
                   "=".

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
  IF T-KortSalg THEN
      ASSIGN
        pcFields = pcFields + 
                   (IF pcFields = ""
                      THEN ""
                      ELSE ",") + 
                   "flBetalingskort"
        pcValues = pcValues + 
                   (IF pcValues = ""
                      THEN ""
                      ELSE chr(1)) + "true"
        pcOperator = pcOperator + 
                     (IF pcOperator = ""
                         THEN ""
                         ELSE ",") + 
                     "=".

  ASSIGN
      pcSort     = IF CB-Sort = 0 THEN
                     "By BongNr"
                   ELSE IF CB-Sort = 1 THEN
                     "By BongStatus By GruppeNr By KasseNr By Dato By BongNr"
                   ELSE IF CB-Sort = 2 THEN
                     "By KassererNr By GruppeNr By KasseNr By Dato By BongNr"
                   ELSE IF CB-Sort = 3 THEN
                     "By SelgerNr By GruppeNr By KasseNr By Dato By BongNr"
                   ELSE IF CB-Sort = 4 THEN
                     "By KundeKort By GruppeNr By KasseNr By Dato By BongNr"
                   ELSE IF CB-Sort = 5 THEN
                     "By MedlemsKort By GruppeNr By KasseNr By Dato By BongNr"
                   ELSE IF CB-Sort = 6 THEN
                     "By MedlemsNr By GruppeNr By KasseNr By Dato By BongNr"
                   ELSE IF CB-Sort = 7 THEN
                     "By Belop by GruppeNr By KasseNr By Belop"
                   ELSE 
                     "By ButikkNr By GruppeNr By KasseNr By Dato By BongNr"
      .

  RUN SokSdo IN h_dbonghode (pcFields,
                             pcValues,
                             pcSort,
                             pcOperator,
                             T-Alle).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

