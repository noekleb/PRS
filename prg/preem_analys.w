&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          data             PROGRESS
*/
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

DEFINE VARIABLE dBokfDato AS DATE       NO-UNDO.
DEFINE VARIABLE iButikkNr AS INTEGER    NO-UNDO.
DEFINE VARIABLE dBokforingsid AS DECIMAL    NO-UNDO.

DEFINE BUFFER bprBMdata FOR prBMdata.

DEFINE BUFFER sokBokforingsdag FOR bokforingsdag.
DEFINE BUFFER bprPGdata FOR prPGdata.
DEFINE BUFFER bprn9HGdata FOR prn9HGdata.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME Br-BdSk

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Bokforingsdag prBMData prKDData prn9HGData ~
prPGData Skift

/* Definitions for BROWSE Br-BdSk                                       */
&Scoped-define FIELDS-IN-QUERY-Br-BdSk Bokforingsdag.ButikkNr ~
Bokforingsdag.Dato Bokforingsdag.GruppeNr Bokforingsdag.BokforingsId ~
Bokforingsdag.pfFlagg ~
substr(string(bokforingsdag.datetime),1,8) + "-" + string(int(substr(string(bokforingsdag.datetime),9)),"HH:MM:SS") ~
" " 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br-BdSk 
&Scoped-define QUERY-STRING-Br-BdSk FOR EACH Bokforingsdag ~
      WHERE  if input FI-Butik = 0 then TRUE else Bokforingsdag.ButikkNr = FI-Butik NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br-BdSk OPEN QUERY Br-BdSk FOR EACH Bokforingsdag ~
      WHERE  if input FI-Butik = 0 then TRUE else Bokforingsdag.ButikkNr = FI-Butik NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br-BdSk Bokforingsdag
&Scoped-define FIRST-TABLE-IN-QUERY-Br-BdSk Bokforingsdag


/* Definitions for BROWSE Br-BmData                                     */
&Scoped-define FIELDS-IN-QUERY-Br-BmData prBMData.Betalingstype ~
getBMTTIdTxt (prBMData.Betalingstype,prBMData.Subtype) prBMData.SubType ~
prBMData.SubSubType prBMData.Belop ~
getSubtypeName (prBMData.Betalingstype,prBMData.Subtype) prBMData.Konto 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br-BmData 
&Scoped-define QUERY-STRING-Br-BmData FOR EACH prBMData ~
      WHERE prBMData.ButikkNr = iButikkNr and ~
data.prBMData.Dato = dBokfDato NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br-BmData OPEN QUERY Br-BmData FOR EACH prBMData ~
      WHERE prBMData.ButikkNr = iButikkNr and ~
data.prBMData.Dato = dBokfDato NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br-BmData prBMData
&Scoped-define FIRST-TABLE-IN-QUERY-Br-BmData prBMData


/* Definitions for BROWSE BR-kddata                                     */
&Scoped-define FIELDS-IN-QUERY-BR-kddata prKDData.Vg prKDData.SubType ~
prKDData.SubTypeNavn prKDData.antall prKDData.Volum 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-kddata 
&Scoped-define QUERY-STRING-BR-kddata FOR EACH prKDData ~
      WHERE prKDData.ButikkNr = iButikknr and ~
data.prKDData.Dato = dBokfDato NO-LOCK ~
    BY prKDData.Dato ~
       BY prKDData.SubType ~
        BY prKDData.Vg INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BR-kddata OPEN QUERY BR-kddata FOR EACH prKDData ~
      WHERE prKDData.ButikkNr = iButikknr and ~
data.prKDData.Dato = dBokfDato NO-LOCK ~
    BY prKDData.Dato ~
       BY prKDData.SubType ~
        BY prKDData.Vg INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BR-kddata prKDData
&Scoped-define FIRST-TABLE-IN-QUERY-BR-kddata prKDData


/* Definitions for BROWSE br-n9HGData                                   */
&Scoped-define FIELDS-IN-QUERY-br-n9HGData prn9HGData.Hg prn9HGData.Konto ~
prn9HGData.SumVolumAntall prn9HGData.SumVaresalg prn9HGData.MvaKr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br-n9HGData 
&Scoped-define QUERY-STRING-br-n9HGData FOR EACH prn9HGData ~
      WHERE prn9HGData.Butikknr = iButikkNr and ~
data.prn9HGData.Dato = dBokfDato NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-br-n9HGData OPEN QUERY br-n9HGData FOR EACH prn9HGData ~
      WHERE prn9HGData.Butikknr = iButikkNr and ~
data.prn9HGData.Dato = dBokfDato NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br-n9HGData prn9HGData
&Scoped-define FIRST-TABLE-IN-QUERY-br-n9HGData prn9HGData


/* Definitions for BROWSE Br-pgData                                     */
&Scoped-define FIELDS-IN-QUERY-Br-pgData getHg(prpgdata.varegr) ~
prPGData.VareGr prPGData.Konto prPGData.SumVolumAntall prPGData.SumVaresalg ~
prPGData.MvaKr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br-pgData 
&Scoped-define QUERY-STRING-Br-pgData FOR EACH prPGData ~
      WHERE prPGData.Butikknr = iButikkNr and ~
data.prPGData.Dato = dBokfDato NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br-pgData OPEN QUERY Br-pgData FOR EACH prPGData ~
      WHERE prPGData.Butikknr = iButikkNr and ~
data.prPGData.Dato = dBokfDato NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br-pgData prPGData
&Scoped-define FIRST-TABLE-IN-QUERY-Br-pgData prPGData


/* Definitions for BROWSE Br-Skift                                      */
&Scoped-define FIELDS-IN-QUERY-Br-Skift Skift.BokforingsId Skift.SkiftNr ~
Skift.n9SkiftNr Skift.SkiftId Skift.pfFlagg ~
substr(string(skift.datetime),1,8) + "-" + string(int(substr(string(skift.datetime),9)),"HH:MM:SS") ~
Skift.datetime 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br-Skift 
&Scoped-define QUERY-STRING-Br-Skift FOR EACH Skift ~
      WHERE Skift.BokforingsId = dBokforingsid NO-LOCK ~
    BY Skift.SkiftNr INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br-Skift OPEN QUERY Br-Skift FOR EACH Skift ~
      WHERE Skift.BokforingsId = dBokforingsid NO-LOCK ~
    BY Skift.SkiftNr INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br-Skift Skift
&Scoped-define FIRST-TABLE-IN-QUERY-Br-Skift Skift


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-Br-BdSk}~
    ~{&OPEN-QUERY-Br-BmData}~
    ~{&OPEN-QUERY-BR-kddata}~
    ~{&OPEN-QUERY-br-n9HGData}~
    ~{&OPEN-QUERY-Br-pgData}~
    ~{&OPEN-QUERY-Br-Skift}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FI-Butik FI-Dato Br-BdSk BR-kddata B-Rapport ~
B-ClipPrPG Br-Skift Br-pgData B-Clip Br-BmData B-Clipn9HG br-n9HGData ~
FI-BFdag FI-KDdata FI-Skift FI-PGdata FI-BetMedel FI-n9HGdata 
&Scoped-Define DISPLAYED-OBJECTS FI-Butik FI-Dato FI-BFdag FI-KDdata ~
FI-Skift FI-PGdata FI-BetMedel FI-n9HGdata 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getBMTTIdTxt C-Win 
FUNCTION getBMTTIdTxt RETURNS CHARACTER
  ( INPUT iBettype AS INTEGER, INPUT iSubtype AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getHg C-Win 
FUNCTION getHg RETURNS CHARACTER
  ( INPUT iVg AS INTEGER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getSubtypeName C-Win 
FUNCTION getSubtypeName RETURNS CHARACTER
  ( INPUT iBettype AS integer, INPUT iSubType AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Clip 
     LABEL "Dumpa till Clip" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Clipn9HG 
     LABEL "Dumpa till Clip" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-ClipPrPG 
     LABEL "Dumpa till Clip" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Rapport 
     LABEL "Rapport" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE FI-BetMedel AS CHARACTER FORMAT "X(256)":U INITIAL "     Betalningsmedel" 
      VIEW-AS TEXT 
     SIZE 26 BY .71
     BGCOLOR 11 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-BFdag AS CHARACTER FORMAT "X(256)":U INITIAL "     Bokföringsdag" 
      VIEW-AS TEXT 
     SIZE 24 BY .71
     BGCOLOR 11 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Butik AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Butik" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Dato AS DATE FORMAT "99/99/99":U 
     LABEL "Datum" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KDdata AS CHARACTER FORMAT "X(256)":U INITIAL "     KDdata" 
      VIEW-AS TEXT 
     SIZE 20 BY .71
     BGCOLOR 11 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-n9HGdata AS CHARACTER FORMAT "X(256)":U INITIAL "     n9HGdata" 
      VIEW-AS TEXT 
     SIZE 20 BY .71
     BGCOLOR 11 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-PGdata AS CHARACTER FORMAT "X(256)":U INITIAL "     PGdata" 
      VIEW-AS TEXT 
     SIZE 20 BY .71
     BGCOLOR 11 FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Skift AS CHARACTER FORMAT "X(256)":U INITIAL "             Skift" 
      VIEW-AS TEXT 
     SIZE 24 BY .71
     BGCOLOR 11 FONT 6 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br-BdSk FOR 
      Bokforingsdag SCROLLING.

DEFINE QUERY Br-BmData FOR 
      prBMData SCROLLING.

DEFINE QUERY BR-kddata FOR 
      prKDData SCROLLING.

DEFINE QUERY br-n9HGData FOR 
      prn9HGData SCROLLING.

DEFINE QUERY Br-pgData FOR 
      prPGData SCROLLING.

DEFINE QUERY Br-Skift FOR 
      Skift SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br-BdSk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br-BdSk C-Win _STRUCTURED
  QUERY Br-BdSk NO-LOCK DISPLAY
      Bokforingsdag.ButikkNr FORMAT ">>>>>9":U
      Bokforingsdag.Dato FORMAT "99/99/99":U
      Bokforingsdag.GruppeNr FORMAT ">9":U
      Bokforingsdag.BokforingsId FORMAT "->>>>>>>>>>>>9":U
      Bokforingsdag.pfFlagg FORMAT ">9":U WIDTH 18
      substr(string(bokforingsdag.datetime),1,8) + "-" + string(int(substr(string(bokforingsdag.datetime),9)),"HH:MM:SS") FORMAT "x(20)":U
      " " WIDTH 21.4
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 85 BY 5.48 ROW-HEIGHT-CHARS .81 EXPANDABLE.

DEFINE BROWSE Br-BmData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br-BmData C-Win _STRUCTURED
  QUERY Br-BmData NO-LOCK DISPLAY
      prBMData.Betalingstype FORMAT ">>>9":U
      getBMTTIdTxt (prBMData.Betalingstype,prBMData.Subtype) COLUMN-LABEL "Betalingstype" FORMAT "x(20)":U
      prBMData.SubType FORMAT ">>9":U
      prBMData.SubSubType FORMAT ">>9":U
      prBMData.Belop FORMAT "->>,>>>,>>9.99":U WIDTH 27.4
      getSubtypeName (prBMData.Betalingstype,prBMData.Subtype) COLUMN-LABEL "Namn" FORMAT "x(15)":U
            WIDTH 10.6
      prBMData.Konto FORMAT ">>9999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 100 BY 11.43 EXPANDABLE.

DEFINE BROWSE BR-kddata
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-kddata C-Win _STRUCTURED
  QUERY BR-kddata NO-LOCK DISPLAY
      prKDData.Vg FORMAT "->>>>>9":U
      prKDData.SubType FORMAT ">>9":U
      prKDData.SubTypeNavn FORMAT "X(30)":U
      prKDData.antall FORMAT "->,>>>,>>9":U
      prKDData.Volum FORMAT "->>,>>,>>9.999":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 82 BY 6.67 EXPANDABLE.

DEFINE BROWSE br-n9HGData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS br-n9HGData C-Win _STRUCTURED
  QUERY br-n9HGData NO-LOCK DISPLAY
      prn9HGData.Hg FORMAT ">>>9":U
      prn9HGData.Konto FORMAT ">>9999":U
      prn9HGData.SumVolumAntall FORMAT "->>,>>>,>>9.999":U
      prn9HGData.SumVaresalg FORMAT "->>,>>>,>>9.99":U
      prn9HGData.MvaKr FORMAT "->,>>>,>>9.99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 69 BY 10.48 EXPANDABLE.

DEFINE BROWSE Br-pgData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br-pgData C-Win _STRUCTURED
  QUERY Br-pgData NO-LOCK DISPLAY
      getHg(prpgdata.varegr) COLUMN-LABEL "Hg"
      prPGData.VareGr FORMAT ">>>>>9":U
      prPGData.Konto FORMAT ">>9999":U
      prPGData.SumVolumAntall FORMAT "->>,>>>,>>9.999":U
      prPGData.SumVaresalg FORMAT "->>,>>>,>>9.99":U
      prPGData.MvaKr FORMAT "->,>>>,>>9.99":U WIDTH 27.6
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 79.8 BY 17.62 EXPANDABLE.

DEFINE BROWSE Br-Skift
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br-Skift C-Win _STRUCTURED
  QUERY Br-Skift NO-LOCK DISPLAY
      Skift.BokforingsId FORMAT "->>>>>>>>>>>>9":U
      Skift.SkiftNr FORMAT "->>>>>>>>9":U
      Skift.n9SkiftNr FORMAT "->>>>>>>>9":U
      Skift.SkiftId FORMAT "->>>>>>>>>>>>9":U WIDTH 25.2
      Skift.pfFlagg FORMAT ">9":U
      substr(string(skift.datetime),1,8) + "-" + string(int(substr(string(skift.datetime),9)),"HH:MM:SS") FORMAT "x(20)":U
            WIDTH 25
      Skift.datetime FORMAT ">>>>>>>>>>>>9":U WIDTH 17.2
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 98 BY 5.24 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     FI-Butik AT ROW 1.24 COL 34.6 COLON-ALIGNED
     FI-Dato AT ROW 1.24 COL 57 COLON-ALIGNED
     Br-BdSk AT ROW 2.57 COL 3
     BR-kddata AT ROW 2.57 COL 111
     B-Rapport AT ROW 2.67 COL 91
     B-ClipPrPG AT ROW 9.48 COL 151
     Br-Skift AT ROW 9.81 COL 3
     Br-pgData AT ROW 10.76 COL 112
     B-Clip AT ROW 15.52 COL 41
     Br-BmData AT ROW 16.95 COL 3
     B-Clipn9HG AT ROW 28.62 COL 152
     br-n9HGData AT ROW 30.1 COL 112
     FI-BFdag AT ROW 1.24 COL 1.4 COLON-ALIGNED NO-LABEL
     FI-KDdata AT ROW 1.48 COL 109.6 COLON-ALIGNED NO-LABEL
     FI-Skift AT ROW 8.67 COL 1.4 COLON-ALIGNED NO-LABEL
     FI-PGdata AT ROW 9.62 COL 124.4 COLON-ALIGNED NO-LABEL
     FI-BetMedel AT ROW 15.52 COL 1 COLON-ALIGNED NO-LABEL
     FI-n9HGdata AT ROW 28.86 COL 123 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 192 BY 39.76.


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
         TITLE              = "Analys preem"
         HEIGHT             = 39.76
         WIDTH              = 192
         MAX-HEIGHT         = 40.62
         MAX-WIDTH          = 195.4
         VIRTUAL-HEIGHT     = 40.62
         VIRTUAL-WIDTH      = 195.4
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
/* BROWSE-TAB Br-BdSk FI-Dato DEFAULT-FRAME */
/* BROWSE-TAB BR-kddata Br-BdSk DEFAULT-FRAME */
/* BROWSE-TAB Br-Skift B-ClipPrPG DEFAULT-FRAME */
/* BROWSE-TAB Br-pgData Br-Skift DEFAULT-FRAME */
/* BROWSE-TAB Br-BmData B-Clip DEFAULT-FRAME */
/* BROWSE-TAB br-n9HGData B-Clipn9HG DEFAULT-FRAME */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br-BdSk
/* Query rebuild information for BROWSE Br-BdSk
     _TblList          = "data.Bokforingsdag"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = " if input FI-Butik = 0 then TRUE else data.Bokforingsdag.ButikkNr = FI-Butik"
     _FldNameList[1]   = data.Bokforingsdag.ButikkNr
     _FldNameList[2]   = data.Bokforingsdag.Dato
     _FldNameList[3]   = data.Bokforingsdag.GruppeNr
     _FldNameList[4]   = data.Bokforingsdag.BokforingsId
     _FldNameList[5]   > data.Bokforingsdag.pfFlagg
"Bokforingsdag.pfFlagg" ? ? "integer" ? ? ? ? ? ? no ? no no "18" yes no no "U" "" ""
     _FldNameList[6]   > "_<CALC>"
"substr(string(bokforingsdag.datetime),1,8) + ""-"" + string(int(substr(string(bokforingsdag.datetime),9)),""HH:MM:SS"")" ? "x(20)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > "_<CALC>"
""" """ ? ? ? ? ? ? ? ? ? no ? no no "21.4" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE Br-BdSk */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br-BmData
/* Query rebuild information for BROWSE Br-BmData
     _TblList          = "data.prBMData"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "data.prBMData.ButikkNr = iButikkNr and
data.prBMData.Dato = dBokfDato"
     _FldNameList[1]   = data.prBMData.Betalingstype
     _FldNameList[2]   > "_<CALC>"
"getBMTTIdTxt (prBMData.Betalingstype,prBMData.Subtype)" "Betalingstype" "x(20)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   = data.prBMData.SubType
     _FldNameList[4]   = data.prBMData.SubSubType
     _FldNameList[5]   > data.prBMData.Belop
"prBMData.Belop" ? ? "decimal" ? ? ? ? ? ? no ? no no "27.4" yes no no "U" "" ""
     _FldNameList[6]   > "_<CALC>"
"getSubtypeName (prBMData.Betalingstype,prBMData.Subtype)" "Namn" "x(15)" ? ? ? ? ? ? ? no ? no no "10.6" yes no no "U" "" ""
     _FldNameList[7]   = data.prBMData.Konto
     _Query            is OPENED
*/  /* BROWSE Br-BmData */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-kddata
/* Query rebuild information for BROWSE BR-kddata
     _TblList          = "data.prKDData"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "data.prKDData.Dato|yes,data.prKDData.SubType|yes,data.prKDData.Vg|yes"
     _Where[1]         = "data.prKDData.ButikkNr = iButikknr and
data.prKDData.Dato = dBokfDato"
     _FldNameList[1]   = data.prKDData.Vg
     _FldNameList[2]   = data.prKDData.SubType
     _FldNameList[3]   = data.prKDData.SubTypeNavn
     _FldNameList[4]   = data.prKDData.antall
     _FldNameList[5]   = data.prKDData.Volum
     _Query            is OPENED
*/  /* BROWSE BR-kddata */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br-n9HGData
/* Query rebuild information for BROWSE br-n9HGData
     _TblList          = "data.prn9HGData"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "data.prn9HGData.Butikknr = iButikkNr and
data.prn9HGData.Dato = dBokfDato"
     _FldNameList[1]   = data.prn9HGData.Hg
     _FldNameList[2]   = data.prn9HGData.Konto
     _FldNameList[3]   = data.prn9HGData.SumVolumAntall
     _FldNameList[4]   = data.prn9HGData.SumVaresalg
     _FldNameList[5]   = data.prn9HGData.MvaKr
     _Query            is OPENED
*/  /* BROWSE br-n9HGData */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br-pgData
/* Query rebuild information for BROWSE Br-pgData
     _TblList          = "data.prPGData"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "data.prPGData.Butikknr = iButikkNr and
data.prPGData.Dato = dBokfDato"
     _FldNameList[1]   > "_<CALC>"
"getHg(prpgdata.varegr)" "Hg" ? ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   = data.prPGData.VareGr
     _FldNameList[3]   = data.prPGData.Konto
     _FldNameList[4]   = data.prPGData.SumVolumAntall
     _FldNameList[5]   = data.prPGData.SumVaresalg
     _FldNameList[6]   > data.prPGData.MvaKr
"prPGData.MvaKr" ? ? "decimal" ? ? ? ? ? ? no ? no no "27.6" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE Br-pgData */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br-Skift
/* Query rebuild information for BROWSE Br-Skift
     _TblList          = "data.Skift"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "data.Skift.SkiftNr|yes"
     _Where[1]         = "data.Skift.BokforingsId = dBokforingsid"
     _FldNameList[1]   = data.Skift.BokforingsId
     _FldNameList[2]   = data.Skift.SkiftNr
     _FldNameList[3]   = data.Skift.n9SkiftNr
     _FldNameList[4]   > data.Skift.SkiftId
"Skift.SkiftId" ? ? "decimal" ? ? ? ? ? ? no ? no no "25.2" yes no no "U" "" ""
     _FldNameList[5]   = data.Skift.pfFlagg
     _FldNameList[6]   > "_<CALC>"
"substr(string(skift.datetime),1,8) + ""-"" + string(int(substr(string(skift.datetime),9)),""HH:MM:SS"")" ? "x(20)" ? ? ? ? ? ? ? no ? no no "25" yes no no "U" "" ""
     _FldNameList[7]   > data.Skift.datetime
"Skift.datetime" ? ? "decimal" ? ? ? ? ? ? no ? no no "17.2" yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE Br-Skift */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Analys preem */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Analys preem */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Clip
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Clip C-Win
ON CHOOSE OF B-Clip IN FRAME DEFAULT-FRAME /* Dumpa till Clip */
DO:
    OUTPUT TO "CLIPBOARD".
    
    FOR EACH bprBMdata NO-LOCK WHERE bprBMData.ButikkNr = iButikkNr and
        bprBMData.Dato = dBokfDato BY Betalingstype BY SUBTYPE BY subsubtype.
    
        PUT UNFORMATTED 
            Betalingstype CHR(9)
            getBMTTIdTxt (bprBMData.Betalingstype,bprBMData.Subtype) CHR(9)
            SUBTYPE  CHR(9)
            subsubtype CHR(9)
            belop      CHR(9)
            getSubtypeName (bprBMData.Betalingstype,bprBMData.Subtype) CHR(9)
            konto SKIP.

    END.
    OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Clipn9HG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Clipn9HG C-Win
ON CHOOSE OF B-Clipn9HG IN FRAME DEFAULT-FRAME /* Dumpa till Clip */
DO:
    OUTPUT TO "CLIPBOARD".
    
    FOR EACH bprn9HGdata
      WHERE bprn9HGdata.Butikknr = iButikkNr and
              bprn9HGdata.Dato = dBokfDato NO-LOCK.
        
        PUT UNFORMATTED 
            bprn9HGdata.hg CHR(9)
            bprn9HGdata.Konto CHR(9)
            bprn9HGdata.SumVolumAntall CHR(9)
            bprn9HGdata.SumVaresalg CHR(9)
            bprn9HGdata.MvaKr
            SKIP.

    END.
    OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-ClipPrPG
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-ClipPrPG C-Win
ON CHOOSE OF B-ClipPrPG IN FRAME DEFAULT-FRAME /* Dumpa till Clip */
DO:
    OUTPUT TO "CLIPBOARD".
    
    FOR EACH bprPGData
      WHERE bprPGData.Butikknr = iButikkNr and
              bprPGData.Dato = dBokfDato NO-LOCK.
        
        PUT UNFORMATTED 
            getHg(bprpgdata.varegr) CHR(9)
            bprPGData.VareGr CHR(9)
            bprPGData.Konto CHR(9)
            bprPGData.SumVolumAntall CHR(9)
            bprPGData.SumVaresalg CHR(9)
            bprPGData.MvaKr
            SKIP.

    END.
    OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rapport
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapport C-Win
ON CHOOSE OF B-Rapport IN FRAME DEFAULT-FRAME /* Rapport */
DO:
  RUN wAnalysrapport.w.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br-BdSk
&Scoped-define SELF-NAME Br-BdSk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br-BdSk C-Win
ON VALUE-CHANGED OF Br-BdSk IN FRAME DEFAULT-FRAME
DO:
  ASSIGN iButikkNr     = bokforingsdag.butikknr
         dBokfDato     = bokforingsdag.dato
         dBokforingsid = bokforingsdag.Bokforingsid.
    {&OPEN-query-Br-Skift}
  {&OPEN-query-Br-KDData}
  {&OPEN-query-Br-BMData}
  {&OPEN-query-Br-PGData}
  {&OPEN-query-Br-n9HGData}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br-BmData
&Scoped-define SELF-NAME Br-BmData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br-BmData C-Win
ON MOUSE-SELECT-UP OF Br-BmData IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE dBelop AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    IF br-bmdata:NUM-SELECTED-ROWS < 2 THEN
        RETURN.
    DO ii = 1 TO br-bmdata:NUM-SELECTED-ROWS:
         br-bmdata:FETCH-SELECTED-ROW(ii).
         ASSIGN dBelop = dBelop + prbmdata.Belop.
    END.
    OUTPUT TO "CLIPBOARD".
    PUT UNFORMATTED dBelop SKIP.
    OUTPUT CLOSE.
    MESSAGE PROGRAM-NAME(1) SKIP
    "Summa belopp " dBelop
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-kddata
&Scoped-define SELF-NAME BR-kddata
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BR-kddata C-Win
ON MOUSE-SELECT-UP OF BR-kddata IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE iAntal AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dVolum AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    IF br-kddata:NUM-SELECTED-ROWS < 2 THEN
        RETURN.
    DO ii = 1 TO br-kddata:NUM-SELECTED-ROWS:
         br-kddata:FETCH-SELECTED-ROW(ii).
         ASSIGN iAntal = iAntal + prkddata.antall
                dVolum = dVolum + prkddata.volum.
    END.
    MESSAGE PROGRAM-NAME(1) SKIP
    "Antal kortköp" iAntal SKIP
    "Volym        " dVolum
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME br-n9HGData
&Scoped-define SELF-NAME br-n9HGData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br-n9HGData C-Win
ON MOUSE-SELECT-UP OF br-n9HGData IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE dVolAntal AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dSum AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dMvaSum AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    IF br-n9hgdata:NUM-SELECTED-ROWS < 2 THEN
        RETURN.
    DO ii = 1 TO br-n9hgdata:NUM-SELECTED-ROWS:
         br-n9hgdata:FETCH-SELECTED-ROW(ii).
         ASSIGN dVolAntal = dVolAntal + prn9hgdata.SumVolumAntall
                dsum = dSum + prn9hgdata.SumVaresalg
             dmvasum = dmvaSum + prn9hgdata.MvaKr
             .               
    END.
    OUTPUT TO "CLIPBOARD".
    PUT UNFORMATTED dVolAntal chr(9) dSum chr(9) dMvaSum SKIP.
    OUTPUT CLOSE.
    MESSAGE PROGRAM-NAME(1) SKIP
    "Vol antal" dVolAntal SKIP
    "Sum        " dSum SKIP
    "Mva        " dMvaSum
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br-pgData
&Scoped-define SELF-NAME Br-pgData
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br-pgData C-Win
ON MOUSE-SELECT-UP OF Br-pgData IN FRAME DEFAULT-FRAME
DO:
    DEFINE VARIABLE dVolAntal AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dSum AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE dMvaSum AS DECIMAL    NO-UNDO.
    DEFINE VARIABLE ii AS INTEGER    NO-UNDO.
    IF br-pgdata:NUM-SELECTED-ROWS < 2 THEN
        RETURN.
    DO ii = 1 TO br-pgdata:NUM-SELECTED-ROWS:
         br-pgdata:FETCH-SELECTED-ROW(ii).
         ASSIGN dVolAntal = dVolAntal + prpgdata.SumVolumAntall
                dsum = dSum + prpgdata.SumVaresalg
             dmvasum = dmvaSum + prpgdata.MvaKr
             .               
    END.
    OUTPUT TO "CLIPBOARD".
    PUT UNFORMATTED dVolAntal chr(9) dSum chr(9) dMvaSum SKIP.
    OUTPUT CLOSE.
    MESSAGE PROGRAM-NAME(1) SKIP
    "Vol antal" dVolAntal SKIP
    "Sum        " dSum SKIP
    "Mva        " dMvaSum
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Butik C-Win
ON RETURN OF FI-Butik IN FRAME DEFAULT-FRAME /* Butik */
DO:
    ASSIGN FI-Butik.
  {&OPEN-QUERY-Br-BdSk}
  IF BROWSE Br-BdSk:FOCUSED-ROW <> ? THEN DO:
      APPLY "ENTRY" TO BROWSE Br-BdSk.
      PROCESS EVENTS.
      APPLY "VALUE-CHANGED" TO BROWSE Br-BdSk.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-Dato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-Dato C-Win
ON RETURN OF FI-Dato IN FRAME DEFAULT-FRAME /* Datum */
DO:
  FIND sokbokforingsdag WHERE sokBokforingsdag.ButikkNr = INPUT FI-butik and
                              sokBokforingsdag.Dato     = INPUT fi-dato NO-LOCK NO-ERROR.
  IF AVAIL sokbokforingsdag THEN DO:
      REPOSITION Br-BdSk TO ROWID ROWID(sokbokforingsdag).
          APPLY "ENTRY" TO BROWSE Br-BdSk.
          PROCESS EVENTS.
          APPLY "VALUE-CHANGED" TO BROWSE Br-BdSk.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br-BdSk
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
  RUN enable_UI.
  APPLY "VALUE-CHANGED" TO BROWSE Br-BdSk.
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
  DISPLAY FI-Butik FI-Dato FI-BFdag FI-KDdata FI-Skift FI-PGdata FI-BetMedel 
          FI-n9HGdata 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE FI-Butik FI-Dato Br-BdSk BR-kddata B-Rapport B-ClipPrPG Br-Skift 
         Br-pgData B-Clip Br-BmData B-Clipn9HG br-n9HGData FI-BFdag FI-KDdata 
         FI-Skift FI-PGdata FI-BetMedel FI-n9HGdata 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send_but_dato C-Win 
PROCEDURE send_but_dato :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER opiButik AS INTEGER    NO-UNDO.
    DEFINE OUTPUT PARAMETER opdDato  AS DATE       NO-UNDO.
    IF BROWSE Br-BdSk:FOCUSED-ROW <> ? THEN
    ASSIGN opiButik = Bokforingsdag.ButikkNr
           opdDato  = Bokforingsdag.dato.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getBMTTIdTxt C-Win 
FUNCTION getBMTTIdTxt RETURNS CHARACTER
  ( INPUT iBettype AS INTEGER, INPUT iSubtype AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cBMtxt    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cRetverdi AS CHARACTER  NO-UNDO.

ASSIGN cBMtxt = "Cash,Card,Other credit,Drive off,IN/OUT Tendering,Station credit,Round off,Loyalty card,Cash Back".

  CASE iBettYpe:
      WHEN 1 THEN
          ASSIGN cRetverdi = ENTRY(iBettype,cBMtxt).
      WHEN 2 THEN
          ASSIGN cRetverdi = ENTRY(iBettype,cBMtxt).
      WHEN 3 THEN
          ASSIGN cRetverdi = ENTRY(iBettype,cBMtxt).
      WHEN 4 THEN
          ASSIGN cRetverdi = ENTRY(iBettype,cBMtxt).
      WHEN 5 THEN DO:
          CASE iSubType:
              WHEN 0 THEN
                  ASSIGN cRetverdi = "IN Tendering".
              WHEN 1 THEN
                  ASSIGN cRetverdi = "OUT Tendering".
              WHEN 2 THEN
                  ASSIGN cRetverdi = "Drop".
          END CASE.
      END.
      WHEN 6 THEN
          ASSIGN cRetverdi = ENTRY(iBettype,cBMtxt).
      WHEN 7 THEN
          ASSIGN cRetverdi = ENTRY(iBettype,cBMtxt).
      WHEN 8 THEN
          ASSIGN cRetverdi = ENTRY(iBettype,cBMtxt).
      WHEN 9 THEN
          ASSIGN cRetverdi = ENTRY(iBettype,cBMtxt).
      OTHERWISE DO:
          FIND Transtype WHERE TransType.TTId = iBettype NO-LOCK NO-ERROR.
          cRetverdi = IF AVAIL Transtype THEN TransType.Beskrivelse ELSE "Okänt".   /* Function return value. */
      END.
  END CASE.

  RETURN cRetverdi.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getHg C-Win 
FUNCTION getHg RETURNS CHARACTER
  ( INPUT iVg AS INTEGER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND vargr WHERE vargr.vg = iVg NO-LOCK NO-ERROR.

  RETURN IF AVAIL vargr THEN string(vargr.hg) ELSE "<?>".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getSubtypeName C-Win 
FUNCTION getSubtypeName RETURNS CHARACTER
  ( INPUT iBettype AS integer, INPUT iSubType AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
   DEFINE VARIABLE cSubtypeName AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cSubTypeNr   AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE cReturNamn   AS CHARACTER  NO-UNDO.
   IF iBettype = 2 AND iSubType > 0 THEN DO:
       ASSIGN cSubtypeNr   = "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25"
              cSubtypename = "PREEM,PREEM VISA,SÅIFA,TEPAR,HY/TEX NO,HY/TEX DK,SAAB/OPEL,VOLVO,NESTE,DKV,OK,UNO-X,BANKKORT,AMEX,DINERS,FINAX,UTA,BONUSKORT,CAMPING,,,,,,".
       IF CAN-DO(cSubtypeNr,STRING(iSubType)) THEN
           ASSIGN cReturNamn = ENTRY(iSubType,cSubTypename).
       IF cReturNamn = "" THEN
           ASSIGN cReturNamn = "OKÄNT".
   END.
   RETURN cReturNamn.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

