&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win

/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tStLinje NO-UNDO LIKE StLinje
/*        field LagerAnt as int */
/*        field PrimoAnt as int */
/*        field OmlHast  as dec */
/*        {tmpstlinje.i}        */
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

/* Parameters Definitions ---                                           */
def input parameter wArtBasRecid       as recid  no-undo.
def input parameter wDataObjekt        as char   no-undo.
def input parameter wStTypeId          as char   no-undo.
def input parameter wPerId             as char   no-undo.
def input parameter wCurrent-Window    as handle no-undo.
def input parameter wParentHandle      as handle no-undo.

/* Local Variable Definitions ---                                       */
def var wExcEkstent       as char   no-undo.
def var wCl               as int    no-undo.
def var wPerLinTxt        as char   no-undo.
def var wButik            as int    no-undo.
def var wVisBut           as char   format "xxxxxxx" no-undo.
def var wFraDato          as date   no-undo.
def var wTilDato          as date   no-undo.
def var wKriterier        as char   no-undo.
DEF var wAAr1             as int NO-UNDO.
DEF var wAAr2             as int NO-UNDO.
DEF var wPerLin1          as int NO-UNDO.
DEF var wPerLin2          as int NO-UNDO.
def var wwButik           as int    no-undo.
def var wWeek             as int    no-undo.
def var wOldPerId         like stLinje.PerId no-undo.

/* Variabler for statistikkbrowser. */
DEF var  wLagerVerdi    as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wDiverseAnt    as INT FORMAT "-zzz,zz9"    NO-UNDO.
DEF var  wDiverseVErdi  as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wVVarekost     as char FORMAT "x(10)"      NO-UNDO.
DEF var  wPrimoAnt      as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wAntSolgt      as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wBrekkAnt      as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wIntAnt        as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wReklAnt       as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wReklLAnt      as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wGjenkjopAnt   as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wKjopAnt       as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wOvAnt         as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wJustAnt       as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wJustVerdi     as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wSvinnAnt      as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wSvinnVerdi    as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wNedAnt        as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wNedVerdi      as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wVerdiSolgt    as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wBrekkVerdi    as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wIntVerdi      as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wReklVerdi     as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wReklLVerdi    as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wGjenkjopVerdi as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wKjopVerdi     as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wOvVerdi       as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wMvaVerdi      as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wDbKr          as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wLagerAnt      as INT FORMAT "-zz,zzz,zz9" NO-UNDO.
DEF var  wUtsolgt%      as dec FORMAT "-zzzzz9.99"  NO-UNDO.
DEF var  wDb%           as dec FORMAT "-zzzzz9.99"  NO-UNDO.
DEF var  wOmlHast       as dec FORMAT "-zzzzz9.99"  NO-UNDO.

def var wVindusTittel as char no-undo.
DEF VAR wTitle        AS CHAR NO-UNDO.
DEF VAR wRowLabel     AS CHAR NO-UNDO.
def var wRowKrit      as char no-undo.
DEF VAR wColLabel     AS CHAR NO-UNDO.
DEF VAR wFColValues   AS CHAR no-undo.
def var wColEnable    as char no-undo.      
def var wAntRowLabel  as int  no-undo.
def var wLoop         as int  no-undo.    
def var w2Loop        as int  no-undo.
def var wDato         as date no-undo.
def var wWDato        as date no-undo.
def var wPris         as char no-undo.    
def var wYYYYWW       as int  no-undo.
def var wFraUke       as int  no-undo.
def var wTilUke       as int  no-undo.
def var wAar          as int  no-undo.
def var wPerLinNr     as int  no-undo.
DEF VAR w2Tekst       AS CHAR NO-UNDO.

/* Sumvariabler. */
def var wKjopt      as int  extent 53 no-undo. 
def var wSolgt      as int  extent 53 no-undo.
def var wGjenkjop   as int  extent 53 no-undo.
def var wKReklam    as int  extent 53 no-undo.
def var wLagerRekl  as int  extent 53 no-undo.
def var wBrekkasje  as int  extent 53 no-undo.
def var wOverfort   as int  extent 53 no-undo.
def var wJustert    as int  extent 53 no-undo.
def var wSvinn      as int  extent 53 no-undo.
def var wIntforbruk as int  extent 53 no-undo.
def var wNedskrevet as int  extent 53 no-undo.
def var wRabatt     as int  extent 53 no-undo. 
def var wTekst      as char extent 12 no-undo.
DEF VAR wIniTxt     AS CHAR           NO-UNDO.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FRAME-Historikk
&Scoped-define BROWSE-NAME BROWSE-Statistikk

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tStLinje

/* Definitions for BROWSE BROWSE-Statistikk                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Statistikk tStLinje.VisBut ~
tStLinje.Aar tStLinje.PerLinTxt tStLinje.KjopAnt tStLinje.AntSolgt ~
tStLinje.Utsolgt% tStLinje.LagerAnt tStLinje.AntRabatt tStLinje.VerdiRabatt ~
tStLinje.ReklAnt tStLinje.ReklLAnt tStLinje.OvAnt wDiverseAnt ~
tStLinje.GjenkjopAnt tStLinje.OmlHast tStLinje.BrekkAnt tStLinje.JustAnt ~
tStLinje.SvinnAnt tStLinje.IntAnt tStLinje.NedAnt 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Statistikk 
&Scoped-define QUERY-STRING-BROWSE-Statistikk FOR EACH tStLinje ~
      WHERE tStLinje.DataObjekt = wDataObjekt and ~
tStLinje.StTypeId   = wStTypeId and ~
tStLinje.PerId      = wPerId and ~
tStLinje.PerLinNr   > wButik and ~
tStLinje.PerLinNr   <= 1999999 ~
 NO-LOCK ~
    BY tStLinje.DataObjekt ~
       BY tStLinje.StTypeId ~
        BY tStLinje.PerId ~
         BY tStLinje.Butik ~
          BY tStLinje.Aar ~
           BY tStLinje.PerLinNr INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-Statistikk OPEN QUERY BROWSE-Statistikk FOR EACH tStLinje ~
      WHERE tStLinje.DataObjekt = wDataObjekt and ~
tStLinje.StTypeId   = wStTypeId and ~
tStLinje.PerId      = wPerId and ~
tStLinje.PerLinNr   > wButik and ~
tStLinje.PerLinNr   <= 1999999 ~
 NO-LOCK ~
    BY tStLinje.DataObjekt ~
       BY tStLinje.StTypeId ~
        BY tStLinje.PerId ~
         BY tStLinje.Butik ~
          BY tStLinje.Aar ~
           BY tStLinje.PerLinNr INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-Statistikk tStLinje
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Statistikk tStLinje


/* Definitions for FRAME FRAME-Historikk                                */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FRAME-Historikk ~
    ~{&OPEN-QUERY-BROWSE-Statistikk}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B-Grafikk COMBO-BOX-Periode T-Tot ~
BROWSE-Statistikk BUTTON-SokDato FILL-IN-Tekst1 
&Scoped-Define DISPLAYED-OBJECTS COMBO-BOX-Periode FI-Filter T-Tot ~
FILL-IN-Tekst1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Excel 
     IMAGE-UP FILE "icon/excel":U NO-FOCUS
     LABEL "Excel..." 
     SIZE 4.4 BY 1 TOOLTIP "Eksporter alle eller merkede tellelinjer til Excel. Alt-X.".

DEFINE BUTTON B-Grafikk 
     LABEL "&Grafikk" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Rapp 
     IMAGE-UP FILE "icon/htmldok":U NO-FOCUS
     LABEL "Htm" 
     SIZE 4.4 BY 1 TOOLTIP "Html".

DEFINE BUTTON B-Rapp-HtmEx 
     IMAGE-UP FILE "icon/htmldok":U NO-FOCUS
     LABEL "HtmlExcel" 
     SIZE 4.4 BY 1 TOOLTIP "Html i Excel".

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE COMBO-BOX-Periode AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Filter AS CHARACTER FORMAT "X(256)" 
     LABEL "Periodefilter" 
     VIEW-AS FILL-IN 
     SIZE 41 BY 1.

DEFINE VARIABLE FILL-IN-Tekst1 AS CHARACTER FORMAT "X(256)":U INITIAL "Statistikktype" 
      VIEW-AS TEXT 
     SIZE 18 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE T-Tot AS LOGICAL INITIAL yes 
     LABEL "Kun totaler" 
     VIEW-AS TOGGLE-BOX
     SIZE 16 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Statistikk FOR 
      tStLinje SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Statistikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Statistikk C-Win _STRUCTURED
  QUERY BROWSE-Statistikk NO-LOCK DISPLAY
      tStLinje.VisBut COLUMN-LABEL "But" FORMAT "xxxxxxx":U
      tStLinje.Aar FORMAT "zzz9":U
      tStLinje.PerLinTxt COLUMN-LABEL "Periode" FORMAT "xxxxxxxxxxxxxxxxxxxx":U
      tStLinje.KjopAnt COLUMN-LABEL "Kjøpt" FORMAT "-zzz,zz9":U
            WIDTH 10.2
      tStLinje.AntSolgt COLUMN-LABEL "Solgt" FORMAT "-zzz,zz9":U
            WIDTH 10
      tStLinje.Utsolgt% COLUMN-LABEL "Utsolgt%" FORMAT "-zzz9.99":U
      tStLinje.LagerAnt COLUMN-LABEL "Lager" FORMAT "-zzz,zz9":U
            WIDTH 9
      tStLinje.AntRabatt COLUMN-LABEL "Rabatt" FORMAT "-zzz,zz9":U
            WIDTH 7.6
      tStLinje.VerdiRabatt COLUMN-LABEL "V-Rabatt" FORMAT "-zz,zzz,zz9.99":U
      tStLinje.ReklAnt COLUMN-LABEL "Reklamert" FORMAT "-zzz,zz9":U
            WIDTH 10.8
      tStLinje.ReklLAnt COLUMN-LABEL "LReklamert" FORMAT "-zzz,zz9":U
      tStLinje.OvAnt COLUMN-LABEL "Overført" FORMAT "-zzz,zz9":U
            WIDTH 10.2
      wDiverseAnt COLUMN-LABEL "Diverse" FORMAT "-zzz,zz9":U
      tStLinje.GjenkjopAnt COLUMN-LABEL "Gjenkjøp" FORMAT "-zzz,zz9":U
      tStLinje.OmlHast COLUMN-LABEL "OmlHast" FORMAT "-zzz9.99":U
      tStLinje.BrekkAnt FORMAT "-zzz,zz9":U
      tStLinje.JustAnt COLUMN-LABEL "Justert" FORMAT "-zzz,zz9":U
      tStLinje.SvinnAnt COLUMN-LABEL "Svinn" FORMAT "-zz,zz9":U
      tStLinje.IntAnt FORMAT "-zzz,zz9":U
      tStLinje.NedAnt COLUMN-LABEL "Nedskrevet" FORMAT "-zz,zz9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 154 BY 15.67 ROW-HEIGHT-CHARS .62 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FRAME-Historikk
     B-Excel AT ROW 1.24 COL 106.2
     B-Rapp AT ROW 1.24 COL 111.2
     B-Rapp-HtmEx AT ROW 1.24 COL 116
     B-Grafikk AT ROW 1.14 COL 140
     COMBO-BOX-Periode AT ROW 1.24 COL 19 COLON-ALIGNED NO-LABEL
     FI-Filter AT ROW 1.24 COL 57 COLON-ALIGNED
     T-Tot AT ROW 1.24 COL 123
     BROWSE-Statistikk AT ROW 2.48 COL 1
     BUTTON-SokDato AT ROW 1.24 COL 100
     FILL-IN-Tekst1 AT ROW 1.48 COL 2 NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.91
         SIZE 154.8 BY 17.38.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tStLinje T "NEW SHARED" NO-UNDO skotex StLinje
      ADDITIONAL-FIELDS:
          field LagerAnt as int
          field PrimoAnt as int
          field OmlHast  as dec
          {tmpstlinje.i}
          
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* SUPPRESS Window definition (used by the UIB) 
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Statistikkvindu"
         HEIGHT             = 27.48
         WIDTH              = 156.6
         MAX-HEIGHT         = 27.48
         MAX-WIDTH          = 159.8
         VIRTUAL-HEIGHT     = 27.48
         VIRTUAL-WIDTH      = 159.8
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
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME FRAME-Historikk
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BROWSE-Statistikk T-Tot FRAME-Historikk */
/* SETTINGS FOR BUTTON B-Excel IN FRAME FRAME-Historikk
   NO-ENABLE                                                            */
ASSIGN 
       B-Excel:HIDDEN IN FRAME FRAME-Historikk           = TRUE.

/* SETTINGS FOR BUTTON B-Rapp IN FRAME FRAME-Historikk
   NO-ENABLE                                                            */
ASSIGN 
       B-Rapp:HIDDEN IN FRAME FRAME-Historikk           = TRUE.

/* SETTINGS FOR BUTTON B-Rapp-HtmEx IN FRAME FRAME-Historikk
   NO-ENABLE                                                            */
ASSIGN 
       B-Rapp-HtmEx:HIDDEN IN FRAME FRAME-Historikk           = TRUE.

ASSIGN 
       BROWSE-Statistikk:NUM-LOCKED-COLUMNS IN FRAME FRAME-Historikk     = 3.

/* SETTINGS FOR FILL-IN FI-Filter IN FRAME FRAME-Historikk
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FILL-IN-Tekst1 IN FRAME FRAME-Historikk
   ALIGN-L                                                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Statistikk
/* Query rebuild information for BROWSE BROWSE-Statistikk
     _TblList          = "Temp-Tables.tStLinje"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "Temp-Tables.tStLinje.DataObjekt|yes,Temp-Tables.tStLinje.StTypeId|yes,Temp-Tables.tStLinje.PerId|yes,Temp-Tables.tStLinje.Butik|yes,Temp-Tables.tStLinje.Aar|yes,Temp-Tables.tStLinje.PerLinNr|yes"
     _Where[1]         = "tStLinje.DataObjekt = wDataObjekt and
tStLinje.StTypeId   = wStTypeId and
tStLinje.PerId      = wPerId and
tStLinje.PerLinNr   > wButik and
tStLinje.PerLinNr   <= 1999999
"
     _FldNameList[1]   > "_<CALC>"
"tStLinje.VisBut" "But" "xxxxxxx" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > Temp-Tables.tStLinje.Aar
"Aar" ? "zzz9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > "_<CALC>"
"tStLinje.PerLinTxt" "Periode" "xxxxxxxxxxxxxxxxxxxx" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > Temp-Tables.tStLinje.KjopAnt
"KjopAnt" "Kjøpt" "-zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no "10.2" yes no no "U" "" ""
     _FldNameList[5]   > Temp-Tables.tStLinje.AntSolgt
"AntSolgt" "Solgt" "-zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" ""
     _FldNameList[6]   > "_<CALC>"
"tStLinje.Utsolgt%" "Utsolgt%" "-zzz9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > "_<CALC>"
"tStLinje.LagerAnt" "Lager" "-zzz,zz9" ? ? ? ? ? ? ? no ? no no "9" yes no no "U" "" ""
     _FldNameList[8]   > Temp-Tables.tStLinje.AntRabatt
"AntRabatt" "Rabatt" "-zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no "7.6" yes no no "U" "" ""
     _FldNameList[9]   > Temp-Tables.tStLinje.VerdiRabatt
"VerdiRabatt" "V-Rabatt" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[10]   > Temp-Tables.tStLinje.ReklAnt
"ReklAnt" "Reklamert" "-zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no "10.8" yes no no "U" "" ""
     _FldNameList[11]   > Temp-Tables.tStLinje.ReklLAnt
"ReklLAnt" "LReklamert" "-zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[12]   > Temp-Tables.tStLinje.OvAnt
"OvAnt" "Overført" "-zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no "10.2" yes no no "U" "" ""
     _FldNameList[13]   > "_<CALC>"
"wDiverseAnt" "Diverse" "-zzz,zz9" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[14]   > Temp-Tables.tStLinje.GjenkjopAnt
"GjenkjopAnt" "Gjenkjøp" "-zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[15]   > "_<CALC>"
"tStLinje.OmlHast" "OmlHast" "-zzz9.99" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[16]   > Temp-Tables.tStLinje.BrekkAnt
"BrekkAnt" ? "-zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[17]   > Temp-Tables.tStLinje.JustAnt
"JustAnt" "Justert" "-zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[18]   > Temp-Tables.tStLinje.SvinnAnt
"SvinnAnt" "Svinn" "-zz,zz9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[19]   > Temp-Tables.tStLinje.IntAnt
"IntAnt" ? "-zzz,zz9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[20]   > Temp-Tables.tStLinje.NedAnt
"NedAnt" "Nedskrevet" "-zz,zz9" "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-Statistikk */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME FRAME-Historikk
/* Query rebuild information for FRAME FRAME-Historikk
     _Query            is NOT OPENED
*/  /* FRAME FRAME-Historikk */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Statistikkvindu */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Statistikkvindu */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Excel C-Win
ON CHOOSE OF B-Excel IN FRAME FRAME-Historikk /* Excel... */
DO:
    RETURN NO-APPLY.
    RUN ExHtmRapp ("EX").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Grafikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Grafikk C-Win
ON CHOOSE OF B-Grafikk IN FRAME FRAME-Historikk /* Grafikk */
DO:
  run StatGrafikk.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapp C-Win
ON CHOOSE OF B-Rapp IN FRAME FRAME-Historikk /* Htm */
DO:
    RETURN NO-APPLY.
    RUN ExHtmRapp ("HTM").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Rapp-HtmEx
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Rapp-HtmEx C-Win
ON CHOOSE OF B-Rapp-HtmEx IN FRAME FRAME-Historikk /* HtmlExcel */
DO:
    RETURN NO-APPLY.
    RUN ExHtmRapp ("HTMEX").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Statistikk
&Scoped-define SELF-NAME BROWSE-Statistikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Statistikk C-Win
ON ROW-DISPLAY OF BROWSE-Statistikk IN FRAME FRAME-Historikk
DO:
  if available tStLinje then
    run RowDisplay.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Win
ON CHOOSE OF BUTTON-SokDato IN FRAME FRAME-Historikk /* ... */
DO:   
  /* I variabelen wTekst vil ved retur fra filterprogram stå avgrensning */
  /* i en kommaseparert liste. Info ligger på formatet                   */
  /*   Fra År, Til år Fra periodelinje, Til periodelinje.                */
  case wPerId:
    when "AAR"   then run d-statfiltaar.w   (input-output wKriterier).
    when "MANED" then run d-statfiltmaned.w (input-output wKriterier).
    when "UKE"   then run d-statfiltuke.w   (input-output wKriterier). 
    when "DAG"   then run d-statfiltdag.w   (input-output wKriterier).
    otherwise         run d-statfiltdag.w   (input-output wKriterier).
  end case. 
  if return-value = "AVBRYT" then
    return no-apply.

  ASSIGN
      wKriterier = wKriterier + ",0,1,1".

  RUN SaveToLokalIni ("PERID", wPerId).
  RUN SaveToLokalIni ("KRITERIER", wKriterier).

  run ByggStLinje.
  
  {&OPEN-QUERY-BROWSE-Statistikk}
  apply "VALUE-CHANGED":U to BROWSE-Statistikk.
  apply "ENTRY":U to BROWSE-Statistikk.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME COMBO-BOX-Periode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL COMBO-BOX-Periode C-Win
ON VALUE-CHANGED OF COMBO-BOX-Periode IN FRAME FRAME-Historikk
DO:
  assign 
    COMBO-BOX-Periode
    wOldPerId = wPerId
    wPerId    = COMBO-BOX-Periode.
    
  /* I variabelen wKriterier vil ved retur fra filterprogram stå avgrensning */
  /* i en kommaseparert liste. Info ligger på formatet                       */
  /*   Fra År, Til år Fra periodelinje, Til periodelinje.                    */
  case wPerId:
    when "AAR"   then 
      do:
        ENTRY(3, wKriterier) = "1".
        ENTRY(4, wKriterier) = "1".
      end.
    when "MANED" then 
      do:
        if wOldPerId = "AAR" then
          do:
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = "1".
          end.
        else if wOldPerId = "MANED" then
          do:
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = "12".
          end.
        else if wOldPerId = "UKE" then
          do:
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = "12".
          end.
        else
          do:
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = "12".
          end.
      end.
    when "UKE"   then 
      do:
        if wOldPerId = "AAR" then
          do:
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = "53".
          end.
        else if wOldPerId = "MANED" then
          do:
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = "53".
          end.
        else if wOldPerId = "UKE" then
          do:
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = "53".
          end.
        else
          do:
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = "53".
          end.
      end. 
    otherwise 
      do:
        if wOldPerId = "AAR" then
          do:
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = "366".
          end.
        else if wOldPerId = "MANED" then
          do:
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = "366".
          end.
        else if wOldPerId = "UKE" then
          do:
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = "366".
          end.
        else
          do:
            ENTRY(3, wKriterier) = "1".
            ENTRY(4, wKriterier) = "366".
          end.
      end. 
  end case.
  
  RUN SaveToLokalIni ("PERID", wPerId).
  RUN SaveToLokalIni ("KRITERIER", wKriterier).
   
  RUN ByggStLinje.

  {&OPEN-QUERY-BROWSE-Statistikk}
  apply "VALUE-CHANGED":U to BROWSE-Statistikk.
  apply "ENTRY":U to BROWSE-Statistikk.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Tot
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Tot C-Win
ON VALUE-CHANGED OF T-Tot IN FRAME FRAME-Historikk /* Kun totaler */
DO:
  ASSIGN
      T-Tot.
  if T-Tot then
    wButik = 999999.
  else
    wButik = 0.

  RUN SaveToLokalIni ("VISTOT", string(T-Tot)).

  {&OPEN-BROWSERS-IN-QUERY-FRAME-Historikk}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
/*
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.             
*/
ASSIGN CURRENT-WINDOW                = wCurrent-Window 
       THIS-PROCEDURE:CURRENT-WINDOW = wCurrent-Window.                    

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
  do:
    /*RUN SaveBrowseSettings.*/
    if valid-handle(wParentHandle) then
      run SlettProg in wParentHandle.
    RUN disable_UI.
  end.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

assign
  wPerLinTxt  = "99/99/99-99/99/99"
  wButik      = 999999 /* Kun vise totaler som default. */
  FI-Filter   = ""
  .
  /*
  wStTypeId          = "ARTIKKEL".
  */
/* Leser kriterier og periodetype fra INI filen */
RUN ReadFromLokalIni ("KRITERIER", OUTPUT wKriterier).
IF wKriterier = "" THEN
DO:
  ASSIGN
    wPerId     = "MANED"
    wKriterier = string(year(today - 182)) + "," +
                 string(year(today)) + "," +
                 "1," + string(month(today)) + ",0,0,1,1" /* All statistikk */.
  RUN SaveToLokalIni ("VISTOT", "YES").
  RUN SaveToLokalIni ("PERID", wPerId).
  RUN SaveToLokalIni ("KRITERIER", wKriterier).  
END.
ELSE DO:
  RUN ReadFromLokalIni ("PERID", OUTPUT wPerId).
  RUN ReadFromLokalIni ("VISTOT", OUTPUT wIniTxt).
  IF CAN-DO("JA,TRUE,YES",wIniTxt) THEN
        T-Tot = TRUE.
    ELSE 
        T-Tot = FALSE.
  if T-Tot then
    wButik = 999999.
  else
    wButik = 0.
END.
/* Setter sentrallager butikk */
{syspara.i 5 1 1 wCl INT}

/* Initierer combo-box for valg av statistikkperiode */
do:
  assign
    w2Tekst = ""
    .
  for each StDef no-lock where
    StDef.StTypeId = wStTypeId:
    w2Tekst = w2Tekst + 
            (if w2Tekst = ""
               then ""
               else ",") + 
            StDef.Beskrivelse + "," + StDef.PerId.                       
  end.  
  ASSIGN
    COMBO-BOX-Periode:LIST-ITEM-PAIRS = w2Tekst
    COMBO-BOX-Periode = entry(2,w2Tekst)
    .  
end.

/* Bygger temp-table */
run Byggstlinje.
find first tStLinje NO-LOCK where
  tStLinje.DataObjekt = wDataObjekt and
  tStLinje.StTypeId   = wStTypeId and
  tStLinje.PerId      = wPerId no-error.

{syspara.i 1 4 1 wExcEkstent}
wExcEkstent = if wExcEkstent = "" then "sdv" else wExcEkstent.   

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    
  &SCOP PROCEDURE-TYPE FRAME 
  RUN enable_UI.
  {lng.i}
  ASSIGN
      COMBO-BOX-Periode:SCREEN-VALUE IN FRAME FRAME-Historikk = COMBO-BOX-Periode
      .

  APPLY "ENTRY":U TO BROWSE-Statistikk IN FRAME FRAME-Historikk.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggStLinje C-Win 
PROCEDURE ByggStLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
  /* Bygger temp-table */
  {sww.i}
  
  do:
    if can-do("AAR,MANED,UKE",wPerId) then
      assign
        FI-Filter  = entry(1,wKriterier) + "/" + 
                     entry(3,wKriterier) + " - " + 
                     entry(2,wKriterier) + "/" + 
                     entry(4,wKriterier).
    else
      assign
        FI-Filter  = string(date(1,1,int(entry(1,wKriterier))) + int(entry(3,wKriterier)) - 1) + " - " +
                     string(date(1,1,int(entry(2,wKriterier))) + int(entry(4,wKriterier)) - 1).

    /* Er det butikkstatistikk, skal bare statistikk for aktuell butikk vises. */
    IF CAN-DO("BUTSTAT",wStTypeId) THEN
    DO:
        ENTRY(5,wKriterier) = STRING(INT(wDataObjekt)).
    END.

    if entry(5,wKriterier) <> "0" then
      FI-Filter = FI-Filter + " Butikk " + entry(5,wKriterier).
    display
      FI-Filter 
    with frame FRAME-Historikk.
  end.
  
  run byggstlinje.p (wDataObjekt,wPerId,wStTypeId,yes,wKriterier).
  {swn.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttObjekt C-Win 
PROCEDURE ByttObjekt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ipDataObjekt as char  no-undo.

  assign
    wDataObjekt = ipDataObjekt.
    
  case wStTypeId:
    when "ARTIKKEL" then
      /* ARTIKKEL */
      do:
        /* Henter artikkelinformasjonen. */
        find ArtBas no-lock where 
          ArtBas.ArtikkelNr = dec(wDataObjekt) no-error.
        if not available ArtBAs then
          return no-apply.
        assign
          wArtBasRecid = recid(ArtBas).
      end. /* ARTIKKEL */
    when "BUTSTAT" then
      do:
        /* Henter Butikkinformasjonen. */
        find Butiker no-lock where 
          Butiker.Butik = int(wDataObjekt) no-error.
        if not available Butiker then
          return no-apply.
        assign
           wArtBasRecid = recid(Butiker).
      end. /* BUTSTAT */
    when "HOVEDGR" then
      do:
        /* Henter Hovedgruppeinformasjonen. */
        find HuvGr no-lock where 
          HuvGr.Hg = int(wDataObjekt) no-error.
        if not available HuvGr then
          return no-apply.
        assign
          wArtBasRecid = recid(HuvGr).
      end. /* HUVEDGR */
    when "KUNDSTAT" then
      do:
        /* Henter kundeinformasjonen. */
        find Kunde no-lock where 
          Kunde.KundeNr = DEC(wDataObjekt) no-error.
        if not available Kunde then
          return no-apply.
        assign
          wArtBasRecid = recid(Kunde).
      end. /* KUNDSTAT */
    when "LEVERAN" then
      do:      
        /* Henter leverandørinformasjonen. */
        find LevBas no-lock where 
          LevBas.LevNr = int(wDataObjekt) no-error.
        if not available LevBas then
          return no-apply.
        assign
          wArtBasRecid = recid(LevBas).
      end. /* LEVERAN */
    when "SELGERSTAT" then
      do:
        /* Henter Selgerinformasjonen. */
        find Forsalj no-lock where 
          Forsalj.ForsNr = int(wDataObjekt) no-error.
        if not available Forsalj then
          return no-apply.
        assign
          wArtBasRecid = recid(Forsalj).
      end. /* SELGERSTAT */
    when "VAREGR" then
      do:
        /* Henter Varegruppeinformasjonen. */
        find VarGr no-lock where 
          VarGr.Vg = int(wDataObjekt) no-error.
        if not available VarGr then
          return no-apply.
        assign
          wArtBasRecid = recid(VarGr).
      end. /* VAREGR */
    when "MEDLEM" then
      do:
        /* Henter Medlemsinformasjon. */
        find Medlem no-lock where 
          Medlem.MedlemsNr = dec(wDataObjekt) no-error.
        if not available Medlem then
          return no-apply.
        assign
          wArtBasRecid = recid(Medlem).
      end. /* MEDLEM */
    when "MEDLEMTOT" then
      do:
        assign
          wArtBasRecid = ?.
      end. /* MEDLEM */
  end case. /* STATISTIKKTYPE */    
    
  {sww.i}  
  /* Er det butikkstatistikk, skal bare statistikk for aktuell butikk vises. */
  IF CAN-DO("BUTSTAT",wStTypeId) THEN
  DO:
    ENTRY(5,wKriterier) = STRING(INT(wDataObjekt)).
  END.

  /* Bygger temp-table */
  run byggstlinje.p (wDataObjekt,wPerId,wStTypeId,yes,wKriterier).
  find first tStLinje NO-LOCK where
    tStLinje.DataObjekt = wDataObjekt and
    tStLinje.StTypeId   = wStTypeId and
    tStLinje.PerId      = wPerId no-error.

  {&OPEN-BROWSERS-IN-QUERY-FRAME-Historikk}
  {swn.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  HIDE FRAME FRAME-Historikk.
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
  DISPLAY COMBO-BOX-Periode FI-Filter T-Tot FILL-IN-Tekst1 
      WITH FRAME FRAME-Historikk.
  ENABLE B-Grafikk COMBO-BOX-Periode T-Tot BROWSE-Statistikk BUTTON-SokDato 
         FILL-IN-Tekst1 
      WITH FRAME FRAME-Historikk.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-Historikk}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExHtmRapp C-Win 
PROCEDURE ExHtmRapp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER wDokTyp AS CHAR NO-UNDO.
  DEFINE VAR wtmpFileName AS CHAR NO-UNDO.
  DEFINE VAR wHead1Set    AS CHAR NO-UNDO.
  DEFINE VAR wColHead     AS CHAR NO-UNDO.
  DEFINE VAR wFields      AS CHAR NO-UNDO.
  DEFINE VAR wColHeadForm AS CHAR NO-UNDO.
  DEFINE VAR wTabell      AS CHAR NO-UNDO.
  DEFINE VAR wSep         AS CHAR INIT ";" NO-UNDO. /* byt inte wSep till "," */
  DEFINE VAR wQY          AS CHAR NO-UNDO.
  DEFINE VAR hQuery       AS HANDLE NO-UNDO.
  DEFINE VAR hLevBas      AS HANDLE NO-UNDO.
  DEFINE VAR wLblHdl      AS HANDLE NO-UNDO.
  DEFINE VAR wTotWidthC   AS DECI NO-UNDO.
  DEFINE VAR wColWidthC   AS CHAR NO-UNDO.
  DEFINE VAR wFormat      AS CHAR NO-UNDO.
  DEFINE VAR wCount       AS INTE NO-UNDO.
  {sww.i}
 DO WITH FRAME {&FRAME-NAME}:
  if valid-handle(wLibHandle) then
    run GetTempFileName in wLibHandle (wDokTyp, IF wDokTyp BEGINS "HTM" THEN "HTM" ELSE wExcEkstent, output wtmpFileName). 
    ASSIGN hQuery = {&BROWSE-NAME}:QUERY.
  DO:
    ASSIGN wLblHdl = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
    REPEAT WHILE VALID-HANDLE(wLblHdl).
      IF wLblHdl:label = "" THEN LEAVE.
      ASSIGN wColHead = wColHead + (if wColHead <> "" then wSep else "") + 
                    TRIM(REPLACE(wLblHdl:label,"*",""))
             wFields  = wFields + (if wFields <> "" then wSep else "") + 
                    TRIM(wLblHdl:NAME)
             wFormat  = wFormat + (if wFormat <> "" then wSep else "") + 
                 IF (wDokTyp = "HTM" AND wLblHdl:FORMAT BEGINS "-") THEN SUBSTR(wLblHdl:FORMAT,2) + "-" ELSE wLblHdl:FORMAT
             wTotWidthC = wTotWidthC + LENGTH(wLblHdl:FORMAT) /* wLblHdl:WIDTH-PIXELS */
             wColHeadForm = wColHeadForm + (if wColHeadForm = "" then "" else wSep) +
                          (IF LENGTH(wLblHdl:FORMAT) = 1 THEN "C" ELSE (IF
                          wLblHdl:DATA-TYPE BEGINS "INTE" OR wLblHdl:DATA-TYPE BEGINS "DECI" THEN "R" ELSE "L"))
             wColWidthC = wColWidthC + (if wColWidthC = "" then "" else wSep) + STRING(LENGTH(wLblHdl:FORMAT)) /* string(INT(wLblHdl:WIDTH-PIXELS)) */
             wLblHdl = wLblHdl:NEXT-COLUMN.
    end.
    ASSIGN wFormat = IF wDokTyp = "HTM" THEN wFormat ELSE REPLACE(wFormat,",","").
    DO wCount = 1 TO NUM-ENTRIES(wColHeadForm,wSep):
      ENTRY(wCount,wColHeadForm,wSep) = ENTRY(wCount,wColHeadForm,wSep) + "," + String(INT(DECI(ENTRY(wCount,wColWidthC,wSep)) / wTotWidthC * 100)) + "%".
    END.
  END.
  ASSIGN wTabell = "Statistik"
         wHead1Set    = "100%,,1,0,2," + STRING(NUM-ENTRIES(wFields,wSep))
         wQY          = hQuery:PREPARE-STRING.
  RUN w-bhtmlstlinje.p(wDokTyp,wSep,wHead1Set,wColHead,wFields,wFormat,wColHeadForm,wTabell,wQY,T-Tot:CHECKED,hQuery,wtmpFileName).
  if valid-handle(wLibHandle) then DO:
    IF wDokTyp = "HTM" THEN
        RUN OpenWeb in wLibHandle (wtmpFileName).
    ELSE run OpenExcelDocument in wLibHandle (wtmpFileName," ").
  END.
  {swn.i}
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTopp C-Win 
PROCEDURE MoveToTopp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
   IF FRAME FRAME-Historikk:MOVE-TO-TOP() THEN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReadFromLokalIni C-Win 
PROCEDURE ReadFromLokalIni :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT  PARAMETER wKey     AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER wValue   AS CHAR NO-UNDO.

DEF VAR wSection AS CHAR NO-UNDO.
DEF VAR wMappe   AS CHAR NO-UNDO.
DEF VAR wIniFil  AS CHAR NO-UNDO.

ASSIGN
    wSection = "FILTERPARAMETRE".

/* Henter mappe og filnavn på den lokale inifilen */
IF VALID-HANDLE(wLibHandle) THEN
DO:
  RUN Mappe  IN wLibHandle (OUTPUT wMappe).
  RUN IniFil IN wLibHandle (OUTPUT wIniFil).
END.
ELSE 
    RETURN "AVBRYT".

/* Lagrer parametre */
IF wMappe <> ? THEN 
  DO:
    /* Laster den lokale ini filen. */
    LOAD wIniFil DIR wMappe BASE-KEY "INI" NO-ERROR.
    /* Finnes den ikke, opprette den. */
    IF ERROR-STATUS:ERROR THEN DO:
       LOAD wIniFil DIR wMappe NEW BASE-KEY "INI" NO-ERROR.
       IF ERROR-STATUS:ERROR THEN RETURN "AVBRYT".
    END.           
    /* Stiller om til lokal ini-fil. */
    USE wIniFil NO-ERROR.
    /* Lagrer parameterverdien */
    IF NOT ERROR-STATUS:ERROR THEN
       GET-KEY-VALUE SECTION wSection KEY wKey VALUE wValue.
    /* Stiller tilbake til oppstarts ini fil. */
    UNLOAD wIniFil NO-ERROR.
  END.

/* Stripper ukjent verdi. */
ASSIGN
    wValue = IF wValue = ? THEN "" ELSE wValue.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RePosStatistikk C-Win 
PROCEDURE RePosStatistikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wDato as date no-undo.

  def var wWork as int no-undo.

  find Periode no-lock where
    Periode.PerId = wPerId no-error.
  if not available Periode then
    return no-apply.  

  /* Henter PeriodeLinje */
  /* Faste perioder.     */
  if Periode.Fast then
    do:
      case Periode.PerId:
        when "AAR"   then /* Kun en linje i et år. */
          wWork = 1. 
        when "MANED" then /* 12 Linjer pr. år */
          wWork = month(wDato).
        when "UKE"   then /* Opptil 53 linjer pr. år */
          do:
            run weeknum.p (wDato, output wWork).
            wWork = int(substring(string(wWork,"999999"),5,2)).
          end.
        when "DAG"   then /* Opptil 366 dager pr. år. */
          wWork = (wDato + 1) - date(01,01,year(wDato)).
      end.
    end.
  /* Fri periode         */
  else do:
    find first PerLin no-lock where
      PerLin.PerId   =  Periode.PerId  and
      PerLin.FraDato <= wDato and 
      PerLin.TilDato >= wDato no-error.
    if not available PerLin then
      find last PerLin where
        PerLin.PerId = Periode.PerId no-error.
    if available PerLin then
      wWork = PerLin.PerLinNr.
    else /* Her er ingenting mer å gjøre. */
      return "NEXT".
  end.  
 
  find first tStLinje no-lock where
    tStLinje.DataObjekt = wDataObjekt and
    tStLinje.StTypeId   = wStTypeId and
    tStLinje.PerId      = wPerId and
    tStLinje.Aar        = year(wDato) and
    tStLinje.PerLinNr   = wWork no-error.
  if not available tStLinje then
    find first tStLinje where
      tStLinje.DataObjekt = wDataObjekt and
      tStLinje.StTypeId   = wStTypeId and
      tStLinje.PerId      = wPerId no-error.
      
  if available tStLinje then    
    REPOSITION BROWSE-Statistikk TO ROWID rowid(tStLinje) NO-ERROR.
  APPLY "ENTRY" TO BROWSE BROWSE-Statistikk.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RowDisplay C-Win 
PROCEDURE RowDisplay :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      wDiverseAnt   = tStlinje.DiverseAnt
      wDiverseVerdi = 0  /* tStlinje.DiverseVerdi */
      wLagerVerdi   = 0. /* tStLinje.LagerVerdi. */
  
  /*-----------------
  assign
    wVerdiSolgt = tStLinje.VerdiSolgt
    wDbKr       = tStLinje.DbKr
    wDb%        = tStLinje.Db%
    wLagerAnt   = tStLinje.LagerAnt
    wUtsolgt%   = tStLinje.Utsolgt%
    wOmlHast    = tStLinje.OmlHast
    wPrimoAnt   = tStLinje.PrimoAnt
    wVisBut     = tStLinje.VisBut
    wPerLinTxt  = tStLinje.PerLinTxt.

  assign
    wVerdiSolgt = tStLinje.VerdiSolgt /*- (tStLinje.ReklVerdi + tStLinje.GjenkjopVerdi)*/
    wDbKr       = wVerdiSolgt - tStLinje.VVareKost
    wDb%        = (wDbKr * 100) / wVerdiSolgt
    wDb%        = if wDb% = ? then 0 else wDb%
    wLagerAnt   = tStLinje.LagerAnt
    wUtsolgt%   = (tStLinje.AntSolgt / (tStLinje.KjopAnt + tStLinje.OvAnt)) * 100
    wUtsolgt%   = if wUtsolgt% = ? then 0 else wUtsolgt%
    wOmlHast    = tStLinje.OmlHast
    wPrimoAnt   = tStLinje.PrimoAnt
    wVisBut     = if tStLinje.Butik <= 999999 
                    then string(tStLinje.Butik)
                    else ""
    wVisBut     = fill(" ",7 - length(wVisBut)) + wVisBut.
    
  /* Hånterer sumlinjer */
  if tStLinje.PerLinNr = 1999999 then
    wPerLinTxt = "G-Tot".
  else if tStLinje.PerLinNr = 1000000 then
    wPerLinTxt = "ButikkTot".
  else  
  case tStLinje.PerId:
    when "AAR"   then wPerLinTxt = string(tStLinje.PerLinNr).
    when "MANED" then wPerLinTxt = string(tStLinje.PerLinNr).
    when "UKE"   then wPerLinTxt = string(tStLinje.PerLinNr).
    when "DAG"   then 
      do:
        find PerLin no-lock where
          PerLin.PerId    = tStLinje.PerId and
          PerLin.PerLinNr = tStLinje.PerLinNr no-error.
        if available PerLin then
          wPerLinTxt = string(date(1,1,tStLinje.Aar) + (tStLinje.PerLinNr - 1)).
        else wPerLinTxt = string(tStLinje.PerLinNr).        
      end.
    otherwise    
      do:
        find PerLin no-lock where
          PerLin.PerId    = tStLinje.PerId and
          PerLin.PerLinNr = tStLinje.PerLinNr no-error.
        if available PerLin then
          wPerLinTxt = string(PerLin.FraDato) + "-" + string(PerLin.TilDato).
        else wPerLinTxt = string(tStLinje.PerLinNr).
      end.
  end case.
  ----------------*/
    
  if tStLinje.PerLinNr > 999999 then
    assign
      tStLinje.AntSolgt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      tStLinje.BrekkAnt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      tStLinje.IntAnt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      tStLinje.ReklAnt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      tStLinje.ReklLAnt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      tStLinje.GjenkjopAnt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      tStLinje.KjopAnt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      tStLinje.OvAnt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      tStLinje.AntRab:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
/*       tStLinje.VerdiRabatt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11 */
      tStLinje.JustAnt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
/*       tStLinje.JustVerdi:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11 */
      tStLinje.SvinnAnt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
/*       tStLinje.SvinnVerdi:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11 */
      tStLinje.NedAnt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
/*       tStLinje.NedVerdi:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11 */
/*       tStLinje.VerdiSolgt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11 */
/*       tStLinje.KjopVerdi:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11 */
/*       tStLinje.BrekkVerdi:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11 */
/*       tStLinje.IntVerdi:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11      */
/*       tStLinje.ReklVerdi:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11     */
/*       tStLinje.ReklLVerdi:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11    */
/*       tStLinje.GjenkjopVerdi:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11 */
/*       tStLinje.OvVerdi:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11       */
      tStLinje.Aar:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
/*       tStLinje.MvaVerdi:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11  */
/*       tStLinje.VVareKost:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11 */

      tStLinje.VisBut:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      tStLinje.OmlHast:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      tStLinje.LagerAnt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      tStLinje.Utsolgt%:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      tStLinje.PerLinTxt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
/*       tStLinje.DbKr:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11 */
/*       tStLinje.Db%:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11 */
      wDiverseAnt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
/*       wDiverseVerdi:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11 */
/*       wLagerVerdi:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11   */

      /*
      wVisBut:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      wOmlHast:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      wLagerAnt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      wUtsolgt%:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      wPerLinTxt:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      wDbKr:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      wDb%:bgcolor in browse BROWSE-Statistikk = if tStLinje.PerLinNr = 1000000 then 8 else 11
      */
    .
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveToLokalIni C-Win 
PROCEDURE SaveToLokalIni :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER wKey     AS CHAR NO-UNDO.
DEF INPUT PARAMETER wValue   AS CHAR NO-UNDO.

DEF VAR wSection AS CHAR NO-UNDO.
DEF VAR wMappe   AS CHAR NO-UNDO.
DEF VAR wIniFil  AS CHAR NO-UNDO.

ASSIGN
    wSection = "FILTERPARAMETRE".

/* Henter mappe og filnavn på den lokale inifilen */
IF VALID-HANDLE(wLibHandle) THEN
DO:
  RUN Mappe  IN wLibHandle (OUTPUT wMappe).
  RUN IniFil IN wLibHandle (OUTPUT wIniFil).
END.
ELSE 
    RETURN "AVBRYT".

/* Lagrer parametre */
IF wMappe <> ? THEN 
  DO:
    /* Laster den lokale ini filen. */
    LOAD wIniFil DIR wMappe BASE-KEY "INI" NO-ERROR.
    /* Finnes den ikke, opprette den. */
    IF ERROR-STATUS:ERROR THEN DO:
       LOAD wIniFil DIR wMappe NEW BASE-KEY "INI" NO-ERROR.
       IF ERROR-STATUS:ERROR THEN RETURN "AVBRYT".
    END.           
    /* Stiller om til lokal ini-fil. */
    USE wIniFil NO-ERROR.
    /* Lagrer parameterverdien */
    IF NOT ERROR-STATUS:ERROR THEN
       PUT-KEY-VALUE SECTION wSection KEY wKey VALUE wValue NO-ERROR.
    /* Stiller tilbake til oppstarts ini fil. */
    UNLOAD wIniFil NO-ERROR.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetEntry C-Win 
PROCEDURE SetEntry :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  APPLY "ENTRY":U TO BROWSE-Statistikk IN FRAME FRAME-Historikk.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettOppStatInfo C-Win 
PROCEDURE SettOppStatInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* STATISTIKKTYPE */
  case wStTypeId:
    when "ARTIKKEL" then
      /* ARTIKKEL */
      do:
        /* Henter artikkelinformasjonen. */
        find ArtBas no-lock where 
          Recid(ArtBas) = wArtBasRecid no-error.
        if not available ArtBAs then
          return no-apply.
        find LevBas of ArtBas no-lock no-error.
        find ArtPris no-lock where
          ArtPris.ArtikkelNr = ArtBas.ArtikkelNr and
          ArtPris.ProfilNr   = Butiker.ProfilNr no-error.
        if available ArtPris then
          do:
            assign
              wPris = "Pris: " + 
                      (if ArtPris.Tilbud = true
                         then string(ArtPris.Pris[2])
                         else string(ArtPris.Pris[1])).
          end.
  
        /* Setter vindu, graf tittler og dataobjekt. */
        assign
          wDataObjekt   = string(ArtBas.ArtikkelNr,"9999999999999")
          wVindusTittel = "Artikkelstatistikk for periode: " + wPerId
          wTitle        = "Artikkel: " + string(ArtBas.Vg) + "/" + string(ArtBas.LopNr) + "  " + 
                          ArtBas.LevKod + "/" + ArtBas.LevFarg + 
                          " Leverandør: " + string(ArtBas.LevNr) + " " + 
                          (if available LevBas then LevBas.LevNamn else "") + " " +
                          wPris.
      end. /* ARTIKKEL */
    when "BUTSTAT" then
      do:
        /* Henter Butikkinformasjonen. */
        find Butiker no-lock where 
          Recid(Butiker) = wArtBasRecid no-error.
        if not available Butiker then
          return no-apply.
        assign
           wPris = "".
        /* Setter vindu, graf tittler og dataobjekt. */
        assign
          wDataObjekt   = string(Butiker.Butik,"999999")
          wVindusTittel = "Butikkstatistikk for periode: " + wPerId
          wTitle        = "Butikk: " + string(Butiker.Butik) + " " + string(Butiker.butNamn).
      end. /* BUTSTAT */
    when "HOVEDGR" then
      do:
        /* Henter Hovedgruppeinformasjonen. */
        find HuvGr no-lock where 
          Recid(HuvGr) = wArtBasRecid no-error.
        if not available HuvGr then
          return no-apply.
        assign
           wPris = "".
        /* Setter vindu, graf tittler og dataobjekt. */
        assign
          wDataObjekt   = string(HuvGr.Hg,"9999")
          wVindusTittel = "Hovedgruppestatistikk for periode: " + wPerId
          wTitle        = "Hovedgruppe: " + string(HuvGr.Hg) + " " + string(HuvGr.HgBeskr).
      end. /* HUVEDGR */
    when "KUNDSTAT" then
      do:
        /* Henter kundeinformasjonen. */
        find Kunde no-lock where
          Recid(Kunde) = wArtBasRecid no-error.
        if not available Kunde then
          return no-apply.
        assign
           wPris = "".
        /* Setter vindu, graf tittler og dataobjekt. */
        assign
          wDataObjekt   = string(Kunde.KundeNr,"9999999999999")
          wVindusTittel = "Kundestatistikk for periode: " + wPerId
          wTitle        = "Kunde: " + string(Kunde.KundeNr) + " " + string(Kunde.Navn).
      end. /* KUNDSTAT */
    when "LEVERAN" then
      do:      
        /* Henter leverandørinformasjonen. */
        find LevBas no-lock where 
          Recid(LevBas) = wArtBasRecid no-error.
        if not available LevBas then
          return no-apply.
        assign
           wPris = "".
        /* Setter vindu, graf tittler og dataobjekt. */
        assign
          wDataObjekt   = string(LevBas.LevNr,"999999")
          wVindusTittel = "Leverandørstatistikk for periode: " + wPerId
          wTitle        = "Leverandør: " + string(LevBas.LevNr) + " " + string(LevBas.LevNamn).
      end. /* LEVERAN */
    when "SELGERSTAT" then
      do:
        /* Henter Selgerinformasjonen. */
        find Forsalj no-lock where 
          Recid(Forsalj) = wArtBasRecid no-error.
        if not available Forsalj then
          return no-apply.
        assign
           wPris = "".
        /* Setter vindu, graf tittler og dataobjekt. */
        assign
          wDataObjekt   = string(Forsalj.ForsNr,"9999999999999")
          wVindusTittel = "Selgerstatistikk for periode: " + wPerId
          wTitle        = "Selger: " + string(Forsalj.ForsNr) + " " + string(Forsalj.FoNamn).
      end. /* SELGERSTAT */
    when "VAREGR" then
      do:
        /* Henter Varegruppeinformasjonen. */
        find VarGr no-lock where 
          Recid(VarGr) = wArtBasRecid no-error.
        if not available VarGr then
          return no-apply.
        assign
           wPris = "".
        /* Setter vindu, graf tittler og dataobjekt. */
        assign
          wDataObjekt   = string(VarGr.Vg,"9999")
          wVindusTittel = "Varegruppestatistikk for periode: " + wPerId
          wTitle        = "Varegruppe: " + string(VarGr.Vg) + " " + string(VarGr.VgBeskr).
      end. /* VAREGR */
  end case. /* STATISTIKKTYPE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StatGrafikk C-Win 
PROCEDURE StatGrafikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR wUkeNr AS INT NO-UNDO.
    
  assign
    wVindusTittel = ""
    wTitle        = ""
    wRowLabel     = ""
    wRowKrit      = ""
    wColLabel     = ""
    wFColValues   = ""
    wColEnable    = ""      
    wAntRowLabel  = 0
    wPris         = ""
    wTekst        = "".    
    
    /* -------------
    assign
    *  wVindusTittel = "Vindustittel"
    *  wTitle        = "Artikkel 11/702 XL-13/94 Testgraf"
    *  wRowLabel     = "Kvartal 1,Kvartal 2,Kvartal 3,Kvartal 4"
    *  wColLabel     = "Kjøpt,Solgt,Gjenkjøp,K.Reklam,Lager.Rekl,Brekkasje,Overført,Justert,Svinn,Int.forbruk,Nedskrevet,Rabatt"
      wFColValues   = "3000,600,1100,1900" + ";" + 
                      "2000,100,200,2100" + ";" + 
                      "4000,800,350,1100" + ";" + 
                      "4500,800,350,1900" + ";" + 
                      "3200,800,350,1600" + ";" + 
                      "3000,600,450,1000" + ";" + 
                      "3000,600,450,1000" + ";" + 
                      "3000,600,450,1000" + ";" + 
                      "3000,600,450,1000" + ";" + 
                      "3000,600,450,1000" + ";" + 
                      "3000,600,450,1000"
      wColEnable    = "1,1,0,0,0,0,0,0,0,0,0".      
    ---------------*/

  /* Sentrallager butikk */
  find butiker no-lock where
    Butiker.butik = wCl no-error.
  if not available butiker then
    do:
      message "Sentrallager er ikke satt opp!"
        view-as alert-box error title "Feil".
      return no-apply.
    end.

  /* Setter opp statistikkinformasjonen */
  run SettOppStatInfo.

  /* LabelListe */
  {syspara.i 2 2 1 wColLabel}
  if wColLabel = "" then
    wColLabel = "Kjøpt,Solgt,Gjenkjøp,K.Reklam,Lager.Rekl,Brekkasje,Overført,Justert,Svinn,Int.forbruk,Nedskrevet,Rabatt".

  /* Bygger  RowLabel */
  
  assign
    wAntRowLabel = 0
    wRowLabel    = ""
    wRowKrit     = "".
  case wPerId:
    when "AAR" then
      do:
        do wLoop = int(entry(1,wKriterier)) to int(entry(2,wKriterier)):
          assign
            wRowLabel    = wRowLabel + 
                           (if wRowLabel = ""
                             then ""
                             else ",") + 
                           string(wLoop)
            wRowKrit     = wRowKrit + 
                           (if wRowKrit = ""
                              then ""
                              else "|") +
                           string(wLoop,"9999") + ",1".        
        end.
      end.
    when "MANED" then
      do:
        /* Forespurt periode er innen for ett år. */
        if int(entry(1,wKriterier)) = int(entry(2,wKriterier)) then
          do:           
            do wLoop = int(entry(3,wKriterier)) to int(entry(4,wKriterier)):
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               string(wLoop)
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(1,wKriterier) + "," + string(wLoop).        
             end.
          end.
        /* Forespurt periode går over flere år. */
        else do:
            /* Første år */
            do wLoop = int(entry(3,wKriterier)) to 12:
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               string(wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(1,wKriterier) + "," + string(wLoop).        
            end.
            /*  Mellomliggende år. */
            do wLoop = int(entry(1,wKriterier)) to int(entry(2,wKriterier)):
              if wLoop = int(entry(1,wKriterier)) or wLoop = int(entry(2,wKriterier)) then
                next.
              do w2Loop = 1 to 12:
                assign
                  wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               string(wLoop)        
                  wRowKrit     = wRowKrit + 
                                 (if wRowKrit = ""
                                    then ""
                                    else "|") +
                                 string(int(entry(1,wKriterier)) + wLoop,"9999") + "," + string(w2Loop).        
              end.
            end. 
            /* Siste år. */
            do wLoop = 1 to int(entry(4,wKriterier)):
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               string(wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(2,wKriterier) + "," + string(wLoop).        
            end.
        
        end.
      end.
    when "UKE" then
      do:        
        /* Forespurt periode er innen for ett år. */
        if int(entry(1,wKriterier)) = int(entry(2,wKriterier)) then
          do:            
            assign
              wFraUke = int(entry(3,wKriterier))
              wTilUke = int(entry(4,wKriterier)).                    
            do wLoop = wFraUke to wTilUke:
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               string(wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(1,wKriterier) + "," + string(wLoop).        
             end.
          end.
        /* Forespurt periode går over flere år. */
        else do:
            /* Fra - til uke første år.   */
            /* Til ukenummer må beregnes. */
            RUN weeknum.p (DATE(12,31,INT(entry(1,wKriterier))),OUTPUT wUkeNr).
            assign
              wFraUke = int(entry(3,wKriterier))
              wTilUke = int(SUBstring(STRING(wUkeNr,"999999"),5,2)).
              /*wTilUke = int(entry(4,wKriterier))*/                    
            /* Første år */
            do wLoop = wFraUke to wTilUke:
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               string(wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(1,wKriterier) + "," + string(wLoop).        
            end.
            /*  Mellomliggende år er ikke aktuelt. Maks 53 perioders oppløsning. */

            /* Finner siste ukenummer siste år.*/         
            ASSIGN
              wFraUke = 1
              wTilUke = int(entry(4,wKriterier)).                    
            /* Siste år. */
            do wLoop = 1 to wTilUke:
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               string(wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(2,wKriterier) + "," + string(wLoop).        
            end.        
        end.
      end. /* UKE */
    when "DAG" then
      do:        
        /* Forespurt periode er innen for ett år. */
        if int(entry(1,wKriterier)) = int(entry(2,wKriterier)) then
          do:            
            do wLoop = int(entry(3,wKriterier)) to int(entry(4,wKriterier)):
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               STRING(date(1,1,INT(ENTRY(1,wKriterier))) - 1 + wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(1,wKriterier) + "," + string(wLoop).        
             end.
          end.
        /* Forespurt periode går over flere år. */
        else do:
            do wLoop = int(entry(3,wKriterier)) to 
                       (date(12,31,int(entry(1,wKriterier))) - date(1,1,int(entry(1,wKriterier))) + 1):
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               STRING(date(1,1,INT(ENTRY(1,wKriterier))) - 1 + wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(1,wKriterier) + "," + string(wLoop).        
            end.
            /*  Mellomliggende år er ikke aktuelt. Maks 53 perioders oppløsning. */

            /* Siste år. */
            do wLoop = 1 to int(entry(4,wKriterier)):
              assign
                wRowLabel    = wRowLabel + 
                               (if wRowLabel = ""
                                 then ""
                                 else ",") + 
                               STRING(date(1,1,INT(ENTRY(2,wKriterier))) - 1 + wLoop)        
                wRowKrit     = wRowKrit + 
                               (if wRowKrit = ""
                                  then ""
                                  else "|") +
                               entry(2,wKriterier) + "," + string(wLoop).        
            end.        
        end.
      end.
  end case.
  assign 
    wAntRowLabel = num-entries(wRowLabel).
  /* Grafen kan ikke vise mer med 53 labler. */
  if wAntRowLabel > 53 then
    do:
      message "Grafen kan ikke vise med enn 53 perioder!"
        view-as alert-box message title "Melding".
      return no-apply.
    end.
  
  /* Bygger ColEnable liste */
  wColEnable = "".
  do wLoop = 1 to num-entries(wColLabel):
    wColEnable = wColEnable + 
                 (if wColEnable <> "" 
                    then ","
                    else "") + 
                 (if wLoop <= 2 
                    then "1"
                    else "0").
  end.

  /* Nullstiller sumvariabler. */
  assign
    wKjopt      = 0 
    wSolgt      = 0
    wGjenkjop   = 0
    wKReklam    = 0
    wLagerRekl  = 0
    wBrekkasje  = 0
    wOverfort   = 0
    wJustert    = 0
    wSvinn      = 0
    wIntforbruk = 0
    wNedskrevet = 0
    wRabatt     = 0.

  /*
  MESSAGE 
  "wAar:" wAar SKIP
  "wRowLabel:" wRowLabel SKIP
  "wRowKrit:" wRowKrit SKIP
  "wKriterier:" wKriterier SKIP
  VIEW-AS ALERT-BOX.
  */

  /* Henter verdier fra statistikken */
  STATISTIK:
  do wLoop = 1 to num-entries(wRowLabel): 
    /* Henter kriterier for hver periode fra kriterielisten */
    /* Setter Kriterier */
    assign
      wAar      = int(entry(1,entry(wLoop,wRowKrit,"|"))) 
      wPerLinNr = int(entry(2,entry(wLoop,wRowKrit,"|"))).
  
    /* Sumerer opp for alle butikker */
    for each tStLinje no-lock where
       tStLinje.StTypeId   = wStTypeId and
       tStLinje.PerId      = wPerId and
       tStLinje.DataObjekt = wDataObjekt and
       tStLinje.Diverse    = "" and
       tStLinje.Butik      > 0 AND 
       tStLinje.Butik      < 1000000 AND
       tStLinje.Aar        = wAar and
       tStLinje.PerLinNr   = wPerLinNr:
        
      assign
        wKjopt[wLoop]      = wKjopt[wLoop]      + tStLinje.KjopAnt
        wSolgt[wLoop]      = wSolgt[wLoop]      + tStLinje.AntSolgt
        wGjenkjop[wLoop]   = wGjenkjop[wLoop]   + tStLinje.GjenkjopAnt
        wKReklam [wLoop]   = wKReklam [wLoop]   + tStLinje.ReklAnt
        wLagerRekl[wLoop]  = wLagerRekl[wLoop]  + tStLinje.ReklLAnt
        wBrekkasje[wLoop]  = wBrekkasje[wLoop]  + tStLinje.BrekkAnt
        wOverfort[wLoop]   = wOverfort[wLoop]   + tStLinje.OvAnt
        wJustert[wLoop]    = wJustert[wLoop]    + tStLinje.JustAnt
        wSvinn[wLoop]      = wSvinn[wLoop]      + tStLinje.SvinnAnt
        wIntforbruk[wLoop] = wIntforbruk[wLoop] + tStLinje.IntAnt
        wNedskrevet[wLoop] = wNedSkrevet[wLoop] + tStLinje.NedAnt
        wRabatt[wLoop]     = wRabatt[wLoop]     + tStLinje.AntRab.
    end.
  end. /* STATISTIK */
  
  /* Bygger verdiliste pr. element.*/
  do wLoop = 1 to num-entries(wRowLabel):  
    assign
      wTekst[ 1] = wTekst[ 1] + (if wTekst[ 1] = "" then "" else ",") + string(wKjopt[wLoop])
      wTekst[ 2] = wTekst[ 2] + (if wTekst[ 2] = "" then "" else ",") + string(wSolgt[wLoop])
      wTekst[ 3] = wTekst[ 3] + (if wTekst[ 3] = "" then "" else ",") + string(wGjenkjop[wLoop])
      wTekst[ 4] = wTekst[ 4] + (if wTekst[ 4] = "" then "" else ",") + string(wKReklam [wLoop])
      wTekst[ 5] = wTekst[ 5] + (if wTekst[ 5] = "" then "" else ",") + string(wLagerRekl[wLoop])
      wTekst[ 6] = wTekst[ 6] + (if wTekst[ 6] = "" then "" else ",") + string(wBrekkasje[wLoop])
      wTekst[ 7] = wTekst[ 7] + (if wTekst[ 7] = "" then "" else ",") + string(wOverfort[wLoop])
      wTekst[ 8] = wTekst[ 8] + (if wTekst[ 8] = "" then "" else ",") + string(wJustert[wLoop])
      wTekst[ 9] = wTekst[ 9] + (if wTekst[ 9] = "" then "" else ",") + string(wSvinn[wLoop])
      wTekst[10] = wTekst[10] + (if wTekst[10] = "" then "" else ",") + string(wIntforbruk[wLoop])
      wTekst[11] = wTekst[11] + (if wTekst[11] = "" then "" else ",") + string(wNedSkrevet[wLoop]).   
      wTekst[12] = wTekst[12] + (if wTekst[12] = "" then "" else ",") + string(wRabatt[wLoop]).   
  end.
  
  /* Setter sammen elementverdistrengen. */
  assign
    wFColValues =  wTekst[ 1] + ";" + 
                   wTekst[ 2] + ";" +
                   wTekst[ 3] + ";" +
                   wTekst[ 4] + ";" +
                   wTekst[ 5] + ";" +
                   wTekst[ 6] + ";" +
                   wTekst[ 7] + ";" +
                   wTekst[ 8] + ";" +
                   wTekst[ 9] + ";" +
                   wTekst[10] + ";" +
                   wTekst[11] + ";" +
                   wTekst[12].

  /* TEST */
  /*
  message  "wVindusTittel" wVindusTittel skip
           "wTitle" wTitle skip
           "wRowLabel" wRowLabel skip
           "wColLabel" wColLabel skip
           "wColEnable" wColEnable skip
           "wFColValues" wFColValues skip 
           " wTekst[ 1]" wTekst[ 1] skip
           " wTekst[ 2]" wTekst[ 2]  skip
           " wTekst[ 3]" wTekst[ 3]  skip
           " wTekst[ 4]" wTekst[ 4]  skip
           " wTekst[ 5]" wTekst[ 5]       skip  
           " wTekst[ 6]" wTekst[ 6]  skip
           " wTekst[ 7]" wTekst[ 7]  skip
           " wTekst[ 8]" wTekst[ 8]  skip
           " wTekst[ 9]" wTekst[ 9]  skip
           " wTekst[10]" wTekst[10]  skip
           " wTekst[11]" wTekst[11]  skip
           " wTekst[12]" wTekst[12] skip
  view-as alert-box.
  */
  /* TEST slutt */

  /* Starter grafisk visning. */
  run d-mschart.w (wVindusTittel,
                   wTitle,
                   wRowLabel,
                   wColLabel,
                   wFColValues,
                   wColEnable).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

