&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Target,Data-Target,Update-Source

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dtstlinje.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table rowObject.VisBut rowObject.Aar ~
rowObject.PerLinTxt rowObject.KjopAnt rowObject.KjopVerdi ~
rowObject.AntSolgt rowObject.VerdiSolgt rowObject.DbKr rowObject.VVarekost ~
rowObject.Db% rowObject.Utsolgt% rowObject.LagerAnt rowObject.LagerVerdi ~
rowObject.AntRabatt rowObject.VerdiRabatt rowObject.ReklAnt ~
rowObject.ReklVerdi rowObject.ReklLAnt rowObject.ReklLVerdi rowObject.OvAnt ~
rowObject.OvVerdi rowObject.GjenkjopAnt rowObject.GjenkjopVerdi ~
rowObject.OmlHast rowObject.MvaVerdi rowObject.BrekkAnt ~
rowObject.BrekkVerdi rowObject.JustAnt rowObject.JustVerdi ~
rowObject.SvinnAnt rowObject.SvinnVerdi rowObject.IntAnt rowObject.IntVerdi ~
rowObject.NedAnt rowObject.NedVerdi rowObject.DiverseAnt ~
rowObject.Diverseverdi 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
&Scoped-define OPEN-QUERY-br_table OPEN QUERY br_table FOR EACH rowObject NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-br_table rowObject
&Scoped-define FIRST-TABLE-IN-QUERY-br_table rowObject


/* Definitions for FRAME F-Main                                         */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS br_table 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
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
      rowObject.VisBut COLUMN-LABEL "Butikk" FORMAT "X(7)":U
      rowObject.Aar FORMAT "9999":U
      rowObject.PerLinTxt COLUMN-LABEL "Periode" FORMAT "xxxxxxxxxxxxxxxxxxxx":U
      rowObject.KjopAnt COLUMN-LABEL "Kjøpt" FORMAT "->>,>>9":U
            WIDTH 8
      rowObject.KjopVerdi COLUMN-LABEL "V-Kjøpt" FORMAT "-zz,zzz,zz9":U
      rowObject.AntSolgt COLUMN-LABEL "Solgt" FORMAT "->>,>>>,>>9":U
      rowObject.VerdiSolgt COLUMN-LABEL "V-Solgt" FORMAT "-zz,zzz,zz9":U
      rowObject.DbKr FORMAT "-zz,zzz,zz9":U
      rowObject.VVarekost FORMAT "-zz,zzz,zz9":U
      rowObject.Db% FORMAT "-zzz9.99":U
      rowObject.Utsolgt% FORMAT "-zzz9.99":U
      rowObject.LagerAnt COLUMN-LABEL "Lager" FORMAT "->,>>>,>>9":U
      rowObject.LagerVerdi COLUMN-LABEL "V-Lager" FORMAT "-zz,zzz,zz9":U
      rowObject.AntRabatt COLUMN-LABEL "Rabatt" FORMAT "->>,>>>,>>9":U
      rowObject.VerdiRabatt COLUMN-LABEL "V-Rabatt" FORMAT "-zz,zzz,zz9":U
      rowObject.ReklAnt FORMAT "->>,>>9":U
      rowObject.ReklVerdi COLUMN-LABEL "V-K.Reklam" FORMAT "-zz,zzz,zz9":U
      rowObject.ReklLAnt FORMAT "->>,>>9":U
      rowObject.ReklLVerdi COLUMN-LABEL "V-Rekl.lev" FORMAT "-zz,zzz,zz9":U
      rowObject.OvAnt COLUMN-LABEL "Overført" FORMAT "->>,>>9":U
      rowObject.OvVerdi COLUMN-LABEL "V-Overført" FORMAT "-zz,zzz,zz9":U
      rowObject.GjenkjopAnt COLUMN-LABEL "Retur" FORMAT "->>,>>9":U
      rowObject.GjenkjopVerdi COLUMN-LABEL "V-Retur" FORMAT "-zz,zzz,zz9":U
      rowObject.OmlHast FORMAT "->>,>>9.99":U
      rowObject.MvaVerdi FORMAT "-zz,zzz,zz9":U
      rowObject.BrekkAnt FORMAT "->>,>>9":U
      rowObject.BrekkVerdi COLUMN-LABEL "V-Brekkasje" FORMAT "-zz,zzz,zz9":U
      rowObject.JustAnt COLUMN-LABEL "Justert" FORMAT "->>,>>9":U
      rowObject.JustVerdi COLUMN-LABEL "V-Justert" FORMAT "-zz,zzz,zz9":U
      rowObject.SvinnAnt COLUMN-LABEL "Svinn" FORMAT "->>,>>9":U
      rowObject.SvinnVerdi COLUMN-LABEL "V-Svinn" FORMAT "-zz,zzz,zz9":U
      rowObject.IntAnt FORMAT "->>,>>9":U
      rowObject.IntVerdi COLUMN-LABEL "V-Internt forbruk" FORMAT "-zz,zzz,zz9":U
      rowObject.NedAnt COLUMN-LABEL "Nedskrevet" FORMAT "->>,>>9":U
      rowObject.NedVerdi COLUMN-LABEL "V-Nedskrevet" FORMAT "-zz,zzz,zz9":U
      rowObject.DiverseAnt FORMAT "->>>,>>>,>>9":U
      rowObject.Diverseverdi FORMAT "-zz,zzz,zz9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE NO-ROW-MARKERS SEPARATORS SIZE 66 BY 6.67 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     br_table AT ROW 1 COL 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataBrowser
   Data Source: "dtstlinje.w"
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
         HEIGHT             = 6.86
         WIDTH              = 66.
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
   NOT-VISIBLE Size-to-Fit                                              */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "rowObject"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > _<SDO>.rowObject.VisBut
"rowObject.VisBut" "Butikk" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   = _<SDO>.rowObject.Aar
     _FldNameList[3]   > _<SDO>.rowObject.PerLinTxt
"rowObject.PerLinTxt" "Periode" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[4]   > _<SDO>.rowObject.KjopAnt
"rowObject.KjopAnt" "Kjøpt" ? "decimal" ? ? ? ? ? ? no ? no no "8" yes no no "U" "" ""
     _FldNameList[5]   > _<SDO>.rowObject.KjopVerdi
"rowObject.KjopVerdi" "V-Kjøpt" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[6]   > _<SDO>.rowObject.AntSolgt
"rowObject.AntSolgt" "Solgt" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[7]   > _<SDO>.rowObject.VerdiSolgt
"rowObject.VerdiSolgt" "V-Solgt" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[8]   = _<SDO>.rowObject.DbKr
     _FldNameList[9]   = _<SDO>.rowObject.VVarekost
     _FldNameList[10]   = _<SDO>.rowObject.Db%
     _FldNameList[11]   = _<SDO>.rowObject.Utsolgt%
     _FldNameList[12]   > _<SDO>.rowObject.LagerAnt
"rowObject.LagerAnt" "Lager" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[13]   > _<SDO>.rowObject.LagerVerdi
"rowObject.LagerVerdi" "V-Lager" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[14]   > _<SDO>.rowObject.AntRabatt
"rowObject.AntRabatt" "Rabatt" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[15]   > _<SDO>.rowObject.VerdiRabatt
"rowObject.VerdiRabatt" "V-Rabatt" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[16]   = _<SDO>.rowObject.ReklAnt
     _FldNameList[17]   > _<SDO>.rowObject.ReklVerdi
"rowObject.ReklVerdi" "V-K.Reklam" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[18]   = _<SDO>.rowObject.ReklLAnt
     _FldNameList[19]   > _<SDO>.rowObject.ReklLVerdi
"rowObject.ReklLVerdi" "V-Rekl.lev" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[20]   > _<SDO>.rowObject.OvAnt
"rowObject.OvAnt" "Overført" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[21]   > _<SDO>.rowObject.OvVerdi
"rowObject.OvVerdi" "V-Overført" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[22]   > _<SDO>.rowObject.GjenkjopAnt
"rowObject.GjenkjopAnt" "Retur" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[23]   > _<SDO>.rowObject.GjenkjopVerdi
"rowObject.GjenkjopVerdi" "V-Retur" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[24]   = _<SDO>.rowObject.OmlHast
     _FldNameList[25]   = _<SDO>.rowObject.MvaVerdi
     _FldNameList[26]   = _<SDO>.rowObject.BrekkAnt
     _FldNameList[27]   > _<SDO>.rowObject.BrekkVerdi
"rowObject.BrekkVerdi" "V-Brekkasje" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[28]   > _<SDO>.rowObject.JustAnt
"rowObject.JustAnt" "Justert" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[29]   > _<SDO>.rowObject.JustVerdi
"rowObject.JustVerdi" "V-Justert" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[30]   > _<SDO>.rowObject.SvinnAnt
"rowObject.SvinnAnt" "Svinn" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[31]   > _<SDO>.rowObject.SvinnVerdi
"rowObject.SvinnVerdi" "V-Svinn" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[32]   = _<SDO>.rowObject.IntAnt
     _FldNameList[33]   > _<SDO>.rowObject.IntVerdi
"rowObject.IntVerdi" "V-Internt forbruk" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[34]   > _<SDO>.rowObject.NedAnt
"rowObject.NedAnt" "Nedskrevet" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[35]   > _<SDO>.rowObject.NedVerdi
"rowObject.NedVerdi" "V-Nedskrevet" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[36]   = _<SDO>.rowObject.DiverseAnt
     _FldNameList[37]   = _<SDO>.rowObject.Diverseverdi
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
    IF RowObject.PerLinNr > 999999 then 
      assign
        RowObject.AntSolgt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.BrekkAnt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.IntAnt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.ReklAnt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.ReklLAnt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.GjenkjopAnt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.KjopAnt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.OvAnt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.AntRab:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.VerdiRabatt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.JustAnt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.JustVerdi:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.SvinnAnt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.SvinnVerdi:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.NedAnt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.NedVerdi:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.VerdiSolgt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.KjopVerdi:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.BrekkVerdi:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.IntVerdi:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.ReklVerdi:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.ReklLVerdi:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.GjenkjopVerdi:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.OvVerdi:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.Aar:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.MvaVerdi:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.VVareKost:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11

        RowObject.VisBut:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.OmlHast:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.LagerAnt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.Utsolgt%:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.PerLinTxt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.DbKr:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.Db%:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.DiverseAnt:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.DiverseVerdi:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
        RowObject.LagerVerdi:bgcolor in browse Br_Table = if RowObject.PerLinNr = 1000000 then 8 else 11
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExHtmRapp bTableWin 
PROCEDURE ExHtmRapp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER wDokTyp     AS CHAR   NO-UNDO.
  DEFINE INPUT PARAMETER T-Tot       AS LOG    NO-UNDO.

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
  DEFINE VAR wExcEkstent  AS CHAR NO-UNDO.

  DEFINE VAR h_dtstlinje  AS HANDLE NO-UNDO.

  ASSIGN
      h_dtstlinje = DYNAMIC-FUNCTION('getDataSource':U)
      .
  RUN GetExcelExtent IN h_dproclib (OUTPUT wExcEkstent).

  {sww.i}
 DO:
  if valid-handle(h_dproclib) then
    run GetTempFileName in h_dproclib 
      (wDokTyp, 
       IF wDokTyp BEGINS "HTM" 
         THEN "HTM" 
         ELSE wExcEkstent, 
       output wtmpFileName). 

    ASSIGN
        hQuery = h_dtstlinje
        .
  DO:
    ASSIGN wLblHdl = BROWSE {&BROWSE-NAME}:FIRST-COLUMN.
    REPEAT WHILE VALID-HANDLE(wLblHdl).
      IF wLblHdl:label = "" THEN LEAVE.
      ASSIGN 
        wColHead     = wColHead + 
                       (if wColHead <> "" then wSep else "") + 
                       TRIM(REPLACE(wLblHdl:label,"*",""))
        wFields      = wFields + (if wFields <> "" then "," else "") + 
                       TRIM(wLblHdl:NAME)
        wFormat      = wFormat + 
                       (if wFormat <> "" then wSep else "") + 
                       IF (wDokTyp = "HTM" AND wLblHdl:FORMAT BEGINS "-") 
                         THEN SUBSTR(wLblHdl:FORMAT,2) + "-" 
                         ELSE wLblHdl:FORMAT
        wTotWidthC   = wTotWidthC + LENGTH(wLblHdl:FORMAT) /* wLblHdl:WIDTH-PIXELS */
        wColHeadForm = wColHeadForm + 
                       (if wColHeadForm = "" then "" else wSep) +
                       (IF LENGTH(wLblHdl:FORMAT) = 1 
                         THEN "C" 
                         ELSE (IF wLblHdl:DATA-TYPE BEGINS "INTE" OR wLblHdl:DATA-TYPE BEGINS "DECI" 
                               THEN "R" 
                               ELSE "L"))
        wColWidthC   = wColWidthC + 
                       (if wColWidthC = "" then "" else wSep) + 
                        STRING(LENGTH(wLblHdl:FORMAT)) /* string(INT(wLblHdl:WIDTH-PIXELS)) */
        wLblHdl      = wLblHdl:NEXT-COLUMN
        .
    end.
    ASSIGN wFormat = IF wDokTyp = "HTM" THEN wFormat ELSE REPLACE(wFormat,",","").
    DO wCount = 1 TO NUM-ENTRIES(wColHeadForm,wSep):
      ENTRY(wCount,wColHeadForm,wSep) = ENTRY(wCount,wColHeadForm,wSep) + 
                                        "," + 
                                        String(INT(DECI(ENTRY(wCount,wColWidthC,wSep)) / wTotWidthC * 100)) + "%".
    END.
  END.
  ASSIGN wTabell      = "Statistik"
         wHead1Set    = "100%,,1,0,2," + STRING(NUM-ENTRIES(wFields))
         wQy          = DYNAMIC-FUNCTION('getQueryWhere':U IN h_dtstlinje)
         .
  RUN htmlstlinje IN h_dtstlinje
      (wDokTyp,
       wSep,
       wHead1Set,
       wColHead,
       wFields,
       wFormat,
       wColHeadForm,
       wTabell,
       T-Tot,
       wtmpFileName).

  if valid-handle(h_dproclib) then DO:
    IF wDokTyp = "HTM" THEN
        RUN OpenWeb in h_dproclib (wtmpFileName).
    ELSE run OpenExcelDocument in h_dproclib (wtmpFileName," ").
  END.
  {swn.i}
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

