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
&Scoped-define DATA-FIELD-DEFS "sdo/dordre.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table OrdreNr Hasteordre EkstId VareBehNr ~
OrdreStatus fStatusTxt LevNr SendtDato BekreftetOrdre BekreftetDato Merknad ~
LapTop LevAdresse1 LevAdresse2 LevPostBoks LevPostNr LevKontakt LevMerknad ~
LevTelefon LeveringsDato fraERP HkOrdre RegistrertDato EDato BrukerID 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table OrdreNr EkstId VareBehNr ~
OrdreStatus LevNr SendtDato BekreftetOrdre BekreftetDato Merknad LapTop ~
LevAdresse1 LevPostBoks LevPostNr LevKontakt LevMerknad LeveringsDato ~
fraERP HkOrdre RegistrertDato EDato BrukerID 
&Scoped-define QUERY-STRING-br_table FOR EACH rowObject NO-LOCK INDEXED-REPOSITION
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
      OrdreNr FORMAT "zzzzzzz9":U
      Hasteordre COLUMN-LABEL "HOrd" FORMAT "*/":U WIDTH 5
      EkstId FORMAT "X(15)":U
      VareBehNr FORMAT ">>>>>>>>>>>>9":U
      OrdreStatus COLUMN-LABEL "S" FORMAT ">9":U
      fStatusTxt FORMAT "x(8)":U
      LevNr FORMAT "zzzzz9":U
      SendtDato FORMAT "99/99/9999":U
      BekreftetOrdre FORMAT "yes/no":U
      BekreftetDato FORMAT "99/99/99":U
      Merknad FORMAT "X(40)":U
      LapTop FORMAT "Ja/Nei":U
      LevAdresse1 FORMAT "X(40)":U
      LevAdresse2 FORMAT "X(40)":U
      LevPostBoks FORMAT "X(40)":U
      LevPostNr FORMAT "X(10)":U
      LevKontakt FORMAT "X(30)":U
      LevMerknad FORMAT "X(50)":U
      LevTelefon FORMAT "X(15)":U
      LeveringsDato FORMAT "99/99/99":U
      fraERP FORMAT "yes/no":U
      HkOrdre FORMAT "yes/no":U
      RegistrertDato FORMAT "99/99/9999":U
      EDato FORMAT "99/99/9999":U
      BrukerID FORMAT "X(10)":U
  ENABLE
      OrdreNr
      EkstId
      VareBehNr
      OrdreStatus
      LevNr
      SendtDato
      BekreftetOrdre
      BekreftetDato
      Merknad
      LapTop
      LevAdresse1
      LevPostBoks
      LevPostNr
      LevKontakt
      LevMerknad
      LeveringsDato HELP "?"
      fraERP
      HkOrdre
      RegistrertDato
      EDato
      BrukerID
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
   Data Source: "sdo/dordre.w"
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
       rowObject.OrdreNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.EkstId:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.VareBehNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.OrdreStatus:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LevNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.SendtDato:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.BekreftetOrdre:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.BekreftetDato:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Merknad:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LapTop:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LevAdresse1:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LevPostBoks:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LevPostNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LevKontakt:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LevMerknad:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LeveringsDato:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.fraERP:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.HkOrdre:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.RegistrertDato:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.EDato:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.BrukerID:COLUMN-READ-ONLY IN BROWSE br_table = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "rowObject"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > _<SDO>.rowObject.OrdreNr
"OrdreNr" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[2]   > _<SDO>.rowObject.Hasteordre
"Hasteordre" "HOrd" ? "logical" ? ? ? ? ? ? no ? no no "5" yes no no "U" "" ""
     _FldNameList[3]   > _<SDO>.rowObject.EkstId
"EkstId" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[4]   > _<SDO>.rowObject.VareBehNr
"VareBehNr" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[5]   > _<SDO>.rowObject.OrdreStatus
"OrdreStatus" "S" ? "integer" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[6]   = _<SDO>.rowObject.fStatusTxt
     _FldNameList[7]   > _<SDO>.rowObject.LevNr
"LevNr" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[8]   > _<SDO>.rowObject.SendtDato
"SendtDato" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[9]   > _<SDO>.rowObject.BekreftetOrdre
"BekreftetOrdre" ? ? "logical" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[10]   > _<SDO>.rowObject.BekreftetDato
"BekreftetDato" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[11]   > _<SDO>.rowObject.Merknad
"Merknad" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[12]   > _<SDO>.rowObject.LapTop
"LapTop" ? ? "logical" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[13]   > _<SDO>.rowObject.LevAdresse1
"LevAdresse1" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[14]   = _<SDO>.rowObject.LevAdresse2
     _FldNameList[15]   > _<SDO>.rowObject.LevPostBoks
"LevPostBoks" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[16]   > _<SDO>.rowObject.LevPostNr
"LevPostNr" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[17]   > _<SDO>.rowObject.LevKontakt
"LevKontakt" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[18]   > _<SDO>.rowObject.LevMerknad
"LevMerknad" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[19]   = _<SDO>.rowObject.LevTelefon
     _FldNameList[20]   > _<SDO>.rowObject.LeveringsDato
"LeveringsDato" ? ? "date" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" ""
     _FldNameList[21]   > _<SDO>.rowObject.fraERP
"fraERP" ? ? "logical" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[22]   > _<SDO>.rowObject.HkOrdre
"HkOrdre" ? ? "logical" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[23]   > _<SDO>.rowObject.RegistrertDato
"RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[24]   > _<SDO>.rowObject.EDato
"EDato" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[25]   > _<SDO>.rowObject.BrukerID
"BrukerID" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
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
ON ANY-PRINTABLE OF br_table IN FRAME F-Main
DO:
  PUBLISH "ANYPRINTABLE".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
ON START-SEARCH OF br_table IN FRAME F-Main
DO:
    PUBLISH "Sortera".
    APPLY "END-SEARCH" TO SELF.
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

