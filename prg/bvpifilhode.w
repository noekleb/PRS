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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Target,Data-Target,Update-Source

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dvpifilhode.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table FilId fuNavnEkstVPILev VpiFilType ~
VPIFilStatus fuStatusTekst FilNavn Storrelse Dato Kl AntLinjer fuEDatoTid ~
Katalog 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table FilId VpiFilType ~
VPIFilStatus FilNavn Storrelse Dato Katalog 
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
      FilId FORMAT ">>>>>>>>>>>>9":U
      fuNavnEkstVPILev FORMAT "x(10)":U
      VpiFilType COLUMN-LABEL "FT" FORMAT ">9":U WIDTH 5.4
      VPIFilStatus COLUMN-LABEL "ST" FORMAT ">9":U WIDTH 5.2
      fuStatusTekst FORMAT "x(12)":U
      FilNavn FORMAT "X(60)":U WIDTH 20
      Storrelse FORMAT ">>,>>>,>>>,>>9":U
      Dato FORMAT "99/99/99":U
      Kl FORMAT "X(8)":U
      AntLinjer FORMAT "->>>,>>>,>>9":U
      fuEDatoTid FORMAT "x(25)":U
      Katalog FORMAT "X(40)":U WIDTH 20
  ENABLE
      FilId
      VpiFilType
      VPIFilStatus
      FilNavn
      Storrelse
      Dato
      Katalog
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE SEPARATORS MULTIPLE SIZE 66 BY 6.67 FIT-LAST-COLUMN.


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
   Data Source: "dvpifilhode.w"
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
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
/* BROWSE-TAB br_table 1 F-Main */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

ASSIGN 
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 3.

ASSIGN 
       rowObject.FilId:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.VpiFilType:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.VPIFilStatus:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.FilNavn:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Storrelse:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Dato:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Katalog:COLUMN-READ-ONLY IN BROWSE br_table = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "rowObject"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > _<SDO>.rowObject.FilId
"FilId" ? ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   = _<SDO>.rowObject.fuNavnEkstVPILev
     _FldNameList[3]   > _<SDO>.rowObject.VpiFilType
"VpiFilType" "FT" ? "integer" ? ? ? ? ? ? yes ? no no "5.4" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > _<SDO>.rowObject.VPIFilStatus
"VPIFilStatus" "ST" ? "integer" ? ? ? ? ? ? yes ? no no "5.2" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = _<SDO>.rowObject.fuStatusTekst
     _FldNameList[6]   > _<SDO>.rowObject.FilNavn
"FilNavn" ? ? "character" ? ? ? ? ? ? yes ? no no "20" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > _<SDO>.rowObject.Storrelse
"Storrelse" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > _<SDO>.rowObject.Dato
"Dato" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = _<SDO>.rowObject.Kl
     _FldNameList[10]   = _<SDO>.rowObject.AntLinjer
     _FldNameList[11]   = _<SDO>.rowObject.fuEDatoTid
     _FldNameList[12]   > _<SDO>.rowObject.Katalog
"Katalog" ? ? "character" ? ? ? ? ? ? yes ? no no "20" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
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
ON ROW-DISPLAY OF br_table IN FRAME F-Main
DO:
    CASE RowOBject.VPIFilStatus:
        WHEN  2 THEN RowObject.fuStatusTekst:BGCOLOR IN BROWSE Br_Table = 12.
        WHEN  3 THEN RowObject.fuStatusTekst:BGCOLOR IN BROWSE Br_Table = 11.
        WHEN  4 THEN RowObject.fuStatusTekst:BGCOLOR IN BROWSE Br_Table = 12.
        WHEN  5 THEN RowObject.fuStatusTekst:BGCOLOR IN BROWSE Br_Table = 10.
        WHEN  7 THEN RowObject.fuStatusTekst:BGCOLOR IN BROWSE Br_Table = 12.
        WHEN  8 THEN RowObject.fuStatusTekst:BGCOLOR IN BROWSE Br_Table = 11.
        WHEN  9 THEN RowObject.fuStatusTekst:BGCOLOR IN BROWSE Br_Table = 13.
    END CASE.
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
  DEF VAR h_Col AS HANDLE NO-UNDO.
  ASSIGN
      h_Col = BROWSE Br_Table:FIRST-COLUMN
      .
  DO WHILE VALID-HANDLE(h_Col):
      IF h_Col:COLUMN-READ-ONLY THEN
          h_Col:READ-ONLY = TRUE.
      h_Col = h_Col:NEXT-COLUMN.
  END.

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

