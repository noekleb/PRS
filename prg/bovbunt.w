&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
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

DEFINE VARIABLE hDataSource AS HANDLE     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Target,Data-Target,Update-Source

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dovbunt.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table BuntNr Merknad opphav DatoOppdatert ~
fuKlOppdatert OppdatertAv fuFakturaNr Faktura_Id BatchNr fuBatchOppdatert 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table BuntNr Merknad opphav ~
DatoOppdatert Faktura_Id BatchNr 
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


/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-br_table 
       MENU-ITEM m_Sl_sammen    LABEL "Slå sammen"    
       RULE
       MENU-ITEM m_Avbryt       LABEL "Avbryt"        .


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
      BuntNr FORMAT "->,>>>,>>9":U
      Merknad FORMAT "X(30)":U
      opphav FORMAT ">9":U
      DatoOppdatert FORMAT "99/99/99":U
      fuKlOppdatert FORMAT "x(8)":U
      OppdatertAv FORMAT "X(15)":U
      fuFakturaNr FORMAT "x(10)":U WIDTH 12
      Faktura_Id COLUMN-LABEL "Faktura_Id" FORMAT ">>>>>>>>>>>>9":U
      BatchNr FORMAT "zzzzzzzz9":U
      fuBatchOppdatert FORMAT "x(25)":U
  ENABLE
      BuntNr
      Merknad HELP "?"
      opphav HELP "?"
      DatoOppdatert
      Faktura_Id
      BatchNr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 66 BY 6.67 FIT-LAST-COLUMN.


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
   Data Source: "dovbunt.w"
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
       br_table:POPUP-MENU IN FRAME F-Main             = MENU POPUP-MENU-br_table:HANDLE.

ASSIGN 
       rowObject.BuntNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Merknad:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.opphav:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.DatoOppdatert:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Faktura_Id:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.BatchNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "rowObject"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > _<SDO>.rowObject.BuntNr
"BuntNr" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > _<SDO>.rowObject.Merknad
"Merknad" ? ? "character" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > _<SDO>.rowObject.opphav
"opphav" ? ? "integer" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > _<SDO>.rowObject.DatoOppdatert
"DatoOppdatert" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = _<SDO>.rowObject.fuKlOppdatert
     _FldNameList[6]   = _<SDO>.rowObject.OppdatertAv
     _FldNameList[7]   > _<SDO>.rowObject.fuFakturaNr
"fuFakturaNr" ? ? "character" ? ? ? ? ? ? no ? no no "12" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > _<SDO>.rowObject.Faktura_Id
"Faktura_Id" "Faktura_Id" ? "decimal" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > _<SDO>.rowObject.BatchNr
"BatchNr" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > _<SDO>.rowObject.fuBatchOppdatert
"fuBatchOppdatert" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON ENTRY OF br_table IN FRAME F-Main
DO:
  IF br_table:FOCUSED-ROW <> ? THEN
     br_table:SELECT-FOCUSED-ROW().
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


&Scoped-define SELF-NAME m_Avbryt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Avbryt bTableWin
ON CHOOSE OF MENU-ITEM m_Avbryt /* Avbryt */
DO:
  BROWSE {&BROWSE-NAME}:DESELECT-ROWS().
  BROWSE {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Sl_sammen
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Sl_sammen bTableWin
ON CHOOSE OF MENU-ITEM m_Sl_sammen /* Slå sammen */
DO:
    IF BROWSE {&BROWSE-NAME}:NUM-SELECTED-ROWS < 2 THEN DO:
        MESSAGE "Velg flere rader"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        RUN EnBunt.
    END.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnBunt bTableWin 
PROCEDURE EnBunt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCount           AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cValgteBuntNr    AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cOppdatertBuntNr AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cFraBuntNr       AS CHARACTER  NO-UNDO.
    
    RUN GetSelectedRows(OUTPUT cValgteBuntNr,OUTPUT cOppdatertBuntNr).
    IF cOppdatertBuntNr <> "" THEN
        MESSAGE "Feil: Bunt " cOppdatertBuntNr " er oppdatert"
            VIEW-AS ALERT-BOX ERROR BUTTONS OK.
    ELSE DO:
        ASSIGN cFraBuntNr = SUBSTR(cValgteBuntNr,INDEX(cValgteBuntNr,",") + 1).
        MESSAGE cFraBuntNr "Slås sammen med bunt " ENTRY(1,cValgteBuntNr)
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO-CANCEL UPDATE choice AS LOGICAL.
        IF choice THEN DO:
            RUN SlaaSammen IN hDataSource (INPUT cValgteBuntNr).
            DYNAMIC-FUNCTION('OpenQuery':U IN hDataSource).
            BROWSE {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
        END.
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
  DEF OUTPUT PARAMETER pcValgteBuntNr    AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER pcOppdatertBuntNr AS CHAR NO-UNDO.
  DEF VAR piLoop1          AS INT    NO-UNDO.
  DEF VAR hQuery           AS HANDLE NO-UNDO.
  DEF VAR hBuff            AS HANDLE NO-UNDO.
  DEF VAR hField           AS HANDLE NO-UNDO.
  DEF VAR hFieldOppdat     AS HANDLE NO-UNDO.

  ASSIGN
    hQuery = br_table:QUERY IN FRAME {&FRAME-NAME}
    hBuff  = hQuery:GET-BUFFER-HANDLE(1) /* Default antall poster er 200 */
                                           /* i RowsToBatch                */
    hFieldOppdat = hBuff:BUFFER-FIELD('DatoOppdatert').
    hField = hBuff:BUFFER-FIELD('BuntNr').
    

  /* Satser på at bruker ikke velger så mange poster at variabelen sprekker */
  DO piLoop1 = 1 TO br_table:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
      br_table:FETCH-SELECTED-ROW(piLoop1).

    assign
        pcValgteBuntNr  = pcValgteBuntNr + 
                           (IF pcValgteBuntNr = ""
                              THEN ""
                              ELSE ",") + 
                           hField:BUFFER-VALUE
        pcOppdatertBuntNr = pcOppdatertBuntNr + 
        (IF pcOppdatertBuntNr = ""
           THEN ""
           ELSE IF hFieldOppdat:BUFFER-VALUE <> ? THEN "," ELSE "") + 
           IF hFieldOppdat:BUFFER-VALUE <> ? THEN hField:BUFFER-VALUE ELSE ""
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

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
  ASSIGN hDataSource = DYNAMIC-FUNCTION('getDataSource':U).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

