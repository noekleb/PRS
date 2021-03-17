&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
          vpi              PROGRESS
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

DEF VAR cLevNamn   AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR cEkstStorl AS CHAR NO-UNDO.
DEF VAR cFormFage  AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Target,Data-Target,Update-Source

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dvpiartbas.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table Vg LopNr FinnesLokalt VareNr LevKod ~
Beskr LevFargKod Storrelser StrTypeID fuGetInnkjopsPris AnbefaltPris LevNr ~
fuLevNamn BongTekst ArtikkelNr ModellFarge fuGetPris KatalogPris1 ~
KatalogPris2 forhRab%1 forhRab%2 suppRab%1 suppRab%2 LevDato1 LevDato2 ~
LevDato3 LevDato4 SaSong SalgsEnhet Hg VMId SattPaKampanje Pakke ~
AnonseArtikkel KjedeVare VPIDato VPIBildeKode 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table Vg LopNr VareNr 
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


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fuEkstStr bTableWin 
FUNCTION fuEkstStr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
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
      Vg COLUMN-LABEL "Vg" FORMAT "zzzzz9":U WIDTH 5
      LopNr COLUMN-LABEL "LøpeNr" FORMAT "zzzzz9":U WIDTH 8
      FinnesLokalt FORMAT "yes/no":U
      VareNr COLUMN-LABEL "Artikkelnr" FORMAT "X(20)":U WIDTH 21.2
      LevKod FORMAT "x(20)":U
      Beskr FORMAT "x(30)":U
      LevFargKod FORMAT "X(15)":U
      Storrelser FORMAT "yes/no":U
      StrTypeID FORMAT ">>>>>9":U
      fuGetInnkjopsPris FORMAT "->>>,>>>,>>9.99":U
      AnbefaltPris FORMAT "->>>,>>9.99":U
      LevNr FORMAT "zzzzz9":U
      fuLevNamn FORMAT "x(25)":U
      BongTekst FORMAT "X(30)":U
      ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      ModellFarge COLUMN-LABEL "Modellnr" FORMAT ">>>>>>>>>>>>9":U
            WIDTH 11
      fuGetPris FORMAT "->>>,>>>,>>9.99":U
      KatalogPris1 FORMAT "->,>>>,>>9.99":U
      KatalogPris2 FORMAT "->,>>>,>>9.99":U
      forhRab%1 FORMAT "->>9.99":U
      forhRab%2 FORMAT "->>9.99":U
      suppRab%1 FORMAT "->>9.99":U
      suppRab%2 FORMAT "->>9.99":U
      LevDato1 FORMAT "99/99/99":U
      LevDato2 FORMAT "99/99/99":U
      LevDato3 FORMAT "99/99/99":U
      LevDato4 FORMAT "99/99/99":U
      SaSong FORMAT ">>9":U
      SalgsEnhet FORMAT "X(4)":U
      Hg FORMAT ">>>9":U
      VMId FORMAT ">>>>>9":U
      SattPaKampanje FORMAT "99/99/9999":U
      Pakke FORMAT "yes/no":U
      AnonseArtikkel FORMAT "J/N":U
      KjedeVare FORMAT "yes/no":U
      VPIDato FORMAT "99/99/99":U
      VPIBildeKode FORMAT "X(30)":U
  ENABLE
      Vg
      LopNr
      VareNr
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE SEPARATORS MULTIPLE SIZE 79 BY 6.67 ROW-HEIGHT-CHARS .63.


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
   Data Source: "dvpiartbas.w"
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
         HEIGHT             = 6.67
         WIDTH              = 79.4.
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
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1.

ASSIGN 
       rowObject.Vg:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LopNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.VareNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "rowObject"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > _<SDO>.rowObject.Vg
"Vg" "Vg" ? "integer" ? ? ? ? ? ? yes ? no no "5" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > _<SDO>.rowObject.LopNr
"LopNr" "LøpeNr" ? "integer" ? ? ? ? ? ? yes ? no no "8" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > _<SDO>.rowObject.FinnesLokalt
"FinnesLokalt" ? ? "logical" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > _<SDO>.rowObject.VareNr
"VareNr" "Artikkelnr" ? "character" ? ? ? ? ? ? yes ? no no "21.2" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = _<SDO>.rowObject.LevKod
     _FldNameList[6]   = _<SDO>.rowObject.Beskr
     _FldNameList[7]   = _<SDO>.rowObject.LevFargKod
     _FldNameList[8]   = _<SDO>.rowObject.Storrelser
     _FldNameList[9]   = _<SDO>.rowObject.StrTypeID
     _FldNameList[10]   = _<SDO>.rowObject.fuGetInnkjopsPris
     _FldNameList[11]   = _<SDO>.rowObject.AnbefaltPris
     _FldNameList[12]   = _<SDO>.rowObject.LevNr
     _FldNameList[13]   = _<SDO>.rowObject.fuLevNamn
     _FldNameList[14]   = _<SDO>.rowObject.BongTekst
     _FldNameList[15]   = _<SDO>.rowObject.ArtikkelNr
     _FldNameList[16]   > _<SDO>.rowObject.ModellFarge
"ModellFarge" "Modellnr" ? "decimal" ? ? ? ? ? ? no ? no no "11" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   = _<SDO>.rowObject.fuGetPris
     _FldNameList[18]   = _<SDO>.rowObject.KatalogPris1
     _FldNameList[19]   = _<SDO>.rowObject.KatalogPris2
     _FldNameList[20]   = _<SDO>.rowObject.forhRab%1
     _FldNameList[21]   = _<SDO>.rowObject.forhRab%2
     _FldNameList[22]   = _<SDO>.rowObject.suppRab%1
     _FldNameList[23]   = _<SDO>.rowObject.suppRab%2
     _FldNameList[24]   = _<SDO>.rowObject.LevDato1
     _FldNameList[25]   = _<SDO>.rowObject.LevDato2
     _FldNameList[26]   = _<SDO>.rowObject.LevDato3
     _FldNameList[27]   = _<SDO>.rowObject.LevDato4
     _FldNameList[28]   = _<SDO>.rowObject.SaSong
     _FldNameList[29]   = _<SDO>.rowObject.SalgsEnhet
     _FldNameList[30]   = _<SDO>.rowObject.Hg
     _FldNameList[31]   = _<SDO>.rowObject.VMId
     _FldNameList[32]   = _<SDO>.rowObject.SattPaKampanje
     _FldNameList[33]   = _<SDO>.rowObject.Pakke
     _FldNameList[34]   = _<SDO>.rowObject.AnonseArtikkel
     _FldNameList[35]   = _<SDO>.rowObject.KjedeVare
     _FldNameList[36]   = _<SDO>.rowObject.VPIDato
     _FldNameList[37]   = _<SDO>.rowObject.VPIBildeKode
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
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
DO:
  PUBLISH "MouseDblClick".
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
ON GO OF br_table IN FRAME F-Main
DO:
  PUBLISH "MouseDblClick".
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
ON RETURN OF br_table IN FRAME F-Main
DO:
  PUBLISH "MouseDblClick".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON ROW-DISPLAY OF br_table IN FRAME F-Main
DO:
  IF RowOBject.FinnesLokalt = TRUE THEN
      ASSIGN RowObject.ArtikkelNr:BGCOLOR IN BROWSE Br_Table = 11
             RowObject.VareNr:BGCOLOR     IN BROWSE Br_Table = 11.
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
  BROWSE {&BROWSE-NAME}:SELECT-FOCUSED-ROW().
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
  DEF OUTPUT PARAMETER pcListe AS CHAR NO-UNDO.

  DEF VAR piLoop1          AS INT    NO-UNDO.
  DEF VAR pcId             AS CHAR   NO-UNDO.
  DEF VAR hQuery           AS HANDLE NO-UNDO.
  DEF VAR hBuff            AS HANDLE NO-UNDO.
  DEF VAR hField           AS HANDLE NO-UNDO.

  ASSIGN
    hQuery = br_table:QUERY IN FRAME {&FRAME-NAME}
    hBuff  = hQuery:GET-BUFFER-HANDLE(1) /* Default antall poster er 200 */
                                           /* i RowsToBatch                */
    hField = hBuff:BUFFER-FIELD('VareNr')
    .

  /* Satser på at bruker ikke velger så mange poster at variabelen sprekker */
  DO piLoop1 = 1 TO br_table:NUM-SELECTED-ROWS IN FRAME {&FRAME-NAME}:
      br_table:FETCH-SELECTED-ROW(piLoop1).

    assign
        pcId              = hField:BUFFER-VALUE
        pcListe           = pcListe + 
                           (IF pcListe = ""
                              THEN ""
                              ELSE chr(1)) + 
                           pcId
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
/*   DEF VAR h_Col AS HANDLE NO-UNDO.         */
/*   ASSIGN                                   */
/*       h_Col = BROWSE Br_Table:FIRST-COLUMN */
/*       .                                    */
/*   DO WHILE VALID-HANDLE(h_Col):            */
/*       IF h_Col:COLUMN-READ-ONLY THEN       */
/*           h_Col:READ-ONLY = TRUE.          */
/*       h_Col = h_Col:NEXT-COLUMN.           */
/*   END.                                     */

  RUN SUPER.
  
  SUBSCRIBE "SetEntryVPIArtBas" ANYWHERE.
  /* Code placed here will execute AFTER standard behavior.    */
/*   IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW <> ? THEN  */
/*       BROWSE {&BROWSE-NAME}:SELECT-FOCUSED-ROW(). */
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
  DEFINE INPUT  PARAMETER cRadListe      AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER piEkstVPILevNr AS INT        NO-UNDO.
  DEFINE VARIABLE         iCount         AS INTEGER    NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      DYNAMIC-FUNCTION('findRowWhere':U IN DYNAMIC-FUNCTION('getDataSource':U),
       INPUT "EkstVPILevNr,VareNr"  /* CHARACTER */,
        INPUT string(piEkstVPILevNr) + chr(1) + ENTRY(1,cRadListe,CHR(1)) /* CHARACTER */,
        INPUT (IF DYNAMIC-FUNCTION('getQueryWhere':U IN DYNAMIC-FUNCTION('getDataSource':U))
               MATCHES "*DESCENDING*" THEN "<=" ELSE ">=" )/* CHARACTER */).
      APPLY "ENTRY" TO br_table.
  END.

/*   DO WITH FRAME {&FRAME-NAME}:                                                 */
/*     IF br_table:FOCUSED-ROW = ? THEN                                           */
/*         RETURN.                                                                */
/*                                                                                */
/*     IF cRadListe = ? THEN DO:                                                  */
/*         IF br_table:NUM-SELECTED-ROWS > 1 THEN                                 */
/*             br_table:DESELECT-ROWS().                                          */
/*         br_table:SELECT-FOCUSED-ROW().                                         */
/*     END.                                                                       */
/*     ELSE DO iCount = 1 TO 1 /*NUM-ENTRIES(cRadListe,CHR(1))*/:                 */
/*         /* Litt FY FY her... */                                                */
/*         FIND VPIArtBas NO-LOCK WHERE                                           */
/*             VPIArtBas.EkstVPILevNr = piEkstVPILevNr AND                        */
/*             VPIArtBas.VareNr = ENTRY(iCount,cRadListe,CHR(1)) NO-ERROR.        */
/*         IF AVAILABLE VPIArtBas THEN                                            */
/*         DO:                                                                    */
/*             br_table:SELECT-ROW(INT(ENTRY(iCount,cRadListe,CHR(1)))) NO-ERROR. */
/*         END.                                                                   */
/*     END.                                                                       */
/*     APPLY "ENTRY" TO br_table.                                                 */
/*   END.                                                                         */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetEntryVPIArtBas bTableWin 
PROCEDURE SetEntryVPIArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
      APPLY "ENTRY" TO br_table.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetRadFokus bTableWin 
PROCEDURE SetRadFokus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  APPLY "ENTRY" TO BROWSE {&BROWSE-NAME}.
  IF BROWSE {&BROWSE-NAME}:FOCUSED-ROW > 1 THEN
      BROWSE {&BROWSE-NAME}:SELECT-FOCUSED-ROW().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fuEkstStr bTableWin 
FUNCTION fuEkstStr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND FIRST VPIStrekkode NO-LOCK where
    VPIStrekkode.EkstVPILevnr = RowObject.EkstVPILevNr AND
    VPISTrekkode.VareNr       = RowObject.VareNr NO-ERROR.
  IF AVAILABLE VPIStrekkode THEN
    RETURN VPIStrekkode.EkstStorl.
  ELSE
    RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

