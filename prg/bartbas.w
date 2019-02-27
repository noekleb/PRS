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
&Scoped-define DATA-FIELD-DEFS "sdo/dartbas.i"

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table rowObject.Beskr rowObject.fuLevNavn ~
rowObject.LevKod rowObject.fVgBeskr rowObject.Vg rowObject.LopNr ~
rowObject.BongTekst rowObject.VgKat rowObject.Hg rowObject.fiSasong ~
rowObject.Farg rowObject.LevFargKod rowObject.StrTypeID rowObject.Pakkenr ~
rowObject.MatKod rowObject.ArtikkelNr rowObject.RegistrertDato ~
rowObject.EDato rowObject.inn_dato rowObject.BildeIKasse rowObject.HkStyrt ~
rowObject.LokPris rowObject.IKasse rowObject.OPris rowObject.Aktivert ~
rowObject.AktivDato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table rowObject.Beskr ~
rowObject.LevKod rowObject.Vg rowObject.LopNr rowObject.BongTekst ~
rowObject.Hg rowObject.LevFargKod rowObject.StrTypeID rowObject.Pakkenr ~
rowObject.ArtikkelNr rowObject.RegistrertDato rowObject.EDato ~
rowObject.Aktivert rowObject.AktivDato 
&Scoped-define ENABLED-TABLES-IN-QUERY-br_table rowObject
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-br_table rowObject
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
      Beskr FORMAT "x(20)":U
      fuLevNavn FORMAT "x(30)":U WIDTH 20
      LevKod COLUMN-LABEL "Levartnr" FORMAT "x(20)":U
      fVgBeskr FORMAT "x(20)":U
      Vg COLUMN-LABEL "Vg" FORMAT "zzzzz9":U WIDTH 6
      LopNr COLUMN-LABEL "Løpenr" FORMAT "zzz9":U WIDTH 8
      BongTekst FORMAT "X(30)":U WIDTH 20
      VgKat COLUMN-LABEL "Kat" FORMAT "z9":U WIDTH 4
      Hg FORMAT ">>>9":U
      fiSasong FORMAT "x(14)":U
      Farg FORMAT ">>9":U
      LevFargKod COLUMN-LABEL "Levfarve" FORMAT "X(15)":U
      StrTypeID FORMAT "zzzzz9":U
      Pakkenr FORMAT "ZZZZ":U WIDTH 9.4
      MatKod COLUMN-LABEL "Material" FORMAT "z9":U
      ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      RegistrertDato FORMAT "99/99/9999":U
      EDato FORMAT "99/99/9999":U
      inn_dato FORMAT "99/99/99":U
      BildeIKasse FORMAT "yes/no":U
      HkStyrt FORMAT "yes/no":U
      LokPris FORMAT "yes/no":U
      IKasse FORMAT "yes/no":U
      OPris FORMAT "yes/no":U
      Aktivert FORMAT "yes/no":U
      AktivDato FORMAT "99/99/9999":U
  ENABLE
      Beskr
      LevKod
      Vg
      LopNr
      BongTekst
      Hg
      LevFargKod
      StrTypeID
      Pakkenr HELP "?"
      ArtikkelNr HELP "?"
      RegistrertDato
      EDato
      Aktivert
      AktivDato
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE NO-ROW-MARKERS SEPARATORS SIZE 125 BY 6.67 EXPANDABLE.


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
   Data Source: "sdo/dartbas.w"
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
         WIDTH              = 125.8.
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
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 1.

ASSIGN 
       rowObject.Beskr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LevKod:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Vg:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LopNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.BongTekst:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Hg:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LevFargKod:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.StrTypeID:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Pakkenr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.ArtikkelNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.RegistrertDato:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.EDato:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Aktivert:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.AktivDato:COLUMN-READ-ONLY IN BROWSE br_table = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "rowObject"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > _<SDO>.rowObject.Beskr
"Beskr" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[2]   > _<SDO>.rowObject.fuLevNavn
"fuLevNavn" ? ? "character" ? ? ? ? ? ? no "?" no no "20" yes no no "U" "" ""
     _FldNameList[3]   > _<SDO>.rowObject.LevKod
"LevKod" "Levartnr" ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[4]   = _<SDO>.rowObject.fVgBeskr
     _FldNameList[5]   > _<SDO>.rowObject.Vg
"Vg" "Vg" ? "integer" ? ? ? ? ? ? yes ? no no "6" yes no yes "U" "" ""
     _FldNameList[6]   > _<SDO>.rowObject.LopNr
"LopNr" "Løpenr" ? "integer" ? ? ? ? ? ? yes ? no no "8" yes no yes "U" "" ""
     _FldNameList[7]   > _<SDO>.rowObject.BongTekst
"BongTekst" ? ? "character" ? ? ? ? ? ? yes ? no no "20" yes no yes "U" "" ""
     _FldNameList[8]   > _<SDO>.rowObject.VgKat
"VgKat" "Kat" ? "integer" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" ""
     _FldNameList[9]   > _<SDO>.rowObject.Hg
"Hg" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[10]   > _<SDO>.rowObject.fiSasong
"fiSasong" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[11]   = _<SDO>.rowObject.Farg
     _FldNameList[12]   > _<SDO>.rowObject.LevFargKod
"LevFargKod" "Levfarve" ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[13]   > _<SDO>.rowObject.StrTypeID
"StrTypeID" ? ? "integer" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[14]   > _<SDO>.rowObject.Pakkenr
"Pakkenr" ? ? "integer" ? ? ? ? ? ? yes "?" no no "9.4" yes no yes "U" "" ""
     _FldNameList[15]   > _<SDO>.rowObject.MatKod
"MatKod" "Material" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[16]   > _<SDO>.rowObject.ArtikkelNr
"ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" ""
     _FldNameList[17]   > _<SDO>.rowObject.RegistrertDato
"RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[18]   > _<SDO>.rowObject.EDato
"EDato" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[19]   > _<SDO>.rowObject.inn_dato
"inn_dato" ? ? "date" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" ""
     _FldNameList[20]   = _<SDO>.rowObject.BildeIKasse
     _FldNameList[21]   = _<SDO>.rowObject.HkStyrt
     _FldNameList[22]   = _<SDO>.rowObject.LokPris
     _FldNameList[23]   = _<SDO>.rowObject.IKasse
     _FldNameList[24]   = _<SDO>.rowObject.OPris
     _FldNameList[25]   > _<SDO>.rowObject.Aktivert
"Aktivert" ? ? "logical" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
     _FldNameList[26]   > _<SDO>.rowObject.AktivDato
"AktivDato" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" ""
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
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main
DO:
  PUBLISH "Artikkelkort".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject bTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */
/*  DEF VAR h_Col AS HANDLE NO-UNDO.          */
/*   ASSIGN                                   */
/*       h_Col = BROWSE Br_Table:FIRST-COLUMN */
/*       .                                    */
/*   DO WHILE VALID-HANDLE(h_Col):            */
/*       IF h_Col:COLUMN-READ-ONLY THEN       */
/*           h_Col:READ-ONLY = TRUE.          */
/*       h_Col = h_Col:NEXT-COLUMN.           */
/*   END.                                     */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

