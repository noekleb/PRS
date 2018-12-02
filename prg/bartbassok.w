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

DEF VAR cFargeKoding      AS CHAR NO-UNDO.
DEF VAR cTilleggskrit     AS CHAR NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Target,Data-Target,Update-Source

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dartbassok.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table Vg LopNr LevKod Beskr LevFargKod ~
Aktivert IKasse Farg fFargBeskr Lokasjon fuPris fuVarekost Hg ArtikkelNr ~
BongTekst fLevNamn SaSong fSesong LevNr RegistrertDato EDato Pakke OPris ~
HkStyrt LokPris BildeIKasse 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table Vg LopNr LevKod Beskr ~
LevFargKod Farg Lokasjon Hg ArtikkelNr BongTekst SaSong LevNr ~
RegistrertDato EDato 
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
      Vg COLUMN-LABEL "Vg" FORMAT "zzzzz9":U WIDTH 5
      LopNr COLUMN-LABEL "LøpeNr" FORMAT "zzzzz9":U WIDTH 7
      LevKod FORMAT "x(20)":U WIDTH 26.4
      Beskr FORMAT "x(30)":U WIDTH 26.2
      LevFargKod FORMAT "X(15)":U
      Aktivert FORMAT "*/":U
      IKasse COLUMN-LABEL "IK" FORMAT "*/":U WIDTH 2
      Farg COLUMN-LABEL "Fa" FORMAT "zzzz9":U WIDTH 7
      fFargBeskr FORMAT "x(20)":U WIDTH 15
      Lokasjon FORMAT "X(20)":U
      fuPris FORMAT "->>>,>>9.99":U
      fuVarekost FORMAT "->>>,>>9.99":U
      Hg COLUMN-LABEL "Hg" FORMAT ">>>9":U WIDTH 5
      ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      BongTekst FORMAT "X(30)":U
      fLevNamn FORMAT "x(20)":U
      SaSong COLUMN-LABEL "Se" FORMAT "zz9":U WIDTH 4
      fSesong FORMAT "x(10)":U
      LevNr COLUMN-LABEL "Le" FORMAT "zzzzz9":U WIDTH 7
      RegistrertDato FORMAT "99/99/9999":U
      EDato FORMAT "99/99/9999":U
      Pakke FORMAT "yes/no":U
      OPris FORMAT "yes/no":U
      HkStyrt FORMAT "yes/no":U
      LokPris FORMAT "yes/no":U
      BildeIKasse FORMAT "yes/no":U
  ENABLE
      Vg
      LopNr
      LevKod
      Beskr
      LevFargKod
      Farg
      Lokasjon
      Hg
      ArtikkelNr HELP "?"
      BongTekst
      SaSong
      LevNr
      RegistrertDato
      EDato
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE NO-ROW-MARKERS SEPARATORS SIZE 122 BY 19.76 FIT-LAST-COLUMN.


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
   Data Source: "dartbassok.w"
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
         HEIGHT             = 19.76
         WIDTH              = 122.2.
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
       rowObject.LevKod:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Beskr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LevFargKod:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Farg:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Lokasjon:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Hg:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.ArtikkelNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.BongTekst:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.SaSong:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LevNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.RegistrertDato:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.EDato:COLUMN-READ-ONLY IN BROWSE br_table = TRUE.

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
"LopNr" "LøpeNr" ? "integer" ? ? ? ? ? ? yes ? no no "7" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > _<SDO>.rowObject.LevKod
"LevKod" ? ? "character" ? ? ? ? ? ? yes ? no no "26.4" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > _<SDO>.rowObject.Beskr
"Beskr" ? ? "character" ? ? ? ? ? ? yes ? no no "26.2" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > _<SDO>.rowObject.LevFargKod
"LevFargKod" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   = _<SDO>.rowObject.Aktivert
     _FldNameList[7]   > _<SDO>.rowObject.IKasse
"IKasse" "IK" ? "logical" ? ? ? ? ? ? no ? no no "2" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > _<SDO>.rowObject.Farg
"Farg" "Fa" ? "integer" ? ? ? ? ? ? yes ? no no "7" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > _<SDO>.rowObject.fFargBeskr
"fFargBeskr" ? ? "character" ? ? ? ? ? ? no "?" no no "15" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > _<SDO>.rowObject.Lokasjon
"Lokasjon" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   = _<SDO>.rowObject.fuPris
     _FldNameList[12]   > _<SDO>.rowObject.fuVarekost
"fuVarekost" ? ? "decimal" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > _<SDO>.rowObject.Hg
"Hg" "Hg" ? "integer" ? ? ? ? ? ? yes ? no no "5" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > _<SDO>.rowObject.ArtikkelNr
"ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > _<SDO>.rowObject.BongTekst
"BongTekst" ? ? "character" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > _<SDO>.rowObject.fLevNamn
"fLevNamn" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > _<SDO>.rowObject.SaSong
"SaSong" "Se" ? "integer" ? ? ? ? ? ? yes ? no no "4" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > _<SDO>.rowObject.fSesong
"fSesong" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > _<SDO>.rowObject.LevNr
"LevNr" "Le" ? "integer" ? ? ? ? ? ? yes ? no no "7" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > _<SDO>.rowObject.RegistrertDato
"RegistrertDato" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > _<SDO>.rowObject.EDato
"EDato" ? ? "date" ? ? ? ? ? ? yes ? no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   = _<SDO>.rowObject.Pakke
     _FldNameList[23]   = _<SDO>.rowObject.OPris
     _FldNameList[24]   = _<SDO>.rowObject.HkStyrt
     _FldNameList[25]   = _<SDO>.rowObject.LokPris
     _FldNameList[26]   = _<SDO>.rowObject.BildeIKasse
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
ON MOUSE-SELECT-DBLCLICK OF br_table IN FRAME F-Main
DO:
    PUBLISH "PostValgt".
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
  DEF VAR bFarge AS LOG NO-UNDO.

  CASE cFargeKoding:
    WHEN "butvarebeh" THEN DO WITH FRAME {&FRAME-NAME}: /* Vareh.butikk */
      IF cTilleggskrit NE "" AND ENTRY(1,cTilleggskrit) = "1" THEN  /* Søk på tvers av suppl.bøker */
        bFarge = DYNAMIC-FUNCTION("getFieldList","BestHode;BestNr,VarebehBestHode",
                                  "WHERE besthode.artikkelnr = " + STRING(rowObject.artikkelnr)
                                + (IF ENTRY(2,cTilleggskrit) = "4" THEN
                                   " AND besthode.beststat = " + ENTRY(2,cTilleggskrit) + " AND besthode.BekreftetDato = ?"
                                   ELSE IF ENTRY(2,cTilleggskrit) = "44" THEN
                                   " AND besthode.beststat   = " + ENTRY(2,cTilleggskrit) + " AND besthode.BekreftetDato NE ?"
                                   ELSE IF ENTRY(2,cTilleggskrit) NE "" THEN
                                   " AND besthode.beststat   = " + ENTRY(2,cTilleggskrit)
                                   ELSE "")
                                + ",FIRST VarebehBestHode WHERE VarebehBestHode.VarebehNr = BestHode.VarebehNr"
                                  ) NE "".
      ELSE IF cTilleggskrit NE "" AND ENTRY(1,cTilleggskrit) = "2" THEN  /* Søk på tvers av mottaks.bøker */
        bFarge = DYNAMIC-FUNCTION("getFieldList","VarebehLinje;ArtikkelNr,VarebehHode",
                                  "WHERE VarebehLinje.Artikkelnr = " + STRING(rowObject.artikkelnr)
                                + ",FIRST VarebehHode WHERE VarebehHode.VarebehType = " + ENTRY(1,cTilleggskrit)
                                  ) NE "".
      ELSE IF cTilleggskrit NE "" THEN                                   /* Søk innenfor en vareh.bok */
        bFarge = DYNAMIC-FUNCTION("getFieldList","VarebehLinje;VarebehNr",
                                  "WHERE VarebehLinje.Artikkelnr = " + STRING(rowObject.artikkelnr)
                                + "  AND VarebehLinje.VarebehNr = " + cTilleggskrit
                                  ) NE "".
      ELSE                                                              /* Søk på tvers av vareh.bøker */
        bFarge = DYNAMIC-FUNCTION("getFieldList","VarebehLinje;ArtikkelNr",
                                  "WHERE VarebehLinje.Artikkelnr = " + STRING(rowObject.artikkelnr)
                                  ) NE "".
    END.
    WHEN "varebeh" THEN DO WITH FRAME {&FRAME-NAME}: /* Vareh.messereg */
      IF cTilleggskrit NE "" THEN 
        bFarge = DYNAMIC-FUNCTION("getFieldList","VarebehLinjeTrans;ArtikkelNr",
                                  "WHERE varebehlinjetrans.artikkelnr = " + STRING(rowObject.artikkelnr)
                                + (IF ENTRY(1,cTilleggskrit) = "global" THEN  /* Søk på tvers av vareh.bøker */
                                   " AND VarebehlinjeTrans.VarebehNr  > -999999"
                                  + (IF ENTRY(1,ENTRY(2,cTilleggskrit),"|") NE "0" THEN
                                     " AND varebehlinjetrans.butikknr   = " + ENTRY(1,ENTRY(2,cTilleggskrit),"|")
                                     ELSE
                                       " AND varebehlinjetrans.butikknr > -99999")                                   
                                  + (IF NUM-ENTRIES(ENTRY(2,cTilleggskrit),"|") = 2 AND ENTRY(2,ENTRY(2,cTilleggskrit),"|") NE "1" THEN
                                     " AND varebehlinjetrans.godkjentbestilling = " + (IF ENTRY(2,ENTRY(2,cTilleggskrit),"|") = "2" THEN "YES" ELSE "NO")
                                     ELSE "")
                                   ELSE                                 /* Søk innenfor en vareh.bok */
                                    " AND VarebehlinjeTrans.VarebehNr = " + cTilleggskrit)
                                + "  AND VarebehlinjeTrans.SeqNr  > -999999"
                                 ) NE "". 
      ELSE 
        bFarge = DYNAMIC-FUNCTION("getFieldList","VarebehLinjeTrans;ArtikkelNr",
                                  "WHERE varebehlinjetrans.artikkelnr = " + STRING(rowObject.artikkelnr)
                                 ) NE "". 

    END.
    WHEN "varebok" THEN DO WITH FRAME {&FRAME-NAME}: /* Varebok */
      IF cTilleggskrit NE "" THEN                                      /* Søk innenfor en varebok */
        bFarge = DYNAMIC-FUNCTION("getFieldList","VarebokLinje;ArtikkelNr",
                                  "WHERE Vareboklinje.Artikkelnr = " + STRING(rowObject.artikkelnr)
                                + "  AND Vareboklinje.VarebokNr = " + cTilleggskrit
                                 ) NE "". 
      ELSE                                                             /* Søk på tvers av varebøker */
        bFarge = DYNAMIC-FUNCTION("getFieldList","VarebokLinje;ArtikkelNr",
                                  "WHERE Vareboklinje.Artikkelnr = " + STRING(rowObject.artikkelnr)
                                 ) NE "". 

    END.
  END CASE.
  IF bFarge THEN
    rowObject.Beskr:BGCOLOR IN BROWSE {&BROWSE-NAME} = 11.
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

PUBLISH "getArtSokFargeKoding" (OUTPUT cFargeKoding,OUTPUT cTilleggskrit).

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields bTableWin 
PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcColValues AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcColValues).

  /* Code placed here will execute AFTER standard behavior.    */
  PUBLISH "NyttBilde" (RowObject.fuBildeFilNavn).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetFokus bTableWin 
PROCEDURE SetFokus :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  APPLY "ENTRY" TO rowObject.fSesong IN BROWSE {&BROWSE-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

