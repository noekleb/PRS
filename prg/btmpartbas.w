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

DEF VAR bOK         AS LOG NO-UNDO INIT TRUE.
DEFINE VARIABLE cSkomodus AS CHARACTER  NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Target,Data-Target,Update-Source

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dtmpartbas.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table rowObject.fuLevNavn ~
rowObject.LevKod rowObject.Beskr rowObject.LevFargKod ~
rowObject.fuAktivvarekost rowObject.fuAktivPris rowObject.Vg ~
rowObject.LopNr rowObject.fVgBeskr rowObject.Hg rowObject.SaSong ~
rowObject.fiSasong rowObject.Farg rowObject.ModellFarge rowObject.fuPris ~
rowObject.fuVarekost rowObject.Aktivert rowObject.Gjennomfaktureres ~
rowObject.IKasse rowObject.Lokasjon rowObject.BongTekst rowObject.VgKat ~
rowObject.StrTypeID rowObject.Pakkenr rowObject.MatKod rowObject.ArtikkelNr ~
rowObject.RAvdNr rowObject.RegistrertDato rowObject.EDato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table rowObject.LevKod ~
rowObject.Beskr rowObject.LevFargKod rowObject.Vg rowObject.LopNr ~
rowObject.Hg rowObject.SaSong rowObject.Farg rowObject.ModellFarge ~
rowObject.Gjennomfaktureres rowObject.Lokasjon rowObject.BongTekst ~
rowObject.StrTypeID rowObject.Pakkenr rowObject.MatKod rowObject.ArtikkelNr ~
rowObject.RAvdNr 
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
      rowObject.fuLevNavn FORMAT "x(30)":U WIDTH 20
      rowObject.LevKod FORMAT "x(20)":U WIDTH 10.4
      rowObject.Beskr FORMAT "x(40)":U WIDTH 30
      rowObject.LevFargKod COLUMN-LABEL "Lev.farg" FORMAT "X(30)":U
            WIDTH 20
      rowObject.fuAktivvarekost COLUMN-LABEL "Aktiv varekost" FORMAT "->>>>>9.99":U
      rowObject.fuAktivPris COLUMN-LABEL "Aktiv pris" FORMAT "->>>>>9.99":U
      rowObject.Vg COLUMN-LABEL "Vg" FORMAT "zzzzz9":U WIDTH 5
      rowObject.LopNr COLUMN-LABEL "Løpnr" FORMAT "zzzzz9":U WIDTH 6
      rowObject.fVgBeskr FORMAT "x(20)":U
      rowObject.Hg COLUMN-LABEL "Hg" FORMAT ">>>9":U WIDTH 4
      rowObject.SaSong COLUMN-LABEL "Ses" FORMAT ">>>>>9":U WIDTH 4
      rowObject.fiSasong FORMAT "x(14)":U WIDTH 10
      rowObject.Farg FORMAT "zzzzzz9":U
      rowObject.ModellFarge FORMAT ">>>>>>>>>>>>9":U
      rowObject.fuPris COLUMN-LABEL "Pris" FORMAT "->>>>>9.99":U
      rowObject.fuVarekost FORMAT "->>>>>9.99":U
      rowObject.Aktivert FORMAT "*/":U
      rowObject.Gjennomfaktureres FORMAT "*/":U
      rowObject.IKasse FORMAT "*/":U
      rowObject.Lokasjon FORMAT "X(20)":U
      rowObject.BongTekst FORMAT "X(30)":U
      rowObject.VgKat FORMAT "z9":U
      rowObject.StrTypeID FORMAT "zzzzz9":U
      rowObject.Pakkenr FORMAT "ZZZZ":U
      rowObject.MatKod FORMAT "z9":U
      rowObject.ArtikkelNr FORMAT "zzzzzzzzzzzz9":U
      rowObject.RAvdNr COLUMN-LABEL "Områdenr." FORMAT ">>9":U
      rowObject.RegistrertDato COLUMN-LABEL "Registrertdato" FORMAT "99/99/9999":U
      rowObject.EDato COLUMN-LABEL "Endret dato" FORMAT "99/99/9999":U
  ENABLE
      rowObject.LevKod HELP "?"
      rowObject.Beskr HELP "?"
      rowObject.LevFargKod HELP "?"
      rowObject.Vg HELP "?"
      rowObject.LopNr HELP "?"
      rowObject.Hg HELP "?"
      rowObject.SaSong HELP "?"
      rowObject.Farg HELP "?"
      rowObject.ModellFarge HELP "?"
      rowObject.Gjennomfaktureres HELP "?"
      rowObject.Lokasjon HELP "?"
      rowObject.BongTekst HELP "?"
      rowObject.StrTypeID HELP "?"
      rowObject.Pakkenr HELP "?"
      rowObject.MatKod HELP "?"
      rowObject.ArtikkelNr HELP "?"
      rowObject.RAvdNr HELP "?"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE SEPARATORS MULTIPLE SIZE 104 BY 6.67 FIT-LAST-COLUMN.


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
   Data Source: "dtmpartbas.w"
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
         WIDTH              = 104.
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
       rowObject.LevKod:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Beskr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LevFargKod:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Vg:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.LopNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Hg:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.SaSong:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Farg:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.ModellFarge:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Gjennomfaktureres:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Lokasjon:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.BongTekst:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.StrTypeID:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.Pakkenr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.MatKod:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.ArtikkelNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE
       rowObject.RAvdNr:COLUMN-READ-ONLY IN BROWSE br_table = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "rowObject"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > _<SDO>.rowObject.fuLevNavn
"rowObject.fuLevNavn" ? ? "character" ? ? ? ? ? ? no "?" no no "20" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > _<SDO>.rowObject.LevKod
"rowObject.LevKod" ? ? "character" ? ? ? ? ? ? yes "?" no no "10.4" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > _<SDO>.rowObject.Beskr
"rowObject.Beskr" ? ? "character" ? ? ? ? ? ? yes "?" no no "30" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > _<SDO>.rowObject.LevFargKod
"rowObject.LevFargKod" "Lev.farg" ? "character" ? ? ? ? ? ? yes "?" no no "20" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > _<SDO>.rowObject.fuAktivvarekost
"rowObject.fuAktivvarekost" "Aktiv varekost" ? "decimal" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > _<SDO>.rowObject.fuAktivPris
"rowObject.fuAktivPris" "Aktiv pris" ? "decimal" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   > _<SDO>.rowObject.Vg
"rowObject.Vg" "Vg" ? "integer" ? ? ? ? ? ? yes "?" no no "5" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[8]   > _<SDO>.rowObject.LopNr
"rowObject.LopNr" "Løpnr" ? "integer" ? ? ? ? ? ? yes "?" no no "6" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   > _<SDO>.rowObject.fVgBeskr
"rowObject.fVgBeskr" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[10]   > _<SDO>.rowObject.Hg
"rowObject.Hg" "Hg" ? "integer" ? ? ? ? ? ? yes "?" no no "4" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > _<SDO>.rowObject.SaSong
"rowObject.SaSong" "Ses" ? "integer" ? ? ? ? ? ? yes "?" no no "4" yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > _<SDO>.rowObject.fiSasong
"rowObject.fiSasong" ? ? "character" ? ? ? ? ? ? no "?" no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > _<SDO>.rowObject.Farg
"rowObject.Farg" ? ? "integer" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > _<SDO>.rowObject.ModellFarge
"rowObject.ModellFarge" ? ? "decimal" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   > _<SDO>.rowObject.fuPris
"rowObject.fuPris" "Pris" ? "decimal" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[16]   > _<SDO>.rowObject.fuVarekost
"rowObject.fuVarekost" ? ? "decimal" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   > _<SDO>.rowObject.Aktivert
"rowObject.Aktivert" ? ? "logical" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[18]   > _<SDO>.rowObject.Gjennomfaktureres
"rowObject.Gjennomfaktureres" ? ? "logical" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[19]   > _<SDO>.rowObject.IKasse
"rowObject.IKasse" ? ? "logical" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[20]   > _<SDO>.rowObject.Lokasjon
"rowObject.Lokasjon" ? ? "character" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[21]   > _<SDO>.rowObject.BongTekst
"rowObject.BongTekst" ? ? "character" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[22]   > _<SDO>.rowObject.VgKat
"rowObject.VgKat" ? ? "integer" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   > _<SDO>.rowObject.StrTypeID
"rowObject.StrTypeID" ? ? "integer" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[24]   > _<SDO>.rowObject.Pakkenr
"rowObject.Pakkenr" ? ? "integer" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[25]   > _<SDO>.rowObject.MatKod
"rowObject.MatKod" ? ? "integer" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[26]   > _<SDO>.rowObject.ArtikkelNr
"rowObject.ArtikkelNr" ? ? "decimal" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > _<SDO>.rowObject.RAvdNr
"rowObject.RAvdNr" "Områdenr." ? "integer" ? ? ? ? ? ? yes "?" no no ? yes no yes "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   > _<SDO>.rowObject.RegistrertDato
"rowObject.RegistrertDato" "Registrertdato" ? "date" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[29]   > _<SDO>.rowObject.EDato
"rowObject.EDato" "Endret dato" ? "date" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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
ON CURSOR-DOWN OF br_table IN FRAME F-Main
DO:
    IF NOT BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ? THEN
        RUN BTN-CURSOR-UPDOWN ("DOWN").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON CURSOR-UP OF br_table IN FRAME F-Main
DO:
    IF NOT BROWSE {&BROWSE-NAME}:FOCUSED-ROW = ? THEN
        RUN BTN-CURSOR-UPDOWN ("UP").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL br_table bTableWin
ON DEFAULT-ACTION OF br_table IN FRAME F-Main
DO:
  RUN ApplyBtnArtikkelKort IN DYNAMIC-FUNCTION("getContainerSource").
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
  IF RowObject.fuTilbud = TRUE THEN
      RowObject.fuAktivPris:BGCOLOR  IN BROWSE br_table = 12.
  IF RowObject.OPris = TRUE THEN
    RowObject.fuLevNavn:BGCOLOR  IN BROWSE br_table = 16.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BTN-CURSOR-UPDOWN bTableWin 
PROCEDURE BTN-CURSOR-UPDOWN :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cUpDwn AS CHARACTER  NO-UNDO.
    BROWSE {&BROWSE-NAME}:SELECT-ROW(BROWSE {&BROWSE-NAME}:FOCUSED-ROW).
    IF cUpDwn = "UP" THEN
        BROWSE {&BROWSE-NAME}:SELECT-PREV-ROW().
    ELSE IF cUpDwn = "DOWN" THEN
            BROWSE {&BROWSE-NAME}:SELECT-NEXT-ROW().
    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.
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
  DEFINE VARIABLE cColList AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iCount   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lFlyttafelt AS LOGICAL  INIT TRUE NO-UNDO.
  {syspara.i 1 1 54 cSkomodus}
/*   ASSIGN cColList = ",,4,,7,8,15,9,,12,13,26". */
  ASSIGN cColList = "7,8,6,,,,15,9,,12,13,26".
                 /* "1,2,4,3,7,8,15,6,5,11,12,26". ursprungspos */
                    
  /* Code placed here will execute PRIOR to standard behavior. */
  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  IF cSkomodus = "1" THEN DO iCount = 1 TO NUM-ENTRIES(cColList):
      IF INT(ENTRY(iCount,cColList)) = 0 THEN
          NEXT.
      BROWSE {&BROWSE-NAME}:MOVE-COLUMN(INT(ENTRY(iCount,cColList)),iCount).
  END.
/* 
[1] fuLevNavn
[2] LevKod
[4] LevFargKod
[3] Beskr
[7] Vg
[8] LopNr
[15]fuPris
[6] fuAktivPris
[5] fuAktivvarekost
[11]SaSong
[12]fiSasong
[26]ArtikkelNr
 */
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
APPLY "LEAVE" TO br_table IN FRAME f-main.
APPLY "entry" TO br_table IN FRAME f-main.

BROWSE {&BROWSE-NAME}:SELECT-ROW(BROWSE {&BROWSE-NAME}:FOCUSED-ROW) NO-ERROR.
IF NOT ERROR-STATUS:ERROR THEN
    APPLY "VALUE-CHANGED" TO BROWSE {&BROWSE-NAME}.

/* br_table:HELP = DYNAMIC-FUNCTION("getStatusString" IN h_dtmpartbas).  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

