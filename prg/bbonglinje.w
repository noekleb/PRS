&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
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

DEF VAR lNettoLinjeSum LIKE BongLinje.LinjeSum NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataBrowser
&Scoped-define DB-AWARE no

&Scoped-define ADM-SUPPORTED-LINKS TableIO-Target,Data-Target,Update-Source

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dbonglinje.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME br_table

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES rowObject

/* Definitions for BROWSE br_table                                      */
&Scoped-define FIELDS-IN-QUERY-br_table LinjeNr fuTransTypeTekst TBId ~
Makulert Strekkode VareGr LopeNr Storrelse Antall LinjeSum MvaKr LinjeRab ~
SubtotalRab BongTekst VVarekost MButikkNr VareGruppeNavn MvaGr ~
MvaGruppeNavn Mva% ArtikkelNr fuTransKl TransTid FeilKode FeilKodeTekst ~
NotatKode NotatKodeTekst RefNr RefTekst BongPris BongNr ButikkNr Dato EAv ~
EDato ETid GruppeNr KasseNr OAv ODato OriginalData OTid TransDato TTId Type ~
HovedGr HovedGrBeskrivelse ReturButikk ReturKasserer ReturKassererNavn b_id ~
SeqNr TransNr AaaaMmDd GenerellRabatt KampEierId KampId KampTilbId ~
Kunderabatt LevNavn LevNr Medlemsrabatt OrgVareGr Personalrabatt ~
PrisPrSalgsenhet ProduktType SalgsType SkiftNr ForKonvertering KampanjeId 
&Scoped-define ENABLED-FIELDS-IN-QUERY-br_table 
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
      LinjeNr COLUMN-LABEL "LNr" FORMAT ">>>>9":U WIDTH 4
      fuTransTypeTekst COLUMN-LABEL "TypeTekst" FORMAT "x(30)":U
            WIDTH 22
      TBId FORMAT ">>9":U
      Makulert FORMAT "*/":U WIDTH 3
      Strekkode FORMAT "X(30)":U WIDTH 35
      VareGr FORMAT ">>>>>9":U WIDTH 5.4
      LopeNr FORMAT ">>>>>9":U
      Storrelse FORMAT "X(4)":U WIDTH 6.8
      Antall FORMAT "->>>,>>9.999":U
      LinjeSum COLUMN-LABEL "Brutto pris" FORMAT "->,>>>,>>>,>>9.99":U
      MvaKr FORMAT "->>,>>>,>>9.99":U WIDTH 7.8
      LinjeRab FORMAT "->,>>>,>>9.99":U WIDTH 10
      SubtotalRab FORMAT "->,>>>,>>9.99":U WIDTH 8.4
      BongTekst FORMAT "X(30)":U WIDTH 27.6
      VVarekost FORMAT "->,>>>,>>9.99":U
      MButikkNr COLUMN-LABEL "MBut" FORMAT ">>>>>9":U WIDTH 7
      VareGruppeNavn FORMAT "X(30)":U
      MvaGr FORMAT ">9":U
      MvaGruppeNavn FORMAT "X(30)":U
      Mva% FORMAT "->>,>>9.99":U
      ArtikkelNr FORMAT "X(20)":U
      fuTransKl FORMAT "x(8)":U
      TransTid FORMAT "->,>>>,>>9":U
      FeilKode FORMAT ">9":U
      FeilKodeTekst FORMAT "X(30)":U
      NotatKode COLUMN-LABEL "TK" FORMAT ">>9":U
      NotatKodeTekst COLUMN-LABEL "Tiltakskodetekst" FORMAT "X(30)":U
      RefNr FORMAT "->>>>>>>>9":U
      RefTekst FORMAT "X(40)":U
      BongPris FORMAT "->>,>>>,>>9.99":U
      BongNr FORMAT ">>>>>>>>>>>>9":U
      ButikkNr FORMAT ">>>>>9":U
      Dato FORMAT "99/99/99":U
      EAv FORMAT "X(15)":U
      EDato FORMAT "99/99/99":U
      ETid FORMAT "->,>>>,>>9":U
      GruppeNr FORMAT ">9":U
      KasseNr FORMAT ">>9":U
      OAv FORMAT "X(15)":U
      ODato FORMAT "99/99/99":U
      OriginalData FORMAT "X(200)":U
      OTid FORMAT "->,>>>,>>9":U
      TransDato FORMAT "99/99/99":U
      TTId FORMAT ">>>9":U
      Type FORMAT "9":U
      HovedGr FORMAT ">>>9":U
      HovedGrBeskrivelse FORMAT "X(30)":U
      ReturButikk FORMAT ">>>>>9":U
      ReturKasserer FORMAT ">>>>>>>>>>>>9":U
      ReturKassererNavn FORMAT "X(30)":U
      b_id FORMAT "->>>>>>>>>>>>>>>>>>>>9":U
      SeqNr FORMAT ">9":U
      TransNr FORMAT "->>,>>>,>>9":U
      AaaaMmDd FORMAT "X(8)":U
      GenerellRabatt FORMAT "->,>>>,>>9.99":U
      KampEierId FORMAT ">>>>>9":U
      KampId FORMAT ">>>>>>>9":U
      KampTilbId FORMAT ">>>>>>>9":U
      Kunderabatt FORMAT "->,>>>,>>9.99":U
      LevNavn FORMAT "X(30)":U
      LevNr FORMAT ">>>>>9":U
      Medlemsrabatt FORMAT "->,>>>,>>9.99":U
      OrgVareGr FORMAT ">>>>>9":U
      Personalrabatt FORMAT "->,>>>,>>9.99":U
      PrisPrSalgsenhet FORMAT "->,>>>,>>>,>>9.99":U
      ProduktType FORMAT "9":U
      SalgsType FORMAT "yes/no":U
      SkiftNr FORMAT ">>>>>9":U
      ForKonvertering FORMAT "X(40)":U
      KampanjeId FORMAT ">>>>>>>9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ASSIGN NO-AUTO-VALIDATE NO-ROW-MARKERS SEPARATORS SIZE 154 BY 6.67 FIT-LAST-COLUMN.


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
   Data Source: "dbonglinje.w"
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
         WIDTH              = 155.
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
       br_table:NUM-LOCKED-COLUMNS IN FRAME F-Main     = 4.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE br_table
/* Query rebuild information for BROWSE br_table
     _TblList          = "rowObject"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > _<SDO>.rowObject.LinjeNr
"LinjeNr" "LNr" ? "integer" ? ? ? ? ? ? no ? no no "4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > _<SDO>.rowObject.fuTransTypeTekst
"fuTransTypeTekst" "TypeTekst" ? "character" ? ? ? ? ? ? no "?" no no "22" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   = _<SDO>.rowObject.TBId
     _FldNameList[4]   > _<SDO>.rowObject.Makulert
"Makulert" ? ? "logical" ? ? ? ? ? ? no ? no no "3" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > _<SDO>.rowObject.Strekkode
"Strekkode" ? ? "character" ? ? ? ? ? ? no ? no no "35" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[6]   > _<SDO>.rowObject.VareGr
"VareGr" ? ? "integer" ? ? ? ? ? ? no ? no no "5.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[7]   = _<SDO>.rowObject.LopeNr
     _FldNameList[8]   > _<SDO>.rowObject.Storrelse
"Storrelse" ? ? "character" ? ? ? ? ? ? no ? no no "6.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[9]   = _<SDO>.rowObject.Antall
     _FldNameList[10]   > _<SDO>.rowObject.LinjeSum
"LinjeSum" "Brutto pris" ? "decimal" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[11]   > _<SDO>.rowObject.MvaKr
"MvaKr" ? ? "decimal" ? ? ? ? ? ? no ? no no "7.8" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[12]   > _<SDO>.rowObject.LinjeRab
"LinjeRab" ? ? "decimal" ? ? ? ? ? ? no ? no no "10" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[13]   > _<SDO>.rowObject.SubtotalRab
"SubtotalRab" ? ? "decimal" ? ? ? ? ? ? no ? no no "8.4" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[14]   > _<SDO>.rowObject.BongTekst
"BongTekst" ? ? "character" ? ? ? ? ? ? no ? no no "27.6" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[15]   = _<SDO>.rowObject.VVarekost
     _FldNameList[16]   > _<SDO>.rowObject.MButikkNr
"MButikkNr" "MBut" ? "integer" ? ? ? ? ? ? no ? no no "7" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[17]   = _<SDO>.rowObject.VareGruppeNavn
     _FldNameList[18]   = _<SDO>.rowObject.MvaGr
     _FldNameList[19]   = _<SDO>.rowObject.MvaGruppeNavn
     _FldNameList[20]   = _<SDO>.rowObject.Mva%
     _FldNameList[21]   = _<SDO>.rowObject.ArtikkelNr
     _FldNameList[22]   > _<SDO>.rowObject.fuTransKl
"fuTransKl" ? ? "character" ? ? ? ? ? ? no "?" no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[23]   = _<SDO>.rowObject.TransTid
     _FldNameList[24]   = _<SDO>.rowObject.FeilKode
     _FldNameList[25]   = _<SDO>.rowObject.FeilKodeTekst
     _FldNameList[26]   > _<SDO>.rowObject.NotatKode
"NotatKode" "TK" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[27]   > _<SDO>.rowObject.NotatKodeTekst
"NotatKodeTekst" "Tiltakskodetekst" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[28]   = _<SDO>.rowObject.RefNr
     _FldNameList[29]   = _<SDO>.rowObject.RefTekst
     _FldNameList[30]   = _<SDO>.rowObject.BongPris
     _FldNameList[31]   = _<SDO>.rowObject.BongNr
     _FldNameList[32]   = _<SDO>.rowObject.ButikkNr
     _FldNameList[33]   = _<SDO>.rowObject.Dato
     _FldNameList[34]   = _<SDO>.rowObject.EAv
     _FldNameList[35]   = _<SDO>.rowObject.EDato
     _FldNameList[36]   = _<SDO>.rowObject.ETid
     _FldNameList[37]   = _<SDO>.rowObject.GruppeNr
     _FldNameList[38]   = _<SDO>.rowObject.KasseNr
     _FldNameList[39]   = _<SDO>.rowObject.OAv
     _FldNameList[40]   = _<SDO>.rowObject.ODato
     _FldNameList[41]   = _<SDO>.rowObject.OriginalData
     _FldNameList[42]   = _<SDO>.rowObject.OTid
     _FldNameList[43]   = _<SDO>.rowObject.TransDato
     _FldNameList[44]   = _<SDO>.rowObject.TTId
     _FldNameList[45]   = _<SDO>.rowObject.Type
     _FldNameList[46]   = _<SDO>.rowObject.HovedGr
     _FldNameList[47]   = _<SDO>.rowObject.HovedGrBeskrivelse
     _FldNameList[48]   = _<SDO>.rowObject.ReturButikk
     _FldNameList[49]   = _<SDO>.rowObject.ReturKasserer
     _FldNameList[50]   = _<SDO>.rowObject.ReturKassererNavn
     _FldNameList[51]   = _<SDO>.rowObject.b_id
     _FldNameList[52]   = _<SDO>.rowObject.SeqNr
     _FldNameList[53]   = _<SDO>.rowObject.TransNr
     _FldNameList[54]   = _<SDO>.rowObject.AaaaMmDd
     _FldNameList[55]   = _<SDO>.rowObject.GenerellRabatt
     _FldNameList[56]   = _<SDO>.rowObject.KampEierId
     _FldNameList[57]   = _<SDO>.rowObject.KampId
     _FldNameList[58]   = _<SDO>.rowObject.KampTilbId
     _FldNameList[59]   = _<SDO>.rowObject.Kunderabatt
     _FldNameList[60]   = _<SDO>.rowObject.LevNavn
     _FldNameList[61]   = _<SDO>.rowObject.LevNr
     _FldNameList[62]   = _<SDO>.rowObject.Medlemsrabatt
     _FldNameList[63]   = _<SDO>.rowObject.OrgVareGr
     _FldNameList[64]   = _<SDO>.rowObject.Personalrabatt
     _FldNameList[65]   = _<SDO>.rowObject.PrisPrSalgsenhet
     _FldNameList[66]   = _<SDO>.rowObject.ProduktType
     _FldNameList[67]   = _<SDO>.rowObject.SalgsType
     _FldNameList[68]   = _<SDO>.rowObject.SkiftNr
     _FldNameList[69]   = _<SDO>.rowObject.ForKonvertering
     _FldNameList[70]   = _<SDO>.rowObject.KampanjeId
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

