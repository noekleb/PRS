&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
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

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE iSortCol   AS INTEGER  NO-UNDO.
DEFINE VARIABLE iSortOrder AS INTEGER  NO-UNDO.

DEFINE VARIABLE iMouseDownRow AS INTEGER    NO-UNDO.
DEFINE VARIABLE cSumWhatSave  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cColLabelSave AS CHARACTER  NO-UNDO.
DEFINE VARIABLE dCtrlWidth    AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cXSolgt%      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTitle        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKundenavn    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPolygon      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cExtraInfo    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPreSelectCols AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrintKunCols AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cPrintNoHiddCols AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cVisSave      AS CHARACTER  NO-UNDO.
DEFINE VARIABLE h_Window      AS HANDLE     NO-UNDO.
DEFINE VARIABLE iCurrPost     AS INTE       NO-UNDO.
DEFINE VARIABLE iCurrRow      AS INTE       NO-UNDO.
DEFINE VARIABLE iWidthPix  AS INTEGER       NO-UNDO.
DEFINE VARIABLE iHeightPix AS INTEGER       NO-UNDO.
DEFINE VARIABLE cJamforenhLst AS CHAR       NO-UNDO.
DEFINE VARIABLE cUtfil        AS CHAR       NO-UNDO.
DEF VAR fCurrArtikkelNr AS DEC NO-UNDO.

DEF VAR hArtikkelkort AS HANDLE NO-UNDO.
DEF VAR hVPIDpidatasett AS HANDLE NO-UNDO.
DEF VAR obOk AS LOG NO-UNDO.

/* DEF VAR cExcel2RigalDir   AS CHAR  INIT "C:\home\lindbak\sendes" NO-UNDO. */
DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR ctmpTxt AS CHAR NO-UNDO.
DEF VAR lFinansPro     AS LOGICAL    NO-UNDO.
DEF VAR piLevNr        AS INT INIT 400 NO-UNDO.
DEF VAR cSalgsEnhListe AS CHAR NO-UNDO.
DEF VAR rStatus        AS CHAR.
DEFINE VARIABLE lNyFil AS LOGICAL     NO-UNDO.
DEF VAR cVPILevLst     AS CHAR        NO-UNDO.
DEF VAR piLoop         AS INT         NO-UNDO.

/* {methodexcel.i} */

DEF VAR chExcelApplication    AS COM-HANDLE.  
DEF VAR chWorkbooks           AS COM-HANDLE.
DEF VAR chWorksheets          AS COM-HANDLE.

{lesexcelvpifil.i &NEW = "NEW" &SHARED = "SHARED"}

DEFINE TEMP-TABLE ttImportFields NO-UNDO
    FIELD FieldNumber  AS INT  FORMAT "99"    LABEL "Felt Nr." 
    FIELD FieldLabel   AS CHAR FORMAT "X(30)" LABEL "Name"
    FIELD FieldName    AS CHAR FORMAT "X(30)" LABEL "Kolonne" 
    FIELD FieldSource  AS CHAR FORMAT "x(8)"  LABEL "Kilde"
    FIELD RowIdent1    AS CHAR .

DEFINE VARIABLE giVpiLevNumber AS INT NO-UNDO. 
DEFINE VARIABLE glCancelImport AS LOGICAL NO-UNDO. 
DEFINE VARIABLE gcErrorMessage AS CHAR    NO-UNDO.
DEFINE VARIABLE glSelectImportColumnsActive AS LOGICAL INIT TRUE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-TT_Error

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TT_Error TT_Prisfil TT_VareOrgInfo

/* Definitions for BROWSE BROWSE-TT_Error                               */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TT_Error tt_Error.Radnr tt_Error.cError   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TT_Error   
&Scoped-define SELF-NAME BROWSE-TT_Error
&Scoped-define QUERY-STRING-BROWSE-TT_Error FOR EACH TT_Error
&Scoped-define OPEN-QUERY-BROWSE-TT_Error OPEN QUERY {&SELF-NAME} FOR EACH TT_Error.
&Scoped-define TABLES-IN-QUERY-BROWSE-TT_Error TT_Error
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TT_Error TT_Error


/* Definitions for BROWSE BROWSE-TT_Prisfil                             */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TT_Prisfil TT_Prisfil.Radnr tt_Prisfil.lNy tt_prisfil.ctandem tt_Prisfil.cStatus tt_Prisfil.artbas_levnr tt_Prisfil.artbas_artnr tt_Prisfil.strekkode_kode tt_Prisfil.pris_bestnr tt_Prisfil.vare_varetekst /* tt_Prisfil.vare_enhet*/ tt_Prisfil.vare_mengde tt_Prisfil.vare_Cenhet tt_Prisfil.vare_hgr tt_Prisfil.pris_engrosn /* tt_Prisfil.rabatt */ tt_Prisfil.pris_utprisn tt_Prisfil.fil_tbproc tt_Prisfil.hgrprof_brutto% tt_Prisfil.pris_veilpris tt_Prisfil.vare_mva tt_Prisfil.vare_mvagr tt_Prisfil.vare_antpkn tt_Prisfil.vare_link tt_PrisFil.ProdNr tt_Prisfil.Vare_Produsent tt_PrisFil.VmId tt_PrisFil.Vare_Varemerke tt_PrisFil.Butik tt_PrisFil.ButNamn tt_Prisfil.Butikk_ButNr tt_Prisfil.cDummy   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TT_Prisfil   
&Scoped-define SELF-NAME BROWSE-TT_Prisfil
&Scoped-define OPEN-QUERY-BROWSE-TT_Prisfil IF RS-Avvik:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N" THEN     OPEN QUERY {&SELF-NAME} FOR EACH TT_Prisfil WHERE TT_Prisfil.lNy = TRUE. ELSE     OPEN QUERY {&SELF-NAME} FOR EACH TT_Prisfil WHERE TT_Prisfil.cStatus MATCHES RS-Avvik.
&Scoped-define TABLES-IN-QUERY-BROWSE-TT_Prisfil TT_Prisfil
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TT_Prisfil TT_Prisfil


/* Definitions for BROWSE BROWSE-TT_VareOrgInfo                         */
&Scoped-define FIELDS-IN-QUERY-BROWSE-TT_VareOrgInfo TT_VareOrgInfo.RadNr TT_VareOrgInfo.cFelt TT_VareOrgInfo.cVerdi   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-TT_VareOrgInfo   
&Scoped-define SELF-NAME BROWSE-TT_VareOrgInfo
&Scoped-define QUERY-STRING-BROWSE-TT_VareOrgInfo FOR EACH TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = TT_prisfil.Radnr
&Scoped-define OPEN-QUERY-BROWSE-TT_VareOrgInfo OPEN QUERY {&SELF-NAME} FOR EACH TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = TT_prisfil.Radnr.
&Scoped-define TABLES-IN-QUERY-BROWSE-TT_VareOrgInfo TT_VareOrgInfo
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-TT_VareOrgInfo TT_VareOrgInfo


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-TT_Error}~
    ~{&OPEN-QUERY-BROWSE-TT_Prisfil}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 RECT-71 B-Fil B-Refresh ~
BUTTON-5 BUTTON-2 BUTTON-4 BROWSE-TT_Error BROWSE-TT_VareOrgInfo RS-Avvik ~
BUTTON-3 FILL-IN-1 BROWSE-TT_Prisfil fldVPILev FI-AvvikbrowseTxt ~
FI-FelbrowseTxt FI-FilterTxt 
&Scoped-Define DISPLAYED-OBJECTS FI-Rigalnr FI-Filnavn FI-Filnamn CB-VPILev ~
RS-Avvik FILL-IN-1 fldVPILev FI-AvvikbrowseTxt FI-FelbrowseTxt FI-FilterTxt 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Fil 
     LABEL "Droppa fil här" 
     SIZE 15 BY 1.14 DROP-TARGET.

DEFINE BUTTON B-Refresh  NO-FOCUS
     LABEL "Frisk opp" 
     SIZE 15.2 BY 1.14.

DEFINE BUTTON BUTTON-2 
     LABEL "Exportera til RIGAL fil" 
     SIZE 22.8 BY 1.14.

DEFINE BUTTON BUTTON-3 
     LABEL "Artikelkort" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-4 
     LABEL "Oppdater VPI mottakskontrollen" 
     SIZE 40.6 BY 1.14.

DEFINE BUTTON BUTTON-5 
     LABEL "VPIMottakskontroll..." 
     SIZE 21.6 BY 1.14.

DEFINE VARIABLE CB-VPILev AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "VPI leverandør" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","0"
     DROP-DOWN-LIST
     SIZE 55 BY 1 TOOLTIP "VPI leverandør er bestemmende for hvilken mappingtabell som brukes." NO-UNDO.

DEFINE VARIABLE FI-AvvikbrowseTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Rader med ändringar" 
      VIEW-AS TEXT 
     SIZE 37 BY 1.05
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-FelbrowseTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Rader med identifierade fel" 
      VIEW-AS TEXT 
     SIZE 42.2 BY 1.05
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Filnamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Filnavn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 52.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FilterTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Ändrade/nya rader exporteras" 
      VIEW-AS TEXT 
     SIZE 42 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Rigalnr AS INTEGER FORMAT "999":U INITIAL 0 
     LABEL "Rigalnr" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U INITIAL "RIGALNUMMER" 
     LABEL "Fill 1" 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1
     FONT 6 NO-UNDO.

DEFINE VARIABLE fldVPILev AS CHARACTER FORMAT "X(256)":U 
     LABEL "VPI leverandør" 
      VIEW-AS TEXT 
     SIZE 37 BY .71 NO-UNDO.

DEFINE VARIABLE RS-Avvik AS CHARACTER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Alla", "*",
"Ändringar", "J",
"Nya", "N"
     SIZE 35 BY 1.19 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 202.4 BY .05.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 202.4 BY .05.

DEFINE RECTANGLE RECT-71
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 44 BY 2.14
     BGCOLOR 12 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-TT_Error FOR 
      TT_Error SCROLLING.

DEFINE QUERY BROWSE-TT_Prisfil FOR 
      TT_Prisfil SCROLLING.

DEFINE QUERY BROWSE-TT_VareOrgInfo FOR 
      TT_VareOrgInfo SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-TT_Error
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TT_Error C-Win _FREEFORM
  QUERY BROWSE-TT_Error DISPLAY
      tt_Error.Radnr
tt_Error.cError
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 103 BY 8.24 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-TT_Prisfil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TT_Prisfil C-Win _FREEFORM
  QUERY BROWSE-TT_Prisfil DISPLAY
      TT_Prisfil.Radnr
 tt_Prisfil.lNy
 tt_prisfil.ctandem
 tt_Prisfil.cStatus
 tt_Prisfil.artbas_levnr    
 tt_Prisfil.artbas_artnr    
 tt_Prisfil.strekkode_kode      
 tt_Prisfil.pris_bestnr   
 tt_Prisfil.vare_varetekst
/* tt_Prisfil.vare_enhet*/    
 tt_Prisfil.vare_mengde   
 tt_Prisfil.vare_Cenhet   
 tt_Prisfil.vare_hgr      
 tt_Prisfil.pris_engrosn  
/*  tt_Prisfil.rabatt */
 tt_Prisfil.pris_utprisn  
 tt_Prisfil.fil_tbproc
 tt_Prisfil.hgrprof_brutto%
 tt_Prisfil.pris_veilpris 
 tt_Prisfil.vare_mva      
 tt_Prisfil.vare_mvagr    
 tt_Prisfil.vare_antpkn
 tt_Prisfil.vare_link
 tt_PrisFil.ProdNr
 tt_Prisfil.Vare_Produsent
 tt_PrisFil.VmId
 tt_PrisFil.Vare_Varemerke
 tt_PrisFil.Butik
 tt_PrisFil.ButNamn
 tt_Prisfil.Butikk_ButNr
 tt_Prisfil.cDummy
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 201 BY 17.43 ROW-HEIGHT-CHARS .52 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-TT_VareOrgInfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-TT_VareOrgInfo C-Win _FREEFORM
  QUERY BROWSE-TT_VareOrgInfo DISPLAY
      TT_VareOrgInfo.RadNr 
    TT_VareOrgInfo.cFelt 
    TT_VareOrgInfo.cVerdi
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95 BY 8.24 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Fil AT ROW 1.43 COL 2
     B-Refresh AT ROW 1.48 COL 70.6 NO-TAB-STOP 
     BUTTON-5 AT ROW 1.48 COL 85.8
     BUTTON-2 AT ROW 1.48 COL 107.6
     FI-Rigalnr AT ROW 1.48 COL 194.4 COLON-ALIGNED
     FI-Filnavn AT ROW 1.52 COL 15.4 COLON-ALIGNED NO-LABEL
     FI-Filnamn AT ROW 1.52 COL 129 COLON-ALIGNED NO-LABEL
     CB-VPILev AT ROW 3.14 COL 16 COLON-ALIGNED
     BUTTON-4 AT ROW 3.43 COL 161
     BROWSE-TT_Error AT ROW 5.52 COL 3
     BROWSE-TT_VareOrgInfo AT ROW 5.52 COL 109
     RS-Avvik AT ROW 13.95 COL 3 NO-LABEL
     BUTTON-3 AT ROW 13.95 COL 165
     FILL-IN-1 AT ROW 14.05 COL 102 COLON-ALIGNED
     BROWSE-TT_Prisfil AT ROW 15.29 COL 3
     fldVPILev AT ROW 3.29 COL 16 COLON-ALIGNED NO-TAB-STOP 
     FI-AvvikbrowseTxt AT ROW 4.29 COL 107 COLON-ALIGNED NO-LABEL
     FI-FelbrowseTxt AT ROW 4.33 COL 1.8 COLON-ALIGNED NO-LABEL
     FI-FilterTxt AT ROW 14.29 COL 44 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.24 COL 1.6
     RECT-2 AT ROW 2.71 COL 1.6
     RECT-71 AT ROW 2.91 COL 159
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 203.8 BY 31.95.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Prisfilbehandling"
         HEIGHT             = 31.95
         WIDTH              = 203.8
         MAX-HEIGHT         = 38.57
         MAX-WIDTH          = 231
         VIRTUAL-HEIGHT     = 38.57
         VIRTUAL-WIDTH      = 231
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
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB C-Win 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/custdevmode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-TT_Error BUTTON-4 DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-TT_VareOrgInfo BROWSE-TT_Error DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-TT_Prisfil FILL-IN-1 DEFAULT-FRAME */
/* SETTINGS FOR COMBO-BOX CB-VPILev IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       CB-VPILev:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN FI-Filnamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Filnavn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Rigalnr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TT_Error
/* Query rebuild information for BROWSE BROWSE-TT_Error
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_Error.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-TT_Error */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TT_Prisfil
/* Query rebuild information for BROWSE BROWSE-TT_Prisfil
     _START_FREEFORM
IF RS-Avvik:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "N" THEN
    OPEN QUERY {&SELF-NAME} FOR EACH TT_Prisfil WHERE TT_Prisfil.lNy = TRUE.
ELSE
    OPEN QUERY {&SELF-NAME} FOR EACH TT_Prisfil WHERE TT_Prisfil.cStatus MATCHES RS-Avvik.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-TT_Prisfil */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-TT_VareOrgInfo
/* Query rebuild information for BROWSE BROWSE-TT_VareOrgInfo
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = TT_prisfil.Radnr.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-TT_VareOrgInfo */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Prisfilbehandling */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Prisfilbehandling */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-RESIZED OF C-Win /* Prisfilbehandling */
DO:
    IF {&WINDOW-NAME}:WIDTH-PIXELS < iWidthPix THEN
            {&WINDOW-NAME}:WIDTH-PIXELS = iWidthPix.
    DYNAMIC-FUNCTION("setWidgetResize",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE:CURRENT-WINDOW,"Resize","").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Fil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Fil C-Win
ON CHOOSE OF B-Fil IN FRAME DEFAULT-FRAME /* Droppa fil här */
DO:
    DEFINE VARIABLE cCSVNavn AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cKonvFil AS CHARACTER   NO-UNDO.
    
    IF search(FI-Filnavn) <> ? THEN DO:
        SESSION:SET-WAIT-STATE("GENERAL").
        
        cCSVNavn = SESSION:TEMP-DIR + "tmp.csv".
        cKonvFil = SESSION:TEMP-DIR + "tmp.txt".
        OS-DELETE VALUE(cCSVNavn) NO-ERROR.
        OS-DELETE VALUE(cKonvFil) NO-ERROR.
        iCurrPost = tt_prisfil.radnr NO-ERROR.
        iCurrRow  = BROWSE BROWSE-TT_Prisfil:FOCUSED-ROW NO-ERROR.
        
        /* Setter opp ExcelObjektet og dumper inholdet til en csv fil. */
        RUN LesEXCEL(FI-Filnavn,cCSVNavn). /* vid uppdatera så skall inte temptabeller tömmas */
        lNyFil = FALSE. 

        /* Leser inn fra CSV filen. */
        IF search(cCsvNavn) <> ? THEN
            RUN importera (cCSVNavn,cKonvFil).
        
        SESSION:SET-WAIT-STATE("").
        APPLY 'VALUE-CHANGED' TO BROWSE BROWSE-TT_Prisfil.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Fil C-Win
ON DROP-FILE-NOTIFY OF B-Fil IN FRAME DEFAULT-FRAME /* Droppa fil här */
DO:
    DEFINE VARIABLE cFilNavn AS CHARACTER   NO-UNDO.
    IF SELF:NUM-DROPPED-FILES = 1 THEN DO:
        ASSIGN cFilNavn = SELF:GET-DROPPED-FILE(1).
        IF NOT CAN-DO("xls,xlsx",ENTRY(NUM-ENTRIES(cFilNavn,"."),cFilNavn,".")) THEN
            MESSAGE "Tillåtna filtyper: '.xls,.xlsx'"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ELSE DO:
            lNyFil = TRUE. /* hantering av temptablar i LesExcel */
            FI-FIlnavn = cFilNavn.
            FI-FIlnavn:SCREEN-VALUE = FI-FIlnavn.
            APPLY "CHOOSE" TO B-Fil.
        END.
    END.
    ELSE DO:
        MESSAGE "Endast 1 fil tillåten!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Refresh
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Refresh C-Win
ON CHOOSE OF B-Refresh IN FRAME DEFAULT-FRAME /* Frisk opp */
DO:
    APPLY "CHOOSE" TO B-Fil.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TT_Error
&Scoped-define SELF-NAME BROWSE-TT_Error
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TT_Error C-Win
ON VALUE-CHANGED OF BROWSE-TT_Error IN FRAME DEFAULT-FRAME
DO:
    FIND TT_Prisfil WHERE TT_Prisfil.Radnr = tt_error.radnr.
    REPOSITION BROWSE-TT_Prisfil TO ROWID ROWID(tt_Prisfil).
    BROWSE BROWSE-TT_Prisfil:DESELECT-FOCUSED-ROW().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TT_Prisfil
&Scoped-define SELF-NAME BROWSE-TT_Prisfil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TT_Prisfil C-Win
ON DEFAULT-ACTION OF BROWSE-TT_Prisfil IN FRAME DEFAULT-FRAME
DO:
    IF CAN-FIND(FIRST tt_prisFil) THEN
    DO:
        IF NOT BROWSE-TT_Prisfil:IS-ROW-SELECTED(BROWSE-TT_Prisfil:FOCUSED-ROW) THEN
            BROWSE-TT_Prisfil:SELECT-ROW(BROWSE-TT_Prisfil:FOCUSED-ROW).

        IF NOT VALID-HANDLE(hArtikkelkort) THEN
            APPLY 'choose' TO BUTTON-3.
        ELSE 
            RUN ByttArtikkel.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TT_Prisfil C-Win
ON MOUSE-SELECT-CLICK OF BROWSE-TT_Prisfil IN FRAME DEFAULT-FRAME
DO:
  IF CAN-FIND(FIRST tt_prisFil) THEN
  DO:
      IF NOT BROWSE-TT_Prisfil:IS-ROW-SELECTED(BROWSE-TT_Prisfil:FOCUSED-ROW) THEN
          BROWSE-TT_Prisfil:SELECT-ROW(BROWSE-TT_Prisfil:FOCUSED-ROW).

      RUN ByttArtikkel.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TT_Prisfil C-Win
ON ROW-DISPLAY OF BROWSE-TT_Prisfil IN FRAME DEFAULT-FRAME
DO:
/*     TT_Prisfil.Radnr  */
/*     tt_Prisfil.cStatus */
    IF tt_Prisfil.cStatus = "" THEN DO:
        tt_Prisfil.artbas_levnr:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.artbas_levnr = ? THEN 12 ELSE ?.
        tt_Prisfil.artbas_artnr:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.artbas_artnr = ? THEN 12 ELSE ?.
        tt_Prisfil.strekkode_kode:BGCOLOR       IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.strekkode_kode    = ? THEN 12 ELSE ?.   
        tt_Prisfil.pris_bestnr:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.pris_bestnr  = ? THEN 12 ELSE ?.  
        tt_Prisfil.vare_varetekst:BGCOLOR IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.vare_varetekst = ? THEN 12 ELSE ?.
        tt_Prisfil.vare_mengde:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.vare_mengde  = ? THEN 12 ELSE ?.
        /*tt_Prisfil.vare_enhet:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.vare_enhet   = ? THEN 12 ELSE ?.*/ 
        tt_Prisfil.vare_Cenhet:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.vare_Cenhet  = ? THEN 12 ELSE ?. 
        tt_Prisfil.vare_hgr:BGCOLOR       IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.vare_hgr     = ? THEN 12 ELSE ?.
        tt_Prisfil.pris_engrosn:BGCOLOR   IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.pris_engrosn = ? OR 
                                                                        tt_Prisfil.pris_engrosn = 0 THEN 12 ELSE ?. 
/*         tt_Prisfil.rabatt:BGCOLOR         IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.rabatt       = ? THEN 12 ELSE ?. */
        tt_Prisfil.pris_utprisn:BGCOLOR   IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.pris_utprisn = ? THEN 12 ELSE ?. 
        tt_Prisfil.pris_veilpris:BGCOLOR  IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.pris_veilpris = ? THEN 12 ELSE ?.
        tt_Prisfil.vare_mva:BGCOLOR       IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.vare_mva       = ? THEN 12 ELSE ?.
        tt_Prisfil.vare_mvagr:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.vare_mvagr     = ? THEN 12 ELSE ?.
        tt_Prisfil.vare_antpkn:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.vare_antpkn   = ? THEN 12 ELSE ?. 
        tt_Prisfil.fil_tbproc:BGCOLOR    IN BROWSE {&BROWSE-NAME}  = IF tt_Prisfil.fil_tbproc - tt_Prisfil.hgrprof_brutto% >= 20 THEN 11 ELSE 
                                                                     IF tt_Prisfil.hgrprof_brutto% - tt_Prisfil.fil_tbproc >= 10 THEN 14 ELSE ?. 
    END.
    ELSE DO:
        tt_Prisfil.lNy:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "ny_vara") THEN 10 ELSE ?.
        tt_Prisfil.cTandem:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "ny_tandem") THEN 10 ELSE ?.
        tt_Prisfil.artbas_levnr:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "artbas_levnr") THEN 10 ELSE ?.
        tt_Prisfil.artbas_artnr:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "artbas_artnr") THEN 10 ELSE ?.
        tt_Prisfil.strekkode_kode:BGCOLOR       IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "strekkode_kode") THEN 10 ELSE ?.   
        tt_Prisfil.pris_bestnr:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "pris_bestnr") THEN 10 ELSE ?.  
        tt_Prisfil.vare_varetekst:BGCOLOR IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "vare_varetekst") THEN 10 ELSE ?.
        tt_Prisfil.vare_mengde:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "vare_mengde") THEN 10 ELSE ?.
        /*tt_Prisfil.vare_enhet:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "vare_enhet") THEN 10 ELSE ?.*/ 
        tt_Prisfil.vare_Cenhet:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "vare_enhet") THEN 10 ELSE ?. 
        tt_Prisfil.vare_hgr:BGCOLOR       IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "vare_hgr") THEN 10 ELSE ?.
        tt_Prisfil.pris_engrosn:BGCOLOR   IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "pris_engrosn") THEN 10 ELSE ?. 
/*         tt_Prisfil.rabatt:BGCOLOR         IN BROWSE {&BROWSE-NAME} = IF tt_Prisfil.rabatt       = ? THEN 12 ELSE ?. */
        tt_Prisfil.pris_utprisn:BGCOLOR   IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "pris_utprisn") THEN 10 ELSE ?. 
        tt_Prisfil.pris_veilpris:BGCOLOR  IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "pris_veilpris") THEN 10 ELSE ?.
        tt_Prisfil.vare_mva:BGCOLOR       IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "vare_mvagr") THEN 10 ELSE ?.
        tt_Prisfil.vare_mvagr:BGCOLOR     IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "vare_mvagr") THEN 10 ELSE ?.
        tt_Prisfil.vare_antpkn:BGCOLOR    IN BROWSE {&BROWSE-NAME} = IF CAN-FIND(TT_VareOrgInfo WHERE TT_VareOrgInfo.Rad = tt_prisfil.Radnr AND TT_VareOrgInfo.cTTfelt = "vare_antpkn") THEN 10 ELSE ?. 
        tt_Prisfil.fil_tbproc:BGCOLOR    IN BROWSE {&BROWSE-NAME}  = IF tt_Prisfil.fil_tbproc - tt_Prisfil.hgrprof_brutto% >= 20 THEN 11 ELSE
                                                                     IF tt_Prisfil.hgrprof_brutto% - tt_Prisfil.fil_tbproc >= 10 THEN 14 ELSE ?.

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-TT_Prisfil C-Win
ON VALUE-CHANGED OF BROWSE-TT_Prisfil IN FRAME DEFAULT-FRAME
DO:
  {&OPEN-QUERY-BROWSE-TT_VareOrgInfo}

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 C-Win
ON CHOOSE OF BUTTON-2 IN FRAME DEFAULT-FRAME /* Exportera til RIGAL fil */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        RUN lesexcelExportRigal.p (RS-Avvik:SCREEN-VALUE, OUTPUT cUtFil).
        CLOSE QUERY BROWSE-TT_Error. 
        CLOSE QUERY BROWSE-TT_Prisfil. 
        CLOSE QUERY BROWSE-TT_VareOrgInfo. 
        EMPTY TEMP-TABLE TT_Prisfil.
        EMPTY TEMP-TABLE TT_Error.
        EMPTY TEMP-TABLE TT_VareOrgInfo.
        FI-Filnamn = cUtFil.
        FI-Filnamn:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = cUtFil.
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 C-Win
ON CHOOSE OF BUTTON-3 IN FRAME DEFAULT-FRAME /* Artikelkort */
DO:
    IF AVAIL tt_PrisFil THEN
        FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = tt_PrisFil.ArtikkelNr NO-ERROR.
    ELSE
        RETURN.
  IF NOT AVAIL ArtBas THEN
      RETURN.
  IF NOT VALID-HANDLE(hArtikkelkort) THEN
    RUN w-vartkor.w  PERSIST SET hArtikkelkort (input recid(ArtBas), "ENDRE," + STRING(THIS-PROCEDURE)).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 C-Win
ON CHOOSE OF BUTTON-4 IN FRAME DEFAULT-FRAME /* Oppdater VPI mottakskontrollen */
DO:
    obOk = FALSE.
    MESSAGE 'Artiklene vil bli oppdatert inn i VPI motakskontrollen.' SKIP(1)
            '(Oppdateringen gjøres via PriCat formatet og VPIMottakskontrollen på VPI leverandør ' +
            STRING(giVpiLevNumber) + '.)'
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO UPDATE obOk.
    IF obOk = FALSE THEN
        RETURN NO-APPLY.
    DO WITH FRAME {&FRAME-NAME}:
        RUN lesexcelExportPricat.p (RS-Avvik:SCREEN-VALUE,STRING(giVpiLevNumber), OUTPUT cUtFil).
        CLOSE QUERY BROWSE-TT_Error. 
        CLOSE QUERY BROWSE-TT_Prisfil. 
        CLOSE QUERY BROWSE-TT_VareOrgInfo. 
        EMPTY TEMP-TABLE TT_Prisfil.
        EMPTY TEMP-TABLE TT_Error.
        EMPTY TEMP-TABLE TT_VareOrgInfo.
        FI-Filnamn = cUtFil.
        FI-Filnamn:SCREEN-VALUE  IN FRAME {&FRAME-NAME} = cUtFil.
    END.
    MESSAGE 'Oppdatering av artikkelregister ferdig.'
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-5
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-5 C-Win
ON CHOOSE OF BUTTON-5 IN FRAME DEFAULT-FRAME /* VPIMottakskontroll... */
DO:
  RUN vpidatasett.w PERSISTENT SET hVPIDpidatasett.

  IF VALID-HANDLE(hVPIDpidatasett) THEN
  DO:
      RUN InitializeObject IN hVPIDpidatasett.
      RUN setEkstVPILevNr IN hVPIDpidatasett (STRING(giVpiLevNumber)).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Avvik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Avvik C-Win
ON VALUE-CHANGED OF RS-Avvik IN FRAME DEFAULT-FRAME
DO:
    ASSIGN RS-Avvik.
  {&OPEN-QUERY-BROWSE-TT_Prisfil}
      IF BROWSE BROWSE-TT_Prisfil:FOCUSED-ROW <> ? THEN
          APPLY "VALUE-CHANGED" TO BROWSE-TT_Prisfil.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-TT_Error
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE) NO-ERROR.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
    IF VALID-HANDLE(hVPIDpidatasett) THEN
        DELETE OBJECT hVPIDpidatasett.
    IF VALID-HANDLE(hArtikkelkort) THEN APPLY "close" TO hArtikkelkort.
    RUN disable_UI.
/*     QUIT. */
END.

ASSIGN iWidthPix  = {&WINDOW-NAME}:WIDTH-PIXELS
       iHeightPix = {&WINDOW-NAME}:HEIGHT-PIXELS.

{syspara.i 50 17 1 cVPILevLst}
IF cVPILevLst = '' THEN
    cVPILevLst = '889'.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  {lng.i}
  RUN initTTMva.

  DO piLoop = 1 TO NUM-ENTRIES(cVPILevLst):
    FIND EkstVPILev NO-LOCK WHERE
         EkstVPILev.EkstVPILevNr = INT(ENTRY(piLoop,cVPILevLst)) NO-ERROR.
    IF AVAILABLE EkstVPILev THEN
        cTekst = cTekst + STRING(EkstVPILev.EkstVPILevNr) + ' ' + EkstVPILev.KortNavn + ','  + STRING(EkstVPILev.EkstVPILevNr) + ','.
  END.
  cTekst = TRIM(cTekst,',').

  RUN SetDivResize.
  CB-VPILev:VISIBLE = FALSE. 

  RUN enable_UI.

  /* Cho */
  IF NOT glSelectImportColumnsActive THEN
  DO:
      ASSIGN
      CB-VPILev:LIST-ITEM-PAIRS = cTekst
      CB-VPILev:SCREEN-VALUE    = ENTRY(2,cTekst)
      CB-VPILev:HIDDEN = FALSE 
      CB-VPILev:SENSITIVE = TRUE 
      fldVPILev:HIDDEN = TRUE 
      fldVPILev:VISIBLE = FALSE. 

  END.
  ELSE 
    ASSIGN
      CB-VPILev:VISIBLE = FALSE
      CB-VPILev:HIDDEN  = TRUE
      fldVPILev:HIDDEN = FALSE 
      fldVPILev:VISIBLE = TRUE
      fldVPILev:SENSITIVE = TRUE
      .

  /* cho -end */ 


  RUN adjustcol IN THIS-PROCEDURE.
  ASSIGN RS-Avvik.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adjustcol C-Win 
PROCEDURE adjustcol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE hCol AS HANDLE      NO-UNDO.

    ASSIGN 
        hCol = BROWSE BROWSE-TT_Prisfil:FIRST-COLUMN.
    
    REPEAT WHILE VALID-HANDLE(hCol):
        ASSIGN 
            hCol:AUTO-RESIZE = TRUE
            hCol:LABEL       = TRIM(hCol:LABEL)
            hCol             = hCol:NEXT-COLUMN.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByttArtikkel C-Win 
PROCEDURE ByttArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF AVAILABLE TT_PrisFil AND VALID-HANDLE(hArtikkelkort) THEN
      IF tt_PrisFil.ArtikkelNr > 0 THEN 
            RUN ByttArtikkel IN hArtikkelkort (tt_PrisFil.ArtikkelNr).

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
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
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
  DISPLAY FI-Rigalnr FI-Filnavn FI-Filnamn CB-VPILev RS-Avvik FILL-IN-1 
          fldVPILev FI-AvvikbrowseTxt FI-FelbrowseTxt FI-FilterTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-1 RECT-2 RECT-71 B-Fil B-Refresh BUTTON-5 BUTTON-2 BUTTON-4 
         BROWSE-TT_Error BROWSE-TT_VareOrgInfo RS-Avvik BUTTON-3 FILL-IN-1 
         BROWSE-TT_Prisfil fldVPILev FI-AvvikbrowseTxt FI-FelbrowseTxt 
         FI-FilterTxt 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Importera C-Win 
PROCEDURE Importera :
/*------------------------------------------------------------------------------
  Purpose:     
  Notes:       
------------------------------------------------------------------------------*/
   DEF INPUT  PARAMETER cFilnamn AS CHARACTER   NO-UNDO.
   DEF INPUT  PARAMETER cKonvnamn AS CHARACTER   NO-UNDO.

   PROCESS EVENTS.

   /* Cho */ 
   IF glSelectImportColumnsActive THEN
   DO:
       glCancelImport = FALSE. 
       RUN SelectImportColumns NO-ERROR.
    
       IF glCancelImport = TRUE THEN 
       DO:
           MESSAGE "Import Avbrutt" VIEW-AS ALERT-BOX. 
           RETURN. 
       END. 

       ExcludeImportFields[1] = ''.
       ExcludeImportFields[2] = ''.
                                  
       FOR EACH ttImportFields WHERE 
                ttImportFields.FieldSource = "db" : 
            
            ExcludeImportFields[1] = ttImportFields.FieldName + "," + ExcludeImportFields[1]. 
            ExcludeImportFields[2] = STRING(ttImportFields.FieldNumber) + "," + ExcludeImportFields[2]. 
       END.
              
       RUN lesexcelvpifilimportera.p (cFilnamn,giVpiLevNumber).
   END. 
   ELSE 
       RUN lesexcelvpifilimportera.p (cFilnamn,giVpiLevNumber).

   {&OPEN-BROWSERS-IN-QUERY-{&FRAME-NAME}}
   IF iCurrPost > 0 THEN DO:
       FIND tt_prisfil WHERE tt_prisfil.radnr = iCurrPost NO-ERROR.
       IF AVAIL tt_prisfil THEN DO:
           BROWSE BROWSE-TT_Prisfil:SET-REPOSITIONED-ROW(iCurrRow,"ALWAYS").
           REPOSITION BROWSE-TT_Prisfil TO ROWID ROWID(tt_prisfil) NO-ERROR.
           APPLY "VALUE-CHANGED" TO BROWSE BROWSE-TT_Prisfil.
       END.
   END.
   ELSE IF BROWSE BRowse-TT_Error:FOCUSED-ROW <> ? THEN DO:
       FIND TT_Prisfil WHERE TT_Prisfil.Radnr = tt_error.radnr NO-ERROR.
       IF AVAILABLE TT_PrisFil THEN
           REPOSITION BROWSE-TT_Prisfil TO ROWID ROWID(tt_Prisfil).
   END.
   BROWSE-TT_Prisfil:SELECT-FOCUSED-ROW() NO-ERROR.
   
   PROCESS EVENTS.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initTTMva C-Win 
PROCEDURE initTTMva :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    OPPRETT_MOMS:
    FOR EACH moms NO-LOCK.
        IF CAN-FIND(FIRST tt_Moms WHERE
                    tt_Moms.MomsKod = Moms.MomsKod) THEN 
                    LEAVE OPPRETT_MOMS.
        CREATE tt_moms.
        BUFFER-COPY moms TO tt_moms NO-ERROR.
        IF ERROR-STATUS:ERROR 
          THEN DELETE tt_Moms.
        ELSE RELEASE tt_moms.
    END. /* OPPRETT_MOMS */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesEXCEL C-Win 
PROCEDURE LesEXCEL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER cFileName  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cUtfilnamn AS CHARACTER   NO-UNDO.
  
  CLOSE QUERY BROWSE-TT_Error. 
  CLOSE QUERY BROWSE-TT_Prisfil. 
  CLOSE QUERY BROWSE-TT_VareOrgInfo.

  IF lNyfil = TRUE THEN DO:
      iCurrPost = 0.
      EMPTY TEMP-TABLE TT_Prisfil.
      EMPTY TEMP-TABLE TT_Error.
      EMPTY TEMP-TABLE TT_VareOrgInfo.
      RS-Avvik:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "*".
      FI-Filnamn:SCREEN-VALUE = "".
      ASSIGN RS-Avvik.
  END.
  PROCESS EVENTS.

  /* Leser Excel filen og lagrer inholdet til en csv fil. */
  RUN lesexcelvpifil.p (cFileName, cUtfilnamn).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nextRow C-Win 
PROCEDURE nextRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
    IF NOT BROWSE-TT_Prisfil:IS-ROW-SELECTED(BROWSE-TT_Prisfil:FOCUSED-ROW) THEN
      BROWSE-TT_Prisfil:SELECT-ROW(BROWSE-TT_Prisfil:FOCUSED-ROW).

    IF BROWSE-TT_Prisfil:SELECT-NEXT-ROW() THEN
      APPLY "value-changed" TO BROWSE-TT_Prisfil.

    RUN ByttArtikkel.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrevNext C-Win 
PROCEDURE PrevNext :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER cDirect AS CHAR NO-UNDO.

  CASE cDirect:
      WHEN 'Prev' THEN RUN prevRow.
      WHEN 'Next' THEN RUN nextRow.
  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE prevRow C-Win 
PROCEDURE prevRow :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME DEFAULT-FRAME:
    IF NOT BROWSE-TT_Prisfil:IS-ROW-SELECTED(BROWSE-TT_Prisfil:FOCUSED-ROW) THEN
      BROWSE-TT_Prisfil:SELECT-ROW(BROWSE-TT_Prisfil:FOCUSED-ROW).

    IF BROWSE-TT_Prisfil:SELECT-PREV-ROW() THEN
      APPLY "value-changed" TO BROWSE-TT_Prisfil.
    
    RUN ByttArtikkel.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelectImportColumns C-Win 
PROCEDURE SelectImportColumns :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttImportFields NO-ERROR. 

    RUN wVpiSelectImportColumns01.w NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        glCancelImport = TRUE.
        gcErrorMessage = RETURN-VALUE.
    END. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetColumnSelection C-Win 
PROCEDURE SetColumnSelection :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER TABLE FOR ttImportFields. 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetDivResize C-Win 
PROCEDURE SetDivResize :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME DEFAULT-FRAME:HANDLE,
                                "RECT-71,BROWSE-TT_Error").


DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,
                                FRAME DEFAULT-FRAME:HANDLE,
                                "RECT-71,BROWSE-TT_Error,BROWSE-TT_VareOrgInfo,Rect-1,Rect-2").


DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW, 
                               FRAME DEFAULT-FRAME:HANDLE,   
                        "FI-Filnamn,BUTTON-3,FI-Rigalnr,BUTTON-4,RECT-71").          

                                                                      /*  BROWSE-TT_VareOrgInfo*/

/* DYNAMIC-FUNCTION("setAddMoveX",THIS-PROCEDURE:CURRENT-WINDOW, */
/*                                 FRAME DEFAULT-FRAME:HANDLE,   */
/*                                  "Btn_Ok,Btn_Help").          */
/* DYNAMIC-FUNCTION("SetResizeADM2panel",TRUE). */
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,iWidthPix,iHeightPix,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetVPILeverandor C-Win 
PROCEDURE SetVPILeverandor :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcVPILeverandorNr AS CHAR NO-UNDO.
    
    giVpiLevNumber = INT(ipcVPILeverandorNr).

    FIND EkstVPILev NO-LOCK WHERE
         EkstVPILev.EkstVPILevNr = INT(ipcVPILeverandorNr) NO-ERROR.
  
    IF AVAILABLE EkstVPILev THEN
      fldVPILev:SCREEN-VALUE IN FRAME {&FRAME-NAME} = EkstVPILev.KortNavn.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaTT_VareOrgInfo C-Win 
PROCEDURE SkapaTT_VareOrgInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER iRad    AS INTEGER     NO-UNDO.
   DEFINE INPUT  PARAMETER cTTfelt AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER cFelt   AS CHARACTER   NO-UNDO.
   DEFINE INPUT  PARAMETER cVerdi AS CHARACTER   NO-UNDO.

   RUN lesexcelvpifil_SkapaTT_VareOrgInfo.p (iRad, cTTfelt, cFelt, cVerdi).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

