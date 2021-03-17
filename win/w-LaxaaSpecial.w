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

DEF var lFilId      AS DEC    NO-UNDO.
DEF VAR h_Parent    AS HANDLE NO-UNDO.
DEF VAR iAntLinjer  AS INT    NO-UNDO.
            
DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.

DEF VAR piLinjeNr AS INT  NO-UNDO.
DEF VAR pcLinje   AS CHAR NO-UNDO.
DEF VAR piAntFeil AS INT  NO-UNDO. 
DEF VAR iKampanjeId AS INT NO-UNDO.
DEF VAR iProfilNr AS INT NO-UNDO.
DEF VAR lRab%     AS DEC NO-UNDO.
DEF VAR cLogg AS CHAR NO-UNDO.

DEFINE VARIABLE lDec  AS DECIMAL NO-UNDO.
DEFINE VARIABLE cStr  AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE bOk   AS LOG NO-UNDO.

DEFINE VARIABLE iFiltyp AS INTEGER     NO-UNDO.

DEF STREAM InnFil.

DEFINE BUFFER bKampanjeHode  FOR KampanjeHode.
DEFINE BUFFER bKampanjeLinje FOR KampanjeLinje.
DEFINE BUFFER bufArtPris FOR ArtPris.

DEFINE TEMP-TABLE ttKampanjeHode LIKE KampanjeHode
  INDEX KampanjeId KampanjeId.
DEFINE TEMP-TABLE ttKampanjeLinje LIKE KampanjeLinje
    FIELD Beskr AS CHAR 
    FIELD LevNr AS INT 
    FIELD LevKod AS CHAR 
    FIELD LevFargKod AS CHAR
    FIELD Rab% AS DEC FORMAT "->>>9.99"
    FIELD Klar AS LOG 
  INDEX KampanjeId KampanjeId Vg LopNr.

DEFINE TEMP-TABLE ttError
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .


DEFINE TEMP-TABLE tt_nya NO-UNDO
    FIELD Levartnr  AS CHAR
    FIELD Namn      AS CHAR
    FIELD Streckkod	AS CHAR
    FIELD Hgrupp	AS INTE
    FIELD HgBeskr   AS CHAR
    FIELD Vgrupp	AS INTE
    FIELD VgBeskr   AS CHAR
    FIELD RekprisKr AS DECI
    FIELD PantKr    AS DECI
    FIELD InkprisKr AS DECI
    FIELD Moms%     AS DECI
    FIELD Jmffaktor	AS DECI
    FIELD Jmfenhet	AS CHAR
    FIELD JamforEnhId AS INTE
    FIELD Enhet	    AS CHAR
    FIELD SalgsEnhId AS INTE
    FIELD Lev       AS INTE
    FIELD momskod    AS INTE
    FIELD pantartikel AS DECI.




{windows.i}
{incl/devmode.i}
{incl/custdevmode.i}
{AssignRutiner.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BR-Nya

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt_nya

/* Definitions for BROWSE BR-Nya                                        */
&Scoped-define FIELDS-IN-QUERY-BR-Nya tt_nya.Levartnr tt_nya.Namn tt_nya.Streckkod tt_nya.Hgrupp tt_nya.HgBeskr tt_nya.Vgrupp tt_nya.VgBeskr tt_nya.RekprisKr tt_nya.PantKr tt_nya.InkprisKr tt_nya.Moms% tt_nya.Jmffaktor tt_nya.Jmfenhet tt_nya.JamforEnhId tt_nya.Enhet tt_nya.SalgsEnhId tt_nya.Lev tt_nya.momskod tt_nya.pantartikel   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BR-Nya   
&Scoped-define SELF-NAME BR-Nya
&Scoped-define QUERY-STRING-BR-Nya FOR EACH tt_nya
&Scoped-define OPEN-QUERY-BR-Nya OPEN QUERY {&SELF-NAME} FOR EACH tt_nya.
&Scoped-define TABLES-IN-QUERY-BR-Nya tt_nya
&Scoped-define FIRST-TABLE-IN-QUERY-BR-Nya tt_nya


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BR-Nya}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-74 B-Getfil FI-Filnamn BUTTON-SokDato ~
FI-Vpityp FI-FilnamnUt FI-Aktiveras FI-Skapade FI-Samma FI-KampanjId ~
B-VisaError B-UppdateraNya FI-FilnamnNya BR-Nya FI-KampanjTxt ~
FI-NyavarorTxt FI-NyavarorError 
&Scoped-Define DISPLAYED-OBJECTS FI-Filnamn FI-Vpityp FI-FilnamnUt ~
FI-Aktiveras FI-Skapade FI-Samma FI-KampanjId FI-FilnamnNya ~
FI-FilnamnNyaFel FI-KampanjTxt FI-NyavarorTxt FI-NyavarorError 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Getfil 
     LABEL "Fil" 
     SIZE 10 BY 1.14.

DEFINE BUTTON B-Skapa 
     LABEL "Skapa nya" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Uppdatera 
     LABEL "Läs in" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-UppdateraNya 
     LABEL "Läs in" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-VisaError 
     LABEL "Visa" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Aktiveras AS DATE FORMAT "99/99/99":U 
     LABEL "Aktiveras" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Filnamn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FilnamnNya AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FilnamnNyaFel AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE FI-FilnamnUt AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KampanjId AS INTEGER FORMAT ">>>>>>9":U INITIAL 0 
     LABEL "Kampanjid" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KampanjTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Läser in och skapar normalpriskampanj" 
     LABEL "" 
      VIEW-AS TEXT 
     SIZE 86 BY 1.38
     FONT 8 NO-UNDO.

DEFINE VARIABLE FI-NyavarorError AS CHARACTER FORMAT "X(256)":U INITIAL "Fil med felaktigt innehåll" 
     LABEL "" 
      VIEW-AS TEXT 
     SIZE 29 BY 1.38
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-NyavarorTxt AS CHARACTER FORMAT "X(256)":U INITIAL "Nya varor som kan aktiveras" 
     LABEL "" 
      VIEW-AS TEXT 
     SIZE 66 BY 1.38
     FONT 8 NO-UNDO.

DEFINE VARIABLE FI-Samma AS CHARACTER FORMAT "X(256)":U 
     LABEL "Oförändrade" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Skapade AS CHARACTER FORMAT "X(256)":U 
     LABEL "Uppdaterade" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Vpityp AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 51 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-74
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 131 BY .24.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BR-Nya FOR 
      tt_nya SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BR-Nya
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BR-Nya C-Win _FREEFORM
  QUERY BR-Nya DISPLAY
      tt_nya.Levartnr FORMAT "x(15)"
 tt_nya.Namn     FORMAT "x(45)"
 tt_nya.Streckkod FORMAT "x(17)"
 tt_nya.Hgrupp   
 tt_nya.HgBeskr  FORMAT "x(15)"
 tt_nya.Vgrupp   FORMAT ">>>>>9"
 tt_nya.VgBeskr  FORMAT "x(15)"
 tt_nya.RekprisKr
 tt_nya.PantKr   
 tt_nya.InkprisKr
 tt_nya.Moms%    
 tt_nya.Jmffaktor
 tt_nya.Jmfenhet 
 tt_nya.JamforEnhId
 tt_nya.Enhet    
 tt_nya.SalgsEnhId
 tt_nya.Lev      
 tt_nya.momskod
 tt_nya.pantartikel
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 128 BY 13.81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Getfil AT ROW 4.1 COL 12
     FI-Filnamn AT ROW 4.14 COL 22 COLON-ALIGNED NO-LABEL
     BUTTON-SokDato AT ROW 8.38 COL 66
     FI-Vpityp AT ROW 5.57 COL 22 COLON-ALIGNED NO-LABEL
     FI-FilnamnUt AT ROW 6.95 COL 22 COLON-ALIGNED NO-LABEL
     B-Uppdatera AT ROW 8.24 COL 23
     FI-Aktiveras AT ROW 8.38 COL 49 COLON-ALIGNED
     FI-Skapade AT ROW 9.62 COL 49 COLON-ALIGNED
     FI-Samma AT ROW 10.86 COL 49 COLON-ALIGNED
     FI-KampanjId AT ROW 12.43 COL 49 COLON-ALIGNED
     B-VisaError AT ROW 16.71 COL 108
     B-UppdateraNya AT ROW 18.14 COL 7
     FI-FilnamnNya AT ROW 18.14 COL 22 COLON-ALIGNED NO-LABEL
     FI-FilnamnNyaFel AT ROW 18.14 COL 75 COLON-ALIGNED NO-LABEL
     B-Skapa AT ROW 19.33 COL 7
     BR-Nya AT ROW 20.76 COL 3
     FI-KampanjTxt AT ROW 1.71 COL 3 COLON-ALIGNED
     FI-NyavarorTxt AT ROW 15.81 COL 3 COLON-ALIGNED
     FI-NyavarorError AT ROW 16.48 COL 75 COLON-ALIGNED
     RECT-74 AT ROW 15.29 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 133 BY 34.14.


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
         TITLE              = "Laxå prisimport"
         HEIGHT             = 34.14
         WIDTH              = 133
         MAX-HEIGHT         = 34.14
         MAX-WIDTH          = 133
         VIRTUAL-HEIGHT     = 34.14
         VIRTUAL-WIDTH      = 133
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BR-Nya B-Skapa DEFAULT-FRAME */
/* SETTINGS FOR BUTTON B-Skapa IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON B-Uppdatera IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FI-Aktiveras:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Filnamn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-FilnamnNya:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FI-FilnamnNyaFel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FI-FilnamnNyaFel:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-FilnamnUt:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       FI-Vpityp:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BR-Nya
/* Query rebuild information for BROWSE BR-Nya
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt_nya.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BR-Nya */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Laxå prisimport */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Laxå prisimport */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Getfil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Getfil C-Win
ON CHOOSE OF B-Getfil IN FRAME DEFAULT-FRAME /* Fil */
DO:
    DEFINE VARIABLE cFilter AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE bOk AS LOGICAL     NO-UNDO.
    DEFINE VARIABLE cKatalog AS CHARACTER   NO-UNDO.
    EMPTY TEMP-TABLE ttKampanjeHode.
    EMPTY TEMP-TABLE ttKampanjeLinje.
    B-Uppdatera:SENSITIVE = FALSE.
    FI-Filnamn   = "".
    FI-FilnamnUt = "".
    FI-Samma     = "".
    FI-Skapade   = "".
    DISPLAY FI-Filnamn FI-Vpityp FI-FilnamnUt FI-Skapade FI-Samma WITH FRAME {&FRAME-NAME}.
    cKatalog = "c:\home\lindbak\ankommet".
    cFilter = "*.csv".
  SYSTEM-DIALOG GET-FILE FI-Filnamn 
              FILTERS "VPI filer: " + cFilter cFilter  
              INITIAL-DIR cKatalog
              RETURN-TO-START-DIR
              MUST-EXIST
              UPDATE bOk.
  IF FI-Filnamn <> "" THEN DO:
      RUN AnalyseraFil.
  END.
  DISPLAY FI-Filnamn WITH FRAME {&FRAME-NAME}.
  FI-FilnamnUt = REPLACE(FI-Filnamn,".csv","_NYA.csv").
  FI-FilnamnUt:SCREEN-VALUE = FI-FilnamnUt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Skapa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Skapa C-Win
ON CHOOSE OF B-Skapa IN FRAME DEFAULT-FRAME /* Skapa nya */
DO:
  RUN SkapaNya.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Uppdatera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Uppdatera C-Win
ON CHOOSE OF B-Uppdatera IN FRAME DEFAULT-FRAME /* Läs in */
DO:
  RUN LesInnFil.
  RUN PosterData.
  FI-FilnamnNya = FI-FilnamnUt.
  FI-FilnamnNya:SCREEN-VALUE = FI-FilnamnNya.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-UppdateraNya
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-UppdateraNya C-Win
ON CHOOSE OF B-UppdateraNya IN FRAME DEFAULT-FRAME /* Läs in */
DO:
  RUN LesInnNya.
  IF CAN-FIND(FIRST tt_nya) THEN DO:
      {&OPEN-QUERY-BR-Nya}
      APPLY "ENTRY" TO BR-Nya IN FRAME {&FRAME-NAME}.
      B-Skapa:SENSITIVE = TRUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-VisaError
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-VisaError C-Win
ON CHOOSE OF B-VisaError IN FRAME DEFAULT-FRAME /* Visa */
DO:
  IF SEARCH(FI-FilnamnNyaFel) = ? THEN DO:
      MESSAGE "Hittar inte filen"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  OS-COMMAND SILENT (VALUE("notepad " + FI-FilnamnNyaFel)).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato C-Win
ON CHOOSE OF BUTTON-SokDato IN FRAME DEFAULT-FRAME /* ... */
or F10 of FI-Aktiveras
DO:
  /* Start søkeprogram */
    DEFINE VARIABLE cTittel AS CHARACTER INIT "Aktiveras" NO-UNDO.
    DEFINE VARIABLE dDato   AS DATE       NO-UNDO.
    ASSIGN dDato = INPUT FI-Aktiveras.
    RUN kalender.w (INPUT-OUTPUT dDato,cTittel).
    IF RETURN-VALUE <> "<avbryt>" THEN DO:
        IF NOT dDato > TODAY THEN DO:
            MESSAGE "Fel datum. Skall vara > idag"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        END.
        ELSE
            ASSIGN FI-Aktiveras              = dDato
                   FI-Aktiveras:SCREEN-VALUE = STRING(dDato).
    END.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BR-Nya
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    FI-Aktiveras = TODAY + 3.
  RUN enable_UI.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnalyseraFil C-Win 
PROCEDURE AnalyseraFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cc AS CHARACTER   NO-UNDO.
INPUT FROM VALUE(FI-Filnamn).
IMPORT UNFORMATTED cc.
INPUT CLOSE.
DO WITH FRAME {&FRAME-NAME}:
    IF ENTRY(1,cc,";") = "Levartnr" THEN DO:
        FI-Vpityp:SCREEN-VALUE = "Prisändring vanliga varor".
        B-Uppdatera:SENSITIVE = TRUE.
        iFilTyp = 1.
    END.
    ELSE IF ENTRY(1,cc,";") = "Lev" THEN DO:
        FI-Vpityp:SCREEN-VALUE = "Prisändring cigaretter".
        B-Uppdatera:SENSITIVE = TRUE.
        iFilTyp = 2.
    END.
    ELSE DO:
        FI-Vpityp:SCREEN-VALUE = "Felaktigt filinnehåll".
        iFilTyp = 0.
    END.
END.
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
  DISPLAY FI-Filnamn FI-Vpityp FI-FilnamnUt FI-Aktiveras FI-Skapade FI-Samma 
          FI-KampanjId FI-FilnamnNya FI-FilnamnNyaFel FI-KampanjTxt 
          FI-NyavarorTxt FI-NyavarorError 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-74 B-Getfil FI-Filnamn BUTTON-SokDato FI-Vpityp FI-FilnamnUt 
         FI-Aktiveras FI-Skapade FI-Samma FI-KampanjId B-VisaError 
         B-UppdateraNya FI-FilnamnNya BR-Nya FI-KampanjTxt FI-NyavarorTxt 
         FI-NyavarorError 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetData C-Win 
PROCEDURE GetData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER cLinje AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER cStreckkod1 AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER cStreckkod2 AS CHARACTER   NO-UNDO.
   DEFINE OUTPUT PARAMETER dPris AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER dInPris AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER dMoms AS DECIMAL     NO-UNDO.
   DEFINE OUTPUT PARAMETER iAntal AS INTEGER     NO-UNDO.
   DEFINE VARIABLE lError AS LOGICAL     NO-UNDO.
   CASE iFilTyp:
       WHEN 1 THEN DO:
           cStreckkod1 = ENTRY(3,cLinje,";").
           IF NOT CAN-FIND(strekkode WHERE strekkode.kode = FILL("0",13 - LENGTH(cStreckkod1)) + cStreckkod1) THEN
               lError = TRUE.
           dPris = DECI(ENTRY(8,cLinje,";")) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN
               lError = TRUE.
           dInPris = DECI(ENTRY(10,cLinje,";")) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN
               lError = TRUE.
           dMoms = DECI(ENTRY(11,cLinje,";")) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN
               lError = TRUE.
           IF lError THEN
               cStreckkod1 = "".
       END.
       WHEN 2 THEN DO:
           dMoms = DECI(ENTRY(7,cLinje,";")) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN
               lError = TRUE.
           dPris = DECI(ENTRY(8,cLinje,";")) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN
               lError = TRUE.
           dInPris = DECI(ENTRY(9,cLinje,";")) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN
               lError = TRUE.
           cStreckkod1 = ENTRY(13,cLinje,";").
           IF NOT CAN-FIND(strekkode WHERE strekkode.kode = FILL("0",13 - LENGTH(cStreckkod1)) + cStreckkod1) THEN
               lError = TRUE.
           cStreckkod2 = ENTRY(14,cLinje,";").
           IF NOT CAN-FIND(strekkode WHERE strekkode.kode = FILL("0",13 - LENGTH(cStreckkod2)) + cStreckkod2) THEN
               cStreckkod2 = "".
           iAntal = INT(ENTRY(15,cLinje,";")) NO-ERROR.
           IF ERROR-STATUS:ERROR THEN
               lError = TRUE.
           IF lError THEN
               cStreckkod1.
       END.
   END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil C-Win 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE b2Ok AS LOG NO-UNDO.
DEFINE VARIABLE cUtfilnamn AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cStreckkod1 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cStreckkod2 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE dPris AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dInPris AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dMoms AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iAntal AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAntalSkapade AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSamma AS INTEGER     NO-UNDO.
  /* Tømmer feillogg. */

  ASSIGN
      piLinjeNr  = 0
      pcLinje    = ''
      piAntFeil  = 0
      iAntLinjer = 0
      b2Ok       = TRUE 
      .
      
  FIND LAST KampanjeHode NO-LOCK NO-ERROR.
  IF AVAILABLE KampanjeHode THEN
  DO:
      iKampanjeId = KampanjeHode.KampanjeId + 1.
      RELEASE KampanjeHode.
  END.
  ELSE 
      iKampanjeId = 1.
  FI-KampanjId:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(iKampanjeId).
  INPUT FROM VALUE(FI-Filnamn) NO-ECHO.
  OUTPUT TO VALUE(FI-FilnamnUt).
  IMPORT UNFORMATTED pcLinje.
  PUT UNFORMATTED pcLinje SKIP. /* rubrikraden */
    
  KAMPANJEHODE:
  DO:
        
        CREATE ttKampanjeHode.
        ASSIGN ttKampanjeHode.StartDato = FI-Aktiveras
               ttKampanjeHode.SluttDato = FI-Aktiveras
               ttKampanjeHode.ProfilNr  = 1
               ttKampanjeHode.Kamp%     = 0
               ttKampanjeHode.KampanjeId  = iKampanjeId
               ttKampanjeHode.Beskrivelse = "Prisimport typ: " + STRING(iFilTyp)
               ttKampanjeHode.NormalPris  = TRUE
               ttKampanjeHode.Aktivert    = FALSE
               ttKampanjeHode.Komplett    = FALSE
               ttKampanjeHode.Notat       = 'Importerad ' + STRING(TODAY) + ' ' + 
                                         STRING(TIME,"HH:MM:SS") + '.'
               iProfilNr = ttKampanjeHode.ProfilNr
               lRab%                      = ttKampanjeHode.Kamp%
               ttKampanjeHode.AktiveresTid = 60
               ttKampanjeHode.GyldigtilTid = 86399.
        
  END. /* KAMPANJEHODE */

  LESERLINJER:
  REPEAT:
    /* Leser linje fra filen */
    IMPORT UNFORMATTED pcLinje.

    ASSIGN iAntLinjer = iAntLinjer + 1.
      .
    /* Skipper tomme linjer */
    IF pcLinje = "" THEN
        NEXT LESERLINJER.
        
    RUN GetData (pcLinje,OUTPUT cStreckkod1,OUTPUT cStreckkod2,OUTPUT dPris,OUTPUT dInPris,OUTPUT dMoms,OUTPUT iAntal).
    IF cStreckkod1 = "" THEN DO:
        PUT UNFORMATTED pcLinje SKIP.
        NEXT LESERLINJER.
    END.
    PRIS:
    DO:
        FIND strekkode WHERE strekkode.kode = FILL("0",13 - LENGTH(cStreckkod1)) + cStreckkod1 NO-LOCK.
        FIND artbas OF strekkode NO-LOCK NO-ERROR.
        IF NOT AVAIL artbas THEN DO:
            PUT UNFORMATTED pcLinje SKIP.
            NEXT LESERLINJER.
        END.
        FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.
        IF NOT AVAIL artpris THEN DO:
            PUT UNFORMATTED pcLinje SKIP.
            NEXT LESERLINJER.
        END.
        IF artpris.varekost[1] = dInpris AND
           artpris.pris[1]     = dPris THEN DO:
            iSamma = iSamma + 1.
            NEXT LESERLINJER.
        END.
        CREATE ttKampanjeLinje.
        ASSIGN ttKampanjeLinje.LevNr      = 1    /* ?? */
               ttKampanjeLinje.ProfilNr   = ttKampanjeHode.ProfilNr
               ttKampanjeLinje.KampanjeId = iKampanjeId
               ttKampanjeLinje.LevNr      = artbas.levnr
               ttKampanjeLinje.LevKod     = artbas.levkod
               ttKampanjeLinje.LevFargKod = artbas.levfargkod
               ttKampanjeLinje.Rab%       = 0
               ttKampanjeLinje.Behandlet  = FALSE
               ttKampanjeLinje.Klar       = FALSE
               ttKampanjeLinje.ArtikkelNr = ArtBas.ArtikkelNr
               ttKampanjeLinje.Beskr      = ArtBas.Beskr
               ttKampanjeLinje.Vg         = ArtBas.Vg
               ttKampanjeLinje.LopNr      = ArtBas.LopNr
               ttKampanjeLinje.Klar       = TRUE
               ttKampanjeLinje.Pris[2]  = dPris
               ttKampanjeLinje.VareKost = dInPris.

    END. /* PRIS */
    IF cStreckkod2 = "" THEN
        NEXT LESERLINJER.
    PRIS2:
    DO:
        FIND strekkode WHERE strekkode.kode = FILL("0",13 - LENGTH(cStreckkod2)) + cStreckkod2 NO-LOCK.
        FIND artbas OF strekkode NO-LOCK NO-ERROR.
        IF NOT AVAIL artbas THEN DO:
            ENTRY(13,cLinje,";") = "".
            PUT UNFORMATTED pcLinje SKIP.
            NEXT LESERLINJER.
        END.
        FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.
        IF NOT AVAIL artpris THEN DO:
            ENTRY(13,cLinje,";") = "".
            PUT UNFORMATTED pcLinje SKIP.
            NEXT LESERLINJER.
        END.
        IF artpris.varekost[1] = dInpris AND
           artpris.pris[1]     = dPris THEN DO:
            iSamma = iSamma + 1.
            NEXT LESERLINJER.
        END.

        CREATE ttKampanjeLinje.
        ASSIGN ttKampanjeLinje.LevNr      = 1    /* ?? */
               ttKampanjeLinje.ProfilNr   = ttKampanjeHode.ProfilNr
               ttKampanjeLinje.KampanjeId = iKampanjeId
               ttKampanjeLinje.LevNr      = artbas.levnr
               ttKampanjeLinje.LevKod     = artbas.levkod
               ttKampanjeLinje.LevFargKod = artbas.levfargkod
               ttKampanjeLinje.Rab%       = 0
               ttKampanjeLinje.Behandlet  = FALSE
               ttKampanjeLinje.Klar       = FALSE
               ttKampanjeLinje.ArtikkelNr = ArtBas.ArtikkelNr
               ttKampanjeLinje.Beskr      = ArtBas.Beskr
               ttKampanjeLinje.Vg         = ArtBas.Vg
               ttKampanjeLinje.LopNr      = ArtBas.LopNr
               ttKampanjeLinje.Klar       = TRUE
               ttKampanjeLinje.Pris[2]  = dPris * iAntal
               ttKampanjeLinje.VareKost = dInPris * iAntal.
    END. /* PRIS2 */
  END. /* LESERLINJER */
  INPUT CLOSE.
  OUTPUT CLOSE.
  FI-Samma:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(iSamma).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnNya C-Win 
PROCEDURE LesInnNya :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VAR  cLevartnr  AS CHAR NO-UNDO.
    DEFINE VAR  cNamn      AS CHAR NO-UNDO.
    DEFINE VAR  cStreckkod AS CHAR NO-UNDO.
    DEFINE VAR  iHgrupp	   AS INTE NO-UNDO.
    DEFINE VAR  cHgBeskr   AS CHAR NO-UNDO.
    DEFINE VAR  iVgrupp	   AS INTE NO-UNDO.
    DEFINE VAR  cVgBeskr   AS CHAR NO-UNDO.
    DEFINE VAR  dRekprisKr AS DECI NO-UNDO.
    DEFINE VAR  dPantKr    AS DECI NO-UNDO.
    DEFINE VAR  dInkprisKr AS DECI NO-UNDO.
    DEFINE VAR  dMoms%     AS DECI NO-UNDO.
    DEFINE VAR  dJmffaktor AS DECI NO-UNDO.
    DEFINE VAR  cJmfenhet  AS CHAR NO-UNDO.
    DEFINE VAR  cEnhet	   AS CHAR NO-UNDO.
    DEFINE VAR  iLev       AS INTE NO-UNDO.
   
    DEFINE VARIABLE cc AS CHARACTER   NO-UNDO.


/*

DEFINE TEMP-TABLE tt_nya NO-UNDO
    FIELD Levartnr  AS CHAR
    FIELD Namn      AS CHAR
    FIELD Streckkod	AS CHAR
    FIELD Hgrupp	AS INTE
    FIELD HgBeskr   AS CHAR
    FIELD Vgrupp	AS INTE
    FIELD VgBeskr   AS CHAR
    FIELD RekprisKr AS DECI
    FIELD PantKr    AS DECI
    FIELD InkprisKr AS DECI
    FIELD Moms%     AS DECI
    FIELD Jmffaktor	AS DECI
    FIELD Jmfenhet	AS CHAR
    FIELD Enhet	    AS CHAR
    FIELD Lev       AS INTE.

*/
    DEFINE VARIABLE cKatalog AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cFilter AS CHARACTER   NO-UNDO.
  IF SEARCH(FI-FilnamnNya) = ? THEN DO:
/*       MESSAGE "Fil saknas"                   */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK. */
      cKatalog = "c:\home\lindbak\ankommet".
      cFilter = "*.csv".
    SYSTEM-DIALOG GET-FILE FI-FilnamnNya 
                FILTERS "VPI filer: " + cFilter cFilter  
                INITIAL-DIR cKatalog
                RETURN-TO-START-DIR
                MUST-EXIST
                UPDATE bOk.
    FI-FilnamnNya:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FI-FilnamnNya.
  END.

  IF SEARCH(FI-FilnamnNya) = ? THEN DO:
      MESSAGE "Fil saknas!!!" SKIP "Rutinen avbryts"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
  END.
  FI-FilnamnNyaFel = REPLACE(FI-FilnamnNya,".csv","_error.csv").
  FI-FilnamnNyaFel:SCREEN-VALUE IN FRAME {&FRAME-NAME} = FI-FilnamnNyaFel.

  INPUT FROM VALUE(FI-FilnamnNya) NO-ECHO.
  OUTPUT TO VALUE(FI-FilnamnNyaFel).
  IMPORT UNFORMATTED cc.
  PUT UNFORMATTED cc SKIP. /* rubrikraden */
  REPEAT:
      IMPORT UNFORMATTED cc.
      cc = TRIM(cc).
      IF cc = "" THEN
          NEXT.

      ASSIGN cLevartnr  = ENTRY(1,cc,";")
             cNamn      = ENTRY(2,cc,";")
             cStreckkod = ENTRY(3,cc,";")
             iHgrupp    = INT(ENTRY(4,cc,";"))
             cHgBeskr   = ENTRY(5,cc,";")
             iVgrupp    = INT(ENTRY(6,cc,";"))
             cVgBeskr   = ENTRY(7,cc,";")
             dRekprisKr = DECI(REPLACE(ENTRY(8,cc,";")," ",""))
             dPantKr    = DECI(REPLACE(ENTRY(9,cc,";")," ",""))
             dInkprisKr = DECI(REPLACE(ENTRY(10,cc,";")," ",""))
             dMoms%     = DECI(REPLACE(ENTRY(11,cc,";")," ",""))
             dJmffaktor = DECI(REPLACE(ENTRY(12,cc,";")," ",""))
             cJmfenhet  = ENTRY(13,cc,";")
             cEnhet     = ENTRY(14,cc,";")
             iLev       = INT(ENTRY(15,cc,";")) NO-ERROR.
      cStreckkod = IF LENGTH(cStreckkod) < 8 THEN cStreckkod ELSE FILL("0",13 - LENGTH(cStreckkod)) + cStreckkod.
      IF ERROR-STATUS:ERROR THEN DO:
          PUT UNFORMATTED "Formatfel i ett fälten" ";" cc  SKIP.
          NEXT.
      END.
      IF NOT CAN-FIND(Levbas WHERE Levbas.LevNr = iLev) THEN DO:
          PUT UNFORMATTED "Leverantör saknas - " iLev ";" cc SKIP.
          NEXT.
      END.
      IF CAN-FIND(artbas WHERE artbas.artikkelnr = DECI(cStreckkod)) THEN DO:
          PUT UNFORMATTED "Artikel finns" ";" cc SKIP.
          NEXT.
      END.
      IF CAN-FIND(strekkode WHERE strekkode.kode = cStreckkod) THEN DO:
          PUT UNFORMATTED "Streckkod finns" ";" cc SKIP.
          NEXT.
      END.
      IF CAN-FIND(artbas WHERE artbas.levnr = iLev AND artbas.levkod = cLevartnr) THEN DO:
          PUT UNFORMATTED "Artikel finns med levnr och levartnr " ";" cc SKIP.
          NEXT.
      END.
      FIND vargr WHERE vargr.vg = iVgrupp NO-LOCK NO-ERROR.
      IF NOT AVAIL vargr THEN DO:
          PUT UNFORMATTED "Varugrupp saknas - " iVgrupp ";" cc SKIP.
          NEXT.
      END.
      IF dRekprisKr <= 0 OR dInkprisKr <= 0 THEN DO:
          PUT UNFORMATTED "Prisfält negativt" ";" cc SKIP.
          NEXT.
      END.
      IF dRekprisKr < dInkprisKr * ((100 + dMoms%) / 100) THEN DO:
          PUT UNFORMATTED "Negativ kalkyl" ";" cc SKIP.
          NEXT.
      END.
      FIND FIRST JamforEnhet WHERE JamforEnhet.JamforEnhet = cJmfenhet NO-LOCK NO-ERROR.
      IF NOT AVAIL Jamforenhet THEN DO:
          PUT UNFORMATTED "Hittar inte Jamforenhet i DB - " cJmfenhet ";" cc SKIP.
          NEXT.
      END.
/*       IF NOT CAN-DO("St,Kg,L",cEnhet) THEN DO:     */
/*           PUT UNFORMATTED "Fel Enhet" ";" cc SKIP. */
/*           NEXT.                                    */
/*       END.                                         */
      FIND FIRST Salgsenhet WHERE Salgsenhet.SalgsEnhTekst = cEnhet NO-LOCK NO-ERROR.
      IF NOT AVAIL Salgsenhet THEN DO:
          PUT UNFORMATTED "Hittar inte Salgsenhet i DB - "  cEnhet ";" cc SKIP.
          NEXT.
      END.
      FIND FIRST Moms WHERE Moms.MomsProc = dMoms% NO-LOCK NO-ERROR.
      IF NOT AVAIL Moms THEN DO:
          PUT UNFORMATTED "Moms% saknas i momsregister" ";" cc SKIP.
          NEXT.
      END.
      IF dPantKr > 0 THEN DO: panttest:
          FOR EACH artbas WHERE artbas.pant = TRUE NO-LOCK:
              FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.
              IF AVAIL artpris AND artpris.pris[1] = dPantKr THEN DO:
                  RELEASE artpris.
                  LEAVE panttest.
              END.

          END.
      END.
      IF dPantKr > 0 AND NOT AVAIL artbas THEN DO:
          PUT UNFORMATTED "Pantartikel saknas: " STRING(dPantkr) "Kr" ";" cc SKIP.
          NEXT.
      END.
      CREATE tt_nya.
      ASSIGN tt_nya.Levartnr    = cLevartnr 
             tt_nya.Namn        = cNamn     
             tt_nya.Streckkod   = cStreckkod
             tt_nya.Hgrupp      = vargr.hg   
/*              tt_nya.HgBeskr   = cHgBeskr */
             tt_nya.Vgrupp      = iVgrupp   
/*              tt_nya.VgBeskr   = cVgBeskr */
             tt_nya.RekprisKr   = dRekprisKr
             tt_nya.PantKr      = dPantKr   
             tt_nya.InkprisKr   = dInkprisKr
             tt_nya.Moms%       = dMoms%    
             tt_nya.Jmffaktor   = dJmffaktor
             tt_nya.Jmfenhet    = cJmfenhet 
             tt_nya.JamforEnhId = JamforEnhet.JamforEnhId
             tt_nya.Enhet       = cEnhet    
             tt_nya.SalgsEnhId  = Salgsenhet.SalgsEnhId
             tt_nya.Lev         = iLev
             tt_nya.momskod     = moms.momskod
             tt_nya.pantartikel = IF tt_nya.PantKr > 0 AND AVAIL artbas THEN artbas.artikkelnr ELSE 0.
  END.
INPUT CLOSE.
OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PosterData C-Win 
PROCEDURE PosterData :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAntalSkapade AS INTEGER     NO-UNDO.
  FOR EACH ttKampanjeHode WHERE 
      ttKampanjeHode.KampanjeId = iKampanjeId TRANSACTION:
          
    FIND KampanjeHode EXCLUSIVE-LOCK WHERE 
      KampanjeHode.KampanjeId = ttKampanjeHode.KampanjeId NO-ERROR.
    IF NOT AVAILABLE KampanjeHode THEN 
    DO:
      CREATE KampanjeHode.
      BUFFER-COPY ttKampanjeHode TO KampanjeHode 
          ASSIGN
          KampanjeHode.Kamp% = ttKampanjeHode.Kamp% * -1
          NO-ERROR.
    END.

    RELEASE KampanjeHode.

    FOR EACH ttKampanjeLinje WHERE
        ttKampanjeLinje.KampanjeId = ttKampanjeHode.KampanjeId AND
        ttKampanjeLinje.Klar       = TRUE:
        CREATE KampanjeLinje.
        BUFFER-COPY ttKampanjeLinje TO KampanjeLinje NO-ERROR.
        IF ERROR-STATUS:ERROR THEN 
        DO:
            IF AVAILABLE KampanjeLinje THEN DELETE KampanjeLinje.
        END.
        ELSE
            iAntalSkapade = iAntalSkapade + 1.
    END.
    FI-Skapade:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(iAntalSkapade).
  END. /* TRANSACTION */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaNya C-Win 
PROCEDURE SkapaNya :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bArt FOR Artbas.
   FOR EACH tt_nya:

       CREATE artbas.
       ASSIGN Artbas.artikkelnr = DECI(tt_nya.Streckkod)
              artbas.levnr      = tt_nya.Lev
              artbas.vg         = tt_nya.vgrupp
              artbas.hg         = tt_nya.Hgrupp
              artbas.levkod     = tt_nya.Levartnr
              artbas.strtypeid  = 2
              artbas.sasong     = 1
              artbas.beskr      = tt_nya.namn
              artbas.bongtekst  = SUBSTR(artbas.beskr,1,20)
              ArtBas.LinkVareNr = tt_nya.pantartikel
              ArtBas.JamforEnhet = tt_nya.Jmfenhet
              ArtBas.SalgsEnhet  = tt_nya.Enhet
              Artbas.mengde      = tt_nya.Jmffaktor
           .
       CREATE strekkode.
       ASSIGN strekkode.artikkelnr = artbas.artikkelnr
              strekkode.kode       = tt_nya.streckkod
              strekkode.kodetype   = 1
              strekkode.strkode    = 1.
       CREATE artpris.
       ASSIGN artpris.artikkelnr       = artbas.artikkelnr
              artpris.profilnr         = 1
              ArtPris.Pris[1]          = tt_nya.RekprisKr
              ArtPris.MvaKr[1]         = ROUND(ArtPris.Pris[1] * tt_nya.Moms% / (100 + tt_nya.Moms%),2)
              artpris.InnkjopsPris[1]  = tt_nya.InkprisKr
              ArtPris.ValPris[1]       = artpris.InnkjopsPris[1]
              ArtPris.VareKost[1]      = artpris.InnkjopsPris[1]
              ArtPris.DBKr[1]          = ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
              ArtPris.DB%[1]           = ROUND(ArtPris.DBKr[1] / (ArtPris.Pris[1] - ArtPris.MvaKr[1]),2)
              ArtPris.Mva%[1]          = tt_nya.Moms%
              ArtPris.MomsKod[1]       = tt_nya.momskod.
       FIND LAST bArt WHERE bArt.vg = artbas.vg USE-INDEX vglopnr NO-LOCK NO-ERROR.
       ASSIGN artbas.lopnr = IF NOT AVAIL bArt THEN 1 ELSE bArt.lopnr + 1.

   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

