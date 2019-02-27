&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
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
DEFINE INPUT  PARAMETER dKundMedlemsNr AS DECIMAL     NO-UNDO.
DEFINE INPUT  PARAMETER cTyp       AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER cResultat  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFilnavn AS CHARACTER   NO-UNDO.
/* Local Variable Definitions ---                                       */
DEFINE VARIABLE dMedlemsnr AS DECIMAL     NO-UNDO.
DEFINE VARIABLE dKundeNr   AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iLeftMargin    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iPOsMedlemData AS INTEGER     NO-UNDO.
DEFINE VARIABLE cSprak AS CHARACTER   NO-UNDO.



DEFINE TEMP-TABLE tt_medlemsaktivitet NO-UNDO
    FIELD tabellnamn AS CHAR LABEL "Tabellnamn" FORMAT "x(30)"
    FIELD antal      AS INTE LABEL "Antal poster"
    FIELD tid        AS INTE LABEL "Tid".
DEFINE TEMP-TABLE tt_kundaktivitet NO-UNDO
    FIELD tabellnamn AS CHAR LABEL "Tabellnamn" FORMAT "x(30)"
    FIELD antal      AS INTE LABEL "Antal poster"
    FIELD tid        AS INTE LABEL "Tid".


{ pdf_inc.i "THIS-PROCEDURE"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-3

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt_medlemsaktivitet tt_kundaktivitet

/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 tt_medlemsaktivitet.tabellnamn tt_medlemsaktivitet.antal tt_medlemsaktivitet.tid   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3   
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH tt_medlemsaktivitet
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY {&SELF-NAME} FOR EACH tt_medlemsaktivitet.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 tt_medlemsaktivitet
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 tt_medlemsaktivitet


/* Definitions for BROWSE BROWSE-4                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-4 tt_kundaktivitet.tabellnamn tt_kundaktivitet.Antal tt_kundaktivitet.tid   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-4   
&Scoped-define SELF-NAME BROWSE-4
&Scoped-define QUERY-STRING-BROWSE-4 FOR EACH tt_kundaktivitet
&Scoped-define OPEN-QUERY-BROWSE-4 OPEN QUERY {&SELF-NAME} FOR EACH tt_kundaktivitet.
&Scoped-define TABLES-IN-QUERY-BROWSE-4 tt_kundaktivitet
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-4 tt_kundaktivitet


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-3}~
    ~{&OPEN-QUERY-BROWSE-4}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Kunde.Adresse1 Kunde.Privat Medlem.MedlemsNr ~
Medlem.KundeNr Kunde.Adresse2 Kunde.PrivatTlf Medlem.ForNavn ~
Medlem.Telefaks Kunde.DeresRef Kunde.SisteKjop Kunde.eMailFirma ~
Kunde.Telefon Medlem.EtterNavn Medlem.RegistrertDato Kunde.ePostAdresse ~
Kunde.TelefonFirma Medlem.Adresse1 Kunde.FaktAdresse1 Kunde.Telefaks ~
Medlem.Adresse2 Kunde.FaktAdresse2 Medlem.PostNr Kunde.FaktPostNr ~
Medlem.PersonNr Kunde.FodtDato Medlem.FodselsDato Medlem.Kjonn Kunde.Kjon ~
Medlem.Telefon Medlem.FodtAr Kunde.KontE-Post Medlem.Land ~
Medlem.ePostAdresse Kunde.KontMobilTlf Medlem.MedlemInfo Medlem.MobilTlf ~
Kunde.KontNavn Kunde.KontTelefaks Kunde.KontTelefon Kunde.KundeNr ~
Kunde.KundeSaldo Kunde.LevAdresse1 Kunde.LevAdresse2 Kunde.LevLand ~
Kunde.LevPostNr Kunde.MobilTlf Kunde.Navn Kunde.Opphort Kunde.OrgNr ~
Kunde.Postgiro Kunde.PostNr 
&Scoped-define ENABLED-TABLES Kunde Medlem
&Scoped-define FIRST-ENABLED-TABLE Kunde
&Scoped-define SECOND-ENABLED-TABLE Medlem
&Scoped-Define ENABLED-OBJECTS BUTTON-1 B-Anonymisera EDITOR-1 BROWSE-3 ~
BROWSE-4 
&Scoped-Define DISPLAYED-FIELDS Kunde.Adresse1 Kunde.Privat ~
Medlem.MedlemsNr Medlem.KundeNr Kunde.Adresse2 Kunde.PrivatTlf ~
Medlem.ForNavn Medlem.Telefaks Kunde.DeresRef Kunde.SisteKjop ~
Kunde.eMailFirma Kunde.Telefon Medlem.EtterNavn Medlem.RegistrertDato ~
Kunde.ePostAdresse Kunde.TelefonFirma Medlem.Adresse1 Kunde.FaktAdresse1 ~
Kunde.Telefaks Medlem.Adresse2 Kunde.FaktAdresse2 Medlem.PostNr ~
Kunde.FaktPostNr Medlem.PersonNr Kunde.FodtDato Medlem.FodselsDato ~
Medlem.Kjonn Kunde.Kjon Medlem.Telefon Medlem.FodtAr Kunde.KontE-Post ~
Medlem.Land Medlem.ePostAdresse Kunde.KontMobilTlf Medlem.MedlemInfo ~
Medlem.MobilTlf Kunde.KontNavn Kunde.KontTelefaks Kunde.KontTelefon ~
Kunde.KundeNr Kunde.KundeSaldo Kunde.LevAdresse1 Kunde.LevAdresse2 ~
Kunde.LevLand Kunde.LevPostNr Kunde.MobilTlf Kunde.Navn Kunde.Opphort ~
Kunde.OrgNr Kunde.Postgiro Kunde.PostNr 
&Scoped-define DISPLAYED-TABLES Kunde Medlem
&Scoped-define FIRST-DISPLAYED-TABLE Kunde
&Scoped-define SECOND-DISPLAYED-TABLE Medlem
&Scoped-Define DISPLAYED-OBJECTS EDITOR-1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD bredd C-Win 
FUNCTION bredd RETURNS DECIMAL
  ( INPUT cText AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getTTIdTxt C-Win 
FUNCTION getTTIdTxt RETURNS CHARACTER
  ( INPUT iTTId AS INTE )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD oversatt C-Win 
FUNCTION oversatt RETURNS CHARACTER
  ( INPUT cTxt AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD skrivmitt C-Win 
FUNCTION skrivmitt RETURNS CHARACTER
  ( INPUT cTxt AS CHARACTER, INPUT iYpos AS INTEGER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-Anonymisera 
     LABEL "Anonymisera" 
     SIZE 15 BY 1.14.

DEFINE BUTTON B-Tabort 
     LABEL "Ta bort" 
     SIZE 15 BY 1.14.

DEFINE BUTTON BUTTON-1 
     LABEL "Print" 
     SIZE 15 BY 1.14.

DEFINE VARIABLE EDITOR-1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 46.2 BY 4.48 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-3 FOR 
      tt_medlemsaktivitet SCROLLING.

DEFINE QUERY BROWSE-4 FOR 
      tt_kundaktivitet SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 C-Win _FREEFORM
  QUERY BROWSE-3 DISPLAY
      tt_medlemsaktivitet.tabellnamn
tt_medlemsaktivitet.antal
tt_medlemsaktivitet.tid
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 57 BY 13.57 FIT-LAST-COLUMN.

DEFINE BROWSE BROWSE-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-4 C-Win _FREEFORM
  QUERY BROWSE-4 DISPLAY
      tt_kundaktivitet.tabellnamn 
tt_kundaktivitet.Antal
tt_kundaktivitet.tid
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 57 BY 13.57 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     B-Tabort AT ROW 1.19 COL 81
     BUTTON-1 AT ROW 1.24 COL 45
     B-Anonymisera AT ROW 1.24 COL 64
     Kunde.Adresse1 AT ROW 1.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kunde.Privat AT ROW 1.48 COL 218 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     Medlem.MedlemsNr AT ROW 2.43 COL 20.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     Medlem.KundeNr AT ROW 2.43 COL 78.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     Kunde.Adresse2 AT ROW 2.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kunde.PrivatTlf AT ROW 2.48 COL 218 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Medlem.ForNavn AT ROW 3.48 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Medlem.Telefaks AT ROW 3.48 COL 78.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Kunde.DeresRef AT ROW 3.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Kunde.SisteKjop AT ROW 3.48 COL 218 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     Kunde.eMailFirma AT ROW 4.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kunde.Telefon AT ROW 4.48 COL 218 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Medlem.EtterNavn AT ROW 4.52 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Medlem.RegistrertDato AT ROW 4.67 COL 78.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Kunde.ePostAdresse AT ROW 5.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kunde.TelefonFirma AT ROW 5.48 COL 218 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Medlem.Adresse1 AT ROW 5.57 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     EDITOR-1 AT ROW 5.91 COL 80.6 NO-LABEL
     Kunde.FaktAdresse1 AT ROW 6.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Kunde.Telefaks AT ROW 6.48 COL 218 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Medlem.Adresse2 AT ROW 6.62 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kunde.FaktAdresse2 AT ROW 7.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Medlem.PostNr AT ROW 7.67 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     Kunde.FaktPostNr AT ROW 8.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Medlem.PersonNr AT ROW 8.71 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Kunde.FodtDato AT ROW 9.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     Medlem.FodselsDato AT ROW 9.76 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     Medlem.Kjonn AT ROW 9.76 COL 41.6 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 4.2 BY 1
     Kunde.Kjon AT ROW 10.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 3.4 BY 1
     Medlem.Telefon AT ROW 10.76 COL 78.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 251.6 BY 31.52.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     Medlem.FodtAr AT ROW 10.81 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     Kunde.KontE-Post AT ROW 11.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Medlem.Land AT ROW 11.81 COL 78.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Medlem.ePostAdresse AT ROW 11.86 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kunde.KontMobilTlf AT ROW 12.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Medlem.MedlemInfo AT ROW 12.86 COL 78.8 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 41.8 BY 1
     Medlem.MobilTlf AT ROW 12.91 COL 20 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Kunde.KontNavn AT ROW 13.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kunde.KontTelefaks AT ROW 14.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Kunde.KontTelefon AT ROW 15.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     BROWSE-3 AT ROW 15.76 COL 7
     BROWSE-4 AT ROW 15.76 COL 72
     Kunde.KundeNr AT ROW 16.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     Kunde.KundeSaldo AT ROW 17.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     Kunde.LevAdresse1 AT ROW 18.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Kunde.LevAdresse2 AT ROW 19.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Kunde.LevLand AT ROW 20.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Kunde.LevPostNr AT ROW 21.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     Kunde.MobilTlf AT ROW 22.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Kunde.Navn AT ROW 23.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     Kunde.Opphort AT ROW 24.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     Kunde.OrgNr AT ROW 25.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Kunde.Postgiro AT ROW 26.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     Kunde.PostNr AT ROW 27.48 COL 162 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 251.6 BY 31.52.


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
         TITLE              = "<insert window title>"
         HEIGHT             = 31.52
         WIDTH              = 251.6
         MAX-HEIGHT         = 31.52
         MAX-WIDTH          = 251.6
         VIRTUAL-HEIGHT     = 31.52
         VIRTUAL-WIDTH      = 251.6
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
/* BROWSE-TAB BROWSE-3 KontTelefon DEFAULT-FRAME */
/* BROWSE-TAB BROWSE-4 BROWSE-3 DEFAULT-FRAME */
/* SETTINGS FOR BUTTON B-Tabort IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       B-Tabort:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt_medlemsaktivitet.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-4
/* Query rebuild information for BROWSE BROWSE-4
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH tt_kundaktivitet.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-4 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* <insert window title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* <insert window title> */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Anonymisera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Anonymisera C-Win
ON CHOOSE OF B-Anonymisera IN FRAME DEFAULT-FRAME /* Anonymisera */
DO:
    DEFINE VARIABLE lGoOn AS LOGICAL     NO-UNDO.
    MESSAGE "Här gäller det att tänka sig om." SKIP
            "Har kunden/medlemmen begärt anonymisering?" SKIP
            "Har du kontroll på kundens reskontra?" SKIP
            "Fakturor kommer inte att kunna identifieras mot kund"
        VIEW-AS ALERT-BOX QUESTION UPDATE lGoOn.
    IF lGoOn THEN DO:
        IF dKundenr > 0 THEN
            RUN AnonymiseraKund.
        IF dMedlemsNr > 0 THEN
            RUN AnonymiseraMedlem.
        EMPTY TEMP-TABLE tt_medlemsaktivitet.
        EMPTY TEMP-TABLE tt_kundaktivitet.

        IF dMedlemsNr > 0 THEN DO:
            RUN getMedlemRelaterat.
        END.
        IF dKundenr > 0 THEN DO:
            RUN getKundRelaterat.
        END.
        RUN enable_UI.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-Tabort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-Tabort C-Win
ON CHOOSE OF B-Tabort IN FRAME DEFAULT-FRAME /* Ta bort */
DO:
    DEFINE VARIABLE lGoOn AS LOGICAL     NO-UNDO.
    MESSAGE "Här gäller det att tänka sig om." SKIP
            "Har kunden/medlemmen begärt borttag?" SKIP
            "Har du kontroll på kundens reskontra?" SKIP
            "Fakturor kommer inte att kunna identifieras mot kund"
        VIEW-AS ALERT-BOX QUESTION UPDATE lGoOn.
    IF lGoOn THEN DO:
        IF dKundenr > 0 THEN
            RUN TaBortKund.
        IF dMedlemsNr > 0 THEN
            RUN TaBortMedlem.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME DEFAULT-FRAME /* Print */
DO:
    RUN RapportPDF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
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

IF cTyp = "MEDLEM" THEN DO:
    FIND medlem WHERE medlem.medlemsnr = dKundMedlemsNr NO-LOCK NO-ERROR.
    IF AVAIL medlem THEN DO:
        dMedlemsnr = medlem.medlemsnr.
        FIND FIRST kunde WHERE kunde.kundenr = medlem.kundenr NO-LOCK NO-ERROR.
        IF AVAIL kunde THEN
            dKundenr = kunde.kundenr.
    END.
END.
ELSE IF cTyp = "KUNDE" THEN DO:
    FIND kunde WHERE kunde.kundenr = dKundMedlemsNr NO-LOCK NO-ERROR.
    IF AVAIL kunde THEN DO:
        dKundenr = kunde.kundenr.
        FIND medlem WHERE medlem.kundenr = dKundeNr NO-LOCK NO-ERROR.
        IF AVAIL medlem THEN
            dMedlemsnr = medlem.medlemsnr.
    END.

END.
ELSE
    RETURN.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
/* FIND medlem WHERE medlem.medlemsnr = dMedlemsNr NO-LOCK NO-ERROR. */
    FIND bruker WHERE bruker.brukerid = USERID("skotex") NO-LOCK NO-ERROR.
    IF AVAIL bruker THEN
        cSprak = Bruker.Lng.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    iLeftMargin = 30.
    iPOsMedlemData = 180.
    IF dMedlemsNr > 0 THEN DO:
        RUN getMedlemRelaterat.
    END.
    IF dKundenr > 0 THEN DO:
        RUN getKundRelaterat.
    END.
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnonymiseraKund C-Win 
PROCEDURE AnonymiseraKund :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH tt_kundaktivitet:
    CASE tt_kundaktivitet.tabellnamn:
        WHEN "Tilgode"        THEN DO:
            FOR EACH Tilgode         WHERE Tilgode.Kundenr = dKundenr:
                ASSIGN Tilgode.KAdresse1 = ""
                       Tilgode.KNavn     = ""
                       Tilgode.KTelefon  = ""
                       Tilgode.MAdresse1 = ""
                       Tilgode.MEtterNavn = ""
                       Tilgode.MForNavn   = ""
                       Tilgode.MTelefon   = "".
            END.
        END.
        WHEN "Translogg"      THEN DO:
/*  bara kundnrfält  FOR EACH TransLogg       WHERE Translogg.Kundnr = dKundenr NO-LOCK: END. */
        END.
        WHEN "ReklamasjonsLogg"      THEN DO:
            FOR EACH ReklamasjonsLogg       WHERE ReklamasjonsLogg.Kundenr = dKundenr:
                ASSIGN ReklamasjonsLogg.KundeAdresse = ""
                       ReklamasjonsLogg.KundeE-Mail  = ""
                       ReklamasjonsLogg.KundeMobil   = ""
                       ReklamasjonsLogg.KundeNavn    = ""
                       ReklamasjonsLogg.KundeTelefon = "".
            END.
        END.
        WHEN "KundeKort"      THEN DO:
            FOR EACH KundeKort       WHERE KundeKort.Kundenr = dKundenr:
                DELETE KundeKort.
            END.
        END.
        WHEN "KundeSaldo"      THEN DO:
/*                     FOR EACH KundeSaldo       WHERE KundeSaldo.Kundenr = dKundenr: */
/*                         DELETE KundeSaldo.                                         */
/*                     END.                                                           */
        END.
        WHEN "KundeTrans"      THEN DO:
/*                     FOR EACH KundeTrans       WHERE KundeTrans.Kundenr = dKundenr NO-LOCK: */
/*                     END.                                                                   */
        END.
        WHEN "Gavekort"      THEN DO:
            FOR EACH Gavekort       WHERE Gavekort.Kundenr = dKundenr:
                ASSIGN Gavekort.KNavn    = ""
                       Gavekort.KPostNr  = ""
                       Gavekort.KTelefon = "".
            END.
        END.
        WHEN "Kundereskontr"      THEN DO:
/*                     FOR EACH Kundereskontr       WHERE Kundereskontr.Kundenr = dKundenr NO-LOCK: */
/*                     END.                                                                         */
        END.
        WHEN "FakturaHode"      THEN DO:
            FOR EACH FakturaHode       WHERE FakturaHode.Kundenr = dKundenr:
                ASSIGN FakturaHode.Adresse1     = ""
                       FakturaHode.Adresse2     = ""
                       FakturaHode.DeresRef     = ""
                       FakturaHode.FaktAdresse1 = ""
                       FakturaHode.FaktAdresse2 = ""
                       FakturaHode.FaktPoststed = ""
                       FakturaHode.LevAdresse1  = ""
                       FakturaHode.LevAdresse2  = ""
                       FakturaHode.LevPostSted  = ""
                       FakturaHode.LevPostNr    = ""
                       FakturaHode.Navn         = ""
                       FakturaHode.PostSted     = ""
                       FakturaHode.Land         = ""
                       FakturaHode.KontNavn     = ""
                       FakturaHode.KontTelefon  = ""
                       FakturaHode.FaktLand     = ""
                       FakturaHode.Telefaks     = ""
                       FakturaHode.Telefon      = "".
            END.
        END.
        WHEN "KundeBetTrans"      THEN DO:
/*                     FOR EACH KundeBetTrans       WHERE KundeBetTrans.Kundenr = dKundenr NO-LOCK: */
/*                     END.                                                                         */
        END.
        WHEN "KOrdreHode"      THEN DO:
            FOR EACH KOrdreHode       WHERE KOrdreHode.Kundenr = dKundenr:
                ASSIGN KOrdreHode.Adresse1     = ""
                       KOrdreHode.Adresse2     = ""
                       KOrdreHode.ePostAdresse = ""
                       KOrdreHode.FaktAdresse1 = ""
                       KOrdreHode.FaktAdresse2 = ""
                       KOrdreHode.FaktLand     = ""
                       KOrdreHode.FaktPoststed = ""
                       KOrdreHode.KontNavn     = ""
                       KOrdreHode.KontTelefon  = ""
                       KOrdreHode.LevAdresse1  = ""
                       KOrdreHode.LevAdresse2  = ""
                       KOrdreHode.LevLand      = ""
                       KOrdreHode.LevPostNr    = ""
                       KOrdreHode.LevPostSted  = ""
                       KOrdreHode.MobilTlf     = ""
                       KOrdreHode.Navn         = ""
                       KOrdreHode.PostSted     = ""
                       KOrdreHode.FirmaAdresse1         = ""
                       KOrdreHode.FirmaAdresse2         = ""
                       KOrdreHode.FirmaEPost            = ""
                       KOrdreHode.FirmaBankKonto        = ""
                       KOrdreHode.FirmaLand             = ""
                       KOrdreHode.FirmaNavn             = ""
                       KOrdreHode.FirmaOrganisasjonsNr  = ""
                       KOrdreHode.FirmaPostgiro         = ""
                       KOrdreHode.FirmaPoststed         = ""
                       KOrdreHode.FirmaTelefaks         = ""
                       KOrdreHode.FirmaTelefon          = ""
                       KOrdreHode.Telefon          = ""
                       KOrdreHode.FirmaURLAdresse       = ""
                       KOrdreHode.DeresRef              = "".
            END.
        END.
        WHEN "Kundeprosjekt"      THEN DO:
/*                     FOR EACH Kundeprosjekt       WHERE Kundeprosjekt.Kundenr = dKundenr NO-LOCK: */
/*                     END.                                                                         */
        END.
        WHEN "KundeKommentar"      THEN DO:
/*                     FOR EACH KundeKommentar       WHERE KundeKommentar.Kundenr = dKundenr NO-LOCK: */
/*                     END.                                                                           */
        END.
        WHEN "PrisListeKunde"      THEN DO:
/*                     FOR EACH PrisListeKunde       WHERE PrisListeKunde.Kundenr = dKundenr NO-LOCK: */
/*                     END.                                                                           */
        END.
        WHEN "ekstbutiker"      THEN DO:
/*                     FOR EACH ekstbutiker       WHERE ekstbutiker.Kundenr = dKundenr NO-LOCK: */
/*                     END.                                                                     */
        END.
        WHEN "BongHode"      THEN DO:
            FOR EACH BongHode       WHERE BongHode.Kundenr = dKundenr:
                ASSIGN BongHode.KundeNavn = ""
                       BongHode.MedlemNavn = "".
            END.
        END.
    END CASE.
END.
FIND kunde WHERE kunde.kundenr = dKundenr NO-ERROR.
IF AVAIL kunde THEN DO:
    ASSIGN Kunde.Adresse1     = ""   
           Kunde.Adresse2     = ""   
           Kunde.Aktiv        = FALSE
           Kunde.BankKonto    = ""   
           Kunde.BankNavn     = ""   
           Kunde.DeresRef     = ""   
           Kunde.eMailFirma   = ""   
           Kunde.ePostAdresse = ""   
           Kunde.FaktAdresse1 = ""   
           Kunde.FaktAdresse2 = ""   
           Kunde.KontE-Post   = ""   
           Kunde.KontMobilTlf = ""   
           Kunde.KontNavn     = ""   
           Kunde.KontTelefaks = ""   
           Kunde.KontTelefon  = ""   
           Kunde.LevAdresse1  = ""   
           Kunde.LevAdresse2  = ""   
           Kunde.MobilTlf     = ""   
           Kunde.Navn         = ""   
           Kunde.Opphort      = TODAY
           Kunde.OrgNr        = ""   
           Kunde.Postgiro     = ""   
           Kunde.PrivatTlf    = ""   
           Kunde.Stilling     = ""   
           Kunde.Telefon      = ""   
           Kunde.Telefaks     = ""   
           Kunde.TelefonFirma = ""   
           Kunde.Tittel       = ""   
           Kunde.UrlFirma     = ""
           Kunde.TilgKilde    = "".
    FIND CURRENT Kunde NO-LOCK.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AnonymiseraMedlem C-Win 
PROCEDURE AnonymiseraMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-----------------------------------------------------------------------------*/
DEFINE VARIABLE dNymedlemsnr AS DECIMAL     NO-UNDO.
RUN getNymedlemsnr (OUTPUT dNymedlemsnr).
FOR EACH tt_medlemsaktivitet:
    CASE tt_medlemsaktivitet.tabellnamn:
        WHEN "Tilgode"        THEN DO:
            FOR EACH Tilgode         WHERE Tilgode.Medlemsnr = dMedlemsNr:
                ASSIGN Tilgode.medlemsnr  = IF dNymedlemsnr > 0 THEN dNymedlemsnr ELSE Tilgode.medlemsnr
                       Tilgode.MAdresse1  = ""
                       Tilgode.MEtterNavn = ""
                       Tilgode.MForNavn   = ""
                       Tilgode.MPostNr    = ""
                       Tilgode.MTelefon   = "".
            END.
        END.
        WHEN "Translogg" THEN DO:
            IF dNymedlemsnr > 0 THEN DO:
                FOR EACH TransLogg       WHERE TransLogg.Medlemsnr = dMedlemsNr:
                    TransLogg.Medlemsnr = dNymedlemsnr.
                END.
            END.
        END.
        WHEN "Kundekort"      THEN DO:
            FOR EACH KundeKort       WHERE KundeKort.Medlemsnr = dMedlemsNr:
                DELETE KundeKort.
            END.     
        END.
        WHEN "Medlemskort"    THEN DO:
            FOR EACH MedlemsKort     WHERE MedlemsKort.Medlemsnr = dMedlemsNr:
                DELETE Medlemskort.
            END.   
        END.
        WHEN "MedTrans"       THEN DO:
            IF dNymedlemsnr > 0 THEN DO:
                FOR EACH MedTrans        WHERE MedTrans.Medlemsnr = dMedlemsNr:
                    MedTrans.Medlemsnr = dNymedlemsnr.
                END.
            END.
        END.
        WHEN "MedlemSaldo"    THEN DO:
            IF dNymedlemsnr > 0 THEN DO:
                FOR EACH MedlemSaldo     WHERE MedlemSaldo.Medlemsnr = dMedlemsNr:
                    MedlemSaldo.Medlemsnr = dNyMedlemsNr.
                END.
            END.
        END.
        WHEN "Gavekort"       THEN DO:
            FOR EACH Gavekort        WHERE Gavekort.Medlemsnr = dMedlemsNr:
                ASSIGN Gavekort.Medlemsnr  = IF dNyMedlemsNr > 0 THEN dNyMedlemsNr ELSE Gavekort.Medlemsnr
                       Gavekort.MAdresse1  = ""
                       Gavekort.MEtterNavn = ""
                       Gavekort.MForNavn   = ""
                       Gavekort.MPostNr    = ""
                       Gavekort.MTelefon   = "".
            END.      
        END.
        WHEN "MedlemBetTrans" THEN DO:
            IF dNymedlemsnr > 0 THEN DO:
                FOR EACH MedlemBetTrans  WHERE MedlemBetTrans.Medlemsnr = dMedlemsNr:
                    MedlemBetTrans.Medlemsnr = dNyMedlemsNr.
                END.
            END.
        END.
        WHEN "KundeBetTrans"  THEN DO:
            IF dNymedlemsnr > 0 THEN DO:
                FOR EACH KundeBetTrans   WHERE KundeBetTrans.Medlemsnr = dMedlemsNr:
                    KundeBetTrans.Medlemsnr = dNyMedlemsNr.
                END.
            END.
        END.
        WHEN "MedKjop"        THEN DO:
            IF dNymedlemsnr > 0 THEN DO:
                FOR EACH MedKjop         WHERE MedKjop.Medlemsnr = dMedlemsNr:
                    MedKjop.Medlemsnr = dNyMedlemsNr.
                END.
            END.
        END.
        WHEN "MedRabReskontr" THEN DO:
            IF dNymedlemsnr > 0 THEN DO:
                FOR EACH MedRabReskontr  WHERE MedRabReskontr.Medlemsnr = dMedlemsNr:
                    MedRabReskontr.Medlemsnr = dNyMedlemsNr.
                END.
            END.
        END.
        WHEN "MedRabSjekk"    THEN DO:
            IF dNymedlemsnr > 0 THEN DO:
                FOR EACH MedRabSjekk     WHERE MedRabSjekk.Medlemsnr = dMedlemsNr:
                    MedRabSjekk.Medlemsnr = dNyMedlemsNr.
                END.
            END.
        END.
        WHEN "BongHode"       THEN DO:
            FOR EACH BongHode WHERE BongHode.Medlemsnr = dMedlemsNr:
                ASSIGN BongHode.Medlemsnr   = IF dNyMedlemsNr > 0 THEN dNyMedlemsNr ELSE BongHode.Medlemsnr
                       BongHode.MedlemNavn  = ""
                       BongHode.MedlemsKort = "".
            END.
        END.
    END CASE.
END.
FIND Medlem WHERE Medlem.Medlemsnr = dMedlemsNr NO-ERROR.
IF AVAIL Medlem THEN DO:
    ASSIGN Medlem.Medlemsnr = IF dNyMedlemsNr > 0 THEN dNyMedlemsNr ELSE Medlem.Medlemsnr
           Medlem.Adresse1     = ""   
           Medlem.Adresse2     = ""   
           Medlem.ePostAdresse = ""   
           Medlem.EtterNavn    = ""   
           Medlem.FodselsDato  = ?    
           Medlem.FodtAr       = 0    
           Medlem.ForNavn      = ""   
           Medlem.MedlemNotat  = ""   
           Medlem.PersonNr     = ""   
           Medlem.Opphort      = TODAY
           Medlem.Telefon      = ""   
           Medlem.WebBrukerId  = ""   
           Medlem.WebPassord   = ""   
           Medlem.Telefaks     = ""   
           Medlem.MobilTlf     = ""   
           Medlem.MedlemInfo   = "".
FIND CURRENT Medlem NO-LOCK.
END.

IF dNyMedlemsNr > 0 THEN
    dMedlemsNr = dNyMedlemsNr.
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
  DISPLAY EDITOR-1 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE Kunde THEN 
    DISPLAY Kunde.Adresse1 Kunde.Privat Kunde.Adresse2 Kunde.PrivatTlf 
          Kunde.DeresRef Kunde.SisteKjop Kunde.eMailFirma Kunde.Telefon 
          Kunde.ePostAdresse Kunde.TelefonFirma Kunde.FaktAdresse1 
          Kunde.Telefaks Kunde.FaktAdresse2 Kunde.FaktPostNr Kunde.FodtDato 
          Kunde.Kjon Kunde.KontE-Post Kunde.KontMobilTlf Kunde.KontNavn 
          Kunde.KontTelefaks Kunde.KontTelefon Kunde.KundeNr Kunde.KundeSaldo 
          Kunde.LevAdresse1 Kunde.LevAdresse2 Kunde.LevLand Kunde.LevPostNr 
          Kunde.MobilTlf Kunde.Navn Kunde.Opphort Kunde.OrgNr Kunde.Postgiro 
          Kunde.PostNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  IF AVAILABLE Medlem THEN 
    DISPLAY Medlem.MedlemsNr Medlem.KundeNr Medlem.ForNavn Medlem.Telefaks 
          Medlem.EtterNavn Medlem.RegistrertDato Medlem.Adresse1 Medlem.Adresse2 
          Medlem.PostNr Medlem.PersonNr Medlem.FodselsDato Medlem.Kjonn 
          Medlem.Telefon Medlem.FodtAr Medlem.Land Medlem.ePostAdresse 
          Medlem.MedlemInfo Medlem.MobilTlf 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE BUTTON-1 B-Anonymisera Kunde.Adresse1 Kunde.Privat Medlem.MedlemsNr 
         Medlem.KundeNr Kunde.Adresse2 Kunde.PrivatTlf Medlem.ForNavn 
         Medlem.Telefaks Kunde.DeresRef Kunde.SisteKjop Kunde.eMailFirma 
         Kunde.Telefon Medlem.EtterNavn Medlem.RegistrertDato 
         Kunde.ePostAdresse Kunde.TelefonFirma Medlem.Adresse1 EDITOR-1 
         Kunde.FaktAdresse1 Kunde.Telefaks Medlem.Adresse2 Kunde.FaktAdresse2 
         Medlem.PostNr Kunde.FaktPostNr Medlem.PersonNr Kunde.FodtDato 
         Medlem.FodselsDato Medlem.Kjonn Kunde.Kjon Medlem.Telefon 
         Medlem.FodtAr Kunde.KontE-Post Medlem.Land Medlem.ePostAdresse 
         Kunde.KontMobilTlf Medlem.MedlemInfo Medlem.MobilTlf Kunde.KontNavn 
         Kunde.KontTelefaks Kunde.KontTelefon BROWSE-3 BROWSE-4 Kunde.KundeNr 
         Kunde.KundeSaldo Kunde.LevAdresse1 Kunde.LevAdresse2 Kunde.LevLand 
         Kunde.LevPostNr Kunde.MobilTlf Kunde.Navn Kunde.Opphort Kunde.OrgNr 
         Kunde.Postgiro Kunde.PostNr 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getKundRelaterat C-Win 
PROCEDURE getKundRelaterat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iAntal AS INTEGER     NO-UNDO.
ETIME(TRUE). FOR EACH Tilgode          WHERE Tilgode.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="Tilgode" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
ETIME(TRUE). FOR EACH Translogg        WHERE Translogg.Kundnr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="Translogg" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
ETIME(TRUE). FOR EACH ReklamasjonsLogg WHERE ReklamasjonsLogg.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="ReklamasjonsLogg" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
/* FOR EACH Kunde            WHERE Kunde.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="Kunde" tt_kundaktivitet.Antal = iAntal. END. iAntal = 0. */
ETIME(TRUE). FOR EACH KundeKort        WHERE KundeKort.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="KundeKort" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
ETIME(TRUE). FOR EACH KundeSaldo       WHERE KundeSaldo.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="KundeSaldo" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
ETIME(TRUE). /* FOR EACH Medlem           WHERE Medlem.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="Medlem" tt_kundaktivitet.Antal = iAntal. END. iAntal = 0. */
ETIME(TRUE). FOR EACH KundeTrans       WHERE KundeTrans.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="KundeTrans" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
ETIME(TRUE). FOR EACH Gavekort         WHERE Gavekort.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="Gavekort" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
ETIME(TRUE). FOR EACH Kundereskontr    WHERE Kundereskontr.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="Kundereskontr" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
ETIME(TRUE). FOR EACH FakturaHode      WHERE FakturaHode.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="FakturaHode" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
ETIME(TRUE). FOR EACH KundeBetTrans    WHERE KundeBetTrans.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="KundeBetTrans" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
/* FOR EACH Individ          WHERE Individ.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="Individ" tt_kundaktivitet.Antal = iAntal. END. iAntal = 0. */
ETIME(TRUE). FOR EACH KOrdreHode       WHERE KOrdreHode.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="KOrdreHode" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
ETIME(TRUE). FOR EACH Kundeprosjekt    WHERE Kundeprosjekt.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="Kundeprosjekt" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
ETIME(TRUE). FOR EACH KundeKommentar   WHERE KundeKommentar.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="KundeKommentar" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
ETIME(TRUE). FOR EACH PrisListeKunde   WHERE PrisListeKunde.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="PrisListeKunde" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
ETIME(TRUE). FOR EACH ekstbutiker      WHERE ekstbutiker.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="ekstbutiker" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
ETIME(TRUE). FOR EACH BongHode         WHERE BongHode.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="BongHode" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
ETIME(TRUE). FOR EACH prKSData         WHERE prKSData.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="prKSData" tt_kundaktivitet.Antal = iAntal tt_kundaktivitet.tid = ETIME. END. iAntal = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getMedlemRelaterat C-Win 
PROCEDURE getMedlemRelaterat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iAntal AS INTEGER     NO-UNDO.
/* DEFINE TEMP-TABLE tt_medlemsaktivitet NO-UNDO      */
/*     FIELD tabellnamn AS CHAR LABEL "Tabellnamn"    */
/*     FIELD antal      AS INTE LABEL "Antal poster". */
iAntal = 0.
ETIME(TRUE).
FOR EACH Tilgode         WHERE Tilgode.Medlemsnr = dMedlemsNr NO-LOCK:
    iAntal = iAntal + 1.
END.
IF iAntal > 0 THEN DO:
    CREATE tt_medlemsaktivitet.
    ASSIGN tt_medlemsaktivitet.tabellnamn = "Tilgode"
           tt_medlemsaktivitet.antal      = iAntal
           tt_medlemsaktivitet.tid        = ETIME / 1000.
END.
iAntal = 0.
ETIME(TRUE).
FOR EACH TransLogg       WHERE TransLogg.Medlemsnr = dMedlemsNr NO-LOCK:
    iAntal = iAntal + 1.
END.
IF iAntal > 0 THEN DO:
    CREATE tt_medlemsaktivitet.
    ASSIGN tt_medlemsaktivitet.tabellnamn = "Translogg"
           tt_medlemsaktivitet.antal      = iAntal
           tt_medlemsaktivitet.tid        = ETIME / 1000.
END.
iAntal = 0.
ETIME(TRUE).
FOR EACH KundeKort       WHERE KundeKort.Medlemsnr = dMedlemsNr NO-LOCK:
    iAntal = iAntal + 1.
END.     
IF iAntal > 0 THEN DO:
    CREATE tt_medlemsaktivitet.
    ASSIGN tt_medlemsaktivitet.tabellnamn = "Kundekort"
           tt_medlemsaktivitet.antal      = iAntal
           tt_medlemsaktivitet.tid        = ETIME / 1000.
END.
/* iAntal = 0.                                                           */
/* FOR EACH Medlem          WHERE Medlem.Medlemsnr = dMedlemsNr NO-LOCK: */
/*     iAntal = iAntal + 1.                                              */
/* END.                                                                  */
/* IF iAntal > 0 THEN DO:                                                */
/*     CREATE tt_medlemsaktivitet.                                       */
/*     ASSIGN tt_medlemsaktivitet.tabellnamn = "Medlem"                  */
/*            tt_medlemsaktivitet.antal      = iAntal.                   */
/* END.                                                                  */
iAntal = 0.
ETIME(TRUE).
FOR EACH MedlemsKort     WHERE MedlemsKort.Medlemsnr = dMedlemsNr NO-LOCK:
    iAntal = iAntal + 1.
END.   
IF iAntal > 0 THEN DO:
    CREATE tt_medlemsaktivitet.
    ASSIGN tt_medlemsaktivitet.tabellnamn = "Medlemskort"
           tt_medlemsaktivitet.antal      = iAntal
           tt_medlemsaktivitet.tid        = ETIME / 1000.
END.
iAntal = 0.
ETIME(TRUE).
FOR EACH MedTrans        WHERE MedTrans.Medlemsnr = dMedlemsNr NO-LOCK:
    iAntal = iAntal + 1.
END.      
IF iAntal > 0 THEN DO:
    CREATE tt_medlemsaktivitet.
    ASSIGN tt_medlemsaktivitet.tabellnamn = "MedTrans"
           tt_medlemsaktivitet.antal      = iAntal
           tt_medlemsaktivitet.tid        = ETIME / 1000.
END.
iAntal = 0.
ETIME(TRUE).
FOR EACH MedlemSaldo     WHERE MedlemSaldo.Medlemsnr = dMedlemsNr NO-LOCK:
    iAntal = iAntal + 1.
END.   
IF iAntal > 0 THEN DO:
    CREATE tt_medlemsaktivitet.
    ASSIGN tt_medlemsaktivitet.tabellnamn = "MedlemSaldo"
           tt_medlemsaktivitet.antal      = iAntal
           tt_medlemsaktivitet.tid        = ETIME / 1000.
END.
iAntal = 0.
ETIME(TRUE).
FOR EACH Gavekort        WHERE Gavekort.Medlemsnr = dMedlemsNr NO-LOCK:
    iAntal = iAntal + 1.
END.      
IF iAntal > 0 THEN DO:
    CREATE tt_medlemsaktivitet.
    ASSIGN tt_medlemsaktivitet.tabellnamn = "Gavekort"
           tt_medlemsaktivitet.antal      = iAntal
           tt_medlemsaktivitet.tid        = ETIME / 1000.
END.
iAntal = 0.
ETIME(TRUE).
FOR EACH MedlemBetTrans  WHERE MedlemBetTrans.Medlemsnr = dMedlemsNr NO-LOCK:
    iAntal = iAntal + 1.
END.
IF iAntal > 0 THEN DO:
    CREATE tt_medlemsaktivitet.
    ASSIGN tt_medlemsaktivitet.tabellnamn = "MedlemBetTrans"
           tt_medlemsaktivitet.antal      = iAntal
           tt_medlemsaktivitet.tid        = ETIME / 1000.
END.
iAntal = 0.
ETIME(TRUE).
/* FOR EACH KundeBetTrans   WHERE KundeBetTrans.Medlemsnr = dMedlemsNr NO-LOCK: */
/*     iAntal = iAntal + 1.                                                     */
/* END.                                                                         */
IF iAntal > 0 THEN DO:
    CREATE tt_medlemsaktivitet.
    ASSIGN tt_medlemsaktivitet.tabellnamn = "KundeBetTrans"
           tt_medlemsaktivitet.antal      = iAntal
           tt_medlemsaktivitet.tid        = ETIME / 1000.
END.
iAntal = 0.
ETIME(TRUE).
FOR EACH MedKjop         WHERE MedKjop.Medlemsnr = dMedlemsNr NO-LOCK:
    iAntal = iAntal + 1.
END.
IF iAntal > 0 THEN DO:
    CREATE tt_medlemsaktivitet.
    ASSIGN tt_medlemsaktivitet.tabellnamn = "MedKjop"
           tt_medlemsaktivitet.antal      = iAntal
           tt_medlemsaktivitet.tid        = ETIME / 1000.
END.
iAntal = 0.
ETIME(TRUE).
FOR EACH MedRabReskontr  WHERE MedRabReskontr.Medlemsnr = dMedlemsNr NO-LOCK:
    iAntal = iAntal + 1.
END.
IF iAntal > 0 THEN DO:
    CREATE tt_medlemsaktivitet.
    ASSIGN tt_medlemsaktivitet.tabellnamn = "MedRabReskontr"
           tt_medlemsaktivitet.antal      = iAntal
           tt_medlemsaktivitet.tid        = ETIME / 1000.
END.
iAntal = 0.
ETIME(TRUE).
FOR EACH MedRabSjekk     WHERE MedRabSjekk.Medlemsnr = dMedlemsNr NO-LOCK:
    iAntal = iAntal + 1.
END.
IF iAntal > 0 THEN DO:
    CREATE tt_medlemsaktivitet.
    ASSIGN tt_medlemsaktivitet.tabellnamn = "MedRabSjekk"
           tt_medlemsaktivitet.antal      = iAntal
           tt_medlemsaktivitet.tid        = ETIME / 1000.
END.
ETIME(TRUE).
FOR EACH BongHode WHERE BongHode.Medlemsnr = dMedlemsNr NO-LOCK:
    iAntal = iAntal + 1.
END.
IF iAntal > 0 THEN DO:
    CREATE tt_medlemsaktivitet.
    ASSIGN tt_medlemsaktivitet.tabellnamn = "BongHode"
           tt_medlemsaktivitet.antal      = iAntal
           tt_medlemsaktivitet.tid        = ETIME / 1000.
END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getNymedlemsnr C-Win 
PROCEDURE getNymedlemsnr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER dNyMnr AS DECIMAL     NO-UNDO.
    DEFINE VARIABLE dNytt AS DECIMAL     NO-UNDO.

    dNytt = 999000000.

    FIND Medlem WHERE medlem.medlemsnr = dMedlemsnr NO-LOCK NO-ERROR.
    IF AVAIL Medlem THEN DO:
        IF LENGTH(Medlem.PersonNr) = 10 AND STRING(Medlem.medlemsnr) BEGINS Medlem.PersonNr  THEN DO:
            FIND LAST medlem WHERE medlem.medlemsnr < 999999999 NO-LOCK NO-ERROR.
            IF AVAIL Medlem AND Medlem.medlemsnr > dNytt THEN
                dNyMnr = medlem.medlemsnr + 1.
            ELSE
                dNyMnr = dNytt + 1.
        END.
        IF CAN-FIND(medlem WHERE medlem.medlemsnr = dNyMnr) THEN DO dNyMnr = dNyMnr + 1 TO 999999999:
            IF NOT CAN-FIND(medlem WHERE medlem.medlemsnr = dNyMnr) THEN
                LEAVE.
        END.
    END.
/*     MESSAGE "HEJ" dNyMnr                   */
/*         VIEW-AS ALERT-BOX INFO BUTTONS OK. */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE new_page C-Win 
PROCEDURE new_page :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN pdf_new_page ("Spdf").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDFdata C-Win 
PROCEDURE PDFdata :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
      DEFINE INPUT  PARAMETER cLabel    AS CHARACTER   NO-UNDO.
      DEFINE INPUT  PARAMETER cTxt      AS CHARACTER   NO-UNDO.
      DEFINE INPUT  PARAMETER iLblPos   AS INTEGER   NO-UNDO.
      DEFINE INPUT  PARAMETER iTxtPos   AS INTEGER   NO-UNDO.
      DEFINE INPUT  PARAMETER lTxtRight AS LOGICAL     NO-UNDO.
      DEFINE INPUT-OUTPUT  PARAMETER iY        AS INTEGER     NO-UNDO.
      RUN pdf_text_xy_dec ("Spdf",cLabel,ilBlPos,iY).
      RUN pdf_text_xy_dec ("Spdf",cTxt,iTxtPos,iY).

      iY = iY - 16.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDF_kunde C-Win 
PROCEDURE PDF_kunde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iY AS INTEGER     NO-UNDO.

RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
iY = 800.
skrivmitt("KUNDINFO",iY).

RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).

iY = 750.    

RUN PDFdata (oversatt("Navn"),STRING(Kunde.Navn),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Alder"),STRING(Kunde.Alder),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Adresse"),STRING(Kunde.Adresse1),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Adresse"),STRING(Kunde.Adresse2),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("PostNr"),STRING(Kunde.PostNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Leveringsadresse"),STRING(Kunde.LevAdresse1),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Leveringsadresse"),STRING(Kunde.LevAdresse2),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("PostNr"),STRING(Kunde.LevPostNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Land"),STRING(Kunde.LevLand),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
/* RUN PDFdata (oversatt("Aktiv"),STRING(Kunde.Aktiv),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).         */
/* RUN PDFdata (oversatt("Avdeling"),STRING(Kunde.Avdeling),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).   */
/* RUN PDFdata (oversatt("Bankkode"),STRING(Kunde.BankKode),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).   */
/* RUN PDFdata (oversatt("Bankkonto"),STRING(Kunde.BankKonto),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
/* RUN PDFdata (oversatt("Banknavn"),STRING(Kunde.BankNavn),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).   */
/* RUN PDFdata (oversatt("Betalings.bet"),STRING(Kunde.BetBet),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).  */
/* RUN PDFdata (oversatt("Betalingstype"),STRING(Kunde.BetType),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
/* RUN PDFdata (oversatt("Bruker"),STRING(Kunde.BrukerID),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
/* RUN PDFdata (oversatt("Butikk"),STRING(Kunde.ButikkNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).       */
/* RUN PDFdata (oversatt("Bydelsnummer"),STRING(Kunde.BydelsNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
RUN PDFdata (oversatt("Bynavn"),STRING(Kunde.ByNavn),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Deres referanse"),STRING(Kunde.DeresRef),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
/* RUN PDFdata (oversatt("Endret"),STRING(Kunde.EDato),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
RUN PDFdata (oversatt("Ekst.kundenr"),STRING(Kunde.EksterntKundeNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("EMail firma"),STRING(Kunde.eMailFirma),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("E-Post adresse"),STRING(Kunde.ePostAdresse),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
/* RUN PDFdata (oversatt("Etablert"),STRING(Kunde.Etablert),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
/* RUN PDFdata (oversatt("Endret tid"),STRING(Kunde.ETid),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).   */
RUN PDFdata (oversatt("Fakt.adresse"),STRING(Kunde.FaktAdresse1),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Fakt.Adresse"),STRING(Kunde.FaktAdresse2),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Land"),STRING(Kunde.FaktLand),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Postnr"),STRING(Kunde.FaktPostNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
/* RUN PDFdata (oversatt("FakturatekstNr"),STRING(Kunde.FaktTekstNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).   */
/* RUN PDFdata (oversatt("Detaljnivå"),STRING(Kunde.FakturaDeltajniva),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
RUN PDFdata (oversatt("Fakturagebyr"),STRING(Kunde.Fakturagebyr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Faktureringsperiode"),STRING(Kunde.Faktureringsperiode),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Født"),STRING(Kunde.FodtDato),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
/* RUN PDFdata (oversatt("Første kjøp"),STRING(Kunde.ForsteKjop),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
RUN PDFdata (oversatt("Kundegruppe"),STRING(Kunde.GruppeId),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Tiltale"),STRING(Kunde.Hilsen),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
/* RUN PDFdata (oversatt("Hovedkunde"),STRING(Kunde.Hovedkunde),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
/* RUN PDFdata (oversatt("Kilde"),STRING(Kunde.Kilde),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).           */
RUN PDFdata (oversatt("Kjønn"),STRING(Kunde.Kjon),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Kundenummer"),STRING(Kunde.KobletTilKunde),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Kommentar"),STRING(Kunde.Kommentar),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("E-Post adresse"),STRING(Kunde.KontE-Post),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Mobiltelefon"),STRING(Kunde.KontMobilTlf),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Kontaktperson"),STRING(Kunde.KontNavn),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Telefaks"),STRING(Kunde.KontTelefaks),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Telefon"),STRING(Kunde.KontTelefon),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
/* RUN PDFdata (oversatt("Kredit sperret"),STRING(Kunde.KreditSperret),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
RUN PDFdata (oversatt("Kundenummer"),STRING(Kunde.KundeNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Saldo"),STRING(Kunde.KundeSaldo),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Land"),STRING(Kunde.Land),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Kreditgrense"),STRING(Kunde.MaksKredit),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Mobiltelefon"),STRING(Kunde.MobilTlf),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Mva"),STRING(Kunde.Momskod),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
/* RUN PDFdata (oversatt("Utsendelser"),STRING(Kunde.MottaeMailUtsendelser),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
/* RUN PDFdata (oversatt("Mva fri"),STRING(Kunde.MvaFri),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).                    */
/* RUN PDFdata (oversatt("Opphørt"),STRING(Kunde.Opphort),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
RUN PDFdata (oversatt("Organisasjonsnummer"),STRING(Kunde.OrgNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
/* RUN PDFdata (oversatt("Postgiro"),STRING(Kunde.Postgiro),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
/* RUN PDFdata (oversatt("Privat"),STRING(Kunde.Privat),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
RUN PDFdata (oversatt("Priv.tlf"),STRING(Kunde.PrivatTlf),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
/* RUN PDFdata (oversatt("Purregebyr"),STRING(Kunde.Purregebyr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).                */
/* RUN PDFdata (oversatt("Region"),STRING(Kunde.Region),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).                        */
/* RUN PDFdata (oversatt("Registrert av"),STRING(Kunde.RegistrertAv),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).           */
/* RUN PDFdata (oversatt("Registrert dato"),STRING(Kunde.RegistrertDato),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).       */
/* RUN PDFdata (oversatt("Registreringstidspunkt"),STRING(Kunde.RegistrertTid),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
/* RUN PDFdata (oversatt("Samlefaktura"),STRING(Kunde.SamleFaktura),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
/* RUN PDFdata (oversatt("Siste kjøp"),STRING(Kunde.SisteKjop),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
/* RUN PDFdata (oversatt("Stilling"),STRING(Kunde.Stilling),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
RUN PDFdata (oversatt("Telefaks"),STRING(Kunde.Telefaks),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Telefon"),STRING(Kunde.Telefon),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Kontortelefon"),STRING(Kunde.TelefonFirma),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
/* RUN PDFdata (oversatt("Tilg.kilde"),STRING(Kunde.TilgKilde),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
/* RUN PDFdata (oversatt("Tittel"),STRING(Kunde.Tittel),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).             */
/* RUN PDFdata (oversatt("Totalrabatt%"),STRING(Kunde.TotalRabatt%),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
/* RUN PDFdata (oversatt("Kundetype"),STRING(Kunde.TypeId),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).          */
RUN PDFdata ("URL",STRING(Kunde.UrlFirma),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
/* RUN PDFdata (oversatt("ValutaKode"),STRING(Kunde.ValKod),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
/* RUN PDFdata ("Kan sende eMail"),STRING(Kunde.WebKanSendeEMail),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
/* RUN PDFdata ("Kan sette ordre"),STRING(Kunde.WebKanSetteOrdre),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY). */
RUN PDFdata (oversatt("Webkunde"),STRING(Kunde.WebKunde),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDF_Kundrelaterat C-Win 
PROCEDURE PDF_Kundrelaterat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iY AS INTEGER     NO-UNDO.
DEFINE VARIABLE iAntal AS INTEGER     NO-UNDO.


    FOR EACH tt_kundaktivitet:
        iY = 800.
        CASE tt_kundaktivitet.tabellnamn:
            WHEN "Tilgode"        THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Tillgodo/Tilgode"),INPUT-OUTPUT iY).
                FOR EACH Tilgode         WHERE Tilgode.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "Translogg"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Translogg/Translogg"),INPUT-OUTPUT iY).
                FOR EACH TransLogg       WHERE Translogg.Kundnr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "ReklamasjonsLogg"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","ReklamationsLogg/ReklamasjonsLogg"),INPUT-OUTPUT iY).
                FOR EACH ReklamasjonsLogg       WHERE ReklamasjonsLogg.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "KundeKort"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","KundKort/KundeKort"),INPUT-OUTPUT iY).
                FOR EACH KundeKort       WHERE KundeKort.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "KundeSaldo"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","KundSaldo/KundeSaldo"),INPUT-OUTPUT iY).
                FOR EACH KundeSaldo       WHERE KundeSaldo.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "KundeTrans"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","KundTrans/KundeTrans"),INPUT-OUTPUT iY).
                FOR EACH KundeTrans       WHERE KundeTrans.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "Gavekort"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Presentkort/Gavekort"),INPUT-OUTPUT iY).
                FOR EACH Gavekort       WHERE Gavekort.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "Kundereskontr"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Kundreskontr/Kundereskontr"),INPUT-OUTPUT iY).
                FOR EACH Kundereskontr       WHERE Kundereskontr.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "FakturaHode"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","FakturaHuvud/FakturaHode"),INPUT-OUTPUT iY).
                FOR EACH FakturaHode       WHERE FakturaHode.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "KundeBetTrans"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","KundBetTrans/KundeBetTrans"),INPUT-OUTPUT iY).
                FOR EACH KundeBetTrans       WHERE KundeBetTrans.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "KOrdreHode"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","KOrdreHode/KOrdreHode"),INPUT-OUTPUT iY).
                FOR EACH KOrdreHode       WHERE KOrdreHode.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "Kundeprosjekt"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Kundprojekt/Kundeprosjekt"),INPUT-OUTPUT iY).
                FOR EACH Kundeprosjekt       WHERE Kundeprosjekt.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "KundeKommentar"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","KundKommentar/KundeKommentar"),INPUT-OUTPUT iY).
                FOR EACH KundeKommentar       WHERE KundeKommentar.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "PrisListeKunde"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","PrisListeKunde/PrisListeKunde"),INPUT-OUTPUT iY).
                FOR EACH PrisListeKunde       WHERE PrisListeKunde.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "ekstbutiker"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","ekstbutiker/ekstbutiker"),INPUT-OUTPUT iY).
                FOR EACH ekstbutiker       WHERE ekstbutiker.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "BongHode"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","BongHode/BongHode"),INPUT-OUTPUT iY).
                FOR EACH BongHode       WHERE BongHode.Kundenr = dKundenr NO-LOCK:
                    RUN PDF_RelateradDataKund (tt_kundaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
        END CASE.
    END.
/* FOR EACH Individ          WHERE Individ.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="Individ" tt_kundaktivitet.Antal = iAntal. END. iAntal = 0. */
/* FOR EACH prKSData         WHERE prKSData.Kundenr = dKundenr NO-LOCK: iAntal = iAntal + 1. END. IF iAntal > 0 THEN DO: CREATE tt_kundaktivitet. ASSIGN tt_kundaktivitet.tabellnamn ="prKSData" tt_kundaktivitet.Antal = iAntal. END. iAntal = 0. */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDF_medlem C-Win 
PROCEDURE PDF_medlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*       DEFINE INPUT  PARAMETER cLabel    AS CHARACTER   NO-UNDO. */
/*       DEFINE INPUT  PARAMETER cTxt      AS CHARACTER   NO-UNDO. */
/*       DEFINE INPUT  PARAMETER iLblPos   AS INTEGER   NO-UNDO.   */
/*       DEFINE INPUT  PARAMETER iTxtPos   AS INTEGER   NO-UNDO.   */
/*       DEFINE INPUT  PARAMETER lTxtRight AS LOGICAL     NO-UNDO. */
/*       DEFINE INPUT  PARAMETER iY        AS INTEGER     NO-UNDO. */
/*       RUN pdf_text_xy_dec ("Spdf",cLabel,ilBlPos,iY).           */
/*       RUN pdf_text_xy_dec ("Spdf",cTxt,iTxtPos,iY).             */

DEFINE VARIABLE iY AS INTEGER     NO-UNDO.

RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
iY = 800.
skrivmitt("MEDLEMSINFO",iY).

RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).

iY = 750.    

RUN PDFdata (oversatt("Navn"),STRING(Medlem.ForNavn),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Etternavn"),STRING(Medlem.EtterNavn),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Medlemsnummer"),STRING(Medlem.MedlemsNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Medlemstype"),STRING(Medlem.MedType),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Medlemsgruppe"),STRING(Medlem.MedGruppe),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Ekst.medlemsnr"),STRING(Medlem.EksterntMedlemsNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Klubbnr"),STRING(Medlem.MKlubbId),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Personnr."),STRING(Medlem.PersonNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Aktiv"),STRING(Medlem.Aktiv),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Opphørt"),STRING(Medlem.Opphort),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Fødselsår"),STRING(Medlem.FodtAr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Fødselsdato"),STRING(Medlem.FodselsDato),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Adresse1"),STRING(Medlem.Adresse1),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Adresse2"),STRING(Medlem.Adresse2),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("PostNr"),STRING(Medlem.PostNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Bonus berettiget"),STRING(Medlem.Bonus_Berettiget),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Bonus forsendelse"),STRING(Medlem.Bonus_Forsendelse),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("E-Post adresse"),STRING(Medlem.ePostAdresse),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Utsendelser"),STRING(Medlem.MottaeMailUtsendelser),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Kundenummer"),STRING(Medlem.KundeNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Info"),STRING(Medlem.MedlemInfo),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Notat"),STRING(Medlem.MedlemNotat),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Mobiltelefon"),STRING(Medlem.MobilTlf),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Telefaks"),STRING(Medlem.Telefaks),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Telefon"),STRING(Medlem.Telefon),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Hovedmedlem"),STRING(Medlem.HovedMedlemFlagg),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Hovedmedlem"),STRING(Medlem.HovedMedlemsNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Kilde"),STRING(Medlem.Kilde),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Kjønn"),STRING(Medlem.Kjonn),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Aktivert fra Web"),STRING(Medlem.AktivertFraWeb),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Rabatt%"),STRING(Medlem.Rabatt),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Regionskode"),STRING(Medlem.RegKode),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Tilg.kilde"),STRING(Medlem.TilgKilde),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Brukerid (Web)"),STRING(Medlem.WebBrukerId),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Passord (Web)"),STRING(Medlem.WebPassord),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Butikk"),STRING(Medlem.ButikkNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Bydelsnummer"),STRING(Medlem.BydelsNr),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).


RUN PDFdata (oversatt("Registrert av"),STRING(Medlem.RegistrertAv),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Registrert dato"),STRING(Medlem.RegistrertDato),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Registreringstidspunkt"),STRING(Medlem.RegistrertTid),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Bruker"),STRING(Medlem.BrukerID),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Endret"),STRING(Medlem.EDato),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).
RUN PDFdata (oversatt("Endret tid"),STRING(Medlem.ETid),iLeftMargin,iPosMedlemData,FALSE,INPUT-OUTPUT iY).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDF_Medlemsrelaterat C-Win 
PROCEDURE PDF_Medlemsrelaterat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iAntal AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iY AS INTEGER     NO-UNDO.
    FOR EACH tt_medlemsaktivitet:
        iY = 800.
        CASE tt_medlemsaktivitet.tabellnamn:
            WHEN "Tilgode"        THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Tillgodo/Tilgode"),INPUT-OUTPUT iY).
                FOR EACH Tilgode         WHERE Tilgode.Medlemsnr = dMedlemsNr NO-LOCK:
                    RUN PDF_RelateradData (tt_medlemsaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "Translogg"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Translogg/Translogg"),INPUT-OUTPUT iY).
                FOR EACH TransLogg       WHERE TransLogg.Medlemsnr = dMedlemsNr NO-LOCK:
                    RUN PDF_RelateradData (tt_medlemsaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "Kundekort"      THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Kundkort/Kundekort"),INPUT-OUTPUT iY).
                FOR EACH KundeKort       WHERE KundeKort.Medlemsnr = dMedlemsNr NO-LOCK:
                    RUN PDF_RelateradData (tt_medlemsaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.     
            END.
            WHEN "Medlemskort"    THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Medlemskort/Medlemskort"),INPUT-OUTPUT iY).
                FOR EACH MedlemsKort     WHERE MedlemsKort.Medlemsnr = dMedlemsNr NO-LOCK:
                    RUN PDF_RelateradData (tt_medlemsaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.   
            END.
            WHEN "MedTrans"       THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Medlemstrans/Medlemstrans"),INPUT-OUTPUT iY).
                FOR EACH MedTrans        WHERE MedTrans.Medlemsnr = dMedlemsNr NO-LOCK:
                    RUN PDF_RelateradData (tt_medlemsaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.      
            END.
            WHEN "MedlemSaldo"    THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Medlemssaldo/Medlemssaldo"),INPUT-OUTPUT iY).
                FOR EACH MedlemSaldo     WHERE MedlemSaldo.Medlemsnr = dMedlemsNr NO-LOCK:
                    RUN PDF_RelateradData (tt_medlemsaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.   
            END.
            WHEN "Gavekort"       THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Presentkort/Gavekort"),INPUT-OUTPUT iY).
                FOR EACH Gavekort        WHERE Gavekort.Medlemsnr = dMedlemsNr NO-LOCK:
                    RUN PDF_RelateradData (tt_medlemsaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.      
            END.
            WHEN "MedlemBetTrans" THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Medlemsbetalning/Medlemsbetaling"),INPUT-OUTPUT iY).
                FOR EACH MedlemBetTrans  WHERE MedlemBetTrans.Medlemsnr = dMedlemsNr NO-LOCK:
                    RUN PDF_RelateradData (tt_medlemsaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "KundeBetTrans"  THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Kundbetalning/Kundebetaling"),INPUT-OUTPUT iY).
                FOR EACH KundeBetTrans   WHERE KundeBetTrans.Medlemsnr = dMedlemsNr NO-LOCK:
                    RUN PDF_RelateradData (tt_medlemsaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END. 
            END.
            WHEN "MedKjop"        THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Medlemsköp/Medlemskjøp"),INPUT-OUTPUT iY).
                FOR EACH MedKjop         WHERE MedKjop.Medlemsnr = dMedlemsNr NO-LOCK:
                    RUN PDF_RelateradData (tt_medlemsaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "MedRabReskontr" THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","MedRabReskontr/MedRabReskontr"),INPUT-OUTPUT iY).
                FOR EACH MedRabReskontr  WHERE MedRabReskontr.Medlemsnr = dMedlemsNr NO-LOCK:
                    RUN PDF_RelateradData (tt_medlemsaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "MedRabSjekk"    THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","MedRabSjekk/MedRabSjekk"),INPUT-OUTPUT iY).
                FOR EACH MedRabSjekk     WHERE MedRabSjekk.Medlemsnr = dMedlemsNr NO-LOCK:
                    RUN PDF_RelateradData (tt_medlemsaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
            WHEN "BongHode"       THEN DO:
                RUN PDF_SkrivSidRubrik (STRING(cSprak = "SE","Kvittohuvud/Bonghode"),INPUT-OUTPUT iY).
                FOR EACH BongHode WHERE BongHode.Medlemsnr = dMedlemsNr NO-LOCK:
                    RUN PDF_RelateradData (tt_medlemsaktivitet.tabellnamn,INPUT-OUTPUT iY).
                END.
            END.
        END CASE.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDF_RelateradData C-Win 
PROCEDURE PDF_RelateradData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTabell AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iY AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iTxtPos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cTTIdTxt AS CHAR EXTENT 10 NO-UNDO.
    ASSIGN cTTIdTxt[1]  = STRING(cSprak = "SE","Köp/Kjøp")
           cTTIdTxt[3]  = STRING(cSprak = "SE","Reklamation/Reklamasjon")
           cTTIdTxt[10] = STRING(cSprak = "SE","Retur/Retur")
    iTxtPos = iLeftMargin + 10.
    CASE cTabell:
        WHEN "Tilgode"        THEN DO:
            cTxt = STRING(Tilgode.Dato) + " Kr: " + STRING(Tilgode.Belop).
            RUN pdf_text_xy_dec ("Spdf",cTxt,iTxtPos,iY).
        END.
        WHEN "Translogg"      THEN DO:
/*             cTxt = STRING(TransLogg.Dato) + " " + (IF translogg.ttid > 10 THEN "Unknown" ELSE cTTIdTxt[translogg.TTId]).                */
/*             TransLogg.RabKr TransLogg.Pris TransLogg.Antall TransLogg.BongTekst TransLogg.Dato TransLogg.LinjeRab TransLogg.SubtotalRab */
            RUN pdf_text_xy_dec ("Spdf",STRING(TransLogg.Dato),iTxtPos,iY).
            RUN pdf_text_xy_dec ("Spdf",STRING(TransLogg.Antall),iTxtPos + 43,iY).
            RUN pdf_text_xy_dec ("Spdf",SUBSTR(TransLogg.BongTekst,1,20),iTxtPos + 60,iY).
            RUN pdf_text_xy_dec ("Spdf",STRING(TransLogg.pris),iTxtPos + 210,iY).
            RUN pdf_text_xy_dec ("Spdf",(IF translogg.ttid > 10 THEN "Unknown" ELSE cTTIdTxt[translogg.TTId]),iTxtPos + 240,iY).
        END.
        WHEN "Kundekort"      THEN DO:
            RUN pdf_text_xy_dec ("Spdf",STRING(KundeKort.Innehaver),iTxtPos,iY).
/*             KundeKort.Innehaver KundeKort.KundeNr KundeKort.MedlemsNr */
        END.
        WHEN "Medlemskort"    THEN DO:
            RUN pdf_text_xy_dec ("Spdf",STRING(MedlemsKort.Innehaver),iTxtPos,iY).
        END.
        WHEN "MedTrans"       THEN DO:
            RUN pdf_text_xy_dec ("Spdf",STRING(MedTrans.Dato),iTxtPos,iY).
            RUN pdf_text_xy_dec ("Spdf",STRING(MedTrans.Antall),iTxtPos + 43,iY).
            RUN pdf_text_xy_dec ("Spdf",SUBSTR(MedTrans.BongTekst,1,20),iTxtPos + 60,iY).
            RUN pdf_text_xy_dec ("Spdf",STRING(MedTrans.Pris),iTxtPos + 210,iY).
            RUN pdf_text_xy_dec ("Spdf",(IF MedTrans.ttid > 10 THEN "Unknown" ELSE cTTIdTxt[MedTrans.TTId]),iTxtPos + 240,iY).
/*              MedTrans.ArtikkelNr MedTrans.BatchNr MedTrans.BongId MedTrans.BongLinjeNr MedTrans.BongTekst MedTrans.Butik MedTrans.Dato MedTrans.ForsNr MedTrans.KassaNr MedTrans.KortNr MedTrans.LevNr MedTrans.LopNr MedTrans.MedlemsNr MedTrans.Mva MedTrans.Pris MedTrans.RabKr MedTrans.RefNr MedTrans.RefTekst MedTrans.RegistrertAv MedTrans.RegistrertDato MedTrans.RegistrertTid MedTrans.SattVVareKost MedTrans.SelgerNr MedTrans.SendtERP MedTrans.SeqNr MedTrans.Storl MedTrans.SubtotalRab MedTrans.TBId MedTrans.Tid MedTrans.TransNr MedTrans.TTId MedTrans.Vg MedTrans.VVarekost */
        END.
        WHEN "MedlemSaldo"    THEN DO:
            RUN pdf_text_xy_dec ("Spdf","Saldo: " + STRING(MedlemSaldo.Saldo) + " Tot oms: " + STRING(MedlemSaldo.TotaltKjop),iTxtPos,iY).
        END.
        WHEN "Gavekort"       THEN DO:
            RUN pdf_text_xy_dec ("Spdf",STRING(Gavekort.Dato) + " Kr: " + STRING(Gavekort.Belop),iTxtPos,iY).
        END.
        WHEN "MedlemBetTrans" THEN DO:
            FIND transtype WHERE transtype.ttid = MedlemBetTrans.TTId NO-LOCK NO-ERROR.
            cTxt = STRING(MedlemBetTrans.Dato) + " Kr: " + STRING(MedlemBetTrans.Belop) + " " + (IF AVAIL transtype THEN TransType.Beskrivelse ELSE "Unknown").
            RUN pdf_text_xy_dec ("Spdf",cTxt,iTxtPos,iY).
/*                 MedlemBetTrans.Avregnet MedlemBetTrans.BatchNr MedlemBetTrans.betBongId MedlemBetTrans.betButik MedlemBetTrans.betKassaNr MedlemBetTrans.BongId MedlemBetTrans.BongLinjeNr MedlemBetTrans.BrukerID MedlemBetTrans.Butik MedlemBetTrans.EDato MedlemBetTrans.ETid MedlemBetTrans.FakturaNr MedlemBetTrans.ForsNr MedlemBetTrans.KassaNr MedlemBetTrans.KontoNr MedlemBetTrans.KortNr MedlemBetTrans.KortType MedlemBetTrans.MedlemsNr MedlemBetTrans.MotPostert MedlemBetTrans.Notat MedlemBetTrans.OrgBelop MedlemBetTrans.RefNr MedlemBetTrans.RefTekst MedlemBetTrans.RegistrertAv MedlemBetTrans.RegistrertDato MedlemBetTrans.RegistrertTid MedlemBetTrans.SelgerNr MedlemBetTrans.SendtERP MedlemBetTrans.SeqNr MedlemBetTrans.Splittet MedlemBetTrans.TBId MedlemBetTrans.Tid MedlemBetTrans.TransNr */
        END.
        WHEN "KundeBetTrans"  THEN DO:
            FIND transtype WHERE transtype.ttid = KundeBetTrans.TTId NO-LOCK NO-ERROR.
            cTxt = STRING(KundeBetTrans.Dato) + " Kr: " + STRING(KundeBetTrans.Belop) + " " + (IF AVAIL transtype THEN TransType.Beskrivelse ELSE "Unknown").
            RUN pdf_text_xy_dec ("Spdf",cTxt,iTxtPos,iY).
/*             KundeBetTrans.TTId                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              */
/*             KundeBetTrans.Avregnet KundeBetTrans.BatchNr KundeBetTrans.Belop KundeBetTrans.betBongId KundeBetTrans.betButik KundeBetTrans.betKassaNr KundeBetTrans.BongId KundeBetTrans.BongLinjeNr KundeBetTrans.BrukerID KundeBetTrans.Butik KundeBetTrans.Dato KundeBetTrans.EDato KundeBetTrans.ETid KundeBetTrans.FakturaNr KundeBetTrans.ForsNr KundeBetTrans.KassaNr KundeBetTrans.KontoNr KundeBetTrans.KortNr KundeBetTrans.KortType KundeBetTrans.KundeNr KundeBetTrans.MedlemsNr KundeBetTrans.MotPostert KundeBetTrans.Notat KundeBetTrans.OrgBelop KundeBetTrans.RefNr KundeBetTrans.RefTekst KundeBetTrans.RegistrertAv KundeBetTrans.RegistrertDato KundeBetTrans.RegistrertTid KundeBetTrans.SelgerNr KundeBetTrans.SeqNr KundeBetTrans.Splittet KundeBetTrans.TBId KundeBetTrans.Tid KundeBetTrans.TransNr */
        END.
        WHEN "MedKjop"        THEN DO:
            cTxt = STRING(MedKjop.Kjopsdato) + " Kr: " + STRING(MedKjop.KjopsBelop).
            RUN pdf_text_xy_dec ("Spdf",cTxt,iTxtPos,iY).
/*             MedKjop.BrukerID MedKjop.ButikkNr MedKjop.B_id MedKjop.EDato MedKjop.ETid   MedKjop.KjopsGrunnlag MedKjop.KjopsTid MedKjop.MedlemsNr MedKjop.Notat MedKjop.RegistrertAv MedKjop.RegistrertDato MedKjop.RegistrertTid MedKjop.Saldo MedKjop.TildeltBelop */
        END.
        WHEN "MedRabReskontr" THEN DO:
        END.
        WHEN "MedRabSjekk"    THEN DO:
        END.
        WHEN "BongHode"       THEN DO:
            cTxt = STRING(BongHode.Dato) + " " + STRING(BongHode.Belop).
            RUN pdf_text_xy_dec ("Spdf",cTxt,iTxtPos,iY).
/*                 BongHode.BongNr BongHode.BongStatus BongHode.ButikkNr BongHode.b_id BongHode.DataSettId BongHode.Dato BongHode.EAv BongHode.EDato BongHode.EksportertDato BongHode.ETid BongHode.flBankkort BongHode.flBetalingskort BongHode.flGavekort BongHode.flKreditkort BongHode.flKupong1 BongHode.flRabatt BongHode.flRekvisisasjon BongHode.flSjekk BongHode.flSlKort BongHode.flSystemkort BongHode.Gradering BongHode.GruppeNr BongHode.Kampanje BongHode.KasseNr BongHode.KassererNavn BongHode.KassererNr BongHode.Konvertert BongHode.KOrdre_Id BongHode.KortType BongHode.KundeKort BongHode.KundeNavn BongHode.KundeNr BongHode.Logg BongHode.Makulert BongHode.MedlemNavn BongHode.MedlemsKort BongHode.MedlemsNr BongHode.OAv BongHode.ODato BongHode.OpdKvit BongHode.OpdUtskKopi BongHode.OTid BongHode.OverforingsNr BongHode.pfFlagg BongHode.SelgerNavn BongHode.SelgerNr BongHode.SkiftId BongHode.SkiftNr BongHode.Systemkort BongHode.Tid BongHode.TTId BongHode.UtskriftsKopi */
        END.
    END CASE.
    iY = iY - 16.
    IF iY < 100 THEN DO:
        iY = 800.
        RUN new_page.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDF_RelateradDataKund C-Win 
PROCEDURE PDF_RelateradDataKund :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cTabell AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iY AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iTxtPos AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cTTIdTxt AS CHAR EXTENT 10 NO-UNDO.
    ASSIGN cTTIdTxt[1]  = STRING(cSprak = "SE","Köp/Kjøp")
           cTTIdTxt[3]  = STRING(cSprak = "SE","Reklamation/Reklamasjon")
           cTTIdTxt[10] = STRING(cSprak = "SE","Retur/Retur")
    iTxtPos = iLeftMargin + 10.

    CASE cTabell:

        WHEN "Tilgode"        THEN DO:
            cTxt = STRING(Tilgode.Dato) + " Kr: " + STRING(Tilgode.Belop).
            RUN pdf_text_xy_dec ("Spdf",cTxt,iTxtPos,iY).
        END.
        WHEN "Translogg"      THEN DO:
            RUN pdf_text_xy_dec ("Spdf",STRING(TransLogg.Dato),iTxtPos,iY).
            RUN pdf_text_xy_dec ("Spdf",STRING(TransLogg.Antall),iTxtPos + 43,iY).
            RUN pdf_text_xy_dec ("Spdf",SUBSTR(TransLogg.BongTekst,1,20),iTxtPos + 60,iY).
            RUN pdf_text_xy_dec ("Spdf",STRING(TransLogg.pris),iTxtPos + 210,iY).
            RUN pdf_text_xy_dec ("Spdf",(IF translogg.ttid > 10 THEN "Unknown" ELSE cTTIdTxt[translogg.TTId]),iTxtPos + 240,iY).
        END.
        WHEN "ReklamasjonsLogg"      THEN DO:
            RUN pdf_text_xy_dec ("Spdf",STRING(ReklamasjonsLogg.RegistrertDato) + " " + ReklamasjonsLogg.Beskr,iTxtPos,iY).
            iY = iY - 12.
            RUN pdf_text_xy_dec ("Spdf",ReklamasjonsLogg.KundeNavn + " " + ReklamasjonsLogg.KundeAdresse + " " + ReklamasjonsLogg.PostNr,iTxtPos,iY).
            iY = iY - 12.
            RUN pdf_text_xy_dec ("Spdf",ReklamasjonsLogg.KundeTelefon + " " + ReklamasjonsLogg.KundeMobil + " " + ReklamasjonsLogg.KundeE-Mail,iTxtPos,iY).
        END.
        WHEN "KundeKort"      THEN DO:
            RUN pdf_text_xy_dec ("Spdf",STRING(KundeKort.Innehaver),iTxtPos,iY).
/*             KundeKort.Innehaver KundeKort.KundeNr KundeKort.MedlemsNr */
        END.
/*         WHEN "KundeSaldo"      THEN DO:    */
/*         END.                               */
/*         WHEN "KundeTrans"      THEN DO:    */
/*         END.                               */
/*         WHEN "Gavekort"      THEN DO:      */
/*         END.                               */
/*         WHEN "Kundereskontr"      THEN DO: */
/*         END.                               */
        WHEN "FakturaHode"      THEN DO:
            RUN pdf_text_xy_dec ("Spdf",STRING(FakturaHode.Dato) + " Nr: " + STRING(FakturaHode.FakturaNr) + " Kr: " + STRING(FakturaHode.Totalt) + " Ref: " + FakturaHode.DeresRef,iTxtPos,iY).
            RUN pdf_text_xy_dec ("Spdf",FakturaHode.Navn + " " + FakturaHode.Adresse1 + " " + FakturaHode.Adresse2 ,iTxtPos,iY - 12).
            RUN pdf_text_xy_dec ("Spdf",FakturaHode.PostNr + " " + FakturaHode.PostSted + " Tel: " + FakturaHode.Telefon + "/" + FakturaHode.Telefaks,iTxtPos,iY - 24).
            iY = iY - 24.    

/*  FakturaHode.FirmaNavn FakturaHode.FirmaAdresse1FakturaHode.FaktAdresse1 FakturaHode.FaktAdresse2 FakturaHode.FaktPostNr FakturaHode.FaktPoststed */
/*  FakturaHode.FirmaAdresse2 FakturaHode.FirmaEPost  FakturaHode.FirmaPostgiro                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          */
/*  FakturaHode.FirmaPostNr FakturaHode.FirmaPoststed FakturaHode.FirmaTelefaks FakturaHode.FirmaTelefon FakturaHode.FirmaURLAdresse FakturaHode.KontNavn FakturaHode.KontTelefon                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        */
/*  FakturaHode.LevAdresse1 FakturaHode.LevAdresse2 FakturaHode.LevPostNr FakturaHode.LevPostSted                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        */
/*                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       */
/*  FakturaHode.FakturertDato FakturaHode.FirmaBankKonto FakturaHode.FirmaOrganisasjonsNr FakturaHode.FirmaLand FakturaHode.Leveringsdato FakturaHode.ForfallsDato FakturaHode.Forskuddsbetalt FakturaHode.KOrdre_Id FakturaHode.KProsjektNr  FakturaHode.Land FakturaHode.Fraktbrevtekst FakturaHode.KundeNr FakturaHode.Godsmerking FakturaHode.KID FakturaHode.FNotat FakturaHode.LevFNr FakturaHode.LevFormBeskrivelse FakturaHode.LevFormMetode FakturaHode.LevLand FakturaHode.Mva FakturaHode.MvaKr FakturaHode.NettoPris FakturaHode.ProduksjonsDato FakturaHode.PurreTrinn FakturaHode.Referanse FakturaHode.RegistrertAv FakturaHode.RegistrertDato FakturaHode.RegistrertTid FakturaHode.SamleFaktura FakturaHode.SendingsNr FakturaHode.TotalRabatt% FakturaHode.TotalRabattKr  FakturaHode.TotaltVolum FakturaHode.Utsendelsesdato FakturaHode.VaarRef FakturaHode.ValKod FakturaHode.ValKurs FakturaHode.Varespesifikasjon */
/*  FakturaHode.AvrundingKr FakturaHode.AvrundingType FakturaHode.BetBet                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 */
        END.
/*         WHEN "KundeBetTrans"      THEN DO: */
/*         END.                               */
        WHEN "KOrdreHode"      THEN DO:
      
            RUN pdf_text_xy_dec ("Spdf",STRING(KOrdreHode.Dato) + " Id: " + STRING(KOrdreHode.KOrdre_Id) + " Kr: " + STRING(KOrdreHode.Totalt) + " Ref: " + KOrdreHode.DeresRef,iTxtPos,iY).

            RUN pdf_text_xy_dec ("Spdf",KOrdreHode.Navn + " " + KOrdreHode.Adresse1 + " " + KOrdreHode.Adresse2 + " " + KOrdreHode.PostNr + " " + KOrdreHode.PostSted,iTxtPos,iY - 12).
            RUN pdf_text_xy_dec ("Spdf","Tel & Epost: " + KOrdreHode.MobilTlf + " " + KOrdreHode.Telefon + " " + KOrdreHode.Telefaks + " " + KOrdreHode.ePostAdresse ,iTxtPos,iY - 24).
            iY = iY - 24.
            IF TRIM(KOrdreHode.FaktAdresse1) <> "" OR TRIM(KOrdreHode.FaktAdresse1) <> "" THEN DO:
                iY = iY - 12.
                RUN pdf_text_xy_dec ("Spdf","Fakt: " + KOrdreHode.FaktAdresse1 + " " + KOrdreHode.FaktAdresse1 + " " + KOrdreHode.FaktPostNr + " " + KOrdreHode.FaktPoststed ,iTxtPos,iY).
            END.
            IF TRIM(KOrdreHode.KontNavn) <> "" THEN DO:
                iY = iY - 12.
                RUN pdf_text_xy_dec ("Spdf","Kont: " + KOrdreHode.KontNavn + " " + KOrdreHode.KontTelefon,iTxtPos,iY).
            END.
            IF TRIM(KOrdreHode.FirmaNavn) <> "" THEN DO:
                iY = iY - 12.
                RUN pdf_text_xy_dec ("Spdf",KOrdreHode.FirmaNavn + " " + KOrdreHode.FirmaAdresse1 + " " + KOrdreHode.FirmaAdresse2 + " " + KOrdreHode.FirmaPostNr + " " + KOrdreHode.FirmaPostSted,iTxtPos,iY).
                iY = iY - 12.
                RUN pdf_text_xy_dec ("Spdf","Tel & Epost: " + KOrdreHode.FirmaTelefon + " " + KOrdreHode.FirmaTelefaks + " " + KOrdreHode.FirmaEPost + " " + KOrdreHode.FirmaURLAdresse ,iTxtPos,iY).
        /*         KOrdreHode.FirmaOrganisasjonsNr */
            END.
            IF TRIM(KOrdreHode.LevAdresse1) <> "" OR TRIM(KOrdreHode.LevAdresse2) <> "" THEN DO:
                iY = iY - 12.
                RUN pdf_text_xy_dec ("Spdf","Lev: " + KOrdreHode.LevAdresse1  + " " + KOrdreHode.LevAdresse2  + " " + KOrdreHode.FirmaAdresse2 + " " + KOrdreHode.LevPostNr + " " + KOrdreHode.LevPostSted,iTxtPos,iY).
            END.
        END.
/*         WHEN "Kundeprosjekt"      THEN DO:  */
/*         END.                                */
/*         WHEN "KundeKommentar"      THEN DO: */
/*         END.                                */
/*         WHEN "PrisListeKunde"      THEN DO: */
/*         END.                                */
/*         WHEN "ekstbutiker"      THEN DO:    */
/*         END.                                */
        WHEN "BongHode"      THEN DO:
            cTxt = STRING(BongHode.Dato) + " " + STRING(BongHode.Belop).
            RUN pdf_text_xy_dec ("Spdf",cTxt,iTxtPos,iY).
/*                 BongHode.BongNr BongHode.BongStatus BongHode.ButikkNr BongHode.b_id BongHode.DataSettId BongHode.Dato BongHode.EAv BongHode.EDato BongHode.EksportertDato BongHode.ETid BongHode.flBankkort BongHode.flBetalingskort BongHode.flGavekort BongHode.flKreditkort BongHode.flKupong1 BongHode.flRabatt BongHode.flRekvisisasjon BongHode.flSjekk BongHode.flSlKort BongHode.flSystemkort BongHode.Gradering BongHode.GruppeNr BongHode.Kampanje BongHode.KasseNr BongHode.KassererNavn BongHode.KassererNr BongHode.Konvertert BongHode.KOrdre_Id BongHode.KortType BongHode.KundeKort BongHode.KundeNavn BongHode.KundeNr BongHode.Logg BongHode.Makulert BongHode.MedlemNavn BongHode.MedlemsKort BongHode.MedlemsNr BongHode.OAv BongHode.ODato BongHode.OpdKvit BongHode.OpdUtskKopi BongHode.OTid BongHode.OverforingsNr BongHode.pfFlagg BongHode.SelgerNavn BongHode.SelgerNr BongHode.SkiftId BongHode.SkiftNr BongHode.Systemkort BongHode.Tid BongHode.TTId BongHode.UtskriftsKopi */
        END.
    END CASE.
    iY = iY - 16.
    IF iY < 100 THEN DO:
        iY = 800.
        RUN new_page.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PDF_SkrivSidRubrik C-Win 
PROCEDURE PDF_SkrivSidRubrik :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER cRubrik AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT  PARAMETER iY AS INTEGER     NO-UNDO.

    RUN new_page.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",12).
    skrivmitt(cRubrik,iY).
    iY = 750.
    RUN pdf_set_font IN h_PDFinc ("Spdf", "Helvetica-Bold",10).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE RapportPDF C-Win 
PROCEDURE RapportPDF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
Font      Storlek Sidlayout Första TO_Vänsterjusterat Sista_TO Sista_AT
Helvetika      10 Landscape      6                    121      285
               11                6                    110      259
               12                5                    100      237
               10 Portrait       6                     82      192
               11                6                     75      174
               12                5                     68      160
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cOrdrenr AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iAntall AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iColLabelPage AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cLoadedFont AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE iWhat  AS INTEGER     NO-UNDO.
  DEFINE VARIABLE iWidth AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dDato AS DATE    NO-UNDO.
  DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dYR AS INTEGER     NO-UNDO.
  DEFINE VARIABLE dYL AS INTEGER     NO-UNDO.
  DEFINE VARIABLE d2 AS INTEGER     NO-UNDO.
  cFilNavn = SESSION:TEMP-DIR + "GDPR_rapport" + "_" + STRING(IF dMedlemsNr > 0 THEN dMedlemsNr ELSE dKundenr) + ".pdf".

    /* skapa ett utlägg pr butik */
  RUN pdf_new ("Spdf",cFilNavn).
  RUN pdf_set_PaperType ("Spdf","A4").
/*   RUN pdf_set_Orientation ("Spdf","Landscape"). */
  
/*   iLeftMargin = 25.                           */
/*   iPageHeight = pdf_PageHeight ("Spdf").      */
/*   iPageWidth  = pdf_PageWidth ("Spdf").       */
/*   iRMarginPos =  iPageWidth - iLeftMargin.    */
/*   iMittenR = iPageWidth / 2 + iPageWidth / 4. */
/*   iStartRow = iPageHeight - 28.               */
  RUN pdf_set_BottomMargin ("Spdf", 60).
/*   iLMp2 = iPageWidth / 2 + iLeftMargin. */
/*   RUN pdf_text_xy_dec ("Spdf","XXXX",iLMp2,100). */
  
/*   pdf_PageHeader ("Spdf",THIS-PROCEDURE:HANDLE,"PageHeader"). */
/*   pdf_PageFooter ("Spdf",THIS-PROCEDURE:HANDLE,"PageFooter"). */
  
  RUN pdf_set_VerticalSpace ("Spdf",13).
/*   RUN pdf_set_VerticalSpace IN h_PDFinc ("Spdf",13). */
  
/* Ladda Image */
/*   RUN pdf_load_image IN h_PDFinc ("Spdf","HEADERLOGO",cImageFile). */

/*   RUN pdf_load_font ("Spdf","GantModern-Regular","pdfinclude\GantModern-Regular.TTF","pdfinclude\GantModern-Regular.AFM",""). */
/*   RUN pdf_load_font ("Spdf","GantModern-Medium","pdfinclude\GantModern-Medium.TTF","pdfinclude\GantModern-Medium.AFM","").    */
/*   RUN pdf_load_font ("Spdf","GantModern-Bold","pdfinclude\GantModern-Bold.TTF","pdfinclude\GantModern-Bold.AFM","").          */
/*   RUN pdf_load_font ("Spdf","Wingding","pdfinclude\Wingding.TTF","pdfinclude\Wingding.AFM","").                               */


  RUN new_page.
  iColLabelPage = 1.

  IF dMedlemsNr > 0 AND AVAIL Medlem THEN DO: /* Här skrivs allt medlemsrelaterat */
      RUN PDF_medlem.
      RUN PDF_Medlemsrelaterat.
  END.
  IF dKundeNr > 0 AND AVAIL Kunde THEN DO: /* Här skrivs allt medlemsrelaterat */
      RUN new_page.
      RUN PDF_kunde.
      RUN PDF_Kundrelaterat.
  END.
  RUN pdf_close ("Spdf").
  RUN browse2pdf\viewxmldialog.w (cFilNavn,"GDPR").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION bredd C-Win 
FUNCTION bredd RETURNS DECIMAL
  ( INPUT cText AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN pdf_text_widthdec ("Spdf",cText).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getTTIdTxt C-Win 
FUNCTION getTTIdTxt RETURNS CHARACTER
  ( INPUT iTTId AS INTE ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cTxt AS CHARACTER   NO-UNDO.
  CASE iTTId:
      WHEN  1 THEN cTxt = STRING(cSprak = "SE","Köp/Kjøp").
      WHEN  3 THEN cTxt = STRING(cSprak = "SE","Reklamation/Reklamasjon").
      WHEN 10 THEN cTxt = STRING(cSprak = "SE","Retur/Retur").
  END CASE.
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION oversatt C-Win 
FUNCTION oversatt RETURNS CHARACTER
  ( INPUT cTxt AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cRetvalue AS CHARACTER   NO-UNDO.
  IF NOT cSprak = "SE" THEN
      RETURN cTxt.
  cRetvalue = cTxt.
  CASE cTxt:
        WHEN "Adresse1"                THEN cRetvalue =  "Adress1"               .
        WHEN "Adresse2"                THEN cRetvalue =  "Adress2"               .
        WHEN "Aktiv"                  THEN cRetvalue =   "Aktiv"                 .
        WHEN "Aktivert fra Web"       THEN cRetvalue =   "Aktiverad från Web"      .
        WHEN "Bonus berettiget"       THEN cRetvalue =   "Bonus berättigad"      .
        WHEN "Bonus forsendelse"      THEN cRetvalue =   "Bonus försändelse"     .
        WHEN "Bruker"                 THEN cRetvalue =   "Användare"                .
        WHEN "Butikk"                 THEN cRetvalue =   "Butik"                .
        WHEN "Bydelsnummer"           THEN cRetvalue =   "Bydelsnummer"          .
        WHEN "Endret"                 THEN cRetvalue =   "Ändrad"                .
        WHEN "Ekst.medlemsnr"         THEN cRetvalue =   "Exst.medlemsnr"        .
        WHEN "E-Post adresse"         THEN cRetvalue =   "E-Mailadress"        .
        WHEN "Endret tid"             THEN cRetvalue =   "Ändret tid"            .
        WHEN "Etternavn"              THEN cRetvalue =   "Efternamn"             .
        WHEN "Fødselsdato"            THEN cRetvalue =   "Födselsedatum"           .
        WHEN "Fødselsår"              THEN cRetvalue =   "Födselseår"             .
        WHEN "Navn"                   THEN cRetvalue =   "Namn"                  .
        WHEN "Hovedmedlem"            THEN cRetvalue =   "Huvudmedlem"           .
        WHEN "Kilde"                  THEN cRetvalue =   "Källa"                 .
        WHEN "Kjønn"                  THEN cRetvalue =   "Kön"                 .
        WHEN "Kundenummer"            THEN cRetvalue =   "Kundnummer"           .
        WHEN "Land"                   THEN cRetvalue =   "Land"                  .
        WHEN "Medlemsgruppe"          THEN cRetvalue =   "Medlemsgrupp"         .
        WHEN "Info"                   THEN cRetvalue =   "Info"                  .
        WHEN "Notat"                  THEN cRetvalue =   "Notering"                 .
        WHEN "Medlemsnummer"          THEN cRetvalue =   "Medlemsnummer"         .
        WHEN "Medlemstype"            THEN cRetvalue =   "Medlemstyp"           .
        WHEN "Klubbnr"                THEN cRetvalue =   "Klubbnr"               .
        WHEN "Mobiltelefon"           THEN cRetvalue =   "Mobiltelefon"          .
        WHEN "Utsendelser"            THEN cRetvalue =   "Utsändelser"           .
        WHEN "Opphørt"                THEN cRetvalue =   "Upphört"               .
        WHEN "Personnr."              THEN cRetvalue =   "Personnr."             .
        WHEN "PostNr"                 THEN cRetvalue =   "PostNr"                .
        WHEN "Rabatt%"                THEN cRetvalue =   "Rabatt%"               .
        WHEN "Registrert av"          THEN cRetvalue =   "Registrerad av"         .
        WHEN "Registrert dato"        THEN cRetvalue =   "Registrerad datum"       .
        WHEN "Registreringstidspunkt" THEN cRetvalue =   "Registreringstidspunkt".
        WHEN "Regionskode"            THEN cRetvalue =   "Regionskode"           .
        WHEN "Telefaks"               THEN cRetvalue =   "Telefax"              .
        WHEN "Telefon"                THEN cRetvalue =   "Telefon"               .
        WHEN "Tilg.kilde"             THEN cRetvalue =   "Tillg.källa"            .
        WHEN "Brukerid (Web)"         THEN cRetvalue =   "Användarid (Web)"        .
        WHEN "Passord (Web)"          THEN cRetvalue =   "Lösenord (Web)"         .
        
  END CASE.

  RETURN cRetvalue.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION skrivmitt C-Win 
FUNCTION skrivmitt RETURNS CHARACTER
  ( INPUT cTxt AS CHARACTER, INPUT iYpos AS INTEGER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iAvailBredd AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iMitten AS INTEGER     NO-UNDO.
    iAvailBredd = pdf_PageWidth("Spdf") - iLeftMargin - iLeftMargin. /* Lika högermarginal */
    iMitten = iAvailBredd / 2.

   RUN pdf_text_xy_dec ("Spdf",cTxt,iMitten - bredd(cTxt) / 2,iYpos).
   
  RETURN "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

