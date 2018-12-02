&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description:        Container for a JukeBox window program

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author:            brynjar@chemistry.no

  Created:           18.oct.2006

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

DEFINE VARIABLE bOk        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hBrowse    AS HANDLE    NO-UNDO.
DEFINE VARIABLE hFieldMap  AS HANDLE    NO-UNDO.
DEFINE VARIABLE hQuery     AS HANDLE    NO-UNDO.
DEFINE VARIABLE hToolbar   AS HANDLE    NO-UNDO.
DEFINE VARIABLE ix         AS INTEGER   NO-UNDO.
DEF    VAR      cButikkNr  AS CHAR      NO-UNDO.

DEFINE VARIABLE hParentQuery AS HANDLE      NO-UNDO.
DEFINE VARIABLE hSearch      AS HANDLE      NO-UNDO.

/*Need to use it to differanciate between browser and detail regarding functionality for toolbar.
  When we start with Detail window, I don't need navigation but SearchButton. When starting as
  browser, I need detail winodow to have navigation but not SearchButton */
DEFINE VARIABLE cParentType  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cAvtaleIDListItemPairs AS CHARACTER   NO-UNDO.

DEFINE VARIABLE lValueChanged AS LOGICAL     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Falck_Sykkelregister

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH Falck_Sykkelregister SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH Falck_Sykkelregister SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME Falck_Sykkelregister
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME Falck_Sykkelregister


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS rectToolbar RECT-1 btnCalDatoSalg RECT-2 ~
RECT-3 RECT-4 KlarForSending Eier_Fornavn Eier_Etternavn Eier_Adresse ~
Eier_PostNr Eier_Mobil Eier_Telefon Eier_epost Foresatt_Fornavn ~
Foresatt_Etternavn Foresatt_Fodt btnPostnr Arsmodell Sykkeltype Fabrikat ~
Rammenummer Sec_Nr AvtaleID ButikkNr ForsNr Dato_Solgt Pris btnButikknr ~
btnCalDato3 
&Scoped-Define DISPLAYED-OBJECTS KlarForSending Eier_Fornavn Eier_Etternavn ~
Eier_Adresse Eier_PostNr Eier_Mobil Eier_Telefon Eier_epost ~
Foresatt_Fornavn Foresatt_Etternavn Foresatt_Fodt Eier_Poststed Arsmodell ~
Sykkeltype Fabrikat Rammenummer Sec_Nr AvtaleID ButikkNr Kampanjekode ~
ForsNr Dato_Solgt Pris lblEier lblEier-2 lblSykkel lblButikk butnamn 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getQueryHandle C-Win 
FUNCTION getQueryHandle RETURNS HANDLE
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setParentType C-Win 
FUNCTION setParentType RETURNS LOGICAL
  (INPUT icParentType AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnButikknr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnCalDato3 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnCalDatoSalg 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnPostnr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE AvtaleID LIKE Falck_Sykkelregister.AvtaleID
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Kun lokalt register",0,
                     "3 mnd gratis",1,
                     "3 års registrering betalt i butikk",2,
                     "Barnesykkel",3,
                     "3 års tyverimerking betalt i butikk",4
     DROP-DOWN-LIST
     SIZE 33 BY 1 NO-UNDO.

DEFINE VARIABLE Arsmodell AS CHARACTER FORMAT "X(4)" 
     LABEL "Årsmodell" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1
     BGCOLOR 14 .

DEFINE VARIABLE ButikkNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Butikknummer" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 14 .

DEFINE VARIABLE butnamn AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 14.6 BY 1 NO-UNDO.

DEFINE VARIABLE Dato_Solgt AS DATE FORMAT "99/99/9999" 
     LABEL "Salgsdato" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 14 .

DEFINE VARIABLE Eier_Adresse AS CHARACTER FORMAT "X(56)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1
     BGCOLOR 14 .

DEFINE VARIABLE Eier_epost AS CHARACTER FORMAT "X(100)" 
     LABEL "EMail" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1.

DEFINE VARIABLE Eier_Etternavn AS CHARACTER FORMAT "X(56)" 
     LABEL "Etternavn" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1
     BGCOLOR 14 .

DEFINE VARIABLE Eier_Fornavn AS CHARACTER FORMAT "X(56)" 
     LABEL "Fornavn" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1
     BGCOLOR 14 .

DEFINE VARIABLE Eier_Mobil AS CHARACTER FORMAT "X(16)" 
     LABEL "Mobil" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 14 .

DEFINE VARIABLE Eier_PostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "Postnr" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1
     BGCOLOR 14 .

DEFINE VARIABLE Eier_Poststed AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 36 BY 1.

DEFINE VARIABLE Eier_Telefon AS CHARACTER FORMAT "X(16)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE EksportertDato AS DATE FORMAT "99/99/9999" 
     LABEL "Eksportert dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE EksportertTid AS INTEGER FORMAT "->,>>>,>>9" INITIAL 0 
     LABEL "Eksportert tid" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE Fabrikat AS CHARACTER FORMAT "X(30)" 
     LABEL "Fabrikat" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 14 .

DEFINE VARIABLE Foresatt_Etternavn AS CHARACTER FORMAT "X(56)" 
     LABEL "Foresatt etternavn" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1.

DEFINE VARIABLE Foresatt_Fodt AS DATE FORMAT "99/99/99" 
     LABEL "Foresatt født" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE Foresatt_Fornavn AS CHARACTER FORMAT "X(56)" 
     LABEL "Foresatt fornavn" 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1.

DEFINE VARIABLE ForsNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Kasserer" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1.

DEFINE VARIABLE Kampanjekode AS CHARACTER FORMAT "X(10)" 
     LABEL "ForhandlerID" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 14 .

DEFINE VARIABLE lblButikk AS CHARACTER FORMAT "X(256)":U INITIAL "Butikk informasjon" 
      VIEW-AS TEXT 
     SIZE 50 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lblEier AS CHARACTER FORMAT "X(256)":U INITIAL "Eier informasjon" 
      VIEW-AS TEXT 
     SIZE 75.6 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lblEier-2 AS CHARACTER FORMAT "X(256)":U INITIAL "Foresatt informasjon" 
      VIEW-AS TEXT 
     SIZE 76 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE lblSykkel AS CHARACTER FORMAT "X(256)":U INITIAL "Sykkel informasjon" 
      VIEW-AS TEXT 
     SIZE 49 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE Pris AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1
     BGCOLOR 14 .

DEFINE VARIABLE Rammenummer AS CHARACTER FORMAT "X(30)" 
     LABEL "Rammenummer" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 14 .

DEFINE VARIABLE Sec_Nr AS CHARACTER FORMAT "X(16)" 
     LABEL "Sec_Nr" 
     VIEW-AS FILL-IN 
     SIZE 25 BY 1.

DEFINE VARIABLE Sykkeltype AS CHARACTER FORMAT "X(30)" 
     LABEL "Modell" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1
     BGCOLOR 14 .

DEFINE VARIABLE KlarForSending AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Securmark", yes,
"Lokal", no
     SIZE 25 BY .95 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 10.43.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 10.48.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 5.95.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 79 BY 5.95.

DEFINE RECTANGLE rectToolbar
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 19 BY .95.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      Falck_Sykkelregister SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     btnCalDatoSalg AT ROW 17.95 COL 116.8
     KlarForSending AT ROW 3.38 COL 21.6 NO-LABEL
     Eier_Fornavn AT ROW 4.33 COL 19.6 COLON-ALIGNED HELP
          "Eiers fornavn"
     Eier_Etternavn AT ROW 5.33 COL 19.6 COLON-ALIGNED HELP
          "Eiers etternavn"
     Eier_Adresse AT ROW 6.33 COL 19.6 COLON-ALIGNED HELP
          "Eiers adresse"
     Eier_PostNr AT ROW 7.33 COL 19.6 COLON-ALIGNED HELP
          "Eiers postnummer"
     Eier_Mobil AT ROW 8.33 COL 19.6 COLON-ALIGNED HELP
          "Eiers mobiltelefonnummer"
     Eier_Telefon AT ROW 9.33 COL 19.6 COLON-ALIGNED HELP
          "Eiers telefonnummer"
     Eier_epost AT ROW 10.33 COL 19.6 COLON-ALIGNED HELP
          "Eiers eMail adresse"
     Falck_Sykkelregister.Kjonn AT ROW 11.38 COL 19.6 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Ukjent",0,
                     "Mann",1,
                     "Kvinne",2
          DROP-DOWN-LIST
          SIZE 18 BY 1 TOOLTIP "Kjønn"
     Foresatt_Fornavn AT ROW 15.95 COL 19.6 COLON-ALIGNED HELP
          "Fornavn på foresatt"
     Foresatt_Etternavn AT ROW 16.95 COL 19.6 COLON-ALIGNED HELP
          "Etternavn på foresatt"
     Foresatt_Fodt AT ROW 17.95 COL 19.6 COLON-ALIGNED HELP
          "Foresattes fødselsdato"
     btnPostnr AT ROW 7.33 COL 37.4 NO-TAB-STOP 
     Eier_Poststed AT ROW 7.33 COL 39.6 COLON-ALIGNED HELP
          "Eiers poststed" NO-LABEL
     EksportertDato AT ROW 11.43 COL 59.6 COLON-ALIGNED
     EksportertTid AT ROW 12.43 COL 59.6 COLON-ALIGNED
     Arsmodell AT ROW 4.14 COL 96 COLON-ALIGNED HELP
          "Årsmodell"
     Sykkeltype AT ROW 5.14 COL 96 COLON-ALIGNED HELP
          "Sykkeltype"
     Fabrikat AT ROW 6.14 COL 96 COLON-ALIGNED HELP
          "Fabrikat"
     Rammenummer AT ROW 7.14 COL 96 COLON-ALIGNED HELP
          "Rammenummer"
     Sec_Nr AT ROW 9.81 COL 96 COLON-ALIGNED
     AvtaleID AT ROW 11.24 COL 96 COLON-ALIGNED HELP
          ""
     ButikkNr AT ROW 14.81 COL 96.6 COLON-ALIGNED HELP
          "Butikknummer"
     Kampanjekode AT ROW 15.81 COL 96.6 COLON-ALIGNED HELP
          "Kampanjekode"
     ForsNr AT ROW 16.81 COL 96.6 COLON-ALIGNED HELP
          "Kasserernummer"
     Dato_Solgt AT ROW 17.81 COL 96.6 COLON-ALIGNED HELP
          "Dato solgt"
     Pris AT ROW 18.81 COL 96.6 COLON-ALIGNED HELP
          "Pris"
     btnButikknr AT ROW 14.76 COL 116.8 NO-TAB-STOP 
     btnCalDato3 AT ROW 17.95 COL 39.6
     lblEier AT ROW 2.48 COL 1.4 COLON-ALIGNED NO-LABEL
     lblEier-2 AT ROW 13.76 COL 3 NO-LABEL
     lblSykkel AT ROW 2.48 COL 80 COLON-ALIGNED NO-LABEL
     lblButikk AT ROW 13.81 COL 80 COLON-ALIGNED NO-LABEL
     butnamn AT ROW 15.95 COL 115.4 COLON-ALIGNED NO-LABEL
     rectToolbar AT ROW 1.14 COL 1.6
     RECT-1 AT ROW 3.19 COL 2
     RECT-2 AT ROW 3.14 COL 81
     RECT-3 AT ROW 14.48 COL 81
     RECT-4 AT ROW 14.48 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.2 ROW 1
         SIZE 132.8 BY 19.57.


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
         TITLE              = "Registrering i Sykkelregisteret"
         HEIGHT             = 19.57
         WIDTH              = 132.8
         MAX-HEIGHT         = 49.29
         MAX-WIDTH          = 384
         VIRTUAL-HEIGHT     = 49.29
         VIRTUAL-WIDTH      = 384
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT C-Win:LOAD-ICON("ico/app16.ico":U) THEN
    MESSAGE "Unable to load icon: ico/app16.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME L-To-R,COLUMNS                                            */
/* SETTINGS FOR COMBO-BOX AvtaleID IN FRAME DEFAULT-FRAME
   LIKE = SkoTex.Falck_Sykkelregister.                                  */
/* SETTINGS FOR FILL-IN butnamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Eier_Poststed IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Eier_Poststed:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN EksportertDato IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       EksportertDato:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN EksportertTid IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       EksportertTid:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN Kampanjekode IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Falck_Sykkelregister.Kjonn IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
/* SETTINGS FOR FILL-IN lblButikk IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblButikk:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblEier IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblEier:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblEier-2 IN FRAME DEFAULT-FRAME
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       lblEier-2:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblSykkel IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblSykkel:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "skotex.Falck_Sykkelregister"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Registrering i Sykkelregisteret */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Registrering i Sykkelregisteret */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Arsmodell
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Arsmodell C-Win
ON ANY-PRINTABLE OF Arsmodell IN FRAME DEFAULT-FRAME /* Årsmodell */
DO:
  IF LASTKEY < 48 OR LASTKEY > 57 THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnButikknr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButikknr C-Win
ON CHOOSE OF btnButikknr IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF butikknr DO:
  DEF VAR cTekst AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                    "butiker"
                     + ";butik"
                     + ";ButNamn" 
                     ,
                   "WHERE true"
                    ,""
                    ,"butik,butnamn",
                    OUTPUT cTekst,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cTekst NE "" THEN DO:
    ASSIGN 
       Butikknr:SCREEN-VALUE      = ENTRY(1,cTekst,"|")
       butnamn:SCREEN-VALUE = ENTRY(2,cTekst,"|")
       .
    APPLY "any-printable" TO butikknr.
  END.
  APPLY "ENTRY" TO butikknr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalDato3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalDato3 C-Win
ON CHOOSE OF btnCalDato3 IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF Foresatt_Fodt
DO:
  RUN Cal.w (Foresatt_Fodt:HANDLE).
  IF Foresatt_Fodt:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCalDatoSalg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCalDatoSalg C-Win
ON CHOOSE OF btnCalDatoSalg IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF dato_solgt
DO:
  RUN Cal.w (dato_solgt:HANDLE).
  IF dato_solgt:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery').  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPostnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPostnr C-Win
ON CHOOSE OF btnPostnr IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF eier_postnr DO:
  DEF VAR cTekst AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,1000,
                    "post"
                     + ";postnr"
                     + ";Beskrivelse"
                     ,
                   "WHERE true"
                    ,""
                    ,"postnr,Beskrivelse",
                    OUTPUT cTekst,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cTekst NE "" THEN DO:
    ASSIGN 
       eier_Postnr:SCREEN-VALUE      = ENTRY(1,cTekst,"|")
       eier_poststed:SCREEN-VALUE = ENTRY(2,cTekst,"|")
       .
    APPLY "any-printable" TO eier_postnr.
  END.
  APPLY "ENTRY" TO eier_postnr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Eier_Adresse
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Eier_Adresse C-Win
ON RETURN OF Eier_Adresse IN FRAME DEFAULT-FRAME /* Adresse */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO:
      APPLY "ENTRY" TO Eier_PostNr.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Eier_epost
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Eier_epost C-Win
ON RETURN OF Eier_epost IN FRAME DEFAULT-FRAME /* EMail */
DO:
    IF INPUT KlarForSending = TRUE AND SELF:SCREEN-VALUE = "" THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Eier_Etternavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Eier_Etternavn C-Win
ON RETURN OF Eier_Etternavn IN FRAME DEFAULT-FRAME /* Etternavn */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO:
      APPLY "ENTRY" TO Eier_Adresse.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Eier_Fornavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Eier_Fornavn C-Win
ON RETURN OF Eier_Fornavn IN FRAME DEFAULT-FRAME /* Fornavn */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO:
      APPLY "ENTRY" TO Eier_Etternavn.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Eier_Mobil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Eier_Mobil C-Win
ON RETURN OF Eier_Mobil IN FRAME DEFAULT-FRAME /* Mobil */
DO:
/*   IF SELF:SCREEN-VALUE <> "" THEN DO:                          */
/*       IF INPUT KlarForSending = TRUE THEN                      */
/*           Eier_Telefon:SCREEN-VALUE = Eier_Mobil:SCREEN-VALUE. */
/*       APPLY "ENTRY" TO Eier_Telefon.                           */
/*       RETURN NO-APPLY.  */
/*   END.                                                         */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Eier_PostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Eier_PostNr C-Win
ON LEAVE OF Eier_PostNr IN FRAME DEFAULT-FRAME /* Postnr */
DO:
DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,200,
                    "post"
                    + ";postnr"
                    + ";beskrivelse"
                   ,"WHERE true"
                    ,""                                                  
                    ,"postnr,beskrivelse",   /* <- return values for these fields */
                    OUTPUT cReturnValues,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN 
      eier_postnr:SCREEN-VALUE = ENTRY(1,cReturnValues,"|")
      eier_poststed:SCREEN-VALUE = ENTRY(2,cReturnValues,"|")
           .

    APPLY "any-printable" TO eier_postnr.
    APPLY "ENTRY" TO Eier_Mobil.
    RETURN NO-APPLY.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Eier_PostNr C-Win
ON RETURN OF Eier_PostNr IN FRAME DEFAULT-FRAME /* Postnr */
DO:
  IF SELF:SCREEN-VALUE <> "" THEN DO:
      APPLY "LEAVE" TO SELF.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Eier_Telefon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Eier_Telefon C-Win
ON RETURN OF Eier_Telefon IN FRAME DEFAULT-FRAME /* Telefon */
DO:
  IF INPUT KlarForSending = TRUE AND SELF:SCREEN-VALUE = "" THEN
      RETURN NO-APPLY.
  IF SELF:SCREEN-VALUE <> "" THEN DO:
      APPLY "ENTRY" TO Eier_epost.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Pris
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Pris C-Win
ON LEAVE OF Pris IN FRAME DEFAULT-FRAME /* Pris */
DO:
  APPLY 'ENTRY' TO Eier_Fornavn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE DO:
  IF NOT DYNAMIC-FUNCTION("DoCleanUpObjects",THIS-PROCEDURE:CURRENT-WINDOW) THEN RETURN NO-APPLY.
  DYNAMIC-FUNCTION("SetCleanUpResize",THIS-PROCEDURE:CURRENT-WINDOW).
  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

cAvtaleIDListItemPairs = "3 mnd gratis,1,3 års registrering betalt i butikk,2,Barnesykkel,3,3 års tyverimerking betalt i butikk,4". 

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ChangeCompany C-Win 
PROCEDURE ChangeCompany :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DYNAMIC-FUNCTION("setAttribute",hBrowse,"basequery","WHERE iJBoxCompanyId = " + STRING(DYNAMIC-FUNCTION("getCompanyId")). */
/*   DYNAMIC-FUNCTION("setCurrentObject",hBrowse).                                                                             */
/*   RUN OpenQuery.                                                                                                            */

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE DisplayRecord C-Win 
PROCEDURE DisplayRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

    RUN SUPER.
Kampanjekode:SENSITIVE = FALSE.
    RUN ValueChangedWidget IN THIS-PROCEDURE.
END.

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

  {&OPEN-QUERY-DEFAULT-FRAME}
  GET FIRST DEFAULT-FRAME.
  DISPLAY KlarForSending Eier_Fornavn Eier_Etternavn Eier_Adresse Eier_PostNr 
          Eier_Mobil Eier_Telefon Eier_epost Foresatt_Fornavn Foresatt_Etternavn 
          Foresatt_Fodt Eier_Poststed Arsmodell Sykkeltype Fabrikat Rammenummer 
          Sec_Nr AvtaleID ButikkNr Kampanjekode ForsNr Dato_Solgt Pris lblEier 
          lblEier-2 lblSykkel lblButikk butnamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE rectToolbar RECT-1 btnCalDatoSalg RECT-2 RECT-3 RECT-4 KlarForSending 
         Eier_Fornavn Eier_Etternavn Eier_Adresse Eier_PostNr Eier_Mobil 
         Eier_Telefon Eier_epost Foresatt_Fornavn Foresatt_Etternavn 
         Foresatt_Fodt btnPostnr Arsmodell Sykkeltype Fabrikat Rammenummer 
         Sec_Nr AvtaleID ButikkNr ForsNr Dato_Solgt Pris btnButikknr 
         btnCalDato3 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Refer to the <jukebox>\winsrc\samples for working examples for Sports2000
------------------------------------------------------------------------------*/
DO WITH FRAME {&FRAME-NAME}:

  cButikkNr =DYNAMIC-FUNCTION("getFieldValues","Bruker",
                              "WHERE BrukerId = '" + USERID('SkoTex') + "'","ButikkNr").
  IF (TRIM(cButikkNr) = '' OR INT(cButikkNr) = 0) THEN
  DO:
      cButikkNr = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                             "WHERE SysHId = 5 and SysGr = 1 and ParaNr = 1","Parameter1").
  END.
  
  DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"container").
                             
  hQuery = DYNAMIC-FUNCTION("NewQuery",1,""
                             ,"Falck_Sykkelregister"
                             + ";Eier_Fornavn"
                             + ";Eier_Etternavn"
                             + ";Eier_Adresse"
                             + ";Eier_PostNr"
                             + ";!Eier_Poststed"
/*                              + ";Eier_Fodt" */
                             + ";Eier_epost"
                             + ";Eier_Mobil"
                             + ";Eier_Telefon"
                             + ";Fabrikat"
                             + ";Sykkeltype"
                             + ";Rammenummer"
                             + ";Arsmodell"
                             + ";Pris"
                             + ";Kjonn"
/*                              + ";DBSNokkelnummer"   */
/*                              + ";DBSKundenummer"    */
/*                              + ";DBSIndividnummer"  */
/*                              + ";DBSArtikkelnummer" */
/*                              + ";!Sec_Nr"            */
                             + ";KlarForSending"
                             + ";AvtaleID"
                             + ";Sec_nr"
/*                              + ";!Kampanjekode2"     */
                             + ";!Kampanjekode"
/*                              + ";!ForhandlerReklame" */
                             + ";Dato_Solgt"
                             + ";ButikkNr"
                             + ";ForsNr"
/*                              + ";!Bulk_Kode"      */
/*                              + ";!Bildereferanse" */
                             + ";Transsaksjonsnumer"
                             + ";BrukerID"
                             + ";!SendtTid"
                             + ";!SendtDato"
/*                              + ";!EksportertTid"   */
/*                              + ";!EksportertDato"  */
                             + ";!RegistrertTid"
                             + ";!RegistrertDato"
                             + ";!EDato"
                             + ";!ETid"
                             + ";!RegistrertAv"
                             + ",post;!postnr;!beskrivelse"
                             + ",butiker;!butik;!butnamn"
                             ," WHERE FALSE, FIRST post no-lock WHERE Post.PostNr = Falck_Sykkelregister.Eier_PostNr"
                                         + ",FIRST Butiker NO-LOCK WHERE Butiker.Butik = Falck_Sykkelregister.ButikkNr "
                             ,"").
   

/*   DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hToolbar). */
  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
  ,hQuery 
  ,FRAME {&FRAME-NAME}:HANDLE
  ,"eier_fornavn,eier_etternavn,eier_adresse,Eier_PostNr,eier_epost,Eier_Telefon,Eier_Mobil,Kjonn,AvtaleID,Sec_nr,EksportertDato,EksportertTid" 
/*   ,"eier_fornavn,eier_etternavn,eier_adresse,Eier_PostNr,eier_epost,eier_fodt,Eier_Telefon,Eier_Mobil,Kjonn,AvtaleID,Sec_nr,EksportertDato,EksportertTid" */
  + ",arsmodell,sykkeltype,fabrikat,rammenummer,butikknr,forsnr,dato_solgt,pris,KlarForSending,Kampanjekode",""
  ,"Eier_Poststed,butnamn",""
  ,"").
  
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hFieldMap).
  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,THIS-PROCEDURE).
  
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"customCreateProc",'create_falck_sykkelregister.p').
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraFields","eier_poststed").

  hToolbar = DYNAMIC-FUNCTION("NewToolbar",
                            rectToolbar:HANDLE,
                            "",
                            "New,Undo,Save,Delete,print,rule"
                            + (IF cParentType = 'Browse' THEN ",first,prev,next,last,rule"
                               ELSE ",StartQuery;&StartSpørring;Start spørring;;bmp/searc16e.bmp"),
                            "maxborder").
 

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,hToolbar).
  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hToolbar).
  RUN InvokeMethod(hQuery,"OpenQuery").
/*   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraFields","Opphav,KundeNr").               */
/*   DYNAMIC-FUNCTION("setAttribute",hFieldMap,"CustomUpdateValProc","=hurtigordre_hode_updval.p"). */


END.

/*   DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).  */

/* When multi-company db, subscribe to the ChangeCompany event: 
SUBSCRIBE TO "ChangeCompany" ANYWHERE.

*/

DYNAMIC-FUNCTION("setNoResizeY",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rectToolbar,rect-1,rect-2,rect-3").
DYNAMIC-FUNCTION("setNoResizeX",THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,"rect-1").
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,600,400,0,0).
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = FALSE.

/* APPLY 'ENTRY' TO Eier_Fornavn IN FRAME Default-Frame. */
Kampanjekode:SENSITIVE = FALSE.
APPLY 'ENTRY' TO KlarForSending IN FRAME Default-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InvalidateHandle C-Win 
PROCEDURE InvalidateHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER ihProgram AS HANDLE NO-UNDO.
DEFINE VAR hTest AS HANDLE NO-UNDO.

DYNAMIC-FUNCTION("setAttribute",hQuery,"basequery","").
/* RUN InvokeMethod(hQuery,"OpenQuery"). */


/* MESSAGE ihProgram SKIP hSearch              */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.      */
/*                                             */
/* hTest = DYNAMIC-FUNCTION("getLinkedObject", */
/*      hQuery,                                */
/*      hParentQuery,                          */
/*      'FROM').                               */
/* MESSAGE hTest                               */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.      */
/* hTest = DYNAMIC-FUNCTION("getLinkedObject", */
/*      hQuery,                                */
/*      hParentQuery,                          */
/*      'TO').                                 */
/* MESSAGE hTest                               */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.      */
/*                                             */
/* IF ihProgram = hSearch THEN                 */
/*   DYNAMIC-FUNCTION("DeleteObjectLink",      */
/*     hQuery,                                 */
/*     hParentQuery).                          */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfFieldHandle C-Win 
PROCEDURE LeaveOfFieldHandle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihField AS HANDLE NO-UNDO.

DEF VAR cReturn AS CHAR NO-UNDO.

IF ihField:MODIFIED THEN 
DO WITH FRAME {&FRAME-NAME}:
  CASE ihField:NAME:
    WHEN "eier_postnr" THEN 
    DO:
      cReturn = DYNAMIC-FUNCTION("getFieldValues","post","WHERE post.postnr = '" + ihField:SCREEN-VALUE + "'","beskrivelse").        
      eier_poststed:SCREEN-VALUE = cReturn.
    END.
  END CASE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MoveToTop C-Win 
PROCEDURE MoveToTop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE newRecord C-Win 
PROCEDURE newRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

RUN SUPER.

    lValueChanged = TRUE.
    RUN ValueChangedWidget IN THIS-PROCEDURE.
ASSIGN
    Dato_Solgt:SCREEN-VALUE IN FRAME default-Frame = STRING(TODAY)
    ButikkNr:SCREEN-VALUE IN FRAME Default-Frame   = cButikkNr
    .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReturnOfWidget C-Win 
PROCEDURE ReturnOfWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEFINE VARIABLE dd AS DATE    NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        CASE FOCUS:NAME:
                WHEN "Eier_Fornavn"   THEN DO:
                      IF Eier_Fornavn:SCREEN-VALUE = "" THEN
                             DYNAMIC-FUNCTION('setWidgetEnter',Eier_Fornavn:HANDLE).
                        RETURN NO-APPLY.
                     END.
                WHEN "Eier_Etternavn" THEN DO:
                         IF Eier_Etternavn:SCREEN-VALUE <> "" THEN
                             IF INPUT KlarForSending = FALSE THEN
                                 DYNAMIC-FUNCTION('setWidgetEnter',Eier_Mobil:HANDLE).
                             ELSE
                                 DYNAMIC-FUNCTION('setWidgetEnter',Eier_Adresse:HANDLE).
                         ELSE
                             DYNAMIC-FUNCTION('setWidgetEnter',Eier_Etternavn:HANDLE).
                         RETURN NO-APPLY.
        
                     END.
                WHEN "Eier_Adresse"   THEN DO:
                         IF Eier_Adresse:SCREEN-VALUE <> "" THEN
                             APPLY "ENTRY" TO Eier_PostNr.
                         RETURN NO-APPLY.
                     END.
                WHEN "Eier_PostNr"    THEN DO:
                     END.
                WHEN "Eier_Mobil"     THEN DO:
                       IF Eier_Mobil:SCREEN-VALUE <> "" THEN DO:
                            IF INPUT KlarForSending = TRUE THEN DO:
                                Eier_Telefon:SCREEN-VALUE = Eier_Mobil:SCREEN-VALUE.
                              APPLY "ENTRY" TO Eier_Telefon.
                            END.
                            ELSE DO:
                                DYNAMIC-FUNCTION('setWidgetEnter',Arsmodell:HANDLE).
                            END.
                       END.
                       ELSE
                           DYNAMIC-FUNCTION('setWidgetEnter',Eier_Mobil:HANDLE).
                    END.
                WHEN "Eier_Telefon"   THEN DO:
                          IF INPUT KlarForSending = TRUE AND Eier_Telefon:SCREEN-VALUE = "" THEN
                              RETURN NO-APPLY.
                          IF Eier_Telefon:SCREEN-VALUE <> "" THEN DO:
                              APPLY "ENTRY" TO Eier_epost.
                              RETURN NO-APPLY.
                          END.
                     END.
                WHEN "Eier_epost"     THEN DO:
/*                          IF INPUT KlarForSending = TRUE AND SELF:SCREEN-VALUE = "" THEN DO: */
                        IF INPUT KlarForSending = TRUE THEN DO:
                             DYNAMIC-FUNCTION('setWidgetEnter',Arsmodell:HANDLE).
                             RETURN NO-APPLY.
                         END.
                     END.
/*                 WHEN "Eier_Fodt"      THEN DO:                          */
/*                          dd = INPUT Eier_Fodt.                          */
/*                          IF INPUT KlarForSending = TRUE AND dd = ? THEN */
/*                              RETURN NO-APPLY.                           */
/*                          APPLY "ENTRY" TO Arsmodell.                    */
/*                          RETURN NO-APPLY.                               */
/*                      END.                                               */
            WHEN "Arsmodell"     THEN DO:
                     IF Arsmodell:SCREEN-VALUE <> "" THEN
                         APPLY "ENTRY" TO Sykkeltype.
                     RETURN NO-APPLY.
                 END.
            WHEN "Sykkeltype" THEN DO:
                IF Sykkeltype:SCREEN-VALUE <> "" THEN
                    APPLY "ENTRY" TO Fabrikat.
                RETURN NO-APPLY.
            END.
            WHEN "Fabrikat" THEN DO:
                IF Fabrikat:SCREEN-VALUE <> "" THEN
                    APPLY "ENTRY" TO Rammenummer.
                RETURN NO-APPLY.
            END.
            WHEN "Rammenummer" THEN DO:
                IF Rammenummer:SCREEN-VALUE <> "" THEN DO:
                    IF INPUT KlarForSending = TRUE THEN
                        DYNAMIC-FUNCTION('setWidgetEnter',Sec_Nr:HANDLE).
                    ELSE
                        DYNAMIC-FUNCTION('setWidgetEnter',ButikkNr:HANDLE).
                END.
                RETURN NO-APPLY.
            END.

            WHEN "AvtaleID" THEN DO:
/*                          IF INPUT KlarForSending = TRUE AND SELF:SCREEN-VALUE = "" THEN DO: */
                         DYNAMIC-FUNCTION('setWidgetEnter',ButikkNr:HANDLE).
                         RETURN NO-APPLY.
                 END.
            WHEN "ButikkNr" THEN DO:
                IF INPUT ButikkNr > 0 THEN DO:
                    FIND butiker WHERE butiker.butik = INPUT ButikkNr NO-LOCK NO-ERROR.
                    IF AVAIL butiker THEN DO:
                        Kampanjekode:SCREEN-VALUE = TRIM(butiker.FalckMedlNr).
                        ASSIGN INPUT Kampanjekode.
                    END.
                    DYNAMIC-FUNCTION('setWidgetEnter',Dato_Solgt:HANDLE).
                    RETURN NO-APPLY.
                END.
            END.
        END CASE.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SaveRecord C-Win 
PROCEDURE SaveRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     IF Eier_Fornavn:SCREEN-VALUE = '' THEN
     DO:
         MESSAGE 'Fornavn må angis'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO Eier_Fornavn.
         RETURN NO-APPLY.
     END.
     IF Eier_Etternavn:SCREEN-VALUE = '' THEN
     DO:
         MESSAGE 'Etternavn må angis'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO Eier_Etternavn.
         RETURN NO-APPLY.
     END.
     IF INPUT KlarForSending = TRUE AND Eier_Adresse:SCREEN-VALUE = '' THEN
     DO:
         MESSAGE 'Adresse må angis'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO Eier_Adresse.
         RETURN NO-APPLY.
     END.
     IF INPUT KlarForSending = TRUE AND Eier_PostNr:SCREEN-VALUE = '' THEN
     DO:
         MESSAGE 'Postnr. må angis'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO Eier_PostNr.
         RETURN NO-APPLY.
     END.
     ELSE DO:
         IF NOT CAN-FIND(Post WHERE Post.PostNr = Eier_PostNr:SCREEN-VALUE) THEN
         DO:
             MESSAGE 'Ugjyldig Postnr'
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'ENTRY' TO Eier_PostNr.
             RETURN NO-APPLY.
         END.
     END.
     IF INPUT KlarForSending = TRUE AND Eier_epost:SCREEN-VALUE = '' THEN
     DO:
         MESSAGE 'Epost. må angis'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO Eier_epost.
         RETURN NO-APPLY.
     END.
/*      IF INPUT KlarForSending = TRUE AND INPUT Eier_Fodt = ? THEN */
/*      DO:                                                         */
/*          MESSAGE 'Fødselsdato må angis'                          */
/*              VIEW-AS ALERT-BOX INFO BUTTONS OK.                  */
/*          APPLY 'ENTRY' TO Eier_Fodt.                             */
/*          RETURN NO-APPLY.                                        */
/*      END.                                                        */
     IF INPUT KlarForSending = TRUE AND Eier_Telefon:SCREEN-VALUE = '' THEN
     DO:
         MESSAGE 'Telefon må angis'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO Eier_Telefon.
         RETURN NO-APPLY.
     END.
     IF Eier_Mobil:SCREEN-VALUE = '' THEN
     DO:
         MESSAGE 'Mobil må angis'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO Eier_Mobil.
         RETURN NO-APPLY.
     END.

     IF Arsmodell:SCREEN-VALUE = '' THEN
     DO:
         MESSAGE 'Årsmodell må angis'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO Arsmodell.
         RETURN NO-APPLY.
     END.
     IF Sykkeltype:SCREEN-VALUE = '' THEN
     DO:
         MESSAGE 'Modell må angis'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO Sykkeltype.
         RETURN NO-APPLY.
     END.
     IF Fabrikat:SCREEN-VALUE = '' THEN
     DO:
         MESSAGE 'Fabrikat må angis'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO Fabrikat.
         RETURN NO-APPLY.
     END.
     IF Rammenummer:SCREEN-VALUE = '' THEN
     DO:
         MESSAGE 'Rammenummer må angis'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO Rammenummer.
         RETURN NO-APPLY.
     END.
     IF INPUT KlarForSending = TRUE AND Sec_Nr:SCREEN-VALUE = "" THEN
     DO:
         MESSAGE 'SECnr må angis ved registrering i Securmark'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO Sec_Nr.
         RETURN NO-APPLY.
     END.
     IF INPUT KlarForSending = TRUE AND AvtaleID:SCREEN-VALUE = "0" THEN
     DO:
         MESSAGE 'AvtaleID må angis ved registrering i Securmark'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO AvtaleID.
         RETURN NO-APPLY.
     END.
     IF INT(ButikkNr:SCREEN-VALUE) = 0 THEN
     DO:
         MESSAGE 'Butikknr. må angis'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO ButikkNr.
         RETURN NO-APPLY.
     END.
     ELSE DO:
         FIND butiker WHERE butiker.butik = INPUT ButikkNr NO-LOCK NO-ERROR.
         IF NOT AVAIL butiker THEN
         DO:
             MESSAGE 'Butikknr finnes ikke'
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'ENTRY' TO ButikkNr.
             RETURN NO-APPLY.
         END.
         ELSE IF TRIM(Butiker.FalckMedlNr) = "" THEN
         DO:
             MESSAGE 'Butikk mangler forhandlerid'
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
             APPLY 'ENTRY' TO ButikkNr.
             RETURN NO-APPLY.
         END.
         ELSE DO:
             Kampanjekode:SCREEN-VALUE = butiker.FalckMedlNr.
             ASSIGN INPUT Kampanjekode.
         END.
     END.
     IF INPUT Dato_Solgt = ? THEN
     DO:
         MESSAGE 'Salgsdato må angis'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO Dato_Solgt.
         RETURN NO-APPLY.
     END.
     IF NOT INPUT Pris > 0 THEN
     DO:
         MESSAGE 'Pris må angis'
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         APPLY 'ENTRY' TO Pris.
         RETURN NO-APPLY.
     END.
/*      IF KlarForSending = TRUE THEN DO: */
/*      END.                              */
     DYNAMIC-FUNCTION("setAttribute",hFieldMap,"BufferExtraValues",eier_poststed:SCREEN-VALUE).
  END.

  RUN SUPER.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE StartQueryRecord C-Win 
PROCEDURE StartQueryRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RUN falck_browse01.w PERSISTEN SET hSearch.
  IF VALID-HANDLE(hSearch) THEN 
  DO:
    DYNAMIC-FUNCTION('setToolbarUpdateable' IN hSearch,FALSE).
    RUN initializeObject IN hSearch.
    SUBSCRIBE TO "InvalidateHandle" IN hSearch.

    hParentQuery = DYNAMIC-FUNCTION("getBrowseHandle" IN hSearch).
    DYNAMIC-FUNCTION("CreateOneToOneLink",hQuery,hParentQuery,"Transsaksjonsnumer").
    RUN MoveToTop IN hSearch.

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedWidget C-Win 
PROCEDURE ValueChangedWidget :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iBGColor AS INTEGER     NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        IF lValueChanged = TRUE OR FOCUS:NAME = "KlarForSending" THEN DO:
                IF INPUT KlarForSending = TRUE THEN
                    iBGColor = 14.
                ELSE
                    iBGColor = ?.

                ASSIGN Eier_Adresse:BGCOLOR = iBGColor
                       Eier_PostNr:BGCOLOR = iBGColor
                       Eier_epost:BGCOLOR = iBGColor
/*                        Eier_Fodt:BGCOLOR = iBGColor */
                       Eier_Telefon:BGCOLOR = iBGColor
                       Sec_nr:BGCOLOR = iBGColor.
                AvtaleID:SENSITIVE = INPUT KlarForSending.
                IF NOT AvtaleID:SENSITIVE THEN
                    AvtaleID:SCREEN-VALUE = "0".
                APPLY "ENTRY" TO Eier_Fornavn.
                lValueChanged = FALSE.
        END.
        ELSE IF FOCUS:NAME = "AvtaleID" THEN DO:
            DYNAMIC-FUNCTION('setWidgetEnter',ButikkNr:HANDLE).
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getQueryHandle C-Win 
FUNCTION getQueryHandle RETURNS HANDLE
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN hQuery.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setParentType C-Win 
FUNCTION setParentType RETURNS LOGICAL
  (INPUT icParentType AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  cParentType = icParentType.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

