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

DEF VAR bOk             AS LOG     NO-UNDO.
DEF VAR ix              AS INT     NO-UNDO.
DEF VAR hBrowse         AS HANDLE  NO-UNDO.
DEF VAR hToolbar        AS HANDLE  NO-UNDO.
DEF VAR iCL             AS INT     NO-UNDO.
DEF VAR iProfilNr       AS INT     NO-UNDO.
DEF VAR cReturnValue    AS CHAR    NO-UNDO.
def var iSkoModus       as int     no-undo.
                                   
DEF VAR hArtBasSok      AS HANDLE  NO-UNDO.
DEF VAR tthTable        AS HANDLE  NO-UNDO.
DEF VAR bhTable         AS HANDLE  NO-UNDO.
DEF VAR hObject         AS HANDLE  NO-UNDO.
DEF VAR fReklamasjonsnr AS DECIMAL NO-UNDO.
DEF VAR cStrKode        AS CHAR NO-UNDO.
DEFINE VARIABLE hSourceProc AS HANDLE      NO-UNDO.
DEFINE VARIABLE lKostnad AS LOGICAL     NO-UNDO.
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ReklamasjonsLogg

/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define QUERY-STRING-DEFAULT-FRAME FOR EACH ReklamasjonsLogg SHARE-LOCK
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH ReklamasjonsLogg SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME ReklamasjonsLogg
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME ReklamasjonsLogg


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS tgFler tgUtskrift ForsNr rsArtikkel Butik ~
tgBonginfo Dato KasseNr BongNr tgKunde cbTTId KundeNr txRegistrering ~
KundeNavn KundeAdresse PostNr KundeE-Mail KundeTelefon btnArtikkelNr ~
KundeMobil Frist-Dato Strekkode ArtikkelNr Vg LopNr LevKod Beskr LevFargKod ~
Storl btnStorl Antall Pris RabKr ReklamUtgifter VVarekost FeilKode ~
AkseptertNotat btnOk btnCancel btnKundeNr btnFristDato btnForsNr btnPostnr ~
btnFeilkode btnButik btnFristDato-2 btnKasseNr NotatKode btnNotatkode ~
RECT-59 RECT-60 
&Scoped-Define DISPLAYED-OBJECTS tgFler tgUtskrift ForsNr rsArtikkel Butik ~
tgBonginfo Dato KasseNr BongNr tgKunde cbTTId KundeNr txRegistrering ~
KundeNavn KundeAdresse PostNr KundeE-Mail KundeTelefon KundeMobil ~
Frist-Dato Strekkode ArtikkelNr Vg LopNr LevKod Beskr LevFargKod Storl ~
Antall Pris RabKr ReklamUtgifter VVarekost FeilKode AkseptertNotat ~
lblInterntBruk FoNamn Poststed FeilkodeBeskrivelse ButNamn NotatKode ~
FeilNotat 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr AS DEC,
    INPUT icStorl      AS CHAR,
    INPUT ifPlukkAnt   AS DEC,
    input icAction     AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnArtikkelNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnButik  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnCancel AUTO-END-KEY 
     LABEL "&Avbryt" 
     SIZE 15 BY 1.14.

DEFINE BUTTON btnFeilkode  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnForsNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnFristDato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnFristDato-2 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnKasseNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnKundeNr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnNotatkode  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnOk 
     LABEL "Lagre / skriv ut" 
     SIZE 18 BY 1.14.

DEFINE BUTTON btnPostnr  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnStorl  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE cbTTId AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 3 
     LABEL "Reklam.type" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Kundereklamasjon",3,
                     "Lagerreklamasjon",4
     DROP-DOWN-LIST
     SIZE 45 BY 1 NO-UNDO.

DEFINE VARIABLE AkseptertNotat AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 63 BY 9.05
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Antall AS DECIMAL FORMAT "-zz,zzz,zz9" INITIAL 0 
     LABEL "Antall" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1.

DEFINE VARIABLE ArtikkelNr AS DECIMAL FORMAT "zzzzzzzzzzzz9" INITIAL 0 
     LABEL "Artikkelnummer" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1.

DEFINE VARIABLE Beskr AS CHARACTER FORMAT "X(30)" 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 32 BY 1.

DEFINE VARIABLE BongNr AS INTEGER FORMAT ">>>>>>>9":U INITIAL 0 
     LABEL "Bongnr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE Butik AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Butikk" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE ButNamn AS CHARACTER FORMAT "x(50)":U 
      VIEW-AS TEXT 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE Dato AS DATE FORMAT "99/99/99" 
     LABEL "Dato" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE FeilKode AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Feilkode" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE FeilkodeBeskrivelse AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 48 BY .95 NO-UNDO.

DEFINE VARIABLE FeilNotat AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 48 BY 1 NO-UNDO.

DEFINE VARIABLE FoNamn AS CHARACTER FORMAT "x(50)":U 
      VIEW-AS TEXT 
     SIZE 29 BY 1 NO-UNDO.

DEFINE VARIABLE ForsNr AS DECIMAL FORMAT "zzzzz9" INITIAL 0 
     LABEL "Kasserernr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1.

DEFINE VARIABLE Frist-Dato AS DATE FORMAT "99/99/99" 
     LABEL "Svarfrist" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE KasseNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Kassenr" 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE KundeAdresse AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE KundeE-Mail AS CHARACTER FORMAT "X(30)" 
     LABEL "E-Mail" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE KundeMobil AS CHARACTER FORMAT "X(15)" 
     LABEL "Mobil" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE KundeNavn AS CHARACTER FORMAT "X(40)" 
     LABEL "Navn" 
     VIEW-AS FILL-IN 
     SIZE 45 BY 1.

DEFINE VARIABLE KundeNr AS DECIMAL FORMAT ">>>>>>>>>>>>9":U INITIAL 0 
     LABEL "Kundenr" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 TOOLTIP "Kundenummer" NO-UNDO.

DEFINE VARIABLE KundeTelefon AS CHARACTER FORMAT "X(15)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1.

DEFINE VARIABLE lblInterntBruk AS CHARACTER FORMAT "X(256)":U INITIAL "Internt bruk:" 
      VIEW-AS TEXT 
     SIZE 62.8 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE LevFargKod AS CHARACTER FORMAT "X(15)" 
     LABEL "LevFargKod" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1.

DEFINE VARIABLE LevKod AS CHARACTER FORMAT "x(20)" 
     LABEL "LevArtNr" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1.

DEFINE VARIABLE LopNr AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.8 BY 1 NO-UNDO.

DEFINE VARIABLE NotatKode AS INTEGER FORMAT ">9" INITIAL 0 
     LABEL "Notatkode" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1.

DEFINE VARIABLE PostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "PostNr" 
     VIEW-AS FILL-IN 
     SIZE 15.6 BY 1.

DEFINE VARIABLE Poststed AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 24 BY .95 NO-UNDO.

DEFINE VARIABLE Pris AS DECIMAL FORMAT "-zz,zzz,zz9.99" INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1.

DEFINE VARIABLE RabKr AS DECIMAL FORMAT "-zz,zzz,zz9.99" INITIAL 0 
     LABEL "Rabatt" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1.

DEFINE VARIABLE ReklamUtgifter AS DECIMAL FORMAT "-zz,zzz,zz9.99" INITIAL 0 
     LABEL "Utgifter" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE Storl AS CHARACTER FORMAT "x(4)" 
     LABEL "Størrelse" 
     VIEW-AS FILL-IN 
     SIZE 9.6 BY 1.

DEFINE VARIABLE Strekkode AS CHARACTER FORMAT "X(200)":U 
     LABEL "Strekkode" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE txRegistrering AS CHARACTER FORMAT "X(256)":U INITIAL "Registrering" 
      VIEW-AS TEXT 
     SIZE 14.2 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE Vg AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     LABEL "Vg/Løpenr" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 NO-UNDO.

DEFINE VARIABLE VVarekost AS DECIMAL FORMAT "-zz,zzz,zz9.99" INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE rsArtikkel AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Alle", 1,
"Strekkode", 2,
"Artikkelnr", 3,
"Vg/Løpenr", 4
     SIZE 30.2 BY 4.29 NO-UNDO.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 35.4 BY 26.91.

DEFINE RECTANGLE RECT-60
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 130 BY 28.43.

DEFINE VARIABLE tgBonginfo AS LOGICAL INITIAL no 
     LABEL "Bonginformasjon" 
     VIEW-AS TOGGLE-BOX
     SIZE 29.2 BY .81 NO-UNDO.

DEFINE VARIABLE tgFler AS LOGICAL INITIAL yes 
     LABEL "Registrer flere" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE tgKunde AS LOGICAL INITIAL yes 
     LABEL "Kundeinformasjon" 
     VIEW-AS TOGGLE-BOX
     SIZE 30.2 BY .81 NO-UNDO.

DEFINE VARIABLE tgUtskrift AS LOGICAL INITIAL yes 
     LABEL "Utskrift kundebilag" 
     VIEW-AS TOGGLE-BOX
     SIZE 29 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY DEFAULT-FRAME FOR 
      ReklamasjonsLogg SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     tgFler AT ROW 2.38 COL 137
     tgUtskrift AT ROW 6.1 COL 137
     ForsNr AT ROW 3.19 COL 18 COLON-ALIGNED HELP
          "Kasserernummer"
     rsArtikkel AT ROW 7.62 COL 136.8 NO-LABEL NO-TAB-STOP 
     Butik AT ROW 2.19 COL 18 COLON-ALIGNED HELP
          "Butikknummer"
     tgBonginfo AT ROW 3.95 COL 137 NO-TAB-STOP 
     Dato AT ROW 4.19 COL 18 COLON-ALIGNED HELP
          "Transaksjonsdato"
     KasseNr AT ROW 5.19 COL 18 COLON-ALIGNED
     BongNr AT ROW 6.19 COL 18 COLON-ALIGNED
     tgKunde AT ROW 5 COL 137 NO-TAB-STOP 
     cbTTId AT ROW 7.71 COL 18 COLON-ALIGNED
     KundeNr AT ROW 8.71 COL 18 COLON-ALIGNED HELP
          "Angi kundenr eller kundenr 0 for tilfeldig kunde.  F10 for søk."
     txRegistrering AT ROW 1.48 COL 141.8 COLON-ALIGNED NO-LABEL
     KundeNavn AT ROW 9.67 COL 18 COLON-ALIGNED HELP
          "Navn på kunde som reklamerer varen."
     KundeAdresse AT ROW 10.62 COL 18 COLON-ALIGNED HELP
          "Adresse til kunden som reklamerte varen."
     PostNr AT ROW 11.57 COL 18 COLON-ALIGNED HELP
          "Postnummer til kunden som reklamerte varen."
     KundeE-Mail AT ROW 12.62 COL 18 COLON-ALIGNED HELP
          "E-Mail adresse til kunde."
     KundeTelefon AT ROW 13.62 COL 18 COLON-ALIGNED
     btnArtikkelNr AT ROW 17.95 COL 44 NO-TAB-STOP 
     KundeMobil AT ROW 13.62 COL 46 COLON-ALIGNED HELP
          "Mobiltelefonnumer til kunden som har reklamert varen."
     Frist-Dato AT ROW 15.19 COL 18 COLON-ALIGNED HELP
          "Svarfrist til kunde. Normalt 14dg. En uke + postgang."
     Strekkode AT ROW 17 COL 18 COLON-ALIGNED
     ArtikkelNr AT ROW 18 COL 18 COLON-ALIGNED
     Vg AT ROW 19 COL 18 COLON-ALIGNED HELP
          "'varegruppenummer"
     LopNr AT ROW 19 COL 29.2 COLON-ALIGNED HELP
          "Løpenummer innenfor varegruppen" NO-LABEL
     LevKod AT ROW 20 COL 18 COLON-ALIGNED HELP
          "Leverandørens artikkelnummer"
     Beskr AT ROW 21 COL 18 COLON-ALIGNED HELP
          "Varetekst - eller kort beskrivelse av varen."
     LevFargKod AT ROW 22 COL 18 COLON-ALIGNED HELP
          "Leverandørens fargekode"
     Storl AT ROW 23 COL 18 COLON-ALIGNED HELP
          "Størrelse"
     btnStorl AT ROW 23 COL 29.6 NO-TAB-STOP 
     Antall AT ROW 24 COL 18 COLON-ALIGNED HELP
          "Antall"
     Pris AT ROW 25 COL 18 COLON-ALIGNED HELP
          "Pris"
     RabKr AT ROW 26 COL 18 COLON-ALIGNED HELP
          "Pris"
     ReklamUtgifter AT ROW 27 COL 18 COLON-ALIGNED HELP
          "Utgifter som er påløpt ved behandling av reklamajson."
     VVarekost AT ROW 27.95 COL 18 COLON-ALIGNED HELP
          "Utgifter som er påløpt ved behandling av reklamajson."
     FeilKode AT ROW 17 COL 66 COLON-ALIGNED HELP
          "Feilkode som beskriver hva som er feil med varen."
     AkseptertNotat AT ROW 5.48 COL 68 NO-LABEL
     btnOk AT ROW 28.86 COL 133
     btnCancel AT ROW 28.86 COL 152
     btnKundeNr AT ROW 8.71 COL 39 NO-TAB-STOP 
     btnFristDato AT ROW 15.19 COL 33.6 NO-TAB-STOP 
     btnForsNr AT ROW 3.19 COL 31.2 NO-TAB-STOP 
     btnPostnr AT ROW 11.62 COL 35.6 NO-TAB-STOP 
     btnFeilkode AT ROW 17 COL 78 NO-TAB-STOP 
     lblInterntBruk AT ROW 4.81 COL 66.2 COLON-ALIGNED NO-LABEL
     FoNamn AT ROW 3.19 COL 34.2 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME DEFAULT-FRAME
     Poststed AT ROW 11.62 COL 38.4 COLON-ALIGNED NO-LABEL
     FeilkodeBeskrivelse AT ROW 17.05 COL 81 COLON-ALIGNED NO-LABEL
     btnButik AT ROW 2.19 COL 31.2 NO-TAB-STOP 
     ButNamn AT ROW 2.19 COL 34.2 COLON-ALIGNED NO-LABEL
     btnFristDato-2 AT ROW 4.19 COL 34.2 NO-TAB-STOP 
     btnKasseNr AT ROW 5.19 COL 34.2 NO-TAB-STOP 
     NotatKode AT ROW 18.24 COL 66 COLON-ALIGNED HELP
          "Feilkode som beskriver hva som er feil med varen."
     btnNotatkode AT ROW 18.29 COL 78 NO-TAB-STOP 
     FeilNotat AT ROW 18.33 COL 81 COLON-ALIGNED NO-LABEL
     RECT-59 AT ROW 1.71 COL 132.8
     RECT-60 AT ROW 1.71 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE .


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
         TITLE              = "Hurtigregistrering av reklamasjon"
         HEIGHT             = 29.33
         WIDTH              = 168.4
         MAX-HEIGHT         = 42.43
         MAX-WIDTH          = 209.2
         VIRTUAL-HEIGHT     = 42.43
         VIRTUAL-WIDTH      = 209.2
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
   FRAME-NAME Size-to-Fit Custom                                        */
ASSIGN 
       FRAME DEFAULT-FRAME:HEIGHT           = 29.43
       FRAME DEFAULT-FRAME:WIDTH            = 168.4.

/* SETTINGS FOR FILL-IN ButNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       ButNamn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FeilkodeBeskrivelse IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FeilkodeBeskrivelse:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FeilNotat IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FeilNotat:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN FoNamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       FoNamn:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN lblInterntBruk IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       lblInterntBruk:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

/* SETTINGS FOR FILL-IN Poststed IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       Poststed:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

ASSIGN 
       txRegistrering:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "skotex.ReklamasjonsLogg"
     _Query            is OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Hurtigregistrering av reklamasjon */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Hurtigregistrering av reklamasjon */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Antall
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Antall C-Win
ON LEAVE OF Antall IN FRAME DEFAULT-FRAME /* Antall */
DO:
/*       RUN LeaveOfField("Antall"). */
/*       RETURN NO-APPLY.            */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Antall C-Win
ON TAB OF Antall IN FRAME DEFAULT-FRAME /* Antall */
OR RETURN OF Antall IN FRAME DEFAULT-FRAME
DO:
    RUN LeaveOfField("Antall").
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtikkelNr C-Win
ON TAB OF ArtikkelNr IN FRAME DEFAULT-FRAME /* Artikkelnummer */
OR RETURN OF ArtikkelNr IN FRAME Default-frame
DO:
  RUN LeaveOfField(SELF:NAME).
  IF Vg:SCREEN-VALUE <> "" THEN
      APPLY "ENTRY" TO Storl.
  IF int(ArtikkelNr:SCREEN-VALUE) > 0 THEN
      RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnArtikkelNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnArtikkelNr C-Win
ON CHOOSE OF btnArtikkelNr IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF artikkelnr
DO:
  IF VALID-HANDLE(hArtBasSok) THEN 
    APPLY "close" TO hArtBasSok.

  RUN ArtBasSok.w PERSIST SET hArtBasSok.
  RUN InitializeObject IN hArtBasSok.
  DYNAMIC-FUNCTION('setCloseOnSelect' IN hArtBasSok,TRUE).
  RUN MoveToTop IN hArtBasSok.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnButik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnButik C-Win
ON CHOOSE OF btnButik IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF Forsnr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "Butik;ButNamn".

  RUN JBoxDLookup.w ("Butiker;Butik;ButNamn", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).
  IF cLookupValue NE "" THEN 
  DO:
    ASSIGN 
      Butik:SCREEN-VALUE   = ENTRY(1,cLookupValue,"|")
      butNamn:SCREEN-VALUE = ENTRY(2,cLookupValue,'|')
    .
    APPLY "ANY-PRINTABLE" TO Butik.
    RUN LeaveOfField("Butik").
    RETURN NO-APPLY.
  END.
  APPLY "entry" TO Butik.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCancel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCancel C-Win
ON CHOOSE OF btnCancel IN FRAME DEFAULT-FRAME /* Avbryt */
DO:
  APPLY 'close' TO THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFeilkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFeilkode C-Win
ON CHOOSE OF btnFeilkode IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF Feilkode
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "FeilKode;Beskrivelse".

  RUN JBoxDLookup.w ("Feilkode;FeilKode;Beskrivelse", 
                     "WHERE Feilkode.FeilKode < 50",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    ASSIGN 
      FeilKode:SCREEN-VALUE            = ENTRY(1,cLookupValue,"|")
      FeilKodeBeskrivelse:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")
    .
    APPLY 'ANY-PRINTABLE' TO FeilKode.
    APPLY "ENTRY" TO Notatkode.
  END.
  ELSE
      APPLY "entry" TO FeilKode.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnForsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnForsNr C-Win
ON CHOOSE OF btnForsNr IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF Forsnr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "ForsNr;ButikkNr;FoNamn".

  RUN JBoxDLookup.w ("Forsalj;ForsNr;ButikkNr;FoNamn", 
                     "WHERE Butikknr = " + Butik:SCREEN-VALUE,
/*                      "WHERE true", */
                     INPUT-OUTPUT cLookupValue).
  IF cLookupValue NE "" THEN 
  DO:
    ASSIGN 
      ForsNr:SCREEN-VALUE = ENTRY(1,cLookupValue,"|")
      Butik:SCREEN-VALUE  = ENTRY(2,cLookupValue,'|')
      FoNamn:SCREEN-VALUE = ENTRY(3,cLookupValue,'|')
    .
/*     cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Butiker','WHERE Butiker.Butik = ' + Butik:SCREEN-VALUE,'Butik,ButNamn'). */
/*     IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN                                                        */
/*         ButNamn:SCREEN-VALUE = "".                                                                                             */
/*     ELSE                                                                                                                       */
/*         ButNamn:SCREEN-VALUE = entry(2,cReturnValue,'|').                                                                      */

    APPLY "ANY-PRINTABLE" TO ForsNr.
    RUN LeaveOfField("ForsNr").
    RETURN NO-APPLY.
  END.
  APPLY "entry" TO ForsNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFristDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFristDato C-Win
ON CHOOSE OF btnFristDato IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF Frist-Dato
DO:
  RUN Cal.w (Frist-Dato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnFristDato-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnFristDato-2 C-Win
ON CHOOSE OF btnFristDato-2 IN FRAME DEFAULT-FRAME /* ... */
OR "F10" OF Dato
DO:
  RUN Cal.w (Dato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKasseNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKasseNr C-Win
ON CHOOSE OF btnKasseNr IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF KasseNr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "KasseNr;Navn".

  RUN JBoxDLookup.w ("Kasse;ButikkNr;KasseNr;Navn", 
                     "WHERE Kasse.ButikkNr = " + Butik:SCREEN-VALUE,
                     INPUT-OUTPUT cLookupValue).
  IF cLookupValue NE "" THEN 
  DO:
    ASSIGN 
      KasseNr:SCREEN-VALUE = ENTRY(1,cLookupValue,"|")
    .
    APPLY "ANY-PRINTABLE" TO KasseNr.
  END.
  APPLY "entry" TO KasseNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnKundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnKundeNr C-Win
ON CHOOSE OF btnKundeNr IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF kundenr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "KundeNr;Navn;Adresse1;PostNr;beskrivelse;ePostAdresse;Telefon;MobilTlf".

  RUN JBoxDLookup.w ("Kunde;Kundenr;Navn;Adresse1;Telefon;MobilTlf;ePostAdresse,Post;postnr;beskrivelse", 
                     "WHERE TRUE" + ',FIRST post OF Kunde NO-LOCK',
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    ASSIGN 
      kundenr:SCREEN-VALUE      = ENTRY(1,cLookupValue,"|")
      kundenavn:SCREEN-VALUE    = ENTRY(2,cLookupValue,"|")
      kundeadresse:SCREEN-VALUE = ENTRY(3,cLookupValue,"|")
      postnr:SCREEN-VALUE       = ENTRY(4,cLookupValue,'|')
      poststed:SCREEN-VALUE     = ENTRY(5,cLookupValue,'|')
      KundeE-Mail:SCREEN-VALUE  = ENTRY(6,cLookupValue,'|')
      KundeTelefon:SCREEN-VALUE = ENTRY(7,cLookupValue,'|')
      KundeMobil:SCREEN-VALUE   = ENTRY(8,cLookupValue,'|')
    .
    APPLY 'ANY-PRINTABLE' TO kundenr.
  END.
  APPLY "entry" TO kundenr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNotatkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNotatkode C-Win
ON CHOOSE OF btnNotatkode IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF Notatkode
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "Notatkode;Beskrivelse".

  RUN JBoxDLookup.w ("Notatkoder;NotatKode;Beskrivelse", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    ASSIGN 
      Notatkode:SCREEN-VALUE            = ENTRY(1,cLookupValue,"|")
      FeilNotat:SCREEN-VALUE = ENTRY(2,cLookupValue,"|")
    .
    APPLY 'ANY-PRINTABLE' TO Notatkode.
  END.
  APPLY "entry" TO FeilKode.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnOk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnOk C-Win
ON CHOOSE OF btnOk IN FRAME DEFAULT-FRAME /* Lagre / skriv ut */
DO:
  DO WITH FRAME Default-Frame:
      RUN doLagre.
      IF RETURN-VALUE <> "" THEN
      DO:
          CASE RETURN-VALUE:
              WHEN 'ForsNr' THEN APPLY 'ENTRY' TO ForsNr.
              WHEN 'KundeNr' THEN APPLY 'ENTRY' TO KundeNr.
              WHEN 'ArtikkelNr' THEN APPLY 'ENTRY' TO ArtikkelNr.
              WHEN 'Frist-Dato' THEN APPLY 'ENTRY' TO Frist-Dato.
              WHEN 'Storl' THEN APPLY 'ENTRY' TO Storl.
              WHEN 'Antall' THEN APPLY 'ENTRY' TO Antall.
              WHEN 'Butik' THEN APPLY 'ENTRY' TO Butik.
              WHEN 'Feilkode' THEN APPLY 'ENTRY' TO Feilkode.
          END CASE.
          RETURN NO-APPLY.
      END.

      /*
      pcRapListe = "Reklamasjon kunde,1," 
                 + "Reklamasjon leverandør,2," 
                 + "Tilgode lapp,3,"
                 + "Reklamasjons liste,4"

      */
      IF tgUtskrift:CHECKED THEN do:
          Run skrivreklamasjonY.p ("1",fReklamasjonsnr).
      end.

      PUBLISH 'refreshBrowse'.

      IF NOT tgFler:CHECKED THEN
          APPLY 'close' TO THIS-PROCEDURE.
      ELSE DO:
          RUN BlankField.
          case int(rsArtikkel:screen-value):
            when 1 then APPLY 'ENTRY' TO Strekkode.
            when 2 then APPLY 'ENTRY' TO Strekkode.
            when 3 then APPLY 'ENTRY' TO ArtikkelNr.
            when 4 then APPLY 'ENTRY' TO Vg.
          end case.
          RETURN NO-APPLY.
      END.

  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPostnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPostnr C-Win
ON CHOOSE OF btnPostnr IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF postnr
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  cLookupValue = "postnr;Beskrivelse".

  RUN JBoxDLookup.w ("Post;postnr;Beskrivelse", 
                     "WHERE true",
                     INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN DO:
    ASSIGN 
      postnr:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
      poststed:SCREEN-VALUE   = ENTRY(2,cLookupValue,"|")
    .
    APPLY 'ANY-PRINTABLE' TO postnr.
/*     IF postnr:MODIFIED AND VALID-HANDLE(hBrowse) THEN RUN InvokeMethod (hBrowse,'OpenQuery'). */
  END.
  APPLY "entry" TO postnr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStorl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStorl C-Win
ON CHOOSE OF btnStorl IN FRAME DEFAULT-FRAME /* ... */
OR F10 OF storl
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.
  DEF VAR iStrTypeId    AS INT  NO-UNDO.

  iStrTypeId = DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.artikkelnr = DEC(' + ArtikkelNr:SCREEN-VALUE + ')','StrTypeId').
  IF iStrTypeId NE 0 AND iStrTypeId NE ? THEN
  DO:
    cLookupValue = "soStorl".
  
    RUN JBoxDLookup.w ("StrType;StrTypeId;KortNavn;Beskrivelse,strTstr;soStorl", 
                       "WHERE StrType.StrTypeId = " + STRING(iStrTypeId) 
                       + ", EACH strTstr OF strType no-lock",
                       INPUT-OUTPUT cLookupValue).
  
    IF cLookupValue NE "" THEN DO:
      ASSIGN 
        storl:SCREEN-VALUE     = ENTRY(1,cLookupValue,"|")
      .
      APPLY 'ANY-PRINTABLE' TO storl.
      RUN LeaveOfField("Storl").
      RETURN NO-APPLY.
    END.
    ELSE APPLY "entry" TO storl.
  END.
  ELSE
    MESSAGE 'Ingen størrelsestype angitt for gjeldene artikkel, vennligst rett dette.'
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Butik C-Win
ON TAB OF Butik IN FRAME DEFAULT-FRAME /* Butikk */
OR RETURN OF Butik IN FRAME DEFAULT-FRAME
DO:
  RUN LeaveOfField(SELF:NAME).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FeilKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FeilKode C-Win
ON TAB OF FeilKode IN FRAME DEFAULT-FRAME /* Feilkode */
OR RETURN OF FeilKode IN FRAME DEFAULT-FRAME
DO:
  RUN LeaveOfField(SELF:NAME).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ForsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ForsNr C-Win
ON TAB OF ForsNr IN FRAME DEFAULT-FRAME /* Kasserernr */
OR RETURN OF ForsNr IN FRAME DEFAULT-FRAME
DO:
  RUN LeaveOfField(SELF:NAME).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KundeNr C-Win
ON TAB OF KundeNr IN FRAME DEFAULT-FRAME /* Kundenr */
OR RETURN OF KundeNr IN FRAME DEFAULT-FRAME
DO:
  RUN LeaveOfField(SELF:NAME).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LopNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LopNr C-Win
ON TAB OF LopNr IN FRAME DEFAULT-FRAME
OR 'RETURN' OF LopNr
DO:
    RUN LeaveOfField(SELF:NAME).
    IF DECI(ArtikkelNr:SCREEN-VALUE) > 0 THEN
    DO:
        APPLY 'ENTRY' TO Storl.
        RETURN NO-APPLY.
    END.
    ELSE DO:
        APPLY 'ENTRY' TO Vg.
        RETURN NO-APPLY.
    END.
/*     ELSE APPLY LASTKEY. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME NotatKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL NotatKode C-Win
ON TAB OF NotatKode IN FRAME DEFAULT-FRAME /* Notatkode */
OR RETURN OF NotatKode IN FRAME DEFAULT-FRAME
DO:
  RUN LeaveOfField(SELF:NAME).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PostNr C-Win
ON TAB OF PostNr IN FRAME DEFAULT-FRAME /* PostNr */
OR RETURN OF PostNr IN FRAME DEFAULT-FRAME
DO:
  RUN LeaveOfField(SELF:NAME).

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ReklamUtgifter
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ReklamUtgifter C-Win
ON TAB OF ReklamUtgifter IN FRAME DEFAULT-FRAME /* Utgifter */
OR 'RETURN' OF ReklamUtgifter
DO:
/*     RUN LeaveOfField(SELF:NAME). */
    IF INPUT ReklamUtgifter > 0 THEN
        APPLY "ENTRY" TO FeilKode.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME rsArtikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL rsArtikkel C-Win
ON VALUE-CHANGED OF rsArtikkel IN FRAME DEFAULT-FRAME
DO:
  RUN ArtikkelRegistrering.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Storl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Storl C-Win
ON TAB OF Storl IN FRAME DEFAULT-FRAME /* Størrelse */
OR RETURN OF Storl IN FRAME DEFAULT-FRAME
DO:
  RUN LeaveOfField(SELF:NAME).
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Strekkode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Strekkode C-Win
ON TAB OF Strekkode IN FRAME DEFAULT-FRAME /* Strekkode */
OR RETURN OF Strekkode
DO:
    RUN LeaveOfField(SELF:NAME).
    IF Storl:SCREEN-VALUE <> "" THEN
        APPLY "ENTRY" TO Antall.
    IF Strekkode:SCREEN-VALUE <> "" THEN
        RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgBonginfo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgBonginfo C-Win
ON VALUE-CHANGED OF tgBonginfo IN FRAME DEFAULT-FRAME /* Bonginformasjon */
DO:
  RUN BongInfo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME tgKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL tgKunde C-Win
ON VALUE-CHANGED OF tgKunde IN FRAME DEFAULT-FRAME /* Kundeinformasjon */
DO:
  RUN KundeInfo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Vg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Vg C-Win
ON TAB OF Vg IN FRAME DEFAULT-FRAME /* Vg/Løpenr */
OR 'RETURN' OF Vg
DO:
    RUN LeaveOfField(SELF:NAME).
    IF Vg:SCREEN-VALUE = "" THEN
        RETURN NO-APPLY.
    ELSE
        RETURN NO-APPLY.
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
  IF VALID-HANDLE(tthTable) THEN DELETE OBJECT tthTable.

  PUBLISH "InvalidateHandle" (THIS-PROCEDURE).
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
hSourceProc = SOURCE-PROCEDURE.
IF CAN-DO(hSourceProc:INTERNAL-ENTRIES,"KostnadRecord") THEN DO:
    lKostnad = DYNAMIC-FUNCTION("Kostnadmodus" IN hSourceProc).
END.
cbTTId:DELIMITER = "|".
IF lKostnad THEN DO:
    cbTTId:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 15 and sysGr = 12 and ParaNr = 62").
    cbTTId = 62.
END.
ELSE DO:
    cbTTId:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","sysPara;ParaNr|Parameter1;ParaNr","where sysHId = 15 and sysGr = 12 and ParaNr <> 62").
    cbTTId = 4.
END.

MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  IF CAN-DO(SOURCE-PROCEDURE:INTERNAL-ENTRIES,"EmbedMe") THEN
    DYNAMIC-FUNCTION("EmbedMe" IN SOURCE-PROCEDURE,THIS-PROCEDURE).
  DO:
/*       tgFler = IF lKostnad THEN FALSE ELSE TRUE. */
      tgKunde = FALSE.
      tgUtskrift = FALSE.
  END.
  RUN enable_UI.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
  &ENDIF
  APPLY "ENTRY" TO Butik.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtikkelRegistrering C-Win 
PROCEDURE ArtikkelRegistrering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME Default-Frame:
    ASSIGN
        Strekkode:SENSITIVE  = can-do("1,2",rsArtikkel:SCREEN-VALUE)
        ArtikkelNr:SENSITIVE = can-do("1,3",rsArtikkel:SCREEN-VALUE)
        Vg:SENSITIVE         = can-do("1,4",rsArtikkel:SCREEN-VALUE)
        LopNr:SENSITIVE      = can-do("1,4",rsArtikkel:SCREEN-VALUE)
        .
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankField C-Win 
PROCEDURE BlankField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME DEFAULT-frame:
    ASSIGN
        Strekkode:SCREEN-VALUE  = ""
        ArtikkelNr:SCREEN-VALUE = ""
        Vg:SCREEN-VALUE         = ""
        LopNr:SCREEN-VALUE      = ""
        LevKod:SCREEN-VALUE     = ""
        Beskr:SCREEN-VALUE  = ""
        LevFargKod:SCREEN-VALUE = ""
        Storl:SCREEN-VALUE      = ""
        Antall:SCREEN-VALUE     = ""
        Pris:SCREEN-VALUE       = ""
        RabKr:SCREEN-VALUE      = ""
        ReklamUtgifter:SCREEN-VALUE = ""
        VVArekost:SCREEN-VALUE  = ""
        Dato:SCREEN-VALUE       = STRING(TODAY)
        Frist-Dato:SCREEN-VALUE = STRING(TODAY + 15)
        KasseNr:SCREEN-VALUE    = ""
        BongNr:SCREEN-VALUE     = ""
        FeilNotat:SCREEN-VALUE  = ""
        FeilKode:SCREEN-VALUE   = ""
        VVarekost:SCREEN-VALUE  = ""
        FeilkodeBeskrivelse:SCREEN-VALUE = ""
        AkseptertNotat:SCREEN-VALUE      = ""
        .
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BongInfo C-Win 
PROCEDURE BongInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME Default-Frame:
    ASSIGN
        Dato:SENSITIVE    = tgBonginfo:CHECKED
        KasseNr:SENSITIVE = tgBonginfo:CHECKED
        BongNr:SENSITIVE  = tgBonginfo:CHECKED
        .
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doLagre C-Win 
PROCEDURE doLagre :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cReturValue AS CHAR NO-UNDO.

  bhTable:EMPTY-TEMP-TABLE().
  bhTable:BUFFER-CREATE().
  
  hObject = FRAME {&FRAME-NAME}:FIRST-CHILD.
  hObject = hObject:FIRST-CHILD.
  

  /* Validering av kasserer. */
  cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Forsalj','WHERE Forsalj.ForsNr = ' + ForsNr:SCREEN-VALUE,'ForsNr,ButikkNr,FoNamn').
  IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
  DO:
    MESSAGE "Ugyldig selger."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN 'Forsnr'.
  END.

  /* Validering av Butikk. */
  cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Butiker','WHERE Butiker.Butik = ' + Butik:SCREEN-VALUE,'Butik,ButNamn').
  IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
  DO:
    MESSAGE "Ugyldig butikk."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN 'Butik'.
  END.
  
  /* Validering av kunde */
  IF INT(Kundenr:SCREEN-VALUE) <> 0 THEN
  DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Kunde','WHERE kunde.Kundenr = ' + kundenr:SCREEN-VALUE,'Kundenr,Navn,Adresse1,PrivatTlf,MobilTlf,ePostAdresse,postnr').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        MESSAGE "Ugyldig kunde."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN 'KundeNr'.
      END.
  END.

  /* Validering av frist dato */
  IF INPUT Frist-Dato = ?  OR INPUT Frist-Dato < TODAY THEN
  DO:
      MESSAGE "Ugyldig frist dato."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN 'Frist-Dato'.
  END.

  /* Validering av artikkel */
  cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.ArtikkelNr = DEC(' + ArtikkelNr:SCREEN-VALUE + ')','ArtikkelNr,LevKod,Beskr,LevFargKod,Vg,LopNr').
  IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
  DO:
    MESSAGE "Ugyldig artikkel."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN 'ArtikkelNr'.
  END.  
  IF INPUT Feilkode = 0 THEN DO:
    MESSAGE "Registrera felkod"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN 'Feilkode'.
  END.
  IF INPUT Notatkode = 0 AND can-find(FIRST notatkoder)THEN DO:
        MESSAGE "Registrera notatkode"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN 'Notatkode'.
  END.

  /* For sikkerhetssyld oppdateres alltid Vg/Lopnr */
  ELSE DO:
      ASSIGN 
          Vg:SCREEN-VALUE    = ENTRY(5,cReturnValue,'|')
          LopNr:SCREEN-VALUE = ENTRY(6,cReturnValue,'|')
          .
  END.

  /* Størrelse */
  IF Storl:SCREEN-VALUE = "" THEN
      RETURN 'Storl'.

  /* Antall */
  IF INPUT Antall <= 0 THEN
      RETURN 'Antall'.

  DO WHILE VALID-HANDLE(hObject):  
    IF CAN-DO('FILL-IN,EDITOR',hObject:TYPE) AND hObject:NAME <> "Notatkode" AND hObject:NAME <> "FeilNotat2" THEN
    DO:
      bhTable:buffer-field(hObject:NAME):BUFFER-VALUE = hObject:SCREEN-VALUE.
    END.
    hObject = hObject:NEXT-SIBLING.
  END.
  bhTable:buffer-field('TTID'):BUFFER-VALUE = cbTTID:SCREEN-VALUE.
  DYNAMIC-FUNCTION("runProc","reklamasjonslogg_hurtig_server.p","",tthTable).
  cReturValue = DYNAMIC-FUNCTION("getTransactionMessage").
  
  /*Retur reklamasjonslogg(rowid))|ReklamasjonsLinje(rowid)|reklamasjonsnr(DECIMAL) */
  fReklamasjonsnr = DECIMAL(ENTRY(3,cReturValue,'|')).
  PUBLISH "refreshByRowid" (cReturValue).

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
  DISPLAY tgFler tgUtskrift ForsNr rsArtikkel Butik tgBonginfo Dato KasseNr 
          BongNr tgKunde cbTTId KundeNr txRegistrering KundeNavn KundeAdresse 
          PostNr KundeE-Mail KundeTelefon KundeMobil Frist-Dato Strekkode 
          ArtikkelNr Vg LopNr LevKod Beskr LevFargKod Storl Antall Pris RabKr 
          ReklamUtgifter VVarekost FeilKode AkseptertNotat lblInterntBruk FoNamn 
          Poststed FeilkodeBeskrivelse ButNamn NotatKode FeilNotat 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE tgFler tgUtskrift ForsNr rsArtikkel Butik tgBonginfo Dato KasseNr 
         BongNr tgKunde cbTTId KundeNr txRegistrering KundeNavn KundeAdresse 
         PostNr KundeE-Mail KundeTelefon btnArtikkelNr KundeMobil Frist-Dato 
         Strekkode ArtikkelNr Vg LopNr LevKod Beskr LevFargKod Storl btnStorl 
         Antall Pris RabKr ReklamUtgifter VVarekost FeilKode AkseptertNotat 
         btnOk btnCancel btnKundeNr btnFristDato btnForsNr btnPostnr 
         btnFeilkode btnButik btnFristDato-2 btnKasseNr NotatKode btnNotatkode 
         RECT-59 RECT-60 
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
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cFormat AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  hObject = FRAME {&FRAME-NAME}:FIRST-CHILD.
  hObject = hObject:FIRST-CHILD.

  cReturnValue = DYNAMIC-FUNCTION('getFieldValues','SysPara','WHERE SysPara.SysHId = 1 and SysPara.SysGr = 1 and SysPara.ParaNr = 54','Parameter1,Parameter2').
  ASSIGN
      iSkoModus = INT(ENTRY(1,cReturnValue,'|')).

  cReturnValue = DYNAMIC-FUNCTION('getFieldValues','SysPara','WHERE SysPara.SysHId = 5 and SysPara.SysGr = 1 and SysPara.ParaNr = 1','Parameter1,Parameter2').
  ASSIGN
      iCL = INT(ENTRY(1,cReturnValue,'|')).
  cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Butiker','WHERE Butiker.Butik = ' + "'" + string(iCL) + "'",'ProfilNr,ButNamn').
  ASSIGN
      iProfilNr = INT(ENTRY(1,cReturnValue,'|')).
  

  CREATE TEMP-TABLE tthTable.

  DO WHILE VALID-HANDLE(hObject):  
    IF CAN-DO('FILL-IN,EDITOR',hObject:TYPE) AND hObject:NAME <> "Notatkode" AND  hObject:NAME <> "FeilNotat2" THEN
    DO:
      cFormat = IF hObject:TYPE = 'EDITOR' THEN 'X(200)' ELSE hObject:FORMAT.
      tthTable:ADD-NEW-FIELD(hObject:NAME,hObject:DATA-TYPE,0,cFormat,"",hObject:NAME).
    END.
    hObject = hObject:NEXT-SIBLING.
  END.
  /* Legger til combo-box feltet */
  tthTable:ADD-NEW-FIELD('TTId','INTEGER',0,'>9','','Rekl.type').

  tthTable:TEMP-TABLE-PREPARE("ttReklamasjonHodeLinje").
  bhTable = tthTable:DEFAULT-BUFFER-HANDLE.
  
  ASSIGN
      LevKod:SENSITIVE     = FALSE
      Beskr:SENSITIVE      = FALSE
      LevFargKod:SENSITIVE = FALSE
      Dato:SCREEN-VALUE       = STRING(TODAY)
      Frist-Dato:SCREEN-VALUE = string(TODAY + 15)
      rsArtikkel:screen-value = if iSkomodus = 1 then '4' else '2'
      .

  RUN BongInfo.
  RUN KundeInfo.
  RUN ArtikkelRegistrering.
END.

DYNAMIC-FUNCTION("setNoMoveY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"storl,btnstorl,antall,pris,rabkr,reklamutgifter,vvarekost").
DYNAMIC-FUNCTION("setNoResizeX", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"rect-59").

DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"AkseptertNotat").
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,480,450,0,0).
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"AkseptertNotat").
DYNAMIC-FUNCTION("initTranslation",THIS-PROCEDURE:CURRENT-WINDOW).
rsArtikkel:screen-value = if iSkomodus = 1 then '4' else '2'.
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
APPLY 'ENTRY' TO Butik.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KundeInfo C-Win 
PROCEDURE KundeInfo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DO WITH FRAME Default-Frame:
    ASSIGN
        KundeNr:SENSITIVE       = tgKunde:CHECKED
        KundeNavn:SENSITIVE     = tgKunde:CHECKED
        KundeAdresse:SENSITIVE  = tgKunde:CHECKED
        PostNr:SENSITIVE        = tgKunde:CHECKED
        btnPostNr:SENSITIVE     = tgKunde:CHECKED
        KundeE-Mail:SENSITIVE   = tgKunde:CHECKED
        KundeTelefon:SENSITIVE  = tgKunde:CHECKED
        KundeMobil:SENSITIVE    = tgKunde:CHECKED
        .
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfField C-Win 
PROCEDURE LeaveOfField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldName AS CHAR NO-UNDO.

DEF VAR cReturnValue AS CHAR NO-UNDO.
DEF VAR fArtNr       AS DEC NO-UNDO.
DEF VAR cKode        AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  CASE icFieldName:
    WHEN 'Strekkode' THEN
    DO:
      IF Strekkode:SCREEN-VALUE <> "" THEN
      DO:
          cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Strekkode','WHERE Strekkode.Kode = ' + "'" + SELF:SCREEN-VALUE + "'",'ArtikkelNr,Kode,Bestillingsnummer,StrKode').
          IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
          DO:
            MESSAGE "Ukjent strekkode."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RUN BlankField.
            APPLY 'ENTRY' TO Strekkode.
            RETURN NO-APPLY.
          END.
          ELSE
          DO:
            ASSIGN 
              ArtikkelNr:SCREEN-VALUE = ENTRY(1,cReturnValue,'|')
              cStrKode                = ENTRY(4,cReturnValue,'|').
            cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.ArtikkelNr = DEC(' + ENTRY(1,cReturnValue,'|') + ')','ArtikkelNr,LevKod,Beskr,LevFargKod,Vg,LopNr').
            RUN VisArtikkel (cReturnValue).
          END.
      END.
    END.
    WHEN 'ArtikkelNr' THEN
    DO:
        IF INT(ArtikkelNr:SCREEN-VALUE) > 0 THEN
        DO:
            cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.ArtikkelNr = DEC(' + ArtikkelNr:SCREEN-VALUE + ')','ArtikkelNr,LevKod,Beskr,LevFargKod,Vg,LopNr').
            IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
            DO:
              MESSAGE "Ugyldig artikkelnummer."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RUN BlankField.
              APPLY "ENTRY" TO ArtikkelNr.
              RETURN NO-APPLY.
              /*
              APPLY 'CHOOSE' TO btnArtikkelNr.
              RETURN NO-APPLY.
              */
            END.
            ELSE
            DO:
              RUN VisArtikkel (cReturnValue).
              APPLY "ENTRY" TO Storl.
              RETURN NO-APPLY.
            END.
        END.
    END.
    WHEN 'Vg' THEN
    DO:
        cReturnValue = DYNAMIC-FUNCTION('getFieldValues','VarGr','WHERE VarGr.Vg = ' + Vg:SCREEN-VALUE,'Vg,VgBeskr').
        IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
        DO:
          MESSAGE "Ugyldig varegruppe eller varegruppe ikke angitt."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RUN BlankField.
        END.
        ELSE
            APPLY "ENTRY" TO LopNr.
    END.
    WHEN 'LopNr' THEN
    DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.Vg = ' + Vg:SCREEN-VALUE + 
                                        ' and ArtBas.LopNr = ' + LopNr:SCREEN-VALUE,'Vg,LopNr,ArtikkelNr').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        MESSAGE "Ugyldig varegruppe/løpenummer." SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN BlankField.
/*         APPLY "ENTRY" TO LopNr. */
        RETURN NO-APPLY.
      END.
      ELSE DO:
          ASSIGN
              ArtikkelNr:SCREEN-VALUE = ENTRY(3,cReturnValue,"|")
              cStrKode     = "".
          cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.ArtikkelNr = DEC(' + ENTRY(3,cReturnValue,'|') + ')','ArtikkelNr,LevKod,Beskr,LevFargKod,Vg,LopNr').
          RUN VisArtikkel (cReturnValue).
          APPLY "ENTRY" TO Storl.
          RETURN NO-APPLY.
      END.
    END.
    WHEN 'feilkode' THEN
    DO:
      IF INT(feilkode:SCREEN-VALUE) < 50 THEN
          cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Feilkode','WHERE FeilKode.feilkode = ' + feilkode:SCREEN-VALUE,'Feilkode,beskrivelse').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        APPLY 'CHOOSE' TO btnFeilkode.
        RETURN NO-APPLY.
      END.
      ELSE
      DO:
        ASSIGN 
          FeilkodeBeskrivelse:SCREEN-VALUE = ENTRY(2,cReturnValue,'|')
        .
        APPLY "ENTRY" TO Notatkode.
      END.

    END.
      WHEN 'Notatkode' THEN
      DO:
        IF notatkode:SCREEN-VALUE = "0" AND NOT CAN-FIND(FIRST notatkoder) THEN
            .
        ELSE DO:
            cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Notatkoder','WHERE Notatkoder.notatkode = ' + notatkode:SCREEN-VALUE,'Notatkode,beskrivelse').
            IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
            DO:
              APPLY 'CHOOSE' TO btnNotatkode.
              RETURN NO-APPLY.
            END.
            ELSE
            DO:
              ASSIGN 
                Feilnotat:SCREEN-VALUE = ENTRY(2,cReturnValue,'|')
              . APPLY "ENTRY" TO btnOk.
                RETURN NO-APPLY.
            END.
        END.

      END.
    WHEN 'storl' THEN 
    DO:
      cReturnValue = storl:SCREEN-VALUE.
      RUN FixStorl IN h_dproclib(INPUT-OUTPUT cReturnValue).
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','strkonv','WHERE strkonv.storl = ' + QUOTER(cReturnValue),'storl').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        APPLY 'CHOOSE' TO btnStorl.
        RETURN NO-APPLY.
      END.
      ELSE
      DO:
        storl:SCREEN-VALUE = cReturnValue.
        IF lKostnad THEN
            APPLY "ENTRY" TO ReklamUtgifter.
        ELSE
            APPLY "ENTRY" TO Antall.
      END.
    END.
    WHEN 'Antall' THEN
    DO:
        IF INPUT Antall <> 0  THEN
          APPLY 'ENTRY' TO FeilKode.
     END.
    WHEN 'Butik' THEN
    DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Butiker','WHERE Butiker.Butik = ' + Butik:SCREEN-VALUE,'Butik,ButNamn').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        APPLY 'CHOOSE' TO btnButik.
        RETURN NO-APPLY.
      END.
      ELSE
      DO:
        ASSIGN 
          ButNamn:SCREEN-VALUE = ENTRY(2,cReturnValue,'|')
        .
        APPLY 'ENTRY' TO ForsNr.
      END.
    END.
    WHEN 'ForsNr' THEN
    DO:
        IF Butik:SCREEN-VALUE = "0" OR NOT CAN-FIND(butiker WHERE butiker.butik = INT(Butik:SCREEN-VALUE)) THEN DO:
            APPLY "ENTRY" TO Butik.
            RETURN NO-APPLY.
        END.
        IF CAN-FIND(butikkforsalj WHERE butikkforsalj.Butik = INT(Butik:SCREEN-VALUE) AND butikkforsalj.ForsNr = INT(ForsNr:SCREEN-VALUE)) THEN DO:
            FIND forsalj WHERE forsalj.forsnr = INT(ForsNr:SCREEN-VALUE) NO-LOCK NO-ERROR.
            IF AVAIL forsalj THEN
                cReturnValue = "||" + forsalj.fonamn.
            ELSE
                cReturnValue = "".
        END.
        ELSE
            cReturnValue = "".
/*       cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Forsalj','WHERE Forsalj.ForsNr = ' + ForsNr:SCREEN-VALUE,'ForsNr,ButikkNr,FoNamn'). */
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        APPLY 'ENTRY' TO ForsNr.  /* btnForsNr. */
        RETURN NO-APPLY.
      END.
      ELSE
      DO:
        ASSIGN 
          FoNamn:SCREEN-VALUE = ENTRY(3,cReturnValue,'|')
/*           Butik:SCREEN-VALUE  = ENTRY(2,cReturnValue,'|') */
        .
        /* Legger opp sentrallager hvis bruker ikke er koblet til butikk. */
        IF int(Butik:SCREEN-VALUE) = 0 THEN
            Butik:SCREEN-VALUE = STRING(iCL).

        cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Butiker','WHERE Butiker.Butik = ' + Butik:SCREEN-VALUE,'Butik,ButNamn').
        IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
            ButNamn:SCREEN-VALUE = "".
        ELSE DO:
            ButNamn:SCREEN-VALUE = ENTRY(2,cReturnValue,'|').
            DO:
                CASE INPUT rsArtikkel:
                    WHEN 1 OR WHEN 2 THEN
                        APPLY 'ENTRY' TO Strekkode.
                    WHEN 3 THEN
                        APPLY 'ENTRY' TO Artikkelnr.
                    WHEN 4 THEN
                        APPLY 'ENTRY' TO Vg.
                END CASE.
                
            END.
        END.
      END.
    END.
    WHEN 'kundenr' THEN
    DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Kunde','WHERE kunde.Kundenr = ' + kundenr:SCREEN-VALUE,'Kundenr,Navn,Adresse1,ePostAdresse,Telefon,MobilTlf,postnr').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        /*
        APPLY 'CHOOSE' TO btnKundenr.
        RETURN NO-APPLY.
        */
      END.
      ELSE
      DO:
        ASSIGN 
          kundenr:SCREEN-VALUE      = ENTRY(1,cReturnValue,"|")
          kundenavn:SCREEN-VALUE    = ENTRY(2,cReturnValue,"|")
          kundeadresse:SCREEN-VALUE = ENTRY(3,cReturnValue,"|")
          KundeE-Mail:SCREEN-VALUE  = ENTRY(4,cReturnValue,'|')   
          KundeTelefon:SCREEN-VALUE = ENTRY(5,cReturnValue,'|')
          KundeMobil:SCREEN-VALUE   = ENTRY(6,cReturnValue,'|')
          postnr:SCREEN-VALUE       = ENTRY(7,cReturnValue,'|')
        .
        cReturnValue = DYNAMIC-FUNCTION('getFieldValues','post','WHERE post.postnr = ' + QUOTER(postnr:SCREEN-VALUE),'postnr,beskrivelse').
        IF cReturnValue NE '' OR cReturnValue NE '?' OR cReturnValue NE ? THEN
        DO:
          ASSIGN 
            postnr:SCREEN-VALUE      = ENTRY(1,cReturnValue,"|")
            poststed:SCREEN-VALUE    = ENTRY(2,cReturnValue,"|")
          .
        END.
      END.
    END.
    WHEN 'postnr' THEN
    DO:
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','post','WHERE post.postnr = ' + QUOTER(postnr:SCREEN-VALUE),'postnr,beskrivelse').
      IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN
      DO:
        APPLY 'CHOOSE' TO btnPostnr.
        RETURN NO-APPLY.
      END.
      ELSE
      DO:
        ASSIGN 
          postnr:SCREEN-VALUE      = ENTRY(1,cReturnValue,"|")
          poststed:SCREEN-VALUE    = ENTRY(2,cReturnValue,"|")
        .
      END.
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
{&WINDOW-NAME}:WINDOW-STATE = 3.
{&WINDOW-NAME}:MOVE-TO-TOP().
APPLY "entry" TO ReklamUtgifter IN FRAME {&FRAME-NAME}.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisArtikkel C-Win 
PROCEDURE VisArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER cReturnValue AS CHAR NO-UNDO.

  DO WITH FRAME default-frame:
      ASSIGN 
        LevKod:SCREEN-VALUE     = ENTRY(2,cReturnValue,'|')
        Beskr:SCREEN-VALUE      = ENTRY(3,cReturnValue,'|')
        LevFargKod:SCREEN-VALUE = ENTRY(4,cReturnValue,'|')
        Vg:SCREEN-VALUE         = ENTRY(5,cReturnValue,'|')
        LopNr:SCREEN-VALUE      = ENTRY(6,cReturnValue,'|')
        Antall:SCREEN-VALUE     = '1'
      .
      IF lKostnad THEN
          Antall:SENSITIVE = FALSE.
      ELSE
          ReklamUtgifter:SENSITIVE = FALSE.
      /* Legger opp størrelse. */
      IF cStrKode <> "" THEN
      DO:
          cReturnValue = DYNAMIC-FUNCTION('getFieldValues','StrKonv','WHERE StrKonv.StrKode = ' + "'" + cStrKode + "'",'StrKode,Storl').
          IF cReturnValue = '' OR cReturnValue = '?' OR cReturnValue = ? THEN. /* Gjør ingenting. */
          ELSE assign
              Storl:SCREEN-VALUE = ENTRY(2,cReturnValue,'|').
      END.
      ELSE
          Storl:SCREEN-VALUE = "".
          
      /* Legger opp varekost */
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Lager','WHERE Lager.ArtikkelNr = DEC(' + ArtikkelNr:SCREEN-VALUE + ') and Lager.Butik = ' + "'" + Butik:SCREEN-VALUE + "'",'ArtikkelNr,VVarekost').
      ASSIGN 
        VVarekost:SCREEN-VALUE = ENTRY(2,cReturnValue,'|').

      /* Legger opp pris */
      cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtPris','WHERE ArtPris.ArtikkelNr = DEC(' + ArtikkelNr:SCREEN-VALUE + ') and ArtPris.ProfilNr = ' + "'" + STRING(iProfilNr) + "'",'ArtikkelNr,Pris[1],VareKost[1]').
      ASSIGN 
        Pris:SCREEN-VALUE = ENTRY(2,cReturnValue,'|').
      IF (dec(VVarekost:SCREEN-VALUE) = 0 OR dec(VVarekost:SCREEN-VALUE) = ?)
          THEN VVarekost:SCREEN-VALUE = ENTRY(3,cReturnValue,'|').
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AddStr C-Win 
FUNCTION AddStr RETURNS LOGICAL
  ( INPUT ifArtikkelNr AS DEC,
    INPUT icStorl      AS CHAR,
    INPUT ifPlukkAnt   AS DEC,
    input icAction     AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose: Legg til eller endre artikkel. Kalles fra artbassok.w 
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR bReturnFocus  AS LOG   NO-UNDO.
  DEF VAR cStrList      AS CHAR  NO-UNDO.
  DEF VAR bOkStr        AS LOG   NO-UNDO.
  DEF VAR cReturnValue  AS CHAR  NO-UNDO.
  DEF VAR iArtNr        AS DEC   NO-UNDO.
  DEF VAR fArtikkelPris AS DEC   NO-UNDO.
  DEF VAR bCurrentFlag  AS LOG   NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      cReturnValue= DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.Artikkelnr = DEC(' + STRING(ifArtikkelNr) + ')','Beskr,levfargkod,levkod,strkode1,Vg,Lopnr')
      ArtikkelNr:SCREEN-VALUE = STRING(ifArtikkelNr)
      Beskr:SCREEN-VALUE      = ENTRY(1,cReturnValue,'|')
      levfargKod:SCREEN-VALUE = ENTRY(2,cReturnValue,'|')
      levkod:SCREEN-VALUE     = ENTRY(3,cReturnValue,'|')
      Vg:SCREEN-VALUE         = ENTRY(5,cReturnValue,'|')
      LopNr:SCREEN-VALUE      = ENTRY(6,cReturnValue,'|')
      storl:SCREEN-VALUE      = icStorl
      antall:SCREEN-VALUE     = IF ifPlukkAnt LE 0 THEN STRING(1) ELSE STRING(ifPlukkAnt)
      /*RabKr:SCREEN-VALUE      = ENTRY(2,cReturnValue,'|')*/
    .
    /*       cReturnValue= DYNAMIC-FUNCTION('getFieldValues','ArtBas','WHERE ArtBas.Artikkelnr = DEC(' + STRING(ifArtikkelNr) + ')','Beskr,levfargkod,levkod,strkode1')
      ArtikkelNr:SCREEN-VALUE = STRING(ifArtikkelNr)
      Beskr:SCREEN-VALUE      = ENTRY(1,cReturnValue,'|')
      levfargKod:SCREEN-VALUE = ENTRY(2,cReturnValue,'|')
      levkod:SCREEN-VALUE     = ENTRY(3,cReturnValue,'|')
      storl:SCREEN-VALUE      = icStorl
      antall:SCREEN-VALUE     = IF ifPlukkAnt LE 0 THEN STRING(1) ELSE STRING(ifPlukkAnt)
      /*RabKr:SCREEN-VALUE      = ENTRY(2,cReturnValue,'|')*/
 */
      
      
      /* Legger opp varekost */
    cReturnValue = DYNAMIC-FUNCTION('getFieldValues','Lager','WHERE Lager.ArtikkelNr = DEC(' + ArtikkelNr:SCREEN-VALUE + ') and Lager.Butik = ' + "'" + Butik:SCREEN-VALUE + "'",'ArtikkelNr,VVarekost').
    ASSIGN 
      VVarekost:SCREEN-VALUE = ENTRY(2,cReturnValue,'|').

    /* Legger opp pris. */
    cReturnValue = DYNAMIC-FUNCTION('getFieldValues','ArtPris','WHERE ArtPris.ArtikkelNr = DEC(' + STRING(ifArtikkelNr) + ')'
                                                               + ' AND ArtPris.ProfilNr   = ' + STRING(1) ,'pris;1,Rab1Kr;1,Varekost;1').
    assign
        pris:SCREEN-VALUE = ENTRY(1,cReturnValue,'|').

    IF dec(VVarekost:SCREEN-VALUE) = 0
        THEN VVarekost:SCREEN-VALUE = ENTRY(3,cReturnValue,'|').

    APPLY 'ENTRY' TO artikkelnr.
    RETURN bOk.
  END. /*FRAME*/

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

