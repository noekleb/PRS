&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
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

DEF VAR bOk           AS LOG    NO-UNDO.
DEF VAR ix            AS INT    NO-UNDO.
DEF VAR hBrowse       AS HANDLE NO-UNDO.
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hToolbar      AS HANDLE NO-UNDO.
DEF VAR hFieldMap     AS HANDLE NO-UNDO.
DEF VAR hFmLinje      AS HANDLE NO-UNDO.
DEF VAR hTbLinje      AS HANDLE NO-UNDO.
DEF VAR hArtBasSok    AS HANDLE NO-UNDO.
DEF VAR cProfilNr     AS CHAR   NO-UNDO.
DEF VAR hParent       AS HANDLE NO-UNDO.
DEF VAR bUndoIsDelete AS LOG    NO-UNDO.
DEF VAR bModRabatt    AS LOG    NO-UNDO.
DEF VAR cKundeNr      AS CHAR   NO-UNDO.
DEF VAR bNoSearchBtn  AS LOG    NO-UNDO.
DEF VAR hFilter       AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS ButikkNr ForsNr AvdelingNr Navn Adresse1 ~
PostNr PostSted ePostAdresse Telefon MobilTlf LevStatus SvarFrist ~
Leveringsdato cBefrakter fFrakt btnSvarFrist InternMerknad RegistrertDato ~
RegistrertAv VareNr Varetekst Storl btnPostnr Antall NettoPris LinjeRab% ~
Pris LevKod Vg LopNr LevFargKod VareKost NettoLinjesum btnLeveringsdato ~
MerknadLabel btnVareNr btnStorl fiSumOrdreEksMva btnNavn KOrdre_Id ~
tbKasseOrdreHode brwKasseOrdreLinjer tbKasseOrdreLinjer 
&Scoped-Define DISPLAYED-OBJECTS ButikkNr ForsNr AvdelingNr Navn Adresse1 ~
PostNr PostSted ePostAdresse Telefon MobilTlf LevStatus SvarFrist ~
Leveringsdato cBefrakter fFrakt InternMerknad RegistrertDato RegistrertAv ~
VareNr Varetekst Storl Antall NettoPris LinjeRab% Pris LevKod Vg LopNr ~
LevFargKod VareKost NettoLinjesum MerknadLabel fiSumOrdre fiSumOrdreEksMva ~
KOrdre_Id 

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icBrowseName AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getMyQueryStat C-Win 
FUNCTION getMyQueryStat RETURNS CHARACTER
  ( INPUT ihDataObject    AS HANDLE,
    INPUT icStatFields    AS CHAR,
    INPUT icCriteria      AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getStat C-Win 
FUNCTION getStat RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setNoSearchBtn C-Win 
FUNCTION setNoSearchBtn RETURNS LOGICAL
  ( INPUT ibNoSearchBtn AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD setVarenr C-Win 
FUNCTION setVarenr RETURNS LOGICAL
  ( INPUT icVareNr AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnLeveringsdato 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnNavn 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnPostnr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnStorl 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE BUTTON btnSvarFrist 
     IMAGE-UP FILE "gif/calendar.gif":U NO-FOCUS FLAT-BUTTON
     LABEL "..." 
     SIZE 3.8 BY 1.

DEFINE BUTTON btnVareNr 
     LABEL "..." 
     SIZE 4 BY 1.

DEFINE VARIABLE AvdelingNr AS CHARACTER FORMAT "X(256)" INITIAL "0" 
     LABEL "Avdeling" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 TOOLTIP "Avdeling".

DEFINE VARIABLE ButikkNr AS CHARACTER FORMAT "X(256)" 
     LABEL "&Butikk" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 TOOLTIP "Butikknr" NO-UNDO.

DEFINE VARIABLE cBefrakter AS CHARACTER FORMAT "X(256)":U 
     LABEL "Befrakter" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 27.6 BY 1 NO-UNDO.

DEFINE VARIABLE ForsNr AS CHARACTER FORMAT "X(256)":U 
     LABEL "Kasserer" 
     VIEW-AS COMBO-BOX INNER-LINES 25
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 36 BY 1 NO-UNDO.

DEFINE VARIABLE LevStatus AS CHARACTER FORMAT "X(256)":U 
     LABEL "Ordrestatus" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Item 1","Item 1"
     DROP-DOWN-LIST
     SIZE 27.6 BY 1 NO-UNDO.

DEFINE VARIABLE InternMerknad AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 67 BY 4.29 TOOLTIP "Intern merknad".

DEFINE VARIABLE Adresse1 AS CHARACTER FORMAT "X(40)" 
     LABEL "Adresse" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Kundens adresse".

DEFINE VARIABLE Antall AS DECIMAL FORMAT "->>,>>9" INITIAL 0 
     LABEL "Antall" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Antall".

DEFINE VARIABLE ePostAdresse AS CHARACTER FORMAT "X(40)" 
     LABEL "E-Post" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "E-Post adresse".

DEFINE VARIABLE fFrakt AS DECIMAL FORMAT "->>,>>9.99":U INITIAL 0 
     LABEL "Sum frakt" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 NO-UNDO.

DEFINE VARIABLE fiSumOrdre AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Totalsum" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 NO-UNDO.

DEFINE VARIABLE fiSumOrdreEksMva AS DECIMAL FORMAT "->>>,>>9.99":U INITIAL 0 
     LABEL "Sum eks.mva" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 NO-UNDO.

DEFINE VARIABLE KOrdre_Id AS DECIMAL FORMAT ">>>>>>>>>>>>9" INITIAL 0 
     LABEL "Nr" 
     VIEW-AS FILL-IN 
     SIZE 20.2 BY 1 TOOLTIP "Internt faktura id. Tildeles autmatisk av systemet.".

DEFINE VARIABLE Leveringsdato AS DATE FORMAT "99/99/9999" 
     LABEL "Lev.dato" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Leveringsdato".

DEFINE VARIABLE LevFargKod AS CHARACTER FORMAT "X(15)" 
     LABEL "LevFargKod" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 TOOLTIP "Leverandørens fargekode".

DEFINE VARIABLE LevKod AS CHARACTER FORMAT "x(20)" 
     LABEL "LevArtNr" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 TOOLTIP "Leverandørens artikkelnummer".

DEFINE VARIABLE LinjeRab% AS DECIMAL FORMAT "->>>,>>9.9" INITIAL 0 
     LABEL "Rabatt%" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Rabattbeløp".

DEFINE VARIABLE LopNr AS INTEGER FORMAT "zzz9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 7.6 BY 1 TOOLTIP "Løpenummer innenfor varegruppen".

DEFINE VARIABLE MerknadLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Intern merknad:" 
      VIEW-AS TEXT 
     SIZE 18 BY .62 NO-UNDO.

DEFINE VARIABLE MobilTlf AS CHARACTER FORMAT "X(15)" 
     LABEL "Mobil" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Mobiltelefon".

DEFINE VARIABLE Navn AS CHARACTER FORMAT "X(40)" 
     LABEL "Kundenavn" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1 TOOLTIP "Navn eller firmanavn".

DEFINE VARIABLE NettoLinjesum AS DECIMAL FORMAT "->>>,>>9.99" INITIAL 0 
     LABEL "Sum" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Linjesum eks. mva".

DEFINE VARIABLE NettoPris AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Nto.pris" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Pris (Til kunde)".

DEFINE VARIABLE PostNr AS CHARACTER FORMAT "X(10)" 
     LABEL "PostNr" 
     VIEW-AS FILL-IN 
     SIZE 13.2 BY 1.

DEFINE VARIABLE PostSted AS CHARACTER FORMAT "X(30)" 
     VIEW-AS FILL-IN 
     SIZE 24.6 BY 1.

DEFINE VARIABLE Pris AS DECIMAL FORMAT "->>,>>9.99" INITIAL 0 
     LABEL "Pris" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 TOOLTIP "Pris (Til kunde)".

DEFINE VARIABLE RegistrertAv AS CHARACTER FORMAT "X(10)" 
     LABEL "Av" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Brukerid på den som registrerte posten".

DEFINE VARIABLE RegistrertDato AS DATE FORMAT "99/99/9999" 
     LABEL "Registrert" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Dato da posten ble registrert i registeret".

DEFINE VARIABLE Storl AS CHARACTER FORMAT "x(10)" 
     LABEL "Størrelse" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Størrelse".

DEFINE VARIABLE SvarFrist AS DATE FORMAT "99/99/9999" 
     LABEL "Svarfrist" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1.

DEFINE VARIABLE Telefon AS CHARACTER FORMAT "X(15)" 
     LABEL "Telefon" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Telefon".

DEFINE VARIABLE VareKost AS DECIMAL FORMAT "->,>>>,>>9.99" INITIAL 0 
     LABEL "Varekost" 
     VIEW-AS FILL-IN 
     SIZE 18 BY 1 TOOLTIP "Varekost".

DEFINE VARIABLE VareNr AS CHARACTER FORMAT "X(20)" 
     LABEL "VareNr" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1 TOOLTIP "Varenummer".

DEFINE VARIABLE Varetekst AS CHARACTER FORMAT "X(256)" 
     LABEL "Varetekst" 
     VIEW-AS FILL-IN 
     SIZE 56.8 BY 1 TOOLTIP "Varetekst".

DEFINE VARIABLE Vg AS INTEGER FORMAT "zzzzz9" INITIAL 0 
     LABEL "Vg/L.Nr" 
     VIEW-AS FILL-IN 
     SIZE 10.4 BY 1 TOOLTIP "'varegruppenummer".

DEFINE RECTANGLE brwKasseOrdreLinjer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 7.38.

DEFINE RECTANGLE tbKasseOrdreHode
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.

DEFINE RECTANGLE tbKasseOrdreLinjer
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.8 BY .91.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     ButikkNr AT ROW 2.67 COL 12.4 COLON-ALIGNED HELP
          "Butikknr"
     ForsNr AT ROW 3.71 COL 12.4 COLON-ALIGNED
     AvdelingNr AT ROW 4.76 COL 12.4 COLON-ALIGNED
     Navn AT ROW 5.86 COL 12.4 COLON-ALIGNED
     Adresse1 AT ROW 6.95 COL 12.4 COLON-ALIGNED
     PostNr AT ROW 8.05 COL 12.4 COLON-ALIGNED HELP
          "Postnummer"
     PostSted AT ROW 8.05 COL 29.8 COLON-ALIGNED HELP
          "Poststed" NO-LABEL
     ePostAdresse AT ROW 9.14 COL 12.4 COLON-ALIGNED
     Telefon AT ROW 10.24 COL 12.4 COLON-ALIGNED
     MobilTlf AT ROW 10.24 COL 37.4 COLON-ALIGNED
     LevStatus AT ROW 2.67 COL 64.6 COLON-ALIGNED
     SvarFrist AT ROW 3.71 COL 64.6 COLON-ALIGNED
     Leveringsdato AT ROW 4.76 COL 64.6 COLON-ALIGNED
     cBefrakter AT ROW 3.71 COL 99.6 COLON-ALIGNED
     fFrakt AT ROW 4.81 COL 99.6 COLON-ALIGNED
     btnSvarFrist AT ROW 3.71 COL 82.6 NO-TAB-STOP 
     InternMerknad AT ROW 6.95 COL 62 NO-LABEL
     RegistrertDato AT ROW 12.24 COL 12.2 COLON-ALIGNED
     RegistrertAv AT ROW 12.24 COL 37.6 COLON-ALIGNED
     VareNr AT ROW 15.1 COL 70.4 COLON-ALIGNED
     Varetekst AT ROW 16.14 COL 70.4 COLON-ALIGNED
     Storl AT ROW 17.19 COL 70.4 COLON-ALIGNED
     btnPostnr AT ROW 8.05 COL 27.6 NO-TAB-STOP 
     Antall AT ROW 18.24 COL 70.4 COLON-ALIGNED
     NettoPris AT ROW 19.29 COL 70.4 COLON-ALIGNED
     LinjeRab% AT ROW 20.33 COL 70.4 COLON-ALIGNED HELP
          "Rabattbeløp"
     Pris AT ROW 21.38 COL 109.2 COLON-ALIGNED
     LevKod AT ROW 18.24 COL 109.2 COLON-ALIGNED
     Vg AT ROW 17.19 COL 109.2 COLON-ALIGNED
     LopNr AT ROW 17.19 COL 119.6 COLON-ALIGNED NO-LABEL
     LevFargKod AT ROW 19.29 COL 109.2 COLON-ALIGNED
     VareKost AT ROW 20.33 COL 109.2 COLON-ALIGNED
     NettoLinjesum AT ROW 21.38 COL 70.4 COLON-ALIGNED
     btnLeveringsdato AT ROW 4.76 COL 82.6 NO-TAB-STOP 
     MerknadLabel AT ROW 6.1 COL 60 COLON-ALIGNED NO-LABEL
     btnVareNr AT ROW 15.1 COL 88.4 NO-TAB-STOP 
     btnStorl AT ROW 17.19 COL 88.4 NO-TAB-STOP 
     fiSumOrdre AT ROW 12.24 COL 70.8 COLON-ALIGNED NO-TAB-STOP 
     fiSumOrdreEksMva AT ROW 12.24 COL 109.2 COLON-ALIGNED NO-TAB-STOP 
     btnNavn AT ROW 5.86 COL 56.4 NO-TAB-STOP 
     KOrdre_Id AT ROW 2.67 COL 99.6 COLON-ALIGNED
     tbKasseOrdreHode AT ROW 1.24 COL 2.2
     brwKasseOrdreLinjer AT ROW 15.14 COL 2
     tbKasseOrdreLinjer AT ROW 13.95 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 129.8 BY 22.1.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Ordreregistrering / behandling"
         HEIGHT             = 22.1
         WIDTH              = 129.8
         MAX-HEIGHT         = 23.52
         MAX-WIDTH          = 129.8
         VIRTUAL-HEIGHT     = 23.52
         VIRTUAL-WIDTH      = 129.8
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
  NOT-VISIBLE,,RUN-PERSISTENT                                           */
/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FILL-IN fiSumOrdre IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       fiSumOrdreEksMva:READ-ONLY IN FRAME DEFAULT-FRAME        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Ordreregistrering / behandling */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Ordreregistrering / behandling */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnLeveringsdato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnLeveringsdato C-Win
ON CHOOSE OF btnLeveringsdato IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN Cal.w (Leveringsdato:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnNavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnNavn C-Win
ON CHOOSE OF btnNavn IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,200,
                    "Kunde"
                    + ";Navn"
                    + ";KundeNr"
                    + ";Adresse1"
                    + ";Postnr"
                    + ";ePostAdresse"
                    + ";Telefon"
                    + ";MobilTlf"
                  + ",Post"
                    + ";Beskrivelse@5"
                   ,"WHERE true,FIRST Post NO-LOCK OF Kunde OUTER-JOIN"
                    ,""                                                  
                    ,"Navn,KundeNr,Adresse1,PostNr,ePostAdresse,Telefon,MobilTlf,Beskrivelse",   /* <- return values for these fields */
                    OUTPUT cReturnValues,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN Navn:SCREEN-VALUE         = ENTRY(1,cReturnValues,"|")
           cKundeNr                  = ENTRY(2,cReturnValues,"|")
           Adresse1:SCREEN-VALUE     = ENTRY(3,cReturnValues,"|")
           Postnr:SCREEN-VALUE       = ENTRY(4,cReturnValues,"|")
           ePostAdresse:SCREEN-VALUE = ENTRY(5,cReturnValues,"|")
           Telefon:SCREEN-VALUE      = ENTRY(6,cReturnValues,"|")
           MobilTlf:SCREEN-VALUE     = ENTRY(7,cReturnValues,"|")
           PostSted:SCREEN-VALUE     = ENTRY(8,cReturnValues,"|")
           .

    APPLY "any-printable" TO Navn.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnPostnr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnPostnr C-Win
ON CHOOSE OF btnPostnr IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cZipCodeFieldList    AS CHAR NO-UNDO.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,50,
                    "Post"
                     + ";Beskrivelse"
                     + ";Postnr"
                     ,
                   "WHERE false"
                    ,""
                    ,"Beskrivelse,Postnr",
                    OUTPUT cZipCodeFieldList,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cZipCodeFieldList NE "" THEN DO:
    ASSIGN 
       PostSted:SCREEN-VALUE   = ENTRY(1,cZipCodeFieldList,"|")
       PostNr:SCREEN-VALUE     = ENTRY(2,cZipCodeFieldList,"|")
       .
    APPLY "any-printable" TO Postnr.
    APPLY "entry" TO Telefon.
  END.
  ELSE APPLY "entry" TO PostNr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnStorl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnStorl C-Win
ON CHOOSE OF btnStorl IN FRAME DEFAULT-FRAME /* ... */
DO:
  DEF VAR cReturnValues   AS CHAR NO-UNDO.
  DEF VAR bOk             AS LOG  NO-UNDO.

  IF DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE ArtikkelNr = " + VareNr:SCREEN-VALUE,"ArtikkelNr") = ? THEN RETURN NO-APPLY.

  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = FALSE.
  RUN JBoxLookup.w (THIS-PROCEDURE,200,
                    "ArtBas"
                    + ";!StrTypeId"
                  + ",StrTStr"
                    + ";SoStorl"
                   ,"WHERE ArtikkelNr = " + VareNr:SCREEN-VALUE
                  + ",EACH StrTStr WHERE StrTStr.StrTypeId = ArtBas.StrTypeId"
                    ,""                                                  
                    ,"SoStorl",   /* <- return values for these fields */
                    OUTPUT cReturnValues,
                    OUTPUT bOK).
  THIS-PROCEDURE:CURRENT-WINDOW:SENSITIVE = TRUE.

  IF bOk AND cReturnValues NE "" THEN DO:
    ASSIGN Storl:SCREEN-VALUE = TRIM(ENTRY(1,cReturnValues,"|"))
           .

    APPLY "any-printable" TO Storl.
  END.
  THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSvarFrist
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSvarFrist C-Win
ON CHOOSE OF btnSvarFrist IN FRAME DEFAULT-FRAME /* ... */
DO:

  RUN Cal.w (SvarFrist:HANDLE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnVareNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnVareNr C-Win
ON CHOOSE OF btnVareNr IN FRAME DEFAULT-FRAME /* ... */
DO:
  IF NOT VALID-HANDLE(hArtBasSok) THEN DO:
    RUN ArtBasSok.w PERSIST SET hArtBasSok.
    DYNAMIC-FUNCTION("setButikkNr" IN hArtBasSok,hFieldMap:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).
    DYNAMIC-FUNCTION("setOrdreId"  IN hArtBasSok,hFieldMap:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE).
    /*DYNAMIC-FUNCTION("setCloseOnSelect" IN hArtBasSok,YES).*/

    RUN InitializeObject IN hArtBasSok.

    IF AvdelingNr:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE ? THEN
      DYNAMIC-FUNCTION("setAvdNr" IN hArtBasSok,AvdelingNr:SCREEN-VALUE,NO).
  END.

  /* getStat().  */

  RUN MoveToTop IN hArtBasSok.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ButikkNr C-Win
ON VALUE-CHANGED OF ButikkNr IN FRAME DEFAULT-FRAME /* Butikk */
DO:
  ASSIGN ForsNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" 
                                + DYNAMIC-FUNCTION("getFieldList",
                                                   "butikkforsalj,forsalj;forsnr|Fonamn;forsnr",
                                                   "where " 
                                                 + (IF ButikkNr:SCREEN-VALUE NE "0" AND ButikkNr:SCREEN-VALUE NE ? THEN
                                                     "butik = " + ButikkNr:SCREEN-VALUE 
                                                    ELSE "true")
                                                 + ",first forsalj NO-LOCK of butikkforsalj")
                                            ,"|")
         ForsNr:SCREEN-VALUE = "0"
         .                                                
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ForsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ForsNr C-Win
ON VALUE-CHANGED OF ForsNr IN FRAME DEFAULT-FRAME /* Kasserer */
DO:
  RUN InvokeMethod(hBrowse,"OpenQuery").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Navn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Navn C-Win
ON F3 OF Navn IN FRAME DEFAULT-FRAME /* Kundenavn */
OR f10 OF Navn DO:
  APPLY "choose" TO btnNavn.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME PostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL PostNr C-Win
ON F10 OF PostNr IN FRAME DEFAULT-FRAME /* PostNr */
OR f3 OF Postnr DO:
  APPLY "choose" TO btnPostnr.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Storl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Storl C-Win
ON F3 OF Storl IN FRAME DEFAULT-FRAME /* Størrelse */
OR f10 OF Storl DO:
  APPLY "choose" TO btnStorl.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME VareNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL VareNr C-Win
ON F3 OF VareNr IN FRAME DEFAULT-FRAME /* VareNr */
OR f10 OF VareNr DO:
  APPLY "choose" TO btnVarenr.
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
  IF VALID-HANDLE(hArtBasSok) THEN APPLY "close" TO hArtBasSok.
  IF VALID-HANDLE(hFilter) THEN APPLY "close" TO hFilter.
  RUN disable_UI.
END.

{incl/wintrigg.i}

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  hParent = SOURCE-PROCEDURE.

  &IF DEFINED(UIB_is_Running) NE 0 &THEN
    RUN InitializeObject.
    RUN MoveToTop.
  &ENDIF

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

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
IF DYNAMIC-FUNCTION("getCurrentObject") = hQuery THEN DO WITH FRAME {&FRAME-NAME}:
  IF hFieldMap:AVAIL THEN DO:
    ButikkNr:SCREEN-VALUE = STRING(hFieldMap:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).
    RUN ValueChangedField("ButikkNr").
    ASSIGN ButikkNr:MODIFIED = NO
           ForsNr:MODIFIED = NO.
  END.
END.
ELSE DO:
  DYNAMIC-FUNCTION("setAttribute",hTbLinje,"enabledEvents","HentArt").
END.

RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hBrowse THEN DO:
  btnStorl:SENSITIVE IN FRAME {&FRAME-NAME} = hFmLinje:AVAIL AND VareNr:SCREEN-VALUE NE "".
  DYNAMIC-FUNCTION("setAttribute",hTbLinje,"enabledEvents","").  
END.
ELSE DO:
  getStat().
  cKundeNr = IF hFieldMap:AVAIL THEN STRING(hFieldMap:BUFFER-FIELD("KundeNr"):BUFFER-VALUE) ELSE "".
END. 

ASSIGN bUndoIsDelete = NO
       bModRabatt    = NO
       .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EmailRecord C-Win 
PROCEDURE EmailRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cMessage AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF ePostAdresse:SCREEN-VALUE = "" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Epost adresse er ikke utfylt","","").
    RETURN.
  END.
  cMessage = "Hei, din ordre er klar for henting." + CHR(10) 
           +  "Vennlig hilsen " 
           + REPLACE(DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + ButikkNr:SCREEN-VALUE,"ButNamn,BuAdr")
                    ,"|",", ").
  RUN SendEmail.w (ePostAdresse:SCREEN-VALUE,
                 "Ordrenr: " + KOrdre_Id:SCREEN-VALUE,
                 cMessage,
                 OUTPUT bOK,
                 OUTPUT cMessage).
  IF bOK THEN DO:
    InternMerknad:SCREEN-VALUE = cMessage + CHR(10) + InternMerknad:SCREEN-VALUE.
    IF LevStatus:SCREEN-VALUE < "30" THEN DO:
      IF DYNAMIC-FUNCTION("DoMessage",0,4,"Sett ordrestatus til sendt?","","") = 6 THEN
        LevStatus:SCREEN-VALUE = "20".
    END.
    RUN InvokeMethod(hFieldMap,"SaveRecord").
  END.
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
  DISPLAY ButikkNr ForsNr AvdelingNr Navn Adresse1 PostNr PostSted ePostAdresse 
          Telefon MobilTlf LevStatus SvarFrist Leveringsdato cBefrakter fFrakt 
          InternMerknad RegistrertDato RegistrertAv VareNr Varetekst Storl 
          Antall NettoPris LinjeRab% Pris LevKod Vg LopNr LevFargKod VareKost 
          NettoLinjesum MerknadLabel fiSumOrdre fiSumOrdreEksMva KOrdre_Id 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE ButikkNr ForsNr AvdelingNr Navn Adresse1 PostNr PostSted ePostAdresse 
         Telefon MobilTlf LevStatus SvarFrist Leveringsdato cBefrakter fFrakt 
         btnSvarFrist InternMerknad RegistrertDato RegistrertAv VareNr 
         Varetekst Storl btnPostnr Antall NettoPris LinjeRab% Pris LevKod Vg 
         LopNr LevFargKod VareKost NettoLinjesum btnLeveringsdato MerknadLabel 
         btnVareNr btnStorl fiSumOrdreEksMva btnNavn KOrdre_Id tbKasseOrdreHode 
         brwKasseOrdreLinjer tbKasseOrdreLinjer 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentArtRecord C-Win 
PROCEDURE HentArtRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT hFieldMap:AVAILABLE THEN RETURN.

IF NOT VALID-HANDLE(hArtBasSok) THEN DO:
  RUN ArtBasSok.w PERSIST SET hArtBasSok.
  DYNAMIC-FUNCTION("setButikkNr" IN hArtBasSok,hFieldMap:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE).
  DYNAMIC-FUNCTION("setOrdreId"  IN hArtBasSok,hFieldMap:BUFFER-FIELD("KOrdre_Id"):BUFFER-VALUE).
  /*DYNAMIC-FUNCTION("setCloseOnSelect" IN hArtBasSok,YES).*/
  
  RUN InitializeObject IN hArtBasSok.
  IF AvdelingNr:SCREEN-VALUE IN FRAME {&FRAME-NAME} NE ? THEN
    DYNAMIC-FUNCTION("setAvdNr" IN hArtBasSok,AvdelingNr:SCREEN-VALUE,NO).
END.

/* getStat().  */

RUN MoveToTop IN hArtBasSok.
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
DYNAMIC-FUNCTION("CreateStatusBar",THIS-PROCEDURE:CURRENT-WINDOW,"",0,YES,?).

DEFINE VARIABLE iBrGrpNr  AS INTEGER NO-UNDO.

DYNAMIC-FUNCTION("NewObject",THIS-PROCEDURE:CURRENT-WINDOW,THIS-PROCEDURE,"procedure").

DO WITH FRAME {&FRAME-NAME}:
  DYNAMIC-FUNCTION("setAttribute",SESSION,"SeButikkNr",
                   DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr")).
  iBrGrpNr = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","BrGrpNr").

  ButikkNr:DELIMITER = "|".
  IF DYNAMIC-FUNCTION("getAttribute",SESSION,"SeButikkNr") NE "" THEN 
    ButikkNr:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","butikktilgang,butiker;butnamn|butik;butik"
                                               ,"WHERE brgrpnr = " + STRING(iBrGrpNr) + ", first Butiker where Butiker.Butik = ButikkTilgang.Butik").
  ELSE 
    ButikkNr:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("GetFieldList","Butiker;Butik|ButNamn;Butik","WHERE true BY ButNamn").

  ASSIGN LevStatus:DELIMITER = "|"
         LevStatus:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","SysPara;ParaNr|Parameter1;ParaNr","WHERE SysHId = 19 and SysGr = 1")
         AvdelingNr:DELIMITER = "|"
         AvdelingNr:LIST-ITEM-PAIRS = DYNAMIC-FUNCTION("getFieldList","Avdeling;AvdelingNr|AvdelingNavn;AvdelingNr","WHERE true")
         ForsNr:DELIMITER = "|"
         cBefrakter:DELIMITER = "|"
         cBefrakter:LIST-ITEM-PAIRS = RIGHT-TRIM("| |" +  DYNAMIC-FUNCTION("getFieldList","SysPara;Beskrivelse;Parameter1","WHERE SysHId = 150 AND SysGr = 10"),"|")
         .

  hQuery = DYNAMIC-FUNCTION("NewQuery"
          ,100
          ,""
          ,"KordreHode"
         + ";AvdelingNr"
         + ";ButikkNr"
         + ";ForsNr"
         + ";LevStatus"
         + ";InternMerknad"
         + ";Adresse1"
         + ";ePostAdresse"
         + ";Leveringsdato"
         + ";MobilTlf"
         + ";Navn"
         + ";PostNr"
         + ";PostSted"
         + ";RegistrertAv"
         + ";RegistrertDato"
         + ";SvarFrist"
         + ";Telefon"
         + ";KOrdre_id"
         + ";KundeNr"
         + ";+cBefrakter|CHARACTER|x(10)|kasseordre_befrakter"
         + ";+fFrakt|DECIMAL|->><>>><>>9.99|kasseordre_frakt"
          ,"WHERE false"
          ,"").

  DYNAMIC-FUNCTION("CreateObjectLink",hQuery,THIS-PROCEDURE).

  DYNAMIC-FUNCTION("setAttribute",hQuery,"calcFieldProc","hurtigordre_hode_qrycalc.p").
  DYNAMIC-FUNCTION("setAttribute",hQuery,"baseQuery","WHERE Opphav = 5").
  DYNAMIC-FUNCTION("setSortString",hQuery,"KOrdre_Id;desc").

  hFieldMap = DYNAMIC-FUNCTION("NewFieldMap"
          ,hQuery
          ,FRAME {&FRAME-NAME}:HANDLE
          ,"ButikkNr,ForsNr,AvdelingNr,Adresse1,Navn,SvarFrist,Leveringsdato,PostNr,PostSted,ePostAdresse,Telefon,MobilTlf,LevStatus,cBefrakter,fFrakt,InternMerknad",""
          ,"RegistrertDato,RegistrertAv,KOrdre_Id",""
          ,"btnLeveringsdato,btnPostnr,btnSvarFrist").

  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraFields","Opphav,KundeNr").
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"CustomUpdateValProc","=hurtigordre_hode_updval.p").

  DYNAMIC-FUNCTION("CreateObjectLink",hFieldMap,hQuery).

  hToolbar = DYNAMIC-FUNCTION("NewToolBar"
          ,tbKasseOrdreHode:HANDLE
          ,"Fil"
          ,"new;Ny,undo;Angre,delete;Sle&tt,save;&Lagre,Print,Email;Send ep&ost,SMS;S&MS"
         + ",-,first;Første,prev;F&orrige,next;Neste,last;S&iste"
         + (IF bNoSearchBtn THEN "" 
            ELSE ",-,SokOrdre;Sø&k ordre;Søk ordre;;bmp/select.bmp")
          ,"maxborder").

  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hQuery).
  DYNAMIC-FUNCTION("CreateObjectLink",hToolbar,hFieldMap).

  hBrowse = DYNAMIC-FUNCTION("NewBrowse"
          ,brwKasseOrdreLinjer:HANDLE
          ,100
          ,""
          ,"KordreLinje"
          + ";VareNr"
          + ";Varetekst"
          + ";Antall"
          + ";Storl"
          + ";LevFargKod"
          + ";LinjeRabattKr"
          + ";LinjeRab%"
          + ";VareKost"
          + ";Pris"
          + ";NettoPris"
          + ";NettoLinjesum"
          + ";+SumEksMvaKr|DECIMAL|->>><>>9.99|kordrelinje_sumeksmvakr|Sum eks MVA"
          + ";!+fraktVare|LOGICAL|yes/no|fraktVare"
        + ",ArtBas"
          + ";Vg"
          + ";Lopnr"
          + ";LevKod@3"
          ,"WHERE false"
         + ",FIRST ArtBas NO-LOCK WHERE ArtikkelNr = DEC(varenr) OUTER-JOIN"    
          ,"").
  DYNAMIC-FUNCTION("CreateParentLink",hBrowse,hQuery,"KOrdre_id").

  DYNAMIC-FUNCTION("setAttribute",hBrowse,"calcfieldproc","kordrelinje_brwcalc.p").

  hFmLinje = DYNAMIC-FUNCTION("NewFieldMap"
          ,hBrowse:QUERY
          ,FRAME {&FRAME-NAME}:HANDLE
          ,"Varenr,Varetekst,Storl,Antall,NettoPris,LinjeRab%",""
          ,"LevFargKod,LevKod,NettoLinjesum,LopNr,Pris,VareKost,Vg",""
          ,"btnVareNr").
  DYNAMIC-FUNCTION("setAttribute",hFmLinje,"CustomCreateProc","hurtigordre_linje_create.p").
/*   DYNAMIC-FUNCTION("setAttribute",hFmLinje,"CustomUpdateValProc","=kasseordre_linje_updval.p"). */
/*   DYNAMIC-FUNCTION("setAttribute",hFmLinje,"bufferExtraFields","cProfilnr").                    */
  DYNAMIC-FUNCTION("setAttribute",hFmLinje,"postupdateproc","kordrelinje_post_update.p").
  DYNAMIC-FUNCTION("CreateObjectLink",hFmLinje,hBrowse).

  hTbLinje = DYNAMIC-FUNCTION("NewToolBar"
          ,tbKasseOrdreLinjer:HANDLE
          ,"Fil"
          ,"new;Ny varectrl-N,undo;Angre endringctrl-z,delete;Slettctrl-d,save;Lagrectrl-s,Excel"
         + ",HentArt;Legg til artikleralt-s"
          ,"maxborder").
  DYNAMIC-FUNCTION("CreateObjectLink",hTbLinje,hFmLinje).
  DYNAMIC-FUNCTION("CreateObjectLink",hTbLinje,hBrowse).

  RUN InvokeMethod(hQuery,"OpenQuery").
END.

DYNAMIC-FUNCTION("setNoMoveX", THIS-PROCEDURE:CURRENT-WINDOW,FRAME {&FRAME-NAME}:HANDLE,
                 "btnLeveringsdato,btnSvarFrist,cBefrakter,LevStatus,InternMerknad,fFrakt,Leveringsdato,MerknadLabel,SvarFrist,btnNavn,KOrdre_Id").
DYNAMIC-FUNCTION("setNoResizeY", THIS-PROCEDURE:CURRENT-WINDOW, FRAME {&FRAME-NAME}:HANDLE,"InternMerknad").
DYNAMIC-FUNCTION("setOrgWinSize",THIS-PROCEDURE:CURRENT-WINDOW,300,800,0,0).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeaveOfFieldHandle C-Win 
PROCEDURE LeaveOfFieldHandle :
/*------------------------------------------------------------------------------
  Purpose:     Retrieve lookup values when a foreign key field is modified
  Parameters:  Handle to foreign key field
  Notes:       The cReturn variable should be replaced with a fill-in
------------------------------------------------------------------------------*/
DEF INPUT PARAM ihField AS HANDLE NO-UNDO.

DEF VAR cReturn AS CHAR NO-UNDO.

IF ihField:MODIFIED THEN DO WITH FRAME {&FRAME-NAME}:
  CASE ihField:NAME:
    WHEN "VareNr" THEN DO:
      IF DYNAMIC-FUNCTION("getToolbarState",hTbLinje) = "new" THEN DO:
        RUN InvokeMethod(hTbLinje,"SaveRecord").
        IF setVareNr(ihField:SCREEN-VALUE) THEN DO:          
          DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hFmLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
          RUN InvokeMethod(hBrowse,"DisplayRecord").
          APPLY "any-printable" TO VareNr.
          APPLY "entry" TO Varetekst.
          bUndoIsDelete = YES.
        END.
      END.
      ELSE IF setVareNr(ihField:SCREEN-VALUE) THEN DO:          
        DYNAMIC-FUNCTION("RefreshRowids",hBrowse,hFmLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE).
        RUN InvokeMethod(hBrowse,"DisplayRecord").
        APPLY "any-printable" TO VareNr.
        APPLY "entry" TO Varetekst.
      END.
    END.
    WHEN "PostNr" THEN 
      PostSted:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Post","WHERE PostNr = '" + PostNr:SCREEN-VALUE + "'"
                                              ,"Beskrivelse").
    WHEN "LinjeRab%" THEN DO:
      IF DECIMAL(ihField:SCREEN-VALUE) NE 0 AND NOT LOGICAL(DYNAMIC-FUNCTION("getFieldValues","ArtBas","WHERE Artikkelnr = DEC('" + VareNr:SCREEN-VALUE + "')","ManRabIKas")) THEN DO:

        DYNAMIC-FUNCTION("DoMessage",0,0,"Artikkel kan ikke rabatteres","","").
        ihField:SCREEN-VALUE = "0".
      END.
      ELSE bModRabatt = YES.
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
THIS-PROCEDURE:CURRENT-WINDOW:HIDDEN = NO.
THIS-PROCEDURE:CURRENT-WINDOW:WINDOW-STATE = 3.
THIS-PROCEDURE:CURRENT-WINDOW:MOVE-TO-TOP().

APPLY "entry" TO ButikkNr IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NewRecord C-Win 
PROCEDURE NewRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cButNr    AS CHAR NO-UNDO.
DEF VAR cKasserer AS CHAR NO-UNDO.

RUN SUPER.

IF DYNAMIC-FUNCTION("getCurrentObject") = hToolbar THEN DO WITH FRAME {&FRAME-NAME}:
  ASSIGN LevStatus:SCREEN-VALUE = LevStatus:ENTRY(1)
         SvarFrist:SCREEN-VALUE = STRING(TODAY + 14)
         cKundeNr               = ""
         .

  cButNr = DYNAMIC-FUNCTION("getFieldValues","Bruker","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "'","ButikkNr").
  IF cButNr = "0" OR cButNr = ? THEN
    cButNr = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE clButikkNr > 0","clButikkNr").  

  IF cButNr NE "0" AND cButNr NE ? THEN DO:
    ButikkNr:SCREEN-VALUE = cButNr.
    APPLY "value-changed" TO ButikkNr.
    /*cKasserer = DYNAMIC-FUNCTION("getFieldValues","FIRST butikkforsalj","WHERE BrukerId = '" + DYNAMIC-FUNCTION("getASuserId") + "' AND Butik = " + cButNr,"ForsNr").*/
    cKasserer = '0'.
    /*IF cKasserer NE ? AND cKasserer NE "0" THEN*/
    ForsNr:SCREEN-VALUE = cKasserer NO-ERROR.

  END.
  ELSE DO:
    ButikkNr:SCREEN-VALUE  = ButikkNr:ENTRY(1).
    APPLY "value-changed" TO ButikkNr.
  END.

  IF NUM-ENTRIES(cBefrakter:LIST-ITEM-PAIRS,"|") > 2 THEN DO:
    cBefrakter:SCREEN-VALUE = cBefrakter:ENTRY(2).
    APPLY "value-changed" TO cBefrakter.
  END.
  hFmLinje:FIND-FIRST("where false") NO-ERROR.
  DYNAMIC-FUNCTION("DisplayFieldMap",hFmLinje).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrintRecord C-Win 
PROCEDURE PrintRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
RUN skrivkundeordre.p (STRING(hFieldMap:BUFFER-FIELD("KOrdre_id"):BUFFER-VALUE) + "|full",
                       NO,"",1,"","").
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
IF DYNAMIC-FUNCTION("getCurrentObject") = hTbLinje THEN DO:
/*   DYNAMIC-FUNCTION("setAttribute",hFmLinje,"bufferExtraValues",cProfilnr).  */
  DYNAMIC-FUNCTION("setServerTransInputParam",IF LinjeRab%:MODIFIED IN FRAME {&FRAME-NAME} THEN "LinjeRab%" ELSE "NettoPris").
END.
ELSE
  DYNAMIC-FUNCTION("setAttribute",hFieldMap,"bufferExtraValues","5|" + cKundeNr).


RUN SUPER.

getStat().

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SMSRecord C-Win 
PROCEDURE SMSRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR cMessage AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  IF MobilTlf:SCREEN-VALUE = "" THEN DO:
    DYNAMIC-FUNCTION("DoMessage",0,0,"Mobiltelefon er ikke utfylt","","").
    RETURN.
  END.
  cMessage = "Hei, din ordre er klar for henting." + CHR(10) 
           +  "Vennlig hilsen " 
           + REPLACE(DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + ButikkNr:SCREEN-VALUE,"ButNamn,BuAdr")
                    ,"|",", ").
  RUN SendSMS.w (MobilTlf:SCREEN-VALUE,
                 "Ordrenr: " + KOrdre_Id:SCREEN-VALUE,
                 cMessage,
                 OUTPUT bOK,
                 OUTPUT cMessage).
  IF bOK THEN DO:
    InternMerknad:SCREEN-VALUE = cMessage + CHR(10) + InternMerknad:SCREEN-VALUE.
    IF LevStatus:SCREEN-VALUE < "30" THEN DO:
      IF DYNAMIC-FUNCTION("DoMessage",0,4,"Sett ordrestatus til bekreftet?","","") = 6 THEN
        LevStatus:SCREEN-VALUE = "30".
    END.
    RUN InvokeMethod(hFieldMap,"SaveRecord").
  END.
END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SokOrdreRecord C-Win 
PROCEDURE SokOrdreRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
IF NOT VALID-HANDLE(hFilter) THEN DO:
  RUN HurtigOrdre.w PERSIST SET hFilter.
  RUN InitializeObject IN hFilter.
  DYNAMIC-FUNCTION("setDetaljerHandle" IN hFilter,THIS-PROCEDURE,hQuery).
END.
RUN MoveToTop IN hFilter.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UndoRecord C-Win 
PROCEDURE UndoRecord :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR bDelete AS LOG NO-UNDO.

bDelete = bUndoIsDelete.

RUN SUPER.

IF bDelete THEN DO:
  DYNAMIC-FUNCTION("setAttribute",hTbLinje,"noDeleteWarning","yes").
  RUN InvokeMethod(hTbLinje,"DeleteRecord").
  DYNAMIC-FUNCTION("setAttribute",hTbLinje,"noDeleteWarning","").
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValueChangedField C-Win 
PROCEDURE ValueChangedField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAM icFieldname AS CHAR NO-UNDO.

DEF VAR cValue AS CHAR NO-UNDO.

DO WITH FRAME {&FRAME-NAME}:
  CASE icFieldname:
    WHEN "ButikkNr" THEN DO:
      ASSIGN ForsNr:LIST-ITEM-PAIRS = RIGHT-TRIM("|0|" 
                                    + DYNAMIC-FUNCTION("getFieldList",
                                                       "butikkforsalj,forsalj;forsnr|Fonamn;forsnr",
                                                       "where " 
                                                     + (IF ButikkNr:SCREEN-VALUE NE "0" AND ButikkNr:SCREEN-VALUE NE ? THEN
                                                         "butik = " + ButikkNr:SCREEN-VALUE 
                                                        ELSE "true")
                                                     + ",first forsalj NO-LOCK of butikkforsalj")
                                                ,"|")
             ForsNr:SCREEN-VALUE = "0"
             cProfilNr = DYNAMIC-FUNCTION("getFieldValues","Butiker","WHERE Butik = " + ButikkNr:SCREEN-VALUE,"ProfilNr")
             .                                                
      IF cProfilNr = ? THEN cProfilNr = "1".
    END.
    WHEN "cBefrakter" THEN DO:
      cValue = DYNAMIC-FUNCTION("getFieldValues","SysPara",
                                "WHERE SysHId = 150 AND SysGr = 10 AND Parameter1 = '" + cBefrakter:SCREEN-VALUE + "'",
                                "Parameter2").  
      IF cValue NE "" AND cValue NE ? THEN
        fFrakt:SCREEN-VALUE = cValue NO-ERROR.
    END.
  END CASE.
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
DEF VAR bReturnFocus AS LOG   NO-UNDO.
DEF VAR cStrList     AS CHAR  NO-UNDO.
DEF VAR bOkStr       AS LOG   NO-UNDO.

IF ifPlukkAnt NE 0 AND ifPlukkAnt NE ? THEN DO WITH FRAME {&FRAME-NAME}:

  bOk = hFmLinje:FIND-FIRST("WHERE VareNr = '" + STRING(ifArtikkelNr) + "'"
                          + "  AND Storl  = '" + icStorl + "'") NO-ERROR.
  IF bOk THEN DO:
    hBrowse:SET-REPOSITIONED-ROW(hBrowse:DOWN,"conditional").
    hBrowse:QUERY:REPOSITION-TO-ROWID(hFmLinje:ROWID).
    RUN InvokeMethod(hBrowse,"DisplayRecord").
    Antall:SCREEN-VALUE = STRING(ifPlukkAnt).
    RUN InvokeMethod(hTbLinje,"SaveRecord").
    RETURN YES.
  END.
END.
  
IF icAction = "add" THEN
  RUN InvokeMethod(hTbLinje,"NewRecord").  
ELSE 
  APPLY "value-changed" TO hBrowse.


VareNr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(ifArtikkelNr). 
RUN LeaveOfFieldHandle(VareNr:HANDLE).

IF icStorl NE "" THEN DO:
  Storl:SCREEN-VALUE = icStorl.
  
  IF ifPlukkAnt NE ? AND ifPlukkAnt NE 0 THEN
    ASSIGN Antall:SCREEN-VALUE = STRING(ifPlukkAnt)
           bReturnFocus = YES.
  
  Antall:MODIFIED = YES.
  APPLY "tab" TO Antall.
END.
ELSE DO:
  cStrList = REPLACE(REPLACE(
             DYNAMIC-FUNCTION("getFieldList","ArtBas;,StrType;AlfaFordeling",
                              "WHERE ArtikkelNr = " + STRING(ifArtikkelNr) + ",FIRST StrType OF ArtBas NO-LOCK")
             ," ","")," ","").
  IF Storl:SCREEN-VALUE NE "" AND CAN-DO(cStrList,Storl:SCREEN-VALUE) THEN
    bOkStr = YES.
  ELSE
    Storl:SCREEN-VALUE = TRIM(ENTRY(1,cStrList)).
END.

RUN InvokeMethod(hTbLinje,"SaveRecord").

RETURN bReturnFocus.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION AdjustBrowseColumns C-Win 
FUNCTION AdjustBrowseColumns RETURNS LOGICAL
  ( INPUT ihBrowse AS HANDLE,
    INPUT icBrowseName AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR hColumn AS HANDLE NO-UNDO.

hColumn = DYNAMIC-FUNCTION("getBrowseColumn",ihBrowse,"Varenr").
hColumn:WIDTH-PIXELS = 70.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getMyQueryStat C-Win 
FUNCTION getMyQueryStat RETURNS CHARACTER
  ( INPUT ihDataObject    AS HANDLE,
    INPUT icStatFields    AS CHAR,
    INPUT icCriteria      AS CHAR ) : 
/*------------------------------------------------------------------------------
  Purpose:  Sum up listed fields in a local query
    Notes:  If no fields are specified go and get them from the querystatfields attribute
            and then store the result back in the querystatfieldvalues
------------------------------------------------------------------------------*/
DEF VAR hBuffer       AS HANDLE NO-UNDO.
DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hQueryBuffer  AS HANDLE NO-UNDO.
DEF VAR cStatFields   AS CHAR   NO-UNDO.
DEF VAR fStatValues   AS DEC    NO-UNDO EXTENT 100.
DEF VAR cReturnString AS CHAR   NO-UNDO.
DEF VAR iCount        AS INT    NO-UNDO.
def var lDec          as dec    no-undo.

IF icStatFields = "" THEN
  cStatFields = DYNAMIC-FUNCTION("getAttribute",ihDataObject,"querystatfields").
ELSE cStatFields = icStatFields.

IF ihDataObject:TYPE = "browse" THEN
  hBuffer = ihDataObject:QUERY:GET-BUFFER-HANDLE(1).
ELSE IF ihDataObject:TYPE = "query" THEN
  hBuffer = ihDataObject:GET-BUFFER-HANDLE(1).
ELSE hBuffer = ihDataObject.

CREATE BUFFER hQueryBuffer FOR TABLE hBuffer.
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hQueryBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + hQueryBuffer:NAME + " " + icCriteria).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  iCount = iCount + 1.
  lDec = dec(hQueryBuffer:BUFFER-FIELD('VareNr'):BUFFER-VALUE) no-error.
  if not error-status:error then 
  DO ix = 1 TO NUM-ENTRIES(cStatFields):
    fStatValues[ix] = fStatValues[ix] + hQueryBuffer:BUFFER-FIELD(ENTRY(ix,cStatFields)):BUFFER-VALUE.
  END.
  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.
DELETE OBJECT hQueryBuffer.

IF icCriteria NE "" THEN RETURN STRING(fStatValues[1]).

DO ix = 1 TO NUM-ENTRIES(cStatFields):
  cReturnString = cReturnString + ENTRY(ix,cStatFields) + "|" + (IF fStatValues[ix] NE ? THEN STRING(fStatValues[ix]) ELSE "") + ";".
END.

IF icStatFields = "" THEN DO:
  DYNAMIC-FUNCTION("setAttribute",ihDataObject,"querystatfieldvalues",TRIM(cReturnString,";")).
  DYNAMIC-FUNCTION("setAttribute",ihDataObject,"recordcount",STRING(iCount)).
  DYNAMIC-FUNCTION("ViewRecordCount",ihDataObject).
END.
ELSE 
  cReturnString = "rowcount|" + STRING(iCount) + ";" + cReturnString.

RETURN TRIM(cReturnString,";").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getStat C-Win 
FUNCTION getStat RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEF VAR cQueryStat AS CHAR NO-UNDO.
DEF VAR fTotRabatt AS DEC  NO-UNDO.
def var lSumOrdreEksMva as dec no-undo.

DO WITH FRAME {&FRAME-NAME}:

  ASSIGN fiSumOrdre:SCREEN-VALUE    = ""
         fiSumOrdreEksMva:SCREEN-VALUE = ""
         .

  cQueryStat = getMyQueryStat(hBrowse,"NettoLinjeSum,SumEksMvaKr","").
  
  IF NUM-ENTRIES(cQueryStat,";") > 1 THEN DO ix = 2 TO NUM-ENTRIES(cQueryStat,";"):
    CASE ix:
      WHEN 2 THEN fiSumOrdre:SCREEN-VALUE    = ENTRY(2,ENTRY(ix,cQueryStat,";"),"|").
      WHEN 3 THEN fiSumOrdreEksMva:SCREEN-VALUE = ENTRY(2,ENTRY(ix,cQueryStat,";"),"|").
    END CASE.
  END.
END.
  
RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setNoSearchBtn C-Win 
FUNCTION setNoSearchBtn RETURNS LOGICAL
  ( INPUT ibNoSearchBtn AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
bNoSearchBtn = ibNoSearchBtn.

RETURN YES.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION setVarenr C-Win 
FUNCTION setVarenr RETURNS LOGICAL
  ( INPUT icVareNr AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose: Assign StrKode to order-line 
    Notes:  
------------------------------------------------------------------------------*/
IF NOT DYNAMIC-FUNCTION("runproc","kordrelinje_set_varenr.p",hFmLinje:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE 
                                                          + "|" + icVareNr 
                                                          + "|" + hFmLinje:BUFFER-FIELD("Storl"):BUFFER-VALUE
                                 ,?) THEN DO:
  DYNAMIC-FUNCTION("DoMessage",0,0,DYNAMIC-FUNCTION("getTransactionMessage"),"","").
  RETURN FALSE.   
END.
ELSE DO:
  VareNr:SCREEN-VALUE IN FRAME {&FRAME-NAME} = DYNAMIC-FUNCTION("getTransactionMessage").
  RETURN YES.
END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

