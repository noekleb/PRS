&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME C-Detalj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Detalj 
/* ---------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

def buffer bufBilder for Bilder.

/* Parameters Definitions ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  def var wBildeRecid as recid no-undo.
  def var wModus      as char  no-undo. /* NY, ENDRE, SLETT */
  assign 
    wModus = "ENDRE". /* Default */
  find first Bilder no-lock no-error.
    if available Bilder then
      wBildeRecid = recid(Bilder).
    else do transaction:
      create Bilder.
      assign 
        Bilder.BildNr = 1
        wBildeRecid = recid(Bilder).
    end.
  assign
    session:data-entry-return = true.
&ELSE
  def input parameter wBildeRecid as recid no-undo.
  def input parameter wModus      as char  no-undo. /* NY, ENDRE, SLETT */
  if wBildeRecid = ? then
    do transaction:
      find last bufBilder no-lock no-error.
      create Bilder.
      assign 
        wModus        = "ENDRE"
        Bilder.BildNr = if available bufBilder then bufBilder.BildNr + 1 else 1
        wBildeRecid   = recid(Bilder).
      release Bilder.
    end. /* TRNASACTION */
&ENDIF

/* Local Variable Definitions ---                                       */
def var wOk as log format "Ja/Nei" no-undo.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-Artikler

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES artbas Bilderegister levbas

/* Definitions for BROWSE BROWSE-Artikler                               */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Artikler artbas.vg artbas.lopnr ~
artbas.beskr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Artikler 
&Scoped-define OPEN-QUERY-BROWSE-Artikler OPEN QUERY BROWSE-Artikler FOR EACH artbas OF Bilderegister ~
      WHERE (if BildeRegister.BildNr <> 0  ~
   then ArtBas.BildNr = BildeRegister.BildNr  ~
   else ArtBas.BildNr > 99999999) NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-Artikler artbas
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Artikler artbas


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define FIELDS-IN-QUERY-DEFAULT-FRAME Bilderegister.BildNr ~
Bilderegister.Tekst Bilderegister.DokumentNr Bilderegister.Dato ~
Bilderegister.RegistrertDato Bilderegister.Merknad Bilderegister.Sted ~
Bilderegister.FilNavn Bilderegister.Notat Bilderegister.LevNr ~
LevBas.levnamn Bilderegister.LevArtNr Bilderegister.EksterntID 
&Scoped-define ENABLED-FIELDS-IN-QUERY-DEFAULT-FRAME Bilderegister.Tekst ~
Bilderegister.Dato Bilderegister.Merknad Bilderegister.Sted ~
Bilderegister.FilNavn Bilderegister.Notat Bilderegister.LevNr ~
Bilderegister.LevArtNr Bilderegister.EksterntID 
&Scoped-define ENABLED-TABLES-IN-QUERY-DEFAULT-FRAME Bilderegister
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-DEFAULT-FRAME Bilderegister
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-Artikler}
&Scoped-define OPEN-QUERY-DEFAULT-FRAME OPEN QUERY DEFAULT-FRAME FOR EACH Bilderegister SHARE-LOCK, ~
      EACH levbas OF Bilderegister SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-DEFAULT-FRAME Bilderegister levbas
&Scoped-define FIRST-TABLE-IN-QUERY-DEFAULT-FRAME Bilderegister
&Scoped-define SECOND-TABLE-IN-QUERY-DEFAULT-FRAME levbas


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Bilderegister.Tekst Bilderegister.Dato ~
Bilderegister.Merknad Bilderegister.Sted Bilderegister.FilNavn ~
Bilderegister.Notat Bilderegister.LevNr Bilderegister.LevArtNr ~
Bilderegister.EksterntID 
&Scoped-define ENABLED-TABLES Bilderegister
&Scoped-define FIRST-ENABLED-TABLE Bilderegister
&Scoped-define DISPLAYED-TABLES Bilderegister LevBas
&Scoped-define FIRST-DISPLAYED-TABLE Bilderegister
&Scoped-define SECOND-DISPLAYED-TABLE LevBas
&Scoped-Define ENABLED-OBJECTS BUTTON-SokFil BUTTON-SokLev BUTTON-Ok ~
BUTTON-First BUTTON-Prev BUTTON-Next BUTTON-Last BUTTON-Ny Btn_Lagre ~
BUTTON-Kopi BUTTON-Slett BUTTON-Angre BT-Zoom BT-Kobling BT-Artikkel ~
BROWSE-Artikler RECT-10 RECT-11 RECT-16 RECT-17 RECT-18 
&Scoped-Define DISPLAYED-FIELDS Bilderegister.BildNr Bilderegister.Tekst ~
Bilderegister.DokumentNr Bilderegister.Dato Bilderegister.RegistrertDato ~
Bilderegister.Merknad Bilderegister.Sted Bilderegister.FilNavn ~
Bilderegister.Notat Bilderegister.LevNr LevBas.levnamn ~
Bilderegister.LevArtNr Bilderegister.EksterntID 
&Scoped-Define DISPLAYED-OBJECTS FI-Endret 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Detalj AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE IMAGE-1 AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chIMAGE-1 AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BT-Artikkel 
     LABEL "Artikkel..." 
     SIZE 15 BY 1 TOOLTIP "Opprette ny artikkel".

DEFINE BUTTON BT-Kobling 
     LABEL "&Koble mot artikkel..." 
     SIZE 19.8 BY 1 TOOLTIP "Åpner søkeliste i artikkelregister (For kobling mot artikkel)".

DEFINE BUTTON BT-Zoom 
     IMAGE-UP FILE "icon\e-vis":U
     LABEL "Zoom..." 
     SIZE 4.6 BY 1.05 TOOLTIP "Vis forstørret bilde".

DEFINE BUTTON Btn_Lagre DEFAULT 
     IMAGE-UP FILE "icon\e-save":U
     LABEL "Lagra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagrer billedinformasjonen"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Angre 
     IMAGE-UP FILE "icon\e-undo":U
     LABEL "Ångra" 
     SIZE 4.6 BY 1.05 TOOLTIP "Angre endring i billedinformasjonen".

DEFINE BUTTON BUTTON-First 
     IMAGE-UP FILE "icon\e-first":U
     LABEL "Første" 
     SIZE 4.6 BY 1.05 TOOLTIP "Første post".

DEFINE BUTTON BUTTON-Kopi 
     IMAGE-UP FILE "icon\e-copy":U
     LABEL "Kopiera" 
     SIZE 4.6 BY 1.05 TOOLTIP "Kopierer billedinformasjonen".

DEFINE BUTTON BUTTON-Last 
     IMAGE-UP FILE "icon\e-last":U
     LABEL "Siste" 
     SIZE 4.6 BY 1.05 TOOLTIP "Siste post".

DEFINE BUTTON BUTTON-Next 
     IMAGE-UP FILE "icon\e-next":U
     LABEL "Neste" 
     SIZE 4.6 BY 1.05 TOOLTIP "Neste post".

DEFINE BUTTON BUTTON-Ny 
     IMAGE-UP FILE "icon\e-ny":U
     LABEL "Ny" 
     SIZE 4.6 BY 1.05 TOOLTIP "Nytt bilde (Lagrer det som er i buffer først)".

DEFINE BUTTON BUTTON-Ok 
     IMAGE-UP FILE "icon\e-exit":U
     LABEL "&Ok" 
     SIZE 4.6 BY 1.05 TOOLTIP "Lagrer og avslutter".

DEFINE BUTTON BUTTON-Prev 
     IMAGE-UP FILE "icon\e-prev":U
     LABEL "Forrige" 
     SIZE 4.6 BY 1.05 TOOLTIP "Forrige post".

DEFINE BUTTON BUTTON-Slett 
     IMAGE-UP FILE "icon\e-del":U
     LABEL "Radera" 
     SIZE 4.6 BY 1.05 TOOLTIP "Slett bilde og billedinformasjon".

DEFINE BUTTON BUTTON-SokFil 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 5 BY 1.

DEFINE BUTTON BUTTON-SokLev 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Endret AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 55.4 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 58.6 BY 18.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57 BY 10.95.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 116.6 BY .1.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 116.6 BY .1.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 56.6 BY 6.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Artikler FOR 
      artbas SCROLLING.

DEFINE QUERY DEFAULT-FRAME FOR 
      Bilderegister, 
      levbas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Artikler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Artikler C-Detalj _STRUCTURED
  QUERY BROWSE-Artikler DISPLAY
      artbas.vg COLUMN-LABEL "Vg" FORMAT "z999":U
      artbas.lopnr COLUMN-LABEL "Løpenr" FORMAT ">>>>9":U
      artbas.beskr COLUMN-LABEL "Beskr" FORMAT "x(38)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54.6 BY 5.52 TOOLTIP "ENTER - Artikkelregister, DEL - Opphev kobling.".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     BUTTON-SokFil AT ROW 9.86 COL 54.6
     BUTTON-SokLev AT ROW 17.48 COL 29
     BUTTON-Ok AT ROW 1.38 COL 112.8 HELP
          "Lagrer og avslutter"
     BUTTON-First AT ROW 1.48 COL 2.2
     BUTTON-Prev AT ROW 1.48 COL 6.8
     BUTTON-Next AT ROW 1.48 COL 11.4
     BUTTON-Last AT ROW 1.48 COL 16
     BUTTON-Ny AT ROW 1.48 COL 22.6 HELP
          "Nytt bilde (Lagrer det som er i buffer først)"
     Btn_Lagre AT ROW 1.48 COL 27.8 HELP
          "Lagrer billedinformasjonen"
     BUTTON-Kopi AT ROW 1.48 COL 33 HELP
          "Kopierer billedinformasjonen"
     BUTTON-Slett AT ROW 1.48 COL 38 HELP
          "Slett bilde og billedinformasjon"
     BUTTON-Angre AT ROW 1.48 COL 43.2 HELP
          "Angre endring i billedinformasjonen"
     BT-Zoom AT ROW 1.48 COL 48.2 HELP
          "Vis forstørret bilde"
     BT-Kobling AT ROW 1.48 COL 53.2 HELP
          "Åpner søkeliste i artikkelregister (For kobling mot artikkel)"
     BT-Artikkel AT ROW 1.48 COL 73.2 HELP
          "Opprette ny artikkel"
     Bilderegister.BildNr AT ROW 3.57 COL 16.2 COLON-ALIGNED
          LABEL "BildNr"
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     Bilderegister.Tekst AT ROW 3.57 COL 24.4 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 33 BY 1
     Bilderegister.DokumentNr AT ROW 4.62 COL 16.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Bilderegister.Dato AT ROW 5.67 COL 16.2 COLON-ALIGNED
          LABEL "Fotodatum"
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Bilderegister.RegistrertDato AT ROW 6.71 COL 16.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Bilderegister.Merknad AT ROW 7.76 COL 16.2 COLON-ALIGNED
          LABEL "Anmärkning"
          VIEW-AS FILL-IN 
          SIZE 41.4 BY 1
     Bilderegister.Sted AT ROW 8.81 COL 16.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 41.4 BY 1
     Bilderegister.FilNavn AT ROW 9.86 COL 16.2 COLON-ALIGNED
          LABEL "Filnamn"
          VIEW-AS FILL-IN 
          SIZE 36.4 BY 1
     Bilderegister.Notat AT ROW 10.91 COL 18.2 NO-LABEL
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 41.2 BY 6.48
     FI-Endret AT ROW 12.91 COL 59.8 COLON-ALIGNED NO-LABEL
     BROWSE-Artikler AT ROW 15.38 COL 62 HELP
          "ENTER - Artikkelregister, DEL - Opphev kobling."
     Bilderegister.LevNr AT ROW 17.48 COL 16.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     LevBas.levnamn AT ROW 17.48 COL 32.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 25.2 BY 1
     Bilderegister.LevArtNr AT ROW 18.62 COL 16.2 COLON-ALIGNED
          LABEL "Lev.art.nr"
          VIEW-AS FILL-IN 
          SIZE 41.6 BY 1
     Bilderegister.EksterntID AT ROW 19.76 COL 16.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 41.6 BY 1
     RECT-10 AT ROW 3.1 COL 1.8
     RECT-11 AT ROW 3.1 COL 61
     RECT-16 AT ROW 1.19 COL 1.2
     RECT-17 AT ROW 2.52 COL 1.2
     RECT-18 AT ROW 14.91 COL 61.2
     "Bildinformation" VIEW-AS TEXT
          SIZE 15.6 BY .62 AT ROW 2.71 COL 3.2
     "Visning av bild" VIEW-AS TEXT
          SIZE 16.4 BY .62 AT ROW 2.71 COL 63.8
     "Koblede artikkler" VIEW-AS TEXT
          SIZE 19.2 BY .62 AT ROW 14.52 COL 62.8
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 117.6 BY 20.38.


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
  CREATE WINDOW C-Detalj ASSIGN
         HIDDEN             = YES
         TITLE              = "Detalj bildregister"
         HEIGHT             = 20.38
         WIDTH              = 117.6
         MAX-HEIGHT         = 20.38
         MAX-WIDTH          = 123.4
         VIRTUAL-HEIGHT     = 20.38
         VIRTUAL-WIDTH      = 123.4
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



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Detalj
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* BROWSE-TAB BROWSE-Artikler FI-Endret DEFAULT-FRAME */
ASSIGN 
       FRAME DEFAULT-FRAME:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Bilderegister.BildNr IN FRAME DEFAULT-FRAME
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN Bilderegister.Dato IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Bilderegister.DokumentNr IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Endret IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Bilderegister.FilNavn IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Bilderegister.LevArtNr IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN LevBas.levnamn IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Bilderegister.Merknad IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Bilderegister.RegistrertDato IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Bilderegister.Tekst IN FRAME DEFAULT-FRAME
   EXP-LABEL                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Detalj)
THEN C-Detalj:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Artikler
/* Query rebuild information for BROWSE BROWSE-Artikler
     _TblList          = "SkoTex.artbas OF SkoTex.Bilderegister"
     _Where[1]         = "(if BildeRegister.BildNr <> 0 
   then ArtBas.BildNr = BildeRegister.BildNr 
   else ArtBas.BildNr > 99999999)"
     _FldNameList[1]   > SkoTex.artbas.vg
"artbas.vg" "Vg" "z999" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   > SkoTex.artbas.lopnr
"artbas.lopnr" "Løpenr" ">>>>9" "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[3]   > SkoTex.artbas.beskr
"artbas.beskr" "Beskr" "x(38)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _Query            is OPENED
*/  /* BROWSE BROWSE-Artikler */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME DEFAULT-FRAME
/* Query rebuild information for FRAME DEFAULT-FRAME
     _TblList          = "SkoTex.Bilderegister,SkoTex.levbas OF SkoTex.Bilderegister"
     _Query            is NOT OPENED
*/  /* FRAME DEFAULT-FRAME */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME IMAGE-1 ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 3.38
       COLUMN          = 63.8
       HEIGHT          = 9.19
       WIDTH           = 51.4
       HIDDEN          = no
       SENSITIVE       = yes.
      IMAGE-1:NAME = "IMAGE-1":U .
/* IMAGE-1 OCXINFO:CREATE-CONTROL from: {9A93B740-C96B-11D0-8883-444553540000} type: Picbuf */
      IMAGE-1:MOVE-AFTER(BT-Artikkel:HANDLE IN FRAME DEFAULT-FRAME).

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Detalj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Detalj C-Detalj
ON END-ERROR OF C-Detalj /* Detalj bildregister */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Detalj C-Detalj
ON WINDOW-CLOSE OF C-Detalj /* Detalj bildregister */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Artikler
&Scoped-define SELF-NAME BROWSE-Artikler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Artikler C-Detalj
ON DELETE-CHARACTER OF BROWSE-Artikler IN FRAME DEFAULT-FRAME
DO:
  def buffer bArtBas for ArtBas.
  
  if available ArtBas then
    do transaction:
      find bArtBas exclusive-lock where
        recid(bArtBas) = recid(ArtBas) no-error.
      if available bArtBas then
        do:
          assign 
            bArtBas.BildNr = 0.
          {&OPEN-QUERY-{&BROWSE-NAME}}
          return no-apply.
        end.
      else do:
        message "Artikkelen oppdateres fra en annen terminal" view-as alert-box.
        return no-apply.
      end.
    end.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-Artikler C-Detalj
ON MOUSE-SELECT-DBLCLICK OF BROWSE-Artikler IN FRAME DEFAULT-FRAME
DO:
  if available ArtBas then
    run v-artkor (input recid(ArtBas), "ENDRE").
  else 
    run w-vartkor (input ?, "NY").  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT-Artikkel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT-Artikkel C-Detalj
ON CHOOSE OF BT-Artikkel IN FRAME DEFAULT-FRAME /* Artikkel... */
DO:
  apply "CHOOSE":U to Btn_Lagre.
  if return-value = "AVBRYT" then
    return no-apply.

  run w-vartkor (input ?, "NY" + ";" + Bilderegister.BildNr:screen-value).  
  if return-value = "AVBRYT" then
    return no-apply.
    
  {&OPEN-QUERY-{&BROWSE-NAME}}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT-Kobling
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT-Kobling C-Detalj
ON CHOOSE OF BT-Kobling IN FRAME DEFAULT-FRAME /* Koble mot artikkel... */
DO:
  apply "CHOOSE":U to Btn_Lagre.
  if return-value = "AVBRYT" then
    return no-apply.

  run w-bartsok.w.  
  if return-value = "AVBRYT" then
    return no-apply.
    
  find ArtBas no-lock where
    recid(ArtBas) = int(return-value) no-error.
  if available ArtBas then
    do:
      find ArtBas exclusive-lock where
        recid(ArtBas) = int(return-value) no-error.
      if available ArtBas then
        do:
          assign
            ArtBas.BildNr = int(Bilderegister.BildNr:screen-value).
          {&OPEN-QUERY-{&BROWSE-NAME}}
        end.
    end.
    
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT-Zoom
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT-Zoom C-Detalj
ON CHOOSE OF BT-Zoom IN FRAME DEFAULT-FRAME /* Zoom... */
DO:
  run d-visbil.w (input recid(BildeRegister)).
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Lagre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Lagre C-Detalj
ON CHOOSE OF Btn_Lagre IN FRAME DEFAULT-FRAME /* Lagra */
DO:
  run LagreBilde.
  if return-value <> "AVBRYT" then
    do:
      assign wModus = "ENDRE"
             Bilder.BildNr:sensitive = false.
    end.
  else
    return no-apply "AVBRYT" .
  run VisBilde.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Angre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Angre C-Detalj
ON CHOOSE OF BUTTON-Angre IN FRAME DEFAULT-FRAME /* Ångra */
DO:
  run VisBilde.
  assign 
    wModus = "ENDRE"
    Bilder.BildNr:sensitive = false.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-First
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-First C-Detalj
ON CHOOSE OF BUTTON-First IN FRAME DEFAULT-FRAME /* Første */
DO:
  run LagreBilde.
  if return-value = "AVBRYT" then
    return no-apply.
  
  else do:
    assign wModus = "ENDRE"
           Bilder.BildNr:sensitive = false.
    find first BildeRegister no-lock no-error.
    if available BildeRegister then
      do:
        assign
          wBildeRecid = recid(BildeRegister).
        run VisBilde.
      end.
  end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Kopi
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Kopi C-Detalj
ON CHOOSE OF BUTTON-Kopi IN FRAME DEFAULT-FRAME /* Kopiera */
DO:
  run LagreBilde.
  if return-value = "AVBRYT" then
    return no-apply.
  
  assign 
    wModus   = "NY".

  assign
    Bilder.BildNr:sensitive = true.

  apply "ENTRY":U to Bilder.BildNr in frame {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Last
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Last C-Detalj
ON CHOOSE OF BUTTON-Last IN FRAME DEFAULT-FRAME /* Siste */
DO:
  run LagreBilde.
  if return-value = "AVBRYT" then
    return no-apply.
  
  else do:
    assign wModus = "ENDRE"
           Bilder.BildNr:sensitive = false.
    find last BildeRegister no-lock no-error.
    if available BildeRegister then
      do:
        assign
          wBildeRecid = recid(BildeRegister).
        run VisBilde.
      end.
  end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Next
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Next C-Detalj
ON CHOOSE OF BUTTON-Next IN FRAME DEFAULT-FRAME /* Neste */
DO:
  run LagreBilde.
  if return-value = "AVBRYT" then
    return no-apply.
  
  else do:
    assign wModus = "ENDRE"
           Bilder.BildNr:sensitive = false.
    find next BildeRegister no-lock no-error.
    if not available BildeRegister then
      find last BildeRegister no-lock no-error.
    if available BildeRegister then
      do:
        assign
          wBildeRecid = recid(BildeRegister).
        run VisBilde.
      end.
  end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ny C-Detalj
ON CHOOSE OF BUTTON-Ny IN FRAME DEFAULT-FRAME /* Ny */
DO:
  run BlankBilde.
  if return-value = "AVBRYT" then
    return no-apply.
  
  assign 
    wModus   = "NY".
  run BlankBilde.

  assign
    Bilder.BildNr:sensitive = true.

  apply "ENTRY":U to Bilder.BildNr in frame {&FRAME-NAME}.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Ok
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Ok C-Detalj
ON CHOOSE OF BUTTON-Ok IN FRAME DEFAULT-FRAME /* Ok */
DO:
  /* Sjekker om bildet er slettet */
  find BildeRegister no-lock where
    recid(BildeRegister) = wBildeRecid no-error.
  
  if available BildeRegister then
    do:
      run LagreBilde.
      if return-value = "AVBRYT" then 
        return no-apply.
      else do:
        APPLY "CLOSE":U TO THIS-PROCEDURE.
       return wModus + "," + string(wBildeRecid).  
      end.
    end.

  else do:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    return "SLETTET".  
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Prev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Prev C-Detalj
ON CHOOSE OF BUTTON-Prev IN FRAME DEFAULT-FRAME /* Forrige */
DO:
  run LagreBilde.
  if return-value = "AVBRYT" then
    return no-apply.
  
  else do:
    assign wModus = "ENDRE"
           Bilder.BildNr:sensitive = false.
    find prev BildeRegister no-lock no-error.
    if not available BildeRegister then
      find first BildeRegister no-lock no-error.
    if available BildeRegister then
      do:
        assign
          wBildeRecid = recid(BildeRegister).
        run VisBilde.
      end.
  end.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Slett
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Slett C-Detalj
ON CHOOSE OF BUTTON-Slett IN FRAME DEFAULT-FRAME /* Radera */
DO:
  assign wOk = false.
  message "Skal billedkortet slettes?" view-as alert-box 
    QUESTION BUTTONS YES-NO
    title "Bekreftelse"
    update wOk.
  if wOk = false then
    return no-apply "AVBRYT".  
  else do:
    run SlettBilde.
    if return-value = "SLETTET" then
      do:
        assign 
          wModus   = "NY".
        run BlankBilde.
        APPLY "CLOSE":U TO THIS-PROCEDURE.
        return "SLETTET".
      end.
    else
      return "AVBRYT".
  end.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokFil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFil C-Detalj
ON CHOOSE OF BUTTON-SokFil IN FRAME DEFAULT-FRAME /* ... */
DO:
  def var wFilNavn     as char no-undo.
  def var wReturnValue as char no-undo.
  
  assign
    wFilNavn = BildeRegister.FilNavn:screen-value in frame DEFAULT-FRAME.
    
  /* Henter filnavn p† bilde som skal kobles. */
  SYSTEM-DIALOG GET-FILE wFilNavn
        TITLE      "Velg bilde som skal kobles ..."
        FILTERS    "Bildefiler       "  "*.bmp,*.jpg,*.gif",
                   "BMP Filer (*.bmp)"  "*.bmp",
                   "JPG Filer (*.jpg)"  "*.jpg",
                   "GIF Filer (*.gif)"  "*.gif"
        MUST-EXIST
        USE-FILENAME
        RETURN-TO-START-DIR
        UPDATE wOK.
   if wOk then 
     do:
       display wFilNavn @ BildeRegister.FilNavn with frame DEFAULT-FRAME.
       if search(wFilNavn) <> ? then
         do:
           assign
             chIMAGE-1:Picbuf:filename  = search(wFilNavn)
             chIMAGE-1:Picbuf:AutoScale = True.
        
           wOk = chIMAGE-1:Picbuf:load.         
           
          if valid-handle(wLibHandle) then
            run LesInnBilde in wLibHandle (BildeRegister.BildNr, wFilNavn, output wReturnValue).
         end.
     end.
   else 
     return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokLev
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokLev C-Detalj
ON CHOOSE OF BUTTON-SokLev IN FRAME DEFAULT-FRAME /* ... */
or F10 of BildeRegister.LevNr
DO:
  {soek.i
    &Felt       = BildeRegister.LevNr
    &Frame-Name = DEFAULT-FRAME
    &Program    = d-blevbas.w 
    &OptDisp    = "LevBas.LevNr    when available LevBas @ BildeRegister.LevNr 
                   LevBas.LevNamn  when available LevBas"
    &PostRun    = "find LevBas no-lock where
                     recid(LevBas) = int(return-value) no-error."
  }
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Bilderegister.EksterntID
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bilderegister.EksterntID C-Detalj
ON RETURN OF Bilderegister.EksterntID IN FRAME DEFAULT-FRAME /* Ekst.kobling */
or "TAB":U of BildeRegister.EksterntID
DO:
  if wModus = "NY" then
    apply "ENTRY":U to Bilderegister.BildNr.
  else
    apply "ENTRY":U to SkoTex.Bilderegister.Tekst.
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Bilderegister.FilNavn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bilderegister.FilNavn C-Detalj
ON RETURN OF Bilderegister.FilNavn IN FRAME DEFAULT-FRAME /* Filnamn */
or "TAB":U of Bilderegister.FilNavn
DO:
  if search(BildeRegister.FilNavn:screen-value) <> ? then
    do:
      assign
        chIMAGE-1:Picbuf:filename  = search(BildeRegister.FilNavn:screen-value)
        chIMAGE-1:Picbuf:AutoScale = True.
       
      wOk = chIMAGE-1:Picbuf:load.         
    end.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Bilderegister.LevNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bilderegister.LevNr C-Detalj
ON RETURN OF Bilderegister.LevNr IN FRAME DEFAULT-FRAME /* Leverandør */
or "TAB":U of BildeRegister.LevNr
DO:
  find first LevBas no-lock where
    LevBas.LevNr = int(BildeRegister.LevNr:screen-value) no-error.
  if available LevBas then
    do:
      display
        LevBas.LevNr @ BildeRegister.LevNr
        LevBas.LevNamn 
      with frame DEFAULT-FRAME.
    end.
  else do:
    display
      "" @ BildeRegister.LevNr
      "" @ LevBas.LevNamn.
    message "Ugyldig leverandørnummer" view-as alert-box.
    return no-apply.
  end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Detalj 


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
  find BildeRegister no-lock where
    recid(BildeRegister) = wBildeRecid no-error.
  RUN enable_UI.
  
  run VisBilde.

  if wModus = "NY" then
    apply "ENTRY":U to Bilderegister.BildNr.
  else
    apply "ENTRY":U to SkoTex.Bilderegister.Tekst.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BlankBilde C-Detalj 
PROCEDURE BlankBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  display
    "" @ Bilderegister.BildNr
    "" @ Bilderegister.Dato 
    "" @ BildeRegister.DokumentNr
    "" @ Bilderegister.FilNavn 
    "" @ Bilderegister.Merknad 
    "" @ Bilderegister.RegistrertDato 
    "" @ Bilderegister.Tekst   
    "" @ Bilderegister.Sted 
    "" @ Bilderegister.FilNavn 
    "" @ Bilderegister.LevNr 
    "" @ Bilderegister.LevArtNr 
    "" @ Bilderegister.EksterntID 
    "" @ LevBas.LevNamn
  with frame DEFAULT-FRAME.

  if search("bilder\blank.bmp") <> ? then
    do:
      assign
        chIMAGE-1:Picbuf:filename  = search("bilder\blank.bmp")
        chIMAGE-1:Picbuf:AutoScale = True.
       
      wOk = chIMAGE-1:Picbuf:load.         
    end.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Detalj  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "v-bilde.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chIMAGE-1 = IMAGE-1:COM-HANDLE
    UIB_S = chIMAGE-1:LoadControls( OCXFile, "IMAGE-1":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "v-bilde.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Detalj  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Detalj)
  THEN DELETE WIDGET C-Detalj.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Detalj  _DEFAULT-ENABLE
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
  RUN control_load.
  DISPLAY FI-Endret 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Detalj.
  IF AVAILABLE Bilderegister THEN 
    DISPLAY Bilderegister.BildNr Bilderegister.Tekst Bilderegister.DokumentNr 
          Bilderegister.Dato Bilderegister.RegistrertDato Bilderegister.Merknad 
          Bilderegister.Sted Bilderegister.FilNavn Bilderegister.Notat 
          Bilderegister.LevNr Bilderegister.LevArtNr Bilderegister.EksterntID 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Detalj.
  IF AVAILABLE LevBas THEN 
    DISPLAY LevBas.levnamn 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Detalj.
  ENABLE BUTTON-SokFil BUTTON-SokLev BUTTON-Ok BUTTON-First BUTTON-Prev 
         BUTTON-Next BUTTON-Last BUTTON-Ny Btn_Lagre BUTTON-Kopi BUTTON-Slett 
         BUTTON-Angre BT-Zoom BT-Kobling BT-Artikkel Bilderegister.Tekst 
         Bilderegister.Dato Bilderegister.Merknad Bilderegister.Sted 
         Bilderegister.FilNavn Bilderegister.Notat BROWSE-Artikler 
         Bilderegister.LevNr Bilderegister.LevArtNr Bilderegister.EksterntID 
         RECT-10 RECT-11 RECT-16 RECT-17 RECT-18 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Detalj.
  VIEW FRAME DEFAULT-FRAME IN WINDOW C-Detalj.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Detalj.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagreBilde C-Detalj 
PROCEDURE LagreBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:     Opprettet dato/tid og endret dato/tid, samt brukerid 
             oppdateres i Create og Write triggere for tabell BildeRegister.  
------------------------------------------------------------------------------*/

  def var wReturVerdi as char initial "OK" no-undo.

  LAGRE:
  do transaction:
    if wModus = "NY" then
      do:
        assign wModus = "ENDRE".
        find Bilderegister no-lock where
          BildeRegister.BildNr = int(BildeRegister.BildNr:screen-value in frame DEFAULT-FRAME) no-error.
        if available Bilderegister then
          do:
            message "Billednummeret" BildeRegister.BildNr "er allerede benyttet!" skip
                    "Angi et annet nummer" view-as alert-box 
              WARNING title "Lagringsfeil".
              return no-apply "AVBRYT".
          end.
        else do:
          create Bilderegister.
          assign
            frame DEFAULT-FRAME Bilderegister.BildNr
            wBildeRecid = recid(BildeRegister).
        end.
      end.
    else  
      find BildeRegister exclusive-lock where
        recid(BildeRegister) = wBildeRecid no-error no-wait.

    if locked BildeRegister then
      do:
        message "Posten oppdateres fra en annen terminal"
          view-as alert-box title "Lagringsfeil".
        wReturVerdi = "AVBRYT".
        leave LAGRE.
      end.
        
    else if not available BildeRegister then
      do:
        message "Bildet er slettet eller oppdateres fra en annen terminal"
          view-as alert-box title "Lagringsfeil".
        wReturVerdi = "AVBRYT".
        leave LAGRE.
      end.
      
    assign
      frame DEFAULT-FRAME Bilderegister.Dato 
      frame DEFAULT-FRAME Bilderegister.FilNavn 
      frame DEFAULT-FRAME Bilderegister.Merknad 
      frame DEFAULT-FRAME Bilderegister.Tekst
      frame DEFAULT-FRAME Bilderegister.Sted 
      frame DEFAULT-FRAME Bilderegister.FilNavn 
      frame DEFAULT-FRAME Bilderegister.LevNr 
      frame DEFAULT-FRAME Bilderegister.LevArtNr 
      frame DEFAULT-FRAME Bilderegister.EksterntID 
      frame DEFAULT-FRAME BildeRegister.Notat.
      
  end. /* LAGRE */
  
  return wReturVerdi.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SlettBilde C-Detalj 
PROCEDURE SlettBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Kan ikke slette billedkort som er lagt inn p† artikler. */
  find first ArtBas no-lock where
    ArtBas.BildNr = int(Bilder.BildNr:screen-value in frame {&FRAME-NAME}) no-error.
  if available ArtBas then
    do:
      message "Bilednummeret er benyttet på en eller flere artikler." skip
              "Kan ikke slettes!" view-as alert-box 
              WARNING title "Advarsel".
      return no-apply ("AVBRYT").
    end.
    
  do TRANSACTION:
    find Bilder exclusive-lock where
      recid(Bilder) = wBildeRecid no-error.
    if available Bilder then
      do:
        delete Bilder.
        return "SLETTET".
      end.
    else do:
       message "Ukjent biledkort eller biledkortet oppdateres fra en annen teminal"
         view-as alert-box Title "Feil ved sletting".
       return "AVBRYT".
    end.
  end.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisBilde C-Detalj 
PROCEDURE VisBilde :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def var wFilNavn as char no-undo.
  
  find Bilderegister no-lock where
    recid(BildeRegister) = wBildeRecid.
  if available BildeRegister then
    do:
      find LevBas no-lock where
        LevBas.LevNr = BildeRegister.LevNr no-error.
      assign
        FI-Endret = "Endret av: " + BildeRegister.BrukerID + "   " +
                    string(BildeRegister.EDato,"99/99/9999") + " " +
                    string(BildeRegister.ETid,"HH:MM:SS").
      display
        Bilderegister.BildNr
        Bilderegister.Dato 
        BildeRegister.DokumentNr
        " " when BildeRegister.DokumentNr = 0 @ BildeRegister.DokumentNr
        Bilderegister.FilNavn 
        Bilderegister.Merknad 
        Bilderegister.RegistrertDato 
        Bilderegister.Tekst
        Bilderegister.Sted 
        Bilderegister.FilNavn 
        Bilderegister.LevNr 
        Bilderegister.LevArtNr 
        Bilderegister.EksterntID 
        BildeRegister.Notat
        LevBas.LevNamn when available LevBas
        FI-Endret
      with frame DEFAULT-FRAME.

      if valid-handle(wLibHandle) then
        do:
          run HentBildePeker in wLibHandle (BildeRegister.BildNr, 3, BildeRegister.FilNavn, output wFilNavn).
          
          /* Nullstiller memory mellom hver lasting av bilde. */
          chIMAGE-1:Picbuf:clear(2).
          
          assign
            chIMAGE-1:Picbuf:filename  = wFilNavn
            chIMAGE-1:Picbuf:AutoScale = True.
       
          wOk = chIMAGE-1:Picbuf:load.         
        end.  
    end.   
    
  find first ArtBas no-lock where
    ArtBas.BildNr = BildeRegister.BildNr no-error.    
  {&OPEN-QUERY-{&BROWSE-NAME}}
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

