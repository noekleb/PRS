&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Dialog-Frame 
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
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  DEFINE VAR wRecid as recid NO-UNDO.
  define var wModus as char  no-undo.
  
&ELSE
  DEFINE INPUT-output PARAMETER wRecid as recid NO-UNDO.
  define input        parameter wModus as char  no-undo.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell LevBas
&scoped-define KeyFelt LevNr
&scoped-define DataType INT /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster find Valuta of LevBas no-lock no-error.
/* Ekstra informasjon i find/where når det er flere ledd i indeks */
&scoped-define OptFind 
/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster LevBas.LevNr when available LevBas ~
                         LevBas.LevNamn when available LevBas
/* Alternative poster som skal vises når VisPost kjøres */
&scoped-define VisAndreData
/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes if can-find(Fylke where ~
         {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)) or ~
         int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
        do: ~
          if int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
            message "Leverandørnummer må være større enn 0" ~
            view-as alert-box title "Lagringsfeil". ~
          else ~
            message "Leverandør finnes allerede med leverandørnummer:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
          return "AVBRYT". ~
        end.
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter LevBas.LevNamn ~
  LevBas.koadr ~
  LevBas.koland ~
  LevBas.kommentar[1]  ~
  LevBas.kommentar[2]  ~
  LevBas.kommentar[3]  ~
  LevBas.kommentar[4]  ~
  LevBas.kopadr  ~
  LevBas.koponr  ~
  LevBas.kotel  ~
  LevBas.kotelefax  ~
  LevBas.kotelex  ~
  LevBas.levadr  ~
  LevBas.levkon  ~
  LevBas.levland  ~
  LevBas.levnamn  ~
  LevBas.levpadr  ~
  LevBas.levponr  ~
  LevBas.levtel  ~
  LevBas.Notat  ~
  LevBas.telefax  ~
  LevBas.telex  ~
  LevBas.valkod

/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
&scoped-define TillegsAssign

/* Local Variable Definitions ---                                       */
def var wRetur-Verdi as char initial "AVBRYT" no-undo.
def var wSortId      like LevSort.SortId      no-undo.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES LevBas Valuta

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame LevBas.levnr LevBas.levnamn ~
LevBas.levadr LevBas.levponr LevBas.levpadr LevBas.levtel LevBas.telefax ~
LevBas.telex LevBas.levland LevBas.kommentar[1] LevBas.kommentar[2] ~
LevBas.kommentar[3] LevBas.kommentar[4] LevBas.valkod LevBas.levkon ~
LevBas.koadr LevBas.koponr LevBas.kopadr LevBas.kotel LevBas.kotelefax ~
LevBas.kotelex LevBas.koland LevBas.Notat Valuta.ValKurs Valuta.ValDatum 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame LevBas.levnr ~
LevBas.levnamn LevBas.levadr LevBas.levponr LevBas.levpadr LevBas.levtel ~
LevBas.telefax LevBas.telex LevBas.levland LevBas.kommentar[1] ~
LevBas.kommentar[2] LevBas.kommentar[3] LevBas.kommentar[4] LevBas.valkod ~
LevBas.levkon LevBas.koadr LevBas.koponr LevBas.kopadr LevBas.kotel ~
LevBas.kotelefax LevBas.kotelex LevBas.koland LevBas.Notat 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame LevBas
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame LevBas
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH LevBas SHARE-LOCK, ~
      EACH Valuta OF LevBas SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame LevBas Valuta
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame LevBas
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame Valuta


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS LevBas.levnr LevBas.levnamn LevBas.levadr ~
LevBas.levponr LevBas.levpadr LevBas.levtel LevBas.telefax LevBas.telex ~
LevBas.levland LevBas.kommentar[1] LevBas.kommentar[2] LevBas.kommentar[3] ~
LevBas.kommentar[4] LevBas.valkod LevBas.levkon LevBas.koadr LevBas.koponr ~
LevBas.kopadr LevBas.kotel LevBas.kotelefax LevBas.kotelex LevBas.koland ~
LevBas.Notat 
&Scoped-define ENABLED-TABLES LevBas
&Scoped-define FIRST-ENABLED-TABLE LevBas
&Scoped-define DISPLAYED-TABLES LevBas Valuta
&Scoped-define FIRST-DISPLAYED-TABLE LevBas
&Scoped-define SECOND-DISPLAYED-TABLE Valuta
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 RECT-4 RECT-5 RECT-7 RECT-8 ~
RECT-9 Btn_OK Btn_Cancel B-KonvTbl BUTTON-Sortiment BUTTON-Artikkler ~
BUTTON-Bestillinger BUTTON-Ordrer Btn_Help BUTTON-Post1 BUTTON-Post-3 ~
BUTTON-Post2 
&Scoped-Define DISPLAYED-FIELDS LevBas.levnr LevBas.levnamn LevBas.levadr ~
LevBas.levponr LevBas.levpadr LevBas.levtel LevBas.telefax LevBas.telex ~
LevBas.levland LevBas.kommentar[1] LevBas.kommentar[2] LevBas.kommentar[3] ~
LevBas.kommentar[4] LevBas.valkod LevBas.levkon LevBas.koadr LevBas.koponr ~
LevBas.kopadr LevBas.kotel LevBas.kotelefax LevBas.kotelex LevBas.koland ~
LevBas.Notat Valuta.ValKurs Valuta.ValDatum 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-KonvTbl 
     LABEL "Konverteringstabell..." 
     SIZE 22 BY 1.14.

DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
     LABEL "Avbryt" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_Help 
     LABEL "&Hjelp" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
     LABEL "OK" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-Artikkler 
     LABEL "Artikkler..." 
     SIZE 14.8 BY 1.14.

DEFINE BUTTON BUTTON-Bestillinger 
     LABEL "Bestillinger..." 
     SIZE 16.4 BY 1.14.

DEFINE BUTTON BUTTON-Ordrer 
     LABEL "Ordrer...." 
     SIZE 16 BY 1.14.

DEFINE BUTTON BUTTON-Post-3 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Post1 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Post2 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-Sortiment 
     LABEL "Sortiment..." 
     SIZE 15.4 BY 1.14.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77.8 BY 1.71.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 77.8 BY 4.48.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59.8 BY 6.57.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59.6 BY 8.86.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 8.86.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 59.4 BY 1.33.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 78 BY 1.33.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      LevBas, 
      Valuta SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     LevBas.levnr AT ROW 1.67 COL 16.2 COLON-ALIGNED
          LABEL "Leverandør" FORMAT "zzzzz9"
          VIEW-AS FILL-IN 
          SIZE 10.8 BY 1
     LevBas.levnamn AT ROW 1.67 COL 27 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 51.6 BY 1
     LevBas.levadr AT ROW 3.57 COL 16.4 COLON-ALIGNED
          LABEL "Adresse"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.levponr AT ROW 4.57 COL 16.4 COLON-ALIGNED
          LABEL "PostNummer"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     LevBas.levpadr AT ROW 4.57 COL 33 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 25.4 BY 1
     LevBas.levtel AT ROW 6.57 COL 16.4 COLON-ALIGNED
          LABEL "Telefon" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     LevBas.telefax AT ROW 7.57 COL 16.4 COLON-ALIGNED
          LABEL "Telefaks" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     LevBas.telex AT ROW 8.57 COL 16.4 COLON-ALIGNED
          LABEL "Mobil" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     LevBas.levland AT ROW 10.57 COL 16.6 COLON-ALIGNED
          LABEL "Land" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.kommentar[1] AT ROW 12.57 COL 16.6 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
     LevBas.kommentar[2] AT ROW 13.57 COL 16.6 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
     LevBas.kommentar[3] AT ROW 14.57 COL 16.6 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
     LevBas.kommentar[4] AT ROW 15.57 COL 16.6 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 62 BY 1
     LevBas.valkod AT ROW 17.57 COL 16.6 COLON-ALIGNED
          LABEL "Kode"
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     LevBas.levkon AT ROW 1.67 COL 97 COLON-ALIGNED
          LABEL "Navn" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.koadr AT ROW 3.57 COL 96.8 COLON-ALIGNED
          LABEL "Adresse" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.koponr AT ROW 4.57 COL 97 COLON-ALIGNED
          LABEL "Postnummer"
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     LevBas.kopadr AT ROW 4.57 COL 113.6 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 25.4 BY 1
     LevBas.kotel AT ROW 6.57 COL 97 COLON-ALIGNED
          LABEL "Telefon" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     LevBas.kotelefax AT ROW 7.57 COL 97 COLON-ALIGNED
          LABEL "Telefaks" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     LevBas.kotelex AT ROW 8.57 COL 97 COLON-ALIGNED
          LABEL "Mobil" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     LevBas.koland AT ROW 10.57 COL 96.8 COLON-ALIGNED
          LABEL "Land" FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     LevBas.Notat AT ROW 12.62 COL 83.2 HELP
          "Notatfelt" NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 58 BY 6 TOOLTIP "Notat om leverandøren. Betalingsbetingelser o.l."
.
/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     Btn_OK AT ROW 19.19 COL 3
     Btn_Cancel AT ROW 19.19 COL 18.4
     B-KonvTbl AT ROW 19.19 COL 37.6
     BUTTON-Sortiment AT ROW 19.19 COL 59.8
     BUTTON-Artikkler AT ROW 19.19 COL 75.4
     BUTTON-Bestillinger AT ROW 19.19 COL 90.4
     BUTTON-Ordrer AT ROW 19.19 COL 107
     Btn_Help AT ROW 19.19 COL 126.6
     Valuta.ValKurs AT ROW 17.57 COL 37 COLON-ALIGNED
          LABEL "Kurs"
          VIEW-AS FILL-IN 
          SIZE 14.6 BY 1
     BUTTON-Post1 AT ROW 4.57 COL 30.2
     Valuta.ValDatum AT ROW 17.57 COL 65.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     BUTTON-Post-3 AT ROW 17.57 COL 27.4
     BUTTON-Post2 AT ROW 4.57 COL 110.8
     RECT-2 AT ROW 17.19 COL 3.2
     RECT-3 AT ROW 12.33 COL 3.2
     RECT-4 AT ROW 12.33 COL 82
     RECT-5 AT ROW 3.1 COL 82
     RECT-7 AT ROW 3.1 COL 3.2
     RECT-8 AT ROW 1.48 COL 82.2
     RECT-9 AT ROW 1.48 COL 3.2
     "Kontoradresse" VIEW-AS TEXT
          SIZE 15.6 BY .62 AT ROW 2.81 COL 4.6
     "Merknad" VIEW-AS TEXT
          SIZE 10.2 BY .62 AT ROW 12.05 COL 4.4
     "Valuta" VIEW-AS TEXT
          SIZE 8.6 BY .62 AT ROW 16.81 COL 4.6
     "Kontaktens adresse" VIEW-AS TEXT
          SIZE 20.2 BY .62 AT ROW 2.81 COL 83.6
     "Notat" VIEW-AS TEXT
          SIZE 7.8 BY .62 AT ROW 12.05 COL 83
     SPACE(51.19) SKIP(7.80)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedlikehold leverandørregister"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN LevBas.koadr IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.koland IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.kommentar[1] IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.kommentar[2] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN LevBas.kommentar[3] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN LevBas.kommentar[4] IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN LevBas.kopadr IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN LevBas.koponr IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN LevBas.kotel IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.kotelefax IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.kotelex IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.levadr IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN LevBas.levkon IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.levland IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.levnamn IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN LevBas.levnr IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.levpadr IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN LevBas.levponr IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN LevBas.levtel IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR EDITOR LevBas.Notat IN FRAME Dialog-Frame
   EXP-HELP                                                             */
ASSIGN 
       LevBas.Notat:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE.

/* SETTINGS FOR FILL-IN LevBas.telefax IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN LevBas.telex IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Valuta.ValDatum IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LevBas.valkod IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Valuta.ValKurs IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.LevBas,SkoTex.Valuta OF SkoTex.LevBas"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Vedlikehold leverandørregister */
DO:
  run LagrePost.
  if return-value = "AVBRYT" then
    return no-apply.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedlikehold leverandørregister */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-KonvTbl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-KonvTbl Dialog-Frame
ON CHOOSE OF B-KonvTbl IN FRAME Dialog-Frame /* Konverteringstabell... */
DO:
  if not available LevBas then
    return.
  run d-bkonvlevbas.w (input LevBas.LevNr).
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Help
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Help Dialog-Frame
ON CHOOSE OF Btn_Help IN FRAME Dialog-Frame /* Hjelp */
OR HELP OF FRAME {&FRAME-NAME}
DO: /* Call Help Function (or a simple message). */
  {diahelp.i}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Artikkler
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Artikkler Dialog-Frame
ON CHOOSE OF BUTTON-Artikkler IN FRAME Dialog-Frame /* Artikkler... */
DO:
  def var wLevKod like ArtBas.LevKod no-undo.
  assign
    wLevKod = "".
  run d-blevartikkler.w (input-output wLevKod, input recid(LevBas)).
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Post-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Post-3 Dialog-Frame
ON CHOOSE OF BUTTON-Post-3 IN FRAME Dialog-Frame /* ... */
or F10 of LevBas.ValKod
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = LevBas.ValKod
    &Program     = d-bvaluta.w
    &Frame       = Dialog-Frame
    &PostRun     = "find Valuta no-lock where
                    recid(Valuta) = int(return-value) no-error."
    &OptDisp     = "Valuta.ValDatum Valuta.ValKurs
                    when available Valuta"
  } 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Post1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Post1 Dialog-Frame
ON CHOOSE OF BUTTON-Post1 IN FRAME Dialog-Frame /* ... */
or F10 of LevBas.LevPoNr
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = LevBas.LevPoNr
    &Program     = d-bpost.w
    &Frame       = Dialog-Frame
    &PostRun     = "find Post no-lock where
                    recid(Post) = int(return-value) no-error."
    &OptDisp     = "Post.Beskrivelse when available Post @ LevBas.levpadr "
  } 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Post2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Post2 Dialog-Frame
ON CHOOSE OF BUTTON-Post2 IN FRAME Dialog-Frame /* ... */
or F10 of LevBas.KoPoNr
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = LevBas.KoPoNr
    &Program     = d-bpost.w
    &Frame       = Dialog-Frame
    &PostRun     = "find Post no-lock where
                    recid(Post) = int(return-value) no-error."
    &OptDisp     = "Post.Beskrivelse when available Post @ LevBas.KoPAdr"
  } 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Sortiment
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sortiment Dialog-Frame
ON CHOOSE OF BUTTON-Sortiment IN FRAME Dialog-Frame /* Sortiment... */
DO:
  if available LevBas then
    run d-blevsort.w (input-output wSortId, input recid(LevBas)).
  return no-apply.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevBas.koponr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevBas.koponr Dialog-Frame
ON LEAVE OF LevBas.koponr IN FRAME Dialog-Frame /* Postnummer */
DO:
  find Post no-lock where
    Post.PostNr = input LevBas.KoPoNr no-error.
   if not available Post then
     do:
       message "Ukjent postnummer!"
         view-as alert-box title "Melding".
       return no-apply.
     end.
   else
     display Post.Beskrivelse @ LevBas.KoPAdr with frame Dialog-Frame.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevBas.levponr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevBas.levponr Dialog-Frame
ON LEAVE OF LevBas.levponr IN FRAME Dialog-Frame /* PostNummer */
DO:
  find Post no-lock where
    Post.PostNr = input LevBas.LevPoNr no-error.
   if not available Post then
     do:
       message "Ukjent postnummer!"
         view-as alert-box title "Melding".
       return no-apply.
     end.
   else
     display Post.Beskrivelse @ LevBas.LevPAdr with frame Dialog-Frame.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME LevBas.valkod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL LevBas.valkod Dialog-Frame
ON LEAVE OF LevBas.valkod IN FRAME Dialog-Frame /* Kode */
or "RETURN":U of LevBas.ValKod
DO:
  find Valuta no-lock where
    Valuta.ValKod = input LevBas.ValKod no-error.
  if not available Valuta then
    do:
      message "Ukjent Valuta!" view-as alert-box  title "Melding".
      return no-apply.
    end.
  if available Valuta then
    display
      Valuta.ValDatum when available Valuta
      Valuta.ValKurs when available Valuta
    with frame Dialog-Frame.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Dialog-Frame 


/* ***************************  Main Block  *************************** */

/* Parent the dialog-box to the ACTIVE-WINDOW, if there is no parent.   */
IF VALID-HANDLE(ACTIVE-WINDOW) AND FRAME {&FRAME-NAME}:PARENT eq ?
THEN FRAME {&FRAME-NAME}:PARENT = ACTIVE-WINDOW.

find {&br-tabell} no-lock where
  recid({&br-tabell}) = wRecid no-error.
if available {&br-tabell} then 
  do: 
    {&FinnRelatertePoster}  
  end.
      
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  {lng.i} RUN enable_UI.

  run VisPost.
  
  if wModus = "Ny" then
    assign
      {&br-tabell}.{&KeyFelt}:sensitive = true.
  else
    assign
      {&br-tabell}.{&KeyFelt}:sensitive = false.
  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
  wRetur-Verdi = "OK".
END.
RUN disable_UI.

&IF DEFINED(UIB_IS_RUNNING) EQ 0 &THEN
 return wretur-verdi.
&else
 message wretur-verdi view-as alert-box.
&endif

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame  _DEFAULT-DISABLE
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
  HIDE FRAME Dialog-Frame.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame  _DEFAULT-ENABLE
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
  IF AVAILABLE LevBas THEN 
    DISPLAY LevBas.levnr LevBas.levnamn LevBas.levadr LevBas.levponr 
          LevBas.levpadr LevBas.levtel LevBas.telefax LevBas.telex 
          LevBas.levland LevBas.kommentar[1] LevBas.kommentar[2] 
          LevBas.kommentar[3] LevBas.kommentar[4] LevBas.valkod LevBas.levkon 
          LevBas.koadr LevBas.koponr LevBas.kopadr LevBas.kotel LevBas.kotelefax 
          LevBas.kotelex LevBas.koland LevBas.Notat 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE Valuta THEN 
    DISPLAY Valuta.ValKurs Valuta.ValDatum 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-2 RECT-3 RECT-4 RECT-5 RECT-7 RECT-8 RECT-9 LevBas.levnr 
         LevBas.levnamn LevBas.levadr LevBas.levponr LevBas.levpadr 
         LevBas.levtel LevBas.telefax LevBas.telex LevBas.levland 
         LevBas.kommentar[1] LevBas.kommentar[2] LevBas.kommentar[3] 
         LevBas.kommentar[4] LevBas.valkod LevBas.levkon LevBas.koadr 
         LevBas.koponr LevBas.kopadr LevBas.kotel LevBas.kotelefax 
         LevBas.kotelex LevBas.koland LevBas.Notat Btn_OK Btn_Cancel B-KonvTbl 
         BUTTON-Sortiment BUTTON-Artikkler BUTTON-Bestillinger BUTTON-Ordrer 
         Btn_Help BUTTON-Post1 BUTTON-Post-3 BUTTON-Post2 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LagrePost Dialog-Frame 
PROCEDURE LagrePost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

do with frame Dialog-Frame transaction:
  /* Sjekker nye poster. */
  if wModus = "Ny" then
    do:
      {&SjekkOmPostFinnes}
      create {&br-tabell}.
      assign 
        {&OptFind}
        {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)
        wRecid   = recid({&br-tabell}).
    end.
  else 
    find {&br-tabell} Exclusive-lock where
      recid({&br-tabell}) = wRecid no-error.
  assign
    {&AssignFelter}.
  {&TillegsAssign}
end. /* TRANSACTION */    

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VisPost Dialog-Frame 
PROCEDURE VisPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  display 
    {&VisPoster}
  with frame Dialog-Frame.
  {&VisAndreData}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

