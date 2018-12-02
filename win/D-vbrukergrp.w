&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame

/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE tt-Butiker NO-UNDO LIKE Butiker
       FIELD NyElValgt AS CHAR.
DEFINE TEMP-TABLE tt-Valgte NO-UNDO LIKE Butiker
       FIELD NyElValgt AS CHAR.


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
  define var wModus as char init "Ny" no-undo.
  
&ELSE
  DEFINE INPUT-output PARAMETER wRecid as recid NO-UNDO.
  define input        parameter wModus as char  no-undo.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell BrukerGrp
&scoped-define KeyFelt BrGrpNr
&scoped-define OptKeyAssign

&scoped-define DataType INT /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster 

/* Ekstra informasjon i find/where når det er flere ledd i indeks */
/* Brukes før KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster {&br-tabell}.BrGrpNr when available {&br-tabell} ~
                         {&br-tabell}.Beskrivelse when available {&br-tabell} ~
                         {&br-tabell}.Notat when available {&br-tabell}

                         
/* Alternative poster som skal vises når VisPost kjøres */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData IF AVAIL {&br-tabell} THEN ~
  assign ~
    CB-Meny:SCREEN-VALUE      = entry(1,{&br-tabell}.Navn) ~
    FI-MenyListe:SCREEN-VALUE = {&br-tabell}.Navn ~
    FI-MenyListe              = {&br-tabell}.Navn ~
    FI-KnappeListe:SCREEN-VALUE    = {&br-tabell}.KnappeListe ~
    FI-KnappeListe            = {&br-tabell}.KnappeListe.


/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes if can-find({&br-tabell} where ~
         {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)) or ~
         int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
        do: ~
          if int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
            message "{&KeyFelt} må være større enn 0" ~
            view-as alert-box title "Lagringsfeil". ~
          else ~
            message "{&br-tabell} finnes allerede med nr:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
          return "AVBRYT". ~
        end.
        
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter {&br-tabell}.Beskrivelse ~
                            {&br-tabell}.Notat


/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
/* Står etter forrige assign. Dvs egen linje.                   */
&scoped-define TillegsAssign ASSIGN ~
               {&br-tabell}.NAVN        = FI-MenyListe:SCREEN-VALUE ~
               {&br-tabell}.KnappeListe = FI-KnappeListe:SCREEN-VALUE.

&scoped-define EkstraSjekk IF {&br-tabell}.Beskrivelse:SCREEN-VALUE = "" THEN DO: ~
                 MESSAGE "Feltet må registreres." VIEW-AS ALERT-BOX TITLE "Lagringsfeil". ~
                 APPLY "ENTRY" TO {&br-tabell}.Beskrivelse. ~
                 RETURN "AVBRYT". ~
             END. ~
             IF NOT CAN-FIND(FIRST tt-Valgte) THEN DO: ~
                 MESSAGE "Minst en butik må velges." VIEW-AS ALERT-BOX TITLE "Lagringsfeil". ~
                 RETURN "AVBRYT". ~
             END. 
/* Local Variable Definitions ---                                       */
def var wRetur-Verdi as char initial "AVBRYT" no-undo.

DEFINE BUFFER btt-Butiker FOR tt-Butiker.
DEFINE BUFFER btt-Valgte  FOR tt-Valgte.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame
&Scoped-define BROWSE-NAME BROWSE-Butiker

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES tt-Butiker tt-Valgte BrukerGrp

/* Definitions for BROWSE BROWSE-Butiker                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Butiker tt-Butiker.Butik tt-Butiker.ButNamn   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Butiker   
&Scoped-define SELF-NAME BROWSE-Butiker
&Scoped-define QUERY-STRING-BROWSE-Butiker FOR EACH tt-Butiker
&Scoped-define OPEN-QUERY-BROWSE-Butiker OPEN QUERY BROWSE-Butiker FOR EACH tt-Butiker.
&Scoped-define TABLES-IN-QUERY-BROWSE-Butiker tt-Butiker
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Butiker tt-Butiker


/* Definitions for BROWSE BROWSE-Valgte                                 */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Valgte tt-Valgte.Butik tt-Valgte.ButNamn   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Valgte   
&Scoped-define SELF-NAME BROWSE-Valgte
&Scoped-define QUERY-STRING-BROWSE-Valgte FOR EACH tt-Valgte
&Scoped-define OPEN-QUERY-BROWSE-Valgte OPEN QUERY BROWSE-Valgte FOR EACH tt-Valgte.
&Scoped-define TABLES-IN-QUERY-BROWSE-Valgte tt-Valgte
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Valgte tt-Valgte


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame BrukerGrp.BrGrpNr ~
BrukerGrp.Beskrivelse BrukerGrp.Notat 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame BrukerGrp.Beskrivelse ~
BrukerGrp.Notat 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame BrukerGrp
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame BrukerGrp
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-Butiker}~
    ~{&OPEN-QUERY-BROWSE-Valgte}
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH BrukerGrp SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH BrukerGrp SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame BrukerGrp
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame BrukerGrp


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS BrukerGrp.Beskrivelse BrukerGrp.Notat 
&Scoped-define ENABLED-TABLES BrukerGrp
&Scoped-define FIRST-ENABLED-TABLE BrukerGrp
&Scoped-Define ENABLED-OBJECTS BUTTON-Fra BUTTON-Til FI-MenyListe ~
BROWSE-Butiker BROWSE-Valgte Btn_OK Btn_Cancel CB-Meny Btn_Help ~
FI-KnappeListe CB-Knapper RECT-1 RECT-2 
&Scoped-Define DISPLAYED-FIELDS BrukerGrp.BrGrpNr BrukerGrp.Beskrivelse ~
BrukerGrp.Notat 
&Scoped-define DISPLAYED-TABLES BrukerGrp
&Scoped-define FIRST-DISPLAYED-TABLE BrukerGrp
&Scoped-Define DISPLAYED-OBJECTS FI-MenyListe CB-Meny FI-KnappeListe ~
CB-Knapper 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
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

DEFINE BUTTON BUTTON-Fra  NO-FOCUS
     LABEL " <" 
     SIZE 4 BY 1.14.

DEFINE BUTTON BUTTON-Til  NO-FOCUS
     LABEL " >" 
     SIZE 4 BY 1.14.

DEFINE VARIABLE CB-Knapper AS CHARACTER FORMAT "X(256)":U 
     LABEL "Knapper" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 48.6 BY 1 NO-UNDO.

DEFINE VARIABLE CB-Meny AS CHARACTER FORMAT "X(256)":U 
     LABEL "Meny" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     LIST-ITEMS "Item 1" 
     DROP-DOWN-LIST
     SIZE 48.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-KnappeListe AS CHARACTER FORMAT "X(256)":U 
     LABEL "Knappemeny" 
     VIEW-AS FILL-IN 
     SIZE 48.6 BY 1 NO-UNDO.

DEFINE VARIABLE FI-MenyListe AS CHARACTER FORMAT "X(256)":U 
     LABEL "Menyliste" 
     VIEW-AS FILL-IN 
     SIZE 48.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 67 BY 21.19.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 73 BY 21.19.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Butiker FOR 
      tt-Butiker SCROLLING.

DEFINE QUERY BROWSE-Valgte FOR 
      tt-Valgte SCROLLING.

DEFINE QUERY Dialog-Frame FOR 
      BrukerGrp SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Butiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Butiker Dialog-Frame _FREEFORM
  QUERY BROWSE-Butiker DISPLAY
      tt-Butiker.Butik
tt-Butiker.ButNamn
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-TAB-STOP SIZE 30 BY 19.24 ROW-HEIGHT-CHARS .61 EXPANDABLE.

DEFINE BROWSE BROWSE-Valgte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Valgte Dialog-Frame _FREEFORM
  QUERY BROWSE-Valgte DISPLAY
      tt-Valgte.Butik
tt-Valgte.ButNamn
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS NO-TAB-STOP SIZE 30 BY 19.24 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BUTTON-Fra AT ROW 7.76 COL 104.8 NO-TAB-STOP 
     BUTTON-Til AT ROW 5.86 COL 104.8 NO-TAB-STOP 
     BrukerGrp.BrGrpNr AT ROW 2.76 COL 15.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     BrukerGrp.Beskrivelse AT ROW 2.76 COL 22 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     FI-MenyListe AT ROW 4.86 COL 15.4 COLON-ALIGNED
     BrukerGrp.Notat AT ROW 8.38 COL 17.2 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 48.8 BY 13.57
     BROWSE-Butiker AT ROW 2.71 COL 72
     BROWSE-Valgte AT ROW 2.71 COL 111
     Btn_OK AT ROW 23.14 COL 2
     Btn_Cancel AT ROW 23.14 COL 17.8
     CB-Meny AT ROW 3.81 COL 15.4 COLON-ALIGNED NO-TAB-STOP 
     Btn_Help AT ROW 23.14 COL 128
     FI-KnappeListe AT ROW 7.19 COL 15.4 COLON-ALIGNED
     CB-Knapper AT ROW 6 COL 15.4 COLON-ALIGNED NO-TAB-STOP 
     RECT-1 AT ROW 1.48 COL 2
     RECT-2 AT ROW 1.48 COL 70
     "Tilgjenglige butikker" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 1.86 COL 74
          FONT 6
     "Valgte butikker" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 1.86 COL 114
          FONT 6
     SPACE(5.99) SKIP(21.99)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Brukergruppe"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tt-Butiker T "?" NO-UNDO SkoTex Butiker
      ADDITIONAL-FIELDS:
          FIELD NyElValgt AS CHAR
      END-FIELDS.
      TABLE: tt-Valgte T "?" NO-UNDO SkoTex Butiker
      ADDITIONAL-FIELDS:
          FIELD NyElValgt AS CHAR
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   Custom                                                               */
/* BROWSE-TAB BROWSE-Butiker Notat Dialog-Frame */
/* BROWSE-TAB BROWSE-Valgte BROWSE-Butiker Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN BrukerGrp.BrGrpNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BrukerGrp.Notat:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Butiker
/* Query rebuild information for BROWSE BROWSE-Butiker
     _START_FREEFORM
OPEN QUERY BROWSE-Butiker FOR EACH tt-Butiker.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-Butiker */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-Valgte
/* Query rebuild information for BROWSE BROWSE-Valgte
     _START_FREEFORM
OPEN QUERY BROWSE-Valgte FOR EACH tt-Valgte.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-Valgte */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.BrukerGrp"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Brukergruppe */
DO:
  APPLY "END-ERROR":U TO SELF.
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


&Scoped-define SELF-NAME Btn_OK
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OK Dialog-Frame
ON CHOOSE OF Btn_OK IN FRAME Dialog-Frame /* OK */
DO:
  run LagrePost.
  if return-value = "AVBRYT" then
    return no-apply.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Fra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Fra Dialog-Frame
ON CHOOSE OF BUTTON-Fra IN FRAME Dialog-Frame /*  < */
DO:
   BUFFER-COPY tt-Valgte USING Butik ButNamn NyElValgt TO btt-Butiker.
  RELEASE btt-Butiker.
  {&OPEN-QUERY-BROWSE-Butiker}
  DELETE tt-Valgte.
  BROWSE-Valgte:DELETE-SELECTED-ROWS().
  ASSIGN SELF:SENSITIVE = BROWSE-Valgte:FOCUSED-ROW <> ?
         BUTTON-Til:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-Til
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Til Dialog-Frame
ON CHOOSE OF BUTTON-Til IN FRAME Dialog-Frame /*  > */
DO:
   BUFFER-COPY tt-Butiker USING Butik ButNamn NyElValgt TO btt-Valgte.
  RELEASE btt-Valgte.
  {&OPEN-QUERY-BROWSE-Valgte}
  DELETE tt-Butiker.
  BROWSE-Butiker:DELETE-SELECTED-ROWS().
  ASSIGN SELF:SENSITIVE = BROWSE-Butiker:FOCUSED-ROW <> ?
         BUTTON-Fra:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Knapper
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Knapper Dialog-Frame
ON VALUE-CHANGED OF CB-Knapper IN FRAME Dialog-Frame /* Knapper */
DO:
  assign
    CB-Knapper.
  if not can-do(FI-KnappeListe,CB-Knapper) then
    FI-KnappeListe = CB-Knapper.
  display
    FI-KnappeListe
  with frame Dialog-Frame.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME CB-Meny
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Meny Dialog-Frame
ON VALUE-CHANGED OF CB-Meny IN FRAME Dialog-Frame /* Meny */
DO:
  assign
    CB-Meny.
  if not can-do(FI-MenyListe,CB-Meny) then
    FI-MenyListe = FI-MenyListe + 
                   (if FI-MenyListe = ""
                      then ""
                      else ",") + 
                   CB-Meny.
  display
    FI-MenyListe
  with frame Dialog-Frame.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-Butiker
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
IF AVAIL {&br-tabell} AND {&br-tabell}.{&KeyFelt} = 99 THEN DO:
    IF wModus <> "HKEKSPORT" THEN DO:
        MESSAGE "Denne post vedlikeholdes fra HK-eksport!"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
END.
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   def var wStr AS CHAR.
  FOR EACH Meny NO-LOCK:
      wStr = 
          IF wStr = "" THEN Meny.Navn ELSE wStr + "," + Meny.Navn.
  END.
  RUN InitTT.
  {lng.i} /* Oversettelse */
  RUN enable_UI.
  IF wModus = "HKEKSPORT" THEN DO:
      ASSIGN wModus = "ENDRE"
             CB-Knapper:SENSITIVE = FALSE
             CB-Meny:SENSITIVE = FALSE
             FI-KnappeListe:SENSITIVE = FALSE
             FI-MenyListe:SENSITIVE = FALSE
             skotex.BrukerGrp.Beskrivelse:SENSITIVE = FALSE
             skotex.BrukerGrp.BrGrpNr:SENSITIVE = FALSE
             skotex.BrukerGrp.Notat:SENSITIVE = FALSE.
  END.
  assign
      CB-Meny:LIST-ITEMS    = wStr
      CB-Knapper:LIST-ITEMS = wStr
      .
  IF wModus = "Ny" THEN CB-Meny:SCREEN-VALUE = ENTRY(1,CB-Meny:LIST-ITEMS).
        ELSE CB-Meny:SCREEN-VALUE = entry(1,{&br-tabell}.Navn).
  IF wModus = "Ny" THEN CB-Knapper:SCREEN-VALUE = ENTRY(1,CB-Knapper:LIST-ITEMS).
        ELSE CB-Knapper:SCREEN-VALUE = entry(1,{&br-tabell}.KnappeListe).
  run VisPost.
  ASSIGN BUTTON-Til:SENSITIVE = BROWSE-Butiker:FOCUSED-ROW <> ?
         BUTTON-Fra:SENSITIVE = BROWSE-Valgte:FOCUSED-ROW <> ?.

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
  DISPLAY FI-MenyListe CB-Meny FI-KnappeListe CB-Knapper 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE BrukerGrp THEN 
    DISPLAY BrukerGrp.BrGrpNr BrukerGrp.Beskrivelse BrukerGrp.Notat 
      WITH FRAME Dialog-Frame.
  ENABLE BUTTON-Fra BUTTON-Til BrukerGrp.Beskrivelse FI-MenyListe 
         BrukerGrp.Notat BROWSE-Butiker BROWSE-Valgte Btn_OK Btn_Cancel CB-Meny 
         Btn_Help FI-KnappeListe CB-Knapper RECT-1 RECT-2 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitTT Dialog-Frame 
PROCEDURE InitTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF AVAIL BrukerGrp THEN DO:
    FOR EACH ButikkTilgang WHERE ButikkTilgang.BrGrpNr = BrukerGrp.BrGrpNr NO-LOCK:
        FIND Butiker WHERE Butiker.Butik = ButikkTilgang.Butik NO-LOCK NO-ERROR.
        IF AVAIL Butiker THEN DO:
          BUFFER-COPY Butiker USING Butik ButNamn TO btt-Valgte.
          ASSIGN btt-Valgte.NyElValgt = "V".
          RELEASE btt-Valgte.
        END.
    END.
  END.
  FOR EACH Butiker NO-LOCK WHERE NOT CAN-FIND(btt-Valgte WHERE 
                                      btt-Valgte.Butik = Butiker.Butik).
    BUFFER-COPY Butiker USING Butik ButNamn TO btt-Butiker.
    ASSIGN  btt-Butiker.NyElValgt = "N".
    RELEASE btt-Butiker.
  END.
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
      {&EkstraSjekk}
      {&OptFind}
      create {&br-tabell}.
      assign 
        {&OptKeyAssign}
        {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)
        wRecid   = recid({&br-tabell}).
    end.
  else do:
    find {&br-tabell} Exclusive-lock where
      recid({&br-tabell}) = wRecid no-error.
      {&EkstraSjekk}
  end.
  assign
    {&AssignFelter}.
  {&TillegsAssign}
end. /* TRANSACTION */    
FOR EACH tt-Butiker WHERE tt-Butiker.NyElValgt = "V":
  FIND ButikkTilgang WHERE ButikkTilgang.BrGrpNr = BrukerGrp.BrGrpNr AND
                           ButikkTilgang.Butik   = tt-Butiker.Butik.
  IF AVAIL ButikkTilgang THEN
    DELETE ButikkTilgang.
END.
FOR EACH tt-Valgte WHERE tt-Valgte.NyElValgt = "N":
  CREATE ButikkTilgang.
  ASSIGN ButikkTilgang.BrGrpNr = BrukerGrp.BrGrpNr
         ButikkTilgang.Butik   = tt-Valgte.Butik.
  RELEASE ButikkTilgang.
END.
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

