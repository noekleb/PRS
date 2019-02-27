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
  DEFINE VAR wBrGrpNr LIKE BrukerGrp.BrGrpNr  INIT 1    NO-UNDO.
  DEFINE VAR wTeamTypeId LIKE TeamType.TeamTypeId INIT 1 NO-UNDO.
  DEFINE VAR wRecid as recid NO-UNDO.
  define var wModus as char init "Ny" no-undo.
  
&ELSE
  DEFINE INPUT        PARAMETER wBrGrpNr LIKE BrukerGrp.BrGrpNr      NO-UNDO.
  DEFINE INPUT        PARAMETER wTeamTypeId LIKE TeamType.TeamTypeId NO-UNDO.
  DEFINE INPUT-output PARAMETER wRecid as recid NO-UNDO.
  define input        parameter wModus as char  no-undo.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell ButikkTeam
&scoped-define KeyFelt TeamNr
&scoped-define OptKeyAssign {&br-tabell}.BrGrpNr = wBrGrpNr ~
                            {&br-tabell}.TeamTypeId = wTeamTypeId

&scoped-define DataType INT /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster 

/* Ekstra informasjon i find/where når det er flere ledd i indeks */
/* Brukes før KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster {&br-tabell}.BrGrpNr when available {&br-tabell} ~
                         {&br-tabell}.TeamTypeId when available {&br-tabell} ~
                         {&br-tabell}.TeamNr when available {&br-tabell} ~
                         {&br-tabell}.Beskrivelse when available {&br-tabell}
                         
/* Alternative poster som skal vises når VisPost kjøres */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData


/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes if can-find({&br-tabell} where ~
         ({&br-tabell}.BrGrpNr = wBrGrpNr AND ~
         {&br-tabell}.TeamTypeId = wTeamTypeId AND ~
         {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value))) or ~
         int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
        do: ~
          if int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
            message "{&KeyFelt} må være større enn 0" ~
            view-as alert-box title "Lagringsfeil". ~
          else ~
            message "{&br-tabell} finnes allerede med nr:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
          APPLY "ENTRY" TO {&br-tabell}.{&KeyFelt}. ~
          return "AVBRYT". ~
        end.
        
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter {&br-tabell}.Beskrivelse ~
                            {&br-tabell}.Notat
&scoped-define EkstraSjekk IF {&br-tabell}.Beskrivelse:SCREEN-VALUE = "" THEN DO: ~
                 MESSAGE "Feltet må registreres." VIEW-AS ALERT-BOX TITLE "Lagringsfeil". ~
                 APPLY "ENTRY" TO {&br-tabell}.Beskrivelse. ~
                 RETURN "AVBRYT". ~
             END. ~
             IF NOT CAN-FIND(FIRST tt-Valgte) THEN DO: ~
                 MESSAGE "Minst en butik må velges." VIEW-AS ALERT-BOX TITLE "Lagringsfeil". ~
                 RETURN "AVBRYT". ~
             END. 


/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
/* Står etter forrige assign. Dvs egen linje.                   */
&scoped-define TillegsAssign

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
&Scoped-define INTERNAL-TABLES tt-Butiker tt-Valgte ButikkTeam

/* Definitions for BROWSE BROWSE-Butiker                                */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Butiker tt-Butiker.Butik tt-Butiker.ButNamn   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Butiker   
&Scoped-define SELF-NAME BROWSE-Butiker
&Scoped-define OPEN-QUERY-BROWSE-Butiker OPEN QUERY BROWSE-Butiker FOR EACH tt-Butiker.
&Scoped-define TABLES-IN-QUERY-BROWSE-Butiker tt-Butiker
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Butiker tt-Butiker


/* Definitions for BROWSE BROWSE-Valgte                                 */
&Scoped-define FIELDS-IN-QUERY-BROWSE-Valgte tt-Valgte.Butik tt-Valgte.ButNamn   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-Valgte   
&Scoped-define SELF-NAME BROWSE-Valgte
&Scoped-define OPEN-QUERY-BROWSE-Valgte OPEN QUERY BROWSE-Valgte FOR EACH tt-Valgte.
&Scoped-define TABLES-IN-QUERY-BROWSE-Valgte tt-Valgte
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-Valgte tt-Valgte


/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame ButikkTeam.BrGrpNr ~
ButikkTeam.TeamTypeId ButikkTeam.TeamNr ButikkTeam.Beskrivelse ~
ButikkTeam.Notat 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame ButikkTeam.TeamNr ~
ButikkTeam.Beskrivelse ButikkTeam.Notat 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame ButikkTeam
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame ButikkTeam
&Scoped-define OPEN-BROWSERS-IN-QUERY-Dialog-Frame ~
    ~{&OPEN-QUERY-BROWSE-Butiker}~
    ~{&OPEN-QUERY-BROWSE-Valgte}
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH ButikkTeam SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame ButikkTeam
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame ButikkTeam


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ButikkTeam.TeamNr ButikkTeam.Beskrivelse ~
ButikkTeam.Notat 
&Scoped-define ENABLED-TABLES ButikkTeam
&Scoped-define FIRST-ENABLED-TABLE ButikkTeam
&Scoped-define DISPLAYED-TABLES ButikkTeam
&Scoped-define FIRST-DISPLAYED-TABLE ButikkTeam
&Scoped-Define ENABLED-OBJECTS BROWSE-Butiker BROWSE-Valgte BUTTON-Til ~
BUTTON-Fra Btn_OK Btn_Cancel Btn_Help RECT-1 
&Scoped-Define DISPLAYED-FIELDS ButikkTeam.BrGrpNr ButikkTeam.TeamTypeId ~
ButikkTeam.TeamNr ButikkTeam.Beskrivelse ButikkTeam.Notat 
&Scoped-Define DISPLAYED-OBJECTS FI-TeamType 

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

DEFINE BUTTON BUTTON-Fra 
     LABEL " <" 
     SIZE 4 BY 1.14.

DEFINE BUTTON BUTTON-Til 
     LABEL " >" 
     SIZE 4 BY 1.14.

DEFINE VARIABLE FI-TeamType AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 24 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 69 BY 10.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-Butiker FOR 
      tt-Butiker SCROLLING.

DEFINE QUERY BROWSE-Valgte FOR 
      tt-Valgte SCROLLING.

DEFINE QUERY Dialog-Frame FOR 
      ButikkTeam SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-Butiker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Butiker Dialog-Frame _FREEFORM
  QUERY BROWSE-Butiker DISPLAY
      tt-Butiker.Butik
tt-Butiker.ButNamn
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 30 BY 10.48 EXPANDABLE.

DEFINE BROWSE BROWSE-Valgte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-Valgte Dialog-Frame _FREEFORM
  QUERY BROWSE-Valgte DISPLAY
      tt-Valgte.Butik
tt-Valgte.ButNamn
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 30 BY 10.48 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     ButikkTeam.BrGrpNr AT ROW 3.19 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ButikkTeam.TeamTypeId AT ROW 4.19 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     FI-TeamType AT ROW 4.19 COL 24.2 COLON-ALIGNED NO-LABEL
     ButikkTeam.TeamNr AT ROW 5.19 COL 17 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7 BY 1
     ButikkTeam.Beskrivelse AT ROW 5.19 COL 24.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     ButikkTeam.Notat AT ROW 6.24 COL 19 NO-LABEL
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 49.2 BY 4.48
     BROWSE-Butiker AT ROW 13.81 COL 2
     BROWSE-Valgte AT ROW 13.81 COL 40.8
     BUTTON-Til AT ROW 17.05 COL 34.2
     BUTTON-Fra AT ROW 18.95 COL 34.2
     Btn_OK AT ROW 24.57 COL 2
     Btn_Cancel AT ROW 24.57 COL 17.6
     Btn_Help AT ROW 24.57 COL 55.4
     RECT-1 AT ROW 1.48 COL 2
     "Tilgjenglige butiker" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 12.91 COL 5.8
     "Valgte butiker" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 12.91 COL 47.2
     SPACE(10.39) SKIP(12.18)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Butikkteam"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
                                                                        */
/* BROWSE-TAB BROWSE-Butiker Notat Dialog-Frame */
/* BROWSE-TAB BROWSE-Valgte BROWSE-Butiker Dialog-Frame */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN ButikkTeam.BrGrpNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-TeamType IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ButikkTeam.TeamTypeId IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
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
     _TblList          = "SkoTex.ButikkTeam"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Butikkteam */
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
      
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  FIND BrukerGrp NO-LOCK WHERE BrukerGrp.BrGrpNr = wBrGrpNr.
  FIND TeamType  NO-LOCK WHERE TeamType.TeamTypeId = wTeamTypeId.
  RUN InitTT.
  RUN enable_UI.
  ASSIGN ButikkTeam.BrGrpNr:SCREEN-VALUE = STRING(wBrGrpNr)
         ButikkTeam.TeamTypeId:SCREEN-VALUE = STRING(wTeamTypeId)
         FI-TeamType:SCREEN-VALUE = TeamType.Beskrivelse.
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
  DISPLAY FI-TeamType 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE ButikkTeam THEN 
    DISPLAY ButikkTeam.BrGrpNr ButikkTeam.TeamTypeId ButikkTeam.TeamNr 
          ButikkTeam.Beskrivelse ButikkTeam.Notat 
      WITH FRAME Dialog-Frame.
  ENABLE ButikkTeam.TeamNr ButikkTeam.Beskrivelse ButikkTeam.Notat 
         BROWSE-Butiker BROWSE-Valgte BUTTON-Til BUTTON-Fra Btn_OK Btn_Cancel 
         Btn_Help RECT-1 
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
  IF AVAIL ButikkTeam THEN DO:
    FOR EACH ButikkKobling WHERE ButikkKobling.BrGrpNr = wBrGrpNr AND
             ButikkKobling.TeamTypeId = wTeamTypeId AND
             ButikkKobling.TeamNr = ButikkTeam.TeamNr NO-LOCK:
        FIND Butiker WHERE Butiker.Butik = ButikkKobling.Butik NO-LOCK NO-ERROR.
        IF AVAIL Butiker THEN DO:
          BUFFER-COPY Butiker USING Butik ButNamn TO btt-Valgte.
          ASSIGN btt-Valgte.NyElValgt = "V".
          RELEASE btt-Valgte.
        END.
    END.
  END.
  FOR EACH ButikkTilgang WHERE ButikkTilgang.BrGrpNr = BrukerGrp.BrGrpNr AND
      NOT CAN-FIND(btt-Valgte WHERE btt-Valgte.Butik = ButikkTilgang.Butik) NO-LOCK:
      FIND Butiker WHERE Butiker.Butik = ButikkTilgang.Butik NO-LOCK NO-ERROR.
      IF AVAIL Butiker THEN DO:
        BUFFER-COPY Butiker USING Butik ButNamn TO btt-Butiker.
        ASSIGN btt-Butiker.NyElValgt = "N".
        RELEASE btt-Butiker.
      END.
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
    {&EkstraSjekk}
    find {&br-tabell} Exclusive-lock where
      recid({&br-tabell}) = wRecid no-error.
  end.
  assign
    {&AssignFelter}.
  {&TillegsAssign}
end. /* TRANSACTION */    
FOR EACH tt-Butiker WHERE tt-Butiker.NyElValgt = "V":
  FIND ButikkKobling WHERE ButikkKobling.BrGrpNr = wBrGrpNr AND
                           ButikkKobling.TeamTypeId = wTeamTypeId AND
                           ButikkKobling.TeamNr = ButikkTeam.TeamNr AND
                           ButikkKobling.Butik   = tt-Butiker.Butik.
  IF AVAIL ButikkKobling THEN
    DELETE ButikkKobling.
END.
FOR EACH tt-Valgte WHERE tt-Valgte.NyElValgt = "N":
  CREATE ButikkKobling.
  ASSIGN ButikkKobling.BrGrpNr = wBrGrpNr
         ButikkKobling.TeamTypeId = wTeamTypeId
         ButikkKobling.TeamNr = ButikkTeam.TeamNr
         ButikkKobling.Butik   = tt-Valgte.Butik.
  RELEASE ButikkKobling.
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

