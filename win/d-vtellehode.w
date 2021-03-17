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
  DEF VAR icButik   AS CHAR  NO-UNDO.
&ELSE
  DEFINE INPUT-output PARAMETER wRecid as recid NO-UNDO.
  define input        parameter wModus as char  no-undo.
  DEF INPUT           PARAM     icButik AS CHAR  NO-UNDO.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell TelleHode
&scoped-define KeyFelt TelleNr
&scoped-define OptKeyAssign

&scoped-define DataType INT /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster

/* Ekstra informasjon i find/where når det er flere ledd i indeks */
/* Brukes før KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster TelleHode.TelleNr when available TelleHode ~
                         TelleHode.Beskrivelse when available TelleHode ~
                         TelleHode.Notat when available TelleHode ~
                         TelleHode.ButikkListe when available TelleHode @ FI-Butiker ~
                         TelleHode.TilButikk WHEN AVAILABLE Tellehode @ FI-TilButikkNr ~
                         TelleHode.OrdreNr WHEN AVAILABLE TelleHode ~
                         TelleHode.PkSdlNr WHEN AVAILABLE TelleHode

/* Alternative poster som skal vises når VisPost kjøres */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData  


/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes if can-find({&br-tabell} where ~
         {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)) or ~
         int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
        do: ~
          if int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
            message "{&KeyFelt} må være større enn 0" ~
            view-as alert-box title "Lagringsfeil". ~
          else ~
            message "{&br-tabell} finnes allerede med TelleNr:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
          return "AVBRYT". ~
        end.
        
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter ~
  TelleHode.Beskrivelse ~
  TelleHode.TTId ~
  TelleHode.TBId = 1~
  TelleHode.StartDato  ~
  TelleHode.Notat ~
  TelleHode.ButikkListe = FI-Butiker ~
  TelleHode.TilButikk = INPUT FI-TilButikkNr ~
  TelleHode.OrdreNr ~
  TelleHode.PkSdlNr 

/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
/* Står etter forrige assign. Dvs egen linje.                   */
&scoped-define TillegsAssign

/* Local Variable Definitions ---                                       */
def var wRetur-Verdi as char initial "AVBRYT" no-undo.
def var wEDB-System as char no-undo.
def var wTabell     as char no-undo.  
def var wCl         as int  no-undo.
def var wNedskriv   as log  no-undo.
def var wTekst      as char no-undo.
def var wTransListe as char no-undo.
DEFINE VARIABLE iTelleNr      AS INTEGER    NO-UNDO.
DEFINE VARIABLE cButikkerBgrp AS CHARACTER  NO-UNDO. /* butikker for bruker */
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
&Scoped-define INTERNAL-TABLES TelleHode

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame TelleHode.Notat ~
TelleHode.TelleNr TelleHode.TTId TelleHode.StartDato TelleHode.Beskrivelse ~
TelleHode.OrdreNr TelleHode.PkSdlNr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame TelleHode.Notat ~
TelleHode.TTId TelleHode.StartDato TelleHode.Beskrivelse TelleHode.OrdreNr ~
TelleHode.PkSdlNr 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame TelleHode
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame TelleHode
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH TelleHode SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH TelleHode SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame TelleHode
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame TelleHode


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS TelleHode.Notat TelleHode.TTId ~
TelleHode.StartDato TelleHode.Beskrivelse TelleHode.OrdreNr ~
TelleHode.PkSdlNr 
&Scoped-define ENABLED-TABLES TelleHode
&Scoped-define FIRST-ENABLED-TABLE TelleHode
&Scoped-Define ENABLED-OBJECTS RECT-52 Btn_OK FI-Butiker FI-TilbutikkNr ~
Btn_Cancel Btn_Help BUTTON-SokFraBut B-SokDato 
&Scoped-Define DISPLAYED-FIELDS TelleHode.Notat TelleHode.TelleNr ~
TelleHode.TTId TelleHode.StartDato TelleHode.Beskrivelse TelleHode.OrdreNr ~
TelleHode.PkSdlNr 
&Scoped-define DISPLAYED-TABLES TelleHode
&Scoped-define FIRST-DISPLAYED-TABLE TelleHode
&Scoped-Define DISPLAYED-OBJECTS FI-Info FI-Butiker FI-TilbutikkNr ~
FI-EndretInfo FI-LblButikListe 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokButikker 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON B-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

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

DEFINE BUTTON BUTTON-SokFraBut 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE FI-Butiker AS CHARACTER FORMAT "X(200)":U 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 10  NO-UNDO.

DEFINE VARIABLE FI-EndretInfo AS CHARACTER FORMAT "X(256)":U 
      VIEW-AS TEXT 
     SIZE 98 BY .62 NO-UNDO.

DEFINE VARIABLE FI-Info AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 99 BY 1 NO-UNDO.

DEFINE VARIABLE FI-LblButikListe AS CHARACTER FORMAT "X(256)":U INITIAL "Butikker:" 
      VIEW-AS TEXT 
     SIZE 9 BY .62 NO-UNDO.

DEFINE VARIABLE FI-TilbutikkNr AS INTEGER FORMAT ">>>>>9":U INITIAL 0 
     LABEL "Til butikk" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1 TOOLTIP "Butikk det skal overføres varer til." NO-UNDO.

DEFINE RECTANGLE RECT-52
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 98.8 BY 12.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      TelleHode SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-SokButikker AT ROW 5.52 COL 34.6
     TelleHode.Notat AT ROW 7.91 COL 4 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 95 BY 5.71 NO-TAB-STOP 
     FI-Info AT ROW 14.33 COL 2 NO-LABEL
     Btn_OK AT ROW 16.67 COL 3
     TelleHode.TelleNr AT ROW 1.95 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     TelleHode.TTId AT ROW 3.14 COL 15 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "0",1
          DROP-DOWN-LIST
          SIZE 34 BY 1
     TelleHode.StartDato AT ROW 4.33 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     FI-Butiker AT ROW 5.52 COL 15 COLON-ALIGNED NO-LABEL
     FI-TilbutikkNr AT ROW 6.71 COL 15 COLON-ALIGNED
     Btn_Cancel AT ROW 16.67 COL 19
     TelleHode.Beskrivelse AT ROW 1.95 COL 32.2 COLON-ALIGNED NO-LABEL FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 65 BY 1
     TelleHode.OrdreNr AT ROW 4.33 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 13.2 BY 1
     TelleHode.PkSdlNr AT ROW 5.52 COL 68 COLON-ALIGNED
          VIEW-AS FILL-IN NATIVE 
          SIZE 13.2 BY 1
     Btn_Help AT ROW 16.67 COL 86
     BUTTON-SokFraBut AT ROW 6.71 COL 34.8
     B-SokDato AT ROW 4.33 COL 34.6
     FI-EndretInfo AT ROW 15.76 COL 3 NO-LABEL
     FI-LblButikListe AT ROW 5.76 COL 6 COLON-ALIGNED NO-LABEL
     RECT-52 AT ROW 1.24 COL 2
     SPACE(0.99) SKIP(4.23)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Detaljer telleliste"
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
   L-To-R,COLUMNS                                                       */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON B-SokButikker IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TelleHode.Beskrivelse IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
ASSIGN 
       BUTTON-SokFraBut:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       FI-Butiker:READ-ONLY IN FRAME Dialog-Frame        = TRUE.

/* SETTINGS FOR FILL-IN FI-EndretInfo IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Info IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-LblButikListe IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       FI-TilbutikkNr:HIDDEN IN FRAME Dialog-Frame           = TRUE.

ASSIGN 
       TelleHode.Notat:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE.

/* SETTINGS FOR FILL-IN TelleHode.TelleNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "skotex.TelleHode"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Detaljer telleliste */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokButikker
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokButikker Dialog-Frame
ON CHOOSE OF B-SokButikker IN FRAME Dialog-Frame /* ... */
DO:
    DEFINE VARIABLE cTekst AS CHARACTER  NO-UNDO.
  DO WITH FRAME FRAME-ArtInfo:
  /* Kaller søkerutine */
    RUN gbutikerBgrp.w (
      INPUT-OUTPUT cTekst, /* Returstreng - chr(1) separert */
      "", /* Feltliste avgrensningsfelt (kommaseparert) */
      "", /* Feltverdier (chr(1) sep) */ 
      FI-Butiker:SCREEN-VALUE /* Post markøren skal stå på */
      ).
    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    IF NUM-ENTRIES(cTekst,CHR(1)) >= 3 THEN
    DO:
        /* Legger opp verdier I de aktuelle feltene */
        ASSIGN FI-Butiker = ENTRY(2,cTekst,CHR(1))
          FI-Butiker:SCREEN-VALUE = FI-Butiker.
          .
    END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokDato Dialog-Frame
ON CHOOSE OF B-SokDato IN FRAME Dialog-Frame /* ... */
or F10 of TelleHode.StartDato
DO:

  def var wTittel as char no-undo.
  do with frame {&FRAME-NAME}:  

  wTittel = "Datosøk".

  /* Start søkeprogram */
  {soek.i
    &Felt        = TelleHode.StartDato
    &Program     = kalender.w
    &Frame       = {&FRAME-NAME}
    &ExtraParam  = "input wTittel"
  }   
  
  end. /* FRAME */
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
  RUN Dummy.

  run LagrePost.

  if return-value = "AVBRYT" then
  DO:
      MESSAGE "Feil ved lagring av post: " RETURN-VALUE "." SKIP
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      return no-apply.  
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokFraBut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFraBut Dialog-Frame
ON CHOOSE OF BUTTON-SokFraBut IN FRAME Dialog-Frame /* ... */
or F10 of FI-TilButikkNr
DO:
  
  /* Start søkeprogram */
  {soek.i
    &Felt        = FI-TilButikkNr
    &Program     = d-bbutiker.w
    &PostRun     = "find butiker no-lock where 
                      recid(Butiker) = int(return-value) no-error."
    &Frame       = Dialog-Frame
  }   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FI-TilbutikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FI-TilbutikkNr Dialog-Frame
ON LEAVE OF FI-TilbutikkNr IN FRAME Dialog-Frame /* Til butikk */
DO:
  IF NOT CAN-FIND(Butiker NO-LOCK WHERE
      Butiker.Butik = INPUT FI-TilButikkNr) THEN
  DO:
      MESSAGE 
      "Ukjent butikknummer." 
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TelleHode.TTId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TelleHode.TTId Dialog-Frame
ON VALUE-CHANGED OF TelleHode.TTId IN FRAME Dialog-Frame /* TransTypeId */
DO:
  IF INPUT TelleHode.TTId <> 6 THEN
      ASSIGN
      FI-TilButikkNr:HIDDEN = TRUE
      BUTTON-SokFraBut:HIDDEN = TRUE
      .
  ELSE
      ASSIGN
      FI-TilButikkNr:HIDDEN = FALSE
      BUTTON-SokFraBut:HIDDEN = FALSE
      .

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

/*{syspara.i 4 1 1 wTransListe}*/
ASSIGN
    wTransliste = "1,2,5,6,7,8,9,11"
    .
{syspara.i 5 1 1 wCl INT}
{syspara.i 1 2 4 wEDB-System}
assign wTabell = "ArtBas".

/* HotKeySøk - DYYYYRT */
/* on ALT-M of frame Dialog-Frame anywhere                   */
/*   do:                                                     */
/*     apply "CHOOSE":U to B-Butikker in frame Dialog-Frame. */
/*   end.                                                    */
      
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    IF wModus = "Ny" THEN DO:
        FIND LAST TelleHode NO-LOCK NO-ERROR.
        ASSIGN iTelleNr = IF AVAIL TelleHode THEN TelleHode.TelleNr + 1 ELSE 1.
        RELEASE TelleHode.
        RUN InitButikker.
    END.
    RUN InitCB.
  {lng.i} RUN enable_UI.

  run VisPost.

  IF INPUT TelleHode.TTId <> 6 THEN
      ASSIGN
      FI-TilButikkNr:HIDDEN = TRUE
      BUTTON-SokFraBut:HIDDEN = TRUE.
  ELSE
      ASSIGN
      FI-TilButikkNr:HIDDEN = FALSE
      BUTTON-SokFraBut:HIDDEN = FALSE.
  IF CAN-FIND(FIRST TelleLinje NO-LOCK WHERE
              TelleLinje.TelleNr = INPUT TelleHode.TelleNr) THEN
      ASSIGN
      FI-TilButikkNr:SENSITIVE = FALSE
      BUTTON-SokFraBut:SENSITIVE = FALSE.
  ELSE
      ASSIGN
      FI-TilButikkNr:SENSITIVE = TRUE
      BUTTON-SokFraBut:SENSITIVE = TRUE.
  
  /* BHa 29.07.04: "Butik" kommer nå som input parameter */
  if wModus = "Ny" then
    do:
      assign
        FI-Butiker:SCREEN-VALUE = icButik
        FI-Butiker = icButik
        {&br-tabell}.{&KeyFelt}:sensitive = true
        {&br-tabell}.{&KeyFelt}:SCREEN-VALUE = STRING(iTelleNr)
        {&br-tabell}.TTId:SCREEN-VALUE = "9".
      APPLY "TAB" TO {&br-tabell}.TTId.
      display today @ TelleHode.StartDato with frame Dialog-Frame.
/*       IF NUM-ENTRIES(cButikkerBgrp) = 1 THEN              */
/*           ASSIGN FI-Butiker = cButikkerBgrp               */
/*                  FI-Butiker:SCREEN-VALUE = cButikkerBgrp. */
/*       ELSE                                                */
/*           APPLY "CHOOSE" TO B-SokButikker.                */
      APPLY "ENTRY" TO TelleHode.Beskrivelse.
    end.
  else do:
    assign
      {&br-tabell}.{&KeyFelt}:sensitive = false
      FI-Butiker = TelleHode.ButikkListe.
      
    if available TelleHode then
      do:
        if can-find(first TelleLinje of TelleHode) then
          assign
            TelleHode.TTId:sensitive = false
            TelleHode.StartDato:sensitive = false
            B-SokDato:sensitive      = false.            
      end.
  end.      
  
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButikkerEndret Dialog-Frame 
PROCEDURE ButikkerEndret :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter IO-Liste  as char no-undo.
  def input parameter wOldListe as char no-undo.

  def var wLoop as int no-undo.
  
  /* Sjekker om noen butikker er tatt bort fra listen. */
  do wLoop = 1 to num-entries(wOldListe):
    if not can-do(IO-Liste,entry(wLoop,wOldListe)) then
      run TattBortButikk (input entry(wLoop,wOldListe)).
  end.
  
  /* Sjekker om det er kommet til noen nye butikker. */
  do wLoop = 1 to num-entries(IO-Liste):
    if not can-do(wOldListe,entry(wLoop,IO-Liste)) then
      run NyButikk (input entry(wLoop,IO-Liste)).
  end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Dummy Dialog-Frame 
PROCEDURE Dummy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  RETURN "".
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
  DISPLAY FI-Info FI-Butiker FI-TilbutikkNr FI-EndretInfo FI-LblButikListe 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE TelleHode THEN 
    DISPLAY TelleHode.Notat TelleHode.TelleNr TelleHode.TTId TelleHode.StartDato 
          TelleHode.Beskrivelse TelleHode.OrdreNr TelleHode.PkSdlNr 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-52 TelleHode.Notat Btn_OK TelleHode.TTId TelleHode.StartDato 
         FI-Butiker FI-TilbutikkNr Btn_Cancel TelleHode.Beskrivelse 
         TelleHode.OrdreNr TelleHode.PkSdlNr Btn_Help BUTTON-SokFraBut 
         B-SokDato 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitButikker Dialog-Frame 
PROCEDURE InitButikker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FIND Bruker WHERE Bruker.BrukerId = USERID("skotex") NO-LOCK.
    FOR EACH Butikktilgang WHERE Butikktilgang.BrGrpNr = Bruker.BrGrpNr NO-LOCK:
        ASSIGN cButikkerBgrp = cButikkerBgrp + 
                               (IF cButikkerBgrp <> "" THEN "," ELSE "") + 
                               STRING(Butikktilgang.butik).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCB Dialog-Frame 
PROCEDURE InitCB :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cListItemPairs AS CHARACTER  NO-UNDO.
    FOR EACH TransType NO-LOCK WHERE CAN-DO(wTransliste,STRING(TransType.TTId)):
        ASSIGN cListItemPairs = cListItemPairs +
                                (IF cListItemPairs <> "" THEN "," ELSE "") +
                                SkoTex.TransType.Beskrivelse + "," + STRING(TransType.TTId).
    END.
    ASSIGN TelleHode.TTId:LIST-ITEM-PAIRS IN FRAME {&FRAME-NAME} = cListItemPairs.

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
  IF INPUT TelleHode.TTID = 6 THEN
  DO:
      IF NOT CAN-FIND(Butiker WHERE Butiker.Butik = INPUT FI-TilbutikkNr) THEN
      DO:
          MESSAGE 
          "Ikke angitt, eller ugyldig mottagende butikk."            
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
      END.
  END.
  if not can-do(wTransListe,string(input TelleHode.TTId)) then
    do:
      message "Ugyldig transaksjonstype." skip
              "Kun følgende transaksjonstyper kan benyttes " + wTransListe + "."
              view-as alert-box message title "Melding".
      return "AVBRYT".
    end.
  IF FI-Butiker:SCREEN-VALUE = "" THEN DO:
      MESSAGE "Velg butikk"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "CHOOSE" TO B-SokButikker.
      RETURN "AVBRYT".
  END.
  IF INPUT TelleHode.Beskrivelse = "" THEN DO:
      MESSAGE "Registrer beskrivelse"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO TelleHode.Beskrivelse.
      RETURN "AVBRYT".
  END.
  /* Sjekker nye poster. */
  if wModus = "Ny" then
    do:
      {&SjekkOmPostFinnes}
      {&OptFind}
      create {&br-tabell}.
      assign 
        {&OptKeyAssign}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE NyButikk Dialog-Frame 
PROCEDURE NyButikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  def input parameter wButik as int.
  
  def var wAntall    as int  no-undo.
  def var wStorl     as char no-undo.
  def var wVVareKost as dec  no-undo.
  
  def buffer bTelleLinje for TelleLinje.

  {syspara.i 4 1 2 wTekst}
  if {&br-tabell}.TTID = int(wTekst) 
    then wNedskriv = true.
  else wNedskriv = false.

  {sww.i}
  /* Legger opp linjer for ny butikk. */
  for each TelleLinje of TelleHode no-lock
    break by TelleLinje.TelleNr
          by TelleLinje.Vg
          by TelleLinje.LopNr
          by TelleLinje.Butik:
    if first-of(TelleLinje.Butik) then
      FIRST-OF-TELLELINJE:
      do:
        /* Sjekker om artikkelen finnes på telling for butikken fra før. */
        if can-find(first KonvReg where
           KonvReg.EDB-System = wEDB-System and
           KonvReg.Tabell     = wTabell     and
           KonvReg.EkstId     = string(TelleLinje.ArtikkelNr) + "," + 
                                string(wButik) and
           KonvReg.InterntId  = string(TelleLinje.ArtikkelNr) + "," + 
                                string(wButik)) then
          leave FIRST-OF-TELLELINJE.

        find Lager no-lock where
          Lager.ArtikkelNr = TelleLinje.ArtikkelNr and
          Lager.Butik      = TelleLinje.Butik no-error.
        if available Lager then
          wVVareKost = Lager.VVareKost.
        else
          wVVareKost = 0.
      
        /* Setter opp tellelås for butikken */
        if not can-find(first KonvReg where
           KonvReg.EDB-System = wEDB-System and
           KonvReg.Tabell     = wTabell     and
           KonvReg.EkstId     = string(TelleLinje.ArtikkelNr) + "," + 
                                string(wButik) and
           KonvReg.InterntId  = string(TelleLinje.ArtikkelNr) + "," + 
                                string(wButik)) then
          do TRANSACTION:
            create KonvReg.
            assign
              KonvReg.EDB-System = wEDB-System 
              KonvReg.Tabell     = wTabell     
              KonvReg.EkstId     = string(TelleLinje.ArtikkelNr) + "," + 
                                   string(wButik)              
              KonvReg.InterntId  = string(TelleLinje.ArtikkelNr) + "," + 
                                   string(wButik)
              KonvReg.DivTekst   = string(TelleHode.TelleNr).
                                   
          end. /* TRANSACTION */ 
         
        /* Oppstandelsen - Oppstandelse pr. størrelse. */      
        ARTLAG:
        for each ArtLag no-lock where
            Artlag.Artikkelnr = Tellelinje.artikkelnr AND
            ArtLag.Butik = wButik:        

          /* For nedskrivning, skal det kun legges opp en størrelse */
          if wNedskriv then
            wStorl = "".
          else
            wStorl = ArtLag.Storl.

          /* Linjen legges ikke opp hvis den finnes fra før. */
          if wNedskriv = false then
            do:
              if can-find(first bTelleLinje of TelleHode where        
                 bTelleLinje.ArtikkelNr = TelleLinje.ArtikkelNr and
                 bTelleLinje.Butik      = wButik and
                 bTelleLinje.Storl      = wStorl) then
                next ARTLAG.  
            end.
          else do:
            find first bTelleLinje of TelleHode exclusive-lock where        
               bTelleLinje.ArtikkelNr = TelleLinje.ArtikkelNr and
               bTelleLinje.Butik      = wButik and
               bTelleLinje.Storl      = wStorl no-error.
          end.

          /* Oppretter post. */
          if not available bTelleLinje then
            do:
              create bTelleLinje.          
        
              /* Setter index. */
              assign
                bTelleLinje.TelleNr    = TelleLinje.TelleNr 
                bTelleLinje.ArtikkelNr = TelleLinje.ArtikkelNr
                bTelleLinje.Butik      = wButik
                bTelleLinje.Storl      = wStorl.
            end.
          
          /* Øvrig informasjon. */
          assign
            wantall                = wAntall + 1
            bTelleLinje.Vg         = TelleLinje.Vg 
            bTelleLinje.LopNr      = TelleLinje.LopNr
            bTelleLinje.LevKod     = TelleLinje.LevKod
            bTelleLinje.AntallPar  = bTelleLinje.AntallPar + Artlag.Lagant
            bTelleLinje.VVareKost  = wVVAreKost
            bTelleLinje.Nedskrevet = wVVAreKost

            bTelleLinje.OpprVerdi  = bTelleLinje.AntallPar * wVVareKost

            bTelleLinje.AntallDiff = if wNedskriv 
                                       then bTelleLinje.AntallDiff + ArtLag.LagAnt
                                       else bTelleLinje.AntallDiff
            bTelleLinje.VerdiDiff  = if wNedskriv 
                                        then 0
                                        else (bTelleLinje.AntallDiff * wVVareKost)                                     
            bTelleLinje.LevNr      = TelleLinje.LevNr
            bTelleLinje.Sasong     = TelleLinje.SaSong
            bTelleLinje.Farg       = TelleLinje.Farg
            bTelleLinje.MatKod     = TelleLinje.MatKod          
            bTelleLinje.VgLopNr    = trim(string(TelleLinje.Vg,">>>9")) + "/" + trim(string(TelleLinje.LopNr,">>>9")).
        end. /* ARTLAG */

        FI-Info = "Legger til linjer for butikk " + string(wButik) +
                  " (" + string(wAntall) + " linjer).".
        pause 0.
        display FI-Info with frame Dialog-Frame.           
      end. /* FIRST-OF-TELLELINJE */         
  end.
  {swn.i}

  FI-Info = "".
  display FI-Info with frame Dialog-Frame.     

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TattBortButikk Dialog-Frame 
PROCEDURE TattBortButikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter wButik as int no-undo.
  
  def var wAntall as int no-undo.

  {sww.i}
  /* Tar bort linjene for butikken. */
  for each TelleLinje of TelleHode exclusive-lock where
    TelleLinje.Butik = wButik 
    break by TelleLinje.TelleNr
         by TelleLinje.Butik
         by TelleLinje.Vg
         by TelleLinje.LopNr:
         
    wAntall = wAntall + 1.
    FI-Info = "Tar bort linjer fra butikk " + string(wButik) +
              " (" + string(wAntall) + " linjer).".
    pause 0.
    display FI-Info with frame Dialog-Frame.     
    
    /* Opphever lås for butikken */
    if first-of(TelleLinje.LopNr) then
      do:
        find first KonvReg exclusive-lock where
          KonvReg.EDB-System = wEDB-System and
          KonvReg.Tabell     = wTabell     and
          KonvReg.EkstId     = string(TelleLinje.ArtikkelNr) + "," + 
                               string(wButik) and
          KonvReg.InterntId  = string(TelleLinje.ArtikkelNr) + "," + 
                                string(wButik) no-error.
        if available KonvReg then
          delete KonvReg.
    end.

    delete TelleLinje.    
    
  end.
  {swn.i}

  FI-Info = "".
  display FI-Info with frame Dialog-Frame.     

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
  if available TelleHode then
    do:
      FI-EndretInfo = "Opprettett: " + string(TelleHode.RegistrertDato) + " " + 
                      string(TelleHode.RegistrertTid,"HH:MM:SS") + " " + 
                      TelleHode.RegistrertAv + " Endret: " +
                      string(TelleHode.EDato) + " " + 
                      string(TelleHode.ETid,"HH:MM:SS") + " " + 
                      TelleHode.BrukerId.
      find TransType of TelleHode no-lock no-error.
    end.
  display 
    FI-EndretInfo
    {&VisPoster}
  with frame Dialog-Frame.
  {&VisAndreData}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

