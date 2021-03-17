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
&scoped-define br-tabell Farg
&scoped-define KeyFelt Farg
&scoped-define KeyFelt2 KFarge
&scoped-define OptKeyAssign

&scoped-define DataType INT /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster 

/* Ekstra informasjon i find/where når det er flere ledd i indeks */
/* Brukes før KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster ~
  Farg.Farg     when available Farg ~
  Farg.KFarge   when available Farg ~
  Farg.FarBeskr when available Farg
                         
/* Alternative poster som skal vises når VisPost kjøres */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData

/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes if can-find({&br-tabell} where ~
         {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)) THEN ~
        do: ~
          if int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
            message "{&KeyFelt} må være større enn 0" ~
            view-as alert-box title "Lagringsfeil". ~
          else ~
            message "{&br-tabell} finnes allerede med fargnr:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
           APPLY "ENTRY" TO {&br-tabell}.{&KeyFelt}. ~
          return "AVBRYT". ~
        end.
&scoped-define SjekkOmPostFinnes2 IF ~
            CAN-FIND({&br-tabell} WHERE {&br-tabell}.{&KeyFelt2} = {&br-tabell}.{&KeyFelt2}:SCREEN-VALUE) AND~
                {&br-tabell}.{&KeyFelt} <> INPUT {&br-tabell}.{&KeyFelt} THEN ~
        DO: ~
            message "{&br-tabell} finnes allerede med kort fargenavn:" ~
            {&br-tabell}.{&KeyFelt2}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
            APPLY "ENTRY" TO {&br-tabell}.{&KeyFelt2}. ~
          return "AVBRYT". ~
        END.
        
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter ~
  Farg.Farg ~
  Farg.KFarge ~
  Farg.FarBeskr

/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
/* Står etter forrige assign. Dvs egen linje.                   */
&scoped-define TillegsAssign

/* Local Variable Definitions ---                                       */
def var wRetur-Verdi as char initial "AVBRYT" no-undo.
DEF BUFFER b{&br-tabell} FOR {&br-tabell}.
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
&Scoped-define INTERNAL-TABLES Farg ArtBas

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame Farg.Farg Farg.KFarge ~
Farg.FarBeskr 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame Farg.Farg Farg.KFarge ~
Farg.FarBeskr 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame Farg
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame Farg
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH Farg SHARE-LOCK, ~
      EACH ArtBas OF Farg SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH Farg SHARE-LOCK, ~
      EACH ArtBas OF Farg SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame Farg ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame Farg
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame ArtBas


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Farg.Farg Farg.KFarge Farg.FarBeskr 
&Scoped-define ENABLED-TABLES Farg
&Scoped-define FIRST-ENABLED-TABLE Farg
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-FIELDS Farg.Farg Farg.KFarge Farg.FarBeskr 
&Scoped-define DISPLAYED-TABLES Farg
&Scoped-define FIRST-DISPLAYED-TABLE Farg


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

DEFINE BUTTON BUTTON-SokFarge 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE VARIABLE TilFarg AS INTEGER FORMAT ">>>>9" INITIAL 0 
     LABEL "Til farge" 
     VIEW-AS FILL-IN 
     SIZE 10.6 BY 1 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 52 BY 4.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      Farg, 
      ArtBas SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BUTTON-SokFarge AT ROW 1.71 COL 47.6
     TilFarg AT ROW 1.71 COL 34.4 COLON-ALIGNED HELP
          "Fargekode"
     Farg.Farg AT ROW 1.76 COL 13.4 COLON-ALIGNED FORMAT "zzzz9"
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     Farg.KFarge AT ROW 2.91 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.6 BY 1
     Farg.FarBeskr AT ROW 4.05 COL 13.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 37 BY 1
     Btn_OK AT ROW 5.76 COL 2.2
     Btn_Cancel AT ROW 5.76 COL 17.8
     Btn_Help AT ROW 5.76 COL 38
     RECT-1 AT ROW 1.38 COL 1.8
     SPACE(0.19) SKIP(1.52)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedlikehold fargekoder"
         DEFAULT-BUTTON Btn_OK CANCEL-BUTTON Btn_Cancel.


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
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON BUTTON-SokFarge IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
ASSIGN 
       BUTTON-SokFarge:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* SETTINGS FOR FILL-IN Farg.Farg IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN TilFarg IN FRAME Dialog-Frame
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       TilFarg:HIDDEN IN FRAME Dialog-Frame           = TRUE.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.Farg,skotex.ArtBas OF SkoTex.Farg"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedlikehold fargekoder */
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
  IF wModus = "Flyttkode" THEN DO:
      ASSIGN TilFarg.
      IF Farg.Farg = TilFarg THEN DO:
          MESSAGE Farg.Farg:LABEL " = " TilFarg:LABEL
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          APPLY "ENTRY" TO TilFarg.
          RETURN NO-APPLY.
      END.
      ELSE IF NOT CAN-FIND(bFarg WHERE bFarg.Farg = TilFarg) THEN DO:
          MESSAGE "Farvekode finnes ikke."
              VIEW-AS ALERT-BOX ERROR BUTTONS OK.
          RETURN NO-APPLY.
      END.
      MESSAGE "Önsker du å flytte alle artikkler fra kode " STRING(Farg.Farg) SKIP
              "til kode " STRING(TilFarg) " ?"
          VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL UPDATE choice AS LOGICAL.
      IF choice THEN
          RUN FlyttKode.
      ELSE
          RETURN NO-APPLY.
  END.
  ELSE
      run LagrePost.
  if return-value = "AVBRYT" then
    return no-apply.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokFarge
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokFarge Dialog-Frame
ON CHOOSE OF BUTTON-SokFarge IN FRAME Dialog-Frame /* ... */
or F10 of TilFarg IN FRAME {&FRAME-NAME}
DO:
do with frame {&FRAME-NAME}:
  ASSIGN TilFarg.
  /* Start søkeprogram */
  {soek.i
    &Felt        = TilFarg
    &Program     = d-bfarg.w
    &Frame       = {&FRAME-NAME}
    &PostRun     = "FIND bFarg no-lock where
                    RECID(bFarg) = INT(RETURN-VALUE) NO-ERROR."
    &OptDisp     = "bFarg.Farg when available bFarg @ TilFarg"
  }   
  
end. 
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
      {&br-tabell}.{&KeyFelt}:SENSITIVE = true.
  ELSE if wModus = "Bytkode" THEN
     assign
      {&br-tabell}.{&KeyFelt}:SENSITIVE = true
      {&br-tabell}.KFarge:SENSITIVE     = FALSE
      {&br-tabell}.FarBeskr:SENSITIVE   = FALSE.
  ELSE if wModus = "Flyttkode" THEN DO:
     assign
      {&br-tabell}.{&KeyFelt}:SENSITIVE = FALSE
      {&br-tabell}.KFarge:SENSITIVE     = FALSE
      {&br-tabell}.FarBeskr:SENSITIVE   = FALSE
/*       TilFarg:HIDDEN                    = FALSE */
      TilFarg:SENSITIVE                 = TRUE
      BUTTON-SokFarge:HIDDEN            = FALSE
      BUTTON-SokFarge:SENSITIVE         = TRUE.
     DISP TilFarg WITH FRAME {&FRAME-NAME}.
  END.
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
  IF AVAILABLE Farg THEN 
    DISPLAY Farg.Farg Farg.KFarge Farg.FarBeskr 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 Farg.Farg Farg.KFarge Farg.FarBeskr Btn_OK Btn_Cancel Btn_Help 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FlyttKode Dialog-Frame 
PROCEDURE FlyttKode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    FOR EACH ArtBas WHERE ArtBas.Farg = Farg.Farg EXCLUSIVE:
        ASSIGN ArtBas.Farg = TilFarg.
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
DEF VAR iOldKode LIKE {&br-tabell}.{&KeyFelt} NO-UNDO.
do with frame Dialog-Frame transaction:
  /* Sjekker nye poster. */
  if wModus = "Ny" then
    do:
      {&SjekkOmPostFinnes}
      {&SjekkOmPostFinnes2}
      {&OptFind}
      create {&br-tabell}.
      ASSIGN
        {&OptKeyAssign}
        {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)
        wRecid   = recid({&br-tabell}).
    end.
  ELSE IF wModus = "Bytkode" THEN DO:
      IF INPUT {&br-tabell}.{&KeyFelt} <> {&br-tabell}.{&KeyFelt} THEN DO:
          {&SjekkOmPostFinnes}
          ASSIGN iOldKode = {&br-tabell}.{&KeyFelt}.
          IF CAN-FIND(FIRST ArtBas WHERE ArtBas.Farg = iOldKode) THEN DO:
              MESSAGE "Artikkler med gammel kode: " iOldKode SKIP
                  " byttes til: " INPUT {&br-tabell}.{&KeyFelt} ". Fortsette?"
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice AS LOGICAL.
              IF NOT choice THEN
                  RETURN "AVBRYT".
          END.
          find {&br-tabell} Exclusive-lock where
            recid({&br-tabell}) = wRecid no-error.
      END.
      ELSE DO:
          MESSAGE "Ingen endring"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
      END.
  END.
  ELSE DO:
    {&SjekkOmPostFinnes2}
    find {&br-tabell} Exclusive-lock where
      recid({&br-tabell}) = wRecid no-error.
  END.
  assign
    {&AssignFelter}.
  {&TillegsAssign}
  IF wModus = "Bytkode" THEN DO:
      FOR EACH ArtBas WHERE ArtBas.Farg = iOldKode:
          ASSIGN ArtBas.Farg = {&br-tabell}.{&KeyFelt}.
      END.
  END.
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

