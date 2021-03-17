&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
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
  define var wModus as char init "Ny" no-undo.
  DEFINE VAR wMedlemsNr AS DEC NO-UNDO.
  
&ELSE
  DEFINE INPUT-output PARAMETER wRecid   as recid NO-UNDO.
  define input        parameter wModus   as char  no-undo.
  DEFINE INPUT        PARAMETER wMedlemsNr AS DEC NO-UNDO.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell MedlemSaldo
&scoped-define KeyFelt ButikkNr
&scoped-define OptKeyAssign

&scoped-define DataType INT /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster ~
  FIND Butiker NO-LOCK WHERE Butiker.Butik = MedlemSaldo.Butik NO-ERROR.

/* Ekstra informasjon i find/where når det er flere ledd i indeks */
/* Brukes før KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster {&br-tabell}.ButikkNr when available {&br-tabell} ~
                         {&br-tabell}.ForsteDato when available {&br-tabell} ~
                         {&br-tabell}.DatoSiste when available {&br-tabell} ~
                         {&br-tabell}.TotaltKjop when available {&br-tabell} ~
                         Butiker.butNamn WHEN AVAILABLE Butiker
                         
/* Alternative poster som skal vises når VisPost kjøres */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData 

/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes if can-find({&br-tabell} where ~
         {&br-tabell}.MedlemsNr    = wMedlemsNr AND ~
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
&scoped-define AssignFelter {&br-tabell}.MedlemsNr = wMedlemsNr ~
                            {&br-tabell}.ButikkNr ~
                            {&br-tabell}.ForsteDato ~
                            {&br-tabell}.DatoSiste ~
                            {&br-tabell}.TotaltKjop

/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
/* Står etter forrige assign. Dvs egen linje.                   */
&scoped-define TillegsAssign 

&scoped-define EkstraSjekk 
             /*
             IF {&br-tabell}.Beskrivelse:SCREEN-VALUE = "" THEN DO: ~
                 MESSAGE "Feltet må registreres." VIEW-AS ALERT-BOX TITLE "Lagringsfeil". ~
                 APPLY "ENTRY" TO {&br-tabell}.Beskrivelse. ~
                 RETURN "AVBRYT". ~
             END.
             */

/* Local Variable Definitions ---                                       */
def var wRetur-Verdi as char initial "AVBRYT" no-undo.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS MedlemSaldo.ButikkNr MedlemSaldo.ForsteDato ~
MedlemSaldo.DatoSiste MedlemSaldo.TotaltKjop 
&Scoped-define ENABLED-TABLES MedlemSaldo
&Scoped-define FIRST-ENABLED-TABLE MedlemSaldo
&Scoped-define DISPLAYED-TABLES MedlemSaldo Butiker
&Scoped-define FIRST-DISPLAYED-TABLE MedlemSaldo
&Scoped-define SECOND-DISPLAYED-TABLE Butiker
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel Btn_Help B-SokButikk ~
RECT-1 
&Scoped-Define DISPLAYED-FIELDS MedlemSaldo.ButikkNr Butiker.ButNamn ~
MedlemSaldo.ForsteDato MedlemSaldo.DatoSiste MedlemSaldo.TotaltKjop 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokButikk 
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

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 79.8 BY 8.81.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     MedlemSaldo.ButikkNr AT ROW 3 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     Butiker.ButNamn AT ROW 3 COL 40.8 NO-LABEL FORMAT "x(40)"
          VIEW-AS FILL-IN 
          SIZE 34 BY 1
     MedlemSaldo.ForsteDato AT ROW 4 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     MedlemSaldo.DatoSiste AT ROW 5 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     MedlemSaldo.TotaltKjop AT ROW 6 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 23 BY 1
     Btn_OK AT ROW 10.52 COL 2
     Btn_Cancel AT ROW 10.52 COL 17
     Btn_Help AT ROW 10.52 COL 67
     B-SokButikk AT ROW 2.95 COL 36.4
     RECT-1 AT ROW 1.48 COL 2.2
     SPACE(0.59) SKIP(1.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kundesaldo"
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
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN Butiker.ButNamn IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L EXP-FORMAT                                         */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Kundesaldo */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokButikk Dialog-Frame
ON CHOOSE OF B-SokButikk IN FRAME Dialog-Frame /* ... */
or "F10" of MedlemSaldo.ButikkNr
DO:
  {soek.i
    &Felt        = MedlemSaldo.ButikkNr
    &Program     = d-bbutiker.w
    &Frame       = Dialog-Frame
    &PostRun     = "find Butiker no-lock where
                    recid(Butiker) = int(return-value) no-error."
    &OptDisp     = "Butiker.ButNamn when available Butiker
                    ' ' when not available Butiker @ Butiker.butNamn"
  } 
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


&Scoped-define SELF-NAME MedlemSaldo.ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MedlemSaldo.ButikkNr Dialog-Frame
ON TAB OF MedlemSaldo.ButikkNr IN FRAME Dialog-Frame /* Butikk */
OR "RETURN" OF MedlemSaldo.ButikkNr
DO:
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = INPUT MedlemSaldo.butikkNr NO-ERROR.
    IF AVAILABLE Butiker THEN
        ASSIGN
          MedlemSaldo.ButikkNr:SCREEN-VALUE = STRING(Butiker.Butik)
          Butiker.ButNamn:SCREEN-VALUE = Butiker.ButNamn
        .
    ELSE 
    ASSIGN
      MedlemSaldo.ButikkNr:SCREEN-VALUE  = ""
      Butiker.ButNamn:SCREEN-VALUE = ""
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
  RUN enable_UI.
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
  IF AVAILABLE Butiker THEN 
    DISPLAY Butiker.ButNamn 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE MedlemSaldo THEN 
    DISPLAY MedlemSaldo.ButikkNr MedlemSaldo.ForsteDato MedlemSaldo.DatoSiste 
          MedlemSaldo.TotaltKjop 
      WITH FRAME Dialog-Frame.
  ENABLE MedlemSaldo.ButikkNr MedlemSaldo.ForsteDato MedlemSaldo.DatoSiste 
         MedlemSaldo.TotaltKjop Btn_OK Btn_Cancel Btn_Help B-SokButikk RECT-1 
      WITH FRAME Dialog-Frame.
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

