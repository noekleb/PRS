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
&scoped-define br-tabell Produsent
&scoped-define KeyFelt ProdNr
&scoped-define DataType INT /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster ~
  find Post of Produsent no-lock no-error.
  
/* Ekstra informasjon i find/where når det er flere ledd i indeks */
&scoped-define OptFind 
/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster ~
  Post.Beskrivelse when available Post ~
  Produsent.Adresse1 when available Produsent ~
  Produsent.Adresse2 when available Produsent ~
  Produsent.Beskrivelse when available Produsent ~
  Produsent.Kontakt when available Produsent ~
  Produsent.Land when available Produsent ~
  Produsent.Merknad when available Produsent ~
  Produsent.Notat when available Produsent ~
  Produsent.PostBoks when available Produsent ~
  Produsent.PostNr when available Produsent ~
  Produsent.ProdNr when available Produsent ~
  Produsent.Telefon when available Produsent
  
/* Alternative poster som skal vises når VisPost kjøres */
&scoped-define VisAndreData

/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes if can-find(first Produsent where ~
         Produsent.ProdNr = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)) then ~
        do: ~
            message "Produsent finnes allerede med nr:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
          return "AVBRYT". ~
        end.
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter ~
  Produsent.Adresse1 ~
  Produsent.Adresse2 ~
  Produsent.Beskrivelse ~
  Produsent.Kontakt ~
  Produsent.Land ~
  Produsent.Merknad ~
  Produsent.Notat ~
  Produsent.PostBoks ~
  Produsent.PostNr ~
  Produsent.ProdNr ~
  Produsent.Telefon

/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
&scoped-define TillegsAssign

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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Produsent Post

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame Produsent.ProdNr ~
Produsent.Beskrivelse Produsent.Adresse1 Produsent.Adresse2 ~
Produsent.PostBoks Produsent.PostNr Produsent.Land Produsent.Kontakt ~
Produsent.Telefon Produsent.Merknad Post.Beskrivelse Produsent.Notat 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame Produsent.Beskrivelse ~
Produsent.Adresse1 Produsent.Adresse2 Produsent.PostBoks Produsent.PostNr ~
Produsent.Land Produsent.Kontakt Produsent.Telefon Produsent.Merknad ~
Produsent.Notat 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame Produsent
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame Produsent
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH Produsent SHARE-LOCK, ~
      EACH Post OF Produsent SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame Produsent Post
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame Produsent
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame Post


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Produsent.Beskrivelse Produsent.Adresse1 ~
Produsent.Adresse2 Produsent.PostBoks Produsent.PostNr Produsent.Land ~
Produsent.Kontakt Produsent.Telefon Produsent.Merknad Produsent.Notat 
&Scoped-define ENABLED-TABLES Produsent
&Scoped-define FIRST-ENABLED-TABLE Produsent
&Scoped-define DISPLAYED-TABLES Produsent Post
&Scoped-define FIRST-DISPLAYED-TABLE Produsent
&Scoped-define SECOND-DISPLAYED-TABLE Post
&Scoped-Define ENABLED-OBJECTS RECT-3 Btn_OK Btn_Cancel BUTTON-Sokeknapp ~
Btn_Help 
&Scoped-Define DISPLAYED-FIELDS Produsent.ProdNr Produsent.Beskrivelse ~
Produsent.Adresse1 Produsent.Adresse2 Produsent.PostBoks Produsent.PostNr ~
Produsent.Land Produsent.Kontakt Produsent.Telefon Produsent.Merknad ~
Post.Beskrivelse Produsent.Notat 

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

DEFINE BUTTON BUTTON-Sokeknapp 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 104 BY 12.76.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      Produsent, 
      Post SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Produsent.ProdNr AT ROW 1.57 COL 12.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     Produsent.Beskrivelse AT ROW 1.57 COL 22.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 42.2 BY 1
     Produsent.Adresse1 AT ROW 3.57 COL 12.4 COLON-ALIGNED FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 52.6 BY 1
     Produsent.Adresse2 AT ROW 4.57 COL 12.4 COLON-ALIGNED FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 52.6 BY 1
     Produsent.PostBoks AT ROW 5.57 COL 12.4 COLON-ALIGNED FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 52.6 BY 1
     Produsent.PostNr AT ROW 6.57 COL 12.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     Produsent.Land AT ROW 7.52 COL 12.4 COLON-ALIGNED FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 52.6 BY 1
     Produsent.Kontakt AT ROW 9.62 COL 12.4 COLON-ALIGNED FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 52.6 BY 1
     Produsent.Telefon AT ROW 10.57 COL 12.4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Produsent.Merknad AT ROW 12.38 COL 12.4 COLON-ALIGNED FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 52.6 BY 1
     Post.Beskrivelse AT ROW 6.57 COL 33 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Produsent.Notat AT ROW 3.57 COL 67.8 NO-LABEL
          VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
          SIZE 37.2 BY 9.81
     Btn_OK AT ROW 14.14 COL 2.4
     Btn_Cancel AT ROW 14.14 COL 18.2
     BUTTON-Sokeknapp AT ROW 6.57 COL 30.4
     Btn_Help AT ROW 14.14 COL 91.2
     RECT-3 AT ROW 1.1 COL 2.4
     SPACE(0.19) SKIP(1.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedlikehold produsent"
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
   Custom                                                               */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Produsent.Adresse1 IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Produsent.Adresse2 IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Post.Beskrivelse IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Produsent.Kontakt IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Produsent.Land IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Produsent.Merknad IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
ASSIGN 
       Produsent.Notat:RETURN-INSERTED IN FRAME Dialog-Frame  = TRUE.

/* SETTINGS FOR FILL-IN Produsent.PostBoks IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Produsent.ProdNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.Produsent,SkoTex.Post OF SkoTex.Produsent"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedlikehold produsent */
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


&Scoped-define SELF-NAME BUTTON-Sokeknapp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Sokeknapp Dialog-Frame
ON CHOOSE OF BUTTON-Sokeknapp IN FRAME Dialog-Frame /* ... */
DO:
  /* Start søkeprogram */
  {soek.i
    &Felt        = Produsent.PostNr
    &Program     = d-bpost.w
    &Frame       = Dialog-Frame
    &PostRun     = "find Post no-lock where
                    recid(Post) = int(return-value) no-error."
    &OptDisp     = "Post.Beskrivelse when available Post"
  } 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Produsent.PostNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Produsent.PostNr Dialog-Frame
ON TAB OF Produsent.PostNr IN FRAME Dialog-Frame /* PostNr */
or "RETURN":U of Produsent.PostNr
DO:
  find Post no-lock where
    Post.PostNr = input Produsent.PostNr no-error.
  if not available Post then
    do:
      message "Ugyldig postnummer!" view-as alert-box title "Melding".
      return no-apply.
    end.
  display 
    Post.Beskrivelse
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Dialog-Frame _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Dialog-Frame _DEFAULT-ENABLE
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
  IF AVAILABLE Post THEN 
    DISPLAY Post.Beskrivelse 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE Produsent THEN 
    DISPLAY Produsent.ProdNr Produsent.Beskrivelse Produsent.Adresse1 
          Produsent.Adresse2 Produsent.PostBoks Produsent.PostNr Produsent.Land 
          Produsent.Kontakt Produsent.Telefon Produsent.Merknad Produsent.Notat 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-3 Produsent.Beskrivelse Produsent.Adresse1 Produsent.Adresse2 
         Produsent.PostBoks Produsent.PostNr Produsent.Land Produsent.Kontakt 
         Produsent.Telefon Produsent.Merknad Produsent.Notat Btn_OK Btn_Cancel 
         BUTTON-Sokeknapp Btn_Help 
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
do with frame Dialog-Frame:
find Post no-lock where
  Post.PostNr = Produsent.PostNr:screen-value in frame Dialog-Frame no-error.
if not available Post then
  do:
    message "Ukjent postnummer!" view-as alert-box  title "Melding".
    return "AVBRYT".
  end.

do with frame Dialog-Frame transaction:
  /* Sjekker nye poster. */
  if wModus = "Ny" then
    do:
      {&SjekkOmPostFinnes}
      {&OptFind}
      create {&br-tabell}.
      assign 
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
end.
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

