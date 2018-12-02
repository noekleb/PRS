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
&scoped-define br-tabell ForSalj
&scoped-define KeyFelt ForsNr
&scoped-define OptKeyAssign

&scoped-define DataType INT /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster ~
  Find Post no-lock where Post.PostNr = FoPoNr no-error.

/* Ekstra informasjon i find/where når det er flere ledd i indeks */
/* Brukes før KeyFelt assignes ved lagring.                       */
&scoped-define OptFind

/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster ~
  Forsalj.FoAdr when available Forsalj ~
  Forsalj.FoAnstNr when available Forsalj ~
  Forsalj.FoNamn when available Forsalj ~
  Forsalj.FoPadr when available Forsalj ~
  Forsalj.FoPersNr when available Forsalj ~
  Forsalj.FoPoNr when available Forsalj ~
  Forsalj.ForsNr when available Forsalj ~
  Forsalj.FoTel when available Forsalj ~
  Forsalj.navnikasse when available Forsalj ~
  Forsalj.FodtDato when available Forsalj ~
  Forsalj.Rabatt when available Forsalj ~
  Forsalj.Prisendring when available Forsalj ~
  Forsalj.Retur when available Forsalj ~
  Forsalj.slettTidligere when available Forsalj ~
  Forsalj.SlettBong when available Forsalj ~
  Forsalj.SletteForste when available Forsalj
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
            message "{&br-tabell} finnes allerede med selgernummer:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
          return "AVBRYT". ~
        end.
        
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter ~
  Forsalj.FoAdr ~
  Forsalj.FoAnstNr ~
  Forsalj.FoNamn ~
  Forsalj.FoPadr ~
  Forsalj.FoPersNr ~
  Forsalj.FoPoNr ~
  Forsalj.ForsNr ~
  Forsalj.FoTel ~
  Forsalj.navnikasse ~
  Forsalj.FodtDato ~
  Forsalj.Rabatt ~
  Forsalj.Prisendring ~
  Forsalj.Retur ~
  Forsalj.slettTidligere ~
  Forsalj.SlettBong ~
  Forsalj.SletteForste

/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
/* Står etter forrige assign. Dvs egen linje.                   */
&scoped-define TillegsAssign

/* Local Variable Definitions ---                                       */
def var wRetur-Verdi as char initial "AVBRYT" no-undo.
DEFINE VARIABLE hFillIn AS HANDLE     NO-UNDO.
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
&Scoped-define INTERNAL-TABLES Forsalj Post

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame Forsalj.ForsNr Forsalj.FoNamn ~
Forsalj.navnikasse Forsalj.FoAnstNr Forsalj.FoPersNr Forsalj.FodtDato ~
Forsalj.FoAdr Forsalj.FoPadr Forsalj.FoPoNr Post.Beskrivelse Forsalj.FoTel ~
Forsalj.Rabatt Forsalj.Prisendring Forsalj.Retur Forsalj.slettTidligere ~
Forsalj.SlettBong Forsalj.SletteForste 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame Forsalj.FoNamn ~
Forsalj.navnikasse Forsalj.FoAnstNr Forsalj.FoPersNr Forsalj.FodtDato ~
Forsalj.FoAdr Forsalj.FoPadr Forsalj.FoPoNr Forsalj.FoTel Forsalj.Rabatt ~
Forsalj.Prisendring Forsalj.Retur Forsalj.slettTidligere Forsalj.SlettBong ~
Forsalj.SletteForste 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame Forsalj
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame Forsalj
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH Forsalj SHARE-LOCK, ~
      EACH Post WHERE TRUE /* Join to Forsalj incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame Forsalj Post
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame Forsalj
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame Post


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Forsalj.FoNamn Forsalj.navnikasse ~
Forsalj.FoAnstNr Forsalj.FoPersNr Forsalj.FodtDato Forsalj.FoAdr ~
Forsalj.FoPadr Forsalj.FoPoNr Forsalj.FoTel Forsalj.Rabatt ~
Forsalj.Prisendring Forsalj.Retur Forsalj.slettTidligere Forsalj.SlettBong ~
Forsalj.SletteForste 
&Scoped-define ENABLED-TABLES Forsalj
&Scoped-define FIRST-ENABLED-TABLE Forsalj
&Scoped-define DISPLAYED-TABLES Forsalj Post
&Scoped-define FIRST-DISPLAYED-TABLE Forsalj
&Scoped-define SECOND-DISPLAYED-TABLE Post
&Scoped-Define ENABLED-OBJECTS BUTTON-Sokeknapp Btn_OK Btn_Cancel Btn_Help ~
RECT-1 RECT-2 
&Scoped-Define DISPLAYED-FIELDS Forsalj.ForsNr Forsalj.FoNamn ~
Forsalj.navnikasse Forsalj.FoAnstNr Forsalj.FoPersNr Forsalj.FodtDato ~
Forsalj.FoAdr Forsalj.FoPadr Forsalj.FoPoNr Post.Beskrivelse Forsalj.FoTel ~
Forsalj.Rabatt Forsalj.Prisendring Forsalj.Retur Forsalj.slettTidligere ~
Forsalj.SlettBong Forsalj.SletteForste 

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

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 10.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 79 BY 6.71.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      Forsalj, 
      Post SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BUTTON-Sokeknapp AT ROW 9 COL 37.6
     Forsalj.ForsNr AT ROW 2 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     Forsalj.FoNamn AT ROW 2 COL 32 COLON-ALIGNED NO-LABEL FORMAT "x(200)"
          VIEW-AS FILL-IN 
          SIZE 41 BY 1
     Forsalj.navnikasse AT ROW 3 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Forsalj.FoAnstNr AT ROW 4 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     Forsalj.FoPersNr AT ROW 5 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17.4 BY 1
     Forsalj.FodtDato AT ROW 6 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     Forsalj.FoAdr AT ROW 7 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Forsalj.FoPadr AT ROW 8 COL 24 COLON-ALIGNED
          LABEL "Postboks"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Forsalj.FoPoNr AT ROW 9 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.6 BY 1
     Post.Beskrivelse AT ROW 9 COL 40.2 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Forsalj.FoTel AT ROW 10 COL 24 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     Forsalj.Rabatt AT ROW 12.19 COL 24 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 21 BY 1
     Forsalj.Prisendring AT ROW 13.19 COL 24 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 21 BY 1
     Forsalj.Retur AT ROW 14.24 COL 24 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 21 BY 1
     Forsalj.slettTidligere AT ROW 15.29 COL 24 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 21 BY 1
     Forsalj.SlettBong AT ROW 16.24 COL 24 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 21 BY 1
     Forsalj.SletteForste AT ROW 17.43 COL 24 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 21 BY 1
     Btn_OK AT ROW 18.81 COL 2
     Btn_Cancel AT ROW 18.81 COL 17.8
     Btn_Help AT ROW 18.81 COL 65.2
     RECT-1 AT ROW 1.48 COL 2
     RECT-2 AT ROW 11.91 COL 2
     SPACE(0.00) SKIP(1.51)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedlikehold selgerregister"
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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Post.Beskrivelse IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Forsalj.FoNamn IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN Forsalj.FoPadr IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Forsalj.ForsNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "skotex.Forsalj,skotex.Post WHERE skotex.Forsalj ..."
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedlikehold selgerregister */
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
or "F10" of Forsalj.FoPoNr
DO:
  {soek.i
    &Felt        = Forsalj.FoPoNr
    &Program     = d-bpost.w
    &Frame       = Dialog-Frame
    &PostRun     = "find Post no-lock where
                    recid(Post) = int(return-value) no-error."
    &OptDisp     = "Post.Beskrivelse when available Post"
  } 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Forsalj.FoPoNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Forsalj.FoPoNr Dialog-Frame
ON TAB OF Forsalj.FoPoNr IN FRAME Dialog-Frame /* Postnummer */
or "RETURN":U of Forsalj.FoPoNr
DO:
  find Post no-lock where
    Post.PostNr = input Forsalj.FoPoNr no-error.
  if available Post then
    do:
      display
        Post.Beskrivelse
      with frame Dialog-Frame.
    end.  
  else do:
    message "Ukjent postnummer!!"
      view-as alert-box MESSAGE title "Melding".
    return no-apply.
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

  {lng.i} 
  RUN InitCombos.
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
  IF AVAILABLE Forsalj THEN 
    DISPLAY Forsalj.ForsNr Forsalj.FoNamn Forsalj.navnikasse Forsalj.FoAnstNr 
          Forsalj.FoPersNr Forsalj.FodtDato Forsalj.FoAdr Forsalj.FoPadr 
          Forsalj.FoPoNr Forsalj.FoTel Forsalj.Rabatt Forsalj.Prisendring 
          Forsalj.Retur Forsalj.slettTidligere Forsalj.SlettBong 
          Forsalj.SletteForste 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE Post THEN 
    DISPLAY Post.Beskrivelse 
      WITH FRAME Dialog-Frame.
  ENABLE BUTTON-Sokeknapp Forsalj.FoNamn Forsalj.navnikasse Forsalj.FoAnstNr 
         Forsalj.FoPersNr Forsalj.FodtDato Forsalj.FoAdr Forsalj.FoPadr 
         Forsalj.FoPoNr Forsalj.FoTel Forsalj.Rabatt Forsalj.Prisendring 
         Forsalj.Retur Forsalj.slettTidligere Forsalj.SlettBong 
         Forsalj.SletteForste Btn_OK Btn_Cancel Btn_Help RECT-1 RECT-2 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombos Dialog-Frame 
PROCEDURE InitCombos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO WITH FRAME {&FRAME-NAME}:
       ASSIGN Forsalj.Rabatt:LIST-ITEM-PAIRS = "Ja,3,Passord/kort,1,Nei,0"
              Forsalj.Prisendring:LIST-ITEM-PAIRS = Forsalj.Rabatt:LIST-ITEM-PAIRS
              Forsalj.Retur:LIST-ITEM-PAIRS = Forsalj.Rabatt:LIST-ITEM-PAIRS
              Forsalj.slettTidligere:LIST-ITEM-PAIRS = Forsalj.Rabatt:LIST-ITEM-PAIRS
              Forsalj.SlettBong:LIST-ITEM-PAIRS = Forsalj.Rabatt:LIST-ITEM-PAIRS
              Forsalj.SletteForste:LIST-ITEM-PAIRS = Forsalj.Rabatt:LIST-ITEM-PAIRS.
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

