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
&scoped-define br-tabell StDef
&scoped-define KeyFelt StTypeId
&scoped-define OptKeyAssign

&scoped-define DataType STRING /* INT STRING DEC Datatype p� n�kkelfelt*/
/* Henter eventuelle relaterte poster - f�r mainblokk */
&scoped-define FinnRelatertePoster 

/* Ekstra informasjon i find/where n�r det er flere ledd i indeks */
/* Brukes f�r KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Felter som skal vises frem n�r rutinen VisPost kj�res */
&scoped-define VisPoster ~
  StDef.StTypeId when available StDef ~
  StDef.Beskrivelse when available StDef ~
  StDef.PerId when available StDef
                         
/* Alternative poster som skal vises n�r VisPost kj�res */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData

/* Sjekker om posten finnes fra f�r */
&scoped-define SjekkOmPostFinnes if can-find({&br-tabell} where ~
         {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value) and ~
          StDef.PerId = StDef.PerId:screen-value) then ~
        do: ~
          message "{&br-tabell} finnes allerede med fylkesnr:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
          return "AVBRYT". ~
        end.
        
/* Felter som assign'es n�r rutinen LagrePost kj�res */        
&scoped-define AssignFelter ~
  StDef.StTypeId = caps(input StDef.StTypeId)~
  StDef.Beskrivelse ~
  StDef.PerId = caps(input StDef.PerId)

/* Tilleggs felter som assign'es n�r rutinen Lagre post kj�res. */
/* St�r etter forrige assign. Dvs egen linje.                   */
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
&Scoped-define INTERNAL-TABLES StDef

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame StDef.StTypeId StDef.PerId ~
StDef.Beskrivelse 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame StDef.Beskrivelse 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame StDef
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame StDef
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH StDef SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame StDef
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame StDef


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS StDef.Beskrivelse 
&Scoped-define ENABLED-TABLES StDef
&Scoped-define FIRST-ENABLED-TABLE StDef
&Scoped-define DISPLAYED-TABLES StDef
&Scoped-define FIRST-DISPLAYED-TABLE StDef
&Scoped-Define ENABLED-OBJECTS RECT-1 BUTTON-Periode BUTTON-StType Btn_OK ~
Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-FIELDS StDef.StTypeId StDef.PerId ~
StDef.Beskrivelse 

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

DEFINE BUTTON BUTTON-Periode 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON BUTTON-StType 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 53 BY 4.52.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      StDef SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     BUTTON-Periode AT ROW 3.14 COL 37
     BUTTON-StType AT ROW 2.19 COL 37
     StDef.StTypeId AT ROW 2.19 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     StDef.PerId AT ROW 3.19 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15.6 BY 1
     StDef.Beskrivelse AT ROW 4.19 COL 19 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Btn_OK AT ROW 6.24 COL 2
     Btn_Cancel AT ROW 6.24 COL 17.6
     Btn_Help AT ROW 6.24 COL 40
     RECT-1 AT ROW 1.48 COL 2
     SPACE(0.99) SKIP(1.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedlikehold statistikkdefinisjoner"
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

/* SETTINGS FOR FILL-IN StDef.PerId IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN StDef.StTypeId IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "skotex.StDef"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Vedlikehold statistikkdefinisjoner */
DO:
  run LagrePost.
  if return-value = "AVBRYT" then
    return no-apply.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedlikehold statistikkdefinisjoner */
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
&Scoped-define SELF-NAME BUTTON-Periode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-Periode Dialog-Frame
ON CHOOSE OF BUTTON-Periode IN FRAME Dialog-Frame /* ... */
or "F10" of StDef.PerId
DO:
  /* Start s�keprogram */
  {soek.i
    &Felt        = StDef.PerId
    &Program     = d-bperiode.w
    &Frame       = Dialog-Frame
    &PostRun     = "find Periode no-lock where
                    recid(Periode) = int(return-value) no-error."
  } 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-StType
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-StType Dialog-Frame
ON CHOOSE OF BUTTON-StType IN FRAME Dialog-Frame /* ... */
or "F10" of StDef.StTypeId
DO:
  /* Start s�keprogram */
  {soek.i
    &Felt        = StDef.StTypeId
    &Program     = d-bsttype.w
    &Frame       = Dialog-Frame
    &PostRun     = "find StType no-lock where
                    recid(StType) = int(return-value) no-error."
  } 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME StDef.PerId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL StDef.PerId Dialog-Frame
ON LEAVE OF StDef.PerId IN FRAME Dialog-Frame /* PeriodeId */
DO:
  if not can-find(Periode where
                  Periode.PerId = input stDef.PerId) then
    do:
      message "Ukjent periodetype!"
        view-as alert-box title "Melding".
      return no-apply.
    end. 
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME StDef.StTypeId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL StDef.StTypeId Dialog-Frame
ON LEAVE OF StDef.StTypeId IN FRAME Dialog-Frame /* Statistikktype */
DO:
  if not can-find(StType where
                  StType.StTypeId = input stDef.StTypeId) then
    do:
      message "Ukjent statistikktype!"
        view-as alert-box title "Melding".
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

  {lng.i} RUN enable_UI.

  run VisPost.
  
  if wModus = "Ny" then
    assign
      {&br-tabell}.{&KeyFelt}:sensitive = true
      {&br-tabell}.PerId:sensitive      = true.
  else
    assign
      {&br-tabell}.{&KeyFelt}:sensitive = false
      {&br-tabell}.PerId:sensitive      = false.
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
  IF AVAILABLE StDef THEN 
    DISPLAY StDef.StTypeId StDef.PerId StDef.Beskrivelse 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 BUTTON-Periode BUTTON-StType StDef.Beskrivelse Btn_OK 
         Btn_Cancel Btn_Help 
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

