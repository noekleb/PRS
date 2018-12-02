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
&scoped-define br-tabell StrType
&scoped-define KeyFelt StrTypeID
&scoped-define DataType INT /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster 
/* Ekstra informasjon i find/where når det er flere ledd i indeks */
&scoped-define OptFind 
/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster StrType.StrTypeID when available StrType ~
                         StrType.Beskrivelse when available StrType ~
                         StrType.KortNavn when available StrType ~
                         StrType.AvdelingNr WHEN AVAILABLE StrType
/* Alternative poster som skal vises når VisPost kjøres */
&scoped-define VisAndreData
/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes if can-find(StrType where ~
         {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)) or ~
         int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
        do: ~
          if int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
            message "StrTypeID må være større enn 0" ~
            view-as alert-box title "Lagringsfeil". ~
          else ~
            message "StrType finnes allerede med fylkesnr:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
          return "AVBRYT". ~
        end.
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter StrType.StrTypeID ~
                            StrType.Beskrivelse ~
                            StrType.KortNavn ~
                            StrType.AvdelingNr ~
                            StrType.Hg
/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
&scoped-define TillegsAssign

/* Local Variable Definitions ---                                       */
def var wRetur-Verdi as char initial "AVBRYT" no-undo.

DEF VAR cAlle AS CHAR NO-UNDO.

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
&Scoped-define INTERNAL-TABLES StrType Avdeling HuvGr

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame StrType.AvdelingNr StrType.Hg ~
StrType.StrTypeID StrType.Beskrivelse StrType.KortNavn 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame StrType.AvdelingNr ~
StrType.Hg StrType.Beskrivelse StrType.KortNavn 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame StrType
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame StrType
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH StrType SHARE-LOCK, ~
      EACH Avdeling OF StrType SHARE-LOCK, ~
      EACH HuvGr OF StrType SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH StrType SHARE-LOCK, ~
      EACH Avdeling OF StrType SHARE-LOCK, ~
      EACH HuvGr OF StrType SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame StrType Avdeling HuvGr
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame StrType
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame Avdeling
&Scoped-define THIRD-TABLE-IN-QUERY-Dialog-Frame HuvGr


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS StrType.AvdelingNr StrType.Hg ~
StrType.Beskrivelse StrType.KortNavn 
&Scoped-define ENABLED-TABLES StrType
&Scoped-define FIRST-ENABLED-TABLE StrType
&Scoped-Define ENABLED-OBJECTS Btn_Help Btn_OK Btn_Cancel RECT-1 
&Scoped-Define DISPLAYED-FIELDS StrType.AvdelingNr StrType.Hg ~
StrType.StrTypeID StrType.Beskrivelse StrType.KortNavn 
&Scoped-define DISPLAYED-TABLES StrType
&Scoped-define FIRST-DISPLAYED-TABLE StrType


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

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL 
     SIZE 70 BY 5.81.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      StrType, 
      Avdeling, 
      HuvGr SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     StrType.AvdelingNr AT ROW 4.1 COL 16 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "0",1
          DROP-DOWN-LIST
          SIZE 52 BY 1
     StrType.Hg AT ROW 5.19 COL 16 COLON-ALIGNED
          LABEL "Hovedgr"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "0",1
          DROP-DOWN-LIST
          SIZE 52 BY 1
     StrType.StrTypeID AT ROW 1.95 COL 16 COLON-ALIGNED FORMAT ">>>>>9"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     StrType.Beskrivelse AT ROW 1.95 COL 30 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 37.4 BY 1
     StrType.KortNavn AT ROW 3 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     Btn_Help AT ROW 7.43 COL 57
     Btn_OK AT ROW 7.43 COL 2
     Btn_Cancel AT ROW 7.43 COL 17.2
     RECT-1 AT ROW 1.38 COL 2
     SPACE(0.99) SKIP(1.56)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedlikehold størrelsestyper"
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

/* SETTINGS FOR COMBO-BOX StrType.Hg IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN StrType.StrTypeID IN FRAME Dialog-Frame
   NO-ENABLE EXP-FORMAT                                                 */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.StrType,skotex.Avdeling OF SkoTex.StrType,skotex.HuvGr OF SkoTex.StrType"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedlikehold størrelsestyper */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME StrType.AvdelingNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL StrType.AvdelingNr Dialog-Frame
ON VALUE-CHANGED OF StrType.AvdelingNr IN FRAME Dialog-Frame /* Avdelingsnr */
DO:
  RUN initDBHovedGr.
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

{syspara.i 1 100 1 cAlle}
      
/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  {lng.i} RUN enable_UI.

  RUN InitCbAvdeling.
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
  IF AVAILABLE StrType THEN 
    DISPLAY StrType.AvdelingNr StrType.Hg StrType.StrTypeID StrType.Beskrivelse 
          StrType.KortNavn 
      WITH FRAME Dialog-Frame.
  ENABLE StrType.AvdelingNr StrType.Hg StrType.Beskrivelse StrType.KortNavn 
         Btn_Help Btn_OK Btn_Cancel RECT-1 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCbAvdeling Dialog-Frame 
PROCEDURE InitCbAvdeling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      pcTekst = cAlle + ",0".
      FOR EACH Avdeling NO-LOCK:

          ASSIGN
              pcTekst = pcTekst + 
                        (IF pcTekst = "" 
                            THEN ""
                            ELSE ",") + 
                          STRING(Avdeling.AvdelingNr) + "/" + Avdeling.AvdelingNavn + "," +
                          STRING(Avdeling.AvdelingNr)
              .
      END.
  END.
  ASSIGN
      StrType.AvdelingNr:LIST-ITEM-PAIRS = pcTekst
      StrType.AvdelingNr:SCREEN-VALUE    = entry(2,pcTekst)
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initDBHovedGr Dialog-Frame 
PROCEDURE initDBHovedGr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
          pcTekst = cAlle + ",0"
          .
      IF StrType.AvdelingNr:SCREEN-VALUE = '0' THEN
      DO:
          ASSIGN
              StrType.Hg:LIST-ITEM-PAIRS = pcTekst
              StrType.Hg:SCREEN-VALUE    = entry(2,pcTekst)
              StrType.Hg:SENSITIVE = FALSE
              .
      END.
      ELSE DO:
          FOR EACH HuvGr NO-LOCK WHERE
              HuvGr.AvdelingNr = int(StrType.AvdelingNr:SCREEN-VALUE):

              ASSIGN
                  pcTekst = pcTekst + 
                            (IF pcTekst = "" 
                                THEN ""
                                ELSE ",") + 
                              STRING(HuvGr.Hg) + "/" + HuvGr.HgBeskr + "," +
                              STRING(HuvGr.Hg)
                  .
          END.
          ASSIGN
              StrType.Hg:LIST-ITEM-PAIRS = pcTekst
              StrType.Hg:SCREEN-VALUE    = entry(2,pcTekst)
              StrType.Hg:SENSITIVE = TRUE
              .
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
  IF AVAILABLE StrType THEN
  DO:
      RUN initDBHovedGr.
      ASSIGN 
      StrType.Hg:SCREEN-VALUE = string(StrType.Hg)
      .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

