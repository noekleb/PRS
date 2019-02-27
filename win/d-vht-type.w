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
&scoped-define br-tabell HT-Type
&scoped-define KeyFelt TypeId
&scoped-define OptKeyAssign

&scoped-define DataType INT /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster 

/* Ekstra informasjon i find/where når det er flere ledd i indeks */
/* Brukes før KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster HT-Type.TypeId when available HT-Type ~
                         HT-Type.Betegnelse when available HT-Type ~
                         HT-Type.ImportKatalog when available HT-Type ~
                         HT-Type.FilPrefix when available HT-Type ~
                         HT-Type.FilEkstent when available HT-Type ~
                         HT-Type.Notat when available HT-Type ~
                         HT-Type.EksportKatalog WHEN AVAILABLE HT-Type ~
                         HT-Type.EkspFilPrefix WHEN AVAILABLE HT-Type ~
                         HT-Type.EkspFilEkstent WHEN AVAILABLE HT-Type ~
                         HT-Type.ImportProg WHEN AVAILABLE HT-Type ~
                         HT-Type.EksportProg WHEN AVAILABLE HT-Type ~
                         HT-Type.HTAktiv WHEN AVAILABLE HT-Type
                         
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
            message "{&br-tabell} finnes allerede med TypeId:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
          return "AVBRYT". ~
        end.
        
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter ~
  HT-Type.Betegnelse ~
  HT-Type.ImportKatalog ~
  HT-Type.FilPrefix ~
  HT-Type.FilEkstent ~
  HT-Type.Notat ~
  HT-Type.EksportKatalog WHEN AVAILABLE HT-Type ~
  HT-Type.EkspFilPrefix WHEN AVAILABLE HT-Type ~
  HT-Type.EkspFilEkstent WHEN AVAILABLE HT-Type ~
  HT-Type.ImportProg WHEN AVAILABLE HT-Type ~
  HT-Type.EksportProg WHEN AVAILABLE HT-Type ~
  HT-Type.HTAktiv WHEN AVAILABLE HT-Type

/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
/* Står etter forrige assign. Dvs egen linje.                   */
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
&Scoped-define INTERNAL-TABLES HT-Type

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame HT-Type.TypeId ~
HT-Type.Betegnelse HT-Type.HTAktiv HT-Type.ImportKatalog HT-Type.FilPrefix ~
HT-Type.FilEkstent HT-Type.importProg HT-Type.Eksportkatalog ~
HT-Type.EkspFilPrefix HT-Type.EkspFilEkstent HT-Type.eksportProg ~
HT-Type.Notat 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame HT-Type.TypeId ~
HT-Type.Betegnelse HT-Type.HTAktiv HT-Type.ImportKatalog HT-Type.FilPrefix ~
HT-Type.FilEkstent HT-Type.importProg HT-Type.Eksportkatalog ~
HT-Type.EkspFilPrefix HT-Type.EkspFilEkstent HT-Type.eksportProg ~
HT-Type.Notat 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame HT-Type
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame HT-Type
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH HT-Type SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH HT-Type SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame HT-Type
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame HT-Type


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS HT-Type.TypeId HT-Type.Betegnelse ~
HT-Type.HTAktiv HT-Type.ImportKatalog HT-Type.FilPrefix HT-Type.FilEkstent ~
HT-Type.importProg HT-Type.Eksportkatalog HT-Type.EkspFilPrefix ~
HT-Type.EkspFilEkstent HT-Type.eksportProg HT-Type.Notat 
&Scoped-define ENABLED-TABLES HT-Type
&Scoped-define FIRST-ENABLED-TABLE HT-Type
&Scoped-Define ENABLED-OBJECTS RECT-54 Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-FIELDS HT-Type.TypeId HT-Type.Betegnelse ~
HT-Type.HTAktiv HT-Type.ImportKatalog HT-Type.FilPrefix HT-Type.FilEkstent ~
HT-Type.importProg HT-Type.Eksportkatalog HT-Type.EkspFilPrefix ~
HT-Type.EkspFilEkstent HT-Type.eksportProg HT-Type.Notat 
&Scoped-define DISPLAYED-TABLES HT-Type
&Scoped-define FIRST-DISPLAYED-TABLE HT-Type
&Scoped-Define DISPLAYED-OBJECTS FI-Tekst1 FI-Tekst2 FI-Tekst3 FI-Tekst-4 

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

DEFINE VARIABLE FI-Tekst-4 AS CHARACTER FORMAT "X(256)":U INITIAL "Programnavn" 
      VIEW-AS TEXT 
     SIZE 29 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst1 AS CHARACTER FORMAT "X(256)":U INITIAL "Katalog" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst2 AS CHARACTER FORMAT "X(256)":U INITIAL "Prefix" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Tekst3 AS CHARACTER FORMAT "X(256)":U INITIAL "Ekstent" 
      VIEW-AS TEXT 
     SIZE 14 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 147 BY 5.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      HT-Type SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     HT-Type.TypeId AT ROW 1.95 COL 16 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     HT-Type.Betegnelse AT ROW 1.95 COL 23 COLON-ALIGNED NO-LABEL FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 47 BY 1
     HT-Type.HTAktiv AT ROW 2 COL 102 
          VIEW-AS TOGGLE-BOX
          SIZE 23 BY .81
     HT-Type.ImportKatalog AT ROW 3.86 COL 16 COLON-ALIGNED
          LABEL "Import" FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 54 BY 1
     HT-Type.FilPrefix AT ROW 3.86 COL 71 COLON-ALIGNED NO-LABEL FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     HT-Type.FilEkstent AT ROW 3.86 COL 85 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     HT-Type.importProg AT ROW 3.86 COL 99 COLON-ALIGNED NO-LABEL FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 46 BY 1
     HT-Type.Eksportkatalog AT ROW 5.05 COL 16 COLON-ALIGNED
          LABEL "Eksport" FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 54 BY 1
     HT-Type.EkspFilPrefix AT ROW 5.05 COL 71 COLON-ALIGNED NO-LABEL FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     HT-Type.EkspFilEkstent AT ROW 5.05 COL 85 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 13.6 BY 1
     HT-Type.eksportProg AT ROW 5.05 COL 99 COLON-ALIGNED NO-LABEL FORMAT "X(40)"
          VIEW-AS FILL-IN 
          SIZE 45 BY 1
     HT-Type.Notat AT ROW 7.19 COL 2 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 147 BY 8.1
     Btn_OK AT ROW 15.52 COL 2
     Btn_Cancel AT ROW 15.52 COL 18
     Btn_Help AT ROW 15.52 COL 133
     FI-Tekst1 AT ROW 3.14 COL 16 COLON-ALIGNED NO-LABEL
     FI-Tekst2 AT ROW 3.14 COL 71 COLON-ALIGNED NO-LABEL
     FI-Tekst3 AT ROW 3.14 COL 85 COLON-ALIGNED NO-LABEL
     FI-Tekst-4 AT ROW 3.14 COL 100 COLON-ALIGNED NO-LABEL
     RECT-54 AT ROW 1.48 COL 2
     SPACE(0.39) SKIP(10.27)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedlikehold håndterminaltyper"
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

/* SETTINGS FOR FILL-IN HT-Type.Betegnelse IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN HT-Type.EkspFilPrefix IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN HT-Type.Eksportkatalog IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN HT-Type.eksportProg IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN FI-Tekst-4 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tekst1 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tekst2 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Tekst3 IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN HT-Type.FilPrefix IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN HT-Type.ImportKatalog IN FRAME Dialog-Frame
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN HT-Type.importProg IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "skotex.HT-Type"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Vedlikehold håndterminaltyper */
DO:
  run LagrePost.
  if return-value = "AVBRYT" then
    return no-apply.  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedlikehold håndterminaltyper */
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
  DISPLAY FI-Tekst1 FI-Tekst2 FI-Tekst3 FI-Tekst-4 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE HT-Type THEN 
    DISPLAY HT-Type.TypeId HT-Type.Betegnelse HT-Type.HTAktiv 
          HT-Type.ImportKatalog HT-Type.FilPrefix HT-Type.FilEkstent 
          HT-Type.importProg HT-Type.Eksportkatalog HT-Type.EkspFilPrefix 
          HT-Type.EkspFilEkstent HT-Type.eksportProg HT-Type.Notat 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-54 HT-Type.TypeId HT-Type.Betegnelse HT-Type.HTAktiv 
         HT-Type.ImportKatalog HT-Type.FilPrefix HT-Type.FilEkstent 
         HT-Type.importProg HT-Type.Eksportkatalog HT-Type.EkspFilPrefix 
         HT-Type.EkspFilEkstent HT-Type.eksportProg HT-Type.Notat Btn_OK 
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

