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
&scoped-define br-tabell StrTStr
&scoped-define KeyFelt StrTypeId
&scoped-define OptKeyAssign

&scoped-define DataType int /* INT STRING DEC Datatype p� n�kkelfelt*/
/* Henter eventuelle relaterte poster - f�r mainblokk */
&scoped-define FinnRelatertePoster 

/* Ekstra informasjon i find/where n�r det er flere ledd i indeks */
/* Brukes f�r KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Felter som skal vises frem n�r rutinen VisPost kj�res */
&scoped-define VisPoster ~
  StrTStr.StrTypeId when available StrTStr ~
  StrTStr.SoStorl when available StrTStr
                         
/* Alternative poster som skal vises n�r VisPost kj�res */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData

/*
/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes if can-find({&br-tabell} where ~
         {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)) or ~
         int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
        do: ~
          if int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
            message "{&KeyFelt} m� v�re st�rre enn 0" ~
            view-as alert-box title "Lagringsfeil". ~
          else ~
            message "{&br-tabell} finnes allerede med fylkesnr:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
          return "AVBRYT". ~
        end.
*/        
        
/* Felter som assign'es n�r rutinen LagrePost kj�res */        
&scoped-define AssignFelter ~
  StrTStr.SoStorl

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
&Scoped-define INTERNAL-TABLES StrTStr

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame StrTStr.StrTypeID ~
StrTStr.SoStorl 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame StrTStr.SoStorl 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame StrTStr
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame StrTStr
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH StrTStr SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame StrTStr
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame StrTStr


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS StrTStr.SoStorl 
&Scoped-define ENABLED-TABLES StrTStr
&Scoped-define FIRST-ENABLED-TABLE StrTStr
&Scoped-define DISPLAYED-TABLES StrTStr
&Scoped-define FIRST-DISPLAYED-TABLE StrTStr
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_OK Btn_Cancel Btn_Help 
&Scoped-Define DISPLAYED-FIELDS StrTStr.StrTypeID StrTStr.SoStorl 

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
     SIZE 55 BY 2.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      StrTStr SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     StrTStr.StrTypeID AT ROW 1.95 COL 18 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     StrTStr.SoStorl AT ROW 1.95 COL 25 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 9.6 BY 1
     Btn_OK AT ROW 3.62 COL 2
     Btn_Cancel AT ROW 3.62 COL 17
     Btn_Help AT ROW 3.62 COL 42
     RECT-1 AT ROW 1.38 COL 2
     SPACE(0.59) SKIP(1.47)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedl. st�rrelser"
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
                                                                        */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN StrTStr.StrTypeID IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "skotex.StrTStr"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedl. st�rrelser */
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


&Scoped-define SELF-NAME StrTStr.SoStorl
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL StrTStr.SoStorl Dialog-Frame
ON LEAVE OF StrTStr.SoStorl IN FRAME Dialog-Frame /* St�relse */
DO:
  def var wTest as char no-undo.
  
  assign 
    wTest = trim(input StrTStr.SoStorl)
    wTest = if length(wTest) = 1 or length(wTest) = 3 
              then " " + wTest
              else wTest.
    StrTStr.SoStorl = wTest.
  display StrTStr.SoStorl with frame Dialog-Frame.
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
  IF AVAILABLE StrTStr THEN 
    DISPLAY StrTStr.StrTypeID StrTStr.SoStorl 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 StrTStr.SoStorl Btn_OK Btn_Cancel Btn_Help 
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

