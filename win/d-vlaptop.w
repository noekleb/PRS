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
&scoped-define br-tabell LapTop
&scoped-define KeyFelt LapTopNr
&scoped-define OptKeyAssign

&scoped-define DataType INT /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster 

/* Ekstra informasjon i find/where når det er flere ledd i indeks */
/* Brukes før KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster LapTop.LapTopNr when available LapTop ~
                         LapTop.Navn when available LapTop ~
                         LapTop.Merknad when available LapTop ~
                         LapTop.DatoSisteKopi when available LapTop ~
                         LapTop.DatoSisOverf when available LapTop ~
                         LapTop.BrukerSistKopi when available LapTop ~
                         LapTop.BrukerSisOverf when available LapTop ~
                         LapTop.LokDB-Katalog when available LapTop ~
                         LapTop.LokDB-Navn when available LapTop ~
                         LapTop.OppkoblParam when available LapTop ~
                         LapTop.ServerDBKopi when available LapTop
                         
/* Alternative poster som skal vises når VisPost kjøres */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData ~
  if available LapTop then  ~
  assign ~
    FI-EndretInfo = (if LapTop.RegistrertDato <> ? ~
                      then string(LapTop.RegistrertDato) ~
                      else "") + " " + ~
                    string(LapTop.RegistrertTid,"HH:MM:SS") + " " + ~
                    LapTop.RegistrertAv + " / " + ~
                    (if LapTop.EDato <> ? ~
                       then string(LapTop.EDato) ~
                       else "") + " " + ~
                    string(LapTop.ETid,"HH:MM:SS") + " " + ~
                    LapTop.BrukerId. ~
  else ~
    FI-EndretInfo = "". ~
  display FI-EndretInfo with frame Dialog-Frame.

/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes if can-find({&br-tabell} where ~
         {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)) or ~
         int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
        do: ~
          if int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
            message "{&KeyFelt} må være større enn 0" ~
            view-as alert-box title "Lagringsfeil". ~
          else ~
            message "{&br-tabell} finnes allerede med LapTopNr:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
          return "AVBRYT". ~
        end.
        
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter ~
  LapTop.Navn ~
  LapTop.Merknad ~
  LapTop.LokDB-Katalog ~
  LapTop.LokDB-Navn ~
  LapTop.OppkoblParam ~
  LapTop.ServerDBKopi

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
&Scoped-define INTERNAL-TABLES LapTop

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame LapTop.LapTopNr LapTop.Navn ~
LapTop.Merknad LapTop.LokDB-Katalog LapTop.LokDB-Navn LapTop.OppkoblParam ~
LapTop.ServerDBKopi LapTop.BrukerSisOverf LapTop.DatoSisOverf ~
LapTop.BrukerSistKopi LapTop.DatoSisteKopi 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame LapTop.Navn ~
LapTop.Merknad LapTop.LokDB-Katalog LapTop.LokDB-Navn LapTop.OppkoblParam ~
LapTop.ServerDBKopi 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame LapTop
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame LapTop
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH LapTop SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame LapTop
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame LapTop


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS LapTop.Navn LapTop.Merknad ~
LapTop.LokDB-Katalog LapTop.LokDB-Navn LapTop.OppkoblParam ~
LapTop.ServerDBKopi 
&Scoped-define ENABLED-TABLES LapTop
&Scoped-define FIRST-ENABLED-TABLE LapTop
&Scoped-define DISPLAYED-TABLES LapTop
&Scoped-define FIRST-DISPLAYED-TABLE LapTop
&Scoped-Define ENABLED-OBJECTS RECT-53 RECT-54 Btn_OK Btn_Cancel Btn_Help ~
FI-EndretInfo 
&Scoped-Define DISPLAYED-FIELDS LapTop.LapTopNr LapTop.Navn LapTop.Merknad ~
LapTop.LokDB-Katalog LapTop.LokDB-Navn LapTop.OppkoblParam ~
LapTop.ServerDBKopi LapTop.BrukerSisOverf LapTop.DatoSisOverf ~
LapTop.BrukerSistKopi LapTop.DatoSisteKopi 
&Scoped-Define DISPLAYED-OBJECTS FI-EndretInfo 

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

DEFINE VARIABLE FI-EndretInfo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Opprettet/endret" 
      VIEW-AS TEXT 
     SIZE 63 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-53
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 128 BY 8.1.

DEFINE RECTANGLE RECT-54
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 128 BY 3.1.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      LapTop SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     LapTop.LapTopNr AT ROW 1.95 COL 35 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     LapTop.Navn AT ROW 1.95 COL 46 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     LapTop.Merknad AT ROW 3.14 COL 35 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     LapTop.LokDB-Katalog AT ROW 4.33 COL 35 COLON-ALIGNED
          LABEL "Lokal databasekatalog"
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     LapTop.LokDB-Navn AT ROW 5.52 COL 35 COLON-ALIGNED
          LABEL "Lokal database navn"
          VIEW-AS FILL-IN 
          SIZE 22 BY 1
     LapTop.OppkoblParam AT ROW 6.71 COL 35 COLON-ALIGNED FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 92 BY 1
     LapTop.ServerDBKopi AT ROW 7.91 COL 35 COLON-ALIGNED FORMAT "X(200)"
          VIEW-AS FILL-IN 
          SIZE 92 BY 1
     LapTop.BrukerSisOverf AT ROW 10.29 COL 35 COLON-ALIGNED
          LABEL "Sist overført til server"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     LapTop.DatoSisOverf AT ROW 10.29 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     LapTop.BrukerSistKopi AT ROW 11.48 COL 35 COLON-ALIGNED
          LABEL "LapTop sist oppdatert"
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     LapTop.DatoSisteKopi AT ROW 11.48 COL 53 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Btn_OK AT ROW 14.57 COL 2
     Btn_Cancel AT ROW 14.57 COL 18
     Btn_Help AT ROW 14.57 COL 116
     FI-EndretInfo AT ROW 13.38 COL 18 COLON-ALIGNED
     RECT-53 AT ROW 1.48 COL 3
     RECT-54 AT ROW 9.81 COL 3
     SPACE(0.99) SKIP(3.08)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedlikehold av LapTop's oppkoblingsparametre"
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

/* SETTINGS FOR FILL-IN LapTop.BrukerSisOverf IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN LapTop.BrukerSistKopi IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN LapTop.DatoSisOverf IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LapTop.DatoSisteKopi IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LapTop.LapTopNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN LapTop.LokDB-Katalog IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN LapTop.LokDB-Navn IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN LapTop.OppkoblParam IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR FILL-IN LapTop.ServerDBKopi IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.LapTop"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedlikehold av LapTop's oppkoblingsparametre */
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
  DISPLAY FI-EndretInfo 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE LapTop THEN 
    DISPLAY LapTop.LapTopNr LapTop.Navn LapTop.Merknad LapTop.LokDB-Katalog 
          LapTop.LokDB-Navn LapTop.OppkoblParam LapTop.ServerDBKopi 
          LapTop.BrukerSisOverf LapTop.DatoSisOverf LapTop.BrukerSistKopi 
          LapTop.DatoSisteKopi 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-53 RECT-54 LapTop.Navn LapTop.Merknad LapTop.LokDB-Katalog 
         LapTop.LokDB-Navn LapTop.OppkoblParam LapTop.ServerDBKopi Btn_OK 
         Btn_Cancel Btn_Help FI-EndretInfo 
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

