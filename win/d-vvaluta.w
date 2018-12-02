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
&scoped-define br-tabell Valuta
&scoped-define KeyFelt ValKod
&scoped-define OptKeyAssign

&scoped-define DataType STRING /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster 

/* Ekstra informasjon i find/where når det er flere ledd i indeks */
/* Brukes før KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster ~
  Valuta.ValKod when available Valuta ~
  Valuta.ValKurs when available Valuta ~
  Valuta.EDato when available Valuta ~
  Valuta.ValLand when available Valuta ~
  Valuta.Valnavn when available Valuta ~
  Valuta.indeks when available Valuta AND Valuta.Indeks > 0                         
/* Alternative poster som skal vises når VisPost kjøres */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData

/* Ved opprettelse - Sjekker om posten finnes fra før */
&scoped-define SjekkOmPostFinnes if can-find({&br-tabell} where ~
         {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)) then ~
        do: ~
            message "{&br-tabell} finnes allerede med valutakode:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
          return "AVBRYT". ~
        end.
        
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter ~
  Valuta.ValKod ~
  Valuta.ValKurs ~
  Valuta.ValLand ~
  Valuta.Valnavn ~
  Valuta.indeks  
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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Valuta

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame Valuta.ValKod Valuta.ValNavn ~
Valuta.ValLand Valuta.ValKurs Valuta.indeks Valuta.EDato 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame Valuta.ValKod ~
Valuta.ValNavn Valuta.ValLand Valuta.ValKurs 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame Valuta
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame Valuta
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH Valuta SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH Valuta SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame Valuta
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame Valuta


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Valuta.ValKod Valuta.ValNavn Valuta.ValLand ~
Valuta.ValKurs 
&Scoped-define ENABLED-TABLES Valuta
&Scoped-define FIRST-ENABLED-TABLE Valuta
&Scoped-Define ENABLED-OBJECTS RECT-1 Btn_OK Btn_Cancel Btn_Help ~
FI-kursinfo FI-antallinfo 
&Scoped-Define DISPLAYED-FIELDS Valuta.ValKod Valuta.ValNavn Valuta.ValLand ~
Valuta.ValKurs Valuta.indeks Valuta.EDato 
&Scoped-define DISPLAYED-TABLES Valuta
&Scoped-define FIRST-DISPLAYED-TABLE Valuta
&Scoped-Define DISPLAYED-OBJECTS FI-kursinfo FI-antallinfo 

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

DEFINE VARIABLE FI-antallinfo AS CHARACTER FORMAT "X(256)":U INITIAL "(Antall i valutanotering)" 
      VIEW-AS TEXT 
     SIZE 27 BY .62 NO-UNDO.

DEFINE VARIABLE FI-kursinfo AS CHARACTER FORMAT "X(256)":U INITIAL "(Per 1 av utlandsk valuta)" 
      VIEW-AS TEXT 
     SIZE 27 BY .62 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 7.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      Valuta SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     Valuta.ValKod AT ROW 2.05 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 8.6 BY 1
     Valuta.ValNavn AT ROW 3.05 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     Valuta.ValLand AT ROW 4.05 COL 15 COLON-ALIGNED FORMAT "x(205)"
          VIEW-AS FILL-IN 
          SIZE 36.4 BY 1
     Valuta.ValKurs AT ROW 5.05 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 15 BY 1
     Valuta.indeks AT ROW 6 COL 15 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 5
          LIST-ITEMS "0" 
          DROP-DOWN-LIST
          SIZE 15 BY 1
     Valuta.EDato AT ROW 7 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     Btn_OK AT ROW 8.76 COL 2.4
     Btn_Cancel AT ROW 8.76 COL 17.8
     Btn_Help AT ROW 8.76 COL 46.4
     FI-kursinfo AT ROW 5.24 COL 31 COLON-ALIGNED NO-LABEL
     FI-antallinfo AT ROW 6.24 COL 31 COLON-ALIGNED NO-LABEL
     RECT-1 AT ROW 1.24 COL 2
     SPACE(0.00) SKIP(1.28)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedlikehold kalkylevalutaer"
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
   FRAME-NAME                                                           */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE
       FRAME Dialog-Frame:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Valuta.EDato IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Valuta.indeks IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Valuta.ValLand IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "SkoTex.Valuta"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON GO OF FRAME Dialog-Frame /* Vedlikehold kalkylevalutaer */
DO:
  RUN ValiderInput.
  if return-value = "AVBRYT" then
    return no-apply.    
  run LagrePost.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedlikehold kalkylevalutaer */
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
  RUN InitCombo.
  {lng.i} RUN enable_UI.

  run VisPost.
  
  if wModus = "Ny" then
    assign
      {&br-tabell}.{&KeyFelt}:sensitive = true
      {&br-tabell}.indeks:SCREEN-VALUE  = "1".
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
  DISPLAY FI-kursinfo FI-antallinfo 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE Valuta THEN 
    DISPLAY Valuta.ValKod Valuta.ValNavn Valuta.ValLand Valuta.ValKurs 
          Valuta.indeks Valuta.EDato 
      WITH FRAME Dialog-Frame.
  ENABLE RECT-1 Valuta.ValKod Valuta.ValNavn Valuta.ValLand Valuta.ValKurs 
         Btn_OK Btn_Cancel Btn_Help FI-kursinfo FI-antallinfo 
      WITH FRAME Dialog-Frame.
  VIEW FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitCombo Dialog-Frame 
PROCEDURE InitCombo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN Valuta.Indeks:LIST-ITEMS IN FRAME {&FRAME-NAME} = "1,100".
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderInput Dialog-Frame 
PROCEDURE ValiderInput :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
  /*
  IF LENGTH(INPUT Valuta.ValKod) <> 3 THEN DO:
      MESSAGE "Valutakod skal ha 3 tegn."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO Valuta.ValKod.
      RETURN "AVBRYT".
  END.
  */
  IF INPUT Valuta.Valnavn = "" THEN DO:
      MESSAGE "Registrer langt valutanavn."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO Valuta.ValNavn.
      RETURN "AVBRYT".
  END.
  IF NOT CAN-DO(TRIM(Valuta.Indeks:LIST-ITEMS), STRING(INPUT Valuta.Indeks)) THEN DO:
      MESSAGE "Gyldige verdier Antall: " TRIM(Valuta.Indeks:LIST-ITEMS)
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY "ENTRY" TO Valuta.Indeks.
      RETURN "AVBRYT".
  END.
  /* Vi har förberett, detta kommer */
/*   IF INPUT Valuta.Retur <> 0 AND INPUT Valuta.Indeks * INPUT Valuta.ValKurs > INPUT Valuta.Retur THEN DO: */
/*       MESSAGE "Returkursen < betalkursen"                                                                 */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                              */
/*       APPLY "ENTRY" TO Valuta.Retur.                                                                      */
/*       RETURN "AVBRYT".                                                                                    */
/*   END.                                                                                                    */
/*   Valuta.retur */
 END.

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

