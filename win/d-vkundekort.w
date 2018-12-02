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
  DEFINE VAR wKundeNr AS DEC NO-UNDO.
  
&ELSE
  DEFINE INPUT-output PARAMETER wRecid   as recid NO-UNDO.
  define input        parameter wModus   as char  no-undo.
  DEFINE INPUT        PARAMETER wKundeNr AS DEC NO-UNDO.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell KundeKort
&scoped-define KeyFelt KortNr
&scoped-define OptKeyAssign ~
               KundeKort.KundeNr = wKundeNr

&scoped-define DataType STRING /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster

/* Ekstra informasjon i find/where når det er flere ledd i indeks */
/* Brukes før KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster {&br-tabell}.KortNr when available {&br-tabell} ~
                         {&br-tabell}.AktivertDato when available {&br-tabell} ~
                         {&br-tabell}.UtgarDato when available {&br-tabell} ~
                         {&br-tabell}.Innehaver when available {&br-tabell} ~
                         {&br-tabell}.Merknad when available {&br-tabell} ~
                         {&br-tabell}.Sperret when available {&br-tabell}
                         
/* Alternative poster som skal vises når VisPost kjøres */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData 

/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes if can-find({&br-tabell} where ~
         {&br-tabell}.KundeNr    = wKundeNr AND ~
         {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)) or ~
         ({&br-tabell}.{&KeyFelt}:screen-value) = '' then ~
        do: ~
          if ({&br-tabell}.{&KeyFelt}:screen-value) = '' then ~
            message "{&KeyFelt} må være større enn 0" ~
            view-as alert-box title "Lagringsfeil". ~
          else ~
            message "{&br-tabell} finnes allerede med nr:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
          return "AVBRYT". ~
        end.
        
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter ~
                           {&br-tabell}.KortNr ~
                           {&br-tabell}.AktivertDato ~
                           {&br-tabell}.UtgarDato ~
                           {&br-tabell}.Innehaver ~
                           {&br-tabell}.Merknad ~
                           {&br-tabell}.Sperret 

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
def var wRetur-Verdi  as char initial "AVBRYT" no-undo.
DEF VAR wMaksLengde   AS INT  NO-UNDO.
DEF VAR wTekst        AS CHAR NO-UNDO.
DEF VAR wAlfaTegn     AS LOG  NO-UNDO.
DEF VAR wLoop         AS INT  NO-UNDO.
DEF VAR wGyldighet    AS INT  NO-UNDO.

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
&Scoped-define INTERNAL-TABLES Handtering Feilkode KundeKort

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame KundeKort.KortNr ~
KundeKort.AktivertDato KundeKort.UtgarDato KundeKort.Innehaver ~
KundeKort.Merknad KundeKort.Sperret 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame KundeKort.KortNr ~
KundeKort.AktivertDato KundeKort.UtgarDato KundeKort.Innehaver ~
KundeKort.Merknad KundeKort.Sperret 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame KundeKort
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame KundeKort
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH MS.Handtering SHARE-LOCK, ~
      EACH MS.Feilkode WHERE TRUE /* Join to MS.Handtering incomplete */ SHARE-LOCK, ~
      EACH KundeKort WHERE TRUE /* Join to MS.Handtering incomplete */ SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH MS.Handtering SHARE-LOCK, ~
      EACH MS.Feilkode WHERE TRUE /* Join to MS.Handtering incomplete */ SHARE-LOCK, ~
      EACH KundeKort WHERE TRUE /* Join to MS.Handtering incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame Handtering Feilkode KundeKort
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame Handtering
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame Feilkode
&Scoped-define THIRD-TABLE-IN-QUERY-Dialog-Frame KundeKort


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS KundeKort.KortNr KundeKort.AktivertDato ~
KundeKort.UtgarDato KundeKort.Innehaver KundeKort.Merknad KundeKort.Sperret 
&Scoped-define ENABLED-TABLES KundeKort
&Scoped-define FIRST-ENABLED-TABLE KundeKort
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel Btn_Help RECT-1 
&Scoped-Define DISPLAYED-FIELDS KundeKort.KortNr KundeKort.AktivertDato ~
KundeKort.UtgarDato KundeKort.Innehaver KundeKort.Merknad KundeKort.Sperret 
&Scoped-define DISPLAYED-TABLES KundeKort
&Scoped-define FIRST-DISPLAYED-TABLE KundeKort


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
     SIZE 79.8 BY 8.81.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      Handtering, 
      Feilkode, 
      KundeKort SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     KundeKort.KortNr AT ROW 2.91 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     KundeKort.AktivertDato AT ROW 3.91 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     KundeKort.UtgarDato AT ROW 4.91 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     KundeKort.Innehaver AT ROW 5.91 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     KundeKort.Merknad AT ROW 6.91 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     KundeKort.Sperret AT ROW 8.14 COL 23
          VIEW-AS TOGGLE-BOX
          SIZE 28 BY .81
     Btn_OK AT ROW 10.52 COL 2
     Btn_Cancel AT ROW 10.52 COL 17
     Btn_Help AT ROW 10.52 COL 67
     RECT-1 AT ROW 1.48 COL 2.2
     SPACE(0.59) SKIP(1.41)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Kundekort"
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

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "MS.Handtering,MS.Feilkode WHERE MS.Handtering ...,skotex.KundeKort WHERE MS.Handtering ..."
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Kundekort */
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


&Scoped-define SELF-NAME KundeKort.Sperret
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KundeKort.Sperret Dialog-Frame
ON RETURN OF KundeKort.Sperret IN FRAME Dialog-Frame /* Sperret */
DO:
  APPLY "TAB" TO SELF.
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
    
{syspara.i 14 2 1 wMaksLengde INT}
{syspara.i 14 2 2 wTekst}
{syspara.i 14 2 3 wGyldighet  INT}
ASSIGN
    wAlfaTegn     = IF CAN-DO("Nei,No,False",wTekst)
                      THEN FALSE
                      ELSE TRUE.
      
FIND Kunde NO-LOCK WHERE
    Kunde.KundeNr = wKundeNr NO-ERROR.

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
  {lng.i}

  run VisPost.

  ASSIGN
      Kundekort.KortNr:FORMAT = FILL("X",wMaksLengde)
      .
  if wModus = "Ny" then
    assign
      {&br-tabell}.{&KeyFelt}:sensitive = true
      KundeKort.AktivertDato:SCREEN-VALUE = string(TODAY)
      KundeKort.UtgarDato:SCREEN-VALUE    = string(TODAY + (IF wGyldighet = 0
                                                       THEN 30
                                                       ELSE wGyldighet))
      KundeKort.Innehaver:SCREEN-VALUE    = Kunde.Navn
      .
  else
    assign
      {&br-tabell}.{&KeyFelt}:sensitive = false.

/*   IF KundeKort.Innehaver = "" THEN                      */
/*       KundeKort.Innehaver:SCREEN-VALUE    = Kunde.Navn. */
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
  IF AVAILABLE KundeKort THEN 
    DISPLAY KundeKort.KortNr KundeKort.AktivertDato KundeKort.UtgarDato 
          KundeKort.Innehaver KundeKort.Merknad KundeKort.Sperret 
      WITH FRAME Dialog-Frame.
  ENABLE KundeKort.KortNr KundeKort.AktivertDato KundeKort.UtgarDato 
         KundeKort.Innehaver KundeKort.Merknad KundeKort.Sperret Btn_OK 
         Btn_Cancel Btn_Help RECT-1 
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
    ASSIGN
        wTekst = INPUT KundeKort.KortNr
        wTekst = LEFT-TRIM(wTekst)
        wTekst = LEFT-TRIM(wTekst,"0")
        .

    IF LENGTH(INPUT KundeKort.KortNr) > wMaksLengde THEN
    DO:
        MESSAGE "For mange siffer i kortnummer." SKIP
                "Maksimalt " wMaksLengde " siffer kan benyttes."
            VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Lagringsfeil".
        APPLY "ENTRY" TO KundeKort.Kortnr.
        RETURN NO-APPLY "AVBRYT".
    END.

    IF wAlfaTegn = FALSE THEN
    DO:
        do wLoop = 1 TO LENGTH(INPUT KundeKort.KortNr):
            IF NOT can-do("0,1,2,3,4,5,6,7,8,9",SUBSTRING(INPUT KundeKort.KortNr,wLoop,1)) THEN
            DO:
                MESSAGE "Kun siffer kan benyttes i kortnummeret."
                    VIEW-AS ALERT-BOX error BUTTONS OK TITLE "Lagringsfeil".
                APPLY "ENTRY" TO KundeKort.Kortnr.
                RETURN NO-APPLY "AVBRYT".
            END.
        END.
        IF INPUT KundeKort.KortNr BEGINS "0" THEN
        DO:
            MESSAGE "Kortnummeret kan ikke begynne med '0'."
                VIEW-AS ALERT-BOX error BUTTONS OK TITLE "Lagringsfeil".
            APPLY "ENTRY" TO KundeKort.Kortnr.
            RETURN NO-APPLY "AVBRYT".
        END.
    END.
    IF wModus = "Ny" THEN
    DO:
        RUN sjekkomkorterunikt.p (INPUT wTekst).
        IF RETURN-VALUE <> "" THEN
        DO:
            MESSAGE RETURN-VALUE
                VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Lagringsfeil".
            RETURN NO-APPLY "AVBRYT".
        END.
    END.

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

  /* Koblede kundekort skal oppdateres med endringe ri Innehaver feltet. */
  IF KundeKort.InterntKKortId > 0 and
     can-find(MedlemsKort WHERE 
              MedlemsKort.InterntKKortId = KundeKort.InterntKKortId) THEN
  DO:

      FIND FIRST MedlemsKort EXCLUSIVE-LOCK WHERE
          MedlemsKort.InterntKKortId = KundeKort.InterntKKortId NO-ERROR.
      IF AVAILABLE MedlemsKort THEN
          ASSIGN
          MedlemsKort.AktivertDato = KundeKort.AktivertDato
          MedlemsKort.UtgarDato    = KundeKort.UtgarDato
          MedlemsKort.Innehaver    = KundeKort.Innehaver
          MedlemsKort.Sperret      = KundeKort.Sperret
          .
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

