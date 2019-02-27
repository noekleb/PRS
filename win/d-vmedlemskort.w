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
&scoped-define br-tabell MedlemsKort
&scoped-define KeyFelt KortNr
&scoped-define OptKeyAssign ~
               MedlemsKort.MedlemsNr = wMedlemsNr

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
                         {&br-tabell}.KundekortNr when available {&br-tabell} ~
                         {&br-tabell}.Merknad when available {&br-tabell} ~
                         {&br-tabell}.KundeRabattKort when available {&br-tabell} ~
                         {&br-tabell}.Sperret when available {&br-tabell}
                         
/* Alternative poster som skal vises når VisPost kjøres */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData 

/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes if can-find({&br-tabell} where ~
         {&br-tabell}.MedlemsNr    = wMedlemsNr AND ~
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
                           {&br-tabell}.KundeKortNr ~
                           {&br-tabell}.Merknad ~
                           {&br-tabell}.KundeRabattKort ~
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
DEF VAR wMaksLengde   AS INT   NO-UNDO.
DEF VAR wTekst        AS CHAR  NO-UNDO.
DEF VAR wAlfaTegn     AS LOG   NO-UNDO.
DEF VAR wLoop         AS INT   NO-UNDO.
DEF VAR wGyldighet    AS INT   NO-UNDO.
DEF VAR cManuellt     AS CHAR  NO-UNDO.  /* manuell inmatning av medlemskortnr om värde = 1 */
DEF VAR iCL           AS INT   NO-UNDO.
DEF VAR iOpprettKKort AS INT   NO-UNDO.
DEF VAR cKortNr       AS CHAR  NO-UNDO.
DEF VAR wKKortRecid   AS RECID NO-UNDO.
DEF VAR wIntertKKortId AS DEC  NO-UNDO.

DEF BUFFER bufMedlemsKort FOR Medlemskort.
DEF BUFFER bufKundeKort   FOR Kundekort.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS MedlemsKort.AktivertDato ~
MedlemsKort.UtgarDato MedlemsKort.Innehaver MedlemsKort.Merknad ~
MedlemsKort.Sperret MedlemsKort.KundeRabattKort MedlemsKort.KundeKortNr 
&Scoped-define ENABLED-TABLES MedlemsKort
&Scoped-define FIRST-ENABLED-TABLE MedlemsKort
&Scoped-Define ENABLED-OBJECTS Btn_OK Btn_Cancel Btn_Help RECT-1 
&Scoped-Define DISPLAYED-FIELDS MedlemsKort.KortNr MedlemsKort.AktivertDato ~
MedlemsKort.UtgarDato MedlemsKort.Innehaver MedlemsKort.Merknad ~
MedlemsKort.Sperret MedlemsKort.KundeRabattKort MedlemsKort.KundeKortNr 
&Scoped-define DISPLAYED-TABLES MedlemsKort
&Scoped-define FIRST-DISPLAYED-TABLE MedlemsKort


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
     SIZE 79.8 BY 9.52.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     MedlemsKort.KortNr AT ROW 2.67 COL 25 COLON-ALIGNED FORMAT "X(22)"
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     MedlemsKort.AktivertDato AT ROW 3.67 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     MedlemsKort.UtgarDato AT ROW 4.67 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13.2 BY 1
     MedlemsKort.Innehaver AT ROW 5.67 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY 1
     MedlemsKort.Merknad AT ROW 7.67 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     MedlemsKort.Sperret AT ROW 8.81 COL 27
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .81
     MedlemsKort.KundeRabattKort AT ROW 9.76 COL 27
          VIEW-AS TOGGLE-BOX
          SIZE 42 BY .81
     Btn_OK AT ROW 12.67 COL 2
     Btn_Cancel AT ROW 12.67 COL 17
     Btn_Help AT ROW 12.67 COL 67
     MedlemsKort.KundeKortNr AT ROW 6.67 COL 25 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     RECT-1 AT ROW 1.48 COL 2.2
     SPACE(0.59) SKIP(2.94)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Medlemskort"
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
   FRAME-NAME Custom                                                    */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN MedlemsKort.KortNr IN FRAME Dialog-Frame
   NO-ENABLE EXP-FORMAT                                                 */
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
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Medlemskort */
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


&Scoped-define SELF-NAME MedlemsKort.KundeRabattKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MedlemsKort.KundeRabattKort Dialog-Frame
ON RETURN OF MedlemsKort.KundeRabattKort IN FRAME Dialog-Frame /* Kunderabattkort */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME MedlemsKort.Sperret
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL MedlemsKort.Sperret Dialog-Frame
ON RETURN OF MedlemsKort.Sperret IN FRAME Dialog-Frame /* Sperret */
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
      
{syspara.i  5 1 1 iCL INT}
{syspara.i 14 1 6 iOpprettKKort INT}
{syspara.i 14 1 1 wMaksLengde INT}
{syspara.i 14 1 2 wTekst}
{syspara.i 14 1 3 wGyldighet  INT}
{syspara.i 14 1 5 cManuellt} /* manuell inmatning av medlemskortnr */
ASSIGN
    wAlfaTegn     = IF CAN-DO("Nei,No,False",wTekst)
                      THEN FALSE
                      ELSE TRUE.

FIND Medlem NO-LOCK WHERE
    Medlem.MedlemsNr = wMedlemsNr NO-ERROR.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
/*    def var wStr AS CHAR.                                           */
/*   FOR EACH Meny NO-LOCK:                                           */
/*       wStr =                                                       */
/*           IF wStr = "" THEN Meny.Navn ELSE wStr + "," + Meny.Navn. */
/*   END.                                                             */
  RUN enable_UI.
  {lng.i}
  run VisPost.
/*   ASSIGN                                                */
/*       Medlemskort.KortNr:FORMAT = FILL("X",wMaksLengde) */
/*       .                                                 */
  if wModus = "Ny" THEN DO:
      IF cManuellt = "1" THEN DO:
          RUN d-kortnummer.w.
          IF RETURN-VALUE = "AVBRYT" THEN
              RETURN.
          ELSE cKortNr = RETURN-VALUE.
          ASSIGN
              MedlemsKort.KortNr:SCREEN-VALUE          = cKortNr
              MedlemsKort.Innehaver:SCREEN-VALUE       = Medlem.ForNavn + " " + Medlem.EtterNavn
              MedlemsKort.KundeKortNr:SCREEN-VALUE     = IF iOpprettKKort = 1
                                                           THEN cKortNr
                                                           ELSE MedlemsKort.KundeKortNr:SCREEN-VALUE
              MedlemsKort.KundeRabattKort:SCREEN-VALUE = IF iOpprettKKort = 1
                                                           THEN "Yes"
                                                           ELSE MedlemsKort.KundeRabattKort:SCREEN-VALUE
              .
      END.
      assign
  /*       {&br-tabell}.{&KeyFelt}:sensitive = true */
        MedlemsKort.AktivertDato:SCREEN-VALUE = string(TODAY)
        MedlemsKort.UtgarDato:SCREEN-VALUE    = string(TODAY + (IF wGyldighet = 0
                                                         THEN 30
                                                         ELSE wGyldighet))
        MedlemsKort.Innehaver:SCREEN-VALUE    = Medlem.ForNavn + " " +
                                                MEdlem.EtterNavn
        MedlemsKort.KundeKort:SCREEN-VALUE    = IF iOpprettKKort = 1 
                                                  THEN cKortNr
                                                  ELSE MedlemsKort.KundeKort:SCREEN-VALUE
        .
  END.
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
  IF AVAILABLE MedlemsKort THEN 
    DISPLAY MedlemsKort.KortNr MedlemsKort.AktivertDato MedlemsKort.UtgarDato 
          MedlemsKort.Innehaver MedlemsKort.Merknad MedlemsKort.Sperret 
          MedlemsKort.KundeRabattKort MedlemsKort.KundeKortNr 
      WITH FRAME Dialog-Frame.
  ENABLE MedlemsKort.AktivertDato MedlemsKort.UtgarDato MedlemsKort.Innehaver 
         MedlemsKort.Merknad MedlemsKort.Sperret MedlemsKort.KundeRabattKort 
         Btn_OK Btn_Cancel Btn_Help MedlemsKort.KundeKortNr RECT-1 
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
      wTekst = INPUT MedlemsKort.KortNr
      wTekst = LEFT-TRIM(wTekst)
      wTekst = LEFT-TRIM(wTekst,"0")
      .
  IF INPUT MedlemsKort.AktivertDato = ? OR
     INPUT MedlemsKort.UtgarDato    = ? THEN
  DO:
      MESSAGE "Aktiveringsdato og utgårdato må angis."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY "AVBRYT".
  END.
/*   IF wAlfaTegn = FALSE THEN                                                                     */
/*   DO:                                                                                           */
/*       do wLoop = 1 TO LENGTH(INPUT MedlemsKort.KortNr):                                         */
/*           IF NOT can-do("0,1,2,3,4,5,6,7,8,9",SUBSTRING(INPUT MedlemsKort.KortNr,wLoop,1)) THEN */
/*           DO:                                                                                   */
/*               MESSAGE "Kun siffer kan benyttes i kortnummeret."                                 */
/*                   VIEW-AS ALERT-BOX error BUTTONS OK TITLE "Lagringsfeil".                      */
/*               RETURN NO-APPLY "AVBRYT".                                                         */
/*           END.                                                                                  */
/*       END.                                                                                      */
/*   END.                                                                                          */
/*   IF LENGTH(INPUT MedlemsKort.KortNr) > wMaksLengde THEN                                        */
/*   DO:                                                                                           */
/*       MESSAGE "For langt kortnummer." SKIP                                                      */
/*               "Maksimalt " wMaksLengde " siffer kan benyttes."                                  */
/*           VIEW-AS ALERT-BOX ERROR BUTTONS OK TITLE "Lagringsfeil".                              */
/*       RETURN NO-APPLY "AVBRYT".                                                                 */
/*   END.                                                                                          */
  IF wModus = "Ny" THEN
  DO:
      IF cManuellt = "1" THEN
          ASSIGN wTekst = Medlemskort.KortNr:SCREEN-VALUE.
      ELSE DO:
          FIND LAST Medlemskort WHERE Medlemskort.Kortnr > (STRING(Medlem.ButikkNr) + "000000") AND 
                                      Medlemskort.Kortnr < (STRING(Medlem.ButikkNr) + "999999") AND 
                                      LENGTH(Medlemskort.Kortnr) = LENGTH(STRING(Medlem.ButikkNr) + "999999") NO-LOCK NO-ERROR.
          ASSIGN wTekst = IF AVAIL Medlemskort THEN
              STRING(DECI(Medlemskort.Kortnr) + 1) ELSE 
                  STRING(Medlem.ButikkNr) + "000001"
                  skotex.MedlemsKort.KortNr:SCREEN-VALUE = wTekst.
      END.
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
/*       MESSAGE wMedlemsnr                     */
/*           VIEW-AS ALERT-BOX INFO BUTTONS OK. */
      {&SjekkOmPostFinnes}
      {&EkstraSjekk}
      {&OptFind}
      create {&br-tabell}.
      assign 
        {&OptKeyAssign}
        {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)
        wRecid   = recid({&br-tabell}).
      IF Medlem.KundeNr <> 0 THEN DO:
          FIND CURRENT Medlem NO-ERROR.
          IF AVAIL Medlem THEN DO:
              ASSIGN Medlem.EDato = TODAY.
              FIND CURRENT Medlem NO-LOCK.
          END.
      END.

      /* Opprette kundekort */
      IF iOpprettKKort = 1 AND MedlemsKort.KundeKortNr:SCREEN-VALUE <> "" THEN
      DO:
          IF NOT CAN-FIND(KundeKort WHERE
                          KundeKort.KortNr = MedlemsKort.KundeKortNr:SCREEN-VALUE) THEN
          DO:
              CREATE KundeKort.
              ASSIGN
                  KundeKort.KundeNr      = Medlem.KundeNr
                  KundeKort.KortNr       = MedlemsKort.KundeKortNr:SCREEN-VALUE
                  KundeKort.aktivertDato = TODAY
                  KundeKort.UtgarDato    = TODAY + 360
                  KundeKort.Innehaver    = MedlemsKort.Innehaver:SCREEN-VALUE
                  KundeKort.MedlemsNr    = Medlem.MedlemsNr                  
                  .
              ASSIGN
                  MedlemsKort.InterntKKortId = KundeKort.InterntKKortId
                  .
          END.
      END.
    

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
  IF MedlemsKort.InterntKKortId > 0 THEN
  DO:
      FIND FIRST KundeKort EXCLUSIVE-LOCK WHERE
          KundeKort.InterntKKortId = MedlemsKort.InterntKKortId NO-ERROR.
      IF AVAILABLE KundeKort THEN
          ASSIGN
          KundeKort.AktivertDato = MedlemsKort.AktivertDato
          KundeKort.UtgarDato    = Medlemskort.UtgarDato
          KundeKort.Innehaver    = MedlemsKort.Innehaver
          KundeKort.Sperret      = MedlemsKort.Sperret
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

