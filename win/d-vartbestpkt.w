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
  DEFINE VAR wArtikkelNr AS DEC NO-UNDO.
  DEFINE VAR wButikkNr   AS INT NO-UNDO.  
  FIND FIRST ArtBas.
  ASSIGN
      wArtikkelNr = ArtBas.ArtikkelNr
      wButikkNr   = 1
      .
&ELSE
  DEFINE INPUT-output    PARAMETER wRecid as recid NO-UNDO.
  define input           parameter wModus as char  no-undo.
  DEFINE INPUT PARAMETER wArtikkelNr AS DEC NO-UNDO.
  DEFINE INPUT PARAMETER wButikkNr   AS INT NO-UNDO.  
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell ArtBestPkt
&scoped-define KeyFelt StrKode
&scoped-define OptKeyAssign

&scoped-define DataType INT /* INT STRING DEC Datatype på nøkkelfelt*/
/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster 

/* Ekstra informasjon i find/where når det er flere ledd i indeks */
/* Brukes før KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster ~
      ArtBestPkt.ArtikkelNr    when available ArtBestPkt ~
      ArtBestPkt.ButikkNr      when available ArtBestPkt ~
      ArtBestPkt.StrKode       when available ArtBestPkt ~
      ArtBestPkt.MinAnt        when available ArtBestPkt ~
      ArtBestPkt.MaksAnt       when available ArtBestPkt ~
      ArtBestPkt.BestAnt       when available ArtBestPkt ~
      ArtBestPkt.TillatBruttPk when available ArtBestPkt ~
      ArtBas.ArtikkelNr WHEN AVAILABLE ArtBas @ ArtBestPkt.ArtikkelNr ~
      Butiker.Butik WHEN AVAILABLE Butiker @ ArtBestPkt.ButikkNr ~
      ArtBas.Beskr             WHEN AVAILABLE ArtBas     ~
      Butiker.ButNamn          WHEN AVAILABLE Butiker
                         
/* Alternative poster som skal vises når VisPost kjøres */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData
  

/* Sjekk om post finnes */
&scoped-define SjekkOmPostFinnes if can-find({&br-tabell} where ~
         {&br-tabell}.ArtikkelNr  = wArtikkelNr AND ~
         {&br-tabell}.butikkNr    = wButikkNr AND ~
         {&br-tabell}.{&KeyFelt}  = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value))  THEN ~
        do: ~
          if int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
            message "{&KeyFelt} må være større enn 0" ~
            view-as alert-box title "Lagringsfeil". ~
          else ~
            message "{&br-tabell} finnes allerede med størrelseskode:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
           APPLY "ENTRY" TO {&br-tabell}.{&KeyFelt}. ~
          return "AVBRYT". ~
        end.
        
/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter ~
  ArtBestPkt.ArtikkelNr     ~
  ArtBestPkt.ButikkNr       ~
  ArtBestPkt.StrKode        ~
  ArtBestPkt.MinAnt         ~
  ArtBestPkt.MaksAnt        ~
  ArtBestPkt.BestAnt        ~
  ArtBestPkt.TillatBruttPk

/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
/* Står etter forrige assign. Dvs egen linje.                   */
&scoped-define TillegsAssign

/* Local Variable Definitions ---                                       */
def var wRetur-Verdi as char initial "AVBRYT" no-undo.
DEF BUFFER b{&br-tabell} FOR {&br-tabell}.
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
&Scoped-define INTERNAL-TABLES ArtBestPkt ArtBas

/* Definitions for DIALOG-BOX Dialog-Frame                              */
&Scoped-define FIELDS-IN-QUERY-Dialog-Frame ArtBestPkt.ArtikkelNr ~
ArtBas.Beskr ArtBestPkt.ButikkNr ArtBestPkt.StrKode ArtBestPkt.MinAnt ~
ArtBestPkt.MaksAnt ArtBestPkt.BestAnt ArtBestPkt.TillatBruttPk 
&Scoped-define ENABLED-FIELDS-IN-QUERY-Dialog-Frame ArtBestPkt.ButikkNr ~
ArtBestPkt.StrKode ArtBestPkt.MinAnt ArtBestPkt.MaksAnt ArtBestPkt.BestAnt ~
ArtBestPkt.TillatBruttPk 
&Scoped-define ENABLED-TABLES-IN-QUERY-Dialog-Frame ArtBestPkt
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-Dialog-Frame ArtBestPkt
&Scoped-define QUERY-STRING-Dialog-Frame FOR EACH ArtBestPkt SHARE-LOCK, ~
      EACH ArtBas OF ArtBestPkt SHARE-LOCK
&Scoped-define OPEN-QUERY-Dialog-Frame OPEN QUERY Dialog-Frame FOR EACH ArtBestPkt SHARE-LOCK, ~
      EACH ArtBas OF ArtBestPkt SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Dialog-Frame ArtBestPkt ArtBas
&Scoped-define FIRST-TABLE-IN-QUERY-Dialog-Frame ArtBestPkt
&Scoped-define SECOND-TABLE-IN-QUERY-Dialog-Frame ArtBas


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS ArtBestPkt.ButikkNr ArtBestPkt.StrKode ~
ArtBestPkt.MinAnt ArtBestPkt.MaksAnt ArtBestPkt.BestAnt ~
ArtBestPkt.TillatBruttPk 
&Scoped-define ENABLED-TABLES ArtBestPkt
&Scoped-define FIRST-ENABLED-TABLE ArtBestPkt
&Scoped-Define ENABLED-OBJECTS btnSokButikk RECT-1 Btn_OK Btn_Cancel ~
Btn_Help btnSokStrKode 
&Scoped-Define DISPLAYED-FIELDS ArtBestPkt.ArtikkelNr ArtBas.Beskr ~
ArtBestPkt.ButikkNr Butiker.ButNamn ArtBestPkt.StrKode StrKonv.Storl ~
ArtBestPkt.MinAnt ArtBestPkt.MaksAnt ArtBestPkt.BestAnt ~
ArtBestPkt.TillatBruttPk 
&Scoped-define DISPLAYED-TABLES ArtBestPkt ArtBas Butiker StrKonv
&Scoped-define FIRST-DISPLAYED-TABLE ArtBestPkt
&Scoped-define SECOND-DISPLAYED-TABLE ArtBas
&Scoped-define THIRD-DISPLAYED-TABLE Butiker
&Scoped-define FOURTH-DISPLAYED-TABLE StrKonv


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnSokButikk  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

DEFINE BUTTON btnSokStrKode  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1.

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
     SIZE 85 BY 8.57.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Dialog-Frame FOR 
      ArtBestPkt, 
      ArtBas SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     btnSokButikk AT ROW 3 COL 33.2 NO-TAB-STOP 
     ArtBestPkt.ArtikkelNr AT ROW 2 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 20.2 BY 1
     ArtBas.Beskr AT ROW 2 COL 42.2 COLON-ALIGNED NO-LABEL FORMAT "x(255)"
          VIEW-AS FILL-IN 
          SIZE 36.8 BY 1
     ArtBestPkt.ButikkNr AT ROW 3 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 9 BY 1
     Butiker.ButNamn AT ROW 3 COL 35.8 COLON-ALIGNED NO-LABEL FORMAT "x(255)"
          VIEW-AS FILL-IN 
          SIZE 43.2 BY 1
     ArtBestPkt.StrKode AT ROW 4 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6.2 BY 1
     StrKonv.Storl AT ROW 4 COL 32.8 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 10.2 BY 1
     ArtBestPkt.MinAnt AT ROW 5 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     ArtBestPkt.MaksAnt AT ROW 6 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     ArtBestPkt.BestAnt AT ROW 7 COL 22 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21 BY 1
     ArtBestPkt.TillatBruttPk AT ROW 8.14 COL 24
          LABEL "Tillat brutt pakke"
          VIEW-AS TOGGLE-BOX
          SIZE 56 BY .81
     Btn_OK AT ROW 9.81 COL 1
     Btn_Cancel AT ROW 9.81 COL 16.6
     Btn_Help AT ROW 9.81 COL 71
     btnSokStrKode AT ROW 4 COL 30.4 NO-TAB-STOP 
     RECT-1 AT ROW 1 COL 1
     SPACE(0.39) SKIP(1.42)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Vedlikehold fargekoder"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Dialog-Frame 
/* ************************* Included-Libraries *********************** */

{incl/devmode.i}
{incl/CustDevMode.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR FILL-IN ArtBestPkt.ArtikkelNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN ArtBas.Beskr IN FRAME Dialog-Frame
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Butiker.ButNamn IN FRAME Dialog-Frame
   NO-ENABLE EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN StrKonv.Storl IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX ArtBestPkt.TillatBruttPk IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK DIALOG-BOX Dialog-Frame
/* Query rebuild information for DIALOG-BOX Dialog-Frame
     _TblList          = "skotex.ArtBestPkt,skotex.ArtBas OF skotex.ArtBestPkt"
     _Options          = "SHARE-LOCK"
     _Query            is NOT OPENED
*/  /* DIALOG-BOX Dialog-Frame */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Dialog-Frame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Vedlikehold fargekoder */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokButikk
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokButikk Dialog-Frame
ON CHOOSE OF btnSokButikk IN FRAME Dialog-Frame /* ... */
OR F10 OF ArtBestPkt.StrKode
DO:
  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "Butik".
  RUN JBoxDLookup.w ("Butiker;Butik;ButNamn","where true",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    ArtBestPkt.ButikkNr:SCREEN-VALUE = cLookupValue.
    cLookupValue = DYNAMIC-FUNCTION("getFieldList","Butiker;ButNamn","WHERE Butik = '" + ButikkNr:SCREEN-VALUE + "'").
    Butiker.butNamn:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnSokStrKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnSokStrKode Dialog-Frame
ON CHOOSE OF btnSokStrKode IN FRAME Dialog-Frame /* ... */
OR F10 OF ArtBestPkt.StrKode
DO:
  IF NOT AVAILABLE artbas THEN
      RETURN.

  DEF VAR cLookupValue AS CHAR NO-UNDO.

  /* Syntaks: Param1: <Tabell>;<Felt>;<Felt>...,<Tabell>;<Felt>;<Felt>...              */
  /*          Param2: <Where sats> m/Join                                              */
  /*          Param3: <Returfelt1>[;<Returfelt2>;......],<Filterfelt1>[;<Filterfelt2>] (Settes i cLookupValue) */
  /* Kalkulerte felt kan også benyttes, label, format o.l..       */
  cLookupValue = "StrKode".
  RUN JBoxDLookup.w ("StrTSTr;StrTypeId;SoStorl,StrKonv;!StrKode;!Storl","where StrTStr.StrTypeId = " + string(ArtBas.StrTypeId) + ",first StrKonv where StrKonv.Storl = StrTStr.SoStorl",INPUT-OUTPUT cLookupValue).

  IF cLookupValue NE "" THEN 
  DO:
    ArtBestPkt.StrKode:SCREEN-VALUE = cLookupValue.
    cLookupValue = DYNAMIC-FUNCTION("getFieldList","StrKonv;Storl","WHERE StrKode = '" + ArtBestPkt.StrKode:SCREEN-VALUE + "'").
    StrKonv.Storl:SCREEN-VALUE = ENTRY(1,cLookupValue,"|").
  END.
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


&Scoped-define SELF-NAME ArtBestPkt.ButikkNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBestPkt.ButikkNr Dialog-Frame
ON RETURN OF ArtBestPkt.ButikkNr IN FRAME Dialog-Frame /* ButikkNr */
OR "TAB" OF ButikkNr
DO:
  IF SELF:MODIFIED THEN
  DO WITH FRAME {&FRAME-NAME}:
      Butiker.ButNamn:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","Butiker",
                                              "WHERE Butiker = " + ArtBestPkt.ButikkNr:SCREEN-VALUE,"ButNamn").  
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ArtBestPkt.StrKode
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ArtBestPkt.StrKode Dialog-Frame
ON RETURN OF ArtBestPkt.StrKode IN FRAME Dialog-Frame /* Num storl */
OR "TAB" OF ArtBestPkt.StrKode
DO:
  IF SELF:MODIFIED THEN
  DO WITH FRAME {&FRAME-NAME}:
      StrKonv.Storl:SCREEN-VALUE = DYNAMIC-FUNCTION("getFieldValues","StrKonv",
                                              "WHERE StrKode = " + ArtBestPkt.StrKode:SCREEN-VALUE,"Storl").  
  END.
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
    FIND StrKonv NO-LOCK WHERE
        StrKonv.StrKode = ArtBestPk.StrKode NO-ERROR.
  end.
      
FIND ArtBas NO-LOCK WHERE
    ArtBas.ArtikkelNr = wArtikkelNr.
FIND butiker NO-LOCK WHERE
    Butiker.Butik = wButikkNr.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  {lng.i} RUN enable_UI.

  run VisPost.
  
  if wModus = "Ny" then
    assign
      {&br-tabell}.{&KeyFelt}:SENSITIVE = true
      ArtBestPkt.TillatBruttPk:SCREEN-VALUE = "yes"
      .
  ELSE 
      assign
      {&br-tabell}.{&KeyFelt}:sensitive = false
      ArtBestPkt.StrKode:SENSITIVE = FALSE
      ArtBestPkt.ButikkNr:SENSITIVE = FALSE
      btnSokbutikk:SENSITIVE = FALSE
      btnSokStrKode:SENSITIVE = FALSE
      .

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
  IF AVAILABLE ArtBas THEN 
    DISPLAY ArtBas.Beskr 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE ArtBestPkt THEN 
    DISPLAY ArtBestPkt.ArtikkelNr ArtBestPkt.ButikkNr ArtBestPkt.StrKode 
          ArtBestPkt.MinAnt ArtBestPkt.MaksAnt ArtBestPkt.BestAnt 
          ArtBestPkt.TillatBruttPk 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE Butiker THEN 
    DISPLAY Butiker.ButNamn 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE StrKonv THEN 
    DISPLAY StrKonv.Storl 
      WITH FRAME Dialog-Frame.
  ENABLE btnSokButikk RECT-1 ArtBestPkt.ButikkNr ArtBestPkt.StrKode 
         ArtBestPkt.MinAnt ArtBestPkt.MaksAnt ArtBestPkt.BestAnt 
         ArtBestPkt.TillatBruttPk Btn_OK Btn_Cancel Btn_Help btnSokStrKode 
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
DEF VAR iOldKode LIKE {&br-tabell}.{&KeyFelt} NO-UNDO.
do with frame Dialog-Frame transaction:
  /* Sjekker nye poster. */
  if wModus = "Ny" then
    do:
      {&SjekkOmPostFinnes}
      {&SjekkOmPostFinnes2}
      {&OptFind}
      create {&br-tabell}.
      ASSIGN
        {&OptKeyAssign}
        {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)
        wRecid   = recid({&br-tabell}).
    end.
  ELSE DO:
    {&SjekkOmPostFinnes2}
    find {&br-tabell} Exclusive-lock where
      recid({&br-tabell}) = wRecid no-error.
  END.
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

