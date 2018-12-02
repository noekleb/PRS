&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          skotex           PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
&Scoped-define FRAME-NAME Dialog-Frame

/* Temp-Table and Buffer definitions                                    */
DEFINE NEW SHARED TEMP-TABLE tKundeTrans NO-UNDO LIKE KundeTrans
       field Referanse as char format "x(30)"
       field Saldo     as dec  format "->>>,>>>,>>9.99"
       field Type      as int.


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
  DEFINE VAR wRecid   as recid NO-UNDO.
  DEFINE VAR wKundeNr LIKE Kunde.KundeNr NO-UNDO.
  define var wModus   as char  no-undo.
  
&ELSE
  DEFINE INPUT-output PARAMETER wRecid   as recid NO-UNDO.
  DEFINE INPUT        PARAMETER wKundeNr LIKE Kunde.KundeNr NO-UNDO.
  define input        parameter wModus   as char  no-undo.
&ENDIF

/* Preprossessor direktiver ---                                         */
&scoped-define br-tabell KundeBetTrans
&scoped-define KeyFelt Butik
&scoped-define OptKeyAssign ~
  KundeBetTrans.KundeNr = wKundeNr

&scoped-define DataType DEC /* INT STRING DEC Datatype på nøkkelfelt*/

/* Henter eventuelle relaterte poster - før mainblokk */
&scoped-define FinnRelatertePoster ~
  FIND Medlem  NO-LOCK WHERE Medlem.MedlemsNr = KundeBetTrans.MedlemsNr NO-ERROR. ~
  FIND Kunde   NO-LOCK WHERE Kunde.KundeNr    = KundeBetTrans.KundeNr   NO-ERROR. ~
  FIND Forsalj NO-LOCK WHERE Forsalj.ForsNr   = int(KundeBetTrans.ForsNr)    NO-ERROR.

/* Ekstra informasjon i find/where når det er flere ledd i indeks */
/* Brukes før KeyFelt assignes ved lagring.                       */
&scoped-define OptFind 

/* Felter som skal vises frem når rutinen VisPost kjøres */
&scoped-define VisPoster ~
  KundeBetTrans.TTId       when available KundeBetTrans ~
  KundeBetTrans.TBId       when available KundeBetTrans ~
  KundeBetTrans.Butik      when available KundeBetTrans ~
  KundeBetTrans.Dato       WHEN AVAILABLE KundeBetTrans ~
  KundeBetTrans.Belop      WHEN AVAILABLE KundeBetTrans ~
  KundeBetTrans.ForsNr     WHEN AVAILABLE KundeBetTrans ~
  Forsalj.FoNamn           WHEN AVAILABLE Forsalj ~
  KundeBetTrans.KundeNr    WHEN AVAILABLE KundeBetTrans ~
  Kunde.Navn               WHEN AVAILABLE Kunde @ FI-Kunde ~
  KundeBetTrans.KortNr     WHEN AVAILABLE KundeBetTrans ~
  KundeBetTrans.KontoNr    WHEN AVAILABLE KundeBetTrans ~
  KundeBetTrans.MedlemsNr  WHEN AVAILABLE KundeBetTrans ~
  Medlem.ForNavn + ' ' + Medlem.Etternavn ~
                           WHEN AVAILABLE Medlem @ FI-Medlem ~
  KundeBetTrans.betButik   WHEN AVAILABLE KundeBetTrans ~
  KundeBetTrans.betKassaNr WHEN AVAILABLE KundeBetTrans ~
  KundeBetTrans.betBongId  WHEN AVAILABLE KundeBetTrans ~
  KundeBetTrans.Notat      WHEN AVAILABLE KundeBetTrans ~
  KundeBetTrans.RefNr      WHEN AVAILABLE KundeBetTrans ~
  KundeBetTrans.RefTekst   WHEN AVAILABLE KundeBetTrans


/* Alternative poster som skal vises når VisPost kjøres */
/* Ligger under display av data. Dvs selvstendig linje. */
&scoped-define VisAndreData

/*
/* Ved sletting - Sjekker om posten kan slettes */
&scoped-define SjekkOmPostFinnes if can-find({&br-tabell} where ~
         {&br-tabell}.{&KeyFelt} = {&DataType}({&br-tabell}.{&KeyFelt}:screen-value)) or ~
         int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
        do: ~
          if int({&br-tabell}.{&KeyFelt}:screen-value) = 0 then ~
            message "{&KeyFelt} må være større enn 0" ~
            view-as alert-box title "Lagringsfeil". ~
          else ~
            message "{&br-tabell} finnes allerede med fargnr:" ~
            {&br-tabell}.{&KeyFelt}:screen-value ~
            view-as alert-box title "Lagringsfeil". ~
           APPLY "ENTRY" TO {&br-tabell}.{&KeyFelt}. ~
          return "AVBRYT". ~
        end.
*/

/* Felter som assign'es når rutinen LagrePost kjøres */        
&scoped-define AssignFelter ~
  KundeBetTrans.TTId       ~
  KundeBetTrans.TBId       ~
  KundeBetTrans.Butik      ~
  KundeBetTrans.Dato       ~
  KundeBetTrans.Belop      ~
  KundeBetTrans.ForsNr     ~
  KundeBetTrans.KundeNr    ~
  KundeBetTrans.KortNr     ~
  KundeBetTrans.MedlemsNr  ~
  KundeBetTRans.KontoNr    ~
  KundeBetTrans.betButik   ~
  KundeBetTrans.betKassaNr ~
  KundeBetTrans.betBongId  ~
  KundeBetTrans.Notat      ~
  KundeBetTrans.RefNr      ~
  KundeBetTrans.RefTekst

/* Tilleggs felter som assign'es når rutinen Lagre post kjøres. */
/* Står etter forrige assign. Dvs egen linje.                   */
&scoped-define TillegsAssign

/* Local Variable Definitions ---                                       */
def var wRetur-Verdi as char initial "AVBRYT" no-undo.
DEF BUFFER b{&br-tabell} FOR {&br-tabell}.
{runlib.i}

def var wTittel as char  no-undo.
DEF VAR rRecid  AS recid NO-UNDO.
DEF VAR rRowId  AS ROWID NO-UNDO.

DEF BUFFER bufKunde FOR Kunde.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE DIALOG-BOX
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Dialog-Frame

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS KundeBetTrans.Butik KundeBetTrans.ForsNr ~
KundeBetTrans.KundeNr KundeBetTrans.MedlemsNr KundeBetTrans.RefNr ~
KundeBetTrans.RefTekst KundeBetTrans.Dato KundeBetTrans.Belop ~
KundeBetTrans.KontoNr KundeBetTrans.betButik KundeBetTrans.betKassaNr ~
KundeBetTrans.betBongId KundeBetTrans.Notat 
&Scoped-define ENABLED-TABLES KundeBetTrans
&Scoped-define FIRST-ENABLED-TABLE KundeBetTrans
&Scoped-Define ENABLED-OBJECTS B-SokBelop BUTTON-SokDato BUTTON-SokKonto ~
BUTTON-SokForsalj BUTTON-SokKunde BUTTON-SokKundeKort BUTTON-SokMedlem ~
BUTTON-SokMedlemsKort Btn_OK Btn_Cancel Btn_Help Btn_OK-NY RECT-56 RECT-57 ~
RECT-58 RECT-59 RECT-60 RECT-61 
&Scoped-Define DISPLAYED-FIELDS KundeBetTrans.TTId KundeBetTrans.TBId ~
KundeBetTrans.Butik KundeBetTrans.ForsNr KundeBetTrans.KundeNr ~
KundeBetTrans.KortNr KundeBetTrans.MedlemsNr KundeBetTrans.RefNr ~
KundeBetTrans.RefTekst KundeBetTrans.Dato KundeBetTrans.Belop ~
Forsalj.FoNamn KundeBetTrans.KortType KundeBetTrans.KontoNr ~
KundeBetTrans.betButik KundeBetTrans.betKassaNr KundeBetTrans.betBongId ~
KundeBetTrans.Notat 
&Scoped-define DISPLAYED-TABLES KundeBetTrans Forsalj
&Scoped-define FIRST-DISPLAYED-TABLE KundeBetTrans
&Scoped-define SECOND-DISPLAYED-TABLE Forsalj
&Scoped-Define DISPLAYED-OBJECTS FI-Medlem FI-Kunde FI-Translabel ~
FI-Kundelabel FI-DatoLabel FI-NotatLabel FI-KontoLabel FI-Kvitteringslabel 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define a dialog box                                                  */

/* Definitions of the field level widgets                               */
DEFINE BUTTON B-SokBelop  NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Viser liste over utestående kvitteringer og faktura.".

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

DEFINE BUTTON Btn_OK-NY AUTO-GO 
     LABEL "OK/NY" 
     SIZE 15 BY 1.14
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-SokDato 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Velg dato fra kalender.".

DEFINE BUTTON BUTTON-SokForsalj 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk på selger.".

DEFINE BUTTON BUTTON-SokKonto 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Viser liste over kontotabell.".

DEFINE BUTTON BUTTON-SokKunde 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk på kunde.".

DEFINE BUTTON BUTTON-SokKundeKort 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk på kundekort".

DEFINE BUTTON BUTTON-SokMedlem 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk på medlem.".

DEFINE BUTTON BUTTON-SokMedlemsKort 
     IMAGE-UP FILE "icon\e-sokpr":U NO-FOCUS
     LABEL "..." 
     SIZE 4.4 BY 1 TOOLTIP "Søk på medlemskort".

DEFINE VARIABLE FI-DatoLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Dato/beløp" 
      VIEW-AS TEXT 
     SIZE 25 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-KontoLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Kontoreferanse" 
      VIEW-AS TEXT 
     SIZE 22 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Kunde AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-Kundelabel AS CHARACTER FORMAT "X(256)":U INITIAL "Kundereferanse" 
      VIEW-AS TEXT 
     SIZE 25 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Kvitteringslabel AS CHARACTER FORMAT "X(256)":U INITIAL "Kvitteringsreferanse" 
      VIEW-AS TEXT 
     SIZE 25 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Medlem AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 31.8 BY 1 NO-UNDO.

DEFINE VARIABLE FI-NotatLabel AS CHARACTER FORMAT "X(256)":U INITIAL "Notat" 
      VIEW-AS TEXT 
     SIZE 15.4 BY .62
     FONT 6 NO-UNDO.

DEFINE VARIABLE FI-Translabel AS CHARACTER FORMAT "X(256)":U INITIAL "Transaksjon" 
      VIEW-AS TEXT 
     SIZE 22 BY .62
     FONT 6 NO-UNDO.

DEFINE RECTANGLE RECT-56
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 37 BY 5.1.

DEFINE RECTANGLE RECT-57
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 6.52.

DEFINE RECTANGLE RECT-58
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 3.57.

DEFINE RECTANGLE RECT-59
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 127 BY 5.48.

DEFINE RECTANGLE RECT-60
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 36.6 BY 2.14.

DEFINE RECTANGLE RECT-61
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 88 BY 2.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Dialog-Frame
     B-SokBelop AT ROW 14.67 COL 44.8
     KundeBetTrans.TTId AT ROW 2 COL 21 COLON-ALIGNED
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 58 BY 1
     BUTTON-SokDato AT ROW 13.62 COL 39.8
     KundeBetTrans.TBId AT ROW 3 COL 21 COLON-ALIGNED
          LABEL "Beskrivelse"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 58 BY 1
     KundeBetTrans.Butik AT ROW 4 COL 21 COLON-ALIGNED
          LABEL "Butikk"
          VIEW-AS COMBO-BOX INNER-LINES 25
          LIST-ITEM-PAIRS "0",1
          DROP-DOWN-LIST
          SIZE 58 BY 1
     KundeBetTrans.ForsNr AT ROW 6.14 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     BUTTON-SokKonto AT ROW 2 COL 119.4
     BUTTON-SokForsalj AT ROW 6.14 COL 47
     KundeBetTrans.KundeNr AT ROW 7.14 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     BUTTON-SokKunde AT ROW 7.14 COL 47
     KundeBetTrans.KortNr AT ROW 8.14 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     BUTTON-SokKundeKort AT ROW 7.14 COL 51.6
     KundeBetTrans.MedlemsNr AT ROW 9.14 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 24 BY 1
     BUTTON-SokMedlem AT ROW 9.14 COL 47
     FI-Medlem AT ROW 9.14 COL 54 COLON-ALIGNED NO-LABEL
     KundeBetTrans.RefNr AT ROW 10.14 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     BUTTON-SokMedlemsKort AT ROW 9.14 COL 51.6
     KundeBetTrans.RefTekst AT ROW 11.14 COL 21 COLON-ALIGNED FORMAT "X(256)"
          VIEW-AS FILL-IN 
          SIZE 42 BY 1
     KundeBetTrans.Dato AT ROW 13.62 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY 1
     KundeBetTrans.Belop AT ROW 14.62 COL 21 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 21.6 BY 1
     Forsalj.FoNamn AT ROW 6.14 COL 49.6 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 36.2 BY 1
     KundeBetTrans.KortType AT ROW 8.14 COL 49.6 COLON-ALIGNED NO-LABEL
          VIEW-AS COMBO-BOX INNER-LINES 20
          LIST-ITEM-PAIRS "Item 1",0
          DROP-DOWN-LIST
          SIZE 36.2 BY 1
     FI-Kunde AT ROW 7.14 COL 54 COLON-ALIGNED NO-LABEL
     KundeBetTrans.KontoNr AT ROW 2 COL 107.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     KundeBetTrans.betButik AT ROW 9 COL 107.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10.4 BY 1
     KundeBetTrans.betKassaNr AT ROW 10 COL 107.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 7.6 BY 1
     KundeBetTrans.betBongId AT ROW 11 COL 107.2 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16 BY 1
     KundeBetTrans.Notat AT ROW 16.95 COL 2.8 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 125 BY 5
     Btn_OK AT ROW 22.43 COL 2
     Btn_Cancel AT ROW 22.43 COL 34
     Btn_Help AT ROW 22.43 COL 114
     Btn_OK-NY AT ROW 22.43 COL 18
     FI-Translabel AT ROW 1.05 COL 2.4 NO-LABEL
     FI-Kundelabel AT ROW 5.29 COL 2 NO-LABEL
     FI-DatoLabel AT ROW 12.57 COL 2 NO-LABEL
     FI-NotatLabel AT ROW 16.05 COL 2.6 NO-LABEL
     FI-KontoLabel AT ROW 1.05 COL 91 COLON-ALIGNED NO-LABEL
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         CANCEL-BUTTON Btn_Cancel.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME Dialog-Frame
     FI-Kvitteringslabel AT ROW 8.14 COL 90 COLON-ALIGNED NO-LABEL
     RECT-56 AT ROW 8.76 COL 92
     RECT-57 AT ROW 5.91 COL 2
     RECT-58 AT ROW 1.71 COL 2
     RECT-59 AT ROW 16.71 COL 2
     RECT-60 AT ROW 1.71 COL 92.4
     RECT-61 AT ROW 13.29 COL 2
     SPACE(39.20) SKIP(7.79)
    WITH VIEW-AS DIALOG-BOX KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D  SCROLLABLE 
         TITLE "Registrering av betalingstransaksjoner"
         CANCEL-BUTTON Btn_Cancel.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: DIALOG-BOX
   Allow: Basic,Browse,DB-Fields,Query
   Other Settings: COMPILE
   Temp-Tables and Buffers:
      TABLE: tKundeTrans T "NEW SHARED" NO-UNDO SkoTex KundeTrans
      ADDITIONAL-FIELDS:
          field Referanse as char format "x(30)"
          field Saldo     as dec  format "->>>,>>>,>>9.99"
          field Type      as int
      END-FIELDS.
   END-TABLES.
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR DIALOG-BOX Dialog-Frame
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME Dialog-Frame:SCROLLABLE       = FALSE.

/* SETTINGS FOR COMBO-BOX KundeBetTrans.Butik IN FRAME Dialog-Frame
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN FI-DatoLabel IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-KontoLabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Kunde IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Kundelabel IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Kvitteringslabel IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-Medlem IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FI-NotatLabel IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN FI-Translabel IN FRAME Dialog-Frame
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN Forsalj.FoNamn IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN KundeBetTrans.KortNr IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX KundeBetTrans.KortType IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN KundeBetTrans.RefTekst IN FRAME Dialog-Frame
   EXP-FORMAT                                                           */
/* SETTINGS FOR COMBO-BOX KundeBetTrans.TBId IN FRAME Dialog-Frame
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR COMBO-BOX KundeBetTrans.TTId IN FRAME Dialog-Frame
   NO-ENABLE                                                            */
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
ON GO OF FRAME Dialog-Frame /* Registrering av betalingstransaksjoner */
DO:
    run LagrePost.
    if return-value = "AVBRYT" then
      return no-apply.
    ELSE
        wRetur-Verdi = "OK".

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Dialog-Frame Dialog-Frame
ON WINDOW-CLOSE OF FRAME Dialog-Frame /* Registrering av betalingstransaksjoner */
DO:
  APPLY "END-ERROR":U TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B-SokBelop
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B-SokBelop Dialog-Frame
ON CHOOSE OF B-SokBelop IN FRAME Dialog-Frame /* ... */
DO:
  RUN d-bkundebetsaldo.w (INPUT-OUTPUT rRecid).
  IF RETURN-VALUE = "AVBRYT" THEN
      RETURN NO-APPLY.

  FIND tKundeTrans WHERE
      RECID(tKundeTrans) = rRecid NO-ERROR.
  IF NOT AVAILABLE tKundeTrans THEN
      RETURN NO-APPLY.

  IF AVAILABLE tKundeTrans THEN
      RUN EtterSok.
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


&Scoped-define SELF-NAME KundeBetTrans.Butik
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KundeBetTrans.Butik Dialog-Frame
ON RETURN OF KundeBetTrans.Butik IN FRAME Dialog-Frame /* Butikk */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokDato
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokDato Dialog-Frame
ON CHOOSE OF BUTTON-SokDato IN FRAME Dialog-Frame /* ... */
or F10 of KundeBetTrans.Dato IN FRAME {&FRAME-NAME}
DO:
do with frame {&FRAME-NAME}:
    wTittel = "Datosøk".

    /* Start søkeprogram */
    {soek.i
      &Felt        = KundeBetTrans.Dato
      &Program     = kalender.w
      &Frame       = {&FRAME-NAME}
      &ExtraParam  = "input wTittel"
    }   
  
end. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokForsalj
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokForsalj Dialog-Frame
ON CHOOSE OF BUTTON-SokForsalj IN FRAME Dialog-Frame /* ... */
or F10 of KundeBetTrans.ForsNr IN FRAME {&FRAME-NAME}
DO:
do with frame {&FRAME-NAME}:
  /* Start søkeprogram */
  {soek.i
    &Felt        = KundeBetTrans.ForsNr
    &Program     = d-bforsalj.w
    &Frame       = {&FRAME-NAME}
    &PostRun     = "FIND Forsalj no-lock where
                    RECID(Forsalj) = INT(RETURN-VALUE) NO-ERROR."
    &OptDisp     = "Forsalj.FoNamn when available Forsalj"
  }   
  
end. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokKonto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokKonto Dialog-Frame
ON CHOOSE OF BUTTON-SokKonto IN FRAME Dialog-Frame /* ... */
or F10 of KundeBetTrans.KontoNr IN FRAME {&FRAME-NAME}
DO:
  do with frame {&FRAME-NAME}:
    FIND KontoTabell NO-LOCK WHERE
        KontoTabell.KontoNr = int(KundeBetTrans.KontoNr:SCREEN-VALUE) NO-ERROR.
    ASSIGN
        rRowId = IF AVAILABLE KontoTabell
                   THEN ROWID(KontoTabell)
                   ELSE ?
        .
    RUN gkontotabell.w (INPUT-OUTPUT rRowId).
    IF RETURN-VALUE = "AVBRYT" THEN
        RETURN NO-APPLY.
    ELSE DO:
        FIND KontoTabell NO-LOCK WHERE
            ROWID(KontoTabell) = rRowId NO-ERROR.
        IF NOT AVAILABLE KontoTabell THEN
            RETURN NO-APPLY.
        ASSIGN
            KundeBetTrans.KontoNr:SCREEN-VALUE = string(KontoTabell.KontoNr).
    END.
  end. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokKunde
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokKunde Dialog-Frame
ON CHOOSE OF BUTTON-SokKunde IN FRAME Dialog-Frame /* ... */
or F10 of KundeBetTrans.KundeNr IN FRAME {&FRAME-NAME}
DO:
do with frame {&FRAME-NAME}:
  /* Start søkeprogram */
  {soek.i
    &Felt        = KundeBetTrans.KundeNr
    &Program     = d-bkunde.w
    &Frame       = {&FRAME-NAME}
    &ParamType   = "INPUT"
    &ExtraParam  = "' '"
    &PostRun     = "FIND Kunde no-lock where
                    RECID(Kunde) = INT(RETURN-VALUE) NO-ERROR."
    &OptDisp     = "Kunde.KundeNr when available Kunde @ KundeBetTrans.KundeNr
                    Kunde.Navn when available Kunde @ FI-Kunde"
  }   
  
end. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokKundeKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokKundeKort Dialog-Frame
ON CHOOSE OF BUTTON-SokKundeKort IN FRAME Dialog-Frame /* ... */
DO:
  IF AVAILABLE bufKunde THEN
    RUN d-bkundekortalle2.w (0,bufKunde.KundeNr,' ').
  ELSE
    RUN d-bkundekortalle.w (0,' ').
  DO WITH FRAME {&FRAME-NAME}:
    FIND KundeKort NO-LOCK WHERE
      RECID(KundeKort) = INT(RETURN-VALUE) NO-ERROR.
    IF AVAILABLE KundeKort THEN
    DO:
      FIND Kunde OF KundeKort NO-LOCK NO-ERROR.
      ASSIGN
          KundeBetTrans.KortNr:SCREEN-VALUE   = KundeKort.KortNr
          KundeBetTrans.KundeNr:SCREEN-VALUE  = string(KundeKort.KundeNr)
          KundeBetTrans.KortType:SCREEN-VALUE = "1"
          FI-Kunde:SCREEN-VALUE               = IF AVAILABLE Kunde 
                                                  THEN Kunde.Navn
                                                  ELSE ""
          .

    END.
    ELSE
        ASSIGN
            KundeBetTrans.KortNr:SCREEN-VALUE   = ""
            KundeBetTrans.KundeNr:SCREEN-VALUE  = ""
            KundeBetTrans.KortType:SCREEN-VALUE = "1"
            FI-Kunde:SCREEN-VALUE               = ""
            .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokMedlem
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokMedlem Dialog-Frame
ON CHOOSE OF BUTTON-SokMedlem IN FRAME Dialog-Frame /* ... */
or F10 of KundeBetTrans.MedlemsNr IN FRAME {&FRAME-NAME}
DO:
do with frame {&FRAME-NAME}:
  IF AVAILABLE bufKunde THEN
      RUN d-bmedlemkunde2.w (INPUT INPUT KundeBetTrans.MedlemsNr,
                            INPUT ' ',
                            INPUT bufKunde.KundeNr).
  ELSE
      RUN d-bmedlemkunde.w (INPUT INPUT KundeBetTrans.MedlemsNr,
                            INPUT ' ').
  FIND Medlem no-lock where
    RECID(Medlem) = INT(RETURN-VALUE) NO-ERROR.

  IF NOT AVAILABLE Medlem THEN
  DO:
      ASSIGN
          KundeBetTrans.MedlemsNr:SCREEN-VALUE = ""
          FI-Medlem:SCREEN-VALUE               = ""
          .
  END.
  ELSE DO:
      IF Medlem.KundeNr = 0 THEN
      DO:
          MESSAGE "Dette medlemmet er ikke koblet til en kunde."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN NO-APPLY.
      END.
      FIND Kunde NO-LOCK WHERE
          Kunde.KundeNr = Medlem.KundeNr NO-ERROR.
      ASSIGN
          KundeBetTrans.MedlemsNr:SCREEN-VALUE = string(Medlem.MedlemsNr)
          FI-Medlem:SCREEN-VALUE               = Medlem.ForNavn + " " + 
                                                 Medlem.EtterNavn
          FI-Kunde:SCREEN-VALUE                = IF AVAILABLE Kunde
                                                   THEN Kunde.Navn
                                                   ELSE ""
          KundeBetTrans.KundeNr:SCREEN-VALUE   = IF AVAILABLE Kunde
                                                   THEN string(Kunde.KundeNr)
                                                   ELSE ""
          KundeBetTrans.KortNr:SCREEN-VALUE    = ""
       .
  END.
end. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-SokMedlemsKort
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-SokMedlemsKort Dialog-Frame
ON CHOOSE OF BUTTON-SokMedlemsKort IN FRAME Dialog-Frame /* ... */
DO:
  IF AVAILABLE bufKunde THEN
    RUN d-bmedlemskortalle2.w (0,bufKunde.KundeNr,' ').
  ELSE
    RUN d-bmedlemskortalle.w (0,' ').
  DO WITH FRAME {&FRAME-NAME}:
    FIND MedlemsKort NO-LOCK WHERE
      RECID(MedlemsKort) = INT(RETURN-VALUE) NO-ERROR.
    
    IF AVAILABLE MedlemsKort THEN
    DO:
      FIND Medlem OF MedlemsKort NO-LOCK NO-ERROR.
      IF AVAILABLE Medlem THEN 
          FIND Kunde NO-LOCK WHERE
            Kunde.KundeNr = Medlem.KundeNr NO-ERROR.
      ASSIGN
          KundeBetTrans.KortNr:SCREEN-VALUE     = MedlemsKort.KortNr
          KundeBetTrans.MedlemsNr:SCREEN-VALUE  = string(MedlemsKort.MedlemsNr)
          KundeBetTrans.KortType:SCREEN-VALUE   = "2"
          FI-Medlem:SCREEN-VALUE                = IF AVAILABLE Medlem 
                                                   THEN Medlem.ForNavn + " " + Medlem.EtterNavn
                                                   ELSE ""
          FI-Kunde:SCREEN-VALUE                 = IF AVAILABLE Kunde
                                                   THEN Kunde.Navn
                                                   ELSE ""
          KundeBetTrans.KundeNr:SCREEN-VALUE    = IF AVAILABLE Kunde
                                                   THEN string(Kunde.KundeNr)
                                                   ELSE ""
          .

    END.
    ELSE
        ASSIGN
            KundeBetTrans.KortNr:SCREEN-VALUE     = ""
            KundeBetTrans.MedlemsNr:SCREEN-VALUE  = ""
            KundeBetTrans.KortType:SCREEN-VALUE   = "1"
            FI-Medlem:SCREEN-VALUE                = ""
            FI-Kunde:SCREEN-VALUE                 = ""
            KundeBetTrans.KundeNr:SCREEN-VALUE    = ""
            .
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KundeBetTrans.ForsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KundeBetTrans.ForsNr Dialog-Frame
ON TAB OF KundeBetTrans.ForsNr IN FRAME Dialog-Frame /* Kasserernummer */
OR "RETURN" OF KundeBetTrans.ForsNr
DO:
    FIND Forsalj NO-LOCK WHERE
        Forsalj.ForsNr = int(INPUT KundeBetTrans.ForsNr) NO-ERROR.
    IF NOT AVAILABLE Forsalj THEN
    DO:
        ASSIGN
            KundeBetTrans.ForsNr:SCREEN-VALUE = ""
            Forsalj.FoNamn:SCREEN-VALUE       = ""
            .
    END.
    ELSE DO:
        ASSIGN
            KundeBetTrans.ForsNr:SCREEN-VALUE = string(Forsalj.ForsNr)
            Forsalj.FoNamn:SCREEN-VALUE       = STRING(Forsalj.FoNamn)
            .
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KundeBetTrans.KortNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KundeBetTrans.KortNr Dialog-Frame
ON TAB OF KundeBetTrans.KortNr IN FRAME Dialog-Frame /* Kortnummer */
OR "RETURN" OF KundeBetTrans.KortNr
DO:
    FIND FIRST KundeKort NO-LOCK WHERE
        KundeKort.KundeNr = int(INPUT KundeBetTrans.KundeNr) and
        KundeKort.KortNr  begins(INPUT KundeBetTrans.KortNr) NO-ERROR.
    IF NOT AVAILABLE KundeKort THEN
    DO:
        ASSIGN
            KundeBetTrans.KortNr:SCREEN-VALUE = ""
            .
    END.
    ELSE DO:
        ASSIGN
            KundeBetTrans.KundeNr:SCREEN-VALUE = string(KundeKort.KundeNr)
            KundeBetTrans.KortNr:SCREEN-VALUE  = KundeKort.KortNr
            .
        FIND Kunde OF KundeKort NO-LOCK NO-ERROR.
        IF AVAILABLE Kunde THEN
          ASSIGN
            FI-Kunde:SCREEN-VALUE              = (Kunde.Navn)
            .
        ELSE
            ASSIGN
                FI-Kunde:SCREEN-VALUE          = ""
                .
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KundeBetTrans.KundeNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KundeBetTrans.KundeNr Dialog-Frame
ON TAB OF KundeBetTrans.KundeNr IN FRAME Dialog-Frame /* Kundenummer */
OR "RETURN" OF KundeBetTrans.KundeNr
DO:
    FIND Kunde NO-LOCK WHERE
        Kunde.KundeNr = int(INPUT KundeBetTrans.KundeNr) NO-ERROR.
    IF NOT AVAILABLE Kunde THEN
    DO:
        ASSIGN
            KundeBetTrans.KundeNr:SCREEN-VALUE = ""
            FI-Kunde:SCREEN-VALUE        = ""
            .
    END.
    ELSE DO:
        ASSIGN
            KundeBetTrans.KundeNr:SCREEN-VALUE   = string(Kunde.KundeNr)
            FI-Kunde:SCREEN-VALUE                = (Kunde.Navn)
            FI-Medlem:SCREEN-VALUE               = ""
            KundeBetTrans.MedlemsNr:SCREEN-VALUE = ""
            .
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KundeBetTrans.MedlemsNr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KundeBetTrans.MedlemsNr Dialog-Frame
ON TAB OF KundeBetTrans.MedlemsNr IN FRAME Dialog-Frame /* Medlemsnummer */
OR "RETURN" OF KundeBetTrans.MedlemsNr
DO:
    IF AVAILABLE bufKunde THEN
        FIND Medlem NO-LOCK WHERE
            Medlem.MedlemsNr = int(INPUT KundeBetTrans.MedlemsNr) and
            Medlem.KundeNr  = bufKunde.KundeNr NO-ERROR.
    ELSE
      FIND Medlem NO-LOCK WHERE
        Medlem.MedlemsNr = int(INPUT KundeBetTrans.MedlemsNr) NO-ERROR.
    IF NOT AVAILABLE Medlem THEN
    DO:
        ASSIGN
            KundeBetTrans.MedlemsNr:SCREEN-VALUE = ""
            FI-Medlem:SCREEN-VALUE               = ""
            .
    END.
    ELSE DO:
        IF Medlem.KundeNr = 0 THEN
        DO:
            MESSAGE "Dette medlemmet er ikke koblet til en kunde."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
        FIND Kunde NO-LOCK WHERE
            Kunde.KundeNr = Medlem.KundeNr NO-ERROR.
        ASSIGN
            KundeBetTrans.MedlemsNr:SCREEN-VALUE = string(Medlem.MedlemsNr)
            FI-Medlem:SCREEN-VALUE               = Medlem.ForNavn + " " + 
                                                   Medlem.EtterNavn
            FI-Kunde:SCREEN-VALUE                = IF AVAILABLE Kunde
                                                     THEN Kunde.Navn
                                                     ELSE ""
            KundeBetTrans.KundeNr:SCREEN-VALUE   = IF AVAILABLE Kunde
                                                     THEN string(Kunde.KundeNr)
                                                     ELSE ""
            KundeBetTrans.KortNr:SCREEN-VALUE    = ""
         .
    END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KundeBetTrans.TBId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KundeBetTrans.TBId Dialog-Frame
ON RETURN OF KundeBetTrans.TBId IN FRAME Dialog-Frame /* Beskrivelse */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KundeBetTrans.TBId Dialog-Frame
ON VALUE-CHANGED OF KundeBetTrans.TBId IN FRAME Dialog-Frame /* Beskrivelse */
DO:
  RUN EnableFields (INPUT INPUT KundeBetTrans.TTId, INPUT INPUT KundeBetTrans.TBId).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME KundeBetTrans.TTId
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KundeBetTrans.TTId Dialog-Frame
ON RETURN OF KundeBetTrans.TTId IN FRAME Dialog-Frame /* TransTypeId */
DO:
  APPLY "TAB" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL KundeBetTrans.TTId Dialog-Frame
ON VALUE-CHANGED OF KundeBetTrans.TTId IN FRAME Dialog-Frame /* TransTypeId */
DO:
  RUN InitTBId (INPUT INPUT KundeBetTrans.TTId).
  DO WITH FRAME Dialog-Frame:
    IF INPUT KundeBetTRans.TTId <> 61 THEN
      ASSIGN
        KundeBetTrans.TBId:SCREEN-VALUE = IF AVAILABLE KundeBetTrans
                                           THEN string(KundeBetTrans.TBId)
                                           ELSE ENTRY(2,KundeBetTrans.TBId:LIST-ITEM-PAIRS)
        .
    ELSE DO:
        IF AVAILABLE KundeBetTrans THEN
        DO:
          IF KundeBetTrans.TBId = 1 THEN
              KundeBetTrans.TBId:SCREEN-VALUE = ENTRY(2,KundeBetTrans.TBId:LIST-ITEM-PAIRS).
          ELSE
            KundeBetTrans.TBId:SCREEN-VALUE = string(KundeBetTrans.TBId).
        END.
        ELSE
          KundeBetTrans.TBId:SCREEN-VALUE = ENTRY(2,KundeBetTrans.TBId:LIST-ITEM-PAIRS).
    END.
    RUN EnableFields (INPUT INPUT KundeBetTrans.TTId, INPUT INPUT KundeBetTrans.TBId).
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
    /* Er posten MotPostert, kan den ikke endres. */
    IF {&br-tabell}.MotPostert THEN
    DO:
        MESSAGE "Posten er motposter og kan ikke endres."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN "AVBRYT".
    END.
    {&FinnRelatertePoster}  
  end.
      
FIND bufKunde NO-LOCK WHERE
    bufKunde.KundeNr = wKundeNr NO-ERROR.
IF wKundeNr <> ? AND wKundeNr <> 0 AND NOT AVAILABLE bufKunde THEN
DO:
    MESSAGE "Ukjent kundenummer " wKundeNr
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
END.

{sww.i}
RUN ByggSaldoListe.
{swn.i}

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:

  RUN InitTTId.
  /*
  RUN InitTBId (IF AVAILABLE KundeBetTrans
                  THEN KundeBetTrans.TTId
                  ELSE 61).
  */
  RUN InitTBId (61).
  RUN InitButikk.
  RUN InitKortType.

  {lng.i} 
  RUN enable_UI.

  run VisPost.
  /* Default transtype */
  ASSIGN
      KundeBetTrans.TTId:SCREEN-VALUE    = "61"
      .

  APPLY "value-changed" TO KundeBetTrans.TTId.
  
  if wModus = "Ny" then
  DO:
    assign
      {&br-tabell}.{&KeyFelt}:SENSITIVE = true
      KundeBetTrans.KundeNr:SCREEN-VALUE = STRING(bufKunde.KundeNr)
      .
    IF AVAILABLE bufKunde THEN
      ASSIGN
        FI-Kunde:SCREEN-VALUE = bufKunde.Navn
        KundeBetTrans.KundeNr:SENSITIVE = FALSE
        BUTTON-SokKunde:SENSITIVE = FALSE
        .
    RUN EnableFields (50, 1).
  END.
  else
    assign
      {&br-tabell}.{&KeyFelt}:sensitive = TRUE /*false*/
      Btn_OK-NY:HIDDEN = TRUE
      .

  APPLY "ENTRY" TO KundeBetTrans.TTId.

  WAIT-FOR GO OF FRAME {&FRAME-NAME}.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggSaldoListe Dialog-Frame 
PROCEDURE ByggSaldoListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR plSum   AS DEC  FORMAT "->>>,>>>,>>9.99" NO-UNDO.
  DEF VAR piSeqNr AS INT                           NO-UNDO.
  
  IF NOT AVAILABLE bufKunde THEN
      RETURN "AVBRYT".

  /* Tømmer tabellen før den bygges igjen. */
  FOR EACH tKundeTrans:
      DELETE tKundeTrans.
  END.

  /* Bygger saldoListe */
  BYGGLISTE-KVITTERING:
  FOR EACH KundeTrans NO-LOCK WHERE
      KundeTrans.KundeNr = bufKunde.KundeNr AND
      KundeTrans.MotPostert = false
      BREAK BY KundeTrans.KundeNr
            BY KundeTrans.MotPostert
            BY KundeTrans.Butik
            BY KundeTrans.KassaNr
            BY KundeTrans.BongId:
      /* Nullstiller */
      IF FIRST-OF(KundeTrans.BongId) THEN
          ASSIGN
            plSum = 0
            .
      
      /* Akkumulerer */
      ASSIGN
          plSum = plSum + KundeTrans.Pris - KundeTrans.RabKr /* Pris er inkl. mva og ekskl. rabatt */
          .

      /* Posterer sum på kvitteringen. */
      IF LAST-OF(KundeTrans.BongId) THEN
      DO:
          FIND tKundeTrans WHERE
               tKundeTrans.KundeNr = KundeTrans.KundeNr AND
               tKundeTrans.Butik   = KundeTrans.Butik   AND
               tKundeTrans.KassaNr = KundeTrans.KassaNr AND
               tKundeTrans.BongId  = KundeTrans.BongId NO-ERROR.
          IF NOT AVAILABLE tKundeTrans THEN
          DO:
              CREATE tKundeTrans.
              BUFFER-COPY KundeTrans TO tKundeTrans.
          END.
          ASSIGN
              tKundeTrans.Saldo     = plSum
              tKundeTrans.Referanse = "Kvittering: " + 
                                      string(KundeTrans.Butik) + "/" +
                                      STRING(KundeTrans.KassaNr) + "/" +
                                      STRING(KundeTrans.BongId)
              tKundeTrans.TYPE      = 1
              .
      END.
  END. /* BYGGLISTE-KVITTERING */

  /* Trekker fra innbetalte beløp på kvittering */
  FOR EACH tKundeTrans WHERE
      tKundeTrans.TYPE = 1:

      /* Beregner utestående netto på kvittering. */
      NETTOBEREGNING:
      FOR EACH KundeBetTrans NO-LOCK WHERE
          KundeBetTrans.KundeNr    = tKundeTrans.KundeNr AND
          KundeBetTrans.betButik   = tKundeTrans.Butik   AND
          KundeBetTrans.betKassaNr = tKundeTrans.KassaNr AND
          KundeBetTrans.betBongId  = tKundeTrans.BongId:

          /* Noen transaksjoner skal ikke telle som betaling. */
          IF CAN-DO("55,60,62,64,65,61",STRING(KundeBetTrans.TTId)) THEN
          SKIPTRANS:
          DO:
              /* Innbetaling på kvittering skal tas med */
              IF KundeBetTrans.TTId = 61 AND
                 KundeBetTrans.TBId = 5 THEN
                LEAVE SKIPTRANS.

              NEXT NETTOBEREGNING.
          END. /* SKIPTRANS */

          ASSIGN
              tKundeTrans.Saldo = tKundeTrans.Saldo - KundeBetTrans.Belop
              .

      END. /* NETTOBEREGNING */
  END.

  /* Trekker fra innbetalt beløp på faktura */
  FOR EACH tKundeTrans WHERE
      tKundeTrans.TYPE = 2:

      /* Beregner utestående netto påfaktura. */
      NETTOBEREGNING:
      FOR EACH KundeBetTrans NO-LOCK WHERE
          KundeBetTrans.KundeNr   = tKundeTrans.KundeNr AND
          KundeBetTrans.FakturaNr = tKundeTrans.FakturaNr:

          ASSIGN
              tKundeTrans.Saldo = tKundeTrans.Saldo - KundeBetTrans.Belop
              .
      END. /* NETTOBEREGNING */
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EnableFields Dialog-Frame 
PROCEDURE EnableFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piTTId AS INT NO-UNDO.
  DEF INPUT PARAMETER piTBId AS INT NO-UNDO.

  DO WITH FRAME {&FRAME-NAME}:
    /* Innbetaling på konto */
    IF CAN-DO("6104",string(piTTId,"99") + string(piTBId,"99")) THEN
        ASSIGN
            KundeBetTrans.KontoNr:SENSITIVE    = TRUE
            KundeBetTrans.BetButik:SENSITIVE   = FALSE
            KundeBetTrans.BetKassaNr:SENSITIVE = FALSE
            KundeBetTrans.BetBongId:SENSITIVE  = FALSE
            KundeBetTrans.BetButik:SCREEN-VALUE   = ""
            KundeBetTrans.BetKassaNr:SCREEN-VALUE = ""
            KundeBetTrans.BetBongId:SCREEN-VALUE  = ""
            .
    /* innbetaling på faktura */
    ELSE IF CAN-DO("6103",string(piTTId,"99") + string(piTBId,"99")) THEN
        ASSIGN
            KundeBetTrans.KontoNr:SENSITIVE    = FALSE 
            KundeBetTrans.BetButik:SENSITIVE   = FALSE
            KundeBetTrans.BetKassaNr:SENSITIVE = FALSE
            KundeBetTrans.BetBongId:SENSITIVE  = FALSE
            KundeBetTrans.KontoNr:SCREEN-VALUE    = "" 
            KundeBetTrans.BetButik:SCREEN-VALUE   = ""
            KundeBetTrans.BetKassaNr:SCREEN-VALUE = ""
            KundeBetTrans.BetBongId:SCREEN-VALUE  = ""
            .
    /* Innbetaling áKonto */
    ELSE IF CAN-DO("6102",string(piTTId,"99") + string(piTBId,"99")) THEN
        ASSIGN
            KundeBetTrans.KontoNr:SENSITIVE    = FALSE 
            KundeBetTrans.BetButik:SENSITIVE   = FALSE
            KundeBetTrans.BetKassaNr:SENSITIVE = FALSE
            KundeBetTrans.BetBongId:SENSITIVE  = FALSE
            KundeBetTrans.KontoNr:SCREEN-VALUE    = "" 
            KundeBetTrans.BetButik:SCREEN-VALUE   = ""
            KundeBetTrans.BetKassaNr:SCREEN-VALUE = ""
            KundeBetTrans.BetBongId:SCREEN-VALUE  = ""
            .
    /* innbetaling på kvittering */
    ELSE IF CAN-DO("6105",string(piTTId,"99") + string(piTBId,"99")) THEN
        ASSIGN
            KundeBetTrans.KontoNr:SENSITIVE    = FALSE 
            KundeBetTrans.BetButik:SENSITIVE   = TRUE
            KundeBetTrans.BetKassaNr:SENSITIVE = TRUE
            KundeBetTrans.BetBongId:SENSITIVE  = TRUE
            KundeBetTrans.KontoNr:SCREEN-VALUE   = "" 
            .
    /* Allt annet. */
    ELSE 
        ASSIGN
            KundeBetTrans.KontoNr:SENSITIVE    = FALSE 
            KundeBetTrans.BetButik:SENSITIVE   = TRUE
            KundeBetTrans.BetKassaNr:SENSITIVE = TRUE
            KundeBetTrans.BetBongId:SENSITIVE  = TRUE
            KundeBetTrans.KontoNr:SCREEN-VALUE   = "" 
            .
  END.

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
  DISPLAY FI-Medlem FI-Kunde FI-Translabel FI-Kundelabel FI-DatoLabel 
          FI-NotatLabel FI-KontoLabel FI-Kvitteringslabel 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE Forsalj THEN 
    DISPLAY Forsalj.FoNamn 
      WITH FRAME Dialog-Frame.
  IF AVAILABLE KundeBetTrans THEN 
    DISPLAY KundeBetTrans.TTId KundeBetTrans.TBId KundeBetTrans.Butik 
          KundeBetTrans.ForsNr KundeBetTrans.KundeNr KundeBetTrans.KortNr 
          KundeBetTrans.MedlemsNr KundeBetTrans.RefNr KundeBetTrans.RefTekst 
          KundeBetTrans.Dato KundeBetTrans.Belop KundeBetTrans.KortType 
          KundeBetTrans.KontoNr KundeBetTrans.betButik KundeBetTrans.betKassaNr 
          KundeBetTrans.betBongId KundeBetTrans.Notat 
      WITH FRAME Dialog-Frame.
  ENABLE B-SokBelop BUTTON-SokDato KundeBetTrans.Butik KundeBetTrans.ForsNr 
         BUTTON-SokKonto BUTTON-SokForsalj KundeBetTrans.KundeNr 
         BUTTON-SokKunde BUTTON-SokKundeKort KundeBetTrans.MedlemsNr 
         BUTTON-SokMedlem KundeBetTrans.RefNr BUTTON-SokMedlemsKort 
         KundeBetTrans.RefTekst KundeBetTrans.Dato KundeBetTrans.Belop 
         KundeBetTrans.KontoNr KundeBetTrans.betButik KundeBetTrans.betKassaNr 
         KundeBetTrans.betBongId KundeBetTrans.Notat Btn_OK Btn_Cancel Btn_Help 
         Btn_OK-NY RECT-56 RECT-57 RECT-58 RECT-59 RECT-60 RECT-61 
      WITH FRAME Dialog-Frame.
  {&OPEN-BROWSERS-IN-QUERY-Dialog-Frame}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE EtterSok Dialog-Frame 
PROCEDURE EtterSok :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 IF NOT AVAILABLE tKundeTrans THEN
     RETURN.
  DO WITH FRAME {&FRAME-NAME}:
    CASE tKundeTrans.TYPE:
      WHEN 1 THEN
        DO:
          /* Er det referanse til en transaksjon, oppfattes dette som en       */
          /* referanse til en kvittering.                                      */
          ASSIGN
            KundeBetTrans.betButik:SCREEN-VALUE   = string(tKundeTrans.Butik)
            KundeBetTrans.betKassaNr:SCREEN-VALUE = string(tKundeTrans.KassaNr)
            KundeBetTrans.betBongId:SCREEN-VALUE  = string(tKundeTrans.BongId)
            KundeBetTrans.Belop:SCREEN-VALUE      = STRING(tKundeTrans.Saldo)
            KundeBetTrans.Dato:SCREEN-VALUE       = STRING(TODAY)
            .
          IF KundeBetTrans.TTId:SCREEN-VALUE  = "61" THEN
              KundeBetTrans.TBId:SCREEN-VALUE = "5".

        END.
      WHEN 2 THEN
        DO:
        END.
      OTHERWISE.
    END CASE.
  END. /* Frame Scoop */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitButikk Dialog-Frame 
PROCEDURE InitButikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.
        
  DO WITH FRAME {&FRAME-NAME}:
    FOR EACH Butiker NO-LOCK WHERE
      Butiker.Butik >= 0:
   
      ASSIGN
          pcTekst = pcTekst + 
                  (IF pcTekst = ""
                     THEN ""
                     ELSE ",") + 
                   STRING(Butiker.Butik,"zzzzz9") + ": " + Butiker.ButNamn + "," + 
                   STRING(Butiker.Butik)
        .
    END.
    ASSIGN
      KundeBetTrans.Butik:LIST-ITEM-PAIRS = pcTekst
      .
  END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitKortType Dialog-Frame 
PROCEDURE InitKortType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.
        
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      KundeBetTrans.KortType:LIST-ITEM-PAIRS = "KundeKort,1,MedlemsKort,2"
      .
  END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitTBId Dialog-Frame 
PROCEDURE InitTBId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piTTId AS INT NO-UNDO.

  DEF VAR pcTekst AS CHAR NO-UNDO.
        
  INIT:
  DO WITH FRAME {&FRAME-NAME}:

    TRANSLOOP:
    FOR EACH TransBeskr NO-LOCK WHERE
      TransBeskr.TTId = piTTId:
   
      IF TransBeskr.TTId = 61 THEN
      DO:
          IF CAN-DO("1",STRING(TransBeskr.TBId)) THEN
              NEXT TRANSLOOP.
      END.

      ASSIGN
          pcTekst = pcTekst + 
                  (IF pcTekst = ""
                     THEN ""
                     ELSE ",") + 
                   STRING(TransBeskr.TBId,"z9") + ": " + TransBeskr.Beskrivelse + "," + 
                   STRING(TransBeskr.TBId)
        .
    END. /* TRANSLOOP */
    ASSIGN
      KundeBetTrans.TBId:LIST-ITEM-PAIRS = pcTekst
      .
  END. /* INIT */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitTTId Dialog-Frame 
PROCEDURE InitTTId :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.
        
  DO WITH FRAME {&FRAME-NAME}:
    TRANSLOOP:
    FOR EACH TransType NO-LOCK WHERE
      TransType.TTId >= 50:
   
      IF CAN-DO("57,58,59,60,62,63,64,65",STRING(TransType.TTId)) THEN
          NEXT TRANSLOOP.

      ASSIGN
          pcTekst = pcTekst + 
                  (IF pcTekst = ""
                     THEN ""
                     ELSE ",") + 
                   STRING(TransType.TTId) + ": " + TransType.Beskrivelse + "," + 
                   STRING(TransType.TTId)
        .
    END. /* TRANSLOOP */
    ASSIGN
      KundeBetTrans.TTId:LIST-ITEM-PAIRS = pcTekst
      KundeBetTrans.TTId:SCREEN-VALUE    = "61"
      .
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
DEF VAR iOldKode LIKE {&br-tabell}.{&KeyFelt} NO-UNDO.
do with frame Dialog-Frame transaction:
  /* Validering */
  IF INPUT KundeBetTrans.Dato = ? THEN
  DO:
      MESSAGE "Ugyldig transaksjonsdato."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
  END.
  IF INPUT KundeBetTrans.Belop = 0 THEN
  DO:
      MESSAGE "Det er ikke angitt et beløp."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
  END.
  IF INPUT KundeBetTrans.KundeNr = 0 THEN
  DO:
      MESSAGE "Kundenummer må angis."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
  END.
  IF INPUT KundeBetTrans.KundeNr <> 0 THEN
  DO:
      IF NOT CAN-FIND(Kunde WHERE
                      Kunde.KundeNr = INPUT KundeBetTrans.KundeNr) THEN
      DO:
          MESSAGE "Ugyldig kundenummer."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
      END.
  END.
  IF NOT can-find(Forsalj where
                  Forsalj.ForsNr = int(INPUT KundeBetTrans.ForsNr)) THEN
  DO:
      MESSAGE "Ukjent kasserernummer."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN "AVBRYT".
  END.

  IF INPUT KundeBet.KortNr <> "" THEN
  DO:
      IF INPUT KundeBetTrans.KortType = 1 THEN
      DO:
          FIND KundeKort NO-LOCK WHERE
               KundeKort.KortNr = INPUT KundeBetTrans.KortNr NO-ERROR.
          IF NOT AVAILABLE KundeKort THEN
          DO:
              MESSAGE "Ugyldig kundekort."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN "AVBRYT".
          END.
          IF KundeKort.KundeNr <> INPUT KundeBetTrans.KundeNr THEN
          DO:
              MESSAGE "Kortet er koblet til kunde " KundeKort.KundeNr
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN "AVBRYT".
          END.
      END.
      ELSE DO:
          FIND MedlemsKort NO-LOCK WHERE
               MedlemsKort.KortNr = INPUT KundeBetTrans.KortNr NO-ERROR.
          IF NOT AVAILABLE MedlemsKort THEN
          DO:
              MESSAGE "Ugyldig medlemskort."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN "AVBRYT".
          END.
          FIND Medlem OF MedlemsKort NO-LOCK NO-ERROR.
          IF NOT AVAILABLE Medlem THEN
          DO:
              MESSAGE "Kortet tilhører et ukjent medlemsnummer " MedlemsKort.MedlemsNr
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN "AVBRYT".
          END.
          IF Medlem.KundeNr <> INPUT KundeBetTrans.KundeNr THEN
          DO:
              MESSAGE "Medlemmet kortet tilhører er koblet til kunde " KundeKort.KundeNr
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN "AVBRYT".
          END.
      END.
  END.
  IF INPUT KundeBetTrans.MedlemsNr <> 0 THEN
  DO:
      FIND Medlem NO-LOCK WHERE
          Medlem.MedlemsNr = INPUT KundeBetTrans.MedlemsNr NO-ERROR.
      IF NOT AVAILABLE Medlem THEN
      DO:
          MESSAGE "Ugyldig medlemsnummer."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
      END.
      IF Medlem.KundeNr <> INPUT KundeBetTrans.KundeNr THEN
      DO:
          MESSAGE "Medlemmet er koblet til kunde " Medlem.KundeNr
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
      END.
  END.
  /* Innbetalingstransaksjoner */
  IF INPUT KundeBetTrans.TTId = 61 THEN
  DO:
      IF INPUT KundeBetTrans.TBId = 3 THEN /* innbetaling på faktura */
      DO:
/*           IF NOT CAN-FIND(Faktura WHERE                                           */
/*                           Faktura.FakturaNr = INPUT KundeBetTrans.FakturaNr) THEN */
/*           DO:                                                                     */
/*               MESSAGE "Ugyldig fakturanummer."                                    */
/*                   VIEW-AS ALERT-BOX INFO BUTTONS OK.                              */
/*               RETURN "AVBRYT".                                                    */
/*           END.                                                                    */
      END.
      ELSE IF INPUT KundeBetTrans.TBId = 4 THEN /* Innbetaling på konto */
      DO:
          IF NOT CAN-FIND(KontoTabell WHERE
                          KontoTabell.KontoNr = INPUT KundeBetTrans.KontoNr) THEN
          DO:
              MESSAGE "Ugyldig kontonummer."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN "AVBRYT".
          END.
      END.
      ELSE IF INPUT KundeBetTrans.TBId = 2 THEN. /* áKonto - Ingen validering.  */
      ELSE IF INPUT KundeBetTrans.TBId = 5 THEN  /* Innbetaling mot kvittering. */
      DO:
          IF NOT CAN-FIND(FIRST KundeTrans WHERE
                          KundeTrans.KundeNr = INPUT KundeBetTrans.KundeNr AND
                          kundeTrans.Butik   = INPUT KundeBetTrans.betButik AND
                          KundeTrans.KassaNr = INPUT KundeBetTrans.betKassaNr AND
                          KundeTrans.BongId  = INPUT KundeBetTrans.betBongID) THEN
          DO:
              MESSAGE "Det er ikke registrert salg på dette kvitteringsnummer."
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
              RETURN "AVBRYT".
          END.
      END.
  END.
  /* Alle andre transaksjoner skal ha en gyldig kvitteringsreferanse */
  ELSE DO:
      IF NOT CAN-FIND(FIRST KundeTrans WHERE
                      KundeTrans.KundeNr = INPUT KundeBetTrans.KundeNr AND
                      kundeTrans.Butik   = INPUT KundeBetTrans.betButik AND
                      KundeTrans.KassaNr = INPUT KundeBetTrans.betKassaNr AND
                      KundeTrans.BongId  = INPUT KundeBetTrans.betBongID) THEN
      DO:
          MESSAGE "Det er ikke registrert salg på dette kvitteringsnummer."
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
      END.
  END.

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
  ELSE IF wModus = "Bytkode" THEN DO:
      IF INPUT {&br-tabell}.{&KeyFelt} <> {&br-tabell}.{&KeyFelt} THEN DO:
          {&SjekkOmPostFinnes}
          ASSIGN iOldKode = {&br-tabell}.{&KeyFelt}.
          IF CAN-FIND(FIRST ArtBas WHERE ArtBas.Farg = iOldKode) THEN DO:
              MESSAGE "Artikkler med gammel kode: " iOldKode SKIP
                  " byttes til: " INPUT {&br-tabell}.{&KeyFelt} ". Fortsette?"
                  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice AS LOGICAL.
              IF NOT choice THEN
                  RETURN "AVBRYT".
          END.
          find {&br-tabell} Exclusive-lock where
            recid({&br-tabell}) = wRecid no-error.
      END.
      ELSE DO:
          MESSAGE "Ingen endring"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RETURN "AVBRYT".
      END.
  END.
  ELSE DO:
    {&SjekkOmPostFinnes2}
    find {&br-tabell} Exclusive-lock where
      recid({&br-tabell}) = wRecid no-error.
  END.

  assign
    {&AssignFelter}.
  {&TillegsAssign}
  IF wModus = "Bytkode" THEN DO:
      FOR EACH ArtBas WHERE ArtBas.Farg = iOldKode:
          ASSIGN ArtBas.Farg = {&br-tabell}.{&KeyFelt}.
      END.
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

  DO WITH FRAME Dialog-Frame:
    ASSIGN
      KundeBetTrans.TTId:SCREEN-VALUE = IF AVAILABLE KundeBetTrans
                                          THEN string(KundeBetTrans.TTId)
                                          ELSE ENTRY(2,KundeBetTrans.TTId:LIST-ITEM-PAIRS)
      KundeBetTrans.TBId:SCREEN-VALUE = IF AVAILABLE KundeBetTrans
                                         THEN string(KundeBetTrans.TBId)
                                         ELSE ENTRY(2,KundeBetTrans.TBId:LIST-ITEM-PAIRS)
      KundeBetTrans.Butik:SCREEN-VALUE = IF AVAILABLE KundeBetTrans
                                          THEN string(KundeBetTrans.Butik)
                                          ELSE ENTRY(2,KundeBetTrans.Butik:LIST-ITEM-PAIRS)
      KundeBetTrans.KortType:SCREEN-VALUE = IF AVAILABLE KundeBetTrans
                                          THEN string(KundeBetTrans.KortType)
                                          ELSE ENTRY(2,KundeBetTrans.KortType:LIST-ITEM-PAIRS)
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

