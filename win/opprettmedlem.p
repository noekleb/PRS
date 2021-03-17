&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : opprettmedlem.p
    Purpose     : Oppretter medlem fra kunde.

    Syntax      : run opprettmedlem.p (<KundeNr>,<output wMedlemsNr>)

    Description : Oppretter et medlem automatisk basert på informasjon 
                  fra kundeposten som er kjent.

    Author(s)   : Tom Nøkleby
    Created     : 29/12-00
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/
DEF INPUT  PARAMETER wKundeNr    LIKE Kunde.KundeNr      NO-UNDO.
DEF INPUT  PARAMETER wKortNummer LIKE MedlemsKort.KortNr NO-UNDO.
DEF INPUT  PARAMETER wButikkNr   LIKE Medlem.ButikkNr    NO-UNDO.
DEF OUTPUT PARAMETER wMedlemsNr  LIKE Medlem.MedlemsNr   NO-UNDO.

/* ***************************  Definitions  ************************** */
DEF VAR wLedige            AS INT    NO-UNDO.
DEF VAR wDbId              AS char   NO-UNDO.
DEF VAR wGyldighet         AS INT    NO-UNDO.


DEF BUFFER ledMedlem FOR Medlem.

{runlib.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

{syspara.i 14 1 3 wGyldighet INT}
{syspara.i 1 1 17 wDbId}          /* DatabaseID */
{syspara.i 14 1 4 wLedige INT}    /* Føste eller neste ledige medlemsnummer. */

FIND Kunde NO-LOCK WHERE
    Kunde.KundeNr = wKundeNr NO-ERROR.

RUN OpprettMedlem.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-OpprettMedlem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettMedlem Procedure 
PROCEDURE OpprettMedlem :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE wTekst   AS CHARACTER  NO-UNDO.

  FIND FIRST MedlemsGruppe NO-LOCK NO-ERROR.
  FIND FIRST MedlemsType   NO-LOCK NO-ERROR.
  
  /*RUN GetMedlemsNr IN wLibHandle (OUTPUT wMedlemsNr).*/

  CREATE Medlem.
  ASSIGN
      /*Medlem.MedlemsNr = wMedlemsNr*/
      Medlem.Kjonn     = TRUE
      .

  ASSIGN
    Medlem.Medtype          = IF AVAILABLE MedlemsType 
                                THEN MedlemsType.Medtype
                                ELSE 0
    Medlem.MedGruppe        = IF AVAILABLE MedlemsGruppe
                                THEN MedlemsGruppe.MedGruppe
                                ELSE 0
    Medlem.HovedMedlemFlagg = true
    Medlem.HovedMedlemsNr   = 0.0
    Medlem.RegKode          = ""
    Medlem.ButikkNr         = wButikkNr
    wMedlemsNr              = Medlem.MedlemsNr
    .
  IF AVAILABLE Kunde THEN
    ASSIGN
      Medlem.ForNavn          = ""
      Medlem.EtterNavn        = Kunde.KontNavn
      Medlem.Adresse1         = Kunde.Adresse1
      Medlem.Adresse2         = Kunde.Adresse2
      Medlem.PostNr           = Kunde.PostNr
      Medlem.Telefon          = Kunde.KontTelefon
      Medlem.Telefaks         = Kunde.KontTelefaks
      
      Medlem.MobilTlf         = Kunde.KontMobilTlf
      Medlem.Land             = Kunde.Land
      Medlem.ButikkNr         = Kunde.ButikkNr
      Medlem.BydelsNr         = Kunde.BydelsNr
      Medlem.ePostAdresse     = Kunde.KontE-Post
      Medlem.KundeNr          = Kunde.KundeNr
    .
  
  /* EAN kode = PPBBBBNNNNNNS. Viktig av butikknummer er 4 siffer. Det samsvarer med */
  /* håndtering av S-Lagsnummer i kassen.                                            */
  /* MEN BUTIKKNUMMER SKAL STRINGES UTEN FORMATERING.                                */
  /* Ved innlesning fra kasse, gjøres det left-trim for ledende 0'er.                */
  IF wKortNummer = "" THEN DO:
      FIND LAST Medlemskort WHERE Medlemskort.Kortnr > (STRING(Medlem.ButikkNr) + "000000") AND 
                                  Medlemskort.Kortnr < (STRING(Medlem.ButikkNr) + "999999") AND 
                                  LENGTH(Medlemskort.Kortnr) = LENGTH(STRING(Medlem.ButikkNr) + "999999") NO-LOCK NO-ERROR.
      ASSIGN wTekst = IF AVAIL Medlemskort THEN
          STRING(DECI(Medlemskort.Kortnr) + 1) ELSE 
              STRING(Medlem.ButikkNr) + "000001".
      REPEAT:
          RUN sjekkomkorterunikt.p (INPUT wTekst).
          IF RETURN-VALUE = "" THEN
          DO:
            CREATE MedlemsKort.
            ASSIGN MedlemsKort.MedlemsNr = Medlem.MedlemsNr
                   MedlemsKort.KortNr    = wTekst
                   MedlemsKort.AktivertDato = TODAY
                   MedlemsKort.UtgarDato    = TODAY + (IF wGyldighet = 0
                                                         THEN 30
                                                         ELSE 365).
              LEAVE.
          END.
          ELSE
              ASSIGN wTekst = STRING(DECI(wTekst) + 1).
      END.
  END.

  ELSE
  KORTNUMMER:
  DO:
      FIND FIRST MedlemsKort WHERE
           MEdlemsKort.KortNr    = wKortNummer no-error.
      IF NOT AVAILABLE MedlemsKort THEN
        CREATE MedlemsKort.
      ASSIGN
          MedlemsKort.MedlemsNr = Medlem.MedlemsNr
          MedlemsKort.KortNr    = wKortNummer
          MedlemsKort.AktivertDato = TODAY
          MedlemsKort.UtgarDato    = TODAY + (IF wGyldighet = 0
                                                THEN 30
                                                ELSE 365)
          MedlemsKort.Innehaver    = Medlem.Fornavn + " " +
                                     Medlem.EtterNavn
          .
  END. /* KORTNUMMER */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

