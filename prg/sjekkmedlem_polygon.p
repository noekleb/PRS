&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :
    
    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER cKortNr        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iButikkNr      AS INT  NO-UNDO.
DEF INPUT  PARAMETER iKasseNr       AS INT  NO-UNDO.
DEF INPUT  PARAMETER dDato          AS DATE NO-UNDO. 
DEF OUTPUT PARAMETER iKortType      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER lMedlemsNr     AS DEC  NO-UNDO.
DEF OUTPUT PARAMETER cMedlemNavn    AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER lKundeNr       AS DEC  NO-UNDO.
DEF OUTPUT PARAMETER cKundeNavn     AS CHAR NO-UNDO.

DEF VAR iOpprettMedlem AS INT  NO-UNDO.
DEF VAR cTekst         AS CHAR NO-UNDO.

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

  /* Opprette medlemmer automatisk for ukjente medlemskort. */
  {syspara.i 3 3 10 cTekst}
  IF CAN-DO("Ja,Yes,True,1",cTekst) THEN
    iOpprettMedlem = 1.
  ELSE
    iOpprettMedlem = 0.

  /* Kontrollerer Kortnummer. Meldem og Kundekort. */
  if cKortNr = "" then
      RETURN.

  /* Tester på medlemskort med og uten ledende nuller. */
  FIND FIRST Medlemskort NO-LOCK where
    MedlemsKort.KortNr = cKortNr NO-ERROR.
  if NOT AVAILABLE MedlemsKort then
    FIND FIRST MedlemsKort NO-LOCK where
      MedlemsKort.KortNr = STRING(INT(cKortNr),"999999") NO-ERROR.

  /* Vi har funnet et medlemskort, altså er dette et medlem. */
  if AVAILABLE MedlemsKort then
  MEDLEMSKORT:
  DO:
    FIND Medlem OF MedlemsKort NO-LOCK NO-ERROR.
    if AVAILABLE Medlem then
    DO:
      assign
        lMedlemsNr = Medlem.MedlemsNr
        iKortType  = 3
        .
      /* Er medlemmet koblet til en kunde, skal også kundenummer settes i transaksjonen. */
      if Medlem.KundeNr <> 0 then
      DO:
        FIND Kunde NO-LOCK WHERE
            Kunde.KundeNr = Medlem.KundeNr NO-ERROR.
        assign
          lKundeNr   = Medlem.KundeNr
          cKundeNavn = IF AVAILABLE Kunde
                         THEN Kunde.Navn
                         ELSE "*Ukjent*"
          .
      END.
    END.
  END. /* MEDLEMSKORT */

  /* Var det ikke medlemskort, sjekkes det om det var et kundekort. */
  ELSE
  KUNDEKORT:
  DO:
    FIND FIRST KundeKort NO-LOCK where
      KundeKort.KortNr = cKortNr NO-ERROR.
    if NOT AVAILABLE KundeKort then
      FIND FIRST KundeKort NO-LOCK where
        KundeKort.KortNr = STRING(INT(cKortNr),"999999") NO-ERROR.
    if AVAILABLE KundeKort then
    DO:
      FIND Kunde OF KundeKort NO-LOCK NO-ERROR.
      if AVAILABLE Kunde then
        DO:
          assign
            iKortType  = 2
            lKundeNr   = Kunde.KundeNr
            cKundeNavn = Kunde.Navn
            .
        END.
    END.
  END. /* KUNDEKORT */

  /* Medlem opprettes automatisk hvis det ikke finnes og kortet ikke er */
  /* et kundekort.                                                      */
  IF (iOpprettMedlem = 1 AND         /* Flagger automatisk opprettelse. */
      NOT AVAILABLE Medlem) THEN /* Medlem finnes ikke fra før      */
  OPPRETTMEDLEM:
  DO:
    /* Er det et gyldig kundenummer, skal medlem ikke opprettes automatisk. */
    IF lMedlemsNr = 0 AND lKundeNr <> 0 THEN 
        LEAVE OPPRETTMEDLEM.

    RUN opprettmedlem.p (?, INPUT cKortNr, INPUT iButikkNr , OUTPUT lMedlemsNr).
    FIND Medlem EXCLUSIVE-LOCK WHERE
        Medlem.MedlemsNr = lMedlemsNr NO-ERROR.
    IF AVAILABLE Medlem THEN
      assign
        Medlem.EtterNavn    = "But/Kasse: " + 
                              STRING(iButikkNr) + "/" + 
                              STRING(iKasseNr) + " " + 
                              STRING(dDato)
        lMedlemsNr = Medlem.MedlemsNr
        iKortType  = 3
        .
    RELEASE Medlem.
  END. /* OPPRETTMEDLEM */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


