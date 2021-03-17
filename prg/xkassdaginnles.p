&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xxkassdaginnles.p
    Purpose     : Innlesning av kassereroppgjør fra 

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR cTekst        AS CHAR NO-UNDO.
DEF VAR bOk           AS LOG  NO-UNDO.
DEF VAR iButikkNr     AS INT  NO-UNDO.
DEF VAR iCL           AS INT  NO-UNDO.
DEF VAR iOppdOverf    AS INT  NO-UNDO. 
DEF VAR iEan          AS INT  NO-UNDO.

DEFINE VARIABLE piLoop                    AS INTEGER NO-UNDO.
DEFINE VARIABLE piButikkNr                AS INTEGER NO-UNDO.
DEFINE VARIABLE piKassererNr              AS INTEGER NO-UNDO.
DEFINE VARIABLE pdDato                    AS DATE FORMAT "99/99/99" NO-UNDO.
DEFINE VARIABLE piZ_Nummer                AS INTEGER NO-UNDO.
DEFINE VARIABLE plOpptaltKontanter        AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltSjekk            AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltReserve          AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltGavekort         AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltBilag            AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltTilgode          AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltLevertBank       AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltGavekortAndre    AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltGavekortUtlevert AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltTilgodeAndre     AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltTilgodeUtlevert  AS DECIMAL NO-UNDO.
DEFINE VARIABLE pcPoseNr                  AS CHARACTER NO-UNDO.
DEFINE VARIABLE plOpptaltFinansiering     AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltInnVeksel        AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltVeksel           AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltValuta           AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltUtbetalt         AS DECIMAL NO-UNDO.
DEFINE VARIABLE plOpptaltKupong           AS DECIMAL NO-UNDO.

DEF STREAM InnFil.

{windows.i}

DEFINE VARIABLE cFelt AS CHARACTER EXTENT 282 NO-UNDO.

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

{syspara.i 5 1 1 iCL INT}

MESSAGE '**Gurre var her xkassdaginnles.p'
VIEW-AS ALERT-BOX.

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn  = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    .

RUN LesInnFil.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
      
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr  AS INT  NO-UNDO.
  DEF VAR pcLinje    AS CHAR NO-UNDO.
  DEF VAR piAntFeil  AS INT  NO-UNDO.
  DEF VAR pcSep      AS CHAR NO-UNDO.
  DEF VAR iButikkNr  AS INT  NO-UNDO.
  
  DEFINE VARIABLE  cNumericFormat    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE  cDateFormat       AS CHARACTER  NO-UNDO.
  
  /* Endrer formater */
  ASSIGN 
    cNumericFormat         = SESSION:NUMERIC-FORMAT
    cDateFormat            = SESSION:DATE-FORMAT
    SESSION:NUMERIC-FORMAT = "American"
    SESSION:DATE-FORMAT    = "ymd".  

  ASSIGN
      pcSep = ";"
      .
  
  RUN TellOppLinjer.

  FIND LAST VPIFilLinje OF VPIFilHode NO-LOCK NO-ERROR.
  IF AVAILABLE VPIFilLinje THEN
      piLinjeNr = VPIFilLinje.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.

  ASSIGN
      iAntLinjer = 0
      .

  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    /* Blanker variabelen med alle 282 ekstenter. */
    ASSIGN
      cFelt = ''
      .
      
    /* Leser linje fra filen */
    IMPORT STREAM InnFil cFelt.

    /* Konverterer til komma som separator. */
    /*
    DO piLoop = 1 TO 282:
      cFelt[piLoop] = REPLACE(cFelt[piLoop],'.',',').
    END.
    */
    ASSIGN
      iAntLinjer = iAntLinjer + 1.

    ASSIGN
        piButikkNr                = int(cFelt[1])
        pdDato                    = DATE(cFelt[2])
        piKassererNr              = int(CFelt[3])
        piZ_Nummer                = 1
        plOpptaltKontanter        = dec(CFelt[52])
        plOpptaltSjekk            = dec(CFelt[53])
        plOpptaltReserve          = dec(CFelt[54])
        plOpptaltGavekort         = dec(CFelt[55])
        plOpptaltBilag            = dec(CFelt[56])
        plOpptaltTilgode          = 0
        plOpptaltLevertBank       = 0
        plOpptaltGavekortAndre    = 0
        plOpptaltGavekortUtlevert = 0
        plOpptaltTilgodeAndre     = 0
        plOpptaltTilgodeUtlevert  = 0
        pcPoseNr                  = ''
        plOpptaltFinansiering     = 0
        plOpptaltInnVeksel        = 0
        plOpptaltVeksel           = 0
        plOpptaltValuta           = 0
        plOpptaltUtbetalt         = dec(CFelt[170])
        plOpptaltKupong           = dec(CFelt[210])        
        piLinjeNr                 = piLinjeNr  + 1
        NO-ERROR.
    IF ERROR-STATUS:ERROR 
      THEN bOk = FALSE.
    ELSE bOk = TRUE.

    IF bOk THEN DO: 
      STATUS DEFAULT "Lese linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
      bOk = FALSE.
      RUN OppdaterKasDag.
    END.  
    ELSE DO:  
      STATUS DEFAULT "Feil på linje " + 
                   STRING(iAntLinjer) + 
                   ".".
      LEAVE LESERLINJER.
    END.
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Stempler posten som oppdatert. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = (IF bOk = TRUE 
                                       THEN 3
                                       ELSE 9)
          .
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.
  /* Stiller tilbake formater. */
  ASSIGN 
    SESSION:NUMERIC-FORMAT = cNumericFormat
    SESSION:DATE-FORMAT    = cDateFormat.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdaterKasDag) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdaterKasDag Procedure 
PROCEDURE OppdaterKasDag :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEF VAR piLoop       AS INT NO-UNDO.
  DEF VAR piKassererId AS INT NO-UNDO.
  DEF VAR piForsNr     AS INT NO-UNDO.
  
  /* Når kassen gir KASSEOPPGJØR, er kasserernummer = 0. Da plasseres oppgjøret */
  /* på den første kasserer vi finner i registeret.                             */
  IF piKassererNr = 0 THEN
  DO:
      FIND FIRST Butikkforsalj NO-LOCK WHERE
          ButikkForsalj.Butik = piButikkNr NO-ERROR.
      IF AVAILABLE butikkforsalj THEN
          ASSIGN
            piKassererNr = ButikkForsalj.ForsNr.
  END.
  ELSE DO:
      /* Endrer kassererid til kasserernummer */
      FIND ButikkForsalj NO-LOCK WHERE
          ButikkForsalj.Butik      = piButikkNr AND
          ButikkForsalj.KassererId = piKassererNr NO-ERROR.
      IF AVAILABLE ButikkForsalj THEN
          piKassererNr = ButikkForsalj.ForsNr.
  END.

  /* Klarte ikke å konvertere kasserernr. */
  IF NOT AVAILABLE ButikkForsalj THEN 
  DO:
    bOk = FALSE.
    RETURN.
  END.

  FIND KassererOppgj EXCLUSIVE-LOCK WHERE
    KassererOppgj.Dato       = pdDato AND
    KassererOppgj.ButikkNr   = piButikkNr AND
    KassererOppgj.KassererNr = piKassererNr AND
    KassererOppgj.Z_Nummer   = piZ_Nummer NO-ERROR.
  IF NOT AVAILABLE KassererOppgj THEN
  OPPRETT-OPPGJOR:
  DO:
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = piButikkNr NO-ERROR.

    CREATE KassererOppgj.
    ASSIGN
      KassererOppgj.Dato       = pdDato
      KassererOppgj.ButikkNr   = piButikkNr
      KassererOppgj.KassererNr = piKassererNr
      KassererOppgj.Z_Nummer   = piZ_Nummer
      .
  END. /* OPPRETT-OPPGJOR */

  ASSIGN
    KassererOppgj.OpptaltInnVeksel = KassererOppgj.OpptaltInnVeksel + 
                                     IF AVAILABLE Butiker
                                       THEN Butiker.StdVeksel
                                       ELSE 0.

  /* Default opprettelse av relaterte poster. */
  /* Oppretter bilag */
  IF NOT CAN-FIND(FIRST KassererBilag WHERE
              KassererBilag.ButikkNr     = piButikkNr AND
              KassererBilag.Dato         = pdDato AND
              KassererBilag.KassererNr   = piKassererNr AND
              KassererBilag.Z_Nummer     = piZ_Nummer) THEN
  DO piLoop = 1 TO 20:
      CREATE KassererBilag.
      ASSIGN
          KassererBilag.ButikkNr     = piButikkNr
          KassererBilag.Dato         = pdDato 
          KassererBilag.KassererNr   = piKassererNr 
          KassererBilag.Z_Nummer     = piZ_Nummer
          KassererBilag.BilagsNr     = piLoop
          .
  END.
  FIND FIRST KassererBilag EXCLUSIVE-LOCK WHERE
    KassererBilag.ButikkNr     = piButikkNr AND
    KassererBilag.Dato         = pdDato AND
    KassererBilag.KassererNr   = piKassererNr AND
    KassererBilag.Z_Nummer     = piZ_Nummer.

  /* Legger inn default fra kasse */
  ASSIGN
  KassererBilag.Belop  = KassererBilag.Belop + 
                         plOpptaltBilag
  KassererBilag.Meknad = "Kasse"
  .

  /* Oppretter valører */
  IF NOT CAN-FIND(FIRST KassererKontanter WHERE
              KassererKontanter.ButikkNr     = piButikkNr AND
              KassererKontanter.Dato         = pdDato AND
              KassererKontanter.KassererNr   = piKassererNr AND
              KassererKontanter.Z_Nummer     = piZ_Nummer) THEN
  DO:
      CREATE KassererKontanter.
      ASSIGN
          KassererKontanter.ButikkNr     = piButikkNr 
          KassererKontanter.Dato         = pdDato 
          KassererKontanter.KassererNr   = piKassererNr 
          KassererKontanter.Z_Nummer     = piZ_Nummer
          .
  END.
  FIND FIRST KassererKontanter EXCLUSIVE-LOCK WHERE
      KassererKontanter.ButikkNr     = piButikkNr AND
      KassererKontanter.Dato         = pdDato AND
      KassererKontanter.KassererNr   = piKassererNr AND
      KassererKontanter.Z_Nummer     = piZ_Nummer.
  /* Beløp fra kasse legges inn i KRONE valøren */
  ASSIGN
      KassererKontanter.Belop[2]        = KassererKontanter.Belop[2] +
                                          plOpptaltKontanter
      KassererKontanter.AntallValor[2]  = KassererKontanter.AntallValor[2] + 
                                          plOpptaltKontanter
      .

  /* Oppretter valuta */
  FOR EACH KasValuta NO-LOCK WHERE
      KasValuta.ValAktiv = TRUE AND
      KasValuta.EgenValuta = FALSE AND
      KasValuta.KasseValkurs <> 0:

      FIND KassererValuta EXCLUSIVE-LOCK WHERE
          KassererValuta.ButikkNr     = piButikkNr AND
          KassererValuta.Dato         = pdDato AND
          KassererValuta.KassererNr   = piKassererNr AND
          KassererValuta.Z_Nummer     = piZ_Nummer AND
          KassererValuta.ValKod       = KasValuta.ValKod NO-ERROR.
      IF NOT AVAILABLE KassererValuta THEN
      DO:
          CREATE KassererValuta.
          ASSIGN
              /* Nøkkel */
              KassererValuta.ButikkNr     = piButikkNr 
              KassererValuta.Dato         = pdDato 
              KassererValuta.KassererNr   = piKassererNr 
              KassererValuta.Z_Nummer     = piZ_Nummer
              KassererValuta.ValKod       = KasValuta.ValKod
              .
      END.
      ASSIGN
          KassererValuta.KasseValkurs = KasValuta.KasseValkurs / KasValuta.Indeks
          KassererValuta.KasseValkurs = IF KassererValuta.KasseValkurs = ?
                                          THEN 0
                                          ELSE KassererValuta.KasseValkurs
          .
  END.

  ASSIGN
    KassererOppgj.OpptaltKontanter        = KassererOppgj.OpptaltKontanter        + plOpptaltKontanter
    KassererOppgj.OpptaltSjekk            = KassererOppgj.OpptaltSjekk            + plOpptaltSjekk
    KassererOppgj.OpptaltReserve          = KassererOppgj.OpptaltReserve          + plOpptaltReserve
    KassererOppgj.OpptaltGavekort         = KassererOppgj.OpptaltGavekort         + plOpptaltGavekort
    KassererOppgj.OpptaltBilag            = KassererOppgj.OpptaltBilag            + plOpptaltBilag
    KassererOppgj.OpptaltTilgode          = KassererOppgj.OpptaltTilgode          + plOpptaltTilgode
    KassererOppgj.OpptaltLevertBank       = KassererOppgj.OpptaltLevertBank       + plOpptaltLevertBank
    KassererOppgj.OpptaltGavekortAndre    = KassererOppgj.OpptaltGavekortAndre    + plOpptaltGavekortAndre
    KassererOppgj.OpptaltGavekortUtlevert = KassererOppgj.OpptaltGavekortUtlevert + plOpptaltGavekortUtlevert
    KassererOppgj.OpptaltTilgodeAndre     = KassererOppgj.OpptaltTilgodeAndre     + plOpptaltTilgodeAndre
    KassererOppgj.OpptaltTilgodeUtlevert  = KassererOppgj.OpptaltTilgodeUtlevert  + plOpptaltTilgodeUtlevert
    KassererOppgj.PoseNr                  = KassererOppgj.PoseNr + (IF KassererOppgj.PoseNr <> '' THEN ', ' ELSE '') + pcPoseNr
    KassererOppgj.OpptaltFinansiering     = KassererOppgj.OpptaltFinansiering     + plOpptaltFinansiering
    KassererOppgj.OpptaltInnVeksel        = KassererOppgj.OpptaltInnVeksel        + plOpptaltInnVeksel
    KassererOppgj.OpptaltVeksel           = KassererOppgj.OpptaltVeksel           + plOpptaltVeksel  
    KassererOppgj.OpptaltValuta           = KassererOppgj.OpptaltValuta           + plOpptaltValuta  
    KassererOppgj.OpptaltUtbetalt         = KassererOppgj.OpptaltUtbetalt         + plOpptaltUtbetalt
    KassererOppgj.OpptaltKupong           = KassererOppgj.OpptaltKupong           + plOpptaltKupong
    bOk = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure 
PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iTotAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) NO-ECHO.
  REPEAT:
    IMPORT STREAM InnFil UNFORMATTED cLinje.
    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

