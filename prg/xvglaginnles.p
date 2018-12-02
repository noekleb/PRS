&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description : 

    Author(s)   : Tom Nøkleby
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
DEF VAR ctmpKatalog   AS CHAR NO-UNDO.
DEF VAR pcLinje       AS CHAR NO-UNDO.
DEF VAR h_PrisKo      AS HANDLE NO-UNDO.

DEF VAR hField1       AS HANDLE NO-UNDO.
DEF VAR c02SjekkListe AS CHAR NO-UNDO.
DEF VAR cGenEan       AS CHAR NO-UNDO.
DEF VAR iLevNr        AS INT  NO-UNDO.
DEF VAR lArtikkelNr   AS DEC  NO-UNDO.
DEF VAR iStrKode      AS INT  NO-UNDO.

DEF VAR cEANNo               AS CHAR NO-UNDO.
DEF VAR cSupplArtNo          AS CHAR NO-UNDO.
DEF VAR cMainSupplierNo      AS CHAR NO-UNDO.
DEF VAR cName                AS CHAR NO-UNDO.
DEF VAR cMainGroupNo         AS CHAR NO-UNDO.
DEF VAR cIntermediateGroupNo AS CHAR NO-UNDO.
DEF VAR cSubGroupNo          AS CHAR NO-UNDO.
DEF VAR cZUsrLevfarge        AS CHAR NO-UNDO.
DEF VAR cLastUpdate          AS CHAR NO-UNDO.

DEF STREAM InnFil.
DEF STREAM UtFil.

DEFINE TEMP-TABLE TT_LevLager LIKE LevLager 
/*     FIELD LevNr LIKE LevBas.LevNr           */
/*     FIELD ArtikkelNr LIKE ArtBas.ArtikkelNr */
/*     FIELD IBestilling LIKE Lager.LagAnt     */
/*     FIELD IOrdre LIKE Lager.LagAnt          */
/*     FIELD ILager LIKE Lager.LagAnt          */
/*     FIELD NesteLevDato AS DATE              */
    .

DEFINE TEMP-TABLE tt_Error NO-UNDO
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .

{windows.i}

DEFINE BUFFER BufTT_LevLager FOR tt_LevLager.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-FixChk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD FixChk Procedure 
FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


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

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
FIND EkstVPILev OF VPIFilHode.
FIND LevBas NO-LOCK WHERE
    LevBas.LevNr = EkstVPILev.LevNr NO-ERROR.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn
    ctmpKatalog = SESSION:TEMP-DIRECTORY
    iLevNr      = IF AVAILABLE LevBas THEN LevBas.LevNr ELSE 0
    .

/* Starter procedure bibliotek. */
IF NOT VALID-HANDLE(h_prisko) THEN
    RUN  prisko.p PERSISTENT SET h_prisko.

RUN LesInnFil.

/* Stopper innlesningsprogram for håndterminalfil. */
IF VALID-HANDLE(h_PrisKo) THEN
    DELETE PROCEDURE h_PrisKo.

RETURN.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-byggTTLevLager) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byggTTLevLager Procedure 
PROCEDURE byggTTLevLager :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER ipOrdreNr AS INTEGER    NO-UNDO.

  EMPTY TEMP-TABLE TT_LevLager.

    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ErrorLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLogg Procedure 
PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   OUTPUT TO VALUE("Error.Txt").                                                 */
/*     PUT UNFORMATTED                                                             */
/*       "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP */
/*       "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn skip      */
/*       .                                                                         */
/*     FOR EACH tt_Error:                                                          */
/*       PUT UNFORMATTED tt_Error.Tekst SKIP.                                      */
/*     END.                                                                        */
/*   OUTPUT CLOSE.                                                                 */
  IF SEARCH("Error.Txt") <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH("Error.Txt"),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FixStorl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixStorl Procedure 
PROCEDURE FixStorl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def INPUT-output parameter wStorl as char NO-UNDO.

  DEF VAR wDecimaler as CHAR NO-UNDO.

  {syspara.i 1 1 16 wDecimaler}

  assign
     wStorl = trim(wStorl)
     wStorl = caps(wStorl)
     wStorl = if (length(wStorl) = 1 or
                  length(wStorl) = 3
                  )
                 then " " + wStorl
                 else wStorl.

  /* Bytter ut eventuelle comma med punkt. */
  if index(wStorl,",") <> 0 then
    OVERLAY(wStorl, index(wStorl,","), 1, "CHARACTER") = ".".

  RETURN wStorl.   /* Function return value. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-genEAN) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE genEAN Procedure 
PROCEDURE genEAN :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plArtikkelNr AS DEC  NO-UNDO.
  DEF INPUT PARAMETER cStorl       AS CHAR NO-UNDO.

  DEF VAR cKode AS CHAR NO-UNDO.

  FIND StrKonv WHERE StrKonv.Storl = cStorl USE-INDEX Storl NO-LOCK NO-ERROR.
  IF NOT AVAIL StrKonv THEN
      RETURN.
  /* Finnes det strekkode på størrrelsen fra før, skal vi ikke legge opp ny. */
  IF CAN-FIND(FIRST StrekKode WHERE StrekKode.ArtikkelNr = ArtBas.ArtikkelNr AND
                              StrekKode.KodeType = 1 AND
                              StrekKode.StrKode  = StrKonv.StrKode
                          /*  AND StrekKode.Kode BEGINS "02" */
                              ) THEN RETURN.

  ASSIGN cKode = "02" + STRING(ArtBas.ArtikkelNr,"9999999") + STRING(StrKonv.StrKode,"999")
         cKode = FixChk(cKode).

  CREATE StrekKode.
  ASSIGN StrekKode.ArtikkelNr = ArtBas.ArtikkelNr
         StrekKode.Kode       = cKode
         StrekKode.KodeType   = 1 /* använd inte iKodeType, vi kan ha 0 */
         StrekKode.StrKode    = StrKonv.StrKode 
         StrekKode.VareId     = ArtBas.ArtikkelNr
      NO-ERROR.
  /* TN Koden kan finnes fra før - 02 koder gav feilmelding. */
  IF ERROR-STATUS:ERROR THEN
  DO:
      IF AVAILABLE StrekKode THEN
          DELETE StrekKode.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr   AS INT  NO-UNDO.
  DEF VAR piAntFeil   AS INT  NO-UNDO.
  DEF VAR pcBkuFil    AS CHAR NO-UNDO.
  DEF VAR piLoop      AS INT  NO-UNDO.
  DEF VAR pcLinje     AS CHAR NO-UNDO.
  DEF VAR dDato       AS DATE NO-UNDO.

  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.
  FOR EACH tt_LevLager:
      DELETE tt_LevLager.
  END.

  RUN TellOppLinjer.

  IF AVAILABLE tt_LevLager THEN
      DELETE tt_LevLager.

  ASSIGN
      piLinjeNr  = 1.
      iAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT TRANSACTION:
    /* Leser linje fra filen */
    IMPORT STREAM InnFil UNFORMATTED pcLinje NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        ASSIGN iAntLinjer = iAntLinjer + 1.
      DO piLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
          ASSIGN piAntFeil = piAntFeil + 1.
          ERROR-STATUS:GET-NUMBER(piLoop).          
          CREATE tt_Error.
          ASSIGN
            tt_Error.LinjeNr = piAntFeil
            tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " " + ERROR-STATUS:GET-MESSAGE(piLoop).
            .
      END.
      NEXT LESERLINJER.
    END.
    /* Skipper Heading */
    IF iantLinjer = 0 THEN
    DO:
        iAntLinjer = 1.
        NEXT LESERLINJER.
    END.
    IF NUM-ENTRIES(pcLinje,";") <> 14 THEN
    DO:
        ASSIGN piAntFeil = piAntFeil + 1.
        ERROR-STATUS:GET-NUMBER(1).          
        CREATE tt_Error.
        ASSIGN
          tt_Error.LinjeNr = piAntFeil
          tt_Error.Tekst   = "** Feil antall entries på linje " + STRING(iAntLinjer) + " " + ERROR-STATUS:GET-MESSAGE(1).
          .
        NEXT LESERLINJER.
    END.

    ASSIGN 
        iAntLinjer  = iAntLinjer + 1
        lArtikkelNr = dec(SUBstring(STRING(DEC(trim(ENTRY(1,pcLinje,";"),'"')),"9999999999999"),1,10))
        iStrKode    = int(SUBstring(STRING(DEC(trim(ENTRY(1,pcLinje,";"),'"')),"9999999999999"),11,3))
        /* Øvrige artikkelinformasjonsfelt som kan oppdateres i Visma. */
        cEANNo               = trim(trim(ENTRY(6,pcLinje,";"),'"'))              
        cSupplArtNo          = trim(trim(ENTRY(7,pcLinje,";"),'"'))                       
        cMainSupplierNo      = trim(trim(ENTRY(8,pcLinje,";"),'"'))                   
        cName                = trim(trim(ENTRY(9,pcLinje,";"),'"'))                             
        cMainGroupNo         = trim(trim(ENTRY(10,pcLinje,";"),'"'))                      
        cIntermediateGroupNo = trim(trim(ENTRY(11,pcLinje,";"),'"'))              
        cSubGroupNo          = trim(trim(ENTRY(12,pcLinje,";"),'"'))                       
        cZUsrLevfarge        = trim(trim(ENTRY(13,pcLinje,";"),'"'))                     
        cLastUpdate          = trim(trim(ENTRY(14,pcLinje,";"),'"'))                       
        NO-ERROR.
    /* Sist oppdatert dato */
    IF NUM-ENTRIES(cLastUpdate,'/') = 3 THEN
        ASSIGN
        dDato = DATE(int(SUbstring(cLastUpdate,4,2)),
                     int(SUbstring(cLastUpdate,1,2)),
                     int("20" + SUbstring(cLastUpdate,7,2)))
        .
    ELSE dDato = ?.

    /* Logger feil som følge av alfanumeriske verdier i de numeriske feltene. */
    IF ERROR-STATUS:ERROR THEN
    DO:
      DO piLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
          ASSIGN piAntFeil = piAntFeil + 1.
          ERROR-STATUS:GET-NUMBER(piLoop).          
          CREATE tt_Error.
          ASSIGN
            tt_Error.LinjeNr = piAntFeil
            tt_Error.Tekst   = "** Feil på linje - Alfanumeriske verdier i artikkelnr/strkode." + STRING(iAntLinjer) + " " + ERROR-STATUS:GET-MESSAGE(piLoop).
            .
      END.
      NEXT LESERLINJER.
    END.

    /* SEnr begynner alltid med 9 og er på 7 siffer. */
    /* Andre nummer er gamle Karlson nummer.         */
    IF lArtikkelNr < 9000000 THEN
        NEXT LESERLINJER.

    /* Denne størrelseskoden skal ikke ha lager. */
    IF iStrKode = 0 THEN
        NEXT LESERLINJER.

    /* Koden skal være 13 tegn lang */
    IF LENGTH(cEANNo) <> 13 THEN
        cEANNo = FILL("0",13 - LENGTH(cEANNo)) + cEANNo.

    /* Validerer artikkel og størrelse. */
    RUN ValiderInput (INPUT-OUTPUT piAntFeil).
    IF RETURN-VALUE = "FEIL" THEN DO:
        NEXT LESERLINJER.
    END.

    /* Artikkel = 0 skal ikke leses inn. */
    IF lArtikkelNr = 0 THEN
        NEXT LESERLINJER.
    /* Henter artikkel */
    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
    DO:
        NEXT LESERLINJER.
    END.

    /* Oppdater artikkelinformasjon */
    IF dDato <> ? AND dDato >= TODAY - 600 THEN
        RUN OppdatArtikkelInformasjon.

    /* Leser inn data fra record. */
    FIND LevLager EXCLUSIVE-LOCK WHERE
        LevLager.LevNr      = (IF AVAILABLE ArtBas 
                                    THEN ArtBas.LevNr
                                    ELSE iLevNr) AND
        LevLager.ArtikkelNr = lArtikkelNr AND
        LevLager.StrKode    = iStrKode NO-ERROR.
    IF NOT AVAILABLE LevLager THEN
    OPPSTANDELSE:
    DO:
        CREATE LevLager.
        ASSIGN
            LevLager.LevNr      = IF AVAILABLE ArtBas 
                                    THEN ArtBas.LevNr
                                    ELSE iLevNr 
            LevLager.ArtikkelNr = lArtikkelNr 
            LevLager.StrKode    = iStrKode
            NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
        DO:
          DO piLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
              ASSIGN piAntFeil = piAntFeil + 1.
              ERROR-STATUS:GET-NUMBER(piLoop).          
              CREATE tt_Error.
              ASSIGN
                tt_Error.LinjeNr = piAntFeil
                tt_Error.Tekst   = "** Feil på linje - Fikk ikke opprettet lagerpost." + STRING(iAntLinjer) + " " + ERROR-STATUS:GET-MESSAGE(piLoop).
                .
          END.
          NEXT LESERLINJER.
        END.
    END. /* OPPSTANDELSE */

    /* Artikkel;IBestilling;IOrdre;Lager;NesteDato */    
    ASSIGN
        LevLager.iBestilling   = dec(REPLACE (ENTRY(2,pcLinje,";"),'.',','))
        LevLager.iOrdre        = dec(REPLACE (ENTRY(3,pcLinje,";"),'.',','))
        LevLager.iLager        = dec(REPLACE (ENTRY(4,pcLinje,";"),'.',','))
        LevLAger.cLager        = IF LevLager.iLager < 20 THEN STRING(int(LevLager.iLager))
                                 ELSE IF LevLager.iLager >=  20 AND LevLager.iLager < 50 THEN " 20+"
                                 ELSE IF LevLager.iLager >=  50 AND LevLager.iLager < 100 THEN " 50+"
                                 ELSE IF LevLager.iLager >= 100 AND LevLager.iLager < 500 THEN "100+"
                                 ELSE "500+"
        LevLager.NesteAnkDato  = DATE(ENTRY(5,pcLinje,";"))
        NO-ERROR.
    /* Logger feil som følge av alfanumeriske verdier i de numeriske feltene. */
    IF ERROR-STATUS:ERROR THEN
    DO:
      DO piLoop = 1 TO ERROR-STATUS:NUM-MESSAGES:
          ASSIGN piAntFeil = piAntFeil + 1.
          ERROR-STATUS:GET-NUMBER(piLoop).          
          CREATE tt_Error.
          ASSIGN
            tt_Error.LinjeNr = piAntFeil
            tt_Error.Tekst   = "** Feil på linje - alfanumeriske verdier i numeriske felt." + STRING(iAntLinjer) + " " + ERROR-STATUS:GET-MESSAGE(piLoop).
            .
      END.
      IF NEW LevLager AND AVAILABLE LevLager THEN
          DELETE LevLager.

      NEXT LESERLINJER.
    END.

    /* Negativt antall skal skjules. */
    IF LevLager.iLager < 0 THEN
        ASSIGN
        LevLager.iLager = 0
        LevLager.cLager = ""
        .

    IF AVAILABLE LevLager THEN
        RELEASE LevLager.

    STATUS DEFAULT "Leser linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  /* Stempler posten som innlest. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 3
          .
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

  IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.

  ASSIGN
  pcBkuFil = VPIFilHode.Katalog + "~\bku" + "\" + 
             VPIFilHode.FilNavn
  .

  /* LAGERFILEN */
  /* Sikrer at backup katalog finnes. */
  OS-CREATE-DIR value(VPIFilHode.Katalog + "~\bku").
  /* Flytter filen til backup katalog. */
  OS-COPY value(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) 
          value(pcBkuFil).
  /* Renser bort fil */
  IF SEARCH(pcBkuFil) <> ? THEN
  DO:
      /* Filen tas bort fra katalogen. */
      IF SEARCH(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) <> ? THEN
          OS-DELETE VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OppdatArtikkelinformasjon) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OppdatArtikkelinformasjon Procedure 
PROCEDURE OppdatArtikkelinformasjon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  ------------------------
  cEANNo              
  cSupplArtNo         
  cMainSupplierNo     
  cName               
  cMainGroupNo        
  cIntermediateGroupNo
  cSubGroupNo         
  cZUsrLevfarge       
  cLastUpdate         
  -------------------------
  
  lArtikkelNr
  iStrKode   
  
  
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lDec     AS DECIMAL NO-UNDO.
DEF VAR cFilNavn AS CHAR NO-UNDO.

DEF BUFFER bStrekkode FOR Strekkode.

/* MESSAGE                                    */
/*     "ArtikkelNR:" lArtikkelNr ArtBAs.Beskr */
/*     "StrKode:" iStrKode SKIP               */
/*     cEANNo              SKIP               */
/*     cSupplArtNo         SKIP               */
/*     cMainSupplierNo     SKIP               */
/*     cName               SKIP               */
/*     cMainGroupNo        SKIP               */
/*     cIntermediateGroupNo SKIP              */
/*     cSubGroupNo         SKIP               */
/*     cZUsrLevfarge       SKIP               */
/*     cLastUpdate                            */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK.     */

  /* Korreksjon av EAN kode. */
  IF cEANNo <> "" THEN
  EAN-KORR:
  DO:
    /* Ligger det ugyldige tegn i kodefeltet behandles det ikke. */
    ASSIGN
        lDec = DEC(cEANNo) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        LEAVE EAN-KORR.

    /* Henter strekkoden hvis den finnes */
    FIND Strekkode EXCLUSIVE-LOCK WHERE
        Strekkode.Kode = cEANNo NO-ERROR.

    /* Finnes ikke strekkoden på artikkel/størrelse, skal den opprettes. */
    IF NOT AVAILABLE Strekkode THEN
    OPPRETT:
    DO:
      CREATE StrekKode.
      ASSIGN StrekKode.ArtikkelNr        = ArtBas.ArtikkelNr
             StrekKode.Kode              = cEANNo
             StrekKode.KodeType          = 1 /* använd inte iKodeType, vi kan ha 0 */
             StrekKode.StrKode           = iStrKode 
             StrekKode.VareId            = ArtBas.ArtikkelNr
             Strekkode.IKasse            = TRUE
             Strekkode.HovedNr           = TRUE
             Strekkode.Bestillingsnummer = cSupplArtNo
          NO-ERROR.
    END. /* OPPRETT */
    ELSE
        Strekkode.HovedNr = TRUE.

    /* TEST */
    /*
    FIND ArtBas OF Strekkode.
    ASSIGN
        ArtBas.Anonseartikkel = TRUE.
    */
    /* TEST - slutt. */

    /* Er det ulik størrelseskode (strkode) på strekkoderecorden ? */
    IF Strekkode.StrKode <> iStrKode THEN
    STRKODE:
    DO:
        /* - Strkode skal byttes ut i strekkodeposten.  */
        ASSIGN
            Strekkode.StrKode = iStrKode
            .
        /* - Dette skal også gjøres i messeordre og suppleringsordrebøkene. */
    END. /* STRKODE */

    /* Ligger strekkoden på en annen artikkel? */
    IF Strekkode.ArtikkelNr <> lArtikkelNr THEN
    ARTIKKELNR:
    DO:
        /* - Strekkoden skal flyttes til riktig artikkel */
        ASSIGN
            Strekkode.ArtikkelNr = ArtBas.ArtikkelNr
            .
        /* - Sjekk om dette påvirker noen vareh.bøker (Messeordre og suppleringsbøker). */
    END. /* ARTIKKELNR */

    /* Ligger det dublettstrekkoder på denne størrelsen?                      */
    /* Alle andre strekkoder på den samme størrelse og artikkel skal slettes. */
    SLETTDUBLETT:
    FOR EACH bStrekkode EXCLUSIVE-LOCK WHERE
      bStrekkode.ArtikkelNr = ArtBas.ArtikkelNr:
        
      /* Det er kun denne størrelsen vi skal ta hånd om. */
      IF bStrekkode.StrKode = iStrKode THEN
      DO:
          /* Alle andre strekkoder på denne størrelsen skal ikke ha hovednummer. */
          IF bStrekkode.Kode <> Strekkode.Kode THEN
          DO:
              /* Her må strekkoden i vareb.bøkene byttes ut. */
              /* DELETE bStrekkode.*/
              Strekkode.HovedNr = FALSE.
          END.
      END.
    END. /* SLETTDUBLETT */
  END. /* EAN-KORR */
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
  repeat:
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

&IF DEFINED(EXCLUDE-ValiderInput) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderInput Procedure 
PROCEDURE ValiderInput :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT-OUTPUT PARAMETER piAntFeil AS INTEGER  NO-UNDO.
  DEFINE VARIABLE               iInputAnt AS INTEGER  NO-UNDO.

  ASSIGN iInputAnt = piAntFeil.
  IF NOT CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = lArtikkelNr) THEN DO:
      CREATE tt_Error.
      ASSIGN
        piAntFeil = piAntFeil + 1
        tt_Error.LinjeNr = piAntFeil
        tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " " + "Finner ikke artikkelen " + STRING(lArtikkelNr) + ".".
        .
  END.
  IF NOT CAN-FIND(StrKonv WHERE StrKonv.StrKode = iStrKode) THEN DO:
      CREATE tt_Error.
      ASSIGN
        piAntFeil = piAntFeil + 1
        tt_Error.LinjeNr = piAntFeil
        tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + " " + "Finner ikke størrelseskoden " + STRING(iStrKode) + ".".
        .
  END.
  
  RETURN STRING(iInputAnt = piAntFeil,"/FEIL").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-FixChk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION FixChk Procedure 
FUNCTION FixChk RETURNS CHARACTER
    ( INPUT cKode AS CHARACTER ) :
  /*------------------------------------------------------------------------------
    Purpose:  
      Notes:  
  ------------------------------------------------------------------------------*/
      DEF VAR iCount1 AS INTE NO-UNDO.
      DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
      DEF VAR iSum AS INTE NO-UNDO.

      DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
          ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
                 iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
      END.
      RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

