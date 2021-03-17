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
DEF INPUT  PARAMETER rRowId    AS ROWID  NO-UNDO.
DEF INPUT  PARAMETER h_PrisKo  AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER cFilError AS CHAR   NO-UNDO.

DEF VAR plJf AS LOG NO-UNDO. /* Flagger håndtering av mottagende butikk */

DEF VAR cError            AS CHAR NO-UNDO.
DEF VAR cPOSKoder         AS CHAR NO-UNDO.
DEF VAR cTTIdKoder        AS CHAR NO-UNDO.
DEF VAR iEntry            AS INT  NO-UNDO.
DEF VAR cTTId             AS CHAR NO-UNDO.
DEF VAR cPOS              AS CHAR NO-UNDO.
DEF VAR cKodeVarelinje    AS CHAR NO-UNDO.
DEF VAR cKodeBetTrans     AS CHAR NO-UNDO.
DEF VAR cVareGruppeNavn   AS CHAR NO-UNDO.
DEF VAR cMvaGruppeNavn    AS CHAR NO-UNDO.
DEF VAR cBongTekst        AS CHAR NO-UNDO.
DEF VAR iKortType         AS INT  NO-UNDO.
DEF VAR dArtikkelNr       AS DEC  NO-UNDO.
DEF VAR iMottagendeButikk AS INT  NO-UNDO.
DEF VAR iKassererNr       AS INT  NO-UNDO.
DEF VAR cKassererNavn     AS CHAR NO-UNDO.
DEF VAR iVg               AS INT  NO-UNDO.
DEF VAR iMomsKod          AS INT  NO-UNDO.
DEF VAR iLopeNr           AS INT  NO-UNDO.
DEF VAR cStorl            AS CHAR NO-UNDO.
DEF VAR iAntall           AS INT  NO-UNDO.
DEF VAR bRabatt           AS DEC  NO-UNDO.
DEF VAR bBelop            AS DEC  NO-UNDO.
DEF VAR iKontoNr          AS INT  NO-UNDO.
DEF VAR cKortNr           AS CHAR NO-UNDO.
DEF VAR iTid              AS INT  NO-UNDO.
DEF VAR cTid              AS char NO-UNDO.
DEF VAR bPinPoint         AS LOG  INITIAL FALSE NO-UNDO.
DEF VAR cTekst            AS CHAR NO-UNDO.
DEF VAR dVVareKost        AS DEC  NO-UNDO.
DEF VAR dMvaKr            AS DEC  NO-UNDO.
DEF VAR dMva%             AS DEC  NO-UNDO.
DEF VAR dMvaKod           AS INT  NO-UNDO.
DEF VAR iHgr              AS INT  NO-UNDO.
DEF VAR cHgrNavn          AS CHAR NO-UNDO.
DEF VAR cKundeKort        AS CHAR NO-UNDO. 
DEF VAR cMedlemsKort      AS CHAR NO-UNDO. 
DEF VAR dKundeNr          AS DEC  NO-UNDO. 
DEF VAR dMedlemsNr        AS DEC  NO-UNDO.
DEF VAR cKundeNavn        AS CHAR NO-UNDO. 
DEF VAR cMedlemsNavn      AS CHAR NO-UNDO.
DEF VAR iCl               AS INT  NO-UNDO.
DEF VAR iFeilKode         AS INT  NO-UNDO.
DEF VAR iNotatKode        AS INT  NO-UNDO.
DEF VAR cFeilKodeTekst    AS CHAR NO-UNDO.
DEF VAR cNotatKodeTekst   AS CHAR NO-UNDO.
DEF VAR cReturKassererNavn AS CHAR NO-UNDO.
DEF VAR iReturButikk      AS INT  NO-UNDO.
DEF VAR iReturKassererNr  AS INT  NO-UNDO.
DEF VAR bLoggSaldoFeil    AS LOG  NO-UNDO.
{dproclibstart.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-Mva2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD Mva2 Procedure 
FUNCTION Mva2 RETURNS DECIMAL
  ( INPUT plBelop AS dec )  FORWARD.

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

/* Konverteringstabell SkoTex POS 1.0 til SkoTex BackOffice.*/ 
ASSIGN
  cPOSKoder      = "000,xxx,xxx,016,xxx,017,xxx,xxx,xxx,002,xxx,001,004,xxx,006,xxx,005,xxx,012,xxx,xxx,xxx,xxx,010,011,020,xxx,008,007,xxx,xxx,xxx,009,013,018,019,080,081,082,021,022,xxx,099"
  cTTIdKoder     = "001,002,003,004,005,006,007,008,009,010,011,012,050,051,052,053,054,055,056,057,058,059,060,061,062,063,064,065,066,067,068,069,070,071,072,073,080,081,082,096,097,098,099"
  cKodeVarelinje = "000,001,002,016,017"
  cKodeBetTrans  = "004,005,006,007,008,009,010,011,012,013,018,019,020,080,081,082"
  .

{syspar2.i 1 10 1 cTekst}
IF cTekst = "1" THEN
    bPinPoint = TRUE.
ELSE
    bPinPoint = FALSE.

{syspara.i 210 3 1 cTekst}
IF cTekst = "1" THEN
    plJf = TRUE.
ELSE
    plJf = FALSE.

/* Sentrallager */
{syspara.i 5 1 1 iCl INT}
/* Logge saldofeil i bong */
{syspara.i 1 1 110 cTekst}
IF cTekst = "1" THEN
    bLoggSaldoFeil = TRUE.
ELSE
    bLoggSaldoFeil = FALSE.

DO TRANSACTION:
  FIND BongHode EXCLUSIVE-LOCK where
      rowid(BongHode) = rRowId NO-ERROR.
  IF NOT AVAILABLE BongHode THEN
    RETURN "** Finner ikke bonghode med rowid: " + STRING(rRowId) + ".".

  RUN KonverterBong.

END. /* TRANSACTION */

/* Returnerer liste med funne feil til kallende programm. */
RETURN cError.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-FixStorl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FixStorl Procedure 
PROCEDURE FixStorl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT-OUTPUT PARAMETER pcStorl AS CHAR NO-UNDO.

    DEF VAR X      AS INT NO-UNDO.
    DEF VAR xx     AS INT NO-UNDO.
    DEF VAR i      AS INT NO-UNDO.
    DEF VAR dstorl AS DEC NO-UNDO.

    /* Tester om det er numeriske st|rrelser.                  */
        /* Er st|rrelsene numerisk, vil xx bli lik 4, ellers ikke. */
        /* Alfanumeriske st|rrelser, passerer rett igjennom.       */
        STR-TEST:
        do:
            /* Er det numeriske st|rrelser, skal st|rrelsen formateres om */
            xx = 0.
            do i = 1 TO 4 :
                if substring(pcStorl,i,1) >= "0" and
                substring(pcStorl,i,1) <= "9"
                then do :
                    xx = xx + 1.
                    if xx = 4 then do:
                        /* Numeriske st|rrelser */
                        dstorl = decimal(pcStorl) / 10.
                        pcStorl = string(dstorl).
                        /* Konverterer st|rrelse "0" til " 1". */
                        if integer(pcStorl) = 0 then pcStorl = "1".
                    end.
                end.
            end.
 
            /* Plokker bort alle blanke som st}r i f|rste posisjon. */
            PLOKK:
            repeat:
                if substring(pcStorl,1,1) = " " then
                do:
                    pcStorl = substring(pcStorl,2,length(pcStorl)).
                    next PLOKK.
                end.
                else leave PLOKK.
            end. /* PLOKK */
 
            /* Formaterer st|rrelsen slik at den sorteres riktig i skotex. */
            if length(pcStorl) = 3 or length(pcStorl) = 1 then
                pcStorl = " " + pcStorl.
        end. /* STR-TEST */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ForDelSubTotRab1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ForDelSubTotRab1 Procedure 
PROCEDURE ForDelSubTotRab1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plSubTotRab  AS DEC NO-UNDO.
  DEF INPUT PARAMETER piTTId       AS INT NO-UNDO.

  DEF VAR plDiff       AS DEC NO-UNDO.
  DEF VAR plRab        AS DEC NO-UNDO.
  DEF VAR plSum        AS DEC NO-UNDO.
  DEF VAR plTildeltRab AS DEC NO-UNDO.

  ASSIGN
      plTildeltRab = 0
      plDiff       = 0
      .

  /* Finner summen det skal fordeles ut fra. */
  FOR EACH BongLinje NO-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id AND
      BongLinje.Makulert = FALSE AND
      BongLinje.TTId = piTTId:
          ASSIGN
          plSum = plSum + BongLinje.LinjeSum - BongLinje.LinjeRab.
  END.

  /* Ikke noe her. */
  IF plSum = 0 THEN
      return.

  /* Fordeler rabatten på varesalgstranser. */
  FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id AND
      BongLinje.TTId = piTTId:  
      ASSIGN
          plRab                 = ROUND(((((BongLinje.LinjeSum / plSum) * 100) * plSubTotRab) / 100),2)
          BongLinje.SubtotalRab = plRab 
          plTildeltRab          = plTildeltRab + plRab                       
          .
  END.

  ASSIGN
      plDiff = plSubTotRab - plTildeltRab
      .

  /* Er det en diff, legges denne på den første raden. */
  IF plDiff <> 0 THEN
  DO:
      FIND FIRST BongLinje EXCLUSIVE-LOCK WHERE
          BongLinje.B_Id = BongHode.B_Id AND
          BongLinje.TTId = piTTId NO-ERROR.
      IF AVAILABLE BongLinje THEN
      ASSIGN
          BongLinje.SubtotalRab = BongLinje.SubtotalRab + plDiff
          .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-FordelSubTotRabatt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FordelSubTotRabatt Procedure 
PROCEDURE FordelSubTotRabatt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF INPUT PARAMETER plSubTotRab AS DEC NO-UNDO.

DEF VAR plSum AS DEC NO-UNDO.

/* 1. Bare varesalgstranser. SubTotRabatt fordeles på varesalgslinjene. */
IF NOT CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.TTId = 10) AND
    NOT CAN-FIND(FIRST BongLinje WHERE
                 BongLinje.B_Id = BongHode.B_Id AND
                 BongLinje.TTid = 3) THEN
    RUN ForDelSubTotRab1 (plSubTotRab, 1).

/* 2. Bare returtranser. SubTotRabatt fordeles på returlinjene */
ELSE IF NOT CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.TTId = 1) AND
    NOT CAN-FIND(FIRST BongLinje WHERE
                 BongLinje.B_Id = BongHode.B_Id AND
                 BongLinje.TTid = 3) THEN
    RUN ForDelSubTotRab1 (plSubTotRab, 10).
/* 3. Bare reklamasjoner. SubTotRabatt fordeles på reklamasjonslinjene */
ELSE IF NOT CAN-FIND(FIRST BongLinje WHERE
                BongLinje.B_Id = BongHode.B_Id AND
                BongLinje.TTId = 1) AND
    NOT CAN-FIND(FIRST BongLinje WHERE
                 BongLinje.B_Id = BongHode.B_Id AND
                 BongLinje.TTid = 10) THEN
    RUN ForDelSubTotRab1 (plSubTotRab, 3).

/* Blanding */
ELSE DO:
    /* Finner summen det skal fordeles ut fra. */
    FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id AND
        BongLinje.Makulert = FALSE:
        IF can-do("01",string(BongLinje.TTId)) THEN
            ASSIGN
            plSum = plSum + (BongLinje.LinjeSum - BongLinje.LinjeRab).
        ELSE IF can-do("10",string(BongLinje.TTId)) THEN
            ASSIGN
            plSum = plSum - (BongLinje.LinjeSum - BongLinje.LinjeRab).
    END.
    /* 3. Bongens sum er positiv. Fordeling gjøres på varesalgslinjene. */ 
    IF plSum > 0 THEN
        RUN ForDelSubTotRab1 (plSubTotRab, 1).
    /* 4. Bongens sum er negativ. Fordeling gjøres på returlinjene. */ 
    ELSE IF plSum < 0 THEN
        RUN ForDelSubTotRab1 (plSubTotRab, 10).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KonverterBong) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KonverterBong Procedure 
PROCEDURE KonverterBong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR prRowId       AS ROWID NO-UNDO.
  DEF VAR cOriginalData AS CHAR  NO-UNDO.
  DEF VAR piBongLinje   AS INT   NO-UNDO.
  DEF VAR plSubTotRab   AS DEC   NO-UNDO.

  DEF VAR plVareVerdi  AS DEC NO-UNDO.
                      
  DEF BUFFER bufBongLinje FOR BongLinje.

  /* NB: Datorecord kommer aldri hit. Det opprettes ikke bonglinjer på dem. */

  ASSIGN
      piBongLinje = 0
      plSubTotRab = 0
      .
  BONGLINJE:
  DO WHILE TRUE:
    FIND NEXT BongLinje EXCLUSIVE-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id NO-ERROR.
    IF NOT AVAILABLE BongLinje THEN
        LEAVE BONGLINJE.

    /* Konverterer fra SkoTex POS 1.0 kode til SkoTex BackOffice kode. */
    ASSIGN
      cPOS        = "0" + SUBSTRING(BongLinje.OriginalData,21,2)
      piBongLinje = piBongLinje
      .

    /* PinPoint gir noen ganger "40" istedenfor "00" og "42" istedenfor "02". */
    /* Her konverteres disse un|yaktigheter midlertidig. Dette skall rettes   */
    /* opp i kassaprogrammet.                                                 */
    if cPOS = "040" then cPOS = "000".
    if cPOS = "042" then cPOS = "002".

    /* Konverterer transaksjonskoden */
    ASSIGN
      iEntry     = LOOKUP(cPOS,cPOSKoder)
      cTTId      = "000"
      .
    IF iEntry = 0 THEN
        cTTId = "000".
    ELSE
        ASSIGN
          cTTId = ENTRY(iEntry,cTTIdKoder)
        NO-ERROR.

    {xkonvkvitteringpos.i}

    /* Nullstiller variablene */
    ASSIGN
    iReturButikk      = 0   iHgr               = 0
    iKontoNr          = 0   cHgrNavn           = ""
    cKortNr           = ""  iReturKassererNr   = 0
    iTid              = 0   cReturKassererNAvn = ""
    cTid              = "" 
    iMottagendeButikk = 0
    cKundeKort        = ""
    cMedlemsKort      = ""
    dKundeNr          = 0
    dMedlemsNr        = 0
    cKundeNavn        = ""
    cMedlemsNavn      = ""
    cKassererNavn     = ""
    iKassererNr       = 0
    iVg               = 0
    iLopeNr           = 0
    cStorl            = ""
    iAntall           = 0
    bRabatt           = 0
    bRabatt           = 0
    bBelop            = 0
    bBelop            = 0
    dVVareKost        = 0
    iNotatKode        = 0
    iFeilKode         = 0
    cFeilKodeTekst    = ""
    cNotatKodeTekst   = ""
    iKortType         = 1
    .

    /* Variablene ButikkNr, GruppeNr, KasseNr, Dato, tidligere satt i datasett posten.   */ 
    /* BongNr er satt i bonghode og bonglinje posten ved oppdatering.                    */
    /* Ved behandling av bongen, benyttes de verdiene som er satt i datasettet m.m.      */
     /* Setter variabler for varelinje.                                                  */
    IF CAN-DO(cKodeVarelinje,cPOS) THEN
    VARELINJE:
    DO:
      ASSIGN
        iKassererNr       = int(SUBSTRING(BongLinje.OriginalData,9,4))
        iVg               = int(SUBSTRING(BongLinje.OriginalData,23,4)) 
        iLopeNr           = int(SUBSTRING(BongLinje.OriginalData,27,4)) 
        cStorl            = SUBSTRING(BongLinje.OriginalData,31,4) 
        iAntall           = INT(SUBSTRING(BongLinje.OriginalData,35,2))
        bRabatt           = (dec(SUBSTRING(BongLinje.OriginalData,37,8)) / 100) * iAntall
        bRabatt           = IF bRabatt = ? THEN 0 ELSE bRabatt
        bBelop            = (dec(SUBSTRING(BongLinje.OriginalData,13,8)) / 100) * iAntall
        bBelop            = IF bBelop  = ? THEN 0 ELSE bBelop
        /*bBelop            = bBelop - bRabatt /* Trekker fra linjerabatt. Subtotalrabatt trekkes fra separat. */*/
        iFeilKode         = int(SUBSTRING(BongLinje.OriginalData,45,2))
        iNotatKode        = int(SUBSTRING(BongLinje.OriginalData,47,2))
        .

      /* Konverterer størrelsen */
      RUN FixStorl (INPUT-OUTPUT cStorl).

      /* Legger på referansenr og tekst */
      /* Leveres ikke fra gammel kasse. */
      ASSIGN
          BongLinje.RefNr    = 0
          BongLinje.RefTekst = ""
          .

      /* Henter mottagende butikk.                     */
      IF cPOS = "017" THEN
      MBUTIKK:
      DO:
        ASSIGN
          prRowId = ROWID(BongLinje)
          .
        FIND NEXT BongLinje EXCLUSIVE-LOCK where
            BongLinje.B_Id = BongHode.B_Id NO-ERROR.

        IF NOT AVAILABLE BongLinje THEN
        DO:
          ASSIGN
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE chr(10)) + 
                     "** Trans med mottakende butikk for overføring mangler. (" + cPOS + 
                     ") BongNr: " + string(BongHode.BongNr) + " LinjeNr: " + string(piBongLinje) + " ikke oppdatert."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - Trans med mottakende butikk for overføring mangler. (" + cPOS + 
                     ") BongNr: " + string(BongHode.BongNr) + " LinjeNr: " + string(piBongLinje) + " ikke oppdatert." + CHR(1) + "3"
            BongHode.Gradering = IF BongHode.Gradering < 3 THEN 3 ELSE BongHode.Gradering
            .
          /* Stiller peker tilbake */
          FIND BongLinje EXCLUSIVE-LOCK WHERE ROWID(BongLinje) = prRowId.
          LEAVE MBUTIKK.
        END.

        /* Sjekker at den neste transen har transkode 17 og at det er en info trans. */
        IF cPOS = ("0" + substring(BongLinje.OriginalData,21,2)) and 
           substring(BongLinje.OriginalData,21,9) = "170000000" THEN. /* GJØR INGENTING */
        /* IKKE FUNNET !!! */
        ELSE
        DO:
          ASSIGN
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE chr(10)) + 
                     "** InfoTrans med mottakende butikk for overføring mangler. (" + cPOS + 
                     ") BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + " ikke oppdatert."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - InfoTrans med mottakende butikk for overføring mangler. (" + cPOS + 
                     ") BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + " ikke oppdatert." + CHR(1) + "3"
            BongHode.Gradering = IF BongHode.Gradering < 3 THEN 3 ELSE BongHode.Gradering
            .
          /* Stiller peker tilbake */
          FIND BongLinje EXCLUSIVE-LOCK WHERE ROWID(BongLinje) = prRowId.
          LEAVE MBUTIKK.
        END.

        /* Setter mottagende butikk.                                      */
        /* Bonglinjen skal også slettes, og peker settes tilbake.         */
        /* Det er her IKKE tatt høyde for feilaktige transer fra PinPoint */
        ASSIGN
          iMottagendeButikk = (IF bPinPoint
                                 THEN int(SUBSTRING(BongLinje.OriginalData,31,3))
                                 ELSE int(SUBSTRING(BongLinje.OriginalData,31,4)))
          cOriginalData     = BongLinje.OriginalData
          .
        FIND CURRENT BongLinje EXCLUSIVE-LOCK.
        DELETE BongLinje.
        /* Stiller peker tilbake */
        FIND BongLinje EXCLUSIVE-LOCK WHERE   
            ROWID(BongLinje) = prRowId.
        /* Legger på data fra trans 2 */
        ASSIGN BongLinje.OriginalData = BongLinje.OriginalData + ", " + cOriginalData.

        /* Kontrollerer gyldig mottagende butikk */
        IF NOT CAN-FIND(Butiker WHERE
                        Butiker.Butik = iMottagendeButikk) THEN
        DO:
          ASSIGN
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE chr(10)) + 
                     "** Ukjent butikknummer satt inn som mottagende butikk. (" + string(iMottagendeButikk) + 
                     ") BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + " ikke oppdatert."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - Ukjent butikknummer satt inn som mottagende butikk. (" + string(iMottagendeButikk) + 
                     ") BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + " ikke oppdatert." + CHR(1) + "3"
              BongHode.Gradering = IF BongHode.Gradering < 3 THEN 3 ELSE BongHode.Gradering
              .
        END.
      END. /* MBUTIKK - Hånterer transkode 17 */

      /* Kundereklamasjon kommer som en 002 direkte etterfulgt av en 16. */
      /* Kommer det et slikt par, skal cTTId konverteres til "003" og    */
      /* 16 transaksjonen skal slettes.                                  */
      /* Kommer det ikke en 16 trans, er det en retur og                 */
      /* cTTId konverteres til "010".                                    */
      IF cPOS = "002" THEN
      KUNDEREKLAMASJON:
      DO:
        ASSIGN prRowId = ROWID(BongLinje).
        FIND NEXT BongLinje EXCLUSIVE-LOCK where
            BongLinje.B_Id = BongHode.B_Id NO-ERROR.

        /* Det er IKKE kundereklamasjon */
        IF NOT AVAILABLE BongLinje THEN
        DO:
            ASSIGN
                cTTId = "010"
                .
            FIND BongLinje EXCLUSIVE-LOCK WHERE ROWID(BongLinje) = prRowId.
        END.
        /* Sjekker etter 16 */
        ELSE DO:
          IF ("0" + substring(BongLinje.OriginalData,21,2)) = "016" THEN
          DO:
              ASSIGN
                  cTTId = "003"
                  cOriginalData     = BongLinje.OriginalData.
              DELETE BongLinje. /* Kaster 16 */
              FIND BongLinje EXCLUSIVE-LOCK WHERE
                   ROWID(BongLinje) = prRowId.
              /* Legger på data fra trans 2 */
              ASSIGN BongLinje.OriginalData = BongLinje.OriginalData + ", " + cOriginalData.
          END.
          else do:
              ASSIGN
                  cTTId = "010"
                  .
              FIND BongLinje EXCLUSIVE-LOCK WHERE ROWID(BongLinje) = prRowId.
          end.
        END.
      END. /* KUNDEREKLAMASJON */
    END. /* VARELINJE */

    /* Setter variabler for betalingstrans og diversetranser. */
    ELSE IF CAN-DO(cKodeBetTrans,cPOS) THEN
    BETALINGSTRANS:
    DO:
      ASSIGN
        iKassererNr       = int(SUBSTRING(BongLinje.OriginalData,9,4))
        iKontoNr          = IF can-do("006,008",cPOS)
                              THEN int(SUBSTRING(BongLinje.OriginalData,31,4))
                              ELSE 0
        cKortNr           = (IF can-do("020,080,081,082",cPOS)
                               THEN ""
                               ELSE LEFT-TRIM(SUBSTRING(BongLinje.OriginalData,43,6),"0")) 
        iTid              = (integer(substring(BongLinje.OriginalData,23,2)) * 3600) +
                                    (integer(substring(BongLinje.OriginalData,25,2)) * 60) +
                                    integer(substring(BongLinje.OriginalData,27,2)) 
        cTid              = STRING(iTid,"HH:MM:SS") 
        bRabatt           = dec(SUBSTRING(BongLinje.OriginalData,37,8)) / 100
        bRabatt           = IF bRabatt = ? THEN 0 ELSE bRabatt
        iAntall           = 0
        bBelop            = (dec(SUBSTRING(BongLinje.OriginalData,13,8)) / 100)
        bBelop            = IF bBelop  = ? THEN 0 ELSE bBelop
        plSubTotRab       = plSubTotRab + (IF CAN-DO("020",cPOS) THEN bBelop ELSE 0).

      IF cPOS = "009" THEN
      VEKSEL-BUG:
      DO:
          ASSIGN prRowId = ROWID(BongLinje) .
          FIND NEXT BongLinje EXCLUSIVE-LOCK WHERE
              BongLinje.B_Id = BongHode.B_Id NO-ERROR.
   
          /* Det er IKKE kundereklamasjon */
          IF NOT AVAILABLE BongLinje THEN
              FIND BongLinje EXCLUSIVE-LOCK WHERE
                   ROWID(BongLinje) = prRowId.
          /* Sjekker etter 09 */
          ELSE DO:
              IF ("0" + substring(BongLinje.OriginalData,21,2)) = "009" THEN
              DO:
                  ASSIGN
                      cOriginalData     = BongLinje.OriginalData
                      .
                  DELETE BongLinje. /* Kaster 09 */
                  FIND BongLinje EXCLUSIVE-LOCK WHERE
                       ROWID(BongLinje) = prRowId.
                  /* Legger på data fra trans 2 */
                  ASSIGN
                      BongLinje.OriginalData = BongLinje.OriginalData + ", " +
                                               cOriginalData.
              END.
              else /* Henter foregående */
                  FIND BongLinje EXCLUSIVE-LOCK WHERE ROWID(BongLinje) = prRowId.
          END.
      END. /* VEKSEL-BUG */
    END. /* BETALINGSTRANS */

    /* Setter variabler for tidslogging. */
    ELSE IF CAN-DO("021,022",cPOS) THEN
    DO:
      ASSIGN
        iKassererNr       = int(SUBSTRING(BongLinje.OriginalData,9,4))
        iTid              = (integer(substring(BongLinje.OriginalData,23,2)) * 3600) +
                                    (integer(substring(BongLinje.OriginalData,25,2)) * 60) +
                                    integer(substring(BongLinje.OriginalData,27,2)) 
        cTid              = STRING(iTid,"HH:MM:SS").
    END.

    /* Koden er ikke satt opp riktig for behandling i programmet. */
    ELSE DO:
      ASSIGN
        cError = cError + 
                 (IF cError = ""
                    THEN ""
                    ELSE chr(10)) + 
                 "** Koden er kjent, men ikke spesifisert som varelinje eller betaling. (" + cPOS + 
                 ") BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + " ikke oppdatert."
        cFilError = cFilError + 
                 (IF cFilError = ""
                    THEN ""
                    ELSE "|") + 
                 " - Koden er kjent, men ikke spesifisert som varelinje eller betaling. (" + cPOS + 
                 ") BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + " ikke oppdatert." + CHR(1) + "3"
          BongHode.Gradering = IF BongHode.Gradering < 3 THEN 3 ELSE BongHode.Gradering
          .
      NEXT BONGLINJE. 
    END.

    /* Valideringer som gjøres av alle linjer på kvitteringen. */
    RUN ValiderAlle.

    /* Kontrollerer varelinjer. Setter på eventuelle feilmeldinger. */
    /* Initierer også endel variabler.                              */
    IF INT(cTTId) < 50 THEN
    DO:
      RUN ValiderVareLinje.
      ASSIGN
        BongLinje.Mva%           = dMva%                BongLinje.HovedGr            = iHgr     
        BongLinje.VareGruppeNavn = cVareGruppeNavn      BongLinje.HovedGrBeskrivelse = cHgrNavn 
        BongLinje.MvaGruppeNavn  = cMvaGruppeNavn
        BongLinje.BongTekst      = cBongTekst
        BongLinje.ArtikkelNr     = string(dArtikkelNr,">>>>>>>>>>>>9").
      /* Retur fra annen butikk ? */
      IF CAN-DO("001,002,016",cPOS) THEN
          RUN ReturAnnenButikk.
      ASSIGN
          BongLinje.Returbutikk       = iReturButikk
          BongLinje.ReturKasserer     = iReturKassererNr
          BongLinje.ReturKassererNavn = cReturKassererNavn
          .
      /* Setter VVareKost */
      IF NOT CAN-DO("017",cPOS) THEN
      DO:
          RUN SetVVareKost (OUTPUT dVVareKost).
          ASSIGN
              BongLinje.VVareKost = dVVareKost * BongLinje.Antall.
      END.
    END.
    
    /* Validerer KortNummer.                                        */
    /* Denne valideringen overstyrer validering av kontonummer.     */
    /* Her settes også variabler for kortnummer, medlem og kontonr. */
    IF (cKortNr <> "" AND CAN-DO(cKodeBetTrans,cPOS)) THEN
        RUN ValiderKortNr.

    /* Validering kontonummer */
    ELSE IF (iKontoNr <> 0 AND CAN-DO(cKodeBetTrans,cPOS)) THEN
        RUN ValiderKontoNr.

        /* Setter verdier i BongHode */
    /* NB: Kortnummer skal pakkes opp og medlems og kundenummer settes. */
    /* NB: Vanlig kontosalg, initierer ikontonr.                        */
    ASSIGN
    BongHode.KassererNr   = iKassererNr
    BongHode.KundeNr      = dKundeNr
    BongHode.KundeNavn    = cKundeNavn
    BongHode.MedlemsNr    = dMedlemsNr
    BongHode.MedlemNavn   = cMedlemsNavn
    BongHode.KundeKort    = IF iKortType = 2
                              THEN cKortNr
                              ELSE ""
    BongHode.MedlemsKort  = IF iKortType = 3
                              THEN cKortNr
                              ELSE ""
    BongHode.KassererNavn = cKassererNavn
    BongHode.KortType     = iKortType.

    /* Konverterer bonglinjen */
    IF CAN-DO("004,005,006,007,008,009,010,011,012,018,019,020,080,081,082",cPOS) THEN
              RUN KonvLinje004 IN THIS-PROCEDURE NO-ERROR.
    ELSE
        RUN value("KonvLinje" + cPOS) IN THIS-PROCEDURE NO-ERROR.
  END. /* BONGLINJE */

  /*
     ** Fordeler subtotalrabatt **
     Subtotalrabatt kan blandes med vanlig varesalg.
     Bongsum er bongens sum før subtotalrabatt.
     -----------------------------------------------------------------
     1. Bare varesalgstranser. Fordeling skjer direkte forholdsmessig.
     2. Bare returtranser.     --- "" --- 
     3. Vare og returtranser. Bongsum blir positiv.
     4. Vare og returtranser. Bongsum blir negativ.
  */
  IF plSubTotRab <> 0 THEN 
      RUN FordelSubTotRabatt (plSubTotRab).

  /* Beregner MvaKr: */
  FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id:
    IF CAN-DO("0,1,2,3,16",string(int(substring(BongLinje.Originaldata,21,2)))) THEN
      ASSIGN BongLinje.MvaKr = Mva2(BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab).
  END.

  /* Setter tidsstempel på bonglinjene */
  RUN SettTidsstempel.

  /* Setter fortegn på bongens linjer. */
  RUN SettFortegn.

  /* Kontroll av bongens saldo. */
  RUN SjekkSaldo.

  /* Kontrollerer om det finnes betalingstransaksjoner på bongen */
  RUN SjekkBetTrans.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KonvLinje000) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KonvLinje000 Procedure 
PROCEDURE KonvLinje000 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  ASSIGN
    BongLinje.TTId           = int(cTTId)
    BongLinje.TBId           = 1
    BongLinje.TransDato      = BongLinje.Dato
    BongLinje.TransTid       = 0
    BongLinje.VareGr         = iVg
    BongLinje.LopeNr         = iLopeNr
    BongLinje.ArtikkelNr     = string(dArtikkelNr)
    BongLinje.Storrelse      = cStorl
    BongLinje.Antall         = iAntall
    BongLinje.LinjeRab       = bRabatt
    BongLinje.LinjeSum       = bBelop
    BongLinje.BongPris       = bBelop
    BongLinje.FeilKode       = iFeilKode
    BongLinje.NotatKode      = iNotatKode
    BongLinje.FeilKodeTekst  = cFeilKodeTekst
    BongLinje.NotatKodeTekst = cNotatKodeTekst
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KonvLinje001) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KonvLinje001 Procedure 
PROCEDURE KonvLinje001 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bufBongLinje FOR BongLinje.

  ASSIGN
    BongLinje.TTId           = int(cTTId)
    BongLinje.TBId           = 1
    BongLinje.TransDato      = BongLinje.Dato
    BongLinje.TransTid       = 0
    BongLinje.VareGr         = iVg
    BongLinje.LopeNr         = iLopeNr
    BongLinje.ArtikkelNr     = string(dArtikkelNr)
    BongLinje.Storrelse      = cStorl
    BongLinje.Antall         = iAntall
    BongLinje.LinjeRab       = bRabatt
    BongLinje.LinjeSum       = bBelop
    BongLinje.BongPris       = bBelop
    BongLinje.Makulert       = TRUE
    .

  FIND FIRST bufBongLinje EXCLUSIVE-LOCK WHERE
      bufBongLinje.ButikkNr  = BongLinje.ButikkNr      AND
      bufBongLinje.GruppeNr  = BongLinje.GruppeNr      AND
      bufBongLinje.KasseNr   = BongLinje.KasseNr       AND
      BufBongLinje.Dato      = BongLinje.Dato          AND
      bufBongLinje.BongNr    = BongLinje.BongNr        AND
      bufBongLinje.VareGr    = BongLinje.VareGr        AND
      bufBongLinje.LopeNr    = BongLinje.LopeNr        AND
      bufBongLinje.Storrelse = BongLinje.Storrelse     AND
      bufBongLinje.LinjeRab  = abs(BongLinje.LinjeRab) AND
      bufBongLinje.LinjeSum  = abs(BongLinje.LinjeSum) AND
      bufBongLinje.Antall    = abs(BongLinje.Antall)   AND
      bufBongLinje.Makulert  = FALSE                   AND
      ROWID(bufBongLinje)    <> ROWID(BongLinje) NO-ERROR.
  IF AVAILABLE bufBongLinje THEN
  DO:
      ASSIGN
          bufBongLinje.Makulert = TRUE
          .
      RELEASE bufBongLinje.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KonvLinje002) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KonvLinje002 Procedure 
PROCEDURE KonvLinje002 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  Retur
  Notes:       
------------------------------------------------------------------------------*/
  
  ASSIGN
    BongLinje.TTId           = int(cTTId)
    BongLinje.TBId           = 1
    BongLinje.TransDato      = BongLinje.Dato
    BongLinje.TransTid       = 0
    BongLinje.VareGr         = iVg
    BongLinje.LopeNr         = iLopeNr
    BongLinje.ArtikkelNr     = string(dArtikkelNr)
    BongLinje.Storrelse      = cStorl
    BongLinje.Antall         = iAntall
    BongLinje.LinjeRab       = bRabatt
    BongLinje.LinjeSum       = bBelop
    BongLinje.BongPris       = bBelop
    BongLinje.FeilKode       = iFeilKode
    BongLinje.NotatKode      = iNotatKode
    BongLinje.FeilKodeTekst  = cFeilKodeTekst
    BongLinje.NotatKodeTekst = cNotatKodeTekst
    .
   /* Trekker ut opprinnelig butikk og validerer denne */
   IF plJf THEN
   DO:
       IF BongHode.KassererNr >= 100 AND 
           BongHode.KassererNr <= 999 THEN
           BongLinje.MButikkNr = int(SUBstring(STRING(BongHode.KassererNr),1,1)).
       IF NOT CAN-FIND(Butiker WHERE
                       Butiker.Butik = BongLinje.MButikkNr) THEN
           BongLinje.MButikkNr = 0.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KonvLinje004) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KonvLinje004 Procedure 
PROCEDURE KonvLinje004 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN
        BongLinje.TTId       = int(cTTId)
        BongLinje.TBId       = 1
        BongLinje.TransDato  = BongLinje.Dato
        BongLinje.TransTid   = iTid
        BongLinje.LinjeRab   = 0
        BongLinje.LinjeSum   = bBelop
        BongLinje.BongPris   = bBelop
        BongLinje.MvaKr      = 0
        BongLinje.Antall     = iAntall
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KonvLinje016) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KonvLinje016 Procedure 
PROCEDURE KonvLinje016 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  ASSIGN
    BongLinje.TTId           = int(cTTId)
    BongLinje.TBId           = 1
    BongLinje.TransDato      = BongLinje.Dato
    BongLinje.TransTid       = 0
    BongLinje.VareGr         = iVg
    BongLinje.LopeNr         = iLopeNr
    BongLinje.ArtikkelNr     = string(dArtikkelNr)
    BongLinje.Storrelse      = cStorl
    BongLinje.Antall         = iAntall
    BongLinje.LinjeRab       = bRabatt
    BongLinje.LinjeSum       = bBelop
    BongLinje.BongPris       = bBelop
    BongLinje.FeilKode       = iFeilKode
    BongLinje.NotatKode      = iNotatKode
    BongLinje.FeilKodeTekst  = cFeilKodeTekst
    BongLinje.NotatKodeTekst = cNotatKodeTekst
    .
  /* Trekker ut opprinnelig butikk og validerer denne */
  IF plJf THEN
  DO:
      IF BongHode.KassererNr >= 100 AND 
          BongHode.KassererNr <= 999 THEN
          BongLinje.MButikkNr = int(SUBstring(STRING(BongHode.KassererNr),1,1)).
      IF NOT CAN-FIND(Butiker WHERE
                      Butiker.Butik = BongLinje.MButikkNr) THEN
          BongLinje.MButikkNr = 0.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KonvLinje017) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KonvLinje017 Procedure 
PROCEDURE KonvLinje017 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Henter varekost i butikken det overføres fra. */      
  /* Dette er pris eExMva.                         */
  FIND Lager NO-LOCK WHERE
      Lager.ArtikkelNr = dArtikkelNr AND
      Lager.Butik      = BongLinje.Butik NO-ERROR.
  IF AVAILABLE Lager THEN
  DO:
      ASSIGN
      dVVarekost = Lager.VVareKost * iAntall
      dMvaKr     = TRUNCATE(((dVVareKost * dMva%) / 100), 2)
      dVVareKost = dVVareKost + dMvaKr
      .
  END.

  ASSIGN
    BongLinje.TTId           = int(cTTId)
    BongLinje.TBId           = 1
    BongLinje.TransDato      = BongLinje.Dato
    BongLinje.TransTid       = 0
    BongLinje.VareGr         = iVg
    BongLinje.LopeNr         = iLopeNr
    BongLinje.ArtikkelNr     = string(dArtikkelNr)
    BongLinje.Storrelse      = cStorl
    BongLinje.Antall         = iAntall
    BongLinje.MButikkNr      = iMottagendeButikk
    BongLinje.LinjeRab       = 0
    BongLinje.LinjeSum       = bBelop /* dVVareKost */
    BongLinje.BongPris       = bBelop /* dVVareKost */
    BongLinje.MvaKr          = 0 /*dMvaKr*/
    .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ReturAnnenButikk) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReturAnnenButikk Procedure 
PROCEDURE ReturAnnenButikk :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 IF iKassererNr > 9999 THEN
     RETURN.
 
 ASSIGN
     iReturKassererNr = iKassererNr
     iReturButikk     = int(SUBstring(STRING(iKassererNr,"9999"),1,2))
     .
 FIND Forsalj NO-LOCK WHERE
     Forsalj.ForsNr = iReturKassererNr NO-ERROR.
 IF AVAILABLE Forsalj THEN
 DO:
     ASSIGN
         cReturKassererNavn = Forsalj.FoNamn
         .
 END.
 ELSE
     ASSIGN
         cReturKassererNavn = "*Ukjent*"
         .

 /*
 FIND Butiker NO-LOCK WHERE
     Butiker.Butik = iReturButikk NO-ERROR.
 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SettFortegn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettFortegn Procedure 
PROCEDURE SettFortegn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR plSum AS DEC NO-UNDO.

  ASSIGN
      plSum = 0
      .
  /*
  NB? - Hva med deponering??????
  */
  
  /* Snur fortegn på de linjene som skal være negative.                        */                                                        
  /* Bongen lagres med fortegn. På varelinjer settes fortegnet i beløpsfeltet. */
  /* På betalingstransaksjoner, settes det direkte i beløpsfeltet.             */
  FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id:
      /* Snur fortegn på Reklamasjon, Retur og Makulering. */
      /* NB: 04 trans blir slettet. Den er bare til info.  */
      IF CAN-DO("003,010,012",STRING(BongLinje.TTId,"999")) THEN
          ASSIGN
          BongLinje.Antall = BongLinje.Antall * -1
          .
      /* Snur ALLTID fortegn på Vekseltransaksjonen */
      IF CAN-DO("070",STRING(BongLinje.TTId,"999")) THEN
          ASSIGN
          BongLinje.LinjeSum = BongLinje.LinjeSum * -1
          .
      /* Sumerer opp varelinjer for å finne beløp som skal dekkes */
      /* av de resterende betalingstransaksjonene.                */
      IF CAN-DO("001,003,010,012",STRING(BongLinje.TTId,"999")) THEN
          plSum = plSum + (IF BongLinje.Antall < 0
                             THEN ((BongLinje.LinjeSum - BongLinje.LinjeRab) * -1)
                             ELSE (BongLinje.LinjeSum - BongLinje.LinjeRab))
          .
  END.

  /* Hvis summen er større eller lik 0, skal ingenting gjøres. Bongen er korrekt.     */
  /* Er summen mindre enn 0, skal fortegnet snus på de andre betalingstransaksjonene. */
  IF plSum < 0 THEN
  FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id:
      /* Snur fortegn på betalingstransaksjonene.                  */
      /* Snur veksel også her, da denne skal ha motsatt fortegn av */
      /* betalingstransaksjonene.                                  */
      IF CAN-DO("050,052,054,056,063,065,066,070,071",STRING(BongLinje.TTId,"999")) THEN
          ASSIGN
          BongLinje.LinjeSum = BongLinje.LinjeSum * -1
          .
  END.
  ELSE
  FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id:
      IF CAN-DO("630",STRING(BongLinje.TTId,"999")) THEN
          ASSIGN
          BongLinje.LinjeSum = BongLinje.LinjeSum * -1
          .

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SettTidsstempel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SettTidsstempel Procedure 
PROCEDURE SettTidsstempel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piTid AS INT NO-UNDO.

  /* Summerer varesalg. */
  SUMMER:
  FOR EACH BongLinje NO-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id:
      ASSIGN
          piTid = BongLinje.TransTid
          .
      IF piTid <> 0 THEN
          LEAVE SUMMER.
  END. /* SUMMER */
  FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id:
      ASSIGN
          BongLinje.TransTid = piTid
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetVVareKost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SetVVareKost Procedure 
PROCEDURE SetVVareKost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAMETER pdVVareKost LIKE BongLinje.VVareKost NO-UNDO. 

  /* Henter varekost i butikken det overføres fra. */      
  /* Dette er pris eExMva.                         */
  FIND Lager NO-LOCK WHERE
      Lager.ArtikkelNr = dArtikkelNr AND
      Lager.Butik      = BongLinje.Butik NO-ERROR.
  IF AVAILABLE Lager THEN
      pdVVarekost = Lager.VVareKost.
  ELSE 
      pdVVareKost = 0.

  /* Sjekker om varekost er satt.                                       */
  /* Er det ikke satt noen varekost, merkes transaksjonen med feilkode. */
  if pdVVareKost = 0 then /* or wBrutto% *** Skal også utføres for brutto% artikkler */
    DO:
      ASSIGN
          /* Omsetning eks. mva */
          pdVVareKost = (IF BongLinje.Antall >= 0
                          THEN (BongLinje.LinjeSum - 
                                (BongLinje.LinjeRab + BongLinje.SubtotalRab) - 
                                BongLinje.MvaKr)
                          ELSE (BongLinje.LinjeSum - 
                                (BongLinje.LinjeRab + BongLinje.SubtotalRab) - 
                                BongLinje.MvaKr) * -1)
          pdVVareKost = pdVVareKost / ABS(BongLinje.Antall)
          .
      if VALID-HANDLE(h_PrisKo) then
        RUN HentVareKost in h_PrisKo (INPUT  BongLinje.ArtikkelNr, 
                                      input  BongLinje.Butik, 
                                      INPUT  pdVVareKost, 
                                      output pdVVareKost).
    END.
  IF pdVVareKost = ? THEN
      pdVVareKost = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkBetTrans) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkBetTrans Procedure 
PROCEDURE SjekkBetTrans :
/*------------------------------------------------------------------------------
  Purpose:     Kontrollerer om en bong har betalingstransaksjoner.
  Parameters:  <none>
  Notes:       Bonger som ikke har betalingstransaksjoner, skal markeres
               som feilbonger. De skal ikke overføres til SkoTex.
  
------------------------------------------------------------------------------*/
  
  DEF VAR pcTransKode AS CHAR NO-UNDO.      
  DEF VAR pbOk        AS LOG  NO-UNDO.
  DEF VAR pcListe     AS CHAR NO-UNDO.

  ASSIGN
      pcTransKode = cKodeBetTrans + ",017"
      pbOk        = FALSE
      pcListe     = ""
      .
                                                                 
  BONG:
  FOR EACH BongLinje NO-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id:
    IF NOT CAN-DO(pcListe,"0" + SUBSTRING(BongLinje.OriginalData,21,2)) THEN
        pcListe = pcListe + 
                  (IF pcListe = ""
                     THEN ""
                     ELSE ",") + "0" +  
                  SUBSTRING(BongLinje.OriginalData,21,2).
                   
    IF CAN-DO(pcTransKode,"0" + SUBSTRING(BongLinje.OriginalData,21,2)) THEN
    DO:
        ASSIGN
            pbOk = TRUE
            .
        LEAVE BONG.
    END.
  END. /* BONG */

  /* Logger hvis bongen ikke har betalingstransaksjoner.              */
  /* Det skal ikke logges for lagerreklamasjoner.                     */
  /* En lagerreklamasjon bong inneholder ikke betalingstransaksjoner. */
  IF pbOk = FALSE AND pcListe <> "016" AND BongHode.BongNr <> 0 THEN
  DO:
      ASSIGN
      BongHode.Gradering = IF BongHode.Gradering < 3 THEN 3 ELSE BongHode.Gradering
      cFilError = cFilError + 
          (IF cFilError = ""
             THEN ""
             ELSE "|") + 
          " - Bongen inneholder ikke betalingstransaksjoner." + 
          " (But/Kas/Dato/BongNr: " + 
          STRING(BongHode.ButikkNr) + "/" + 
          STRING(BongHode.KasseNr) + "/" + 
          STRING(BongHode.Dato) + "/" + 
          STRING(BongHode.BongNr) +  ")." + CHR(1) + "3"
      BongHode.Logg = BongHode.Logg + 
                         (IF BongHode.Logg = ""
                            THEN ""
                            ELSE CHR(10)) + 
                         "Bongen inneholder ikke betalingstransaksjoner."
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkSaldo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkSaldo Procedure 
PROCEDURE SjekkSaldo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR plVareSum     AS DEC   NO-UNDO.
  DEF VAR plBetSum      AS DEC   NO-UNDO.

  /* Sjekker at bongens sum er = 0. */
  ASSIGN plVareSum = 0
         plBetSum  = 0.
  /* NB: Overføringer kontrolleres ikke. */
  FOR EACH BongLinje NO-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id:
    /* Sum varetranser (NB: 04 skal ikke telles med her.) */
    IF CAN-DO("001,003,010,012",string(BongLinje.TTId,"999")) THEN
    DO:
        ASSIGN
          plVareSum = plVareSum + 
                      ((BongLinje.LinjeSum - 
                        BongLinje.LinjeRab -
                        BongLinje.SubTotalRab) * (IF BongLinje.Antall < 0
                                                                      THEN -1 
                                                                      ELSE 1))
          .
    END.

    /* Betalingstransaksjoner.                                */
    /* Subtotalrabatt er trukket fra fra før og skal ikke tas */
    /* med her.                                               */
    ELSE IF CAN-DO("050,052,054,056,065,066,070,071,072,073",string(BongLinje.TTId,"999")) THEN
    DO:
        /* Varelinjer + deponering */
        IF CAN-DO("050,052,054,056,065,066,070,071,072",string(BongLinje.TTId,"999")) THEN
        ASSIGN
          plBetSum = plBetSum + BongLinje.LinjeSum
          .
        /* Betaling av deponering og veksel */
        ELSE IF CAN-DO("073",string(BongLinje.TTId,"999")) THEN
        ASSIGN
          plBetSum = plBetSum - BongLinje.LinjeSum
          .
        
    END.
  END.
  /* Logger hvis bongsummen ikke er lik 0. */
  IF (plVareSum - plBetSum <> 0) AND
     bLoggSaldoFeil = TRUE THEN
  DO:
      /* Vi ignorerer øresavrunding - dvs beløp mindre enn 0.49 øre. */
      IF ABSOLUTE(plVareSum - plBetSum) > 0.49 THEN
      ASSIGN
      cFilError = cFilError + 
          (IF cFilError = ""
             THEN ""
             ELSE "|") + 
          " - Bongen's sum <> 0" + 
          " Diff: " + STRING(plVareSum,"->>>,>>>,>>9.99") + " - " + 
          STRING(plBetSum,"->>>,>>>,>>9.99") + " = " + 
          STRING(plVareSum - plBetSum,"->>>,>>>,>>9.99") + "." + 
          " (But/Kas/Dato/BongNr: " + 
          STRING(BongHode.ButikkNr) + "/" + 
          STRING(BongHode.KasseNr) + "/" + 
          STRING(BongHode.Dato) + "/" + 
          STRING(BongHode.BongNr) +  ")." + CHR(1) + "3"
      .
  END.

  /* Oppdaterer bonghode med beløp. */
  ASSIGN BongHode.Belop = plBetSum.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValiderAlle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderAlle Procedure 
PROCEDURE ValiderAlle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Gyldig kasserernr */
  IF NOT CAN-FIND(Forsalj WHERE
                  Forsalj.ForsNr = iKassererNr) THEN
    ASSIGN  
      cError = cError + 
               (IF cError = ""
                  THEN ""
                  ELSE chr(10)) + 
               "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
               " Ukjent kasserer på transaksjonen " + STRING(iKassererNr) + 
               "." 
      cFilError = cFilError + 
               (IF cFilError = ""
                  THEN ""
                  ELSE "|") + 
               " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
               " Ukjent kasserer på transaksjonen " + STRING(iKassererNr) + 
               "." + CHR(1) + "2"
      cKassererNavn = "*Ukjent*"
      BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
      .
  /* Setter kassererinfo. */
  ELSE DO:
      FIND Forsalj NO-LOCK WHERE
          Forsalj.ForsNr = iKassererNr.
      ASSIGN
          cKassererNavn  = Forsalj.FoNamn
          .
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValiderKontoNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderKontoNr Procedure 
PROCEDURE ValiderKontoNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Kontrollerer gyldig kontonummer. */                
  IF NOT CAN-FIND(Kunde WHERE
                  Kunde.KundeNr  = dec(SUBSTRING(BongLinje.OriginalData,1,2))) THEN
    ASSIGN  
      cError = cError + 
               (IF cError = ""
                  THEN ""
                  ELSE chr(10)) + 
               "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
               " Ukjent kontonummer på transaksjonen " + SUBSTRING(BongLinje.OriginalData,31,4) + 
               "."
      cFilError = cFilError + 
               (IF cFilError = ""
                  THEN ""
                  ELSE "|") + 
               " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
               " Ukjent kontonummer på transaksjonen " + SUBSTRING(BongLinje.OriginalData,31,4) + 
               "." + CHR(1) + "2"
      BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
      .
  ELSE DO:
      FIND Kunde NO-LOCK WHERE
          Kunde.KundeNr = dec(SUBSTRING(BongLinje.OriginalData,1,2))
          NO-ERROR.
      ASSIGN
          cKundeNavn = Kunde.Navn
          dKundeNr   = Kunde.KundeNr
          iKortType  = 0
          .
  END.

   

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValiderKortNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderKortNr Procedure 
PROCEDURE ValiderKortNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      iKortType = 0
      .
        
  /* Påfører KortType, Medlemsnummer og kundenummer. */
  RUN sjekkmedlem_polygon.p (INPUT  cKortNr,
                     INPUT  BongHode.ButikkNr,
                     INPUT  BongHode.KasseNr,
                     INPUT  BongHode.Dato,
                     OUTPUT iKortType,
                     OUTPUT dMedlemsNr,
                     OUTPUT cMedlemsNavn,
                     OUTPUT dKundeNr,
                     OUTPUT cKundeNavn).


  IF iKortType = 0 THEN
      ASSIGN  
        cError = cError + 
                 (IF cError = ""
                    THEN ""
                    ELSE chr(10)) + 
                 "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent kort på transaksjonen " + cKortNr + 
                 "." 
        cFilError = cFilError + 
                 (IF cFilError = ""
                    THEN ""
                    ELSE "|") + 
                 " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent kort på transaksjonen " + cKortNr + 
                 "." + CHR(1) + "2"
        BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
        .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValiderVareLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderVareLinje Procedure 
PROCEDURE ValiderVareLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      dVVareKost      = 0
      dMva%           = 0
      dMvaKr          = 0
      cVareGruppeNavn = ""
      cMvaGruppeNavn  = ""
      cBongTekst      = ""
      dArtikkelNr     = 0
      .
      
  /* Kontrollerer gyldig varegruppe */
  IF NOT CAN-FIND(VarGr WHERE
                  VarGr.Vg = iVg) THEN
    ASSIGN  
      cError = cError + 
               (IF cError = ""
                  THEN ""
                  ELSE chr(10)) + 
               "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
               " Ukjent varegruppe på transaksjonen " + SUBSTRING(BongLinje.OriginalData,1,2) + 
               "."
      cFilError = cFilError + 
               (IF cFilError = ""
                  THEN ""
                  ELSE "|") + 
               " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
               " Ukjent varegruppe på transaksjonen " + SUBSTRING(BongLinje.OriginalData,1,2) + 
               "." + CHR(1) + "2".
      BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
      .

  /* Kontrollerer gyldig artikkel */
  IF NOT CAN-FIND(ArtBas WHERE
                  ArtBas.Vg    = iVg AND
                  ArtBas.LopNr = iLopeNr) THEN
    ASSIGN  
      cError = cError + 
               (IF cError = ""
                  THEN ""
                  ELSE chr(10)) + 
               "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
               " Ukjent artikkel på transaksjonen " + STRING(iVg) + "/" + STRING(iLopeNr) + 
               "."
      cFilError = cFilError + 
               (IF cFilError = ""
                  THEN ""
                  ELSE "|") + 
               " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
               " Ukjent artikkel på transaksjonen " + STRING(iVg) + "/" + STRING(iLopeNr) + 
               "." + CHR(1) + "2"
      dArtikkelNr = 0
      BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
      .
  /* Henter ArtikkelNr */
  ELSE DO:
      FIND ArtBas NO-LOCK WHERE
          ArtBas.Vg    = iVg AND
          ArtBas.LopNr = iLopeNr NO-ERROR.
      IF AVAILABLE ArtBas THEN
          ASSIGN
          dArtikkelNr = ArtBas.ArtikkelNr
          cBongTekst  = ArtBas.BongTekst
          .
  END.

  /* Henter og kontrollerer mva koden */
  FIND VarGr NO-LOCK where
      VarGr.Vg = iVg no-error.
  IF AVAILABLE VarGr then
  VGSJEKK:
  DO:
      ASSIGN
          cVareGruppeNavn = VarGr.VgBeskr
          iMomsKod        = VarGr.MomsKod
          .
      FIND Moms  OF VarGr NO-LOCK NO-ERROR.
      FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Moms THEN
          ASSIGN  
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE chr(10)) + 
                     "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Ukjent mva på varegruppen Vg/MvaKode: " + string(iVg) + "/" + string(VarGr.MomsKod) + 
                     "."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Ukjent mva på varegruppen Vg/MvaKode: " + string(iVg) + "/" + string(VarGr.MomsKod) + 
                     "." + CHR(1) + "2"
            BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
            .
      ELSE
          ASSIGN
          cMvaGruppeNavn  = Moms.Beskrivelse
          dMva%           = Moms.MomsProc
          dMvaKr          = Mva2(BongLinje.LinjeSum)
          .
      IF NOT AVAILABLE HuvGr THEN
          ASSIGN  
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE chr(10)) + 
                     "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Ukjent hovedgruppe på den varegruppe som er på varelinjen: " + string(iVg) + "/" + STRING(VarGr.Hg) +
                     "."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     "  Ukjent hovedgruppe på den varegruppe som er på varelinjen: : " + string(iVg) + "/" + STRING(VarGr.Hg) +  
                     "." + CHR(1) + "2"
            BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
            .
      ELSE
          ASSIGN
          iHGr      = VarGr.Hg
          cHgrNavn  = HuvGr.HgBeskr
          .
  END. /* VGSJEKK */

  /* Kontrollerer gyldig feilkode */
  IF iFeilKode <> 0 THEN
  DO:
    FIND FeilKode NO-LOCK WHERE
        FeilKode.FeilKode = iFeilKode NO-ERROR.
    IF NOT AVAILABLE FeilKode THEN
      ASSIGN  
        cError = cError + 
                 (IF cError = ""
                    THEN ""
                    ELSE chr(10)) + 
                 "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent feilkode på transaksjonen " + string(iFeilKode) + 
                 "."
        cFilError = cFilError + 
                 (IF cFilError = ""
                    THEN ""
                    ELSE "|") + 
                 " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent feilkode på transaksjonen " + string(iFeilKode) + 
                 "." + CHR(1) + "2"
        BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
        .
    ELSE
        cFeilKodeTekst = FeilKode.Beskrivelse.
  END.
      
  /* Kontrollerer gyldig notatkode.                                  */
  /* Notatkode benyttes ikke. Tiltakskode legges inn her istedenfor. */
  IF iNotatKode <> 0 THEN
  DO:
    FIND Kravkode NO-LOCK WHERE
        KravKode.KravKode = iNotatKode NO-ERROR.
    IF NOT AVAILABLE Kravkode THEN
      ASSIGN  
        cError = cError + 
                 (IF cError = ""
                    THEN ""
                    ELSE chr(10)) + 
                 "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent tiltakskode på transaksjonen " + string(iNotatKode) + 
                 "."
        cFilError = cFilError + 
                 (IF cFilError = ""
                    THEN ""
                    ELSE "|") + 
                 " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent tiltakskode på transaksjonen " + string(iNotatKode) + 
                 "." + CHR(1) + "2"
        BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
        .
    ELSE
        cNotatKodeTekst = KravKode.Beskrivelse.
  END.
  


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-Mva2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Mva2 Procedure 
FUNCTION Mva2 RETURNS DECIMAL
  ( INPUT plBelop AS dec ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR dWork as DEC NO-UNDO.

  ASSIGN
     dWork = ((plBelop) /* / iAntall */) -
            (((plBelop) /* / iAntall */) / (1 + (dMva% / 100))).
  if dWork = ? THEN dWork = 0.

  RETURN dWork.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

