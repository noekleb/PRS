&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xkonvipeljournal.p
    Purpose     : Behandler eljournal fra InfoPOS kassene.

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

DEF VAR lAvrunding        AS DEC  NO-UNDO.
DEF VAR lVeksel           AS DEC  NO-UNDO.
DEF VAR lCashBack         AS DEC  NO-UNDO.
DEF VAR lTilgodeUt        AS DEC  NO-UNDO.
DEF VAR iLinjeNr          AS INT  NO-UNDO.
DEF VAR cPOSKoder         AS CHAR NO-UNDO.
DEF VAR cTTIdKoder        AS CHAR NO-UNDO.
DEF VAR cTekst            AS CHAR NO-UNDO.
DEF VAR cStorl            AS CHAR NO-UNDO.
DEF VAR iCl               AS INT  NO-UNDO.
DEF VAR bLoggSaldoFeil    AS LOG  NO-UNDO.
DEF VAR cError            AS CHAR NO-UNDO.
DEF VAR iEntry            AS INT  NO-UNDO.
DEF VAR cTTId             AS CHAR NO-UNDO.
DEF VAR c2TTId            AS CHAR NO-UNDO.
DEF VAR cWebReservasjon   AS CHAR NO-UNDO.
DEF VAR cRetur            AS CHAR NO-UNDO.
DEF VAR cPOS              AS CHAR NO-UNDO.
DEF VAR bBongSlettet      AS LOG  NO-UNDO.
DEF VAR iLagerType        AS INT  NO-UNDO.
DEF VAR iMButikkNr        AS INT  NO-UNDO.
DEF VAR bOverforing       AS LOG  NO-UNDO.
DEF VAR iRefNr            AS INT  NO-UNDO.
DEF VAR cRefTekst         AS CHAR NO-UNDO.
DEF VAR lSubtotalRab      AS DEC  NO-UNDO.
DEF VAR bTrening          AS LOG  NO-UNDO.
DEF VAR bUtbetaling       AS LOG  NO-UNDO.
DEF VAR bInnbetaling      AS LOG  NO-UNDO.
DEF VAR bDropp            AS LOG  NO-UNDO.
DEF VAR bVarekjop         AS LOG  NO-UNDO.
DEF VAR iSiste            AS INT  NO-UNDO.
DEF VAR bPakkeVare        AS LOG  NO-UNDO.
DEF VAR bUtbetType        AS INT  NO-UNDO.
DEF VAR bDepositum        AS LOG  NO-UNDO.
DEF VAR bRetur            AS LOG  NO-UNDO.
DEF VAR bReklamasjon      AS LOG  NO-UNDO.
DEF VAR cGaveTilgId       AS CHAR NO-UNDO.
DEF VAR cGaveTilgId2      AS CHAR NO-UNDO.
DEF VAR lGaveTilgType     AS DEC  NO-UNDO.
DEF VAR lGaveEget         AS LOG  INITIAL TRUE NO-UNDO.
DEF VAR iButAndre         AS INT  NO-UNDO.
DEF VAR bVareretur        AS LOG  NO-UNDO.
DEF VAR bPaVg             AS LOG  INITIAL FALSE NO-UNDO.
DEF VAR dPrisPrSalgsenhet AS DECI NO-UNDO.

DEF BUFFER bBonglinje FOR BongLinje.
DEF BUFFER bButiker   FOR Butiker.

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
  ( INPUT plBelop AS DEC )  FORWARD.

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
         HEIGHT             = 30
         WIDTH              = 59.6.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Konverteringstabell SkoTex InfoPOS til SkoTex BackOffice.              */ 
/* TN 1/4-04 Midlertidig fiks. 111 (Finansiering) styres inn som Kupong1. */
/*           Stockholm og Seven Eleven.                                   */
ASSIGN
    cPOSKoder      = "003,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,020,055,056,xxx,xxx,057," + 
                     "058,xxx,xxx,035,059,052,xxx,023,022,xxx,xxx,060,094,xxx,xxx,xxx," + 
                     "xxx,xxx,077,xxx,xxx,xxx,xxx,075,061,082,080,062,019,074,068,024,092,030,xxx,xxx,xxx," +
                     "xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,088,095,102," +
                     "xxx,xxx,xxx,xxx,106,078,073,104,108,105,111,112,012,113,091,125"

    cTTIdKoder     = "001,002,003,004,005,006,007,008,009,010,011,012,013,014,015,016,017,018,019,022,023,024,050,079,051,052,053," + 
                     "054,055,056,057,058,059,060,061,062,063,064,065,066,067,068,069," + 
                     "070,071,072,073,080,081,082,086,087,088,089,090,091,092,093,094,095,096,097,098,099," +
                     "100,101,102,103,104,105,106,107,108,120,121,122,123,130,131,140,141,142,143,144,145,146," +
                     "200,201,202,203,147,150,132,135,134,136,056,136,203,113,149,125"
  /*
  cKodeVarelinje = "003"
  cKodeBetTrans  = "022,023,035,053,055,056,057,058,059,060,094,077,080"
  */
  .

/* Sentrallager */
{syspara.i 5 1 1 iCl INT}
/* Logge saldofeil i bong */
{syspara.i 1 1 110 cTekst}
IF cTekst = "1" THEN
    bLoggSaldoFeil = TRUE.
ELSE
    bLoggSaldoFeil = FALSE.
/* Poster ukjente artikler på PLU (Varegruppe). */
{syspara.i 3 3 11 cTekst}
IF cTekst = "1" THEN
    bPaVg = TRUE.
ELSE
    bPAVg = FALSE.

/* Henter bongen som skal konverteres.              */
/* Konvertering av en bong ligger i en transaksjon. */
DO TRANSACTION:
  FIND BongHode EXCLUSIVE-LOCK WHERE
      ROWID(BongHode) = rRowId NO-ERROR.
  IF NOT AVAILABLE BongHode THEN
    RETURN "** Finner ikke bonghode med rowid: " + STRING(rRowId) + ".".

  RUN KonverterBong.
  
  /*
  MESSAGE BongHode.BongNr SKIP
        BongHode.KundeNr
        BongHode.KundeKort  SKIP
        BongHode.MedlemsNr
        BongHode.Medlemskort
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  */

  RELEASE BongHode.
END. /* TRANSACTION */

/* Returnerer liste med funne feil til kallende programm. */
RETURN cError.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-doTransType003) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType003 Procedure 
PROCEDURE doTransType003 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  TN 19/9-02.
          Returer som har fått påført feilkode <> 0, skal håndteres som reklamasjon.
          Årsaken er at LRS ikke kan innføre registrering av reklamasjonskoder i
          sin håndtering av reklamasjoner i kassen pr. idag.
  
------------------------------------------------------------------------------*/
    DEF VAR piLoop AS INT NO-UNDO.                                       

    ASSIGN
      BongLinje.Strekkode  =     ENTRY( 8,Bonglinje.Originaldata,";")
      BongLinje.VareGr     = int(ENTRY( 9,Bonglinje.Originaldata,";"))
      BongLinje.OrgVareGr  = BongLinje.VareGr
      BongLinje.BongTekst  = TRIM(ENTRY(10,Bonglinje.Originaldata,";"),'"')
      BongLinje.Antall     = dec(ENTRY(11,Bonglinje.Originaldata,";")) / 1000
      BongLinje.LinjeSum   = (IF BongLinje.Antall > 0
                                THEN (DEC(ENTRY(12,Bonglinje.Originaldata,";"))) / 100
                                ELSE ABS(DEC(ENTRY(12,Bonglinje.Originaldata,";"))) / 100
                             )
    /*BongLinje.LinjeSum   = ABS(DEC(ENTRY(12,Bonglinje.Originaldata,";"))) / 100*/
      BongLinje.BongPris   = BongLinje.LinjeSum

      /* Varekosten fra kassen er hentet fra varekost i gjeldene kalkyle. */
      /* Denne skal benyttes på ikke lagerstyrte varer.                   */
      BongLinje.VVarekost  = (IF BongLinje.Antall > 0
                                THEN (DEC(ENTRY(13,Bonglinje.Originaldata,";"))) / 100
                                ELSE ABS(DEC(ENTRY(13,Bonglinje.Originaldata,";"))) / 100
                             )
      BongLinje.Mva%       = DEC(ENTRY(14,Bonglinje.Originaldata,";")) / 100
      BongLinje.MvaKr      = (IF Bonglinje.Antall < 0 
                               THEN ABS(DEC(ENTRY(15,Bonglinje.Originaldata,";"))) / 100
                               ELSE (DEC(ENTRY(15,Bonglinje.Originaldata,";"))) / 100)
      BongLinje.FeilKode   = int(ENTRY(16,Bonglinje.Originaldata,";"))
      BongLinje.NotatKode  = int(ENTRY(17,Bonglinje.Originaldata,";")) /* Tiltakskode */
      BongLinje.RefNr      = iRefNr
      BongLinje.RefTekst   = cRefTekst
      .
    /* BugHide 1 */
    IF BongLinje.Strekkode BEGINS "29" AND
       LENGTH(BongLinje.Strekkode) = 12 THEN
        BongLinje.Strekkode = "0" + BongLinje.Strekkode.

    /* BugHide 2 - Feil i pakkerecord fra kasse. Transaksjonen skal ignoreres. */
    IF TRIM(BongLinje.Strekkode) = "0000000000000" AND
        BongLinje.Antall = 0 AND
        BongLinje.LinjeSum = 0 THEN
        BongLinje.Makulert = TRUE.

    /* Identifiserer artikkelen og henter størrelsen. */
    RUN ValiderArtikkel.
    IF cStorl <> "" THEN
    DO:
        ASSIGN
            BongLinje.Storrelse = cStorl
            cStorl              = ""
            .
    END.

    /* Hent varelinjeinfo - ArtikkelNr må settes her. */
    RUN HentVareLinjeInfo.

    /* Posterer rabatter */
    DO piLoop = 1 TO 3:
        CASE int(ENTRY(17 + piLoop,Bonglinje.Originaldata,";")):
            WHEN  0 THEN. /* Ingenting postert. */
            WHEN  1 THEN DO: 
                           BongLinje.Generellrabatt = BongLinje.Generellrabatt + ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100).
                           {xkonvipeljournal_log.i VANLIGSALG}                        
                         END.
            WHEN  2 THEN DO: 
                           BongLinje.Tilbudsrabatt = BongLinje.Tilbudsrabatt + ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100).
                           {xkonvipeljournal_log.i TILBUD} 
                           IF dec(BongLinje.ArtikkelNr) > 0 THEN 
                           SETT_EKSTRAPRIS:
                           DO:
                             FOR EACH KampanjeLinje NO-LOCK WHERE
                               KampanjeLinje.ArtikkelNr = dec(BongLinje.ArtikkelNr),
                               LAST KampanjeHode OF KampanjeLinje NO-LOCK WHERE
                                 KampanjeHode.StartDato <= BongLinje.Dato AND
                                 KampanjeHode.SluttDato >= BongLinje.Dato,
                               FIRST KampanjeMixMatch NO-LOCK WHERE 
                                     KampanjeMixMatch.KampId = KampanjeHode.KampId,
                               FIRST KampanjeTilbud OF KampanjeMixMatch NO-LOCK:                                 
                               ASSIGN
                                 BongLinje.KampId     = KampanjeMixMatch.KampId
                                 BongLinje.KampTilbId = KampanjeTilbud.KamptilbId
                                 BongLinje.KampEierId = KampanjeMixMatch.KampEierId
                                .                                 
                               LEAVE SETT_EKSTRAPRIS.
                             END.
                           END. /* SETT_EKSTRAPRIS */                      
                         END.
            WHEN  3 THEN DO: 
                           BongLinje.MixMatchRabatt = BongLinje.MixMatchRabatt + ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100).
                           {xkonvipeljournal_log.i MIXMATCH}
                           IF dec(BongLinje.ArtikkelNr) > 0 THEN 
                           SETT_MIXMATCH: 
                           FOR EACH KampanjeTilbArtikkel NO-LOCK WHERE
                                    KampanjeTilbArtikkel.KampTilbArtId = dec(BongLinje.ArtikkelNr),
                             LAST  KampanjeTilbud OF KampanjeTilbArtikkel NO-LOCK WHERE
                                   KampanjeTilbud.KampTilbTypeId > 9 ,
                             LAST  KampanjeMixMatch OF KampanjeTilbArtikkel NO-LOCK WHERE
                                   KampanjeMixMatch.KampStartDato <= BongLinje.Dato AND
                                   KampanjeMixMatch.KampSluttDato >= BongLinje.Dato
                                   BREAK BY KampanjeTilbArtikkel.KampId DESCENDING     
                                   BY KampanjeTilbArtikkel.KampTilbId 
                                   BY KampanjeTilbArtikkel.KamptilbArtId:                               
                             ASSIGN
                               BongLinje.KampId     = KampanjeTilbArtikkel.KampId
                               BongLinje.KampTilbId = KampanjeTilbArtikkel.KamptilbId
                               BongLinje.KampEierId = KampanjeMixMatch.KampEierId
                              .
                             LEAVE SETT_MIXMATCH.       
                           END. /* SETT_MIXMATCH */                                                          
                         END.
            WHEN  4 THEN DO: 
                           BongLinje.Medlemsrabatt          = BongLinje.Medlemsrabatt  + ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100).
                           {xkonvipeljournal_log.i MEDLEMSRABATT}                        
                         END.
            WHEN  5 THEN DO: 
                           BongLinje.Kunderabatt            = BongLinje.Kunderabatt    + ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100).
                           {xkonvipeljournal_log.i KUNDERABATT}                        
                         END.
            WHEN  6 THEN DO: 
                           BongLinje.Personalrabatt         = BongLinje.Personalrabatt + ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100).
                           {xkonvipeljournal_log.i PERS.RAB_VIA_KUNDE}                        
                         END.
            WHEN  7 THEN DO:
                           BongLinje.AlternativPrisRabatt   = BongLinje.AlternativPrisRabatt + ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100).
                           {xkonvipeljournal_log.i ALTERNATIV_PRIS}                        
                         END.
            WHEN  8 THEN DO:
                           BongLinje.ManuelEndretPrisRabatt = BongLinje.ManuelEndretPrisRabatt + ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100).
                           {xkonvipeljournal_log.i MANUELT_ENDRET_PRIS}                        
                         END.
            WHEN  9 THEN DO: 
                           BongLinje.Generellrabatt         = BongLinje.Generellrabatt + ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100).
                           {xkonvipeljournal_log.i _LINJERABATT}                        
                         END.
            WHEN 10 THEN DO: 
                           BongLinje.Generellrabatt         = BongLinje.Generellrabatt + ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100).
                           {xkonvipeljournal_log.i SUBTOTRAB}                        
                         END.
            WHEN 11 THEN DO: 
                           BongLinje.Personalrabatt         = BongLinje.Personalrabatt + ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100).
                           {xkonvipeljournal_log.i SUBTOTRAB_Personal}                        
                         END.
            WHEN 12 THEN DO: 
                           BongLinje.Personalrabatt         = BongLinje.Personalrabatt + ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100).
                           {xkonvipeljournal_log.i LINJERAB_Personal}                        
                         END.
            WHEN 13 THEN DO: 
                           BongLinje.Generellrabatt         = BongLinje.Generellrabatt + ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100).
                           {xkonvipeljournal_log.i PAKKERABATT}                        
                         END.
            /* All annen gørr havner her. */
            OTHERWISE BongLinje.Generellrabatt = BongLinje.Generellrabatt + ABSOLUTE(DEC(ENTRY(18 + piLoop,Bonglinje.Originaldata,";")) / 100).
        END CASE.
    END.

    /* Håntering av gjenkjøp/retur */
    IF BongLinje.Antall < 0 AND 
       BongLinje.Antall > -9999 THEN
        ASSIGN
        BongLinje.TTId               = 10 /* Retur eller reklamasjon */
        BongLinje.TTId               = (IF (BongLinje.FeilKode <> 0 OR
                                           BongLinje.Notatkode <> 0)
                                         THEN 3
                                         ELSE BongLinje.TTId)
        BongLinje.ReturKassererNavn  = ""
        BongLinje.ReturButikk        = 0
        BongLinje.ReturKasserer      = 0
        bRetur                       = BongLinje.TTID = 10
        bReklamasjon                 = BongLinje.TTID = 3
        bVareretur                   = TRUE
        .

    /* Rabatter */
    RABATTER:
    DO piLoop = 1 TO 3:
      IF CAN-DO("1,2,3,4,5,6,7,8,9,12,13,14,15,16,17,18,19,20",TRIM(ENTRY(16 + (piLoop * 2),Bonglinje.Originaldata,";"))) THEN
          ASSIGN
          BongLinje.LinjeRab = BongLinje.LinjeRab + ABS(DEC(ENTRY(17 + (piLoop * 2),Bonglinje.Originaldata,";")))  / 100
          .
      ELSE /*10,11 - 11 er subtotalrabatt personale */
          ASSIGN
          BongLinje.SubTotalRab = BongLinje.SubTotalRab + ABS(DEC(ENTRY(17 + (piLoop * 2),Bonglinje.Originaldata,";")))  / 100
          .
    END. /* RABATTER */

    /* Fikser beløpsfeltet */
    ASSIGN
        BongLinje.LinjeSum = BongLinje.LinjeSum + (BongLinje.LinjeRab + BongLinje.SubTotalRab)
        BongLinje.BongPris = BongLinje.LinjeSum
        lSubtotalRab       = lSubTotalRab + BongLinje.SubTotalRab
        BongLinje.PrisPrSalgsenhet = dPrisPrSalgsenhet
        dPrisPrSalgsenhet  = 0.


    /* Reservasjoner fra WebButik, skal overføres motsatte vei.          */
    /* Dette er flagget på 61 transaksjonen ved at feiltype er satt = 0. */
    IF BongLinje.TTID = 6 AND cWebReservasjon <> '' THEN
        BongLinje.Antall = BongLinje.Antall * -1.  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType012) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType012 Procedure 
PROCEDURE doTransType012 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
      Bonglinje.BongTekst = "NULLSALG"
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType019) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType019 Procedure 
PROCEDURE doTransType019 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
      BongLinje.LinjeSum  = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 /* Total       */ 
      BongLinje.Antall    = DEC(ENTRY( 9,Bonglinje.Originaldata,";"))       /* Antall feil */ 
      Bonglinje.BongTekst = "BONG SLETTET"
      bBongSlettet        = TRUE
      BongLinje.BongPris  = BongLinje.LinjeSum
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType022) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType022 Procedure 
PROCEDURE doTransType022 :
/*------------------------------------------------------------------------------
  Purpose:     UTBETALING
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iUtBettype AS INTEGER    NO-UNDO.
  ASSIGN
      bUtbetaling         = IF BongLinje.Makulert = FALSE
                              THEN TRUE
                              ELSE FALSE
      BongLinje.LinjeSum  = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 /* Beløp  */ 
      BongLinje.BongPris  = BongLinje.LinjeSum
      .
  /* Detta är en hantering för Preem */
  IF NUM-ENTRIES(Bonglinje.Originaldata,";") > 9 AND ENTRY( 9,Bonglinje.Originaldata,";") = "UTTYPE" THEN DO:
      ASSIGN iUtBettype = INT(ENTRY(10,Bonglinje.Originaldata,";")) NO-ERROR.
      IF iUtbetType > 0 THEN
          ASSIGN BongLinje.Storrelse = ENTRY(10,Bonglinje.Originaldata,";").
  END.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType023) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType023 Procedure 
PROCEDURE doTransType023 :
/*------------------------------------------------------------------------------
  Purpose:     INNBETALING
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iInBettype AS INTEGER    NO-UNDO.
  ASSIGN
      bInnbetaling        = IF BongLinje.Makulert = FALSE
                              THEN TRUE
                              ELSE FALSE
      BongLinje.LinjeSum  = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 /* Beløp  */ 
      BongLinje.BongPris  = BongLinje.LinjeSum
      .

  /* Detta är en hantering för Preem */
  IF NUM-ENTRIES(Bonglinje.Originaldata,";") > 9 AND ENTRY( 9,Bonglinje.Originaldata,";") = "INTYPE" THEN DO:
      ASSIGN iInBettype = INT(ENTRY(10,Bonglinje.Originaldata,";")) NO-ERROR.
      IF iInbetType > 0 THEN
          ASSIGN BongLinje.Storrelse = ENTRY(10,Bonglinje.Originaldata,";").
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType024) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType024 Procedure 
PROCEDURE doTransType024 :
/*------------------------------------------------------------------------------
  Purpose:     MEDLEM
  Parameters:  <none>
  Notes:       Validering av medlemskort og påføring av medlemsnummer og
               navn gjøres i en egen rutine.
------------------------------------------------------------------------------*/

  /* Medlemskort bare settes på her. Det gjøres ikke oppslag på medlem. Hvis det også kommer transtype 62 */
  /* vil behandling av transtype 62 legge på kundenr og kundenavn. Eventuelt opprette kunden.             */
  /* Etter at bongen er behandlet, sjekkes det om medlemskort er påført. Dette skjer i SjekkMedlem.       */
  ASSIGN
      BongLinje.StrekKode  = (ENTRY( 8,Bonglinje.Originaldata,";"))    /* Medlemsnummer  */ 
      /* BongHode */
      BongHode.MedlemsKort = LEFT-TRIM(BongLinje.StrekKode,"0")
      .
                     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType030) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType030 Procedure 
PROCEDURE doTransType030 :
/*------------------------------------------------------------------------------
  Purpose:     Sign ON/OFF
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  ASSIGN
      BongLinje.Strekkode  = (ENTRY( 8,Bonglinje.Originaldata,";"))    /* KassererNr    */ 
      BongLinje.BongTekst  = TRIM((ENTRY( 9,Bonglinje.Originaldata,";")),'"')    /* KassererNavn  */
      BongLinje.LinjeSum   = DEC(ENTRY(10,Bonglinje.Originaldata,";")) /* Tid innlogget */
      BongLinje.TTId       = IF INT(ENTRY( 8,Bonglinje.Originaldata,";")) = 0
                               THEN 97
                               ELSE 96
      BongLinje.BongPris   = BongLinje.LinjeSum
      .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType035) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType035 Procedure 
PROCEDURE doTransType035 :
/*------------------------------------------------------------------------------
  Purpose:     PANT
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cKode AS CHARACTER NO-UNDO.
  
  DEFINE BUFFER pantArtBas FOR ArtBas.
  
  ASSIGN
      cKode                = '99999'
      BongLinje.Antall     = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 1000 /* Antall        */ 
      BongLinje.LinjeSum   = (DEC(ENTRY( 9,Bonglinje.Originaldata,";")) / 100) * -1  /* Beløp         */
      BongLinje.BongPris   = BongLinje.LinjeSum
      .
 
  FIND Strekkode NO-LOCK WHERE 
    Strekkode.Kode = cKode NO-ERROR.
  IF AVAILABLE Strekkode THEN 
    FIND pantArtBas OF Strekkode NO-ERROR.
  
  IF AVAILABLE pantArtBas THEN 
  DO:
    ASSIGN
      BongLinje.TTID = 1
      BongLinje.Strekkode  = cKode
      BongLinje.VareGr     = pantArtBas.Vg
      BongLinje.ArtikkelNr = STRING(pantArtBas.ArtikkelNr)
      BongLinje.OrgVareGr  = BongLinje.VareGr
      BongLinje.BongTekst  = pantArtBas.Beskr
      BongLinje.Antall     = 1
      .
  END.
  
    /* Identifiserer artikkelen og henter størrelsen. */
    RUN ValiderArtikkel.

    /* Hent varelinjeinfo */
    RUN HentVareLinjeInfo.
    ASSIGN
        BongLinje.Storrelse = cStorl
        BongLinje.VVAreKost = 0
        .
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType052) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType052 Procedure 
PROCEDURE doTransType052 :
/*------------------------------------------------------------------------------
  Purpose:     DROPP
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
      bDropp               = TRUE
      BongLinje.LinjeSum   = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 /* Beløp         */
      BongLinje.BongPris   = BongLinje.LinjeSum
      .
                    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType055) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType055 Procedure 
PROCEDURE doTransType055 :
/*------------------------------------------------------------------------------
  Purpose:     KONTANT
  Parameters:  <none>
  Notes:         
------------------------------------------------------------------------------*/
  DEF VAR plNoVeksel AS LOG NO-UNDO.
  DEF VAR plNegVare  AS LOG NO-UNDO.

  DEF VAR lPengerTilbake AS DEC NO-UNDO.

  DEF BUFFER b2BongLinje FOR BongLinje.
  
  ASSIGN
      BongLinje.BongPris  = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 /* Subtotal    */ 
      BongLinje.LinjeSum  = DEC(ENTRY( 9,Bonglinje.Originaldata,";")) / 100 /* Innbetalt Beløp       */ 
      lPengerTilbake      = DEC(ENTRY(10,Bonglinje.Originaldata,";")) / 100 /* Penger tilbake  */ 
      BongLinje.Antall    = DEC(ENTRY(12,Bonglinje.Originaldata,";"))    /* KortType    */ 
      /* BongHode */
      BongHode.flBetalingsKort = IF BongLinje.Antall <> 0 THEN TRUE ELSE FALSE
      BongHode.flBankKort      = IF BongLinje.Antall <> 0 THEN TRUE ELSE FALSE
      .
  /* Håndtering av bonger hvor det gis penger tilbake. */
  /* F.eks retur hvor beløp står = 0 og veksel = beløp som skal tilbakebetales. */
  IF bDropp = FALSE THEN
      IF BongLinje.LinjeSum = 0 THEN
      DO:
        ASSIGN
          plNoVeksel         = TRUE /* Retur hvor beløp utbetales til kunde */
          .
        /* Spesial spesial */
        /* Er bongens saldo minus avrundingen lik 0, skal linjesum alikevel nullstilles. */
        IF (DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100) + (DEC(ENTRY(11,Bonglinje.Originaldata,";")) / 100) = 0 THEN
          BongLinje.Linjesum = 0.
        ELSE
          BongLinje.LinjeSum = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100.
      END.
  /* Håndtering av reklamasjon - kunden får penger tilbake TN 23/2-05 */
  IF bDropp = FALSE THEN
      IF BongLinje.BongPris < 0 AND (BongLinje.BongPris + BongLinje.Linjesum = 0) THEN
          BongLinje.LinjeSum = BongLinje.BongPris.

  /* Er det en utbetaling, skal utbetalt beløp settes inn. */
  IF bUtbetaling THEN
      BongLinje.LinjeSum  = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 /* Beløp       */ 
      .
  /* Er det en innbetaling, skal innbetalt beløp settes inn. */
  IF bInnbetaling THEN
      BongLinje.LinjeSum  = DEC(ENTRY( 9,Bonglinje.Originaldata,";")) / 100 /* Beløp       */ 
      .

  /* Håndtering av veksel hvis dette er siste betalingstrans på bongen. */
  IF BongLinje.LinjeNr = iSiste THEN
  DO:
    ASSIGN
      lVeksel             = IF plNoVeksel
                              THEN 0
                              ELSE DEC(ENTRY(10,Bonglinje.Originaldata,";")) / 100 /* Veksel      */ 
      lAvrunding          = DEC(ENTRY(11,Bonglinje.Originaldata,";")) / 100 /* Avrunding   */ 
      .
  END.

  /* Ved dropp, er kontant = 0. Da skal veksel ikke beregnes. */
  /* Kontantbeløpet er da lik bongens saldo.                  */
  IF (BongLinje.LinjeSum = 0 AND bDropp) THEN
    ASSIGN
      lVeksel             = 0
      BongLinje.LinjeSum  = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100
      .
  /* Atter en spesial */
  /* Bare returer på bongen, og kunden betaler inn et lite beløp for å få et rundt beløp tilbake. */
  /* NB: bUtbetType settes bare ved utbetaling av tilgodelapp (104). Ellers er den null.          */
  IF (DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100) < 0 AND
     (DEC(ENTRY( 9,Bonglinje.Originaldata,";")) / 100) > 0 AND 
     (DEC(ENTRY(10,Bonglinje.Originaldata,";")) / 100) > 0 THEN
  DO:
      lVeksel = (DEC(ENTRY(10,Bonglinje.Originaldata,";")) / 100).
      FIND LAST b2BongLinje NO-LOCK WHERE
          b2BongLinje.B_Id = BongHode.B_Id NO-ERROR.

      IF AVAILABLE b2BongLinje THEN
      DO:
          IF bUtbetType = 0 THEN 
          DO: /* Kontant tilbake */
              CREATE bBongLinje.
              BUFFER-COPY b2BongLinje TO bBongLinje
                  ASSIGN
                    bBongLinje.LinjeNr      = b2BongLinje.LinjeNr + 1
                    bBongLinje.LinjeSum     = lVeksel * -1 
                    bBongLinje.TTId         = 70
                    bBongLinje.Originaldata = ";;;;;;;;"
                    bBongLinje.Strekkode    = ""
                    bBongLinje.BongTekst    = ""
                    bUtbetType = 0
                    .
              RELEASE bBongLinje.
          END.
          ELSE IF bUtbetType <> 94 THEN 
          DO: /* Kontant tilbake */
              CREATE bBongLinje.
              BUFFER-COPY b2BongLinje TO bBongLinje
                  ASSIGN
                    bBongLinje.LinjeNr      = b2BongLinje.LinjeNr + 1
                    bBongLinje.LinjeSum     = lVeksel * -1
                    bBongLinje.TTId         = (IF bUtbetType = 94
                                                 THEN 69 /* Utlevert tilgodelapp */
                                                 ELSE 70)
                    bBongLinje.Originaldata = ";;;;;;;;"
                    bBongLinje.Strekkode    = ""
                    bBongLinje.BongTekst    = ""
                    bUtbetType = 0
                    .
              RELEASE bBongLinje.
          END.
          ELSE /* Tilgodelapp tilbake */
              ASSIGN
                  lTilgodeUt = lVeksel * -1
                  .
          ASSIGN lVeksel = 0.
      END.
  END.

  /* Håndtering av kontant utbetaling som er oppstått pga retur av varer. */
  /*IF BongLinje.BongPris < 0 AND */
  IF DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 < 0 AND
     NOT bDropp AND
     NOT bUtbetaling AND
     NOT bInnbetaling AND
     NOT bDepositum AND
     NOT bRetur AND
     NOT bReklamasjon
  THEN DO:
      plNegVare = FALSE.
      /* Sjekker om det er negativ vare. */
      FIND Strekkode NO-LOCK WHERE
          Strekkode.Kode = LEFT-TRIM(ENTRY( 10,Bonglinje.Originaldata,";"),'0') NO-ERROR.
      IF NOT AVAILABLE Strekkode THEN
          FIND Strekkode NO-LOCK WHERE
              Strekkode.Kode = ENTRY( 10,Bonglinje.Originaldata,";") NO-ERROR.
      IF AVAILABLE Strekkode THEN
          FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
      IF AVAILABLE ArtBas AND ArtBas.NegVare THEN
          plNegVare = TRUE.

      IF plNegVare = FALSE THEN
      ASSIGN
          BongLinje.LinjeSum = (DEC(ENTRY( 10,Bonglinje.Originaldata,";")) / 100) * -1
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType056) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType056 Procedure 
PROCEDURE doTransType056 :
/*------------------------------------------------------------------------------
  Purpose:     Reserveløsing
  Parameters:  <none>
  Notes:       Det gis aldri veksel på dette media.
               Er det veksel på bongen, gir BABS feilmelding og godtar ikke transen.
------------------------------------------------------------------------------*/
  ASSIGN
      BongLinje.BongPris  = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 /* Subtotal    */ 
      BongLinje.LinjeSum  = DEC(ENTRY( 9,Bonglinje.Originaldata,";")) / 100 /* Beløp       */ 
      BongLinje.Antall    = DEC(ENTRY(12,Bonglinje.Originaldata,";"))    /* KortType    */ 
      /* BongHode */
      BongHode.flBetalingsKort = IF BongLinje.Antall <> 0 THEN TRUE ELSE FALSE
      BongHode.flBankKort      = IF BongLinje.Antall <> 0 THEN TRUE ELSE FALSE
      .
  /* Håndtering av veksel hvis dette er siste betalingstrans på bongen. */
  IF BongLinje.LinjeNr = iSiste THEN
  DO:
    ASSIGN
      lVeksel             = 0 /*DEC(ENTRY(10,Bonglinje.Originaldata,";")) / 100*/ /* Veksel      */ 
      lAvrunding          = DEC(ENTRY(11,Bonglinje.Originaldata,";")) / 100 /* Avrunding   */ 
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType057) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType057 Procedure 
PROCEDURE doTransType057 :
/*------------------------------------------------------------------------------
  Purpose:     KONTANT
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iTest AS INTEGER    NO-UNDO.
  ASSIGN
      BongLinje.BongPris  = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 /* Subtotal    */ 
      BongLinje.LinjeSum  = DEC(ENTRY( 9,Bonglinje.Originaldata,";")) / 100 /* Beløp       */ 
      BongLinje.Antall    = iButAndre /*lGaveTilgType *//* KortType    */ 
      BongLinje.MButik    = iButAndre
      BongLinje.Strekkode = cGaveTilgId
      cGaveTilgId         = ""
      BongLinje.BongTekst = IF lGaveEget 
                        THEN "0,Eget," + STRING(iButAndre) + "," + string(lGaveTilgType)
                        ELSE "1,Andre," + STRING(iButAndre) + "," + string(lGaveTilgType)
/*       BongLinje.BongTekst = IF lGaveEget                                                        */
/*                               THEN "0,Eget," + STRING(iButAndre) + "," + string(lGaveTilgType)  */
/*                               ELSE "1,Andre," + STRING(iButAndre) + "," + string(lGaveTilgType) */
      lGaveTilgType       = 0
      lGaveEget           = TRUE
      iButAndre           = 0
      .
    /* För att kunna hantera olika typer av bonus-presentkort hos Preem */
    IF NUM-ENTRIES(Bonglinje.Originaldata,";") > 13 AND 
                      ENTRY(13,Bonglinje.Originaldata,";") = "PREEMGAVE" THEN DO:
        ASSIGN iTest = INT(ENTRY(14,Bonglinje.Originaldata,";")) NO-ERROR.
        IF iTest > 1 AND iTest < 21 THEN DO:
            ASSIGN Bonglinje.Storrelse = ENTRY(14,Bonglinje.Originaldata,";")
                   Bonglinje.Bongtekst = IF Bonglinje.Storrelse = "02" THEN "Preem presentkort" ELSE
                                         IF Bonglinje.Storrelse = "03" THEN "Preem Bonuscheck" ELSE
                                            "Preem annan kredittyp"
            .
        END.
    END.


  /* Håndtering av veksel hvis dette er siste betalingstrans på bongen.       */
  /* Gis tilgode ved retur/reklam, skal det ikke legges opp ekstra bonglinje. */
  /* I disse tilfellene nullstilles lTilgodeUt.                               */
  IF BongLinje.LinjeNr = iSiste THEN
  DO:
    ASSIGN
      lVeksel             = DEC(ENTRY(10,Bonglinje.Originaldata,";")) / 100 /* Veksel      */ 
      /*
      lTilgodeUt = DEC(ENTRY(10,Bonglinje.Originaldata,";")) / 100 /* Veksel      */ 
      lTilgodeUt = lTilgodeUt * (IF BongLinje.LinjeSum > 0
                                  THEN -1 /* Snur fortegn på TilgodeUt. */
                                  ELSE 0) /* Ingen ekstra linje. Nullstiller. */
      */
      lAvrunding = DEC(ENTRY(11,Bonglinje.Originaldata,";")) / 100 /* Avrunding   */ 
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType058) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType058 Procedure 
PROCEDURE doTransType058 :
/*------------------------------------------------------------------------------
  Purpose:     KONTANT
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      BongLinje.BongPris  = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 /* Subtotal    */ 
      BongLinje.LinjeSum  = DEC(ENTRY( 9,Bonglinje.Originaldata,";")) / 100 /* Beløp       */ 
      BongLinje.Antall    = DEC(ENTRY(12,Bonglinje.Originaldata,";"))       /* KortType    */ 
      /* BongHode */
/*       BongHode.flBetalingsKort = IF BongLinje.Antall <> 0 THEN TRUE ELSE FALSE */
/*       BongHode.flBankKort      = IF BongLinje.Antall <> 0 THEN TRUE ELSE FALSE */
      .
  /* Håndtering av veksel hvis dette er siste betalingstrans på bongen. */
  IF BongLinje.LinjeNr = iSiste THEN
  DO:
    ASSIGN
      lVeksel             = DEC(ENTRY(10,Bonglinje.Originaldata,";")) / 100 /* Veksel      */ 
      lAvrunding          = DEC(ENTRY(11,Bonglinje.Originaldata,";")) / 100 /* Avrunding   */ 
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType059) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType059 Procedure 
PROCEDURE doTransType059 :
/*------------------------------------------------------------------------------
  Purpose:     KONTANT
  Parameters:  <none>
  Notes:       
Korttypene som listes ut i "vår" 
InfoPOS med betegnelsen "bankkort" på de typene som pr. def. er et bankkort.
Her er dump av lista:
  1  *  Bankkort
  2  *  Bankkort (Postbanken)
  3     Visa
  4     Eurocard
  5     American express
  6     Diners
  7     Coop kort
  8     Multikort
  9  *  Bankkort (GE Capital)
 10  *  Bankkort (Gjensidige bank)
 11     JCB
 12     Trumf
 13     Domino
 14     Maestro
 15     Lindexkortet
 16     IKAno
 17  *  Bankkort (DnB kort)
 18     Proton
 19     SH kortet
 20  *  Bankkort (COOP)
 21     BBL
 22     Gavekort Kjede
 23     Gavekort Senter

NB: Behandler også kode 0 som bankkort for å sikre kompabilitet 
med gamle filer som inneholder feil.

------------------------------------------------------------------------------*/
  DEF VAR piKortType AS INT NO-UNDO. /*0-Debit/Bank kort, 1-Kreditkort */
  DEF VAR piKortTNr  AS INT NO-UNDO.

  ASSIGN
      BongLinje.BongPris  = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 /* Subtotal    */ 
      BongLinje.LinjeSum  = DEC(ENTRY( 9,Bonglinje.Originaldata,";")) / 100 /* Beløp       */ 
      lCashBack           = DEC(ENTRY(10,Bonglinje.Originaldata,";")) / 100 /* CashBack    */ 
      lAvrunding          = DEC(ENTRY(11,Bonglinje.Originaldata,";")) / 100 /* Avrunding   */ 
      BongLinje.Antall    = DEC(ENTRY(12,Bonglinje.Originaldata,";"))       /* KortType    */ 
      piKortTNr           = BongLinje.Antall
      /* BongHode */
      BongHode.flBetalingsKort = IF BongLinje.Antall <> 0 THEN TRUE ELSE FALSE
      BongHode.flBankKort      = IF BongLinje.Antall <> 0 THEN TRUE ELSE FALSE
      .

  /* Wayne eller LRS kasse */
  IF NUM-ENTRIES(Bonglinje.Originaldat,";") > 12 THEN
  WAYNE-POS:
  DO:
      ASSIGN
          piKortType     = DEC(ENTRY(13,Bonglinje.Originaldata,";"))
          BongLinje.TTId = IF piKortTNr <> 13 
                             THEN 52 
                             ELSE BongLinje.TTId.
          .
  END. /* WAYNE-POS */

  /* Lindbak kasse */
  ELSE LRS-POS: DO:
      /* Transtype BANKORT */
      CASE BongLinje.Antall:
        WHEN  0.0 THEN BongLinje.Antall = 1. /* BANKORT */
        WHEN  1.0 THEN. /* BANKORT Bankkort (BankAxept) */
        WHEN  2.0 THEN. /* BANKORT Postbanken (BankAxept) */
        WHEN  9.0 THEN. /* BANKORT GE Capital (BankAxept) */
        WHEN 10.0 THEN. /* BANKORT (Utgått fra listen) */
        WHEN 17.0 THEN. /* BANKORT (Utgått fra listen) */
        WHEN 20.0 THEN. /* BANKORT Coop (BankAxept) */
        WHEN 30.0 THEN. /* BANKORT BAX Smartkort */
        OTHERWISE BongLinje.TTId = 52. /* KORT */
      END CASE.
  END. /* LRS-POS */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType060) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType060 Procedure 
PROCEDURE doTransType060 :
/*------------------------------------------------------------------------------
  Purpose:     KONTANT
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iTest AS INTEGER    NO-UNDO.
  ASSIGN
      BongLinje.BongPris  = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 /* Subtotal    */ 
      BongLinje.LinjeSum  = DEC(ENTRY( 9,Bonglinje.Originaldata,";")) / 100 /* Beløp       */ 
      BongLinje.Antall    = DEC(ENTRY(12,Bonglinje.Originaldata,";"))       /* KortType    */ 
      /* BongHode */
      BongHode.flBetalingsKort = IF BongLinje.Antall <> 0 THEN TRUE ELSE FALSE
      BongHode.flBankKort      = IF BongLinje.Antall <> 0 THEN TRUE ELSE FALSE
      .
  /* För att kunna hantera olika typer av bonus-presentkort hos Preem */
  IF NUM-ENTRIES(Bonglinje.Originaldata,";") > 13 AND 
                    ENTRY(13,Bonglinje.Originaldata,";") = "PREEMMSLIP" THEN DO:
      ASSIGN iTest = INT(ENTRY(14,Bonglinje.Originaldata,";")) NO-ERROR.
      IF iTest = 1 THEN DO:
          ASSIGN Bonglinje.Storrelse = ENTRY(14,Bonglinje.Originaldata,";")
                 Bonglinje.Bongtekst = "Preem manuell slip"
          .
      END.
  END.
  
  /* Håndtering av veksel hvis dette er siste betalingstrans på bongen. */
  IF BongLinje.LinjeNr = iSiste THEN
  DO:
    ASSIGN
      /*
      lVeksel             = 0 /*DEC(ENTRY(10,Bonglinje.Originaldata,";")) / 100*/ /* Veksel      */ 
      lAvrunding          = 0 /*DEC(ENTRY(11,Bonglinje.Originaldata,";")) / 100*/ /* Avrunding   */ 
      */
      lVeksel             = DEC(ENTRY(10,Bonglinje.Originaldata,";")) / 100 /* Veksel      */ 
      lAvrunding          = DEC(ENTRY(11,Bonglinje.Originaldata,";")) / 100 /* Avrunding   */ 
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType061) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType061 Procedure 
PROCEDURE doTransType061 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:  
               
------------------------------------------------------------------------------*/
  DEF BUFFER bufButiker FOR Butiker.

  ASSIGN
      BongLinje.Antall      = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) /* Type              */ 
      BongLinje.LinjeSum    = DEC(ENTRY( 9,Bonglinje.Originaldata,";")) /* Mottagende butikk */
      BongLinje.LinjeRab    = DEC(ENTRY(10,Bonglinje.Originaldata,";")) /* Feilkode          */
      BongLinje.SubTotalRab = DEC(ENTRY(11,Bonglinje.Originaldata,";")) /* Tiltakskode       */
      .

  ASSIGN
      iLagerType     = int(ENTRY(8,BongLinje.OriginalData,";"))
      BongLinje.TBId = iLagerType
      .

  CASE iLagerType:
      WHEN 1 THEN ASSIGN /* Brekkasje  */
                    c2TTId       = "002"
                    bVarekjop    = TRUE
                    .
      WHEN 2 THEN ASSIGN /* Internt forbruk */
                    c2TTId       = "011"
                    bVarekjop    = TRUE
                    .
      WHEN 3 THEN ASSIGN /* Retur */
                    c2TTId       = "010"
                    .
      WHEN 4 THEN ASSIGN /* Reklamasjon */
                    c2TTId       = "003"
                    .
      WHEN 5 THEN ASSIGN /* Varemottak  */
                    c2TTId       = "005"
                    bVarekjop    = TRUE
                    .
      WHEN 6 THEN ASSIGN /* Lagerjustering */
                    c2TTId       = "007"
                    .
      WHEN 7 THEN ASSIGN /* Varetelling */
                    c2TTId       = "009"
                    .
      WHEN 9 THEN DO:    /* Internsalg = Overføring */
          /* Når det kommer overføringer fra Web butikk benyttes 0 i feilkode feltet for retur. */
          /* Retur fra Web butikk, vil si at det overføres fra Web butikken tilbake til de      */
          /* andre butikkene. Det vil si at den skal behandles på samme måte som vanlig.        */
          /* Ligger det imidlertid feilkode = 1, reserverer web butikken fra en av de andre     */
          /* butikkene. Da skal antall på varelinjen ganges med -1 for at overføring skal gå    */
          /* andre veien.                                                                       */
          ASSIGN /* Overføring */
                    c2TTId          = "006"
                    iMButikkNr      = int(ENTRY(9,BongLinje.OriginalData,";"))
                    cWebReservasjon = IF int(ENTRY(10,BongLinje.OriginalData,";")) = 1
                                         THEN 'RESERVASJON'
                                         ELSE ''
                    .
          FIND bufButiker NO-LOCK WHERE
              bufButiker.Butik = iMButikkNr NO-ERROR.
          IF AVAILABLE bufButiker THEN
          DO:
              IF bufButiker.KundeNr > 0  THEN
                  ASSIGN
                  BongHode.KundeNr = bufButiker.KundeNr
                  .
          END.

      END.
      WHEN 10 THEN ASSIGN
                   bTrening = TRUE
                   .
      WHEN 20 THEN ASSIGN /* Etiketter */
                    c2TTId       = "024"
                    .
      WHEN 19 THEN ASSIGN /* Etiketter batch fra varetelling */
                    c2TTId       = "025"
                    .
  END CASE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType062) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType062 Procedure 
PROCEDURE doTransType062 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  ASSIGN
      /* Bonglinje */
      BongLinje.Strekkode   = (ENTRY( 8,Bonglinje.Originaldata,";"))          /* KundeNr      */
      BongLinje.LinjeSum    = dec(ENTRY( 9,Bonglinje.Originaldata,";"))       /* FakturaNr    */ 
      BongLinje.BongTekst   = TRIM(ENTRY(10,Bonglinje.Originaldata,";"),'"')  /* Forfallsdato */
      BongLinje.BongPris    = BongLinje.LinjeSum
      /* Bonghode */
      BongHode.KundeKort    = TRIM(LEFT-TRIM(ENTRY( 8,Bonglinje.Originaldata,";"))) /* KundeNr      */
      BongHode.KortType     = 2 /* Kundekort. */
      .
      
  /* TN Her opprettes kunder som ikke finnes fra før. */
  IF NOT CAN-FIND(FIRST KundeKort WHERE
                  KundeKort.KortNr = BongHode.KundeKort)
    /* Er medlemskortet lagt på plass, legges det inn her. Hvis ikke påføres det senere i SjekkMedlem. */ 
    THEN RUN OpprettKundeMedlem (0,BongHode.KundeKort).
  ELSE DO:
    FIND FIRST KundeKort NO-LOCK WHERE
        KundeKort.KortNr = BongHode.KundeKort NO-ERROR.
    IF AVAILABLE KundeKort THEN 
      FIND Kunde OF KundeKort NO-LOCK NO-ERROR.
    IF AVAILABLE Kunde THEN 
      ASSIGN 
      BongHode.KundeNr   = Kunde.KundeNr
      BongHode.KundeNavn = Kunde.Navn.
  END.                  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType068) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType068 Procedure 
PROCEDURE doTransType068 :
/*------------------------------------------------------------------------------
  Purpose:     GrandTotal
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      BongLinje.LinjeRab    = dec(ENTRY( 8,Bonglinje.Originaldata,";"))       /* Z-Nummer   */
      BongLinje.LinjeSum    = dec(ENTRY( 9,Bonglinje.Originaldata,";")) / 100 /* GrandTotal */ 
      BongLinje.SubtotalRab = dec(ENTRY(10,Bonglinje.Originaldata,";"))       /* RingCount  */
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType073) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType073 Procedure 
PROCEDURE doTransType073 :
/*------------------------------------------------------------------------------
  Purpose:     INNBETALING
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    BongLinje.Antall  = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 /* Beløp  */ 
    bPakkevare        = IF BongLinje.Antall = 32754.0 
                              THEN TRUE
                              ELSE FALSE
    .


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType074) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType074 Procedure 
PROCEDURE doTransType074 :
/*------------------------------------------------------------------------------
  Purpose:     EOD
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iButikkNr LIKE Butiker.Butik    NO-UNDO.
  DEFINE VARIABLE dDato     AS DATE       NO-UNDO.
  DEFINE VARIABLE iRapptype AS INTEGER  INIT ?  NO-UNDO.
  DEFINE VARIABLE hRapport1 AS HANDLE    NO-UNDO.
  DEFINE VARIABLE hRapport2 AS HANDLE    NO-UNDO.
  
  ASSIGN
      BongLinje.LinjeSum    = dec(ENTRY(9,Bonglinje.Originaldata,";")) / 100 /* GrandTotal */ 
      BongLinje.LinjeRab    = dec(ENTRY(8,Bonglinje.Originaldata,";"))       /* Z-Nummer   */
      iRappType             = 11
      .

  
/*   RUN w-rkassarapportx.w PERSISTENT SET hRapport1.                    */
/*   RUN AutoInit IN hRapport1 (iRappType,BongHode.Butik,BongHode.Dato). */
/*                                                                       */
/*   ASSIGN                                                              */
/*       iRappType = 10                                                  */
/*       .                                                               */
/*                                                                       */
/*   RUN w-rkassarapportx.w PERSISTENT SET hRapport2.                    */
/*   RUN AutoInit IN hRapport2 (iRappType,BongHode.Butik,BongHode.Dato). */
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType075) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType075 Procedure 
PROCEDURE doTransType075 :
/*------------------------------------------------------------------------------
  Purpose:     KUNDEORDRE
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      BongLinje.Strekkode   = (ENTRY( 8,Bonglinje.Originaldata,";"))    /* KundeOrdreNr */
      BongLinje.Antall      = dec(ENTRY( 9,Bonglinje.Originaldata,";")) /* Antall solgt */ 
      BongLinje.BongTekst   = TRIM(ENTRY(10,Bonglinje.Originaldata,";"),'"')    /* Betalt flagg */
      BongLinje.LinjeRab    = DEC(ENTRY(11,Bonglinje.Originaldata,";")) /* RadNr        */
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType077) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType077 Procedure 
PROCEDURE doTransType077 :
/*------------------------------------------------------------------------------
  Purpose:     DEPOSITUM
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      BongLinje.LinjeSum   = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 /* Beløp */
      bDepositum           = TRUE
      BongLinje.TTId       = IF BongLinje.LinjeSum >= 0
                               THEN 73 /* Kunde har "kjøpt" et depositum.  */
                               ELSE 72 /* Kunden har betalt med depositum. */
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType078) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType078 Procedure 
PROCEDURE doTransType078 :
/*------------------------------------------------------------------------------
  Purpose: Kassereroppgjør    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLoop       AS INT NO-UNDO.
  DEF VAR piKassererId AS INT NO-UNDO.
  DEF VAR piForsNr     AS INT NO-UNDO.
  
  ASSIGN
      BongLinje.LinjeSum  = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100 /* Beløp  */ 
      piKassererId        = INT(ENTRY( 6,Bonglinje.Originaldata,";"))
      .
  /* Når kassen gir KASSEOPPGJØR, er kasserernummer = 0. Da plasseres oppgjøret */
  /* på den første kasserer vi finner i registeret.                             */
  IF piKassererId = 0 THEN
  DO:
      FIND FIRST Butikkforsalj NO-LOCK WHERE
          ButikkForsalj.Butik = INT(ENTRY( 1,Bonglinje.Originaldata,";")) NO-ERROR.
      IF AVAILABLE butikkforsalj THEN
          piKassererId = ButikkForsalj.KassererId.
      piForsNr = INT(ENTRY( 2,Bonglinje.Originaldata,";")).
  END.
  ELSE DO:
      /* Endrer kassererid til kasserernummer */
      FIND ButikkForsalj NO-LOCK WHERE
          ButikkForsalj.Butik = BongLinje.ButikkNr AND
          ButikkForsalj.KassererId = piKassererId NO-ERROR.
      IF AVAILABLE ButikkForsalj THEN
          piForsNr = ButikkForsalj.ForsNr.
      ELSE
          piForsNr = piKassererId.
  END.

  FIND KassererOppgj EXCLUSIVE-LOCK WHERE
    KassererOppgj.Dato       = BongHode.Dato AND
    KassererOppgj.ButikkNr   = BongLinje.ButikkNr AND
    KassererOppgj.KassererNr = piForsNr AND
    KassererOppgj.Z_Nummer   = 1 NO-ERROR.
/*   IF AVAILABLE KassererOppgj THEN */
/*     DELETE KassererOppgj.         */
  IF NOT AVAILABLE KassererOppgj THEN
  OPPRETT-OPPGJOR:
  DO:
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = BongLinje.ButikkNr NO-ERROR.

    CREATE KassererOppgj.
    ASSIGN
        KassererOppgj.Dato       = BongHode.Dato 
        KassererOppgj.ButikkNr   = BongLinje.ButikkNr 
        KassererOppgj.KassererNr = piForsNr 
        KassererOppgj.Z_Nummer   = 1
        .
  END. /* OPPRETT-OPPGJOR */

  ASSIGN
    KassererOppgj.OpptaltInnVeksel = KassererOppgj.OpptaltInnVeksel + 
                                     IF AVAILABLE Butiker
                                       THEN Butiker.StdVeksel
                                       ELSE 0
    .

  /* Default opprettelse av relaterte poster. */
  /* Oppretter bilag */
  IF NOT CAN-FIND(FIRST KassererBilag WHERE
              KassererBilag.ButikkNr     = BongHode.ButikkNr AND
              KassererBilag.Dato         = BongHode.Dato AND
              KassererBilag.KassererNr   = piForsNr AND
              KassererBilag.Z_Nummer     = 1) THEN
  DO piLoop = 1 TO 20:
      CREATE KassererBilag.
      ASSIGN
          KassererBilag.ButikkNr     = BongHode.ButikkNr 
          KassererBilag.Dato         = BongHode.Dato 
          KassererBilag.KassererNr   = piForsNr 
          KassererBilag.Z_Nummer     = 1
          KassererBilag.BilagsNr     = piLoop
          .
  END.
  FIND FIRST KassererBilag EXCLUSIVE-LOCK WHERE
      KassererBilag.ButikkNr     = BongHode.ButikkNr AND
      KassererBilag.Dato         = BongHode.Dato AND
      KassererBilag.KassererNr   = piForsNr AND
      KassererBilag.Z_Nummer     = 1.

  /* Legger inn default fra kasse */
  ASSIGN
  KassererBilag.Belop  = KassererBilag.Belop + 
                         KassererOppgj.OpptaltBilag
  KassererBilag.Meknad = "Kasse"
  .

  /* Oppretter valører */
  IF NOT CAN-FIND(FIRST KassererKontanter WHERE
              KassererKontanter.ButikkNr     = BongHode.ButikkNr AND
              KassererKontanter.Dato         = BongHode.Dato AND
              KassererKontanter.KassererNr   = piForsNr AND
              KassererKontanter.Z_Nummer     = 1) THEN
  DO:
      CREATE KassererKontanter.
      ASSIGN
          KassererKontanter.ButikkNr     = BongHode.ButikkNr 
          KassererKontanter.Dato         = BongHode.Dato 
          KassererKontanter.KassererNr   = piForsNr 
          KassererKontanter.Z_Nummer     = 1
          .
  END.
  FIND FIRST KassererKontanter EXCLUSIVE-LOCK WHERE
      KassererKontanter.ButikkNr     = BongHode.ButikkNr AND
      KassererKontanter.Dato         = BongHode.Dato AND
      KassererKontanter.KassererNr   = piForsNr AND
      KassererKontanter.Z_Nummer     = 1.
  /* Beløp fra kasse legges inn i KRONE valøren */
  ASSIGN
      KassererKontanter.Belop[2]        = KassererKontanter.Belop[2] +
                                          KassererOppgj.OpptaltKontanter
      KassererKontanter.AntallValor[2]  = KassererKontanter.AntallValor[2] + 
                                          KassererOppgj.OpptaltKontanter
      .

  /* Oppretter valuta */
  FOR EACH KasValuta NO-LOCK WHERE
      KasValuta.ValAktiv = TRUE AND
      KasValuta.EgenValuta = FALSE AND
      KasValuta.KasseValkurs <> 0:

      FIND KassererValuta EXCLUSIVE-LOCK WHERE
          KassererValuta.ButikkNr     = BongHode.ButikkNr AND
          KassererValuta.Dato         = BongHode.Dato AND
          KassererValuta.KassererNr   = piForsNr AND
          KassererValuta.Z_Nummer     = 1 AND
          KassererValuta.ValKod       = KasValuta.ValKod NO-ERROR.
      IF NOT AVAILABLE KassererValuta THEN
      DO:
          CREATE KassererValuta.
          ASSIGN
              /* Nøkkel */
              KassererValuta.ButikkNr     = BongHode.ButikkNr 
              KassererValuta.Dato         = BongHode.Dato 
              KassererValuta.KassererNr   = piForsNr 
              KassererValuta.Z_Nummer     = 1
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

  /* Gammelt record layout - Opp til og med 14 entries.*/
  ASSIGN
    KassererOppgj.OpptaltKontanter  = DEC(ENTRY(  8,Bonglinje.Originaldata,";")) / 100
    KassererOppgj.OpptaltSjekk      = DEC(ENTRY(  9,Bonglinje.Originaldata,";")) / 100
    KassererOppgj.OpptaltReserve    = DEC(ENTRY( 10,Bonglinje.Originaldata,";")) / 100
    KassererOppgj.OpptaltGavekort   = DEC(ENTRY( 11,Bonglinje.Originaldata,";")) / 100
    KassererOppgj.OpptaltBilag      = DEC(ENTRY( 12,Bonglinje.Originaldata,";")) / 100
    KassererOppgj.OpptaltTilgode    = DEC(ENTRY( 13,Bonglinje.Originaldata,";")) / 100
    KassererOppgj.OpptaltLevertBank = DEC(ENTRY( 14,Bonglinje.Originaldata,";")) / 100
    .
  /* Nytt layout */
  IF NUM-ENTRIES(BongLinje.Originaldata,";") >= 18 THEN
  DO:
      ASSIGN /* Kassereroppgjør skal slås sammen. */
      KassererOppgj.OpptaltGavekortAndre    = DEC(ENTRY( 15,Bonglinje.Originaldata,";")) / 100
      KassererOppgj.OpptaltGavekortUtlevert = DEC(ENTRY( 16,Bonglinje.Originaldata,";")) / 100
      KassererOppgj.OpptaltTilgodeAndre     = DEC(ENTRY( 17,Bonglinje.Originaldata,";")) / 100
      KassererOppgj.OpptaltTilgodeUtlevert  = DEC(ENTRY( 18,Bonglinje.Originaldata,";")) / 100
      .
  END.
  /* Bankpose nummer */
  IF NUM-ENTRIES(BongLinje.Originaldata,";") >= 19 THEN
      KassererOppgj.PoseNr = TRIM(ENTRY(19,Bonglinje.Originaldata,";"),'"').
  /* Finansiering */
  IF NUM-ENTRIES(BongLinje.Originaldata,";") >= 20 THEN
      KassererOppgj.OpptaltFinansiering = DEC(ENTRY(20,Bonglinje.Originaldata,";")) / 100.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType080) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType080 Procedure 
PROCEDURE doTransType080 :
/*------------------------------------------------------------------------------
  Purpose:     INNBETALING KUNDE
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      BongLinje.LinjeSum   = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100    /* Beløp */
      BongLinje.BongPris   = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100    /* Beløp */
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType082) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType082 Procedure 
PROCEDURE doTransType082 :
/*------------------------------------------------------------------------------
  Purpose:     KUNDEINFO
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
        
  ASSIGN
      BongLinje.Strekkode = ENTRY(9,Bonglinje.Originaldata,";")
      BongLinje.BongTekst = TRIM(ENTRY(8,Bonglinje.Originaldata,";"),'"')
      BongLinje.RefNr     = int(ENTRY(9,Bonglinje.Originaldata,";"))
      BongLinje.RefTekst  = TRIM(ENTRY(8,Bonglinje.Originaldata,";"),'"')
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType088) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType088 Procedure 
PROCEDURE doTransType088 :
/*------------------------------------------------------------------------------
  Purpose:     Tekst bankterminal (Eldre kasseversjoner)
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      BongLinje.BongTekst   = TRIM(ENTRY(8,Bonglinje.Originaldata,";"),'"') /* Tekst */ 
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType091) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType091 Procedure 
PROCEDURE doTransType091 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      BongLinje.StrekKode  = (ENTRY( 8,Bonglinje.Originaldata,";"))          /* Varenummer  */ 
      BongLinje.LinjeSum   = DEC(ENTRY( 9,Bonglinje.Originaldata,";")) / 100 /* Ny pris     */ 
      BongLinje.LinjeRab   = DEC(ENTRY(10,Bonglinje.Originaldata,";")) / 100 /* Gammel pris */ 
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType092) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType092 Procedure 
PROCEDURE doTransType092 :
/*------------------------------------------------------------------------------
  Purpose:     Fri tekst/tekst fra bankterminal
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      BongLinje.BongTekst  = TRIM(ENTRY( 8,Bonglinje.Originaldata,";"),'"')    /* Tekst  */ 
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType094) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType094 Procedure 
PROCEDURE doTransType094 :
/*------------------------------------------------------------------------------
  Purpose:     Tilgodelapp
  Parameters:  <none>
  Notes:       cGaveTilgId iButAndre
------------------------------------------------------------------------------*/
  DEF VAR plTilbake  AS DEC NO-UNDO.
  
  DEF BUFFER bBongLinje  FOR BongLinje.
  DEF BUFFER b2BongLinje FOR BongLinje.

  ASSIGN
      BongLinje.StrekKode = cGaveTilgId2 
      BongLinje.LinjeSum  = DEC(ENTRY( 9,Bonglinje.Originaldata,";")) / 100 /* Beløp          */ 
      BongLinje.Antall    = DEC(ENTRY(12,Bonglinje.Originaldata,";"))       /* KortType       */ 
      plTilbake           = DEC(ENTRY(10,Bonglinje.Originaldata,";")) / 100 /* Penger tilbake */ 
      BongLinje.BongTekst = IF lGaveEget 
                              THEN "0,Eget," + STRING(iButAndre) + "," + string(lGaveTilgType)
                              ELSE "1,Andre," + STRING(iButAndre) + "," + string(lGaveTilgType)
      lGaveEget           = TRUE
      .
  /* Mottatte tilgode og gavekort som er kommet fra en annen butikk. */
  IF iButAndre <> 0 THEN
      ASSIGN
      BongLinje.MButikk = iButAndre.
      iButAndre         = 0
      .

  /* Spesiell håndtering av tilgodelapper. Gjort for å håndtere at 
     det bongen legges ut slik:
   278;4;12/01/04;58920;3196;4;102;251;"Andreas"
   278;4;12/01/04;58920;3196;4;106;3
   278;4;12/01/04;58920;3196;4;1003;0000000000000;0;"SWIX 12801 JAKKE M";1.000;1099.00;482.00;24.00;212.71;0;0;1;0.00;0;0.00;0;0.00
   278;4;12/01/04;58920;3196;4;106;3
   278;4;12/01/04;58920;3196;4;3;2046604105480;2000;"SWIX 12801 JAKKE M";-1.000;-769.30;-482.00;24.00;-148.90;0;0;9;-329.70;0;0.00;0;0.00
   278;4;12/01/04;58920;3196;4;104;94;209
   278;4;12/01/04;58920;3196;4;94;-769.30;769.50;769.50;-0.20;0
   Her ser vi at på siste linje er subtotal neg, samtidig som det er innbetalt et beløp og kunden
   skal også ha penger tilbake. Ref. bong fra Lefstad.
   Nb: Denne feilen er rettet i journalutlegget. Koden er lagt inn for å håndtere utlegg
   i gamle journalfiler.
 */
  IF iSiste = BongLinje.LinjeNr AND (DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100) < 0 AND
      (DEC(ENTRY( 9,Bonglinje.Originaldata,";")) / 100) = (DEC(ENTRY( 10,Bonglinje.Originaldata,";")) / 100)  THEN
  SPESIAL-TILGODE-UT:
  DO:
      ASSIGN
          /*BongLinje.LinjeSum = plTilbake * -1*/
          BongLinje.TTID     = (IF bUtbetType = 94
                                             THEN 69 /* Tilgodelapp ut */
                                             ELSE 50 /* Kontant ut     */)
          BongLinje.LinjeSum = IF (DEC(ENTRY( 8,Bonglinje.Originaldata,";")) / 100) < 0 AND BongLinje.LinjeSum > 0
                               THEN BongLinje.LinjeSum * -1
                               ELSE BongLinje.LinjeSum
          .
  END. /* SPESIAL-TILGODE-UT */
  ELSE STANDARD: DO:
      /* Beløpet er negativt og det utleveres kontant eller tilgodelapp. */
      IF iSiste = BongLinje.LinjeNr AND BongLinje.LinjeSum < 0 THEN
      TILGODEUT:
      DO:
          ASSIGN
              /*BongLinje.LinjeSum = plTilbake * -1*/
              BongLinje.TTID     = (IF bUtbetType = 94
                                                 THEN 69 /* Tilgodelapp ut */
                                                 ELSE 50 /* Kontant ut     */).
      END. /* TILGODEUT */
      ELSE IF iSiste = BongLinje.LinjeNr AND plTilbake > 0 THEN
      NYUTTRANS:
      DO:
          FIND LAST b2BongLinje NO-LOCK WHERE
              b2BongLinje.B_Id = BongHode.B_Id NO-ERROR.

          IF AVAILABLE b2BongLinje THEN
          DO:
            IF bUtbetType <> 94 THEN
            DO: /* Kontant tilbake */
                CREATE bBongLinje.
                BUFFER-COPY b2BongLinje TO bBongLinje
                    ASSIGN
                      bBongLinje.LinjeNr      = b2BongLinje.LinjeNr + 1
                      bBongLinje.Strekkode    = cGaveTilgId2
                      bBongLinje.LinjeSum     = plTilbake * -1
                      bBongLinje.TTId         = 50 /* Kontant ut */
                      bBongLinje.Originaldata = ";;;;;;;;"
                      .
                RELEASE bBongLinje.
            END.
            ELSE /* Tilgodelapp tilbake. */
                ASSIGN
                lTilgodeUt = plTilbake * -1
                .
          END.
      END. /* NYUTTRANS */
  END.
     

  ASSIGN
      cGaveTilgId2 = ""
      .
  /* Håndtering av avrunding hvis dette er siste betalingstrans på bongen. */
  IF BongLinje.LinjeNr = iSiste THEN
  DO:
    ASSIGN
      lAvrunding = DEC(ENTRY(11,Bonglinje.Originaldata,";")) / 100 /* Avrunding   */ 
      .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType095) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType095 Procedure 
PROCEDURE doTransType095 PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     Feilkode/tiltakskode 
               
  Parameters:  <none>
  Notes:       Denne transaksjonen kommer KUN når det er angitt koder. Dvs
               at det må testes og flagges retur i behandling av varetrans 
               også.
------------------------------------------------------------------------------*/
  ASSIGN
      BongLinje.LinjeSum  = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) /* Feilkode    */ 
      BongLinje.LinjeRab  = DEC(ENTRY( 9,Bonglinje.Originaldata,";")) /* Tiltakskode */ 
      BongLinje.FeilKode  = int(ENTRY(8,Bonglinje.Originaldata,";"))
      BongLinje.NotatKode = int(ENTRY(9,Bonglinje.Originaldata,";"))  /* Tiltakskode */
      BongLinje.BongTekst = "REKLAMASJONSKODER"
      bRetur              = TRUE
      .
  IF BongLinje.Linjesum > 1 OR BongLinje.LinjeRab > 1 THEN
      ASSIGN
      bRetur = FALSE
      bReklamasjon = TRUE
      .

  /* Validerer feilkode */
  IF NOT CAN-FIND(FIRST FeilKode WHERE
                  FeilKode.FeilKode = INT(ENTRY( 8,Bonglinje.Originaldata,";"))) THEN
  ASSIGN
    cError = cError + 
             (IF cError = ""
                THEN ""
                ELSE CHR(10)) + 
             "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
             " Ukjent feilkode på transaksjonen " + ENTRY( 8,Bonglinje.Originaldata,";") + 
             "."
    cFilError = cFilError + 
             (IF cFilError = ""
                THEN ""
                ELSE "|") + 
             " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
             " Ukjent feilkode på transaksjonen " + ENTRY( 8,Bonglinje.Originaldata,";") + 
             "." + CHR(1) + "2"
    BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
    .

  /* Validerer tiltakskode. */
  IF NOT CAN-FIND(FIRST Handtering WHERE
                  Handtering.HandKode = INT(ENTRY( 9,Bonglinje.Originaldata,";"))) THEN
  ASSIGN
    cError = cError + 
             (IF cError = ""
                THEN ""
                ELSE CHR(10)) + 
             "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
             " Ukjent tiltakskode på transaksjonen " + ENTRY( 9,Bonglinje.Originaldata,";") + 
             "."
    cFilError = cFilError + 
             (IF cFilError = ""
                THEN ""
                ELSE "|") + 
             " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
             " Ukjent tiltakskode på transaksjonen " + ENTRY( 9,Bonglinje.Originaldata,";") + 
             "." + CHR(1) + "2"
    BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
    .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType102) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType102 Procedure 
PROCEDURE doTransType102 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      BongLinje.Strekkode =     ENTRY( 8,Bonglinje.Originaldata,";")
      BongLinje.BongTekst = TRIM(ENTRY( 9,Bonglinje.Originaldata,";"),'"')
      BongHode.SelgerNr  = IF INT(ENTRY( 8,Bonglinje.Originaldata,";")) = ? THEN 0 ELSE INT(ENTRY( 8,Bonglinje.Originaldata,";")) 
      .

  IF BongHode.SelgerNr <> 0 THEN
  DO:
      /* Konverterer selgernummer */
      FIND ButikkSelger NO-LOCK WHERE
          ButikkSelger.ButikkNr = BongHode.ButikkNr AND
          ButikkSelger.SelgerId = BongHode.SelgerNr NO-ERROR.
      IF AVAILABLE ButikkSelger THEN
          ASSIGN
          BongHode.SelgerNr = ButikkSelger.SelgerNr.

      FIND Selger NO-LOCK WHERE
          Selger.SelgerNr = BongHode.SelgerNr NO-ERROR.
      IF NOT AVAILABLE Selger THEN
      DO:
          ASSIGN  
            BongHode.SelgerNavn = "** Ukjent selger **"
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE CHR(10)) + 
                     "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Ukjent selger på transaksjonen " + STRING(BongHode.SelgerNr) + 
                     "."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Ukjent selger på transaksjonen " + STRING(BongHode.SelgerNr) + 
                     "." + CHR(1) + "2".
            BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
            .
      END.
      ELSE DO:
          ASSIGN
              BongHode.SelgerNavn = Selger.Navn
              .
      END.

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType104) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType104 Procedure 
PROCEDURE doTransType104 :
/*------------------------------------------------------------------------------
  Purpose:     Utlever tilgode
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
      BongLinje.Bongtekst  = ENTRY( 8,Bonglinje.Originaldata,";") /* Utbetaltype */ 
      BongLinje.Strekkode  = ENTRY( 9,Bonglinje.Originaldata,";") /* Nummer på tilgodelapp */
      cGaveTilgId2         = ENTRY( 9,Bonglinje.Originaldata,";")
      .
  IF INT(BongLinje.BongTekst) = 94 THEN
    bUtbetType = 94.
  ELSE
    bUtbetType = 0.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType105) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType105 Procedure 
PROCEDURE doTransType105 :
/*------------------------------------------------------------------------------
  Purpose:     Mottak av Gavekort/tilgodelapp
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
      BongLinje.Antall     = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) /* Butikknummer */ 
      BongLinje.Strekkode  = TRIM(ENTRY( 9,Bonglinje.Originaldata,";"),'"') /* Nummer på Gavekort/tilgodelapp */
      cGaveTilgId2         = TRIM((ENTRY( 9,Bonglinje.Originaldata,";")),'"')
      lGaveTilgType        = IF NUM-ENTRIES(Bonglinje.Originaldata,";") >= 10
                               THEN INT(DEC(ENTRY(10,Bonglinje.Originaldata,";"))) /* Gavekorttype */
                               ELSE 0
      lGaveEget            = IF BongLinje.ButikkNr = int(BongLinje.Antall) 
                               THEN TRUE  /* Eeget gavekort */
                               ELSE FALSE /* Andres - skal faktureres. */
      iButAndre            = BongLinje.Antall
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType106) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType106 Procedure 
PROCEDURE doTransType106 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* For Preem lagres alltid StrKoden i Bongtekst feltet */
  /* For å bevare den opprinnelige strekkoden/VareNr.    */
  ASSIGN
      BongLinje.BongTekst = LEFT-TRIM(ENTRY( 8,Bonglinje.Originaldata,";"),"0")
      .
  /* Ordinær håndtering av størrelseskoden. */
  ASSIGN
      BongLinje.Antall  = INT((ENTRY( 8,Bonglinje.Originaldata,";")))    /* Størrelseskode  */ 
      NO-ERROR.
  IF ERROR-STATUS:ERROR = FALSE THEN
  DO:
      FIND StrKonv NO-LOCK WHERE
          StrKonv.StrKode = int(BongLinje.Antall) NO-ERROR.
      IF AVAILABLE StrKonv THEN
          ASSIGN
          cStorl              = StrKonv.Storl
          BongLinje.Storrelse = StrKonv.Storl
          .
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTranstype108) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTranstype108 Procedure 
PROCEDURE doTranstype108 :
/*------------------------------------------------------------------------------
  Purpose:     INNBETALING
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
      BongLinje.Antall    = INT(ENTRY( 8,Bonglinje.Originaldata,";")) /* Antall */ 
      BongLinje.LinjeSum  = DEC(ENTRY( 9,Bonglinje.Originaldata,";")) / 100 /* Beløp  */ 
      BongLinje.LinjeRab  = IF NUM-ENTRIES(Bonglinje.Originaldata,";") >= 12
                              THEN DEC(ENTRY(12,Bonglinje.Originaldata,";")) / 100 /* Beløp  */ 
                              ELSE 0.0
      BongLinje.LinjeSum  = BongLinje.LinjeSum + BongLinje.LinjeRab
      BongLinje.BongPris  = BongLinje.LinjeSum
      BongLinje.Strekkode = TRIM(ENTRY(10,Bonglinje.Originaldata,";"),'"') /* GavekortIdent  */ 
      BongLinje.BongTekst = ENTRY(11,Bonglinje.Originaldata,";") /* Gyldig fra dato */ 
      .
  IF (bRetur OR bReklamasjon) AND NOT bVareretur THEN
      ASSIGN
      BongLinje.LinjeSum  = BongLinje.LinjeSum * -1
      BongLinje.BongPris  = BongLinje.LinjeSum * -1
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType111) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType111 Procedure 
PROCEDURE doTransType111 PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     FINANSIERING.
  Parameters:  <none>
  Notes:         
------------------------------------------------------------------------------*/
  
  RUN doTranstype055.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType112) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType112 Procedure 
PROCEDURE doTransType112 :
/*------------------------------------------------------------------------------
  Purpose:     Mottak av Gavekort/tilgodelapp - Med type av kort.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN
      BongLinje.Antall     = DEC(ENTRY( 8,Bonglinje.Originaldata,";")) /* Butikknummer */ 
      BongLinje.Strekkode  = TRIM(ENTRY( 9,Bonglinje.Originaldata,";"),'"') /* Nummer på Gavekort/tilgodelapp */
      BongLinje.BongTekst  = ENTRY(10,Bonglinje.Originaldata,";") /* Gavekorttype */
      cGaveTilgId          = TRIM((ENTRY( 9,Bonglinje.Originaldata,";")),'"')
      lGaveTilgType        = INT(DEC(ENTRY(10,Bonglinje.Originaldata,";"))) /* Gavekorttype */
      lGaveEget            = IF BongLinje.ButikkNr = int(BongLinje.Antall) 
                               THEN TRUE  /* Eeget gavekort */
                               ELSE FALSE /* Andres - skal faktureres. */
      iButAndre            = BongLinje.Antall
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType113) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType113 Procedure 
PROCEDURE doTransType113 :
/*------------------------------------------------------------------------------
  Purpose:     Mottak av Gavekort/tilgodelapp - Med type av kort.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cActionType AS CHARACTER  NO-UNDO.
  ASSIGN cActionType = ENTRY(8,Bonglinje.Originaldata,";").
  CASE cActionType:
      WHEN "1000" THEN DO:
          ASSIGN BongLinje.BongTekst = "Skiftnr: " + ENTRY(10,Bonglinje.Originaldata,";") /* SkiftNre */
                 BongHode.SkiftId    = DECI(ENTRY(11,Bonglinje.Originaldata,";"))
                 BongHode.SkiftNr    = INT(ENTRY(10,Bonglinje.Originaldata,";"))
                 .
        /*       BongLinje.Antall     = DEC(ENTRY( 9,Bonglinje.Originaldata,";")) /* BongNr som skiftet skal inn på. */ */
      END.
      WHEN "1001" THEN DO:
          ASSIGN BongLinje.BongTekst = "Pris/st.: " + ENTRY( 9,Bonglinje.Originaldata,";")
                 dPrisprSalgsenhet = DEC(ENTRY( 9,Bonglinje.Originaldata,";")) NO-ERROR.
          IF dPrisprSalgsenhet = ? THEN
              ASSIGN dPrisprSalgsenhet = 0.
      END.
  END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransType125) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransType125 Procedure 
PROCEDURE doTransType125 :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

  ASSIGN
      BongLinje.Strekkode = ENTRY( 8,Bonglinje.Originaldata,";")  
      Bonglinje.BongTekst = "Alternativt varenr."
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransTypeAvrunding) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransTypeAvrunding Procedure 
PROCEDURE doTransTypeAvrunding :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plAvrunding AS DEC NO-UNDO.

  DEF BUFFER bBongLinje FOR BongLinje.
  DEF BUFFER b2BongLinje FOR BongLinje.

  FIND LAST b2BongLinje NO-LOCK WHERE
      b2BongLinje.B_Id = BongHode.B_Id NO-ERROR.

  IF AVAILABLE b2BongLinje THEN
  DO:
      CREATE bBongLinje.
      BUFFER-COPY b2BongLinje TO bBongLinje
          ASSIGN
            bBongLinje.LinjeNr  = b2BongLinje.LinjeNr + 1
            bBongLinje.LinjeSum = plAvrunding * -1
            bBongLinje.TTId     = 78
            bBongLinje.Originaldata = ";;;;;;;;"
          .
    RELEASE bBongLinje.
  END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransTypeCashBack) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransTypeCashBack Procedure 
PROCEDURE doTransTypeCashBack :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plCashBack AS DEC NO-UNDO.

  DEF BUFFER bBongLinje  FOR BongLinje.
  DEF BUFFER b2BongLinje FOR BongLinje.

  DEF VAR plKunRetur AS LOG NO-UNDO.
  DEF VAR piAntVre   AS INT NO-UNDO.

  ASSIGN
      plKunRetur = TRUE  
      .

  /* TN 20/12-03 Midlertidig fiks til avklaring kommer fra LRS.            */
  /* Analyser bong for å se om det skal legges opp vekseltransaksjon.      */
  /* Er det bare returtransaksjoner på bongen og kun en betalingstrans,    */
  /* skal det ikke legges opp vekseltransaksjon.                           */
  FOR EACH b2BongLinje WHERE
      b2BongLinje.B_Id = BongHode.B_Id:

      IF b2BongLinje.TTId < 50 THEN
      DO:
          ASSIGN
              piAntVre = piAntVre + 1
              .
          IF NOT CAN-DO("003,010",STRING(b2BongLinje.TTId,"999")) THEN
              plKunRetur = FALSE.
      END.
  END.
  /* Inneholder bongen ikke varesalgstranser, skal veksel håndtering være normal */
  IF piAntVre = 0 THEN
      plKunRetur = FALSE.

  FIND LAST b2BongLinje NO-LOCK WHERE
      b2BongLinje.B_Id = BongHode.B_Id NO-ERROR.

  IF AVAILABLE b2BongLinje AND b2BongLinje.TTId <> 67 AND plKunRetur = FALSE THEN
  DO:
      CREATE bBongLinje.
      BUFFER-COPY b2BongLinje TO bBongLinje
          ASSIGN
            bBongLinje.LinjeNr      = b2BongLinje.LinjeNr + 1
            bBongLinje.LinjeSum     = plCashBack * -1
            bBongLinje.TTId         = 67
            bBongLinje.Originaldata = ";;;;;;;;"
            .
      RELEASE bBongLinje.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransTypeRabatt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransTypeRabatt Procedure 
PROCEDURE doTransTypeRabatt :
/*------------------------------------------------------------------------------
  Purpose:     SUBTOTALRABATT
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF BUFFER bBongLinje FOR BongLinje.
  DEF BUFFER b2BongLinje FOR BongLinje.

  FIND LAST b2BongLinje NO-LOCK WHERE
      b2BongLinje.B_Id = BongHode.B_Id NO-ERROR.

  IF AVAILABLE b2BongLinje THEN
  DO:
      CREATE bBongLinje.
      BUFFER-COPY b2BongLinje TO bBongLinje
          ASSIGN
            bBongLinje.LinjeNr  = b2BongLinje.LinjeNr + 1
            bBongLinje.LinjeSum = lSubTotalRab * -1
            bBongLinje.TTId     = 63
            bBongLinje.Originaldata = ";;;;;;;;"
            lSubTotalRab        = 0
            .
      RELEASE bBongLinje.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransTypeSlettet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransTypeSlettet Procedure 
PROCEDURE doTransTypeSlettet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
      BongLinje.B_Id = BongHode.B_ID:
      ASSIGN
          BongLinje.Makulert = TRUE
          .
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransTypeTilgodeUt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransTypeTilgodeUt Procedure 
PROCEDURE doTransTypeTilgodeUt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plTilgodeUt AS DEC NO-UNDO.

  DEF VAR pcIdentNr AS CHAR NO-UNDO.
  DEF VAR piLoop    AS INT  NO-UNDO.

  DEF BUFFER bBongLinje  FOR BongLinje.
  DEF BUFFER b2BongLinje FOR BongLinje.

  FIND LAST b2BongLinje NO-LOCK WHERE
      b2BongLinje.B_Id = BongHode.B_Id NO-ERROR.

  IF AVAILABLE b2BongLinje THEN
  DO:
      ASSIGN
          pcIdentNr = IF cGaveTilgId2 <> ""
                        THEN cGaveTilgId2
                        ELSE b2BongLinje.Strekkode
          .

      FIND Tilgode EXCLUSIVE-LOCK WHERE
          Tilgode.ButNr   = BongLinje.ButikkNr AND
          Tilgode.IdentNr = pcIdentNr NO-ERROR.
      /* Her skal vi ikke finne noe kort fra før. */
      IF AVAILABLE Tilgode THEN
      DO:
          pcIdentNr = pcIdentNr + "-TILG".
          FIND Tilgode EXCLUSIVE-LOCK WHERE
               Tilgode.ButNr   = BongLinje.ButikkNr AND
               Tilgode.IdentNr = pcIdentNr NO-ERROR.
      END.
      /* Finner vi alikevel et kort, opprettes et nytt id. */
      IF AVAILABLE Tilgode THEN
      ERRORLOOP:
      DO WHILE TRUE:
          piLoop = piLoop + 1.
          FIND Tilgode WHERE
              Tilgode.ButNr    = BongLinje.ButikkNr AND
              Tilgode.IdentNr  = pcIdentNr + "-TILG" + string(piLoop) NO-ERROR.
          IF AVAILABLE Tilgode THEN
              NEXT ERRORLOOP.
          /* Nytt Id */
          pcIdentNr = pcIdentNr + "-TILG" + string(piLoop).
          LEAVE ERRORLOOP.
      END. /* ERRORLOOP */        
      ELSE
          pcIdentNr = pcIdentNr.

      CREATE bBongLinje.
      BUFFER-COPY b2BongLinje TO bBongLinje
          ASSIGN
            bBongLinje.LinjeNr      = b2BongLinje.LinjeNr + 1
            bBongLinje.LinjeSum     = plTilgodeUt
            bBongLinje.TTId         = 69
            bBongLinje.Strekkode    = pcIdentNr
            bBongLinje.Originaldata = ";;;;;;;;"
            bBongLinje.BongTekst    = "0,Eget," + STRING(b2BongLinje.ButikkNr)
            .
      RELEASE bBongLinje.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTranstypeTrening) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTranstypeTrening Procedure 
PROCEDURE doTranstypeTrening :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
      BongLinje.B_Id = BongHode.B_ID:
      ASSIGN
          BongLinje.Makulert = TRUE
          .
  END.
  ASSIGN
      bTrening = FALSE
      .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransTypeVarekjop) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransTypeVarekjop Procedure 
PROCEDURE doTransTypeVarekjop :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
      BongLinje.B_Id       = BongHode.B_ID AND
      DECIMAL(BongLinje.ArtikkelNr) > 0 AND
      BongLinje.Makulert   = FALSE:

      /* Tar kun relevante linjer. 002-Brekkasje, 005-varekjøp, 011-Internt forbruk*/
      IF NOT CAN-DO("002,005,011",STRING(BongLinje.TTId)) THEN
          NEXT.

      DO:
          /* Endrer prisen på varemottakstransaksjoner */
          FIND Lager NO-LOCK WHERE
              Lager.ArtikkelNr = ArtBas.ArtikkelNr  AND
              Lager.Butik      = BongLinje.ButikkNr NO-ERROR.
      
          IF ArtBas.Lager = FALSE OR NOT AVAILABLE Lager OR Lager.VVareKost = 0 OR Lager.VVarekost = ? THEN
          DO:
              FIND Butiker NO-LOCK WHERE
                  Butiker.Butik = BongLinje.ButikkNr NO-ERROR.
              IF AVAILABLE Butiker THEN
                  FIND ArtPris NO-LOCK WHERE
                       ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                       ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
              IF AVAILABLE ArtPris THEN
                  BongLinje.LinjeSum = ArtPris.Varekost[IF ArtPris.tilbud
                                                          THEN 2
                                                          ELSE 1] * BongLinje.Antall.
          END.
          ELSE
              BongLinje.LinjeSum = Lager.VVareKost * BongLinje.Antall.
          ASSIGN
              BongLinje.MvaKr = 0
              .
      END.
  END.
          .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-doTransTypeVeksel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE doTransTypeVeksel Procedure 
PROCEDURE doTransTypeVeksel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER plVeksel AS DEC NO-UNDO.

  DEF BUFFER bBongLinje  FOR BongLinje.
  DEF BUFFER b2BongLinje FOR BongLinje.

  DEF VAR plKunRetur AS LOG NO-UNDO.
  DEF VAR piAntVre   AS INT NO-UNDO.

  ASSIGN
      plKunRetur = TRUE  
      .

  /* TN 20/12-03 Midlertidig fiks til avklaring kommer fra LRS.            */
  /* Analyser bong for å se om det skal legges opp vekseltransaksjon.      */
  /* Er det bare returtransaksjoner på bongen og kun en betalingstrans,    */
  /* skal det ikke legges opp vekseltransaksjon.                           */
  FOR EACH b2BongLinje WHERE                                               
      b2BongLinje.B_Id = BongHode.B_Id:                                    
                                                                           
      IF b2BongLinje.TTId < 50 OR b2BongLinje.TTId = 62 THEN               
      DO:                                                                  
          ASSIGN                                                           
              piAntVre = piAntVre + 1                                      
              .                                                            
          IF NOT CAN-DO("003,010,062",STRING(b2BongLinje.TTId,"999")) THEN 
              plKunRetur = FALSE.                                          
      END.                                                                 
  END.                                                                     
  /* Inneholder bongen ikke varesalgstranser, skal veksel håndtering være normal */
  IF piAntVre = 0 THEN
      plKunRetur = FALSE.

  FIND LAST b2BongLinje NO-LOCK WHERE
      b2BongLinje.B_Id = BongHode.B_Id NO-ERROR.

  IF AVAILABLE b2BongLinje AND plKunRetur = FALSE THEN
  DO:
      IF bUtbetType <> 94 THEN
      DO: /* Veksel tilbake */
          CREATE bBongLinje.
          BUFFER-COPY b2BongLinje TO bBongLinje
              ASSIGN
                bBongLinje.LinjeNr      = b2BongLinje.LinjeNr + 1
                bBongLinje.LinjeSum     = plVeksel * -1
                bBongLinje.TTId         = 70
                bBongLinje.Originaldata = ";;;;;;;;"
                bBongLinje.Strekkode    = ""
                bBongLinje.BongTekst    = ""
                bUtbetType = 0
                .
          RELEASE bBongLinje.
      END.
      ELSE /* Gavekort tilbake */
          ASSIGN
              lTilgodeUt = plVeksel * -1
              .
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
    DEF INPUT-OUTPUT PARAMETER pcStorl AS CHAR NO-UNDO.

    DEF VAR X      AS INT NO-UNDO.
    DEF VAR xx     AS INT NO-UNDO.
    DEF VAR i      AS INT NO-UNDO.
    DEF VAR dstorl AS DEC NO-UNDO.

        /* Tester om det er numeriske st|rrelser.                  */
        /* Er st|rrelsene numerisk, vil xx bli lik 4, ellers ikke. */
        /* Alfanumeriske st|rrelser, passerer rett igjennom.       */
        STR-TEST:
        DO:
            /* Er det numeriske st|rrelser, skal st|rrelsen formateres om */
            xx = 0.
            DO i = 1 TO 4 :
                IF SUBSTRING(pcStorl,i,1) >= "0" AND
                SUBSTRING(pcStorl,i,1) <= "9"
                THEN DO :
                    xx = xx + 1.
                    IF xx = 4 THEN DO:
                        /* Numeriske st|rrelser */
                        dstorl = DECIMAL(pcStorl) / 10.
                        pcStorl = STRING(dstorl).
                        /* Konverterer st|rrelse "0" til " 1". */
                        IF INTEGER(pcStorl) = 0 THEN pcStorl = "1".
                    END.
                END.
            END.
 
            /* Plokker bort alle blanke som st}r i f|rste posisjon. */
            PLOKK:
            REPEAT:
                IF SUBSTRING(pcStorl,1,1) = " " THEN
                DO:
                    pcStorl = SUBSTRING(pcStorl,2,LENGTH(pcStorl)).
                    NEXT PLOKK.
                END.
                ELSE LEAVE PLOKK.
            END. /* PLOKK */
 
            /* Formaterer st|rrelsen slik at den sorteres riktig i skotex. */
            IF LENGTH(pcStorl) = 3 OR LENGTH(pcStorl) = 1 THEN
                pcStorl = " " + pcStorl.
        END. /* STR-TEST */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-HentVareLinjeInfo) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HentVareLinjeInfo Procedure 
PROCEDURE HentVareLinjeInfo :
/*------------------------------------------------------------------------------
  Purpose:     Hent varelinjeinfo
  Parameters:  <none>
  Notes:       

------------------------------------------------------------------------------*/
  /* Henter mva koden.                                      */
/*  FIND Moms NO-LOCK WHERE                                       */
/*    Moms.MomsKod = BongLinje.MvaGr NO-ERROR.                    */
/*  IF NOT AVAILABLE Moms THEN                                    */
/*    FIND LAST Moms NO-LOCK WHERE                                */
/*      Moms.MomsProc = BongLinje.Mva%  USE-INDEX momsin NO-ERROR.*/
/*  IF AVAILABLE Moms THEN                                        */
/*      ASSIGN                                                    */
/*      BongLinje.MvaGr         = Moms.MomsKod                    */
/*      BongLinje.MvaGruppeNavn = Moms.Beskrivelse                */
/*      .                                                         */

  FIND FIRST Moms NO-LOCK WHERE
    Moms.MomsProc = BongLinje.Mva% USE-INDEX momsin NO-ERROR.
  IF AVAILABLE Moms THEN
      ASSIGN
      BongLinje.MvaGr         = Moms.MomsKod
      BongLinje.MvaGruppeNavn = Moms.Beskrivelse
      .
  ELSE DO:
      IF Bonglinje.Makulert = FALSE THEN
      ASSIGN
      /*BongLinje.MvaGr         = 0*/
      BongLinje.MvaGruppeNavn = "** Ukjent momskode **"
      cError = cError + 
               (IF cError = ""
                  THEN ""
                  ELSE CHR(10)) + 
               "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
               " Ukjent MvaKode på transaksjonen " + string(BongLinje.MvaGr) + 
               "."
      cFilError = cFilError + 
               (IF cFilError = ""
                  THEN ""
                  ELSE "|") + 
               " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
               " Ukjent MvaKode på transaksjonen " + string(BongLinje.MvaGr) + 
               "." + CHR(1) + "2"
      .
  END.

  /* Henter feilkodeteksten */
  FIND FIRST FeilKode NO-LOCK WHERE
      FeilKode.FeilKode = BongLinje.FeilKode NO-ERROR.
  IF AVAILABLE FeilKode THEN
      BongLinje.FeilKodeTekst = FeilKode.Beskrivelse.
  ELSE DO:
      IF Bonglinje.Makulert = FALSE THEN
      ASSIGN  
          cError = cError + 
                   (IF cError = ""
                      THEN ""
                      ELSE CHR(10)) + 
                   "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent feilkode på transaksjonen " + string(BongLinje.FeilKode) + 
                   "."
          cFilError = cFilError + 
                   (IF cFilError = ""
                      THEN ""
                      ELSE "|") + 
                   " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent feilkode på transaksjonen " + string(BongLinje.FeilKode) + 
                   "." + CHR(1) + "2"
          BongLinje.FeilKodeTekst = "** Ukjent feilkode **".
  END.

  /* Henter tiltakskode */
  FIND FIRST KravKode NO-LOCK WHERE
      KravKode.KravKode = BongLinje.NotatKode NO-ERROR.
  IF AVAILABLE KravKode THEN
      BongLinje.NotatKodeTekst = KravKode.Beskrivelse.
  ELSE DO:
      IF Bonglinje.Makulert = FALSE THEN
      ASSIGN  
          cError = cError + 
                   (IF cError = ""
                      THEN ""
                      ELSE CHR(10)) + 
                   "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent tiltakskode på transaksjonen " + string(BongLinje.FeilKode) + 
                   "."
          cFilError = cFilError + 
                   (IF cFilError = ""
                      THEN ""
                      ELSE "|") + 
                   " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                   " Ukjent tiltakskode på transaksjonen " + string(BongLinje.FeilKode) + 
                   "." + CHR(1) + "2"
          BongLinje.NotatKodeTekst = "** Ukjent tiltakskode **"
          .
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
  DEF VAR piBongLinje AS INT NO-UNDO.
  DEF VAR plVeksel    AS DEC NO-UNDO.

  ASSIGN
      lVeksel       = 0
      lCashBack     = 0
      lTilgodeUt    = 0
      lAvrunding    = 0
      bBongSlettet  = FALSE
      iLagerType    = 0
      iMButikkNr    = 0
      bOverforing   = FALSE
      c2TTId        = ""
      cWebReservasjon = ''
      iRefNr        = 0
      cRefTekst     = ""
      lSubtotalRab  = 0
      bTrening      = FALSE
      bUtbetaling   = FALSE
      bInnbetaling  = FALSE
      bDropp        = FALSE
      bVarekjop     = FALSE
      bPakkeVare    = FALSE
      bUtbetType    = 0
      iSiste        = 0
      .

  /* Finner siste linje på bongen. */
  FIND LAST bBongLinje NO-LOCK WHERE
    bBongLinje.B_Id = BongHode.B_Id NO-ERROR.
  IF AVAILABLE bBongLinje THEN
  DO:
    iSiste = bBongLinje.LinjeNr.
    RELEASE bBonglinje.
  END.
  ELSE
    iSiste = 0.

  BONGLINJE:
  DO WHILE TRUE:
    FIND NEXT BongLinje EXCLUSIVE-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id NO-ERROR.
    IF NOT AVAILABLE BongLinje THEN
        LEAVE BONGLINJE.

    /* Bonglinjer som opprettes under konvertering skal ikke leses */
    IF BongLinje.LinjeNr > iSiste THEN
      LEAVE BONGLINJE.

    /* Bonglinjer som er makulert av kassen. */
    /* Varelinje for pakkevare - transkode 8195. Den makuleres også inntil videre. */
    IF (int(ENTRY(7,BongLinje.OriginalData,";")) >= 1000) THEN
    DO:
        /*
        DELETE BongLinje.
        NEXT BONGLINJE.
        */
        ASSIGN
            BongLinje.Makulert = TRUE
            .
    END.
    /* Varegruppesalg - skal ikke forekomme. */
    /* Opptrer kun i testsituasjoner.        */
    IF int(ENTRY(7,BongLinje.OriginalData,";")) = 5 THEN
    DO:
        DELETE BongLinje.
        NEXT BONGLINJE.
    END.
    /* Korreksjonspost finansposter - skal ikke forekomme. */
    /* For internt bruk i InfoPOS.                         */
    IF int(ENTRY(7,BongLinje.OriginalData,";")) = 83 THEN
    DO:
        DELETE BongLinje.
        NEXT BONGLINJE.
    END.

    /* Konverterer fra InfoPOS kode til SkoTex BackOffice kode. */
    ASSIGN
      cPOS        = STRING(int(ENTRY(7,BongLinje.OriginalData,";")),">999")
      cPOS        = IF LENGTH(cPos) > 3 
                    THEN SUBSTRING(cPos,2)
                    ELSE cPos
      piBongLinje = piBongLinje
      .
    /* Konverterer transaksjonskoden */
    KONVTRANSKODE:
    DO:
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
        /* Kontroll av transkode */
        IF BongLinje.Originaldata = ";;;;;;;;" THEN
            NEXT BONGLINJE.
        ELSE DO:
            {xkonvkvitteringpos.i}
        END.
    END. /* KONVTRANSKODE */

    /* Setter felles informasjon */
    ASSIGN
        /*---------------- BONGHODE -------------------*/
        BongHode.Tid         = (IF BongHode.Tid = 0
                                  THEN int(ENTRY(4,Bonglinje.Originaldata,";"))
                                  ELSE BongHode.Tid)
        BongHode.KassererNr  = (IF BongHode.KassererNr = 0
                                  THEN int(ENTRY(6,Bonglinje.Originaldata,";"))
                                  ELSE Bonghode.KassererNr)
        /*---------------- BONGLINJE ------------------*/
        BongLinje.TTId       = int(cTTId)
        BongLinje.TBId       = 1
        BongLinje.TransDato  = BongHode.Dato
        BongLinje.TransTid   = int(ENTRY(4,Bonglinje.Originaldata,";"))
        BongLinje.MButikkNr  = IF iMButikkNr <> 0 /* Mottagende butikk overføring */
                                 THEN iMButikkNr 
                                 ELSE 0
        .
     /* c2TTId settes for transtype 61 og representerer lagertransaksjon. */
     /* Alle 001 (Varesalgstransaksjoner) som kommer etter denne, er av   */
     /* den type som er satt i c2TTId.                                    */ 
     /* Det forekommer kun en type lagertransaksjoner på en bong.         */
     IF c2TTId <> "" AND BongLinje.TTId = 1 THEN DO:
         ASSIGN BongLinje.TTId = INT(c2TTId).
     END.

    /* Konverterer fra KassererId til KassererNr */
    FIND ButikkForsalj NO-LOCK WHERE
        ButikkForsalj.Butik      = BongHode.ButikkNr AND
        ButikkForsalj.KassererId = int(BongHode.KassererNr) NO-ERROR.
    IF AVAILABLE ButikkForsalj THEN
        BongHode.KassererNr = ButikkForsalj.ForsNr.

    /* Behandler bonglinjen. */
    RUN value("doTransType" + cPOS).

  END. /* BONGLINJE */

  /* Validerer kasserer */
  RUN ValiderKasserer.

  /* Treningsbong */
  IF bTrening THEN
      RUN doTranstypeTrening.
  /* Oppretter veksel */
  IF lVeksel <> 0 THEN
      RUN doTransTypeVeksel (lVeksel).
  /* Oppretter CashBack */
  IF lCashBack <> 0 THEN
      RUN doTransTypeCashBack (lCashBack).
  
  /* Oppretter tilgode ut */
  IF lTilgodeUt <> 0 THEN
      RUN doTransTypeTilgodeUt (lTilgodeUt).
  
  /* Oppretter avrunding. */
  IF lAvrunding <> 0 THEN
      RUN doTransTypeAvrunding (lAvrunding).
  /* Opprettet sybtotalpost. */
  IF lsubTotalRab <> 0 THEN
      RUN doTransTypeRabatt.
  /* Bong er slettet */
  IF bBongSlettet THEN
      RUN doTransTypeSlettet.
  /* Fikser pris på varekjøp, internt forbruk og brekkasje. */
  IF bVarekjop THEN
      RUN doTransTypeVarekjop.
  /* Setter alle flagg i bonghode. */
  RUN settbongflagg.p (BongHode.B_Id).
  /* Sumerer og kontrollerer saldo */
  RUN SjekkSaldo.p (BongHode.B_Id, INPUT-OUTPUT cFilError, INPUT-OUTPUT bLoggSaldoFeil).
  
  /* KortValidering.                                                                              */
  /* Kundekort er behandlet i transtype 62. Er det også medlemskort på bongen, sjekkes dette her. */
  IF BongHode.MedlemsKort <> "" THEN
      RUN SjekkMedlem.

  /* Nullstiller variabler */
  ASSIGN
      cGaveTilgId2 = "" 
      bDepositum   = FALSE
      bRetur       = FALSE
      bReklamasjon = FALSE
      bVareretur   = FALSE
      .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-OpprettKundeMedlem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE OpprettKundeMedlem Procedure 
PROCEDURE OpprettKundeMedlem :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:  Hvis det kommer en ukjent kunde hvor kunde og medlemskort er 
                                angitt, og medlemsnr er ulike kunde kortnr, blir dette feil.
                                Kunden får da et kundekortnr som består av siste del av 
                                medlemmets kortnr. Dette er en situasjon som ikke skal 
                                kunne forekomme. Da slike kombinasjoner må være opprettet
                                på bakrom. 
                                                                                                                                                                          
        ------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER dKundeNr AS DECIMAL NO-UNDO.
  DEFINE INPUT PARAMETER cKort    AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE bOk    AS LOG       NO-UNDO.
  DEFINE VARIABLE cMsgs  AS CHARACTER NO-UNDO.
  
  IF AVAILABLE Kunde THEN RELEASE Kunde.
  
  /* Sendes det inn et kortnr, skal det benyttes. Hvis ikke benyttes det som står i bonghode. */
  IF cKort = '' THEN
      cKort = BongHode.KundeKort.
  
  /* Er kortnummeret større enn 6 siffer og kundenummer ikke er angitt. Skal bare medlem opprettes. */
  IF LENGTH(cKort) > 6 THEN 
  BARE_MEDLEM:
  DO:
      IF dKundeNr > 0 AND NOT CAN-FIND(Kunde WHERE Kunde.KundeNr = dKundeNr)THEN 
      GENKUNDE:
      DO:
          CREATE Kunde.
          ASSIGN
              Kunde.Navn         = "Ukjent"
              Kunde.ButikkNr     = BongHode.ButikkNr
              Kunde.GruppeId     = 1
              Kunde.TypeId       = 1
              Kunde.MaksKredit   = 100
              Kunde.BetType      = 2 /* Kreditkunde */
              BongHode.KundeNavn = Kunde.Navn 
              .
      END. /* GENKUNDE */
      
      GENMEDLEM:
      DO:
          CREATE Medlem.
          ASSIGN
              Medlem.Fornavn   = "Ukjent" /*Kunde.Navn*/
              Medlem.ButikkNr  = BongHode.ButikkNr
              Medlem.KundeNr   = dKundeNr
              Medlem.MedGruppe = 1
              Medlem.MedType   = 1
              .
          ASSIGN
              BongHode.MedlemsNr  = Medlem.MedlemsNr
              BongHode.MedlemNavn = Medlem.Fornavn 
              .
          IF NOT CAN-FIND(FIRST MedlemsKort WHERE 
                          MedlemsKort.KortNr = cKort) THEN 
          DO:              
              CREATE Medlemskort.
              ASSIGN
                  Medlemskort.Medlemsnr       = Medlem.MedlemsNr
                  Medlemskort.KortNr          = cKort
                  Medlemskort.Innehaver       = Medlem.Fornavn
                  Medlemskort.AktivertDato    = TODAY
                  Medlemskort.UtgarDato       = TODAY + 999
                  Medlemskort.Kunderabattkort = TRUE
                  .
          END.    
      END. /* GENMEDLEM */
  END. /* BARE_MEDLEM */
  
  ELSE 
  OPPRETT_KUNDE_OG_MEDLEM: 
  DO:
    IF dKundeNr > 0 THEN
        FIND Kunde EXCLUSIVE-LOCK WHERE
             Kunde.KundeNr = dKundeNr NO-ERROR.
    IF AVAILABLE Kunde THEN
        FIND KundeGruppe OF Kunde NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Kunde THEN 
    DO:
      FIND FIRST KundeGruppe NO-LOCK WHERE
         KundeGruppe.GruppeId > 0 NO-ERROR.
      CREATE Kunde.
      ASSIGN
        Kunde.Navn            = "Automatisk opprettet " + STRING(TODAY)
        Kunde.BetType         = 1 /* Kontantkunde - skal stoppes i kassen. */
        Kunde.ButikkNr        = BongHode.ButikkNr /* Kobler kunden til butikken. */
        Kunde.SamleFaktura    = FALSE  
        Kunde.Fakturagebyr    = FALSE 
        Kunde.Purregebyr      = TRUE 
        Kunde.EksterntKundeNr = BongHode.KundeKort
        Kunde.GruppeId        = IF AVAILABLE KundeGruppe THEN KundeGruppe.GruppeId ELSE 1
        .
      /* Tar bort kundekort som opprettes i trigger da vi har fått inn kundekort fra kassen. */
      IF cKort <> '' THEN
        FOR EACH KundeKort OF Kunde:
          DELETE KundeKort.
        END.
    END.
    /* Oppdaterer bonginfo. */
    ASSIGN 
      BongHode.KundeNr      = Kunde.KundeNr
      BongHode.KortType     = 2 /* Kundekort. */
      .
      
    /* Bare medlem skal opprettes. */
    RUN genkundeMedlem.p (BongHode.ButikkNr,
                          (IF AVAILABLE KundeGruppe THEN KundeGruppe.GruppeId ELSE 1),
                          INPUT-OUTPUT Kunde.KundeNr,
                          OUTPUT BongHode.MedlemsNr,
                          OUTPUT bOk,
                          OUTPUT cMsgs).
    /* Kundekortene legges opp på samme kunde, men unike medlemskort */
    /* legges på separate medlemmer.                              */
    RUN genkundekort_og_medlem.p (BongHode.ButikkNr,
                                  Kunde.KundeNr,
                                  BongHode.MedlemsNr,
                                  cKort,
                                  cKort,
                                  999,
                                  OUTPUT bOk,
                                  OUTPUT cMsgs).
  END. /* OPPRETT_KUNDE_OG_MEDLEM */
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
      IF CAN-DO("03,10,12",STRING(BongLinje.TTId,"99")) THEN
          ASSIGN
          BongLinje.Antall = BongLinje.Antall * -1
          .
      /* Snur ALLTID fortegn på Vekseltransaksjonen */
      IF CAN-DO("70",STRING(BongLinje.TTId,"99")) THEN
          ASSIGN
          BongLinje.LinjeSum = BongLinje.LinjeSum * -1
          .
      /* Sumerer opp varelinjer for å finne beløp som skal dekkes */
      /* av de resterende betalingstransaksjonene.                */
      IF CAN-DO("01,03,10,12",STRING(BongLinje.TTId,"99")) THEN
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
      IF CAN-DO("50,52,54,56,63,65,66,70,71",STRING(BongLinje.TTId,"99")) THEN
          ASSIGN
          BongLinje.LinjeSum = BongLinje.LinjeSum * -1
          .
  END.
  ELSE
  FOR EACH BongLinje EXCLUSIVE-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id:
      IF CAN-DO("63",STRING(BongLinje.TTId,"99")) THEN
          ASSIGN
          BongLinje.LinjeSum = BongLinje.LinjeSum * -1
          .

  END.

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
/*
  ASSIGN
      pcTransKode = cKodeBetTrans + ",17"
      pbOk        = FALSE
      pcListe     = ""
      .
                                                                 
  BONG:
  FOR EACH BongLinje NO-LOCK WHERE
      BongLinje.B_Id = BongHode.B_Id:
    IF NOT CAN-DO(pcListe,SUBSTRING(BongLinje.OriginalData,21,2)) THEN
        pcListe = pcListe + 
                  (IF pcListe = ""
                     THEN ""
                     ELSE ",") + 
                  SUBSTRING(BongLinje.OriginalData,21,2).
                   
    IF CAN-DO(pcTransKode,SUBSTRING(BongLinje.OriginalData,21,2)) THEN
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
  IF pbOk = FALSE AND pcListe <> "16" AND BongHode.BongNr <> 0 THEN
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
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkMedlem) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkMedlem Procedure 
PROCEDURE SjekkMedlem :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

  /* Sjekker om medlemskortet finnes og om det er det samme medlemmet som er lagt opp på bongen fra før. */
  /* Medlemsnr. er blitt påført i transtype 62 hvis kunden ble opprettet.                                */
  FIND FIRST Medlemskort NO-LOCK WHERE
    MedlemsKort.KortNr = BongHode.MedlemsKort NO-ERROR.

  IF NOT AVAILABLE Medlemskort THEN 
  DO:    
    /* Er det lagt opp en kunde, skal det legges opp nytt medlem på kunden. */
    IF BongHode.Kundenr > 0 THEN
      RUN OpprettKundeMedlem(BongHode.KundeNr, BongHode.MedlemsKort).
    /* Finnes ikke en kunde, skal det genereres kunde og medlem. */
    ELSE
      RUN OpprettKundeMedlem(0, BongHode.MedlemsKort).
  END.  
  
  /* Nytt forsøk. */
  FIND FIRST Medlemskort NO-LOCK WHERE
    MedlemsKort.KortNr = BongHode.MedlemsKort NO-ERROR.

  IF AVAILABLE MedlemsKort THEN 
    DO:
      /* Finnes medlemmet og medlemsnr er forskjellig fra det som er lagt på ved behandling av transtype 62, skal */
      /* medlemsnummeret byttes ut. Er det ikke lagt inn medlemsnr, skal det påføres.                             */
      IF BongHode.MedlemsNr <> MedlemsKort.MedlemsNr THEN 
        DO:
          FIND Medlem OF MedlemsKort NO-LOCK NO-ERROR.
          IF AVAILABLE Medlem THEN 
          ASSIGN 
            BongHode.MedlemsNr  = Medlem.MedlemsNr
            BongHode.MedlemNavn = Medlem.ForNavn + Medlem.Etternavn
            .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SjekkSaldoxxx) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SjekkSaldoxxx Procedure 
PROCEDURE SjekkSaldoxxx :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*   DEF VAR plVareSum     AS DEC   NO-UNDO.                                                                                                       */
/*   DEF VAR plBetSum      AS DEC   NO-UNDO.                                                                                                       */
/*   DEF VAR plInnbetaling AS DEC   NO-UNDO.                                                                                                       */
/*                                                                                                                                                 */
/*   /* Sjekker at bongens sum er = 0. */                                                                                                          */
/*   ASSIGN plVareSum = 0                                                                                                                          */
/*          plBetSum  = 0.                                                                                                                         */
/*   /* NB: Overføringer kontrolleres ikke. */                                                                                                     */
/*   FOR EACH BongLinje NO-LOCK WHERE                                                                                                              */
/*       BongLinje.B_Id = BongHode.B_Id AND                                                                                                        */
/*       BongLinje.Makulert = FALSE:                                                                                                               */
/*                                                                                                                                                 */
/*     /* Sum varetranser */                                                                                                                       */
/*     /* TN 20/11-02 Deponering skal behandles som salg av vare. */                                                                               */
/*     /* Dropp skal summeres som vare */                                                                                                          */
/*     /* GAvekort ut skal behandles som vare i sumering */                                                                                        */
/*     IF CAN-DO("001,002,003,004,005,006,007,008,009,010,011,012,057,072,134",string(BongLinje.TTId,"999")) THEN                                  */
/*     DO:                                                                                                                                         */
/*         IF can-do("134",string(BongLinje.TTID,"999")) THEN /* Solgt GAVEKORT */                                                                 */
/*           ASSIGN                                                                                                                                */
/*           plVareSum = plVareSum + (BongLinje.LinjeSum - BongLinje.LinjeRab)                                                                     */
/*           .                                                                                                                                     */
/*         ELSE                                                                                                                                    */
/*         ASSIGN                                                                                                                                  */
/*           plVareSum = plVareSum +                                                                                                               */
/*                       ((BongLinje.LinjeSum -                                                                                                    */
/*                         BongLinje.LinjeRab -                                                                                                    */
/*                         BongLinje.SubTotalRab) * (IF BongLinje.Antall < 0                                                                       */
/*                                                                       THEN -1                                                                   */
/*                                                                       ELSE 1))                                                                  */
/*           .                                                                                                                                     */
/*     END.                                                                                                                                        */
/*                                                                                                                                                 */
/*     /* Betalingstransaksjoner.                                */                                                                                */
/*     /* Subtotalrabatt er trukket fra fra før og skal ikke tas */                                                                                */
/*     /* med her.                                               */                                                                                */
/*     /* Dropp kontrolleres ikke. Kontant er der = 0.           */                                                                                */
/*     ELSE IF CAN-DO("050,051,052,053,054,055,056,058,059,061,062,064,065,066,067,069,070,071,073,078,079,089",string(BongLinje.TTId,"999")) THEN */
/*     DO:                                                                                                                                         */
/*         /* Betaling av deponering, og veksel */                                                                                                 */
/*         /* Innbetaling på konto.            */                                                                                                  */
/*         IF CAN-DO("061,073,089",string(BongLinje.TTId,"999")) THEN                                                                              */
/*             ASSIGN                                                                                                                              */
/*             plBetSum      = plBetSum - BongLinje.LinjeSum                                                                                       */
/*             /* Spesiell håndtering av innbetalinger. */                                                                                         */
/*             plInnbetaling = plInnbetaling +                                                                                                     */
/*                             (IF CAN-DO("061",STRING(BongLinje.TTId,"999"))                                                                      */
/*                                THEN BongLinje.LinjeSum                                                                                          */
/*                                ELSE 0)                                                                                                          */
/*             .                                                                                                                                   */
/*         /* Betalingstranser. */                                                                                                                 */
/*         ELSE                                                                                                                                    */
/*             ASSIGN                                                                                                                              */
/*             plBetSum = plBetSum + BongLinje.LinjeSum                                                                                            */
/*             .                                                                                                                                   */
/*     END.                                                                                                                                        */
/*   END.                                                                                                                                          */
/*                                                                                                                                                 */
/*   /* Logger hvis bongsummen ikke er lik 0. */                                                                                                   */
/*   IF (plVareSum - plBetSum <> 0) AND                                                                                                            */
/*      bLoggSaldoFeil = TRUE THEN                                                                                                                 */
/*   DO:                                                                                                                                           */
/*       ASSIGN                                                                                                                                    */
/*       cFilError = cFilError +                                                                                                                   */
/*           (IF cFilError = ""                                                                                                                    */
/*              THEN ""                                                                                                                            */
/*              ELSE "|") +                                                                                                                        */
/*           " - Bongen's sum <> 0" +                                                                                                              */
/*           " Diff: " + STRING(plVareSum,"->>>,>>>,>>9.99") + " - " +                                                                             */
/*           STRING(plBetSum,"->>>,>>>,>>9.99") + " = " +                                                                                          */
/*           STRING(plVareSum - plBetSum,"->>>,>>>,>>9.99") + "." +                                                                                */
/*           " (But/Kas/Dato/BongNr: " +                                                                                                           */
/*           STRING(BongHode.ButikkNr) + "/" +                                                                                                     */
/*           STRING(BongHode.KasseNr) + "/" +                                                                                                      */
/*           STRING(BongHode.Dato) + "/" +                                                                                                         */
/*           STRING(BongHode.BongNr) +  ")." + CHR(1) + "3"                                                                                        */
/*       .                                                                                                                                         */
/* /* MESSAGE "Gurre slapp løs" SKIP         */                                                                                                    */
/* /*     cFilError SKIP                     */                                                                                                    */
/* /*                                        */                                                                                                    */
/* /*     plVareSum - plBetSum skip          */                                                                                                    */
/* /*      bLoggSaldoFeil SKIP               */                                                                                                    */
/* /*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */                                                                                                    */
/*   END.                                                                                                                                          */
/*                                                                                                                                                 */
/*   /* Oppdaterer bonghode med beløp. */                                                                                                          */
/*   ASSIGN BongHode.Belop = IF plInnbetaling <> 0                                                                                                 */
/*                             THEN plInnbetaling                                                                                                  */
/*                             ELSE plVareSum.                                                                                                     */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValiderArtikkel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderArtikkel Procedure 
PROCEDURE ValiderArtikkel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Konverterer PLU koder. */
  IF DEC(BongLinje.Strekkode) <= 99999 THEN
      BongLinje.Strekkode = LEFT-TRIM(BongLinje.Strekkode,"0").

  FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = BongLinje.Strekkode NO-ERROR.
  IF NOT AVAILABLE Strekkode THEN
      FIND Strekkode NO-LOCK WHERE
      Strekkode.Kode = LEFT-TRIM(BongLinje.Strekkode,"0") NO-ERROR.

  /* Blank strekkode eller ukjent strekkode */
  IF BongLinje.Strekkode = "" OR NOT AVAILABLE Strekkode THEN
  SJEKK-KODE:
  DO:
      IF Bonglinje.Makulert = FALSE THEN
      DO:
          /* poster ukjent salg på varegruppe */
          IF bPaVg = TRUE THEN
          DO:
              /* PLU artikler har normalt 4 siffer i artikkelnr. */
              FIND FIRST ArtBas NO-LOCK WHERE
                  ArtBas.ArtikkelNr <= 9999 AND 
                  ArtBas.Vg          = BongLinje.VareGr AND
                  ArtBas.Opris       = TRUE NO-ERROR.
              /* Det forekommer unntak */
              IF NOT AVAILABLE ArtBas THEN
                  FIND FIRST ArtBas NO-LOCK WHERE
                      ArtBas.Vg          = BongLinje.VareGr AND
                      ArtBas.Opris       = TRUE NO-ERROR.
              IF NOT AVAILABLE ArtBas THEN
                  RETURN.
              FIND FIRST STrekkode OF ArtBas NO-LOCK NO-ERROR.
              IF NOT AVAILABLE Strekkode THEN
                  RETURN.
              ASSIGN
              BongLinje.ArtikkelNr = ""
              BongLinje.LopeNr     = 0
              cError = cError + 
                       (IF cError = ""
                          THEN ""
                          ELSE CHR(10)) + 
                       "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                       " Ukjent strekkode --> Postert på PLU. " +
                       "."
              cFilError = cFilError + 
                       (IF cFilError = ""
                          THEN ""
                          ELSE "|") + 
                       " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                       " Ukjent strekkode --> Postert på PLU.. " +
                       "." + CHR(1) + "2"
              BongHode.Gradering = IF BongHode.Gradering < 3 THEN 3 ELSE BongHode.Gradering
              BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
              .
              LEAVE SJEKK-KODE.
          END.
          ELSE
              ASSIGN
              BongLinje.ArtikkelNr = ""
              BongLinje.LopeNr     = 0
              cError = cError + 
                       (IF cError = ""
                          THEN ""
                          ELSE CHR(10)) + 
                       "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                       " Ukjent strekkode/PLU nummer. " +
                       "."
              cFilError = cFilError + 
                       (IF cFilError = ""
                          THEN ""
                          ELSE "|") + 
                       " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                       " Ukjent strekkode/PLU nummer. " +
                       "." + CHR(1) + "2"
              BongHode.Gradering = IF BongHode.Gradering < 3 THEN 3 ELSE BongHode.Gradering
              BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
              .
          RETURN.
      END.
      ELSE /* Makulerte rader */
          RETURN.
  END.
  
  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = Strekkode.ArtikkelNr NO-ERROR.

  /* Kontrollerer varegruppen */
  IF AVAILABLE ArtBas THEN
  DO:
      IF BongLinje.VareGr <> ArtBas.Vg THEN
      DO:
          IF Bonglinje.Makulert = FALSE THEN
          ASSIGN  
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE CHR(10)) + 
                     "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Varegruppe er byttet på artikkel (ArtikkelNr: " + STRING(Strekkode.ArtikkelNr) + ") " +
                     " Fra VG: " + STRING(BongLinje.VareGr) + " til VG: " + STRING(ArtBas.Vg) +
                     "."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Varegruppe er byttet på artikkel (ArtikkelNr: " + STRING(Strekkode.ArtikkelNr) + ") " +
                     " Fra VG: " + STRING(BongLinje.VareGr) + " til VG: " + STRING(ArtBas.Vg) +
                     "." + CHR(1) + "2"
            BongHode.Gradering = IF BongHode.Gradering < 1 THEN 1 ELSE BongHode.Gradering
            BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
            .
      END.
  END.
  /* Ukjent artikkel */
  ELSE 
  UKJENT-ARTIKKEL:
  DO:
      IF Bonglinje.Makulert = FALSE THEN
      ASSIGN  
        cError = cError + 
                 (IF cError = ""
                    THEN ""
                    ELSE CHR(10)) + 
                 "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent artikkel på transaksjonen (ArtikkelNr: " + STRING(Strekkode.ArtikkelNr) + ")" + 
                 "."
        cFilError = cFilError + 
                 (IF cFilError = ""
                    THEN ""
                    ELSE "|") + 
                 " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent artikkel på transaksjonen (ArtikkelNr: " + STRING(Strekkode.ArtikkelNr) + ")" + 
                 "." + CHR(1) + "2"
        BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
        BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
        .
  END. /* UKJENT-ARTIKKEL */

  ASSIGN
      BongLinje.ArtikkelNr = (IF AVAILABLE ArtBas
                                THEN STRING(ArtBas.ArtikkelNr,">>>>>>>>>>>>9")
                                ELSE STRING(Strekkode.ArtikkelNr,">>>>>>>>>>>>9"))
      BongLinje.VareGr     = (IF AVAILABLE ArtBas
                               THEN ArtBas.Vg
                               ELSE BongLinje.VareGr)
      BongLinje.LopeNr     = (IF AVAILABLE ArtBas
                               THEN ArtBas.LopNr
                               ELSE ?)
      .

  /* Kontrollerer gyldig varegruppe */
  FIND VarGr NO-LOCK WHERE
      VarGr.Vg = BongLinje.VareGr NO-ERROR.

  IF AVAILABLE VarGr THEN
    ASSIGN 
      BongLinje.VareGruppeNavn = VarGr.VgBeskr
      /*BongLinje.MvaGr          = VarGr.MomsKod*/ 
      .
  ELSE DO:
      IF Bonglinje.Makulert = FALSE THEN
      ASSIGN  
        BongLinje.VareGruppeNavn = "** Ukjent varegruppe **"
        cError = cError + 
                 (IF cError = ""
                    THEN ""
                    ELSE CHR(10)) + 
                 "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent varegruppe på transaksjonen " + STRING(BongLinje.VareGr) + 
                 "."
        cFilError = cFilError + 
                 (IF cFilError = ""
                    THEN ""
                    ELSE "|") + 
                 " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                 " Ukjent varegruppe på transaksjonen " + STRING(BongLinje.VareGr) + 
                 "." + CHR(1) + "2"
        BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
        BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
        .
  END.

  IF AVAILABLE VarGr THEN
  DO:
      FIND HuvGr NO-LOCK WHERE
          HuvGr.Hg = VarGr.Hg NO-ERROR.
      IF NOT AVAILABLE HuvGr AND 
         Bonglinje.Makulert = FALSE THEN
          ASSIGN  
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE CHR(10)) + 
                     "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Ukjent hovedgruppe på den varegruppe som er på varelinjen: " + string(BongLinje.VareGr) + "/" + STRING(VarGr.Hg) +
                     "."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     "  Ukjent hovedgruppe på den varegruppe som er på varelinjen: : " + string(BongLinje.VareGr) + "/" + STRING(VarGr.Hg) +  
                     "." + CHR(1) + "2"
            BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
            BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
            .
      ELSE
          ASSIGN
          BongLinje.HovedGr            = VarGr.Hg
          BongLinje.HovedGrBeskrivelse = HuvGr.HgBeskr
          .
  END.

  /* Henter størrelsen hvis det er en størrelseskode <> 0 */
  IF Strekkode.StrKode <> 0 THEN
  DO:
      FIND StrKonv NO-LOCK WHERE
          StrKonv.StrKode = StrekKode.StrKode NO-ERROR.
      IF AVAILABLE StrKonv THEN
          BongLinje.Storrelse = StrKonv.Storl.
      ELSE DO:
          IF Bonglinje.Makulert = FALSE THEN
          ASSIGN  
            cError = cError + 
                     (IF cError = ""
                        THEN ""
                        ELSE CHR(10)) + 
                     "** BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
                     " Ukjent størrelse på bonglinjen: " + string(BongLinje.Storrelse) +
                     "."
            cFilError = cFilError + 
                     (IF cFilError = ""
                        THEN ""
                        ELSE "|") + 
                     " - BongNr: " + string(BongLinje.BongNr) + " LinjeNr: " + string(BongLinje.LinjeNr) + ": " +
              " Ukjent størrelse på bonglinjen: " + string(BongLinje.Storrelse) +
                     "." + CHR(1) + "2"
            BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
            BongHode.Gradering = IF BongLinje.Makulert THEN 0 ELSE BongHode.Gradering
            BongLinje.Storrelse = ""

            .
      END.
  END.
  /* Endrer til størrelse " 1". */
  ELSE DO:
      ASSIGN
          BongLinje.Storrelse = " 1"
          .
  END.

  /* Oppdaterer varekost. */
  IF AVAILABLE ArtBas THEN
  SETT-VVAREKOST:
  DO:
      IF AVAILABLE Lager THEN RELEASE Lager.

      /* Endrer prisen på varemottakstransaksjoner.                                  */
      /* Finnes ikke lagerpost, det er åpen pris eller varekost er <= 0, benyttes    */
      /* varekost fra kassen.                                                        */
      IF ArtBas.OPris = FALSE THEN 
      DO:
        IF ArtBas.Lager = TRUE THEN 
          FIND Lager NO-LOCK WHERE        
            Lager.ArtikkelNr = ArtBas.ArtikkelNr  AND
            Lager.Butik      = BongLinje.ButikkNr NO-ERROR.
      END. 

      /* TN 4/5-05 Ny håndtering av vvarekost. */
      /* Er varekost eller lagerantall 0 eller negativ, skal varekost fra kalkyle benyttes */
      /* Ikke lagerstyrte varer skal også ha varekosten satt fra kalkylen hvis denne er 0. */
      IF (AVAILABLE Lager AND (Lager.VVareKost <= 0 OR Lager.Lagant <= 0 OR Lager.VVareKost = ?)) OR
         (ArtBas.Lager = FALSE AND BongLinje.VVarekost = 0) THEN
      DO:
          /* Initierer vektet varekost. */
          FIND bButiker NO-LOCK WHERE
              bButiker.Butik = BongLinje.ButikkNr NO-ERROR.
          IF AVAILABLE bButiker THEN
              FIND ArtPris OF ArtBas NO-LOCK WHERE
                   ArtPris.ProfilNr = bButiker.ProfilNr NO-ERROR.

          IF NOT AVAILABLE bButiker OR NOT AVAILABLE ArtPris THEN
          DO:
              FIND bButiker WHERE bButiker.Butik = iCl NO-ERROR.
              IF AVAILABLE bButiker THEN
                  FIND ArtPris OF ArtBas NO-LOCK WHERE
                       ArtPris.ProfilNr = bButiker.ProfilNr NO-ERROR.
          END.
          IF AVAILABLE ArtPris THEN
              BongLinje.VVarekost = ArtPris.VareKost[IF ArtPris.Tilbud THEN 2 ELSE 1] * abs(BongLinje.Antall).
      END.
      /* Setter varekost på lagerstyrte varer. */
      ELSE IF AVAILABLE Lager AND ArtBas.Lager THEN
          BongLinje.VVarekost = Lager.VVareKost * abs(BongLinje.Antall).

      /* Det kan forekomme at BongLinje.Antall = 0. Da blir det ? i varekost. */
      IF BongLinje.VVareKost = ? THEN
          BongLinje.VVarekost = 0.

      /* Har artikkelen åpen pris, skal varekost settes fra kalkylen. */
      IF ArtBas.OPris AND (BongLinje.VVarekost = 0 OR BongLinje.VVarekost = ?) THEN
          BongLinje.VVarekost = 0.

      /* Sjekker om varekost er satt.                                       */
      /* Er det ikke satt noen varekost, merkes transaksjonen med feilkode. */
      IF BongLinje.VVarekost = 0 THEN /* or wBrutto% *** Skal også utføres for brutto% artikkler */
        DO:
          IF VALID-HANDLE(h_PrisKo) THEN
            /* NB: Varekost skal regnes av pris eksklusive rabatter       */
            /* Mva trekkes fra i rutinen som kalles. Fordi hvis det er    */
            /* gitt rabatt, er det feil mva. MvaKr må da beregnes pånytt. */
            RUN HentVareKost IN h_PrisKo (INPUT ArtBas.ArtikkelNr, 
                                          INPUT BongLinje.ButikkNr, 
                                          INPUT ((BongLinje.LinjeSum / ABSOLUTE(BongLinje.Antall)) - ((BongLinje.LinjeSum / ABSOLUTE(BongLinje.Antall)) - ((BongLinje.LinjeSum / ABSOLUTE(BongLinje.Antall)) / (1 + (BongLinje.Mva% / 100))))), 
                                          OUTPUT BongLinje.VVarekost).
            ASSIGN
                BongLinje.VVarekost = BongLinje.VVarekost * ABSOLUTE(BongLinje.Antall)
                BongLinje.VVarekost = IF BongLinje.VVareKost = ? 
                                        THEN 0
                                        ELSE BongLinje.VVarekost
                .
        END.
  END. /* SETT-VVAREKOST */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ValiderKasserer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValiderKasserer Procedure 
PROCEDURE ValiderKasserer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Gyldig kasserernr */
  IF NOT CAN-FIND(Forsalj WHERE
                  Forsalj.ForsNr = int(BongHode.KassererNr)) THEN
    ASSIGN  
      cError = cError + 
               (IF cError = ""
                  THEN ""
                  ELSE CHR(10)) + 
               "** BongNr: " + string(BongHode.BongNr) + ": " +
               " Ukjent kasserer på transaksjonen " + STRING(BongHode.KassererNr) + 
               "." 
      cFilError = cFilError + 
               (IF cFilError = ""
                  THEN ""
                  ELSE "|") + 
               " - BongNr: " + string(BongHode.BongNr) + ": " +
               " Ukjent kasserer på transaksjonen " + STRING(BongHode.KassererNr) + 
               "." + CHR(1) + "2"
      BongHode.KassererNavn = "*Ukjent*"
      BongHode.Gradering = IF BongHode.Gradering < 2 THEN 2 ELSE BongHode.Gradering
      .
  /* Setter kassererinfo. */
  ELSE DO:
      FIND Forsalj NO-LOCK WHERE
          Forsalj.ForsNr = int(BongHode.KassererNr).
      ASSIGN
          BongHode.KassererNavn  = Forsalj.FoNamn
          .
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-Mva2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION Mva2 Procedure 
FUNCTION Mva2 RETURNS DECIMAL
  ( INPUT plBelop AS DEC ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR dWork AS DEC NO-UNDO.

  /*
  ASSIGN
     dWork = ((plBelop) /* / iAntall */) -
            (((plBelop) /* / iAntall */) / (1 + (dMva% / 100))).
  if dWork = ? THEN dWork = 0.
  */
  RETURN dWork.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

