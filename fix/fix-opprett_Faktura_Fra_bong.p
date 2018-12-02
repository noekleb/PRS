DEF VAR bBestNr AS LOG NO-UNDO.
DEF VAR cTekst AS CHAR NO-UNDO.
DEF VAR bInnkjopspris AS LOG NO-UNDO.
DEF VAR iCL AS INT NO-UNDO.
DEF VAR bVarespes AS LOG NO-UNDO.

DEF BUFFER clButiker FOR Butiker.

{syspara.i 19 100 1 cTekst}
IF CAN-DO("Ja,yes,true,1",cTekst) THEN
    bBestNr = TRUE. 
ELSE
    bBestNr = FALSE. 

{syspara.i 19 100 3 cTekst}
IF CAN-DO("Ja,yes,true,1",cTekst) THEN
    bInnkjopspris = TRUE. 
ELSE
    bInnkjopspris = FALSE. 

{syspara.i 5 1 1 iCl INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCl NO-ERROR.

/* Utvidet varespes på fakturalinje. */
{syspara.i 19 100 4 cTekst}
IF CAN-DO('1,Ja,J,Yes,Y,True',cTekst) 
  THEN bVarespes = TRUE.
  ELSE bVarespes = FALSE.


FIND BongHode NO-LOCK WHERE
    BongHode.ButikkNr = 192  AND
    BongHode.GruppeNr = 1 AND
    BongHode.KasseNr  = 1  AND
    BongHode.Dato     = 02/15/2011 AND
    BongHode.BongNr   = 112853 NO-ERROR.

/* Poster kreditsalg som faktura. */
IF (BongHode.KundeNr <> 0 OR BongHode.KundeKort <> "") THEN
  do:
    find Kunde no-lock where
      Kunde.KundeNr = BongHode.KundeNr.
    RUN posterFaktura.
  MESSAGE 'Postert bong:' BongHode.BongNr
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
  end.
  
else message 'Fant ikke bong' view-as alert-box.
  
  
PROCEDURE posterFaktura :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEF VAR plFaktura_Id  AS DEC  FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEF VAR piLinjeNr     AS INT  NO-UNDO.
DEF VAR pcVaretran    AS CHAR NO-UNDO.
DEF VAR pcBetaling    AS CHAR NO-UNDO.
DEF VAR pcRefNr       AS CHAR NO-UNDO.
DEF VAR pcRefTekst    AS CHAR NO-UNDO.
DEF VAR pcTekst       AS CHAR NO-UNDO.
DEF VAR ocReturn      AS CHAR NO-UNDO.
DEF VAR obOk          AS LOG  NO-UNDO.
DEF VAR pbOverforing  AS LOG NO-UNDO.
DEF VAR pcVareNr      AS CHAR NO-UNDO.

ASSIGN
    pcBetaling   = "50,51,52,53,54,55,56,57,58,61,66,69,70,71,72,73,78,79"
    pcVaretran   = "1,3,4,6,10,134"
    plFaktura_Id = 0
    pbOverforing = FALSE
    .

message BongHode.KOrdre_Id 'TEST-0' view-as alert-box.

/* Bonger som allerede er fakturert fra kundeordre, skal ikke faktureres her. */
IF BongHode.KOrdre_Id > 0 THEN
    RETURN.

/* Leser og posterer betalingstransaksjoner. */
IF AVAILABLE Kunde THEN
POSTER-FAKTURA:
DO:
    /* ekstern referanse */
    FIND FIRST BongLinje NO-LOCK WHERE
        BongLinje.B_Id = Bonghode.B_Id AND
        BongLinje.TTId = 88 NO-ERROR.
    IF AVAILABLE BongLinje THEN
        ASSIGN
        pcRefNr    = STRING(BongLinje.RefNr)
        pcRefTekst = BongLinje.RefTekst
        .
    FIND Butiker NO-LOCK WHERE
        Butiker.Butik = BongHode.Butik NO-ERROR.

    /* Er det kredittsalg eller overføringer på bongen. */
    IF NOT CAN-FIND(FIRST BongLinje WHERE
                    BongLinje.B_Id = BongHode.B_id AND
                    CAN-DO("065,087",STRING(BongLinje.TTId,"999"))) THEN
        LEAVE POSTER-FAKTURA.
    /* Flagger at det er en overføring */
    IF CAN-FIND(FIRST BongLinje WHERE
                      BongLinje.B_Id = BongHode.B_id AND
                    CAN-DO("087",STRING(BongLinje.TTId,"999"))) THEN
        pbOverforing = TRUE.
        
    /* ---------- Gammelt interface ------------------------
    /* Oppretter faktura, setter kundenr m.m. og returnerer faktura_id. */
    IF BongHode.Belop >= 0 THEN
        RUN getfaktura_id.p (Kunde.KundeNr,BongHode.ButikkNr,1,YES,OUTPUT plFaktura_Id).
    ELSE
    /* For negative bonger skal det oprpettes separat faktura - Kreditnota. */
        RUN getfaktura_id.p (Kunde.KundeNr,BongHode.ButikkNr,999,YES,OUTPUT plFaktura_Id).
    -------------- Gammel slutt ---------------------------- */

    /* Oppretter faktura, setter kundenr m.m. og returnerer faktura_id. */
    IF BongHode.Belop >= 0 THEN
        RUN getfaktura_id.p (Kunde.KundeNr,BongHode.ButikkNr,1,YES,BongHode.Dato,OUTPUT plFaktura_Id).
    ELSE
    /* For negative bonger skal det opprpettes separat faktura - Kreditnota. */
        RUN getfaktura_id.p (Kunde.KundeNr,BongHode.ButikkNr,999,YES,BongHode.Dato,OUTPUT plFaktura_Id).

    IF NOT CAN-FIND(FakturaHode WHERE FakturaHode.Faktura_Id = plFaktura_Id) THEN DO:
        MESSAGE 
        PROGRAM-NAME(1)  SKIP
        "FEIL - Hentet Id på ukjent faktura: " RETURN-VALUE SKIP plFaktura_Id
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
        LEAVE POSTER-FAKTURA.
    END.

    IF CAN-FIND(FakturaHode WHERE FakturaHode.Faktura_Id = plFaktura_Id) THEN
    FAKTURAINFO:
    DO:
        FIND FakturaHode NO-LOCK WHERE
            FakturaHode.Faktura_Id = plFaktura_Id.
        /* Initierer faktura med kundeinfo m.m. */
        IF FakturaHode.Navn = "" AND FakturaHode.Adresse1 = "" THEN
        DO TRANSACTION:
            RUN update_fakturahode.p (plfaktura_Id,"INIT","",1).
            /* Vi må her overstyre bilagstype, som er satt tilbake til 1 i overstående INIT */
            RUN update_fakturahode.p (plfaktura_Id,"Butikksalg,TotalRabatt%,Leveringsdato,LevFNr,Leveringsdato,Utsendelsesdato,BilagsType",
                                      "Yes" + chr(1) + 
                                       STRING(Kunde.TotalRabatt%) + CHR(1) + 
                                       STRING(BongHode.Dato) + CHR(1) + 
                                       "1" + CHR(1) + 
                                       STRING(BongHode.Dato) + CHR(1) + 
                                       STRING(BongHode.Dato) + CHR(1) +
                                       (IF BongHode.Belop >= 0 THEN '1' ELSE '2'),1) .
            FIND CURRENT FakturaHode NO-LOCK.
        END.
    END. /* FAKTURAINFO */

    /* Setter linjenr */
    piLinjeNr = 0.
    FOR EACH FakturaLinje NO-LOCK OF FakturaHode
        BY FakturaLinje.Faktura_Id
        BY FakturaLinje.FakturaLinjeNr:
        piLinjeNr = FakturaLinje.FakturaLinjeNr.
    END.
    piLinjeNr = piLinjeNr + 1.

    /* Posterer bonglinjene på faktura. */
    KREDITSALG:
    FOR EACH BongLinje NO-LOCK WHERE
        BongLinje.B_Id = BongHode.B_Id AND
        BongLinje.Makulert = FALSE:

        /* Er det overføring, skal ikke betalignstransaksjonen med */
        IF CAN-DO(pcVaretran + "," + (IF pbOverforing = FALSE
                                        THEN pcBetaling
                                        ELSE ""),STRING(BongLinje.TTId)) THEN
        OPPRETT-FAKTURA:
        DO:
            CREATE FakturaLinje.
            ASSIGN
                FakturaLinje.Faktura_Id     = FakturaHode.Faktura_Id
                FakturaLinje.FakturaLinjeNr = piLinjeNr
                piLinjeNr                   = piLinjeNr + 1
                FakturaLinje.Opphav         = 1 /* Artikkel */
                FakturaLinje.Leveringsdato  = BongLinje.TransDato
                FakturaLinje.TTId           = BongLinje.TTId
                FakturaLinje.TBId           = BongLinje.TBId
                FakturaLinje.B_Id           = BongLinje.B_Id
                FakturaLinje.BongLinjeNr    = BongLinje.LinjeNr
                FakturaLinje.EkstRefId      = pcRefNr
                FakturaLinje.EkstRefTekst   = pcRefTekst
                .
     
            ASSIGN
                FakturaLinje.Notat          = "Kvittering: "  +
                                              "Butikk: "   + STRING(BongLinje.ButikkNr) + "/" +
                                              " Kasse: "  + STRING(BongLinje.KasseNr) + "/" +
                                              " Dato: " + STRING(BongLinje.TransDato) + "/" +
                                              " Nr: "   + STRING(BongLinje.BongNr) + "/" + 
                                              " Rad: "   + STRING(BongLinje.LinjeNr)
                   .
            /* Varelinjer */
            IF CAN-DO(pcVaretran,STRING(BongLinje.TTId)) THEN
            VARELINJE:
            DO:
                /* Henter artikkelen */
                IF AVAILABLE ArtBas THEN
                    RELEASE ArtBas.
                IF bBestNr THEN
                    FIND ArtBas WHERE
                    ArtBas.ArtikkelNr = dec(BongLinje.ArtikkelNr) NO-ERROR.
                ASSIGN
                    pcVareNr = IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE STRING(BongLinje.ArtikkelNr)
                    pcVareNr = IF pcVareNr = "" THEN STRING(BongLinje.ArtikkelNr) ELSE pcVareNr
                    .

                FIND FIRST Moms NO-LOCK WHERE
                    Moms.MomsProc = BongLinje.Mva% NO-ERROR.
                IF bInnkjopsPris THEN DO:
                    FIND ArtPris NO-LOCK WHERE
                        ArtPris.ArtikkelNr = dec(BongLinje.ArtikkelNr) AND
                        ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
                    IF NOT AVAILABLE ArtPris THEN
                        FIND ArtPris NO-LOCK WHERE
                            ArtPris.ArtikkelNr = dec(BongLinje.ArtikkelNr) AND
                            ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
                END.                

                ASSIGN
                    FakturaLinje.Antall        = BongLinje.Antall
                    FakturaLinje.VareNr        = pcVareNr
                    FakturaLinje.ArtikkelNr    = IF AVAILABLE ArtBas
                                                   THEN ArtBas.ArtikkelNr
                                                   ELSE dec(BongLinje.ArtikkelNr)
                    Fakturalinje.LevFargKod    = IF AVAILABLE ArtBas
                                                   THEN ArtBas.LevFargKod
                                                   ELSE ""
                    FakturaLinje.Varetekst     = IF FakturaLinje.TTId = 134
                                                   THEN "Gavekort pålydende kr: " + STRING(BongLinje.LinjeSum / BongLinje.Antall) 
                                                   ELSE BongLinje.BongTekst
                    FakturaLinje.LinjeRabattKr = (BongLinje.LinjeRab + BongLinje.SubtotalRab) / (BongLinje.Antall)
                    FakturaLinje.TotalrabattKr = (BongLinje.LinjeRab + BongLinje.SubtotalRab)
                    FakturaLinje.LinjeRab%     = ABS((FakturaLinje.LinjeRabattKr * BongLinje.Antall) / BongLinje.LinjeSum) * 100
                    FakturaLinje.LinjeRab%     = IF FakturaLinje.LinjeRab% = ? THEN 0 ELSE FakturaLinje.LinjeRab%
                    FakturaLinje.TotRab%       = FakturaLinje.LinjeRab%

                    FakturaLinje.NettoPris     = IF (pbOverforing AND bInnkjopspris = TRUE AND AVAILABLE ArtPris)
                                                   THEN ArtPris.Innkjopspris[1]
                                                 ELSE IF (pbOverforing AND bInnkjopspris = false) 
                                                     THEN BongLinje.VVarekost / BongLinje.Antall
                                                 ELSE (BongLinje.LinjeSum - (BongLinje.LinjeRab + BongLinje.SubtotalRab) - BongLinje.MvaKr) / BongLinje.Antall

                    FakturaLinje.Pris          = IF (pbOverforing AND bInnkjopsPris = TRUE AND AVAILABLE ArtPris)
                                                   THEN ArtPris.InnkjopsPris[1]
                                                 ELSE IF (pbOverforing AND bInnkjopsPris = FALSE) 
                                                     THEN BongLinje.VVarekost / BongLinje.Antall
                                                 ELSE (BongLinje.LinjeSum / (1 + (BongLinje.Mva% / 100))) / BongLinje.Antall

                    FakturaLinje.MvaKr         = IF (pbOverforing AND bInnkjopsPris = TRUE AND AVAILABLE ArtPris)
                                                   THEN (ArtPris.MvaKr[1] * FakturaLinje.Antall) /* TN 5/12-08 Multipliser med antall*/
                                                 ELSE IF (pbOverforing AND bInnkjopsPris = FALSE)
                                                   THEN (((FakturaLinje.NettoPris * BongLinje.Mva%) / 100) * FakturaLinje.Antall) /* TN 5/12-08 Multipliser med antall*/
                                                 ELSE BongLinje.MvaKr * (IF FakturaLinje.Antall < 0 THEN -1 ELSE 1)
                    FakturaLinje.NettoLinjeSum = FakturaLinje.NettoPris * abs(FakturaLinje.Antall)
                    FakturaLinje.Linjesum      = FakturaLinje.NettoPris * abs(FakturaLinje.Antall) + FakturaLinje.MvaKr

                    FakturaLinje.Mva%          = BongLinje.Mva%
                    FakturaLinje.MomsKod       = IF AVAILABLE Moms
                                                   THEN Moms.MomsKod
                                                   ELSE 0
                    FakturaLinje.Leveringsdato = BongLinje.TransDato
                    FakturaLinje.Storl         = BongLinje.Storrelse
                    FakturaLinje.DbKr          = (FakturaLinje.NettoPris - (IF BongLinje.Antall < 0 THEN BongLinje.VVArekost * -1 ELSE BongLinje.VVarekost))
                    FakturaLinje.Db%           = ABS(FakturaLinje.DbKr / FakturaLinje.NettoLinjeSum) * 100
                    FakturaLinje.Db%           = IF FakturaLinje.Db% = ? THEN 0 ELSE FakturaLinje.Db%
                    FakturaLinje.TTId          = BongLinje.TTId
                    FakturaLinje.TBId          = BongLinje.TBId
                    .
                    IF bVarespes THEN 
                    DO:
                      IF AVAILABLE ArtBas THEN 
                        FIND Farg OF ArtBas NO-LOCK NO-ERROR.
                      IF AVAILABLE Farg THEN 
                        Fakturalinje.Varespesifikasjon = STRING(Farg.Farg) + ' ' + Farg.FarBeskr + 
                                                         (IF Fakturalinje.Varespesifikasjon <> ''
                                                           THEN CHR(10) + Fakturalinje.Varespesifikasjon
                                                           ELSE '').
                    END.
            END. /* VARELINJE */
            /* Betalingstransaksjoner */
            IF CAN-DO(pcBetaling,STRING(BongLinje.TTId)) THEN
            BETALINGSLINJE:
            DO:
                FIND TransType NO-LOCK WHERE
                    TransType.TTId = BongLinje.TTId NO-ERROR.
                ASSIGN
                    FakturaLinje.Antall = 1
                    FakturaLinje.Varetekst     = (IF AVAILABLE TransType
                                                    THEN TransType.Beskrivelse
                                                    ELSE BongLinje.BongTekst)
                    FakturaLinje.NettoPris     = BongLinje.LinjeSum * -1
                    FakturaLinje.NettoLinjeSum = BongLinje.LinjeSum * -1
                    FakturaLinje.Linjesum      = BongLinje.LinjeSum * -1
                    .
            END. /* BETALINGSLINJE */

            FIND CURRENT FakturaLinje NO-LOCK.
        END. /* OPPRETT-FAKTURA */
    END. /* KREDITSALG */

    /* Summerer opp fakturahode. */
    IF plFaktura_Id <> 0 THEN
    FAKTURASUM:
    DO:
        /* Legger på fakturagebyr på vanlige faktura. Ikke på kreditnota. */
        IF BongHode.Belop >= 0 AND Kunde.Fakturagebyr THEN
            RUN update_fakturahode.p (plfaktura_Id,"Fakturagebyr","",1).
        ELSE 
            RUN update_fakturahode.p (plfaktura_Id,"Dato",STRING(BongHode.Dato),1).
        /* Påfører og beregner rabatt. Rabatt pr. linje tildeles automatisk når totalrabatt <> 0 blir satt. */
        RUN update_fakturahode.p (plfaktura_Id,"KalkulerTotaler","",1).
    END. /* FAKTURASUM */

    /* direkte utskrift av faktura. */
    IF Kunde.Samlefaktura = FALSE AND
        Butiker.dirFakturaUtskrift = TRUE THEN
    DO:
        RUN faktura_produksjon ("idlist|" + STRING(plfaktura_Id),
                                ?,
                                "",
                                OUTPUT ocReturn,
                                OUTPUT obOk).
        IF obOk THEN
        DO:
            RUN faktura_fakturaskriver.p (STRING(BongHode.ButikkNr) + "|1|" + STRING(BongHode.KasseNr),
                                    ?,
                                    "",
                                    OUTPUT ocReturn,
                                    OUTPUT obOk).
            IF obOk THEN DO:
                pcTekst = ocReturn.
                IF pcTekst <> "" THEN DO:
                    RUN skrivfaktura.p (STRING(plfaktura_Id) + "|",ENTRY(1,pcTekst,"|"),ENTRY(2,pcTekst,"|"),ENTRY(3,pcTekst,"|"),ENTRY(4,pcTekst,"|"),ENTRY(5,pcTekst,"|")). 
                    /* Ekstra kopi til butikk? */
                    IF Butiker.FaktKopiRappskriver AND Butiker.RapPrinter <> "" THEN
                        RUN skrivfaktura.p (STRING(plfaktura_Id) + "|",ENTRY(1,pcTekst,"|"),Butiker.RapPrinter,"1",ENTRY(4,pcTekst,"|"),ENTRY(5,pcTekst,"|")). 
                END.
            END.
        END.
    END.

END. /* POSTER-FAKTURA */
END PROCEDURE.



