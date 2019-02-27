/* Fakturer gavekort
   
   Created: 21.10.04 by Brynjar Hasle                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR bOk          AS LOG    NO-UNDO.
DEF VAR httTable     AS HANDLE NO-UNDO.
DEF VAR hGKTLbuff    AS HANDLE NO-UNDO.
DEF VAR plFaktura_Id AS DEC NO-UNDO.
DEF VAR piLinjeNr    AS INT NO-UNDO.
DEF VAR bGave        AS LOG NO-UNDO.
DEF VAR hBuffGK      AS HANDLE NO-UNDO.

bGave = ENTRY(2,icParam) = "gavekort".

IF bGave THEN
  hGKTLbuff = BUFFER Gavekort:HANDLE.
ELSE
  hGKTLbuff = BUFFER Tilgode:HANDLE.

/* If the input parameter is a list, create temp-table records first */
IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam) > 2 THEN DO:
  CREATE TEMP-TABLE httTable.
  CREATE BUFFER hBuffGK FOR TABLE "Gavekort".
  DO ix = 1 TO hBuffGK:NUM-FIELDS:
    httTable:ADD-LIKE-FIELD(hBuffGK:BUFFER-FIELD(ix):NAME,hBuffGK:BUFFER-FIELD(ix)).
  END.
  httTable:ADD-NEW-FIELD("RowIdent1","CHARACTER").
  httTable:TEMP-TABLE-PREPARE("ttGavekort").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  DO ix = 3 TO NUM-ENTRIES(icParam):
    IF bGave THEN DO:
      FIND Gavekort WHERE ROWID(Gavekort) = TO-ROWID(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAIL Gavekort THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER Gavekort:HANDLE).
        ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = STRING(ROWID(Gavekort)).
      END.
    END.
    ELSE DO:
      FIND Tilgode WHERE ROWID(Tilgode) = TO-ROWID(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAIL Tilgode THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER Tilgode:HANDLE).
        ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE = STRING(ROWID(Tilgode)).
      END.
    END.
  END.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " EXCLUSIVE-LOCK").
hQuery:QUERY-OPEN().

DO TRANSACTION ON ERROR UNDO, LEAVE:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    FIND Butiker
         WHERE Butiker.butik = int(ihBuffer:BUFFER-FIELD("ButNr"):BUFFER-VALUE) NO-LOCK NO-ERROR.
    IF NOT AVAIL Butiker THEN DO:
        IF NOT AVAIL butiker THEN DO:
            FIND ekstButiker
                 WHERE ekstButiker.butik = int(ihBuffer:BUFFER-FIELD("ButNr"):BUFFER-VALUE) NO-LOCK NO-ERROR.
        END.
        IF NOT AVAIL ekstButiker THEN DO:
            ocReturn = "Finner ikke butikk: " + string(ihBuffer:BUFFER-FIELD("ButNr"):BUFFER-VALUE).
            RETURN.
        END.
    END.
    FIND Kunde WHERE Kunde.KundeNr = IF AVAIL butiker THEN Butiker.KundeNr ELSE ekstButiker.KundeNr NO-LOCK NO-ERROR.
    IF NOT AVAIL Kunde THEN DO:
      ocReturn = "Finner ikke kunde for butikk butikk. Kundenr i butikkregister: " + STRING(butiker.kundenr).
      RETURN.
    END.

    /* Oppretter faktura, setter kundenr m.m. og returnerer faktura_id. */
    RUN getfaktura_id.p (Kunde.KundeNr,INT(ENTRY(1,icParam)),1,YES,TODAY,OUTPUT plFaktura_Id).
    IF RETURN-VALUE <> "AVBRYT" AND CAN-FIND(FakturaHode WHERE FakturaHode.Faktura_Id = plFaktura_Id) THEN
    FAKTURAINFO:
    DO:
      FIND FakturaHode NO-LOCK WHERE
          FakturaHode.Faktura_Id = plFaktura_Id.
      /* Initierer faktura med kundeinfo m.m. */
      IF FakturaHode.Navn = "" AND FakturaHode.Adresse1 = "" THEN DO:
        RUN update_fakturahode.p (plfaktura_Id,"INIT","",1).
        RUN update_fakturahode.p (plfaktura_Id,"Butikksalg,TotalRabatt%,Leveringsdato,LevFNr,Leveringsdato,Utsendelsesdato",
                                  "yes" + chr(1) + 
                                   STRING(Kunde.TotalRabatt%) + CHR(1) + 
                                   STRING(TODAY) + CHR(1) + 
                                   "1" + CHR(1) + 
                                   STRING(TODAY) + CHR(1) + 
                                   STRING(TODAY),1) .
        FIND CURRENT FakturaHode NO-LOCK.
      END.
    END. /* FAKTURAINFO */
    ELSE DO:
      ocReturn = "Kunne ikke opprette faktura".
      UNDO, LEAVE.
    END.

    /* Setter linjenr */
    piLinjeNr = 0.
    FOR EACH FakturaLinje NO-LOCK OF FakturaHode
        BY FakturaLinje.Faktura_Id
        BY FakturaLinje.FakturaLinjeNr:
        piLinjeNr = FakturaLinje.FakturaLinjeNr.
    END.
    piLinjeNr = piLinjeNr + 1.

    bOk = hGKTLbuff:FIND-BY-ROWID(TO-ROWID(ihBuffer:BUFFER-FIELD("Rowident1"):BUFFER-VALUE),EXCLUSIVE-LOCK) NO-ERROR.
    IF bOK THEN DO:

      FIND FIRST BongLinje WHERE 
          BongLinje.b_id = DEC(hGKTLbuff:BUFFER-FIELD("BruktB_id"):BUFFER-VALUE) NO-LOCK NO-ERROR.
      CREATE FakturaLinje.
      ASSIGN
          FakturaLinje.Faktura_Id     = FakturaHode.Faktura_Id
          FakturaLinje.FakturaLinjeNr = piLinjeNr
          piLinjeNr                   = piLinjeNr + 1
          FakturaLinje.Opphav         = 1 /* Artikkel */
          FakturaLinje.Leveringsdato  = ihBuffer:BUFFER-FIELD("Dato"):BUFFER-VALUE
          /*FakturaLinje.TTId           = IF bGave THEN 53 ELSE 66*/
          /*FakturaLinje.TBId           = 1*/
          FakturaLinje.Notat          = IF AVAIL BongLinje THEN 
                                        "Bong: "  +
                                        "But: "   + STRING(BongLinje.ButikkNr) + "/" +
                                        " Kas: "  + STRING(BongLinje.KasseNr) + "/" +
                                        " Dato: " + STRING(BongLinje.TransDato) + "/" +
                                        " Nr: "   + STRING(BongLinje.BongNr)
                                        ELSE ""
          FakturaLinje.EkstRefId      = IF AVAIL BongLinje THEN STRING(BongLinje.RefNr) ELSE ""
          FakturaLinje.EkstRefId      = IF TRIM(FakturaLinje.EkstRefId) = "0" THEN "" ELSE FakturaLinje.EkstRefId
          FakturaLinje.EkstRefId      = IF NOT bGave THEN hGKTLbuff:BUFFER-FIELD("identnr"):BUFFER-VALUE ELSE FakturaLinje.EkstRefId
          FakturaLinje.EkstRefTekst   = IF AVAIL BongLinje THEN STRING(BongLinje.RefTekst) ELSE ""

          FakturaLinje.Antall        = 1
          FakturaLinje.VareNr        = ""
          FakturaLinje.Varetekst     = (IF bGave THEN "Gavekort" ELSE "Tilgode") /* + " " + hGKTLbuff:BUFFER-FIELD("identnr"):BUFFER-VALUE) */
          FakturaLinje.LinjeRabattKr = 0
          FakturaLinje.LinjeRab%     = 0
          FakturaLinje.LinjeRab%     = 0
          FakturaLinje.TotRab%       = 0
          FakturaLinje.NettoPris     = DEC(hGKTLbuff:BUFFER-FIELD("Belop"):BUFFER-VALUE)

          FakturaLinje.MvaKr         = 0
          FakturaLinje.NettoLinjeSum = DEC(hGKTLbuff:BUFFER-FIELD("Belop"):BUFFER-VALUE)
          FakturaLinje.Linjesum      = DEC(hGKTLbuff:BUFFER-FIELD("Belop"):BUFFER-VALUE)

          FakturaLinje.Mva%          = 0
          FakturaLinje.MomsKod       = 0
          FakturaLinje.Leveringsdato = DATE(hGKTLbuff:BUFFER-FIELD("BruktDato"):BUFFER-VALUE)
          FakturaLinje.Storl         = ""
          FakturaLinje.DbKr          = 0
          FakturaLinje.Db%           = 0
          FakturaLinje.Db%           = 0
          hGKTLbuff:BUFFER-FIELD("Fakturert"):BUFFER-VALUE     = TRUE
          hGKTLbuff:BUFFER-FIELD("FakturertDato"):BUFFER-VALUE = TODAY
          hGKTLbuff:BUFFER-FIELD("Faktura_id"):BUFFER-VALUE    = plfaktura_Id
          NO-ERROR.
    END.
    hQuery:GET-NEXT().
  END.
  RUN update_fakturahode.p (plfaktura_Id,"KalkulerTotaler","",5).
  RUN beregn_kunde_saldo.p ("idlist|" + STRING(FakturaHode.KundeNr),
                          ?,
                          "",
                          OUTPUT ocReturn,
                          OUTPUT obOk).

END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.


