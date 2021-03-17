DEFINE VARIABLE iButikkNr      AS INTEGER    NO-UNDO.
DEFINE VARIABLE dDato          AS DATE       NO-UNDO.
DEFINE VARIABLE deProduktkod   AS DECIMAL    NO-UNDO.
DEFINE VARIABLE iBetalingstype AS INTEGER    NO-UNDO.
DEFINE VARIABLE iSubType       AS INTEGER    NO-UNDO.
DEFINE VARIABLE deVolym        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE deBelop        AS DECIMAL    NO-UNDO.
DEFINE VARIABLE deDatotid      AS DECIMAL    NO-UNDO.
DEFINE VARIABLE cDatoTid       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iKoeff         AS INTEGER    NO-UNDO.
DEFINE VARIABLE cBetTTId       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE lBetalkort     AS LOGICAL    NO-UNDO.
DEFINE TEMP-TABLE TT_prVsData NO-UNDO LIKE prVSData.
DEFINE BUFFER bBongHode FOR Bonghode.
DEFINE BUFFER bufprVSData FOR prVSData.
/* cDatoTid = STRING(YEAR(TODAY),"9999") +             */
/*            STRING(MONTH(TODAY),"99")  +             */
/*            STRING(DAY(TODAY),"99")    +             */
/*            REPLACE(STRING(TIME,"HH:MM:SS"),":",""). */
/* deDatoTid = DECI(cDatoTid).                         */
    DEFINE TEMP-TABLE tt_butdato NO-UNDO
        FIELD butikknr AS INTE
        FIELD dato AS DATE
        INDEX butdato IS PRIMARY UNIQUE butikknr dato.

ASSIGN cBetTTId = "50,51,52,53,54,55,56,57,58,59,60,61,62,64,65,66,69,70,71,73,79,89".

cDatoTid = STRING(YEAR(TODAY),"9999") + 
           STRING(MONTH(TODAY),"99")  +
           STRING(DAY(TODAY),"99")    +
           REPLACE(STRING(TIME,"HH:MM:SS"),":","").
deDatoTid = DECI(cDatoTid).

FOR EACH bonghode WHERE Bonghode.eksportertdato = ? NO-LOCK:
    EMPTY TEMP-TABLE TT_prVSData.

    ASSIGN iBetalingstype = 0
           iSubType       = ?
           lBetalkort     = FALSE.

    IF bonghode.makulert <> 2 THEN /* makulerad */ DO:
        FOR EACH Bonglinje WHERE bonglinje.b_id = bonghode.b_id NO-LOCK:
            IF Bonglinje.makulert = TRUE THEN
                NEXT.
          IF Bonglinje.Originaldata BEGINS "ItemId" AND CAN-DO("1,3,10",STRING(BongLinje.TTId)) AND BongLinje.Antall <> 0 THEN SALG: DO:

/*               IF NOT CAN-DO(cHgMapListe,STRING(Bonglinje.HuvGr)) THEN                             */
/*                   NEXT.  /* Vi skall bara hantera ett fåtal varor */                              */
/*                                                                                                   */
            ASSIGN iKoeff       =  IF BongLinje.Antall > 0 THEN 1 ELSE -1
/*                    dSalgssumTmp = BongLinje.LinjeSum - BongLinje.Linjerab - BongLinje.SubtotalRab */
/*                    dKostPris = dKostPris + iKoeff * BongLinje.VVarekost                           */
/*                    .                                                                              */
            deProduktKod = DECI(ENTRY(2,bonglinje.originaldata,"=")).
            FIND TT_prVSData WHERE TT_prVSData.ButikkNr      = Bonghode.butikknr AND
                                   TT_prVSData.Dato          = bonghode.dato     AND
                                   TT_prVSData.produktkod    = deProduktKod      AND
                                   TT_prVSData.Betalingstype = 0                 AND
                                   TT_prVSData.SubType       = ? NO-ERROR.
            IF NOT AVAIL TT_prVSData THEN DO:
                CREATE TT_prVSData.
                ASSIGN TT_prVSData.ButikkNr      = bonghode.butikknr
                       TT_prVSData.Dato          = bonghode.dato
                       TT_prVSData.produktkod    = deProduktKod
                       TT_prVSData.Betalingstype = 0
                       TT_prVSData.SubType       = ?.
            END.
            ASSIGN TT_prVSData.Volym         = TT_prVSData.Volym + bonglinje.antall
                   TT_prVSData.Belop         = TT_prVSData.Belop + (iKoeff * bonglinje.linjesum).

          END. /* SALG: */
          /* har vi träffat på ett betalkort kredit/bank så gäller det */
          IF lBetalkort = FALSE AND BongLinje.Makulert = FALSE THEN DO:
              IF CAN-DO(cBetTTId,STRING(bongLinje.TTId)) THEN DO:
                  /*  */
                  IF (bongLinje.TTId = 52 OR (bongLinje.TTId = 58 AND bonglinje.antall = 13)) /* AND bonglinje.forkonvertering = "JA" */ THEN
                      ASSIGN lBetalkort = TRUE
                             iBetalingstype = 2
                             iSubtype       = bonglinje.antall.
              END.
              IF iBetalingstype = 0 THEN DO:
                  CASE Bonglinje.TTId:
                      WHEN 50 OR WHEN 60 THEN /* Kontant or Valuta */
                          ASSIGN iBetalingsType = 1
                                 iSubType       = ?.
                      WHEN 70 THEN /* Växel */
                          ASSIGN iBetalingsType = 1
                                 iSubType       = ?.
                      WHEN 53 THEN /* Gavekort */
                          ASSIGN iBetalingsType = 3
                                 iSubType       = INT(BongLinje.Storrelse).
                      WHEN 54 THEN /* check -> drive off */
                          ASSIGN iBetalingsType = 4
                                 iSubType       = ?.
                      WHEN 65 THEN DO: /* Kredit */
                          IF TRIM(Bonglinje.storrelse) = "0610" OR TRIM(Bonglinje.storrelse) = "0612" THEN DO:
                              ASSIGN iBetalingsType = 6
                                     iSubType       = 1.
                          END.
                          ELSE DO:
                              ASSIGN iBetalingsType = 3
                                     iSubType       = 1.
                          END.
                      END.
                  END CASE.
              END.
          END.
        END. /* Bonglinjer slut */
    END.
    IF CAN-FIND(FIRST TT_prVSData) THEN DO:
        FOR EACH TT_prVSData:
            ASSIGN TT_prVSData.Betalingstype = iBetalingstype
                   TT_prVSData.SubType       = iSubType.

        END.
        FOR EACH TT_prVSData:
            FIND prVSData WHERE prVSData.ButikkNr      = TT_prVSData.ButikkNr      AND
                                prVSData.Dato          = TT_prVSData.Dato          AND
                                prVSData.produktkod    = TT_prVSData.produktkod    AND
                                prVSData.Betalingstype = TT_prVSData.Betalingstype AND
                                prVSData.SubType       = TT_prVSData.SubType       NO-ERROR.
/*             cDatoTid = STRING(YEAR(TODAY),"9999") +             */
/*                        STRING(MONTH(TODAY),"99")  +             */
/*                        STRING(DAY(TODAY),"99")    +             */
/*                        REPLACE(STRING(TIME,"HH:MM:SS"),":",""). */
/*             deDatoTid = DECI(cDatoTid).                         */
            IF NOT AVAIL prVsData THEN DO:
                CREATE prVSData.
                ASSIGN prVSData.ButikkNr      = TT_prVSData.ButikkNr     
                       prVSData.Dato          = TT_prVSData.Dato         
                       prVSData.produktkod    = TT_prVSData.produktkod   
                       prVSData.Betalingstype = TT_prVSData.Betalingstype
                       prVSData.SubType       = TT_prVSData.SUBTYPE
                       prVSData.datotid       = deDatoTid.
            END.
            ASSIGN prVSData.Volym         = prVSData.Volym + TT_prVSData.Volym
                   prVSData.Belop         = prVSData.Belop + TT_prVSData.Belop.
            IF prVSData.datotid <> deDatoTid THEN
                prVSData.datotid = deDatoTid.
            IF NOT CAN-FIND(tt_butdato WHERE tt_butdato.butikknr = TT_prVSData.ButikkNr AND tt_butdato.dato = TT_prVSData.dato) THEN DO:
                CREATE tt_butdato.
                ASSIGN tt_butdato.butikknr = TT_prVSData.ButikkNr
                       tt_butdato.dato     = TT_prVSData.dato NO-ERROR.
            END.
        END.
        FIND bBongHode WHERE ROWID(bBongHode) = ROWID(bonghode).
        ASSIGN bBongHode.eksportertdato = bBongHode.dato + 1.
    END.
    ELSE DO:
        FIND bBongHode WHERE ROWID(bBongHode) = ROWID(bonghode).
        ASSIGN bBongHode.eksportertdato = bBongHode.dato - 1.
    END.
END.
FOR EACH tt_butdato:
    FOR EACH prVSdata WHERE prVSData.ButikkNr      = tt_butdato.ButikkNr AND
                            prVSData.Dato          = tt_butdato.Dato NO-LOCK.
        IF prVSData.datotid = deDatoTid THEN
            NEXT.
        FIND bufprVSData WHERE ROWID(bufprVSData) = ROWID(prVSData) EXCLUSIVE NO-WAIT NO-ERROR.
        IF AVAIL bufprVSData THEN DO:
            bufprVSData.datotid = deDatoTid.
            RELEASE bufprVSData.
        END.
    END.
END.
QUIT.
/* cHgMapListe   = "70,71,72,73,74,78,79" */
/* cItemMapListe = "01,02,03,04,04,09,04" */

