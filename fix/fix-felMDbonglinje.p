      DEFINE VARIABLE iKasseloop  AS INTEGER    NO-UNDO.
      DEFINE VARIABLE FI-DatoLoop AS DATE       NO-UNDO.
      DEFINE VARIABLE cNystrekkode AS CHARACTER  NO-UNDO.
      DEFINE BUFFER Bbonglinje FOR bonglinje.
      FOR EACH butiker NO-LOCK WHERE CAN-FIND(FIRST kasse WHERE kasse.butikknr = butiker.butik AND
                                                    kasse.modell = 30):  /* 30 = megadisk 20 = hugin */
        DO iKasseLoop = 1 TO 12: /* Vi tror inte någon butik har mer än 5 kassor */
/*         FOR EACH Kasse WHERE Kasse.Butik = TT_ValgteButiker.Butik NO-LOCK. */
            DO FI-DatoLoop = DATE(01,01,2004) TO DATE(04,30,2004):
                FOR EACH BongHode WHERE BongHode.ButikkNr = butiker.butik AND
                                        BongHode.GruppeNr = 1                      AND
                                        BongHode.KasseNr  = iKasseLoop             AND
                                        BongHode.Dato     = FI-DatoLoop USE-INDEX Bong NO-LOCK:
                    FOR EACH bonglinje WHERE bonglinje.b_id      = bonghode.b_id AND
                                             bonglinje.TTId      < 50 AND
                                             bonglinje.StrekKode <> "" AND
                                             bonglinje.antall    <> 0 and
                                             TRIM(bonglinje.BongTekst) BEGINS "SL " NO-LOCK.
                        FIND StrekKode WHERE StrekKode.kode = bonglinje.StrekKode NO-LOCK NO-ERROR.
                        IF NOT AVAIL StrekKode THEN DO:
                             ASSIGN cNystrekkode = "".
                             CASE bonglinje.StrekKode:
                                 WHEN "3131" OR WHEN "1260" THEN
                                     ASSIGN cNystrekkode = "1020".
                                 WHEN "3232" OR WHEN "1261" THEN
                                     ASSIGN cNystrekkode = "1021".
                                 WHEN "15019" THEN
                                     ASSIGN cNystrekkode = "1057".
                                 WHEN "15020" THEN
                                     ASSIGN cNystrekkode = "1058".
                                 WHEN "1518" THEN
                                     ASSIGN cNystrekkode = "1031".
                                 WHEN "1503" OR WHEN "1512" THEN
                                     ASSIGN cNystrekkode = "1047".
                                 WHEN "1517" THEN
                                     ASSIGN cNystrekkode = "1050".
                                 WHEN "2727" THEN
                                     ASSIGN cNystrekkode = "1025".
                                 WHEN "288" THEN
                                     ASSIGN cNystrekkode = "1026".
                                 WHEN "2929" THEN
                                     ASSIGN cNystrekkode = "1027".
                                 WHEN "3030" THEN
                                     ASSIGN cNystrekkode = "1028".
                             END CASE.
                             IF cNystrekkode <> "" THEN DO:
                                 FIND Bbonglinje WHERE ROWID(Bbonglinje) = ROWID(bonglinje).
                                 FIND StrekKode WHERE StrekKode.kode = cNystrekkode NO-LOCK.
                                 ASSIGN BbongLinje.Levnavn = "XX " + Bbonglinje.strekkode
                                        Bbonglinje.Strekkode = cNyStrekkode
                                        Bbonglinje.Artikkelnr = STRING(Strekkode.Artikkelnr) NO-ERROR.
                                 RELEASE Bbonglinje.
                             END.
                        END.
                    END.
                END.
            END. /* Datoloop */
        END. /* Kasse */
      END.
MESSAGE "klar"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
