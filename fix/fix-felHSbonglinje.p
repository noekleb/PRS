      DEFINE VARIABLE iKasseloop  AS INTEGER    NO-UNDO.
      DEFINE VARIABLE FI-DatoLoop AS DATE       NO-UNDO.
      DEFINE VARIABLE cNystrekkode AS CHARACTER  NO-UNDO.
      DEFINE BUFFER Bbonglinje FOR bonglinje.
      FOR EACH butiker NO-LOCK WHERE CAN-FIND(FIRST kasse WHERE kasse.butikknr = butiker.butik AND
                                                    kasse.modell = 20):  /* 30 = megadisk 20 = hugin */
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
                        IF AVAIL StrekKode AND AND bonglinje.strekkode = "73102113" THEN DO:
                                FIND Bbonglinje WHERE ROWID(Bbonglinje) = ROWID(bonglinje).
                                FIND StrekKode WHERE StrekKode.kode = "1025" NO-LOCK.
                                ASSIGN BbongLinje.Levnavn = "HS " + Bbonglinje.strekkode
                                       Bbonglinje.Strekkode = "1025"
                                       Bbonglinje.Artikkelnr = STRING(Strekkode.Artikkelnr) NO-ERROR.
                                RELEASE Bbonglinje.
                        END.
                        ELSE IF NOT AVAIL StrekKode THEN DO:
                             ASSIGN cNystrekkode = "".
                             CASE bonglinje.StrekKode:
                                 WHEN "393528101704" OR WHEN "5393528101704" OR WHEN "7253528101704" OR
                                 WHEN "7393500101704" OR WHEN "7393528101704" OR WHEN "1260" OR WHEN "3131" THEN
                                     ASSIGN cNystrekkode = "1020".
                                 WHEN "7393528101700" OR WHEN "7393528101711" OR WHEN "1261" OR WHEN "3232" THEN
                                     ASSIGN cNystrekkode = "1021".
                                 WHEN "7163528210055" THEN
                                     ASSIGN cNystrekkode = "1022".
                                 WHEN "7393528210062" THEN
                                     ASSIGN cNystrekkode = "1023".
                                 WHEN "7393528101728" THEN
                                     ASSIGN cNystrekkode = "1025".
                                 WHEN "7393528101735" THEN
                                     ASSIGN cNystrekkode = "1026".
                                 WHEN "7393528101742" OR WHEN "7393528101759" THEN
                                     ASSIGN cNystrekkode = "1027".
                                 WHEN "7393528210123" OR WHEN "1518" THEN
                                     ASSIGN cNystrekkode = "1031".
                                 WHEN "7393528210130" THEN
                                     ASSIGN cNystrekkode = "1032".
                             END CASE.
                             IF cNystrekkode <> "" THEN DO:
                                 FIND Bbonglinje WHERE ROWID(Bbonglinje) = ROWID(bonglinje).
                                 FIND StrekKode WHERE StrekKode.kode = cNystrekkode NO-LOCK.
                                 ASSIGN BbongLinje.Levnavn = "HS " + Bbonglinje.strekkode
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

    
    


