      DEFINE TEMP-TABLE TT_T
          FIELD Strkode LIKE bonglinje.strekkode
          FIELD ArtNr LIKE bonglinje.artikkelnr
          FIELD Feil  AS CHAR
          FIELD Btekst AS CHAR
          INDEX strkode strkode.

      DEFINE VARIABLE iKasseloop  AS INTEGER    NO-UNDO.
      DEFINE VARIABLE FI-DatoLoop AS DATE       NO-UNDO.
      FOR EACH butiker NO-LOCK WHERE CAN-FIND(FIRST kasse WHERE kasse.butikknr = butiker.butik AND
                                                    kasse.modell = 20):
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
                        IF AVAIL StrekKode THEN DO:
                            FIND analyseartikkel WHERE AnalyseArtikkel.AnalyseId = 2 and
                                                       AnalyseArtikkel.ArtikkelNr = strekkode.artikkelnr NO-LOCK NO-ERROR.
                            IF NOT AVAIL analyseartikkel THEN DO:
                                FIND TT_T WHERE TT_T.strkode = bonglinje.strekkode NO-ERROR.
                                IF NOT AVAIL TT_T THEN DO:
                                    CREATE TT_T.
                                    ASSIGN TT_T.strkode = bonglinje.strekkode
                                           TT_T.artnr   = bonglinje.artikkelnr
                                           TT_T.Btekst  = bonglinje.bongtekst
                                           TT_T.feil = "IA".
                                    RELEASE TT_T.
                                END.
                                ELSE IF TT_T.artnr = "" THEN
                                    ASSIGN TT_T.artnr = bonglinje.artikkelnr.
                            END.
                        END.
                        ELSE DO:
                            FIND TT_T WHERE TT_T.strkode = bonglinje.strekkode NO-ERROR.
                            IF NOT AVAIL TT_T THEN DO:
                                CREATE TT_T.
                                ASSIGN TT_T.strkode = bonglinje.strekkode
                                       TT_T.artnr   = bonglinje.artikkelnr
                                       TT_T.Btekst  = bonglinje.bongtekst
                                       TT_T.feil = "IS".
                                RELEASE TT_T.
                            END.
                            ELSE IF TT_T.artnr = "" THEN
                                ASSIGN TT_T.artnr = bonglinje.artikkelnr.
                        END.
                    END.
                END.
            END. /* Datoloop */
        END. /* Kasse */
      END.
      OUTPUT TO "CLIPBOARD".
      FOR EACH TT_T.
          PUT UNFORMATTED TT_T.feil chr(9) TT_T.strkode CHR(9) artnr CHR(9) btekst  SKIP.
      END.
      OUTPUT CLOSE.
MESSAGE "klar"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
