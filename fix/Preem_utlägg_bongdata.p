DEFINE VARIABLE dFra  AS DATE       NO-UNDO.
DEFINE VARIABLE dTil  AS DATE       NO-UNDO.
DEFINE VARIABLE dDato AS DATE       NO-UNDO.
DEFINE VARIABLE cSubtypeName AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSubTypeNr   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKortNamn   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE IsUBTYPE AS INTEGER    NO-UNDO.
ASSIGN cSubtypeNr   = "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25"
      cSubtypename = "PREEM,PREEM VISA,SÅIFA,TEPAR,HY/TEX NO,HY/TEX DK,SAAB/OPEL,VOLVO,NESTE,DKV,OK,UNO-X,BANKKORT,AMEX,DINERS,FINAX,UTA,BONUSKORT,CAMPING,,,,,,".
dFra = DATE(10,1,2007).
dTil = DATE(10,31,2007).
FIND butiker WHERE butiker.butik = 31602 NO-LOCK. /* "ULLEVIMOTET" */
OUTPUT TO "c:\tmp\bongdata.txt".
PUT UNFORMATTED "Butik" ";"
                "Kassenr" ";"
                "Datum" ";"
                "Kl" ";"
                "Numtid" ";"
                "Kvittonr" ";"
                "Ttid" ";"
                "Transbeskr" ";"
                "Kort" ";"
                "Artikkelnr" ";"
                "Ean" ";"
                "Kvittotext" ";"
                "Hgr" ";"
                "Antal" ";"
                "Sum" ";"
                "Mva" ";"
                "Rab" SKIP.

DO dDato = dFra TO dTil:
    FOR EACH kasse WHERE kasse.butikknr = butiker.butik NO-LOCK:
        FOR EACH bonghode WHERE bonghode.butikknr = butiker.butik AND
                                        bonghode.gruppenr = 1 AND
                                        bonghode.kassenr  = kasse.kassenr AND
                                        bonghode.dato     = dDato NO-LOCK.
            FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id NO-LOCK.
                cKortNamn = "".
                FIND transtype OF bonglinje NO-LOCK NO-ERROR.
                IF bonglinje.ttid = 52 OR bonglinje.ttid = 58 THEN DO:
                    iSubtype = bonglinje.antall.
                    IF CAN-DO(cSubtypeNr,STRING(iSubType)) THEN
                        ASSIGN cKortnamn = ENTRY(iSubType,cSubTypename).
                END.
                PUT UNFORMATTED butiker.butik ";"
                                kasse.kassenr ";"
                                bonghode.dato ";"
                                STRING(bonghode.tid,"HH:MM:SS") ";"
                                bonghode.tid ";"
                                bonghode.bongnr ";"
                                bonglinje.ttid ";"
                                (IF AVAIL transtype THEN TransType.Beskrivelse ELSE "") ";"
                                cKortnamn ";"
                                TRIM(bonglinje.artikkelnr) ";"
                                TRIM(bonglinje.strekkode) ";"
                                REPLACE(Bonglinje.bongtekst,";"," ") ";"
                                (IF bonglinje.hovedgr > 0 THEN string(bonglinje.hovedgr) ELSE "") ";"
                                (IF bonglinje.ttid < 13 THEN STRING(bonglinje.antall) ELSE "") ";"
                                bonglinje.linjesum ";"
                                bonglinje.mvakr ";"
                                bonglinje.linjerab SKIP
                                 .
            END.
        END.
    END.
END.
MESSAGE "Klar"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

