
/* Provisjons rapport data grunnlag - */ 


DEFINE TEMP-TABLE tt_prov NO-UNDO
      FIELD butik  AS INTE
      FIELD datum  AS DATE
      FIELD fsg    AS DECI
      FIELD reklam AS DECI
      FIELD ret_fr_andra AS DECI
      FIELD ret_i_andra  AS DECI
      INDEX dabu IS PRIMARY datum butik.

DEFINE INPUT PARAMETER  dFraDag AS DATE NO-UNDO.
DEFINE INPUT PARAMETER  dTilDag AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR tt_Prov . 


DEFINE VARIABLE dDatumLoop AS DATE NO-UNDO.
  
    FOR EACH butiker NO-LOCK WHERE 
        CAN-FIND(FIRST kasse WHERE kasse.butikknr = butiker.butik AND 
                                   kasse.aktiv = TRUE):
    
        DO dDatumLoop = dFraDag TO dTilDag:
            CREATE tt_prov.
            ASSIGN tt_prov.butik = butiker.butik
                   tt_prov.datum = dDatumLoop.
            
            FIND Dags_Rap WHERE dags_rap.butikk = butiker.butik AND 
                                dags_rap.dato = dDatumLoop NO-LOCK NO-ERROR.
    
            IF AVAIL dags_rap THEN 
                ASSIGN tt_prov.fsg   = hg1_oms + hg2_oms + hg3_oms + hg4_oms + hg5_oms + hg6_oms + hg7_oms + hg8_oms + hg9_oms.
        END.
    END.
    
    
    FOR EACH butiker NO-LOCK:
        FOR EACH kasse WHERE kasse.butikknr = butiker.butik NO-LOCK:
    
            DO dDatumLoop = dFraDag TO dTilDag:
    
                FOR EACH bonghode WHERE bonghode.butikknr = butiker.butik  AND
                                        bonghode.gruppenr = kasse.gruppenr AND
                                        bonghode.kassenr  = kasse.kassenr  AND
                                        bonghode.dato     = dDatumLoop     AND
                                        bonghode.makulert <> 2             NO-LOCK:
                    FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id NO-LOCK.
                        IF bonglinje.makulert THEN
                            NEXT.
    
                        /* kundreklamation */
                        IF bonglinje.ttid = 3 THEN 
                        DO:
                            FIND tt_prov WHERE tt_prov.butik = butiker.butik AND
                                               tt_prov.datum = dDatumLoop.
    
                            ASSIGN tt_prov.reklam = tt_prov.reklam + BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab.
                                   tt_prov.fsg    = tt_prov.fsg - (BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab).
                        END.
                        /* returer */
                        ELSE IF bonglinje.ttid = 10 THEN 
                        DO:
                            IF bonglinje.ReturButikk > 0 AND bonglinje.ReturButikk <> bonglinje.butikknr THEN 
                            DO:
                                FIND tt_prov WHERE tt_prov.butik = butiker.butik AND
                                                   tt_prov.datum = dDatumLoop.
                                ASSIGN tt_prov.ret_fr_andra = tt_prov.ret_fr_andra + BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab.
                                FIND tt_prov WHERE tt_prov.butik = bonglinje.ReturButikk AND
                                                   tt_prov.datum = dDatumLoop NO-ERROR.
                                IF NOT AVAIL tt_prov THEN DO:
                                    CREATE tt_prov.
                                    ASSIGN tt_prov.butik = bonglinje.ReturButikk
                                           tt_prov.datum = dDatumLoop.
                                END.
                                ASSIGN tt_prov.ret_i_andra = tt_prov.ret_i_andra + BongLinje.LinjeSum - BongLinje.LinjeRab - BongLinje.SubtotalRab.
                            END.
                        END.
                    END.
                END.
            END.
        END.
    END.
