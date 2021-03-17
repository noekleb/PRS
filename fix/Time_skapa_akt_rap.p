/* CONNECT -db infopos -H 10.125.250.27 -S 8010 -N tcp */
DEFINE VARIABLE dDato AS DATE    NO-UNDO.
DO dDato = DATE(2,1,2010) TO today /*DATE(2,28,2010)*/:
    FOR EACH infopos.timedag WHERE infopos.timedag.dato = dDato and
      infopos.timedag.butNr = 5 NO-LOCK:
        CREATE akt_rap.
        ASSIGN akt_rap.dato        = infopos.timedag.dato
               akt_rap.uke_dag     = WEEKDAY(akt_rap.dato)
/*                akt_rap.uke_nr      = */
               akt_rap.mnd         = MONTH(akt_rap.dato)
               akt_rap.butik       = infopos.timedag.butnr
               akt_rap.kasse       = 1
               akt_rap.tid         = infopos.timedag.tim * 10000
               akt_rap.tid_txt     = SUBSTR(STRING(akt_rap.tid,"999999"),1,4)
               akt_rap.oms_ant     = INT(infopos.timedag.vareant)
               akt_rap.oms_verd    = infopos.timedag.salgssum - infopos.timedag.mvakr
               akt_rap.ant_kunder  = INT(infopos.timedag.kundeant)
               akt_rap.svk         = infopos.timedag.kostpris
/*  ant_ret      = */
/*  verd_ret     = */
               akt_rap.ant_kvitto  = akt_rap.ant_kunder
               akt_rap.mva_kr      = infopos.timedag.mvakr.
    END.
END.



