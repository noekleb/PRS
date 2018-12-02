CURRENT-WINDOW:WIDTH = 150.

DEF BUFFER bSysPara FOR SysPara.


EVIGHET:
REPEAT WHILE TRUE:
    ETILOOP:
    FOR EACH bSysPara WHERE 
        bSysPara.SysHId = 210 and
        bSysPara.SysGr  = 100 AND
        bSysPara.ParaNr >= 100:

        FIND TelleHode NO-LOCK WHERE
            TelleHode.TelleNr = int(entry(1,bSysPara.Parameter1,CHR(1))) NO-ERROR.
        IF NOT AVAILABLE Tellehode THEN
            NEXT ETILOOP.
        FIND Butiker NO-LOCK WHERE
            Butiker.Butik = int(TelleHode.ButikkListe) NO-ERROR.
        IF NOT AVAILABLE Butiker THEN
            NEXT ETILOOP.

        PAUSE 0 BEFORE-HIDE.
        DISPLAY
            bsysPara.Beskrivelse
            entry(1,bSysPara.Parameter1,CHR(1))
            string(Butiker.BELayout)
            string(Butiker.BEPrinter)
            string(Butiker.BETerminalklient)
            WITH FRAME b WIDTH 150 DOWN.
        DOWN 1 WITH FRAME b.

        RUN batchEtikettTelling.p (int(entry(1,bSysPara.Parameter1,CHR(1))),
                                   string(Butiker.BELayout),
                                   string(Butiker.BEPrinter),
                                   string(Butiker.BETerminalklient),
                                   "TELLING").
        /* Lagt inn pause fordi skriver ikke rekker å bli klar med forrige utskrift. */
        /* Forårsaker at sesjonen går ned uten feilmelding.                          */
        PAUSE 20 NO-MESSAGE.


        IF AVAILABLE bSysPara then
            DELETE bSysPara.


    END. /* ETILOOP */
    PAUSE 10 NO-MESSAGE.
END. /* EVIGHET */

                
