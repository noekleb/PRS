/*
  fix-endrekassedefinisjoner.p 
  Oppdaterer kassadefinisjonene for MegaDisk og HuginSweda kassene,
  slik at 
*/

CURRENT-WINDOW:WIDTH = 200.

DEF VAR cModell AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR piLoop  AS INT                 NO-UNDO.

FOR EACH Kasse WHERE
    Kasse.ModellNr >= 20 AND
    Kasse.ModellNr <= 40: 

    {syspara.i 1 10 Kasse.ModellNr cModell}

    IF Kasse.ModellNr >= 20 AND
       Kasse.ModellNR <= 30 THEN
        assign
            Kasse.KvitteringId  = REPLACE(Kasse.KvitteringId,'"',';')
            Kasse.Kvittering[2] = REPLACE(Kasse.Kvittering[2],'txt','r1')
            Kasse.KasseNr       = IF Kasse.KasseNr < 10 
                                    THEN Kasse.KasseNr + 10
                                    ELSE Kasse.KasseNr                               
            .
    ELSE IF Kasse.ModellNr = 40 THEN
        ASSIGN
        Kasse.Navn = "Kasse " + STRING(Kasse.KasseNr)
        .

    PAUSE 0.
    DISPLAY 
        ButikkNr
        KasseNr
        ModellNr
        cModell
        ElJournalId
        ElJournal[1]
        ElJournal[2]
        KvitteringId
        Kvittering[1]
        Kvittering[2]
        WITH WIDTH 198
        .
END.

/* Opprettelse av StorePoint kassadefinisjoner */
FOR EACH Butiker NO-LOCK:
    DO piLoop = 1 TO 5:
        IF NOT CAN-FIND(FIRST Kasse WHERE
                        Kasse.ButikkNr = Butiker.Butik AND
                        Kasse.Gruppe   = 1 AND
                        Kasse.KasseNr  = piLoop) THEN
        DO:
            CREATE Kasse.
            ASSIGN
                Kasse.ButikkNr = Butiker.Butik 
                Kasse.Gruppe   = 1 
                Kasse.KasseNr  = piLoop 
                .
        END.
        ELSE FIND Kasse WHERE
                  Kasse.ButikkNr = Butiker.Butik AND
                  Kasse.Gruppe   = 1 AND
                  Kasse.KasseNr  = piLoop.
        /* Oppdaterer kassadefinisjonen */
        ASSIGN
            Kasse.Modell            = 40 /* StorePoint */
            Kasse.Navn              = "Kasse " + STRING(Kasse.KasseNr)
            Kasse.Aktiv             = TRUE
            Kasse.ElJournal[1]      = "DTBR"
            Kasse.ElJournal[2]      = "*"
            Kasse.ElJournalOperand  = 2
            Kasse.ElJournalId       = "+0000" + string(Butiker.Butik,"99999") + ",+"
            Kasse.ElJournalKatalog  = "c:\home\pressbyran\ankommet\" + string(Butiker.Butik,"99999")
            Kasse.ElJournalKonv     = TRUE
            Kasse.ElJournalAktiv    = TRUE
            Kasse.ElJournalInnles   = "xinndteljournalpos"
            Kasse.ElJournalBehandle = "xbehdteljournalpo"
            .
    END.
END.
