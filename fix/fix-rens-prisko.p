DEF VAR wLoop AS INT NO-UNDO.

DEF STREAM ut.

CURRENT-WINDOW:WIDTH = 128.
    
OUTPUT STREAM ut TO VALUE("prisko.err") NO-ECHO APPEND.

PUT STREAM ut UNFORMATTED SKIP(2) 
    "** Tar bort priser uten artikkler - " TODAY STRING(TIME,"HH:MM:SS") SKIP.

/* Tar bort priser uten artikkler */
FOR EACH PrisKo EXCLUSIVE-LOCK:
    IF NOT CAN-FIND(ArtBas OF prisKo) THEN 
        do:
          EXPORT STREAM ut PrisKo.
          DELETE Prisko.
        END.
END.

PUT STREAM ut UNFORMATTED SKIP(2)
    "** Tar overflødige priskøposter - " TODAY STRING(TIME,"HH:MM:SS") SKIP.
FOR EACH ArtBas NO-LOCK:
    ASSIGN
        wLoop = 0.

    FOR EACH PrisKo OF ArtBas WHERE
        PrisKo.Tilbud = TRUE
        BREAK BY PrisKo.ArtikkelNr
              BY PrisKo.ProfilNr
              BY PrisKo.AktiveresDato
              BY PrisKo.AktiveresTid
              BY PrisKo.Tilbud:
       ASSIGN
           wLoop = wLoop + 1.

       PAUSE 0 BEFORE-HIDE.
       DISPLAY
         prisko.ArtikkelNr
           Prisko.ProfilNr
           PrisKo.AktiveresDato
           PrisKo.AktiveresTid
           PrisKo.Tilbud
           PrisKo.TYPE
      WITH WIDTH 126.
      IF LAST-OF(PrisKo.ArtikkelNr) AND wLoop > 1 THEN
      DO:
          DISPLAY "*". 
      END.

      /*
      IF wLoop > 1 THEN
      DO:
          EXPORT STREAM ut prisko.
          DELETE prisko.
      END.
      */
    END.

END.

OUTPUT STREAM ut CLOSE.
