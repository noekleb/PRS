DEFINE INPUT PARAMETER lMedlemsNr AS DECIMAL NO-UNDO.

DEF VAR piLoop AS INT NO-UNDO.

DEFINE VARIABLE iSisteNr    AS INT NO-UNDO.
DEFINE VARIABLE iOldSisteNr AS INTEGER NO-UNDO.
DEFINE VARIABLE wRecid      AS RECID NO-UNDO.

DEFINE BUFFER bSysPara FOR SysPara.
DEFINE BUFFER bMedlemsKort FOR MedlemsKort.

/* Må være et gyldig medlem. */
FIND Medlem NO-LOCK WHERE
  Medlem.MedlemsNr = lMedlemsNr NO-ERROR.
IF NOT AVAILABLE Medlem
  THEN RETURN '* Ukjent medlemsnr ' + STRING(lMedlemsNr) + '.'.

/* Medlemmet må være koblet til en kunde og kunden må eksistere. */
IF Medlem.KundeNr = 0 THEN 
  RETURN '* Ukjent kundenr på medlem ' + STRING(lMedlemsNr) + ' Kundenr: ' + STRING(Medlem.KundeNr) + '.'.
IF NOT CAN-FIND(Kunde WHERE
  Kunde.KundeNr = Medlem.MedlemsNr) THEN 
    RETURN '* Ukjent kunde på medlem ' + STRING(lMedlemsNr) + ' Kundenr: ' + STRING(Medlem.KundeNr) + '.'.

{syspara.i 14 2 5 iSisteNr int}.
iOldSisteNr = iSisteNr.

MEDLEMS_KORT:
FOR EACH MedlemsKort OF Medlem NO-LOCK:

  /* Skal ikke gjøres hvis det er gjort før :) */
  IF MedlemsKort.InterntKKortId <> 0 THEN 
    NEXT MEDLEMS_KORT.
    

  LOOPEN:
  DO piLoop = iSisteNr + 1 TO 999999:
    IF NOT CAN-FIND(KundeKort WHERE KundeKort.KortNr = string(piLoop)) THEN
    DO TRANSACTION:
      ASSIGN iSisteNr = piLoop.

      /* Opprett kundekort. */
      RUN createKundekort.p (INPUT Medlem.KundeNr, string(iSisteNr), INPUT 720, OUTPUT wRecid).
      FIND KundeKort NO-LOCK WHERE
        RECID(KundeKort) = wRecid NO-ERROR.
      
      /* Koble kundekort til medlemskort. */
      IF AVAILABLE KundeKort THEN 
      DO:
        FIND bMedlemsKort WHERE
          RECID(bMedlemsKort) = RECID(MedlemsKort) EXCLUSIVE-LOCK.
        ASSIGN
          bMedlemskort.Kunderabattkort = TRUE
          bMedlemskort.KundeKortNr     = STRING(iSisteNr)
          bMedlemsKort.InterntKKortId  = IF AVAILABLE KundeKort
                                            THEN KundeKort.InterntKKortId
                                            ELSE MedlemsKort.InterntKKortId
          . 
        LEAVE LOOPEN.      
      END.
    END. /* TRANSACTION */
  END. /* LOOPEN */
END. /* MEDLEMS_KORT */


/* Lagrer siste brukte nr. */
IF iOldSisteNr <> iSisteNr THEN 
DO FOR bSysPara TRANSACTION:
    FIND bSysPara EXCLUSIVE-LOCK WHERE
        bSysPara.SysHId =  14 AND
        bSysPara.SysGr  =  2 AND
        bSysPara.ParaNr =  5 NO-ERROR.
    IF NOT AVAILABLE bSysPara THEN
        DO:
          CREATE bSysPara.
          ASSIGN
              bSysPara.SysHId =  14 
              bSysPara.SysGr  =  2 
              bSysPara.ParaNr =  5 
              bSyspara.Beskrivelse = "Siste brukte kundekortnr"
              bSysPara.Parameter1  = "0"
              .
    END.
    ASSIGN  
          bSysPara.Parameter1  = STRING(iSisteNr)
          .
    RELEASE bSysPara.
END.      
