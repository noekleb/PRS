TRIGGER PROCEDURE FOR WRITE OF Medlem OLD BUFFER oldMedlem.

DEF VAR trgiAnt AS INT NO-UNDO.
DEFINE VARIABLE trgLoggMedlem AS LOG NO-UNDO.

/* Teller opp kundekort. */
FOR EACH MedlemsKort OF Medlem EXCLUSIVE-LOCK:
    ASSIGN trgIAnt = trgIAnt + 1.
END.
/* Oppdaterer kort med blank innehaver. */
IF trgiAnt = 1 THEN
    FOR EACH MedlemsKort OF medlem EXCLUSIVE-LOCK:
        ASSIGN Medlemskort.Innehaver = Medlem.Fornavn + " " + Medlem.EtterNavn NO-ERROR.
    END.
ELSE
    /* Oppdaterer kort med blank innehaver. */
    FOR EACH MedlemsKort OF Medlem EXCLUSIVE-LOCK WHERE (MedlemsKort.Innehaver = "" 
                                                         OR MedlemsKort.Innehaver MATCHES "*Ukjent*"
                                                         OR Medlemskort.Innehaver = oldMedlem.Fornavn + " " + oldMedlem.EtterNavn):
        ASSIGN MedlemsKort.Innehaver = Medlem.Fornavn + " " + Medlem.EtterNavn NO-ERROR.
    END.

/* Kobler medlemskort mot kundekort */
IF Medlem.KundeNr > 0 THEN 
  RUN gen_kundekort_for_gamle_medlemskort.p (Medlem.MedlemsNr).

ASSIGN
  Medlem.EDato = TODAY
  Medlem.ETid  = TIME
  Medlem.BrukerId = USERID("skotex")
  .

IF Medlem.Kundenr <> 0 THEN 
DO:
    KASSE:
    DO:
        FIND ELogg WHERE 
             ELogg.TabellNavn     = "Kunde" AND
             ELogg.EksterntSystem = "POS"    AND
             ELogg.Verdier        = STRING(Medlem.KundeNr) NO-ERROR.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "Kunde"
                   ELogg.EksterntSystem = "POS"   
                   ELogg.Verdier        = STRING(Medlem.KundeNr).
        END.
        ASSIGN ELogg.EndringsType = 1
               ELogg.Behandlet    = FALSE.
    END. /* KASSE */

    /* Logger for sending av fil til Webside for initiering */
    MEDLEM_TIL_WEB:
    DO:
        FIND ELogg WHERE 
             ELogg.TabellNavn     = "Medlem" AND
             ELogg.EksterntSystem = "WEBINIT"    AND
             ELogg.Verdier        = STRING(Medlem.MedlemsNr) NO-ERROR.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "Medlem"
                   ELogg.EksterntSystem = "WEBINIT"   
                   ELogg.Verdier        = STRING(Medlem.MedlemsNr).
        END.
        ASSIGN ELogg.EndringsType = 1
               ELogg.Behandlet    = FALSE.
    END. /* MEDLEM_TIL_WEB */
END.
ELSE DO:
  /* Logger også medlemsendringer hvis de ikke er koblet til kunde. */
  LESSYSPARA:
  FOR EACH SysPara NO-LOCK WHERE 
    SysPara.SysHId = 14 AND 
    SysPara.SysGr  = 200:
    IF SysPara.Parameter1 = '1' THEN 
      DO:
        trgLoggMedlem = TRUE.
        LEAVE LESSYSPARA.
      END.   
  END. /* LESSYSPARA */  
  IF trgLoggMedlem THEN 
  DO:
    /* Logger for sending av fil til Webside for initiering */
    MEDLEM_TIL_WEB:
    DO:
        FIND ELogg WHERE 
             ELogg.TabellNavn     = "Medlem" AND
             ELogg.EksterntSystem = "WEBINIT"    AND
             ELogg.Verdier        = STRING(Medlem.MedlemsNr) NO-ERROR.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "Medlem"
                   ELogg.EksterntSystem = "WEBINIT"   
                   ELogg.Verdier        = STRING(Medlem.MedlemsNr).
        END.
        ASSIGN ELogg.EndringsType = 1
               ELogg.Behandlet    = FALSE.
    END. /* MEDLEM_TIL_WEB */
  END. 
END.

RELEASE ELogg.


