/* Kalkulerte felter for størrelsesbrowse i bestillingsforslag
   Opprettet: 20.08.09 av BHa
-------------------------------------------------------------------*/   
DEF VAR iProfNr AS INT NO-UNDO INIT 1.

DEFINE VARIABLE iCL AS INTEGER NO-UNDO.
{syspara.i 5 1 1 iCL INT}

FIND FIRST Butiker NO-LOCK
     WHERE Butiker.Sentrallager
     NO-ERROR.
IF AVAIL Butiker THEN
  iProfNr = Butiker.ProfilNr.
ELSE DO:
  FIND FIRST Prisprofil NO-LOCK NO-ERROR.
  IF AVAIL Prisprofil THEN
    iProfNr = Prisprofil.ProfilNr.
END.
                                 
PROCEDURE plliste_alfastr:
  DEF INPUT  PARAM irPlListeLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId    AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn       AS CHAR NO-UNDO.

  FIND PlListeLinje WHERE ROWID(PlListeLinje) = irPlListeLinje NO-LOCK NO-ERROR.
  IF AVAIL PlListeLinje THEN DO:
    FIND FIRST StrKonv NO-LOCK
         WHERE StrKonv.StrKode = PlListeLinje.StrKode
         NO-ERROR.
    IF AVAIL StrKonv THEN 
      ocReturn = TRIM(StrKonv.Storl).
  END.
END PROCEDURE.

PROCEDURE plliste_strseq:
  DEF INPUT  PARAM irPlListeLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId    AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn       AS CHAR NO-UNDO.

  FIND PlListeLinje WHERE ROWID(PlListeLinje) = irPlListeLinje NO-LOCK NO-ERROR.
  IF AVAIL PlListeLinje THEN DO:
    FIND FIRST ArtBas NO-LOCK OF PlListeLinje
         NO-ERROR.
    IF AVAIL ArtBas THEN DO:
      FIND FIRST StrKonv NO-LOCK
           WHERE StrKonv.StrKode = PlListeLinje.StrKode
           NO-ERROR.
      IF AVAIL StrKonv THEN DO:
        FIND FIRST StrTStr NO-LOCK
             WHERE StrTStr.StrTypeID     = ArtBas.StrTypeID
               AND TRIM(StrTStr.SoStorl) = TRIM(StrKonv.Storl)
             NO-ERROR.
        IF AVAIL StrTStr THEN
          ocReturn = STRING(StrTStr.SeqNr).
      END.
    END.
  END.
END PROCEDURE.

PROCEDURE plliste_forslkost:    
  DEF INPUT  PARAM irPlListeLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId    AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn       AS CHAR NO-UNDO.

  FIND PlListeLinje WHERE ROWID(PlListeLinje) = irPlListeLinje NO-LOCK NO-ERROR.
  IF AVAIL PlListeLinje THEN DO:
    FIND FIRST ArtPris NO-LOCK 
         WHERE ArtPris.ArtikkelNr = PlListeLinje.ArtikkelNr
           AND ArtPris.ProfilNr = iProfNr
         NO-ERROR.
    IF AVAIL ArtPris THEN 
      ocReturn = STRING(ArtPris.VareKost[1] * PlListeLinje.Antall).
  END.
END PROCEDURE.

PROCEDURE plliste_bekrkost:    
  DEF INPUT  PARAM irPlListeLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId    AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn       AS CHAR NO-UNDO.

  FIND PlListeLinje WHERE ROWID(PlListeLinje) = irPlListeLinje NO-LOCK NO-ERROR.
  IF AVAIL PlListeLinje THEN DO:
    FIND FIRST ArtPris NO-LOCK 
         WHERE ArtPris.ArtikkelNr = PlListeLinje.ArtikkelNr
           AND ArtPris.ProfilNr = iProfNr
         NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris NO-LOCK 
         WHERE ArtPris.ArtikkelNr = PlListeLinje.ArtikkelNr NO-ERROR.
    IF AVAIL ArtPris THEN 
      ocReturn = STRING(ArtPris.VareKost[1] * PlListeLinje.AntallPlukket).
  END.
END PROCEDURE.

PROCEDURE plliste_cllagant:    
  DEF INPUT  PARAM irPlListeLinje AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId    AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocReturn       AS CHAR NO-UNDO.

  FIND PlListeLinje WHERE ROWID(PlListeLinje) = irPlListeLinje NO-LOCK NO-ERROR.
  IF AVAIL PlListeLinje THEN DO:
  
    FIND FIRST ArtLag NO-LOCK WHERE 
        ArtLag.ArtikkelNr = plListeLinje.ArtikkelNr AND 
        ArtLag.Butik      = iCL AND 
        ArtLag.StrKode    = plListeLinje.StrKode NO-ERROR.
  
    IF AVAIL ArtLag THEN 
      ocReturn = STRING(ArtLag.LagAnt).
  END.
END PROCEDURE.

