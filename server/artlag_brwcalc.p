DEFINE VARIABLE iCl            AS INT NO-UNDO.
DEFINE VARIABLE iUtvidetStatus AS INTEGER NO-UNDO.

DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER bufArtLag FOR ArtLag.
DEFINE VARIABLE iNetbut AS INTEGER NO-UNDO.

{syspara.i 150 1 2 iNetBut INT}
{syspara.i 5 1 1 iCl INT}.
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl NO-ERROR.


PROCEDURE ArtLag_Recid:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
      ocValue = STRING(RECID(ArtLag)).
    ELSE ocValue = ''.
END PROCEDURE.

PROCEDURE ArtLag_NOS:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN 
            ocValue = ArtBas.Lagerkoder.        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_Beskr:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN 
            ocValue = ArtBas.Beskr.        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_LevNr:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN 
            ocValue = STRING(ArtBas.LevNr).        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_LevKod:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN 
            ocValue = ArtBas.LevKod.        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_LevFargKod:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        FIND ArtBas NO-LOCK WHERE 
            ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN 
            ocValue = ArtBas.LevFargKod.        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_Sasong:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN 
          FIND SaSong OF ArtBas NO-LOCK NO-ERROR.
        IF AVAILABLE Sasong THEN 
            ocValue = STRING(Sasong.Sasong).        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_SasId:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN 
          FIND SaSong OF ArtBas NO-LOCK NO-ERROR.
        IF AVAILABLE Sasong THEN 
            ocValue = STRING(SaSong.Sasong).
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_Varemerke:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN 
          FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
        IF AVAILABLE Varemerke THEN 
            ocValue = Varemerke.Beskrivelse.        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_VmId:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN 
          FIND Varemerke OF ArtBas NO-LOCK NO-ERROR.
        IF AVAILABLE Varemerke THEN 
            ocValue = STRING(Varemerke.VmId).        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_VVareKost:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        FIND Lager NO-LOCK WHERE 
          Lager.ArtikkelNr = ArtLag.ArtikkelNr AND 
          Lager.Butik      = ArtLag.butik NO-ERROR.
        IF AVAILABLE Lager THEN 
            ocValue = IF Lager.VVarekost <> ? THEN STRING(Lager.VVareKost) ELSE ''.        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_Tilbud:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        IF AVAILABLE ArtPris THEN RELEASE ArtPris.
        FIND Butiker NO-LOCK WHERE 
          Butiker.Butik = ArtLag.butik NO-ERROR.
        IF AVAILABLE Butiker THEN 
        DO:
          FIND ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtLag.ArtikkelNr AND 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        END.
        IF AVAILABLE ArtPris THEN 
            ocValue = IF ArtPris.TilBud THEN '*' ELSE ''.        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_Pris:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        IF AVAILABLE ArtPris THEN RELEASE ArtPris.
        FIND Butiker NO-LOCK WHERE 
          Butiker.Butik = ArtLag.butik NO-ERROR.
        IF AVAILABLE Butiker THEN 
        DO:
          FIND ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtLag.ArtikkelNr AND 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        END.
        IF AVAILABLE ArtPris THEN 
            ocValue = STRING(ArtPris.Pris[1]).        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_TilbPris:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        IF AVAILABLE ArtPris THEN RELEASE ArtPris.
        FIND Butiker NO-LOCK WHERE 
          Butiker.Butik = ArtLag.butik NO-ERROR.
        IF AVAILABLE Butiker THEN 
        DO:
          FIND ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtLag.ArtikkelNr AND 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        END.
        IF AVAILABLE ArtPris THEN 
            ocValue = IF ArtPris.Tilbud THEN STRING(ArtPris.Pris[2]) ELSE ''.        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_Varekost:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        IF AVAILABLE ArtPris THEN RELEASE ArtPris.
        FIND Butiker NO-LOCK WHERE 
          Butiker.Butik = ArtLag.butik NO-ERROR.
        IF AVAILABLE Butiker THEN 
        DO:
          FIND ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtLag.ArtikkelNr AND 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        END.
        IF AVAILABLE ArtPris THEN 
            ocValue = STRING(ArtPris.Varekost[IF ArtPris.tilbud THEN 2 ELSE 1]).        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_Rab%:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        IF AVAILABLE ArtPris THEN RELEASE ArtPris.
        FIND Butiker NO-LOCK WHERE 
          Butiker.Butik = ArtLag.butik NO-ERROR.
        IF AVAILABLE Butiker THEN 
        DO:
          FIND ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtLag.ArtikkelNr AND 
            ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
          FIND FIRST ArtPris NO-LOCK WHERE 
            ArtPris.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        END.
        IF AVAILABLE ArtPris THEN 
            ocValue = STRING(ArtPris.Rab1%[IF ArtPris.tilbud THEN 2 ELSE 1]).        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_Reservert:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        FIND bufArtLag NO-LOCK WHERE 
          bufArtLag.ArtikkelNr = ArtLag.ArtikkelNr AND 
          bufArtLag.Butik      = iNetBut AND 
          bufArtLag.StrKode    = ArtLag.StrKode NO-ERROR.
        IF AVAILABLE bufArtLag THEN 
            ocValue = STRING(bufArtLag.LagAnt).        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtLag_Kode:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        FIND LAST Strekkode NO-LOCK WHERE 
          StrekKode.ArtikkelNr = ArtLag.ArtikkelNr AND 
          StrekKode.StrKode    = ArtLag.StrKode NO-ERROR.
        IF AVAILABLE Strekkode THEN 
            ocValue = StrekKode.Kode.        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtBas_WebButikkArtikkel:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN 
            ocValue = STRING(ArtBas.WebButikkArtikkel).        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.

PROCEDURE ArtBas_PubliserINettButikk:
  DEF INPUT  PARAM irArtLag  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
    FIND ArtLag NO-LOCK
        WHERE ROWID(ArtLag) = irArtLag
        NO-ERROR.
    IF AVAILABLE ArtLag THEN 
    DO:
        FIND ArtBas NO-LOCK WHERE 
          ArtBas.ArtikkelNr = ArtLag.ArtikkelNr NO-ERROR.
        IF AVAILABLE ArtBas THEN 
            ocValue = STRING(ArtBas.PubliserINettButikk).        
        ELSE ocValue = ''.
    END.
    ELSE ocValue = ''.

END PROCEDURE.
























