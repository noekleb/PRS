DEFINE VARIABLE iCl AS INT NO-UNDO.
DEFINE VARIABLE obOk AS LOG NO-UNDO.
DEFINE VARIABLE cButLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNettButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iNetbutProfilNr AS INTEGER NO-UNDO.

/*{syspara.i 5 1 1 iCl INT}.           */
/*FIND Butiker NO-LOCK WHERE           */
/*  Butiker.Butik = iCl NO-ERROR.      */

DEFINE BUFFER bufButiker FOR Butiker.

{syspara.i 150 1 2 iNettButNr INT}

/* Skal normalt bare vise artikler som lager eller reservasjoner i nettbutikken. */
{syspar2.i 150 1 3 cButLst}
IF cButLst = '' THEN 
  cButLst = '16'.

/* Finner nettbutikken. */
FIND bufButiker NO-LOCK WHERE 
  bufButiker.Butik = iNettButNr NO-ERROR.
IF AVAILABLE bufButiker THEN 
  iNetbutProfilNr = bufButiker.ProfilNr.
ELSE 
  iNetbutProfilNr = 1. 

PROCEDURE ArtBas_HarLager:
  DEF INPUT  PARAM irArtBas  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

  ocValue = '0'.
  
  FIND ArtBas NO-LOCK
    WHERE ROWID(ArtBas) = irArtBas
    NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO:
    LAGERLOOP: 
    FOR EACH Lager OF ArtBas NO-LOCK WHERE 
      Lager.Butik = 16:
/*      Lager.Butik = INT(ENTRY(1,cButLst)):*/
        
/*      IF NOT CAN-DO(cButLst,STRING(Lager.Butik)) THEN*/
/*        NEXT.                                        */
      IF Lager.LagAnt > 0 THEN
        DO:
          ocValue = '1'.
          LEAVE LAGERLOOP.
        END.
    END.  /* LAGERLOOP  */
  END.
  
END PROCEDURE. 

PROCEDURE ArtBas_Kampanje:
  DEF INPUT  PARAM irArtBas  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

  ocValue = ''.
  
  FIND ArtBas NO-LOCK
    WHERE ROWID(ArtBas) = irArtBas
    NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO:
    FIND ArtPris OF ArtBas NO-LOCK WHERE 
      ArtPris.ProfilNr = iNetbutProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND ArtPris OF ArtBas NO-LOCK WHERE 
        ArtPris.ProfilNr = 1 NO-ERROR.
    IF AVAILABLE ArtPris AND 
      ArtPris.Tilbud = TRUE THEN 
      ocValue = '1'.
    ELSE 
      ocValue = '0'.
  END.
  
END PROCEDURE. 

PROCEDURE ArtBas_TilbudsPris:
  DEF INPUT  PARAM irArtBas  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

  ocValue = ''.
  
  FIND ArtBas NO-LOCK
    WHERE ROWID(ArtBas) = irArtBas
    NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO:
    FIND ArtPris OF ArtBas NO-LOCK WHERE 
      ArtPris.ProfilNr = iNetbutProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND ArtPris OF ArtBas NO-LOCK WHERE 
        ArtPris.ProfilNr = 1 NO-ERROR.
    IF AVAILABLE ArtPris AND 
      ArtPris.Tilbud = TRUE THEN 
        ASSIGN 
          ocValue = STRING(ArtPris.Pris[2])
          ocValue = IF ocValue = ? THEN '' ELSE OcValue
          .
    ELSE 
      ocValue = ''.
  END.
  
END PROCEDURE. 

PROCEDURE ArtBas_Pris:
  DEF INPUT  PARAM irArtBas  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.

  ocValue = ''.
  
  FIND ArtBas NO-LOCK
    WHERE ROWID(ArtBas) = irArtBas
    NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO:
    FIND ArtPris OF ArtBas NO-LOCK WHERE 
      ArtPris.ProfilNr = iNetbutProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND ArtPris OF ArtBas NO-LOCK WHERE 
        ArtPris.ProfilNr = 1 NO-ERROR.
    IF AVAILABLE ArtPris THEN 
        ASSIGN 
          ocValue = STRING(ArtPris.Pris[1])
          ocValue = IF ocValue = ? THEN '' ELSE OcValue
          .
    ELSE 
      ocValue = ''.
  END.
  
END PROCEDURE. 

PROCEDURE ArtBas_KampRab%:
  DEF INPUT  PARAM irArtBas  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  DEFINE VARIABLE plRabKr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE plRab%  AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lPris AS DECIMAL NO-UNDO.

  ocValue = ''.
  
  FIND ArtBas NO-LOCK
    WHERE ROWID(ArtBas) = irArtBas
    NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO:
    FIND ArtPris OF ArtBas NO-LOCK WHERE 
      ArtPris.ProfilNr = iNetbutProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND ArtPris OF ArtBas NO-LOCK WHERE 
        ArtPris.ProfilNr = 1 NO-ERROR.
    IF AVAILABLE ArtPris AND 
      ArtPris.Tilbud = TRUE THEN 
      DO:
/*        /* Henter pris fra kampanjen. */                   */
/*        FOR EACH KampanjeHode NO-LOCK WHERE                */
/*          KampanjeHode.Aktivert = TRUE AND                 */
/*          KampanjeHode.StartDato <= TODAY AND              */
/*          KampanjeHode.SluttDato >= TODAY,                 */
/*          FIRST KampanjeLinje OF KampanjeHode NO-LOCK WHERE*/
/*          KampanjeLinje.ArtikkelNr = ArtBas.ArtikkelNr:    */
/*          lPris = KampanjeLinje.Pris[1].                   */
/*        END.                                               */
/*        IF lPris = ? OR lPris = 0 THEN                     */
/*          lPris = ArtPris.Pris[1].                         */
        
        ASSIGN
/*          plRabKr = lPris - ArtPris.Pris[2]*/
          plRabKr = ArtPris.Pris[1] - ArtPris.Pris[2]
/*          plRab%  = ROUND((plRabKr * 100) / lPris,1)*/
          plRab%  = ROUND((plRabKr * 100) / ArtPris.Pris[1],0)    
          ocValue = STRING(plRab%)
          ocValue = IF ocValue = ? THEN '' ELSE OcValue
          .
      END.
    ELSE 
      ocValue = ''.
  END.
  
END PROCEDURE. 

PROCEDURE ArtBas_KampPris:
  DEF INPUT  PARAM irArtBas  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  DEFINE VARIABLE plRabKr AS DECIMAL NO-UNDO.
  DEFINE VARIABLE plRab%  AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lPris AS DECIMAL NO-UNDO.

  ocValue = ''.
  
  FIND ArtBas NO-LOCK
    WHERE ROWID(ArtBas) = irArtBas
    NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO:
    FIND ArtPris OF ArtBas NO-LOCK WHERE 
      ArtPris.ProfilNr = iNetbutProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND ArtPris OF ArtBas NO-LOCK WHERE 
        ArtPris.ProfilNr = 1 NO-ERROR.
    IF AVAILABLE ArtPris AND 
      ArtPris.Tilbud = TRUE THEN 
      DO:
        FOR EACH KampanjeHode NO-LOCK WHERE 
          KampanjeHode.Aktivert = TRUE AND
          KampanjeHode.StartDato <= TODAY AND 
          KampanjeHode.SluttDato >= TODAY,
          FIRST KampanjeLinje OF KampanjeHode NO-LOCK WHERE 
          KampanjeLinje.ArtikkelNr = ArtBas.ArtikkelNr:
          lPris = KampanjeLinje.Pris[1].
        END.
        IF lPris = ? OR lPris = 0 THEN 
          lPris = ArtPris.Pris[1].
        
        ASSIGN
          ocValue = STRING(lPris)
          ocValue = IF ocValue = ? THEN '' ELSE OcValue
          .
      END.
    ELSE 
      ocValue = ''.
  END.
  
END PROCEDURE. 

PROCEDURE ArtBas_HovedKategori:
  DEF INPUT  PARAM irArtBas  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  ocValue = ''.
  
  FIND ArtBas NO-LOCK
    WHERE ROWID(ArtBas) = irArtBas
    NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO:
    FIND HovedKategori NO-LOCK WHERE 
      HovedKategori.HovedKatNr = ArtBas.HovedKatNr NO-ERROR.
    IF AVAILABLE HovedKategori THEN 
      DO:
        ASSIGN
          ocValue = HovedKategori.HovedKatTekst
          .
      END.
    ELSE 
      ocValue = ''.
  END.
END PROCEDURE.

PROCEDURE ArtBas_Varemerke:
  DEF INPUT  PARAM irArtBas  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  ocValue = ''.
  
  FIND ArtBas NO-LOCK
    WHERE ROWID(ArtBas) = irArtBas
    NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO:
    FIND Varemerke NO-LOCK WHERE 
      Varemerke.VmId = ArtBas.VmId NO-ERROR.
    IF AVAILABLE VareMerke THEN 
      DO:
        ASSIGN
          ocValue = Varemerke.Beskrivelse
          .
      END.
    ELSE 
      ocValue = ''.
  END.
  
END PROCEDURE. 

PROCEDURE ArtBas_Brukskode:
  DEF INPUT  PARAM irArtBas  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  ocValue = ''.
  
  FIND ArtBas NO-LOCK
    WHERE ROWID(ArtBas) = irArtBas
    NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO:
    FIND Anv-Kod NO-LOCK WHERE 
      Anv-Kod.Anv-Id = ArtBas.Anv-Id NO-ERROR.
    IF AVAILABLE Anv-Kod THEN 
      DO:
        ASSIGN
          ocValue = Anv-Kod.AnvBeskr
          .
      END.
    ELSE 
      ocValue = ''.
  END.
END PROCEDURE.

PROCEDURE ArtBas_Produsent:
  DEF INPUT  PARAM irArtBas  AS ROWID NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR NO-UNDO.
  
  ocValue = ''.
  
  FIND ArtBas NO-LOCK
    WHERE ROWID(ArtBas) = irArtBas
    NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO:
    FIND Produsent NO-LOCK WHERE 
      Produsent.ProdNr = ArtBas.ProdNr NO-ERROR.
    IF AVAILABLE Produsent THEN 
      DO:
        ASSIGN
          ocValue = Produsent.Beskrivelse
          .
      END.
    ELSE 
      ocValue = ''.
  END.
  
END PROCEDURE. 










