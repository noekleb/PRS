/* kordrehode_opprett_returordre.p

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lKOrdre_Id AS DECIMAL NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE cBruker AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER bufKOrdreHode FOR KOrdreHode.

ASSIGN 
    bTest       = TRUE
    obOk        = TRUE
    lKOrdre_Id  = INT(ENTRY(1,icParam,'|'))
    cBruker     = ENTRY(2,icParam,'|')  
    cLogg       = 'kordrehode_opprett_returordre' + REPLACE(STRING(TODAY),'/','') 
    .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
IF bTest THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        'Start' 
        ).    
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    Parametre: ' + icParam 
        ).    
  END.

FIND KOrdreHode NO-LOCK WHERE 
  KOrdrEHode.KOrdre_Id = lKOrdre_Id NO-ERROR.
IF AVAILABLE KOrdrEHode THEN 
DO TRANSACTION:
  CREATE bufKOrdrEHode.
  BUFFER-COPY KOrdreHode 
  EXCEPT KOrdre_Id RefKORdre_Id LevStatus VerkstedMerknad DatoTidOpprettet  ShipmentSendt RegistrertDato RegistrertTid Faktura_id FakturertDato FakturertTid FakturertAv
  TO bufKOrdreHode 
  ASSIGN
      /* KOrdre_Id settes i trigger.. */
      bufKOrdreHode.RefKOrdre_Id     = KOrdreHode.KOrdre_Id
      bufKOrdreHode.LevStatus        = '47' /* Utlevert status. */
      bufKOrdreHode.VerkstedMerknad  = 'Retur fra ordre: ' + KORdreHode.EkstOrdreNr + '.' + CHR(10) +
                                       'KordreId : ' + STRING(KORdreHode.Kordre_Id) + '.' + 
                                       'Fra butikk: ' + STRING(KOrdreHode.ButikkNr) + '.'
      bufKOrdreHode.SendingsNr       = 'RETUR'
      bufKOrdreHode.EkstOrdreNr      = KOrdreHode.EkstOrdreNr + ' ' + 'RETUR'
      bufKOrdreHode.RegistrertDato   = TODAY
      bufKOrdreHode.RegistrertTid    = TIME 
      bufKOrdreHode.DatoTidOpprettet = NOW
      bufKOrdreHode.DatoTidEndret    = NOW
      bufKOrdreHode.RegistrertAv     = cBruker
      bufKOrdreHode.BrukerId         = cBruker
      bufKORdreHode.ShipmentSendt    = ?
      bufKOrdrEHode.RegistrertDato   = TODAY 
      bufKOrdrEHode.RegistrertTid    = TIME 
      ocReturn = STRING(bufKOrdreHode.KOrdre_Id) 
      .
  IF bTest THEN 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    Retur ordre ' + string(bufKOrdreHode.KOrdre_Id) + ' er opprettet fra '+ string(KOrdreHode.KOrdre_Id) + '.' 
          ).    
  RELEASE bufKORdreHode.
END. /* TRANSACTION */  

ASSIGN 
  obOk = TRUE
  .
IF bTest THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        'Slutt' 
        ).    
  END.