/* kampanje_opprettny.p

            IF NOT JBoxServerAPI:Instance:CallServerProc("kampanje_opprettny.p",
                     STRING(plRab%) + '|' +
                     STRING(dStartDato) + '|' +
                     STRING(iAktiveresTid) + '|' +
                     STRING(dSluttDato) + '|' +
                     STRING(iGyldigTilTid) + '|' +
                     STRING(cNotat) + '|' +
                     STRING(bIgnorerNOS) + '|' +
                     STRING(lKampanjePris) + '|' +
                     STRING(lKronerabatt) + '|' +
                     STRING(lMinstePris) + '|' +
                     STRING(iAvslagType),?) THEN

-----------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iKampanjeId AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE VARIABLE plRab% AS DECIMAL NO-UNDO.
DEFINE VARIABLE dStartDato AS DATE NO-UNDO.
DEFINE VARIABLE iAktiveresTid AS INTEGER NO-UNDO.
DEFINE VARIABLE dSluttDato AS DATE NO-UNDO.
DEFINE VARIABLE iGyldigTilTid AS INTEGER NO-UNDO.
DEFINE VARIABLE cNotat AS CHARACTER NO-UNDO.
DEFINE VARIABLE bIgnorerNOS AS LOG NO-UNDO.
DEFINE VARIABLE lKampanjePris AS DECIMAL NO-UNDO.
DEFINE VARIABLE lKronerabatt AS DECIMAL NO-UNDO.
DEFINE VARIABLE lMinstePris AS DECIMAL NO-UNDO.
DEFINE VARIABLE iAvslagType AS INTEGER NO-UNDO.
DEFINE VARIABLE cBeskrivelse AS CHARACTER NO-UNDO.
DEFINE VARIABLE iProfilNr AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
rStandardFunksjoner  = NEW cls.Stdfunk.StandardFunksjoner( INPUT cLogg ).    

ASSIGN 
  cLogg = 'kampanje_opprettny' + REPLACE(STRING(TODAY),'/','')
  .

/* Starter med tom linje i loggen. */
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '' 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.' 
    ).    

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Parametre:' 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    icParam: ' + icParam 
    ).    

ASSIGN 
  plRab% = DEC(ENTRY(1,icParam,'|'))
  dStartDato = DATE(ENTRY(2,icParam,'|')) 
  iAktiveresTid = INT(ENTRY(3,icParam,'|'))
  dSluttDato = DATE(ENTRY(4,icParam,'|')) 
  iGyldigTilTid = INT(ENTRY(5,icParam,'|'))
  cNotat = ENTRY(6,icParam,'|')
  lKampanjePris = DEC(ENTRY(8,icParam,'|'))
  lKronerabatt = DEC(ENTRY(9,icParam,'|'))
  lMinstePris = DEC(ENTRY(10,icParam,'|'))
  iAvslagType = INT(ENTRY(11,icParam,'|'))
  iProfilNr = INT(ENTRY(13,icParam,'|'))
  cBeskrivelse = ENTRY(12,icParam,'|')
  NO-ERROR.

  IF CAN-DO('TRUE,YES,JA',ENTRY(7,icParam,'|'))
    THEN bIgnorerNOS  = TRUE.
  ELSE bIgnorerNOS = FALSE.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cBeskrivelse: ' + cBeskrivelse  
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    plRab%: ' + STRING(plRab%) 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    dStartDato: ' + STRING(dStartDato) 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    iAktiveresTid: ' + STRING(iAktiveresTid) 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    dSluttDato: ' + STRING(dSluttDato) 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    iGyldigTilTid: ' + STRING(iGyldigTilTid) 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    bIgnorerNOS: ' + STRING(bIgnorerNOS) 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    lKampanjePris: ' + STRING(lKampanjePris) 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    lKronerabatt: ' + STRING(lKronerabatt) 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    lMinstePris: ' + STRING(lMinstePris) 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    iAvslagType: ' + STRING(iAvslagType) 
    ).    
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cNotat: ' + cNotat 
    ).    

DO TRANSACTION:
  CREATE KampanjeHode.
  ASSIGN 
    KampanjeHode.Beskrivelse = cBeskrivelse
    KampanjeHode.StartDato = dStartDato
    KampanjeHode.AktiveresTid = iAktiveresTid
    KampanjeHode.SluttDato = dSluttDato
    KampanjeHode.GyldigTilTid = iGyldigTilTid
    KampanjeHode.Kamp% = ABS(plRab%) * -1
    KampanjeHode.KampanjePris = lKampanjePris
    KampanjeHode.KroneRabatt = lKronerabatt
    KampanjeHode.MistePris = lMinstePris
    KampanjeHode.NormalPris = FALSE 
    KampanjeHode.AvslagType = iAvslagType 
    KampanjeHode.ProfilNr = iProfilNr 
    KampanjeHode.IgnorerNOS = bIgnorerNOS
    KampanjeHode.Notat = 'Kampanje hvor alle lagerstyrte artikler med lager, som ikke sto på lager, er lagt til.' + 
                         IF cNotat <> '' THEN CHR(10) ELSE '' + 
                         cNotat
    NO-ERROR.
  ASSIGN 
    obOK     = YES
    ocReturn = STRING(KampanjeHode.KampanjeId)
    .    
  RELEASE KampanjeHode.
END. /* TRANSACTION */

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.' 
    ).    


RETURN ocReturn.


