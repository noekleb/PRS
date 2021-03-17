
/*------------------------------------------------------------------------
    File        : pksdlAvsluttKampanje.p
    Purpose     : Ved varemottak av pakksedler fra Gant Global, skal kampanje avsluttes. Årsak er at varemottak initierer at artikkelen går inn i en ny sesong, og skal ikke lenger selges på tilbud. Gjelder for varemottak av pakksedler for Gant global, men ikke fo rpakksedler som kommer fra overføringer mellom butikker.

    Syntax      :

    Description : På Outlet - avslutter kampanje som er aktive på varer som ligger på pakkseddelen. Sletter også artikkelen fra de kampanjene den er med på i kampanjeregisteret.

    Author(s)   : 
    Created     : Fri Aug 25 13:17:52 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER lPkSdlId LIKE PkSdlHode.PkSdlId NO-UNDO.

DEFINE VARIABLE cOutletLst    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iButNr        AS INTEGER   NO-UNDO.
DEFINE VARIABLE iGantAktiv    AS INTEGER   NO-UNDO.
DEFINE VARIABLE cLogg         AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLinjeNr      AS INTEGER NO-UNDO.
DEFINE VARIABLE ocReturn AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail          AS cls.SendEMail.SendEMail        NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
{syspara.i 22 5 2 cOutletLst}
{syspara.i 210 100 8 iGantAktiv INT}

DEFINE TEMP-TABLE ttMailLogg 
  FIELD LinjeNr AS INTEGER 
  FIELD Tekst AS CHARACTER 
  .

DEFINE STREAM Ut.

ASSIGN 
    cLogg = 'pksdlAvsluttKampanje' + REPLACE(STRING(TODAY),'/','')
    .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Start.' 
  ).

/* SJekker at aktuell kunde er aktiv. */
IF iGantaktiv <> 1 THEN
DO: 
  ocReturn = '  ** Gant ikke aktiv. - avbryter.'. 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    ocReturn 
    ).
  RETURN ocReturn.
END.
FIND PkSdlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlId = lPkSdlId  NO-ERROR.

/* Ukjent pakkseddel */
IF NOT AVAILABLE pkSdlHode THEN 
DO:
  ocReturn = '  ** Ukjent pakkseddel - avbryter.'. 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    ocReturn 
    ).
  RETURN ocReturn.
END.
    
/* Bare FORWARD ordre fra Gant Global skal behandles. */
IF NOT CAN-DO('1,12',STRING(PkSdlHode.OrdreType)) THEN
DO: 
  ocReturn = '  ** Pakksedel har feil opphav - avbryter.'. 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    ocReturn 
    ).
  RETURN ocReturn.
END. 

/* Bare pakksedler til Outlet skal ha denne behandllingen. */
FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK  NO-ERROR.
IF NOT AVAILABLE PkSdlLinje THEN
DO: 
  ocReturn = '  ** Pakkseddel uten varelinjer.'. 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    ocReturn 
    ).
  RETURN ocReturn.
END.
/* Henter butikken for å finne prisprofilen */
FIND Butiker NO-LOCK WHERE 
    butiker.butik = PkSdlLinje.butik NO-ERROR.
IF NOT AVAILABLE butiker THEN
DO: 
  ocReturn = '  ** Ukjent butikk på pakkseddel.'. 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    ocReturn 
    ).
  RETURN ocReturn.
END.
ibutNr = Butiker.butik.

rStandardFunksjoner:SkrivTilLogg(cLogg, 
    'Behandler pakkseddel - PakkseddelId: ' + STRING(PkSdlHode.PkSdlId)   
    + ' PkSdlNr: ' + PkSdlHode.PkSdlNr 
    + ' Butikk: ' + STRING(PkSdlLinje.butikkNr) 
    + '.' 
    ).
    
EMPTY TEMP-TABLE ttMailLogg.
    
/* Slår av kampanje på alle artikler på pakkseddelen. */
FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK WHERE
    CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr) AND 
      PkSdlLinje.NySesongkode > '':

    FIND ArtBas NO-LOCK WHERE 
        ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
    
    /* Sesong skal være byttet, dvs. at det er flagget sesongbytte på linjen.               */
    /* Og sesongen på linjen skal være lik sesongen på artikkelen.                          */
    /* Sesongkoden på linjen ble satt på artikkelen når pakkseddelen ble importert.         */
    IF AVAILABLE ArtBas THEN 
    REMOVE_KAMPANJE:
    DO:
      RUN artbasPriskoTilbudDeAktiver.p (cLogg, PkSdlLinje.ArtikkelNr, PksdlLinje.PkSdlLinjeId, INPUT-OUTPUT TABLE ttMailLogg).
    END. /* REMOVE_KAMPANJE */   
END.      

IF CAN-FIND(FIRST ttMailLogg) THEN
DO: 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Start av sendeMailKampanje.'
      ).
  RUN sendeMailKampanje.
END.
rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Slutt.' 
  ).
  
PROCEDURE sendeMailKampanje:
  DEFINE VARIABLE cFilNavn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPrefix AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cTekst  AS CHARACTER NO-UNDO.

  IF SEARCH('tnc.txt') <> ? THEN 
    cPrefix = 'TEST '.
  ELSE 
    cPrefix = ''.
  cTekst = cPrefix + 'Avsluttet kampanje på artikler. Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + '.'.

  cFilNavn = rStandardFunksjoner:getTempFileName().
  OUTPUT STREAM Ut TO VALUE(cFilNavn).
  FOR EACH ttMailLogg
    BY ttMailLogg.LinjeNr:
    PUT STREAM Ut UNFORMATTED 
      ttMailLogg.Tekst
      SKIP.
  END.
  OUTPUT STREAM Ut CLOSE.    

  {syspara.i 50 50 37 rSendEMail:parToADDRESS}  
  rSendEMail:parMessage-Charset = 'iso-8859-1'. /* Blank eller 'UTF-8' når det går fra fil. */
  rSendEMail:parMessage-File    = cFilNavn. 
  rSendEMail:parMESSAGE         =  ''.
  rSendEMail:parMailType        = 'FakturaMail'.
  rSendEMail:parSUBJECT         = cTekst. 
  rSendEMail:parFILE            = ''.
  IF rSendEMail:parToADDRESS <> '' THEN   
    rSendEMail:send( ).
END PROCEDURE. 
       
