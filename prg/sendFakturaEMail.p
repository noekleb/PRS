/* sendFakturaEMail.p 

Sende mail med faktura ved:
    * Varemottak i Outlett av pakksedler som kommer fra 20 (Fra overføringer). Opphav = 4.
    * Varemottak i 16 av pakksedler hvor pakkseddel er tatt fra outlet's pakkseddel liste. Opphav = 7.
    * Varemottak i 20 hvor varer er overført fra 16.

*/

DEFINE INPUT PARAMETER lFaktura_Id AS DECIMAL NO-UNDO.

DEFINE VARIABLE ocReturn            AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE iMotbutNr           AS INTEGER                        NO-UNDO.
DEFINE VARIABLE cLogg               AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE piAktiv             AS INTEGER                        NO-UNDO.
DEFINE VARIABLE cMottagerLst        AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE cSenderLst          AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE bTest               AS LOG                            NO-UNDO.
DEFINE VARIABLE cTekst              AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE iNettBut AS INTEGER NO-UNDO.
DEFINE VARIABLE iNettLager AS INTEGER NO-UNDO.

DEFINE VARIABLE iEyScanAktiv   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iEyScanAdrType AS INTEGER   NO-UNDO.
DEFINE VARIABLE cEyScanEMail   AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail          AS cls.SendEMail.SendEMail        NO-UNDO.
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

/* Er mailsending aktiv? - hvis ikke - avslutt. */
{syspar2.i 50 50 34 piAktiv INT}
/* Gyldige sendere og mottagere */
{syspara.i 50 50 35 cMottagerLst}
{syspar2.i 50 50 35 cSenderLst}

{syspara.i 50 55 5 iEyScanAktiv INT}
{syspara.i 50 55 6 iEyScanAdrType INT}
{syspar2.i 50 55 6 cEyScanEMail}
{syspara.i 150 1 2 iNettBut INT}
{syspara.i 150 1 3 iNettLager INT}

SUBSCRIBE 'fakturaFilNavn' ANYWHERE.

ASSIGN 
  bTest = TRUE 
  cLogg = 'SendEMail' + REPLACE(STRING(TODAY),'/','')
  .

rStandardFunksjoner:SkrivTilLogg(cLogg,
  '' 
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  'sendFakturaEMail:' 
  ).

HANDLING:
DO:
        
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'SendeMail Start.' 
    ).

  IF piAktiv <> 1 THEN 
  DO:
    ocReturn = 'Sending av fakturamail er ikke aktiver. Se syspara 50 50 34.'.
    LEAVE HANDLING.
  END.

/* TN 11/5-20 Dette taes ikke lenger hensyn til lenger nede. Det er åpnet for alle     */
/* butikker.                                                                           */
/*  IF cMottagerLst = '' OR cSenderLst = '' THEN                                       */
/*  DO:                                                                                */
/*    ocReturn = 'Gyldige sendere og mottagere er ikke satt opp. Se syspara 50 50 35.'.*/
/*    LEAVE HANDLING.                                                                  */
/*  END.                                                                               */
    
  FIND FakturaHode NO-LOCK WHERE 
    FakturaHode.Faktura_Id = lFaktura_Id NO-ERROR.
  IF NOT AVAILABLE FakturaHode THEN
  DO:
    ocReturn = 'Ukjent fakura id (' + STRING(lFaktura_Id) + ').'.
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      ocReturn 
      ).
    LEAVE HANDLING.
  END. 
  FIND Butiker NO-LOCK WHERE 
    Butiker.butik = FakturaHode.butikkNr NO-ERROR.
  IF NOT AVAILABLE Butiker THEN
  DO:
    ocReturn = 'Ukjent butikk angitt på faktura (' + STRING(FakturaHode.butikkNr) + ').'.
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      ocReturn 
      ).
    LEAVE HANDLING.
  END. 
    
  IF FakturaHode.FakturaNr > 0 THEN 
  DO:
    FIND LAST PkSdlHode NO-LOCK WHERE 
      PkSdlHode.FakturaNr = FakturaHode.FakturaNr NO-ERROR.
  END.
  IF NOT AVAILABLE PkSdlHode THEN
  DO:
    ocReturn =  'Ingen pakkseddel koblet til fakturen (' + STRING(FakturaHode.FakturaNr) + ').'.
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      ocReturn 
      ).
/*    LEAVE HANDLING.*/
  END.
    
  /* Henter mottagende butikk fra pakkseddel linjen. */
  IF AVAILABLE PkSdlHode THEN 
  DO:
    FIND FIRST PkSdlLinje OF PkSdlHode NO-LOCK NO-ERROR.
    IF AVAILABLE PkSdlLinje THEN 
      iMotButNr = PkSdlLinje.ButikkNr.
    ELSE 
      iMotButNr = 0. 
  END.
  
  /* Faktura skal ikke sendes ved overføring til +/- butikkene. */
  FIND Kunde OF FakturaHode NO-LOCK NO-ERROR.
  IF AVAILABLE Kunde AND  
    CAN-DO('848,849',STRING(Kunde.ButikkNr)) THEN 
  DO:
    IF bTest THEN
      ocReturn =  'Det skal ikke sendes mail ved overføring til +/- butikkene.  (Fra: ' + STRING(FakturaHode.ButikkNr) +  ' til ' + STRING(Kunde.ButikkNr) + ').'.
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      ocReturn 
      ).
    LEAVE HANDLING.
  END.
  /* Faktura mellom Nettbutikk og Nettbutikks lager, skal ikke sendes. */
  IF AVAILABLE Kunde THEN 
  DO:
    IF FakturaHode.ButikkNr = iNettBut AND 
      Kunde.ButikkNr = iNettLager THEN
      DO:
        ocReturn = 'Avbrutt. Fra nettbutikk til nettbutikks lager.'.
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          ocReturn 
          ).
        LEAVE HANDLING.
      END.
    IF FakturaHode.ButikkNr = iNettLager AND 
      Kunde.ButikkNr = iNettBut THEN
      DO: 
        ocReturn = 'Avbrutt. Fra nettbutikks lager til nettbutikks.'.
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          ocReturn 
          ).
        LEAVE HANDLING.
      END.
  END.
  IF AVAILABLE Kunde AND iMotbutNr = 0 THEN
    iMotButNr = Kunde.ButikkNr. 

  /* TN 12/1-20 Nå skal faktura alltid sendes ved overføringer mellom butikkene. */
/*  /* Sjekk av gylidige sender og mottagere */                                                                  */
/*  IF NOT CAN-DO(cSenderLst,STRING(FakturaHode.ButikkNr)) OR NOT CAN-DO(cMottagerLst,STRING(iMotButNr)) THEN    */
/*  DO:                                                                                                          */
/*    IF bTest THEN                                                                                              */
/*      ocReturn =  'Ugyldig sender/mottager (' + STRING(FakturaHode.ButikkNr) +  '/' + STRING(iMotButNr) + ').'.*/
/*    LEAVE HANDLING.                                                                                            */
/*  END.                                                                                                         */
    
  /* Generer bare faktura pdf fil. Sender filnavn tilbake med publish når kode BAREFIL sendes inn.        */
  /* NB: Legges koden inn i første parmeter, virker det ikke selv om allt ser ok ut på mottagersiden :( . */
  /* Når utskriften er gjort, publiseres til 'fakturafilnavn', og faktura sendes på email.                */
  cTekst = 'BAREFIL'.
  RUN skrivfaktura.p (STRING(FakturaHode.Faktura_Id) + "|",
                      TRUE,
                      Butiker.RapPrinter,
                      -1, /* Flagger eMail utskrift. */
                      cTekst,
                      1).
END. /* HANDLING */

IF ocReturn <> '' THEN 
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  ' + ocReturn 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  'sendFakturaEMail ferdig' 
  ).

RETURN ocReturn.

/* Her mottas filnavn og mail sendes. */
PROCEDURE fakturaFilNavn:
  DEFINE INPUT PARAMETER pcFilNavn AS CHARACTER NO-UNDO.

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    'SUBSCRIBE fakturaFilNavn ' + pcFilNavn 
    ).
  
  DEFINE VARIABLE cPrefix AS CHARACTER NO-UNDO.
  IF SEARCH('tnc.txt') <> ? THEN 
    cPrefix = 'TEST '.
  ELSE 
    cPrefix = ''.

  cTekst = ''.
  IF AVAILABLE PkSdlHode THEN 
  DO:
    IF PkSdlHode.OrdreType = '90' THEN 
      cTekst = cPrefix + 'Varemottak ekstern leverandør ' + STRING(PkSdlHode.levnr) + ' ' + PkSdlHode.levnamn + ' mottatt i butikk ' + STRING(iMotbutNr) + ' Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + '.'.
    ELSE IF CAN-DO('1,12',PkSdlHode.OrdreType) THEN 
      cTekst = cPrefix + 'Varemottak FORWARD ordre fra leverandør ' + STRING(PkSdlHode.levnr) + ' ' + PkSdlHode.levnamn + ' mottatt i butikk ' + STRING(iMotbutNr) + ' Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + '.'.
    ELSE IF PkSdlHode.OrdreType <> '' THEN 
      cTekst = cPrefix + 'Varemottak STOCK ordre fra leverandør ' + STRING(PkSdlHode.levnr) + ' ' + PkSdlHode.levnamn + ' mottatt i butikk ' + STRING(iMotbutNr) + ' Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + '.'.
  END.
  IF cTekst = '' THEN 
    cTekst = cPrefix + 'Faktura fra overf. fra ' + STRING(FakturaHode.butikkNr) + ' til ' + STRING(iMotbutNr) + ' Dato/Tid: ' + STRING(NOW,"99/99/99 hh:mm:ss") + '.'.

  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Fil: ' + pcFilNavn 
    ).

  /* TN 9/3-20 Lagt inn eksport av tekst. */
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  cTekst: ' + cTekst 
    ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Overført fra: ' + STRING(FakturaHode.butikkNr) 
    ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Overført til: ' + STRING(iMotButNr) 
    ).

  /* TN 12/1-20 Faktura skal sendes på eMail til eyescan. Overstyrer default adresse som settes over.*/
  IF iEyScanAktiv = 1 AND NUM-ENTRIES(cEyScanEMail,'@') > 1 THEN  
  EYSCAN:
  DO:
    /* 0=Sende til kundens email adresse. Hentes fra butikkens eMail. */ 
    IF iEyScanAdrType = 0 THEN 
      LEAVE EYSCAN.
    /* 1=Sender bare til EyScan adressen. */
    ELSE IF iEyScanAdrType = 1 THEN 
      rSendEMail:parToADDRESS = cEyScanEMail.
    /* 2=Sender til begge adresser. */ 
    ELSE IF iEyScanAdrType = 2 THEN
      rSendEMail:parToADDRESS = TRIM(rSendEMail:parToADDRESS,';') + (IF cEyScanEMail <> '' THEN ';' ELSE '') + cEyScanEMail.  
  END. /* EYSCAN */

  IF AVAILABLE PkSdlHode THEN 
    rSendEMail:parMESSAGE         =  cPrefix + 'Vedrørende pakkseddel: ' + PkSdlHode.PkSdlNr + '.' + CHR(10) +   
      PkSdlHode.Merknad.
  ELSE 
    rSendEMail:parMESSAGE         =  cPrefix + 'Overføring.'.
  
  rSendEMail:parMailType        = 'FakturaMail'.
  rSendEMail:parSUBJECT         = cTekst. 
  rSendEMail:parMessage-Charset = 'iso-8859-1'. /* Blank eller 'UTF-8' når det går fra fil. */
  rSendEMail:parFILE            = pcFilNavn.  
  rSendEMail:send( ).
END PROCEDURE. 

