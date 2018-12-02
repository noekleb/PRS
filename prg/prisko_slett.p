/* prisko_slett.p 
*/

DEFINE INPUT PARAMETER pRowIdPrisKo AS ROWID NO-UNDO.

DEFINE VARIABLE cTekst        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCL           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iClProfilNr   AS INTEGER   NO-UNDO.
DEFINE VARIABLE bKopierPrisko AS LOG       NO-UNDO.

DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER LokPrisKo FOR PrisKo.

/* Henter profilnr på sentrallageret. */
{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCl NO-ERROR.
IF AVAILABLE clButiker THEN 
  iClProfilNr = clButiker.ProfilNr.
ELSE
  iClProfilNr = 1.
  
/* Sjekker om priskøpost skal kopieres til alle andre prisprofiler. */
{syspara.i 2 4 40 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bKopierPrisko = TRUE.
ELSE
  bKopierPrisko = FALSE.

/* Kopiering skal ikke gjøres */
IF bKopierPrisko = FALSE THEN 
  RETURN.

/* Henter priskøposten som skal kopieres */
FIND Prisko NO-LOCK WHERE
  ROWID(Prisko) = pRowIdPrisKo NO-ERROR.
IF NOT AVAILABLE PrisKo THEN 
  RETURN.
  
/* Kun køposter på hk profilen skal kunne kopieres. */
IF PrisKo.ProfilNr <> iClProfilNr THEN 
  RETURN.
    
BUTIKKLOOP:
FOR EACH Butiker NO-LOCK  WHERE 
  Butiker.harButikksystem  = TRUE AND
  Butiker.ApningsDato     <= TODAY AND
  Butiker.NedlagtDato      = ?:
  /* Kopierer ikke til seg selv. */
  IF Butiker.ProfilNr = PrisKo.ProfilNr THEN 
    NEXT BUTIKKLOOP.
    
  /* Hent/opprett aktiveringspost. */
  FIND LokPrisKo EXCLUSIVE-LOCK WHERE
    LokPrisKo.ArtikkelNr    = PrisKo.ArtikkelNr AND
    LokPrisKo.ProfilNr      = Butiker.ProfilNr AND
    LokPrisKo.AktiveresDato = PrisKo.AktiveresDato AND
    LokPrisKo.AktiveresTid  = PrisKo.AktiveresTid AND
    LokPrisKo.Tilbud        = PrisKo.Tilbud NO-ERROR.
  /* Hvis den finnes, skal den slettes. */
  IF AVAILABLE LokPrisKo THEN 
    DELETE LokPrisKo.
  
END. /* BUTIKKLOOP */  