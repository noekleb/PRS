
/*------------------------------------------------------------------------
    File        : opprettHPrisko.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Wed Feb 12 09:30:00 CET 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cLogg               AS CHARACTER                      NO-UNDO.
DEFINE VARIABLE hQuery              AS HANDLE                         NO-UNDO.
DEFINE VARIABLE iEndringsNr         AS INTEGER                        NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

DEFINE BUFFER bufArtPris FOR artPris.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN 
  cLogg = 'opprettHPrisko' + REPLACE(STRING(TODAY),'/','')
  .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Slutt.' 
  ).

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

/* Det er alltid eCom prisen vi får inn her. Profilnr 16. */
BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
  FIND bufArtPris EXCLUSIVE-LOCK WHERE
    bufArtPris.ArtikkelNr = DECIMAL(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) AND 
    bufArtPris.ProfilNr   = INTEGER(ihBuffer:BUFFER-FIELD("ProfilNr"):BUFFER-VALUE) NO-ERROR.
  IF AVAILABLE bufArtPris THEN
  DO: 
    RUN opprettHPrisko.
  END.
  hQuery:GET-NEXT().
END. /* BLOKKEN */

DELETE OBJECT hQuery NO-ERROR.

obOk = TRIM(ocReturn) = ''.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Slutt.' 
  ).

RETURN ocReturn.

/* ***************************  Prodedure *************************** */
PROCEDURE opprettHPrisko:
  DEFINE BUFFER HPrisKo FOR HPrisKo.
  
  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = bufArtPris.ArtikkelNr NO-ERROR.
    
  IF AVAILABLE bufArtPris AND AVAILABLE ArtBas THEN 
  DO FOR hPrisKo TRANSACTION:  
    FIND FIRST hPrisKo NO-LOCK WHERE /* NB: EndringsNr er Descending */ 
      hPrisKo.ArtikkelNr = bufArtPris.ArtikkelNr AND 
      HPrisKo.ProfilNr   = bufArtPris.ProfilNr NO-ERROR.
    IF AVAILABLE hPrisKo THEN 
      iEndringsNr = hPrisKo.endringsNr + 1.
    ELSE 
      iEndringsNr = 1.
      
    /* Logger endret pris i priskø. */
    CREATE HPrisKo.
    ASSIGN
      HPrisKo.ArtikkelNr = bufArtPris.ArtikkelNr
      HPrisKo.ProfilNr   = bufArtPris.ProfilNr
      HPrisKo.EndringsNr = iEndringsNr
      .
    ASSIGN
      HPrisKo.LevNr        = ArtBas.LevNr
      HPrisKo.ValPris      = bufArtPris.ValPris[1]
      HPrisKo.InnkjopsPris = bufArtPris.InnKjopsPris[1]
      HPrisKo.Rab1Kr       = bufArtPris.Rab1Kr[1]
      HPrisKo.Rab1%        = bufArtPris.Rab1%[1]
      HPrisKo.Rab2Kr       = bufArtPris.Rab2Kr[1]
      HPrisKo.Rab2%        = bufArtPris.Rab2%[1]
      HPrisKo.Frakt        = bufArtPris.Frakt[1]
      HPrisKo.Frakt%       = bufArtPris.Frakt%[1]
      HPrisKo.DivKostKr    = bufArtPris.DivKostKr[1]
      HPrisKo.DivKost%     = bufArtPris.DivKost%[1]
      HPrisKo.Rab3Kr       = bufArtPris.Rab3Kr[1]
      HPrisKo.Rab3%        = bufArtPris.Rab3%[1]
      HPrisKo.DBKr         = bufArtPris.DBKr[1]
      HPrisKo.DB%          = bufArtPris.DB%[1]
      HPrisKo.Pris         = bufArtPris.Pris[1]
      HPrisKo.EuroPris     = bufArtPris.EuroPris[1]
      HPrisKo.EuroManuel   = bufArtPris.EuroManuel.

    ASSIGN
      HPrisKo.Tilbud         = FALSE
      HPrisKo.AktiveresDato  = TODAY
      HPrisKo.GyldigTilDato  = ?
      HPrisKo.AktiveresTid   = TIME
      HPrisKo.GyldigTilTid   = 0
      HPrisKo.Timestyrt      = bufArtPris.TilbudTimeStyrt

      HPrisKo.Aktivert       = TRUE
      HPrisKo.Type           = 1 /* NormalPris endring. */
      HPrisKo.VareKost       = bufArtPris.VareKost[1]
      HPrisKo.MvaKr          = bufArtPris.MvaKr[1]
      HPrisKo.Mva%           = bufArtPris.Mva%[1]

      HPrisKo.EDato          = TODAY
      HPrisKo.ETid           = TIME
      HPrisKo.BrukerID       = USERID("dictdb")

      HPrisKo.RegistrertDato = TODAY
      HPrisKo.RegistrertTid  = TIME
      HPrisKo.RegistrertAv   = USERID("dictdb")
      .

    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  HPrisko: ' + STRING(ArtBas.ArtikkelNr) + ' ' +
        STRING(HPrisKo.ProfilNr) + ' ' + 
        STRING(HPrisko.EndringsNr) 
      ).
      
    RELEASE HPrisko.
  END. /* TRANSACTION */
  
END PROCEDURE.  