
/*------------------------------------------------------------------------
    File        : PkSdlSetLandedCost.p
    Purpose     : 

    Syntax      :

    Description : Påfører landed kost på pakkseddelhode.

    Author(s)   : Tom Nøkleby
    Created     : Fri Jul 20 11:29:43 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF INPUT  PARAM icParam     AS CHAR   NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR   NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR   NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG    NO-UNDO.

DEFINE VARIABLE lPkSdlId    AS DECIMAL   FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEFINE VARIABLE lLandedCost AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE cLogg       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst      AS CHARACTER NO-UNDO.
DEFINE VARIABLE iButNr      AS INTEGER   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN 
  cLogg    = 'PkSdlSetLandedCost' + REPLACE(STRING(TODAY),'/','')
  lPkSdlId = DEC(ENTRY(1,icParam,'|'))
  .

FIND PkSdlHode NO-LOCK WHERE 
  PkSdlHode.PkSdlId = lPkSdlId NO-ERROR.
IF AVAILABLE PkSdlHode THEN
  RUN setLandedCost.
obOk = TRUE.

/* **********************  Internal Procedures  *********************** */

PROCEDURE setLandedCost:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE lKjedeInnkPris AS DECIMAL NO-UNDO.
  DEFINE VARIABLE iSasong        AS INTEGER NO-UNDO.
  DEFINE VARIABLE iAnt           AS INTEGER NO-UNDO.
    
  DEFINE BUFFER bufArtBas FOR ArtBas.
     
  ASSIGN 
    lLandedCost = 0.

  FIND CURRENT PkSdlHode EXCLUSIVE-LOCK.
  FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK,
    FIRST ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr: 
            
    iAnt           = iAnt + PkSdlLinje.AntLevert.          
    lKjedeInnkPris = ArtBas.KjedeInnkPris.    
            
    /* Henter LC fra en artikkel med annen sesongkode. */
    IF lKjedeInnkPris = 0 THEN 
    DO:
      BUFLOOP:
      FOR EACH bufArtBas NO-LOCK WHERE 
        bufArtBas.LevNr = ArtBas.LevNr AND 
        bufArtBas.LevKod = ArtBas.LevKod AND 
        bufArtBas.LevFargKod = ArtBas.LevFargKod AND 
        /*bufArtBas.RegistrertDato >= 01/01/2015 AND*/ 
        bufArtBas.KjedeInnkPris > 0:
        ASSIGN 
          lKjedeInnkPris = bufArtBas.KjedeInnkPris.    
      END. /* BUFLOOP */
    END.         
    /* Beregner LC fra innkjøpspris. */
    IF lKjedeInnkPris = 0 THEN 
    DO:
      FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
      IF AVAILABLE ArtPris THEN
      DO: 
        FIND bufArtBas EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT LOCKED bufArtBas AND AVAILABLE bufArtBas THEN 
        DO:
          bufArtBas.KjedeInnkPris = ROUND((ArtPris.InnkjopsPris[1] * 45) / 100,2).
          IF bufArtBas.KjedeInnkPris = ? THEN 
            bufArtBas.KjedeInnkPris = 0.
          lKjedeInnkPris = bufArtBas.KjedeInnkPris.
          FIND CURRENT bufArtBas NO-LOCK NO-ERROR NO-WAIT.
        END.
      END.
    END.
        
    IF lKjedeInnkPris > 0 THEN    
      ASSIGN 
        lLandedCost = lLandedCost + (PkSdlLinje.Antall * lKjedeInnkPris)
        .
    ELSE
      RUN bibl_loggDbFri.p (cLogg, '    Mangler LC: ;' + 
        STRING(ArtBas.ArtikkelNr) + ';' + 
        STRING(ArtBas.LevKod) + ';' +
        STRING(ArtBas.LEvFargKod) + ';' + 
        STRING(ArtBas.Sasong) 
        ). 
            
    ASSIGN 
      iSaSong = ArtBas.Sasong
      .
  END.
  IF lLandedCost = ? OR lLandedCost < 0 THEN 
    lLandedCost = 0.
  ASSIGN 
    PkSdlHode.MeldingFraLev = 'Ordretype: 0' + CHR(10) + 
                                  'Sesongkode: ' + STRING(iSasong) + CHR(10) + 
                                  'LandedCost: ' + STRING(lLandedCost) + CHR(10) + 
                                  PkSdlHode.MeldingFraLEv
    pksdlHode.ButikkNr      = IF PkSdlHode.ButikkNr = 0 THEN iButNr ELSE PkSdlHode.butikkNr
    PkSdlHode.LandedCost    = lLandedCost
    ocReturn = STRING(PkSdlHode.LandedCost) + '|' + STRING(iAnt).
    .
  FIND CURRENT PkSdlHode NO-LOCK.

END PROCEDURE.

