
/*------------------------------------------------------------------------
    File        : pksdllinje_opprett_pris.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Sat Aug 17 20:13:00 CEST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER icParam     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ihBuffer    AS HANDLE NO-UNDO.
DEFINE INPUT  PARAMETER icSessionId AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER ocReturn    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lPkSdlId AS DECIMAL NO-UNDO.
DEFINE VARIABLE iPkSdlLinjeId AS INTEGER NO-UNDO.
DEFINE VARIABLE iCl AS INTEGER NO-UNDO.
DEFINE VARIABLE bStdPrisOverf AS LOG       NO-UNDO.

DEFINE BUFFER bufPkSdlLinje FOR PkSdlLinje.
DEFINE BUFFER clButiker     FOR Butiker.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

{syspara.i 5 1 1 iCl INT}
{syspara.i 5 26 1 bStdPrisOverf LOGICAL}

ASSIGN 
    lPkSdlId      = DEC(ENTRY(1,icParam,'|'))  
    iPkSdlLinjeId = INT(ENTRY(2,icParam,'|'))  
    .

/*MESSAGE 'pksdllinje_opprett_pris.p' SKIP*/
/*'   lPkSdlId ' lPkSdlId SKIP            */
/*'   iPkSdlLinjeId ' iPkSdlLinjeId       */
/*VIEW-AS ALERT-BOX.                      */

FIND PkSdlLinje NO-LOCK WHERE 
  PkSdlLinje.PkSdlId = lPkSdlId AND 
  PkSdlLinje.PkSdlLinjeId = iPkSdlLinjeId NO-ERROR.
IF AVAILABLE PkSdlLinje THEN 
DO TRANSACTION:
  FIND Butiker NO-LOCK WHERE 
    Butiker.butik = PkSdlLinje.ButikkNr NO-ERROR.
  FIND clButiker NO-LOCK WHERE 
    clButiker.butik = iCl NO-ERROR.
    
  /* Legger opp pris hvis ikke denne finnes fra før. */
  FIND PkSdlPris EXCLUSIVE-LOCK WHERE
    PkSdlPris.PkSdlId    = PkSdlLinje.PkSdlId AND
    PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
  IF NOT AVAILABLE PkSdlPris THEN 
  DO:
    FIND FIRST ArtPris NO-LOCK
      WHERE ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr
      AND ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris NO-LOCK
        WHERE ArtPris.ArtikkelNr = PkSdlLinje.ArtikkelNr
        AND ArtPris.ProfilNr     = clButiker.ProfilNr NO-ERROR.
    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
    IF AVAILABLE ArtPris THEN 
    DO:
      CREATE PkSdlPris.
      ASSIGN
        PkSdlPris.PkSdlId    = PkSdlLinje.PkSdlId
        PkSdlPris.ArtikkelNr = PkSdlLinje.ArtikkelNr.      
      IF AVAILABLE ArtBas THEN   
        BUFFER-COPY ArtBas   
          EXCEPT    ArtikkelNr BrukerId EDato Etid RegistrertDato RegistrertTid RegistrertAv 
          TO        PkSdlPris.
      ASSIGN 
        PkSdlPris.VareKost       = ArtPris.VareKost[1]
        PkSdlPris.Rab1%          = ArtPris.Rab1%[1]
        PkSdlPris.Pris           = ArtPris.Pris[1]
        PkSdlPris.Frakt          = ArtPris.Frakt[1]
        PkSdlPris.Db%            = ArtPris.Db%[1]
        PkSdlPris.InnkjopsPris   = ArtPris.InnkjopsPris[1]
        PkSdlPris.OverstyrPris   = bStdPrisOverf
        /* Ny pris som skal gjelde i butikken */
        PkSdlPris.NyPris         = PkSdlPris.Pris
        PkSdlPris.NyVarekost     = PkSdlPris.VareKost 
        PkSdlPris.NyRab1%        = PkSdlPris.Rab1%
        PkSdlPris.NyInnkjopsPris = PkSdlPris.InnkjopsPris
        PkSdlPris.NyFrakt        = 0
        PkSdlPris.NyDB%          = PkSdlPris.Db%
        PkSdlPris.OverstyrPris   = YES
        .
      RELEASE PkSdlPris.
    END.       
  END.
END. /* TRANSACTION */  