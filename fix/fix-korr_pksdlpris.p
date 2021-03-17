  DEF VAR fMvaKr        AS DEC  NO-UNDO.
  DEF VAR fDbKr         AS DEC  NO-UNDO.
  DEF VAR fAntBest      AS DEC  NO-UNDO.
  DEF VAR fAntLevert    AS DEC  NO-UNDO.
  DEF VAR cTekst        AS CHAR NO-UNDO.

DEF VAR lHK AS LOG NO-UNDO.
{syspara.i 1 1 18 cTekst}
IF CAN-DO("1,yes,true",cTekst) THEN lHK = TRUE.
ELSE lHK = FALSE.

CURRENT-WINDOW:WIDTH = 250.

PUBLISH 'infoDisp' ("Initierer VPI informasjon..").

FOR EACH PkSdlHode WHERE
  PksdlHode.PkSdlId = 146125,
  EACH PkSdlPris OF PkSdlHode:
  
  FIND FIRST ArtPris WHERE
    ArtPris.ArtikkelNr = PkSdlPris.ArtikkelNr AND 
    ArtPris.ProfilNr = 2 NO-LOCK NO-ERROR.
  
    ASSIGN
        PkSdlPris.Pris           = ArtPris.Pris[1]
        PkSdlPris.NyRab1%        = ArtPris.Rab1%[1]
        PkSdlPris.InnkjopsPris   = ArtPris.InnkjopsPris[1]        
        PkSdlPris.Varekost       = ArtPris.VareKost[1]
        fMvaKr                   = PkSdlPris.Pris - (PkSdlPris.Pris / (1 + (ArtPris.Mva%[1] / 100)))
        fDbKr                    = PkSdlPris.Pris - fMvaKr - PkSdlPris.Varekost
        PkSdlPris.DB%            = ROUND((fDbKr * 100) / (PkSdlPris.Varekost + fDbKr),2)
        PkSdlPris.DB%            = IF PkSdlPris.DB% = ? THEN 0 ELSE PkSdlPris.DB%
                
        PkSdlPris.NyInnkjopsPris = ArtPris.InnkjopsPris[1]        
        PkSdlPris.NyVarekost     = ArtPris.VareKost[1]
        PkSdlPris.NyPris         = ArtPris.Pris[1] /* Lokal lpris skal gjelde. */
        PkSdlPris.NyFrakt        = PkSdlPris.Frakt
        fMvaKr                   = PkSdlPris.NyPris - (PkSdlPris.NyPris / (1 + (ArtPris.Mva%[1] / 100)))
        fDbKr                    = PkSdlPris.NyPris - fMvaKr - PkSdlPris.NyVarekost
        PkSdlPris.NyDB%          = ROUND((fDbKr * 100) / (PkSdlPris.NyVarekost + fDbKr),2)
        PkSdlPris.NyDB%          = IF PkSdlPris.NyDB% = ? THEN 0 ELSE PkSdlPris.NyDB%
        .
  
  
  display
  ArtPris.Innkjopspris[1]
  ArtPris.Varekost[1]
  ArtPris.Pris[1]
  /*
      PkSdlHode.PkSdlId
  pksdlhode.EkstId
  */
  PkSdlPris.ArtikkelNr
  PkSdlPris.LevKod
  /*PkSdlPris.Beskr*/
  PkSdlPris.Pris
  PkSdlPris.Rab1%
  PkSdlPris.NyRab1%
  PkSdlPris.Varekost
  PkSdlPris.NyVareKost
  PkSdlPris.Db%
  PkSdlPris.NyDb%
  with width 250.
  
  
END.  

PUBLISH 'infoDisp' ("..").

