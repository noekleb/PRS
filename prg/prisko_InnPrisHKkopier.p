/* prisko_kopier.p 
*/

DEFINE INPUT PARAMETER pRowIdArtBas  AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER iProfilNr      AS INTEGER NO-UNDO.

DEFINE VARIABLE cTekst        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCL           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iClProfilNr   AS INTEGER   NO-UNDO.
DEFINE VARIABLE iCLOpt AS INTEGER     NO-UNDO.
DEFINE VARIABLE bKopierInnPrisPrisko AS LOG       NO-UNDO.
DEFINE VARIABLE cOptProfilbutik AS CHARACTER   NO-UNDO.
DEFINE BUFFER clButiker FOR Butiker.
DEFINE BUFFER bufArtPris FOR ArtPris.

/* Det skal ikke skapes ELogg poster ved denne type av kopiering */
ON WRITE OF ArtPris OVERRIDE DO: END.

/* Henter profilnr på sentrallageret. */
{syspara.i 5 1 1 iCL INT}
{syspar2.i 5 1 1 cOptProfilbutik}
cOptProfilbutik = TRIM(cOptProfilbutik).

FIND clButiker NO-LOCK WHERE
  clButiker.Butik = iCl NO-ERROR.
IF AVAILABLE clButiker THEN 
  iClProfilNr = clButiker.ProfilNr.
ELSE
  iClProfilNr = 1.

/* Sjekker om Innpris priskøpost skal kopieres til alle andre lokale. */
{syspara.i 2 4 47 cTekst}
IF CAN-DO('1,J,Ja,Y,YES,True',cTekst) THEN 
  bKopierInnPrisPrisko = TRUE.
ELSE
  bKopierInnPrisPrisko = FALSE.

/* Sjekker om det skal kopieres innpris. */
IF bKopierInnPrisPrisko = FALSE THEN 
  RETURN.
  
/* Henter priskøposten som skal kopieres */
FIND ArtBas NO-LOCK WHERE
  ROWID(ArtBas) = pRowIdArtBas NO-ERROR.
IF NOT AVAILABLE ArtBas THEN 
  RETURN.
FIND ArtPris NO-LOCK WHERE 
  ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
  ArtPris.ProfilNr   = iProfilNr NO-ERROR.
IF NOT AVAILABLE ArtPris THEN 
  RETURN.
  
/* Kun køposter på hk profilen skal kunne kopieres. */
/* detta förutsätts vid anropet */
/* IF ArtPris.ProfilNr <> iClProfilNr THEN */
/*   RETURN.                               */
IF cOptProfilbutik <> "" THEN DO:
    /* hitta butiken */
    iCLOpt = 0.
    FOR EACH butiker WHERE butiker.profilnr = iProfilnr NO-LOCK.
        IF butiker.sentrallager = TRUE THEN DO:
            ASSIGN iCLOpt = butiker.butik.
            LEAVE.       
        END.
    END.
END.

KOPIER_INNPRIS:
FOR EACH bufArtPris EXCLUSIVE-LOCK WHERE
  bufArtPris.ArtikkelNr = ArtPris.ArtikkelNr:
  /* Koperer ikke til seg selv */
  IF bufArtPris.ProfilNr = ArtPris.ProfilNr THEN 
    NEXT KOPIER_INNPRIS.
  IF cOptProfilbutik <> "" AND iCLOpt <> 0 THEN DO:
      FIND FIRST butiker WHERE butiker.profilnr = bufArtPris.profilnr.
      IF AVAIL butiker AND butiker.clButikknr <> iCLOpt THEN
          NEXT KOPIER_INNPRIS.
  END.
  /* Endring i Normalpris */  
  IF bufArtPris.VareKost[1] <> ArtPris.VareKost[1] THEN 
  DO:
      /* Kopierer felt */
      ASSIGN
      bufArtPris.ValPris[1]      = ArtPris.ValPris[1]
      bufArtPris.InnkjopsPris[1] = ArtPris.InnkjopsPris[1]
      bufArtPris.Rab1Kr[1]       = ArtPris.Rab1Kr[1]
      bufArtPris.Rab1%[1]        = ArtPris.Rab1%[1]
      bufArtPris.Rab2Kr[1]       = ArtPris.Rab2Kr[1]
      bufArtPris.Rab2%[1]        = ArtPris.Rab2%[1]
      bufArtPris.Rab3Kr[1]       = ArtPris.Rab3Kr[1]
      bufArtPris.Rab3%[1]        = ArtPris.Rab3%[1]
      bufArtPris.Frakt[1]        = ArtPris.Frakt[1]
      bufArtPris.Frakt%[1]       = ArtPris.Frakt%[1]
      bufArtPris.DivKostKr[1]    = ArtPris.DivKostKr[1]
      bufArtPris.DivKost%[1]     = ArtPris.DivKost%[1]
      bufArtPris.Varekost[1]     = ArtPris.VareKost[1]
      bufArtPris.Mva%[1]         = ArtPris.Mva%[1]
      .
      
      /* Regner om MvaKr (Mva% er likog % og DbKr og % */
      ASSIGN
          bufArtPris.MvaKr  = bufArtPris.Pris[1] - (bufArtPris.Pris[1] / (1 + (bufArtPris.Mva%[1] / 100)))      
          bufArtPris.DbKr   = bufArtPris.Pris[1] - bufArtPris.MvaKr[1] - bufArtPris.VareKost[1]
          bufArtPris.DB%[1] = ROUND((bufArtPris.DBKr[1] * 100) / (bufArtPris.VareKost[1] + bufArtPris.DBKr[1]),2)
          .
      
  END.     
END. /* KOPIER_INNPRIS */   
    
