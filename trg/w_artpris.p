TRIGGER PROCEDURE FOR WRITE OF SkoTex.ArtPris  OLD BUFFER oldArtPris.

DEFINE BUFFER trgArtBas FOR ArtBas.
DEFINE BUFFER trgStrekkode FOR Strekkode.

DEFINE VARIABLE ctrgTekst AS CHAR NO-UNDO.
DEFINE VARIABLE btrgHk    AS LOG  NO-UNDO.
DEFINE VARIABLE bUndertrykk       AS LOG       NO-UNDO.
DEFINE VARIABLE bNix      AS LOG  NO-UNDO.
DEFINE VARIABLE itrgKommisjonsProfil AS INTEGER NO-UNDO.
DEFINE VARIABLE itrgKommisjonAktiv AS INTEGER NO-UNDO.

/* Skal utlegg til kasse av artikkelendringer gjørs? */
{syspara.i 2 4 43 ctrgTekst}
IF CAN-DO('1,j,ja,y,yes,true',ctrgTekst) 
  THEN bUndertrykk = TRUE.
  ELSE bUndertrykk = FALSE. 

{syspara.i 1 1 18 ctrgTekst}
IF CAN-DO("ja,1,true,yes",ctrgTekst) THEN
    btrgHK = TRUE.
ELSE
    btrgHK = FALSE.

{trg\c_w_trg.i &Fil=SkoTex.ArtPris &Type=W}

{syspara.i 5 40 12 itrgKommisjonsProfil INT}
{syspara.i 5 40  4 itrgKommisjonAktiv INT}

bNix = FALSE.
/* Det skal bare skapes elogg når noen av disse feltene er endret. */
DO:
    IF oldArtPris.VareKost[2]     <> ArtPris.VareKost[2]     THEN bNix = TRUE.
    IF oldArtPris.InnkjopsPris[2] <> ArtPris.InnkjopsPris[2] THEN bNix = TRUE.  
    IF oldArtPris.Db%[2]          <> ArtPris.Db%[2]          THEN bNix = TRUE.
    IF oldArtPris.Pris[2]         <> ArtPris.Pris[2]         THEN bNix = TRUE.

    IF oldArtPris.VareKost[1]     <> ArtPris.VareKost[1]     THEN bNix = TRUE.
    IF oldArtPris.InnkjopsPris[1] <> ArtPris.InnkjopsPris[1] THEN bNix = TRUE.
    IF oldArtPris.Db%[1]          <> ArtPris.Db%[1]          THEN bNix = TRUE. 
    IF oldArtPris.Pris[1]         <> ArtPris.Pris[1]         THEN bNix = TRUE.
    IF oldArtPris.Tilbud          <> ArtPris.Tilbud          THEN bNix = TRUE.
END.

/* Er det bare lagt opp tilbudskalkyle, skal denne kopieres over i normalpris kalkylen   */
/* Ved create av posten er innkjopspris og varekost initiert til 1. Derfor testen under. */
IF (ArtPris.Pris[1] + ArtPris.VareKost[1] + ArtPris.InnkjopsPris[1] + ArtPris.Rab1%[1]) = 2 AND
   (ArtPris.Pris[2] + ArtPris.VareKost[2] + ArtPris.InnkjopsPris[2] + ArtPris.Rab1%[2]) > 2 AND
   ArtPris.Tilbud THEN
   ASSIGN
    ArtPris.VareKost[1]     = ArtPris.VareKost[2]
    ArtPris.MvaKr[1]        = ArtPris.MvaKr[2]
    ArtPris.ValPris[1]      = ArtPris.ValPris[2]
    ArtPris.Rab1Kr[1]       = ArtPris.Rab1Kr[2]
    ArtPris.Rab1%[1]        = ArtPris.Rab1%[2]
    ArtPris.Rab2Kr[1]       = ArtPris.Rab2Kr[2]
    ArtPris.Rab2%[1]        = ArtPris.Rab2%[2]
    ArtPris.Frakt[1]        = ArtPris.Frakt[2]
    ArtPris.Frakt%[1]       = ArtPris.Frakt%[2]
    ArtPris.DivKostKr[1]    = ArtPris.DivKostKr[2]
    ArtPris.DivKost%[1]     = ArtPris.DivKost%[2]
    ArtPris.Rab3Kr[1]       = ArtPris.Rab3Kr[2]
    ArtPris.Rab3%[1]        = ArtPris.Rab3%[2]
    ArtPris.DBKr[1]         = ArtPris.DBKr[2]
    ArtPris.DB%[1]          = ArtPris.DB%[2]
    ArtPris.EuroPris[1]     = ArtPris.EuroPris[2]
    ArtPris.InnkjopsPris[1] = ArtPris.InnkjopsPris[2]
    ArtPris.Mva%[1]         = ArtPris.Mva%[2]
    ArtPris.Pris[1]         = ArtPris.Pris[2]
    ArtPris.MomsKod[1]      = ArtPris.MomsKod[2]
    NO-ERROR. 

/* Artikkel skal ikke logges før prisrecord er opprettet.      */
/* Logging for overføring til VPI register og deretter til HK. */
/*
IF (btrgHK = FALSE AND (OldArtPris.ArtikkelNr = 0 AND
                        ArtPris.ArtikkelNr    > 0 AND
                        ArtPris.ArtikkelNr < 8500000)) THEN
RAPPORT-LOKALE-ARTIKLER:
DO WHILE TRUE:
    /* Det skal ikke sendes noe til HK før artikkelen har fått sin EAN kode. */
    IF CAN-FIND(FIRST Strekkode NO-LOCK WHERE
                Strekkode.ArtikkelNr = ArtPris.ArtikkelNr) THEN 
    STREKKODESJEKK:
    DO:
        FIND ELogg EXCLUSIVE-LOCK WHERE 
             ELogg.TabellNavn     = "ArtBas"  AND
             ELogg.EksterntSystem = "TILKORRPOS" AND
             ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) NO-ERROR NO-WAIT.
        IF LOCKED ELogg THEN 
          NEXT RAPPORT-LOKALE-ARTIKLER.
        IF NOT AVAILABLE ELogg THEN   
        DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "ArtBas"
                   ELogg.EksterntSystem = "TILKORRPOS"   
                   ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
            DO:
              DELETE ELogg.
              NEXT RAPPORT-LOKALE-ARTIKLER.
            END.
        END.
        IF ELogg.EndringsType <> 2 OR ELogg.Behandlet = TRUE THEN 
        DO:
          ASSIGN ELogg.EndringsType = 1
                 ELogg.Behandlet    = FALSE NO-ERROR.
          IF ERROR-STATUS:ERROR THEN 
          NEXT RAPPORT-LOKALE-ARTIKLER.
        END.  

        IF AVAILABLE ELogg THEN 
          RELEASE ELogg.
    END. /* STREKKODESJEKK */
    
    LEAVE RAPPORT-LOKALE-ARTIKLER.
END. /* RAPPORT-LOKALE-ARTIKLER */
*/

/* Utlegg til ferskvarevekt */
IF bNix = TRUE THEN
DO FOR trgArtBas:
    FIND trgArtBas NO-LOCK WHERE 
      trgArtBas.ArtikkelNr = ArtPris.ArtikkelNr NO-ERROR.
    IF AVAILABLE trgArtBas AND 
       CAN-FIND (FIRST SysPara WHERE SysPara.SysHId = 23 AND SysPara.SysGr = 1 AND SysPara.Parameter1 > '0') THEN
    XELOGG: 
    DO FOR ELogg:
      FOR EACH trgStrekkode OF trgArtBas NO-LOCK WHERE 
        LENGTH(trgStrekkode.Kode) = 13:
        FIND ELogg WHERE 
             ELogg.TabellNavn     = "ArtBas" AND
             ELogg.EksterntSystem = "FVEKT"    AND
             ELogg.Verdier        = STRING(trgArtBas.ArtikkelNr) + CHR(1) + STRING(trgStrekkode.Kode) NO-ERROR NO-WAIT.
        IF LOCKED ELogg THEN LEAVE XELOGG.
        IF NOT AVAIL Elogg THEN DO:
            CREATE Elogg.
            ASSIGN ELogg.TabellNavn     = "ArtBas"
                   ELogg.EksterntSystem = "FVEKT"   
                   ELogg.Verdier        = STRING(trgArtBas.ArtikkelNr) + CHR(1) + STRING(trgStrekkode.Kode).
        END.
        ASSIGN ELogg.EndringsType = IF trgArtBas.ArtSlag = 1 THEN 1 ELSE 3 
               ELogg.Behandlet    = FALSE.
        RELEASE ELogg.
      END.
      IF AVAILABLE ELogg THEN RELEASE ELogg.
    END. /* XELOGG */
    
    IF AVAILABLE trgArtBas THEN 
    DO: 
        FIND CURRENT trgArtBas EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE trgArtBas THEN 
          ASSIGN 
            trgArtBAs.EDato = TODAY
            trgArtBas.ETid  = TIME.
        IF AVAILABLE trgArtBas THEN RELEASE trgArtBas.
    END. 
END.

/* Logging mot kasse gjøres pr. profil. */
IF bNix = TRUE AND 
   bUndertrykk = FALSE AND 
   CAN-FIND(ArtBas WHERE ArtBas.ArtikkelNr = Artpris.ArtikkelNr AND
                         ArtBas.iKasse     = TRUE) THEN
ELOGG_ARTPRIS:
DO FOR ELogg, trgArtBas:
  FIND trgArtBas NO-LOCK WHERE 
    trgArtBas.ArtikkelNr = ArtPris.ArtikkelNr NO-ERROR.

  FIND ELogg EXCLUSIVE-LOCK WHERE 
     ELogg.TabellNavn     = "ArtPris" AND
     ELogg.EksterntSystem = "POS"    AND
     ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(ArtPris.ProfilNr) NO-ERROR NO-WAIT.
  
  IF LOCKED ELogg THEN
  DO: 
    RELEASE ELogg.
    LEAVE ELOGG_ARTPRIS.
  END.
  IF NOT AVAIL Elogg THEN DO:
    CREATE Elogg.
    ASSIGN ELogg.TabellNavn     = "ArtPris"
           ELogg.EksterntSystem = "POS"   
           ELogg.Verdier        = STRING(ArtPris.ArtikkelNr) + CHR(1) + string(ArtPris.ProfilNr) 
           ELogg.EndringsType   = 1
           ELogg.Behandlet      = FALSE           
           NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
      DELETE ELogg.
      LEAVE ELOGG_ARTPRIS.
    END.
  END.
  IF AVAILABLE ELogg THEN RELEASE ELogg.

  IF AVAILABLE trgArtBas THEN 
  DO: 
      FIND CURRENT trgArtBas EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE trgArtBas THEN 
        ASSIGN 
          trgArtBAs.EDato = TODAY
          trgArtBas.ETid  = TIME.
      IF AVAILABLE trgArtBas THEN RELEASE trgArtBas.
  END. 

END. /* ELOGG_ARTPRIS */
  
/* Logger for kommisjonsbutikkene. */
IF itrgKommisjonAktiv = 0 THEN 
DO:
  IF ArtPris.ProfilNr = itrgKommisjonsProfil THEN 
    RUN opprettArtPrisELogg.p(ArtPris.ArtikkelNr, ArtPris.ProfilNr, 'PRICAT_KOMMISJON').
END.

