/* Registrer innleveranse fra pakkseddel
   Parameter:  Funksjon,fArtikkelnr
   Funksjon inneholder hvem regel som blir kjørt. F.eks. VPIArtBas, kjører
   opprettVPIArtBas. hBuffer vil da ha buffer fra VPIartbas.
   Opprettet: 25.11.2007  Geir Otto
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fArtikkelNr          AS DEC    NO-UNDO.
DEF VAR iEkstVPILevNr        AS INT    NO-UNDO.
DEF VAR cVareNr              AS CHAR   NO-UNDO.
DEF VAR bNew                 AS LOG    NO-UNDO.

DEF VAR piCl            AS INT  NO-UNDO.
DEF VAR piProfilNr      AS INT  NO-UNDO.
DEF VAR cTekst          AS CHAR NO-UNDO.
DEF VAR bHk             AS LOG  NO-UNDO.
DEF VAR bAuto           AS LOG  NO-UNDO.
DEF VAR cGenEAN         AS CHAR NO-UNDO.
DEF VAR cStrTypeId      AS CHAR NO-UNDO.
DEF VAR cFieldList      AS CHAR NO-UNDO.
DEFINE VARIABLE cJamforEnhet AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOverfBedrIntEAN AS LOG NO-UNDO.
DEFINE VARIABLE bSettLopNr AS LOG NO-UNDO.

DEF BUFFER bVPIArtBas FOR VPIArtBas.
DEF BUFFER clButiker  FOR Butiker.

FUNCTION FixChk RETURNS CHAR (INPUT cKode AS CHAR) FORWARD.
FUNCTION doUpdate RETURNS LOG (INPUT icFieldName AS CHAR) FORWARD.


/*************************************** Lagt inn for å initiere inndata*/
  /* Henter sentrallager. */
  {syspara.i 5   1 1 piCl       INT}

  FIND clButiker NO-LOCK WHERE
      clButiker.Butik = piCl.

  {syspara.i 50 15 16 cTekst}
  IF CAN-DO("1,yes,true,Ja",cTekst) THEN
     bOverfBedrIntEAN = TRUE.
  ELSE
     bOverfBedrIntEAN = FALSE.

  {syspara.i 50 15 44 cTekst}
  IF cTekst = '' OR CAN-DO("1,yes,true,Ja",cTekst) THEN
     bSettLopNr = TRUE.
  ELSE
     bSettLopNr = FALSE.

  {syspara.i 1 1 18 cTekst}
  IF CAN-DO("1,yes,true,Ja",cTekst) THEN
     bHk = TRUE.
  ELSE
     bHk = FALSE.
  
  ASSIGN 
    bAuto      = TRUE
    bNew       = FALSE
    piProfilNr = clButiker.ProfilNr
  .

  {syspara.i 2 4 8 cGenEan} 
  {syspara.i 50 15 1 cStrTypeId}

/************************************************************/

  ASSIGN 
    iEkstVPILevNr = INT(ENTRY(1,icParam,';'))
    cFieldList    = ENTRY(2,icParam,';')
    fArtikkelNr   = DEC(ENTRY(3,icParam,';'))
  .
  FIND bVPIArtBas WHERE bVPIArtBas.EkstVPILevNr = iEkstVPILevNr
                    AND bVPIArtBas.VareNr       = STRING(fArtikkelNr)
                  NO-LOCK NO-ERROR.

  IF AVAIL bVPIArtBas THEN
    RUN OpprettVPIArtBas.
  
  obOk = ocReturn = ''.



/* **********************  Internal Procedures  *********************** */


PROCEDURE OpprettKarakteristikk:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

  KARAKTERISTIKK:
  FOR EACH VPIArtBasKarakteristikk NO-LOCK 
      WHERE VPIArtBasKarakteristikk.EkstVPILevNr = bVPIArtBas.EkstVPILevNr 
        AND VPIArtBasKarakteristikk.VareNr       = bVPIArtBas.varenr TRANSACTION:


      FIND ArtBasKarakteristikk EXCLUSIVE-LOCK WHERE
          ArtBasKarakteristikk.ArtikkelNr       = DEC(VPIArtBasKarakteristikk.VareNr) AND
          ArtBasKarakteristikk.KarakteristikkId = VPIArtBasKarakteristikk.KarakteristikkId NO-ERROR.

      IF NOT AVAILABLE ArtBasKarakteristikk THEN
      DO:
          CREATE ArtBasKarakteristikk.
          ASSIGN
              ArtBasKarakteristikk.ArtikkelNr       = DEC(VPIArtBasKarakteristikk.VareNr)
              ArtBasKarakteristikk.KarakteristikkId = VPIArtBasKarakteristikk.KarakteristikkId NO-ERROR.
              .
      END.
      RELEASE ArtBasKarakteristikk.
  END. /* KARAKTERISTIKK */


END PROCEDURE.

PROCEDURE OpprettVPIArtBas:

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       

------------------------------------------------------------------------------*/

  DEF VAR pcArtListe      AS CHAR NO-UNDO.
  DEF VAR piLoop          AS INT  NO-UNDO.
  DEF VAR pcAvbrudd       AS CHAR NO-UNDO.
  DEF VAR pbArtikkelNr    AS DEC  NO-UNDO.
  DEF VAR piLoop2         AS INT  NO-UNDO.
  DEF VAR piLopNr         AS INT  NO-UNDO.
  DEF VAR plModellFarge   AS DEC  NO-UNDO.
  DEF VAR pl2ModellFarge  AS DEC  NO-UNDO.
  
  DEFINE BUFFER b2VPIArtBas  FOR VPIArtBas.
  DEFINE BUFFER bVPIArtPris  FOR VPIArtPris.
  DEFINE BUFFER clVPIArtPris FOR VPIArtPris.
  DEFINE BUFFER clArtPris    FOR ArtPris.
  DEFINE BUFFER bVPIErstattningsvare FOR VPIErstattningsvare.
  DEFINE BUFFER pkVPIArtBas  FOR VPIArtBAs.

  BYGG_ARTIKKELLISTE:
  DO:
        
      /* Håndterer pakkevare for komplette VPI sett. */
      IF AVAILABLE bVPIArtBas THEN
      PAKKE:
      DO:
          FOR EACH VPIPakkeLinje OF bVPIArtBas NO-LOCK:
              IF NOT CAN-DO(pcArtListe,STRING(VPIPakkeLinje.PkArtikkelNr)) THEN
                  pcArtListe = pcArtListe + 
                               (IF pcArtListe = ""
                                  THEN ""
                                  ELSE ",") + 
                               STRING(VPIPAkkeLinje.PkArtikkelNr).
              /* Er pakkemedlemmet med i en modell, skal hele modellen ut. */
              FIND b2VPIArtBas NO-LOCK WHERE
                   b2VPIArtBas.EkstVPILevNr = VPIPakkeLinje.EkstVPILevNr AND
                   b2VPIArtBas.VareNr       = string(VPIPakkeLinje.PkArtikkelNr) NO-ERROR.
              
              IF AVAILABLE b2VPIArtBas AND b2VPIArtBas.ModellFarge <> 0 THEN
              DO:
                  FOR EACH pkVPIArtBas NO-LOCK WHERE
                      pkVPIArtBas.EkstVPILevNr = b2VPIArtBas.EkstVPILevNr AND
                      pkVPIArtBas.ModellFarge  = b2VPIArtBas.ModellFarge:
                    IF NOT CAN-DO(pcArtListe,STRING(bVPIArtBas.VareNr)) THEN
                        pcArtListe = pcArtListe + 
                                     (IF pcArtListe = ""
                                        THEN ""
                                        ELSE ",") + 
                                     STRING(pkVPIArtBas.VareNr).
                  END.
              END.
          END.
      END. /* PAKKE */
      /* Håndterer modell */
      IF AVAILABLE bVPIArtBAs AND bVPIArtBas.ModellFarge <> 0 THEN
      MODELL:
      DO:
          ASSIGN
              plModellFarge = bVPIArtBas.ModellFarge
              .
          FOR EACH b2VPIArtBas NO-LOCK WHERE
              b2VPIArtBas.EkstVPILevNr = bVPIArtBas.EkstVPILevNr AND
              b2VPIArtBas.ModellFarge  = bVPIArtBas.ModellFarge:
            IF NOT CAN-DO(pcArtListe,STRING(b2VPIArtBas.VareNr)) THEN
            DO:
                pcArtListe = pcArtListe + 
                             (IF pcArtListe = ""
                                THEN ""
                                ELSE ",") + 
                             STRING(b2VPIArtBas.VareNr).
            END.
          END.
      END. /* MODELL */
      ELSE plModellFarge = 0.

      /* Håndterer erstattningsvarer. */
      FIND FIRST VPIErstattningsvare NO-LOCK WHERE
                 VPIErstattningsvare.EkstVPILEvNr = bVPIArtBas.EkstVPILevNr AND
                 VPIErstattningsvare.VareNr       = bVPIArtBas.VareNr NO-ERROR.
      IF AVAILABLE VPIErstattningsvare THEN
      DO:
          FOR EACH bVPIErstattningsvare NO-LOCK WHERE
              bVPIErstattningsvare.EkstVPILevNr = bVPIArtBas.EkstVPILevNr AND
              bVPIErstattningsvare.ErstattId    = VPIErstattningsvare.ErstattId:
              IF NOT CAN-DO(pcArtListe,STRING(bVPIErstattningsvare.VareNr)) THEN
              DO:
                  pcArtListe = pcArtListe + 
                               (IF pcArtListe = ""
                                  THEN ""
                                  ELSE ",") + 
                               STRING(bVPIErstattningsvare.VareNr).
              END.
          END.
      END.
  END. /* BYGG_ARTIKKELLISTE */
  
  /* Legger inn valgt artikkel */
  OPPRETTELSE:
  DO TRANSACTION:
    /* Legger ut Modell, pakkevare og sammenligningsvarer. */
    FIND CURRENT bVPIArtBas EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bVPIArtBas THEN
    DO:
      ASSIGN
        obOk     = FALSE
        ocReturn = 'AVBRYT'
      .
      RETURN.
    END.
    /* Slipper artbas hvis den er tilgjengelig fra før. */
    IF AVAILABLE ArtBas THEN
        RELEASE ArtBas.

    ASSIGN
      pbArtikkelNr          = dec(bVPIArtBas.VareNr)
      bVPIArtBas.ArtikkelNr = dec(bVPIArtBas.VareNr)
      .
    
    FIND_SJEKK:
    DO:
      FIND ArtBas EXCLUSIVE-LOCK WHERE
          ArtBas.ArtikkelNr = dec(bVPIArtBas.VareNr) NO-WAIT NO-ERROR.
      IF LOCKED ArtBas  THEN DO:
        MESSAGE "Artikkel post låst i artikkelregister (ArtBas). Gjelder artikkel: " + bVPIArtBas.VareNr + "." SKIP
                "Vpi leverandør er: " + STRING(bVPIArtBas.EkstVPILevNr) + "." SKIP 
                "Artbas mangler: " AVAILABLE ArtBas SKIP
                "ArtBas låst:" LOCKED ArtBas SKIP(1)
                "Trykk OK for å fortsette (Det er ikke mulig å avbryte)." SKIP 
                "Kommer meldingen flere ganger, vent en stund før du trykker OK igjen."
        VIEW-AS ALERT-BOX.
      END.
    END. /* FIND_SJEKK */
     
    ARTBASOPPDATERING:
    DO:
      /* Oppretter ny artikkel */
      IF NOT AVAILABLE ArtBas THEN
      OPPRETTARTBAS:
      DO:
          bNew = TRUE.
          CREATE ArtBas.
          ASSIGN
              ArtBas.ArtikkelNr  = DECIMAL(bVPIArtBas.VareNr)
              ArtBas.LopNr       = ?.
          BUFFER-COPY bVPIArtBas EXCEPT ArtikkelNr LopNr
              TO ArtBas
              ASSIGN
              ArtBas.ArtikkelNr  = DECIMAL(bVPIArtBas.VareNr)
              ArtBas.LopNr       = ?
              /* Disse må være med i buffer-copy da de har ulik ekstent. */
              ArtBas.ForhRab%        = bVPIArtBas.forhRab%[1]
              ArtBas.SupRab%         = bVPIArtBas.suppRab%[1]
              ArtBas.KatalogPris     = bVPIArtBas.KatalogPris[1]
              .
          
          /* Tildeler løpenummer til ny artikkel */
          IF bSettLopNr THEN 
          DO:
            IF bVPIArtBas.LopNr <> ? AND bVPIArtBas.LopNr > 0 AND 
               NOT CAN-FIND(ArtBas WHERE
                            ArtBas.Vg    = bVPIArtBas.Vg AND 
                            ArtBas.LopNr = bVPIArtBAs.LopNr) 
               THEN ASSIGN 
                   ArtBas.LopNr = bVPIArtBas.LopNr.
               ELSE 
                   RUN settlopnr.p (INPUT bVPIArtBas.Vg, INPUT 'N', OUTPUT ArtBas.LopNr).
          END.
          
          /* Overstyrer tildelte verdier */
          ASSIGN
              ArtBas.StrTypeId     = (IF bVPIArtBas.Pakke 
                                      THEN 2                                                                         
                                      ELSE bVPIArtBas.StrTypeId)
              ArtBas.Lager         = IF bVPIArtBas.Pakke 
                                      THEN FALSE
                                      ELSE bVPIArtBas.Lager
              ArtBas.Storrelser    = TRUE
              ArtBas.LevNr         = bVPIArtBas.LevNr      
              ArtBas.Aktivert      = TRUE
              ArtBas.Beskr         = bVPIArtBas.Beskr
              ArtBas.BongTekst     = (IF bVPIArtBas.BongTekst <> ""
                                        THEN bVPIArtBas.BongTekst  
                                        ELSE SUBSTRING(bVPIArtBas.Beskr,1,30))
              ArtBas.VmId          = bVPIArtBas.VmId      
              ArtBas.Notat         = bVPIArtBas.Notat  
              ArtBas.Farg          = bVPIArtBas.Farg
              ArtBas.ValKod        = bVPIArtBas.ValKod              

              /*ArtBas.AnbefaltPris    = bVPIArtBas.AnbefaltPris*/
              /*ArtBas.KjedeInnkPris   = bVPIArtBas.kjedeInnkPris*/
              /*ArtBas.KjedeRab%       = bVPIArtBas.KjedeRab%*/
              ArtBas.KjedeValutaPris = bVPIArtBas.KjedeValutaPris
              ArtBas.KjedeProdusent  = bVPIArtBas.KjedeProdusent
              ArtBas.LevKod          = bVPIArtBas.LevKod  
              ArtBas.SalgsEnhet      = 'Stk' 
              ArtBas.Etikettekst1    = bVPIArtBas.Etikettekst1
              ArtBas.Etikett         = bVPIArtBas.Etikett        
            .
      END. /* OPPRETTARTBAS */

      /* Informasjon som alltid skal over. */
      ASSIGN
        ArtBas.VPIDato          = bVPIArtBas.VPIDato
        /*ArtBas.AnbefaltPris    = bVPIArtBas.AnbefaltPris*/
        /*ArtBas.KjedeInnkPris   = bVPIArtBas.kjedeInnkPris*/
        /*ArtBas.KjedeRab%       = bVPIArtBas.KjedeRab%*/
        ArtBas.KatalogPris      = bVPIArtBas.KatalogPris[1]
        ArtBas.ValKod           = bVPIArtBas.ValKod
        ArtBas.StrKode1         = bVPIArtBas.StrKode1
        ArtBas.StrKode2         = bVPIArtBas.StrKode2
        ArtBas.LevVaretekst     = bVPIArtBas.LevVareTekst
        ArtBas.ManRabIKas       = TRUE
        ArtBas.ModellFarge      = bVPIArtBas.ModellFarge
        ArtBas.HovedmodellFarge = bVPIArtBas.HovedmodellFarge
        ArtBas.Sortimentkoder   = bVPIArtBas.Sortimentkoder
        ArtBas.Lagerkoder       = bVPIArtBas.Lagerkoder        
        ArtBas.Kampanjeuker     = bVPIArtBas.Kampanjeuker                     
        ArtBas.Kampanjestotte   = bVPIArtBas.Kampanjestotte
        ArtBas.StrTypeId        = bVPIArtBas.StrTypeId
        ArtBas.SalgsStopp       = bVPIArtBas.SalgsStopp
        ArtBas.AlfaKode2        = bVPIArtBas.AlfaKode2   
        /* Felter som bare oppdateres hvis det kommer nye data. */
        ArtBas.KjedeValutaPris  = IF bVPIArtBas.KjedeValutaPris <> '' THEN bVPIArtBas.KjedeValutaPris ELSE ArtBas.KjedeValutaPris
        ArtBas.KjedeProdusent   = IF bVPIArtBas.KjedeProdusent <> '' THEN bVPIArtBas.KjedeProdusent ELSE ArtBas.KjedeProdusent
        ArtBas.PostBredde       = (IF bVPIArtBas.PostBredde > 0 THEN bVPIArtBas.PostBredde ELSE ArtBas.PostBredde)        
        ArtBas.PostHoyde        = (IF bVPIArtBas.PostHoyde > 0 THEN bVPIArtBas.PostHoyde ELSE ArtBas.PostHoyde)        
        ArtBas.PostLengde       = (IF bVPIArtBas.PostLengde > 0 THEN bVPIArtBas.PostLengde ELSE ArtBas.PostLengde)        
        ArtBas.PostVekt         = (IF bVPIArtBas.PostVekt > 0 THEN bVPIArtBas.PostVekt ELSE ArtBas.PostVekt)        
        ArtBas.Alder            = (IF bVPIArtBas.Alder > 0 THEN bVPIArtBas.Alder ELSE ArtBas.Alder)        
        ArtBas.LinkVareAnt      = (IF bVPIArtBas.LinkVareAnt > 0 THEN bVPIArtBas.LinkVareAnt ELSE ArtBas.LinkVareAnt)        
        ArtBas.GarantiKl        = (IF bVPIArtBas.GarantiKl > 0 THEN bVPIArtBas.GarantiKl ELSE ArtBas.GarantiKl)        
        .
      
      /* Oppdaterer annen info på eksisterende artikler. */
      IF doUpdate('forhrab%')     THEN ArtBas.ForhRab%       = bVPIArtBas.forhRab%[1].
      IF doUpdate('suprab%')      THEN ArtBas.SupRab%        = bVPIArtBas.suppRab%[1].
      IF doUpdate('VPIDato')      THEN ArtBas.VPIDato        = bVPIArtBas.VPIDato.
      IF doUpdate('LevKod')       THEN ArtBas.LevKod         = bVPIArtBas.LevKod.   
      IF doUpdate('AnbefaltPris') THEN ArtBas.Anbefaltpris   = bVPIArtBas.AnbefaltPris.
      IF doUpdate('AntIPkn')      THEN ArtBas.AntIPakn       = bVPIArtBas.AntIPkn.
      IF doUpdate('LevDato1')     THEN ArtBas.LevDato1       = bVPIArtBas.LevDato1.
      IF doUpdate('LevDato2')     THEN ArtBas.LevDato2       = bVPIArtBas.LevDato2.
      IF doUpdate('LevDato3')     THEN ArtBas.LevDato3       = bVPIArtBas.LevDato3.
      IF doUpdate('LevDato4')     THEN ArtBas.LevDato4       = bVPIArtBas.LevDato4.
      IF doUpdate('Beskr')        THEN ArtBas.Beskr          = bVPIArtBas.Beskr.
      IF doUpdate('LevFargKod')   THEN ArtBas.LevFargKod     = bVPIArtBas.LevFargKod.
      IF doUpdate('RAvdNr')       THEN ArtBas.RAvdNr         = bVPIArtBas.RAvdNr.
      IF doUpdate('VPIBildekode') THEN ArtBas.VPIBildekode   = IF bVPIArtBas.VPIBildekode <> ''
                                                                 THEN bVPIArtBas.VPIBildekode
                                                                 ELSE ArtBas.VPIBildekode.
      IF doUpdate('LevNr')        THEN ArtBas.LevNr          = bVPIArtBas.LevNr.      
      IF doUpdate('ProdNr')       THEN ArtBas.ProdNr         = bVPIArtBas.ProdNr.      
      IF doUpdate('SaSong')       THEN ArtBas.SaSong         = bVPIArtBas.SaSong.      
      IF doUpdate('JamforEnhet')  THEN ArtBas.JamforEnhet    = bVPIArtBas.JamforEnhet.      
      IF doUpdate('Mengde')       THEN ArtBas.Mengde         = bVPIArtBas.Mengde.      
      IF doUpdate('LinkVareNr')   THEN ArtBas.LinkVareNr     = bVPIArtBas.LinkVareNr.      
      IF doUpdate('Gjennomfaktureres') THEN ArtBas.Gjennomfaktureres = bVPIArtBas.Gjennomfaktureres.
      IF doUpdate('Grunnsortiment') THEN ArtBas.Grunnsortiment = bVPIArtBas.Grunnsortiment.      
      IF doUpdate('LokPris')      THEN ArtBas.LokPris        = bVPIArtBas.LokPris.      
      IF doUpdate('Kjedevare')    THEN ArtBas.Kjedevare      = bVPIArtBas.Kjedevare.      
      IF doUpdate('VmId')         THEN ArtBas.VmId           = (IF bVPIArtBas.VmId <> 0 THEN bVPIArtBas.VmId ELSE ArtBas.VmId).      
      IF doUpdate('SalgsEnhet')   THEN ArtBas.SalgsEnhet     = bVPIArtBas.SalgsEnhet.      
      IF doUpdate('Etikettekst1') THEN ArtBas.Etikettekst1   = bVPIArtBas.Etikettekst1.      
      IF doUpdate('BongTekst')    THEN ArtBas.BongTekst      = bVPIArtBas.BongTekst.      
      IF doUpdate('AnonseArtikkel') THEN ArtBas.AnonseArtikkel = bVPIArtBas.AnonseArtikkel.      
      IF doUpdate('Vg')           THEN 
        DO:
          IF bVPIArtBas.Vg <> ArtBas.Vg THEN 
            DO:
                ASSIGN
                ArtBas.LopNr = ?
                ArtBas.Vg    = bVPIArtBas.Vg
                ArtBas.Hg    = bVPIArtBas.Hg.
                RUN settlopnr.p (INPUT ArtBas.Vg, INPUT 'N',OUTPUT ArtBas.LopNr).
            END.
        END.
      IF doUpdate('KjedeRab%')    THEN ASSIGN
                                        ArtBas.KjedeRab%     = bVPIArtBas.KjedeRab%
                                        ArtBas.KjedeInnkPris = bVPIArtBas.KjedeInnkPris
                                        .      
      IF doUpdate('KjedeSupRab%') THEN ASSIGN
                                        ArtBas.KjedeSupRab%     = bVPIArtBas.KjedeSupRab%
                                        ArtBas.KjedeSupInnkPris = bVPIArtBas.KjedeSupInnkPris
                                        .  
      IF doUpdate('EkstStrTypeNavn') THEN ArtBas.EkstStrTypeNavn   = bVPIArtBas.EkstStrTypeNavn.                                                  

      /* Tildeler løpenummer fra VPIArtBas hvis dette er satt. */
      IF ArtBas.LopNr = ? AND bVPIArtBas.LopNr <> ? AND bVPIArtBAs.LopNr > 0 THEN
      DO:
          IF NOT CAN-FIND(ArtBas WHERE
                          ArtBas.Vg    = bVPIArtBas.Vg AND 
                          ArtBas.LopNr = bVPIArtBAs.LopNr) 
             THEN ASSIGN 
                 ArtBas.LopNr = bVPIArtBas.LopNr.
      END.

      /* Tømmer bufferet hvis der skulle være noe. */
      IF AVAILABLE ArtPris THEN RELEASE Artpris.

      /* Henter kalkylen fra VPI..                                      */
      /* Er det butikkens VPI lomme, vil profilnr <> fra cl's profilnr. */
      FIND FIRST VPIArtPris NO-LOCK WHERE
        VPIArtPris.EkstVpiLevNr = bVPIArtBas.EkstVPILevNr AND
        VPIArtPris.VareNr       = bVPIArtBas.VareNr NO-ERROR.
      
      /* Henter kalkylen for profilen*/
      IF VPIArtPris.ProfilNr <> clButiker.ProfilNr THEN 
      DO:
        FIND ArtPris EXCLUSIVE-LOCK WHERE
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
            ArtPris.ProfilNr   = VPIArtPris.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
        DO:
          CREATE ArtPris.
          BUFFER-COPY VPIArtPris 
              TO ArtPris.
        END.
      END.
      /* Henter kalkylen for CL*/
      ELSE DO: 
        FIND ArtPris EXCLUSIVE-LOCK WHERE
             ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
             ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
        IF NOT AVAILABLE ArtPris THEN 
        DO:
          CREATE ArtPris.
          BUFFER-COPY VPIArtPris 
              EXCEPT ProfilNr
              TO ArtPris
              ASSIGN
                 ArtPris.ProfilNr = clButiker.ProfilNr.
        END.
      END.

      IF AVAILABLE VPIArtPris AND AVAILABLE ArtPris THEN
      DO:
          IF doUpdate('KInnkjopspris') THEN ArtPris.Valpris[1]      = VPIArtPris.ValPris[1].
          IF doUpdate('KInnkjopspris') THEN ArtPris.Innkjopspris[1] = VPIArtPris.Innkjopspris[1].
          IF doUpdate('Krab1%')        THEN ArtPris.Rab1%[1]        = VPIArtPris.Rab1%[1].
          IF doUpdate('KRab2%')        THEN ArtPris.Rab2%[1]        = VPIArtPris.Rab2%[1].
          IF doUpdate('KRab3%')        THEN ArtPris.Rab3%[1]        = VPIArtPris.Rab3%[1].
          IF doUpdate('KFrakt%')       THEN ArtPris.Frakt%[1]       = VPIArtPris.Frakt%[1].
          IF doUpdate('KDivKost%')     THEN ArtPris.DivKost%[1]     = VPIArtPris.DivKost%[1].
          IF doUpdate('KPris')         THEN ArtPris.Pris[1]         = VPIArtPris.Pris[1].
          IF doUpdate('KPris')         THEN ArtPris.EuroPris[1]     = VPIArtPris.EuroPris[1].

          ASSIGN
          ArtPris.Rab1Kr[1]    = 0       
          ArtPris.Rab2Kr[1]    = 0       
          ArtPris.Rab3Kr[1]    = 0       
          ArtPris.Frakt[1]     = 0       
          ArtPris.DivKostKr[1] = 0       
          ArtPris.VareKost[1]  = 0     
          ArtPris.DbKr[1]      = 0         
          ArtPris.Db%[1]       = 0          
          ArtPris.MvaKr[1]     = 0       
          ArtPris.Mva%[1]      = VPIArtPris.Mva%[1]         
          .
          /* Utregning av ny kalkyle */
          ASSIGN
              ArtPris.Rab1Kr[1]    = (ArtPris.Innkjopspris[1] * ArtPris.Rab1%[1]) / 100 
              ArtPris.Rab2Kr[1]    = ((ArtPris.Innkjopspris[1] - ArtPris.Rab1Kr[1]) * ArtPris.Rab2%[1]) / 100 
              ArtPris.Rab3Kr[1]    = ((ArtPris.Innkjopspris[1] - ArtPris.Rab1Kr[1] - ArtPris.Rab2Kr[1]) * ArtPris.Rab3%[1]) / 100 
              ArtPris.Frakt[1]     = ((ArtPris.Innkjopspris[1] - ArtPris.Rab1Kr[1] - ArtPris.Rab2Kr[1] - ArtPris.Rab3Kr[1]) * ArtPris.Frakt%[1]) / 100  
              ArtPris.DivKostKr[1] = ((ArtPris.Innkjopspris[1] - ArtPris.Rab1Kr[1] - ArtPris.Rab2Kr[1] - ArtPris.Rab3Kr[1] + ArtPris.Frakt[1]) * ArtPris.DivKost%[1]) / 100   
              ArtPris.Varekost[1]  = (ArtPris.Innkjopspris[1] - ArtPris.Rab1Kr[1] - ArtPris.Rab2Kr[1] - ArtPris.Rab3Kr[1] + ArtPris.Frakt[1] + ArtPris.DivKostKr[1])
              ArtPris.MvaKr[1]     = ArtPris.Pris[1] * (ArtPris.Mva%[1] / (100 + ArtPris.Mva%[1]))
              ArtPris.DbKr[1]      = ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.Varekost[1]
              ArtPris.Db%[1]       = ROUND((ArtPris.DbKr[1] * 100)/ (ArtPris.Pris[1] - ArtPris.MvaKr[1]),2)
              ArtPris.Db%[1]       = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
              .
     
          /* Sjekker om lokal pris er lik cl's pris. Er den det, slettes den lokale prisen. */
          IF ArtPris.ProfilNr <> clButiker.ProfilNr THEN 
          DO:
            FIND clArtPris NO-LOCK WHERE
              clArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND 
              clArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
            IF AVAILABLE clArtPris THEN 
            DO:
              /* TN 1/4-20 Dette skal ikke gjøres. */
/*              IF clArtPris.InnkjopsPris[1] = ArtPris.InnkjopsPris[1] AND*/
/*                 clArtPris.VareKost[1]     = ArtPris.VareKost[1] AND    */
/*                 clArtPris.Pris[1]         = ArtPris.Pris[1]            */
/*                 THEN DELETE ArtPris.                                   */
/*              IF NOT AVAILABLE ArtPris THEN                             */
/*                FIND ArtPris NO-LOCK WHERE                              */
/*                  ArtPris.ArtikkelNr = clArtPris.ArtikkelNr AND         */
/*                  ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.     */
            END.
            /* Er det ny artikkel, må også hk profil legges. oppselv om denne er lik butikkens profil. */
            ELSE DO:
              CREATE clArtPris.
              BUFFER-COPY ArtPris 
                          EXCEPT ProfilNr
                          TO clArtPris
                          ASSIGN
                            clArtPris.ProfilNr = clButiker.ProfilNr.
            END.
          END.
      END.
     
      /* Opprettelse av Strekkoder.                                            */
      /* Må gjøres her under ny, da vi må kunne teste på om StrType må endres. */
      RUN OpprettStrekkoder.
      IF CAN-DO("1,j,y,ja,yes,true",cGenEan) THEN 
        RUN genStrekKode.p (ArtBas.ArtikkelNr,1,""). 
      
      /* Er det sko og 2av5Interleaved, så skal interleavkodene byttes ut. */
      RUN genInterleaf.p (ArtBas.Artikkelnr) NO-ERROR.

      /* Oppretter kobling til karakteristikk */
      RUN OpprettKarakteristikk.        
    END. /* ARTBASOPPDATERING */

    /* Håndtering av varemerke og valutakode */
    REGISTERFIX:
    DO:
        IF NOT CAN-FIND(VareMerke WHERE
                        VareMerke.VmId = ArtBas.VmId) THEN
        DO:
            FIND FIRST VareMerke NO-LOCK NO-ERROR.
            IF AVAILABLE VareMerke THEN
                ArtBas.VmId = VareMerke.VmId.
        END.
        FIND LevBas NO-LOCK WHERE LevBas.LevNr = ArtBas.LevNr NO-ERROR.
        IF AVAILABLE LevBas THEN
            ArtBas.ValKod = LevBas.ValKod.
    END. /* REGISTERFIX */

    /* Oppretter lagerposter */
    FOR EACH Butiker NO-LOCK WHERE
        Butiker.Butik > 0:
        IF NOT CAN-FIND(Lager WHERE
                        Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                        Lager.Butik      = Butiker.Butik) THEN
        DO:
            CREATE Lager.
            ASSIGN
                Lager.ArtikkelNr = ArtBas.ArtikkelNr
                Lager.Butik      = Butiker.Butik
                .
        END.
    END.

    /* Bilderegister og bildedata */
    RUN OpprettBilder.
    /* Erstattningsvarer */
    RUN OpprettErstattningsvarer.
    /* Pakkevare */
    IF bVPIArtBas.Pakke THEN
      RUN OpprettPakkelinjer.

    IF AVAILABLE ArtBas THEN FIND CURRENT ArtBas NO-LOCK.
    IF AVAILABLE ArtPris THEN FIND CURRENT ArtPris NO-LOCK.
    IF AVAILABLE Lager THEN RELEASE Lager.
    IF AVAILABLE VPIArtBAs THEN FIND CURRENT VPIArtBas NO-LOCK.
    IF AVAILABLE VPIArtPris THEN FIND CURRENT VPIArtPris NO-LOCK.
  END. /* OPPRETTELSE TRANSACTION */
END PROCEDURE.

PROCEDURE OpprettStrekkoder:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    DEF VAR piLoop  AS INT  NO-UNDO.
    DEF VAR pcEAN   AS CHAR NO-UNDO.
    DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.

    /* Oppretter strekkoder og setter ArtikkelNr  */
    /* Her kommer alle strekkoder med..           */
    /* Det kommer inn bare ett og ett artikkelnr. */
    STREKKODER:
    DO:
        /* Legger opp strekkoder. */
        STREKKODER-2:
        FOR EACH VPIStrekkode OF bVPIArtBas NO-LOCK
            WHERE VPISTrekkode.KodeType < 9 TRANSACTION:
            
            /* Bedriftsinterne EAN skal ikke kunne hentes inn fra VPI registeret. */
            /*
            IF bOverfBedrIntEAN = FALSE THEN 
            DO:
              IF LENGTH(VPIStrekkode.Kode) = 13 AND
                 SUBSTRING(VPIStrekkode.Kode,1,2) = "02" THEN
                NEXT STREKKODER-2.
            END.
            */
            /* Blanke koder skal ikke legges opp */
            IF VPIStrekkode.Kode = "" THEN
                NEXT STREKKODER-2.
                
            /* Kommer det ugyldige EAN kdoer, skal de ikke pakkes ut. */
            ASSIGN lDec = DECIMAL(VPIStrekkode.Kode) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN 
                NEXT STREKKODER-2.

            /* Ligger strekkode på en annen artikkel skal den flyttes. */
            /* Har strekkoden en annen strKode, skal den endres.       */
            PA-FEIL-ARTIKKEL:
            DO:
                FIND Strekkode WHERE
                    Strekkode.Kode = trim(VPIStrekkode.Kode) EXCLUSIVE-LOCK NO-ERROR.
                /* Ligger strekkoden på feil artikkel skal den flyttes. */
                IF AVAILABLE Strekkode AND Strekkode.ArtikkelNr <> DECIMAL(bVPIArtBas.VareNr) THEN
                    ASSIGN
                      Strekkode.ArtikkelNr        = DECIMAL(bVPIArtBas.VareNr)
                      Strekkode.Bestillingsnummer = ''.
                /* Har strekkoden blitt flyttet til annen størrelse, skal det endres. */
                IF AVAILABLE Strekkode AND
                    Strekkode.StrKode <> VPISTrekkode.StrKode THEN
                    Strekkode.StrKode = VPISTrekkode.StrKode.
                IF AVAILABLE Strekkode THEN RELEASE Strekkode.
            END.
            /* Finnes ikke strekkoden, skal den opprettes */ 
            IF NOT CAN-FIND(Strekkode WHERE
                            Strekkode.Kode = trim(VPIStrekkode.Kode)) THEN
            DO:
                CREATE Strekkode.
                BUFFER-COPY VPIStrekkode TO Strekkode
                    ASSIGN
                    Strekkode.Kode       = TRIM(VPIStrekkode.Kode)
                    Strekkode.ArtikkelNr = DECIMAL(bVPIArtBas.VareNr)
                    /* Strekkode.VareId     = DECIMAL(bVPIArtBas.VareNr) Denne sprekker på store artikkelnr. */
                    Strekkode.HovedNr    = FALSE
                    /*Strekkode.Bestillingsnummer = VPIStrekkode.EkstStorl*/
                    .
                /* Håndtering av ukjente størrelseskoder. */
                IF  Strekkode.StrKode = 0 OR
                    NOT CAN-FIND(StrKonv WHERE
                                StrKonv.StrKode = Strekkode.StrKode) THEN
                    Strekkode.StrKode = 1.
            END.
            /* Bestillingsnr skal oppdateres. */
            ELSE DO:
                FIND Strekkode EXCLUSIVE-LOCK WHERE
                    Strekkode.Kode = trim(VPIStrekkode.Kode) NO-ERROR.
                IF AVAILABLE Strekkode AND VPIStrekkode.Bestillingsnummer <> '' THEN ASSIGN Strekkode.BestillingsNummer = VPIStrekkode.Bestillingsnummer.
                IF AVAILABLE Strekkode AND VPIStrekkode.ERPNr             <> '' THEN ASSIGN Strekkode.ERPNr             = VPIStrekkode.ERPNr.
            END.
        END. /* STREKKODER-2 */

        /* Legger opp strekkoder. */
        EAN-128:
        FOR EACH VPIStrekkode OF bVPIArtBas NO-LOCK
            WHERE VPISTrekkode.KodeType = 9 TRANSACTION:

            ASSIGN
                pcEAN = "02" + 
                        string(VPIStrekkode.VareNr,"9999999") + 
                        STRING(VPIStrekkode.StrKode,"999")
                pcEAN = fixChk(pcEAN)
                .

            /* Henter Strekkoden for å legge på koden i kommentarfeltet. */
            FIND Strekkode EXCLUSIVE-LOCK WHERE
                 Strekkode.Kode = pcEAN NO-ERROR.
            IF NOT AVAILABLE Strekkode THEN
            DO:
                CREATE Strekkode.
                ASSIGN
                    Strekkode.ArtikkelNr        = bVPIArtBas.ArtikkelNr 
                    Strekkode.Kode              = pcEAN
                    Strekkode.StrKode           = VPIStrekkode.StrKode
                    Strekkode.KodeType          = 1
                    Strekkode.BestillingsNummer = VPIStrekkode.Bestillingsnummer
                    Strekkode.ERPNr             = VPIStrekkode.ERPNr
                    .
            END.
            ELSE ASSIGN Strekkode.BestillingsNummer = IF VPIStrekkode.BestillingsNummer <> '' THEN VPIStrekkode.Bestillingsnummer ELSE Strekkode.BestillingsNummer
                        Strekkode.ERPNr             = IF VPIStrekkode.ERPNr <> '' THEN VPIStrekkode.ERPNr ELSE Strekkode.ERPNr.
        END. /* EAN-128 TRANSACTION */
        
        IF AVAILABLE Strekkode THEN RELEASE Strekkode.
        
    END. /* STREKKODER */

END PROCEDURE.


PROCEDURE OpprettBilder:

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  BILDER:
  FOR EACH VPIBildeRegister NO-LOCK 
      WHERE VPIBildeRegister.EkstVPILevNr = bVPIArtBas.EkstVPILevNr 
        AND VPIBildeRegister.VareNr       = bVPIArtBas.varenr TRANSACTION:

      /* Renser bort gammelt bilde */
      FIND BildeRegister EXCLUSIVE-LOCK WHERE
          BildeRegister.BildNr = VPIBildeRegister.BildNr NO-ERROR.
      IF AVAILABLE BildeRegister THEN
      DO:
          FOR EACH BildeData OF BildeRegister EXCLUSIVE-LOCK:
              DELETE BildeData.
          END.
          DELETE BildeRegister.
      END.
      /* Legger opp nytt bilde. */
      IF NOT CAN-FIND(BildeRegister WHERE
                      BildeRegister.BildNr = VPIBilderegister.BildNr) THEN
      DO:
          CREATE BildeRegister.
          BUFFER-COPY VPIBildeRegister TO BildeRegister
              .
      END.
      FOR EACH VPIBildeData OF VPIBildeRegister:
          IF NOT CAN-FIND(BildeData WHERE
                          BildeData.BildNr = VPIBildeData.BildNr AND
                          BildeData.Teller = VPIBildeData.Teller) THEN
          DO:
              CREATE BildeData.
              BUFFER-COPY VPIBildeData TO BildeData
                  .
          END.
      END.
      IF AVAILABLE BildeRegister THEN RELEASE BildeRegister.
      IF AVAILABLE BildeData THEN RELEASE BildeData.
  END. /* BILDER */

END PROCEDURE.

PROCEDURE OpprettErstattningsvarer:

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ERSTATTNINGSVARER:
  FOR EACH VPIErstattningsvare NO-LOCK 
      WHERE VPIErstattningsvare.EkstVPILevNr = bVPIArtBas.EkstVPILevNr 
        AND VPIErstattningsvare.VareNr       = bVPIArtBas.varenr TRANSACTION:

      FIND Erstattningsvare EXCLUSIVE-LOCK WHERE
          Erstattningsvare.ArtikkelNr = VPIErstattningsvare.ArtikkelNr NO-ERROR.
      IF NOT AVAILABLE Erstattningsvare THEN
      DO:
          CREATE Erstattningsvare.
      END.
      BUFFER-COPY VPIErstattningsvare TO Erstattningsvare.
      RELEASE Erstattningsvare.
  END. /* ERSTATTNINGSVARER */

END PROCEDURE.

PROCEDURE OpprettPakkeLinjer:

/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  PAKKELINJE:
  FOR EACH VPIPakkelinje NO-LOCK 
      WHERE VPIPakkelinje.EkstVPILevNr = bVPIArtBas.EkstVPILevNr 
        AND VPIPakkeLinje.VareNr       = bVPIArtBas.varenr TRANSACTION:

      FIND Pakkelinje EXCLUSIVE-LOCK WHERE
          Pakkelinje.ArtikkelNr   = VPIPakkelinje.ArtikkelNr AND
          Pakkelinje.PkArtikkelNr = VPIPakkelinje.PkArtikkelNr NO-ERROR.
      IF NOT AVAILABLE PakkeLinje THEN
      DO:
          CREATE PakkeLinje.
      END.
      BUFFER-COPY VPIPakkeLinje TO PakkeLinje.
      RELEASE Pakkelinje.
  END. /* PAKKELINJE */

END PROCEDURE.

FUNCTION FixChk RETURNS CHAR 
  (INPUT cKode AS CHAR):
      
  DEF VAR iCount1 AS INTE NO-UNDO.
  DEF VAR iMulti  AS INTE INIT 1 NO-UNDO.
  DEF VAR iSum AS INTE NO-UNDO.
  
  DO iCount1 = LENGTH(cKode) TO 1 BY -1:  
      ASSIGN iMulti = IF iMulti = 1 THEN 3 ELSE 1
             iSum = iSum + INT(SUBSTR(cKode,iCount1,1)) * iMulti.
  END.
  RETURN cKode + string((10 - iSum MODULO 10) MODULO 10).
END FUNCTION.

FUNCTION doUpdate RETURNS LOG (INPUT icFieldName AS CHAR):
  IF bNew THEN RETURN TRUE.
  ELSE RETURN LOOKUP(icFieldName,cFieldList,'|') GT 0.
END FUNCTION.
