TRIGGER PROCEDURE FOR WRITE OF ArtBas OLD BUFFER oldArtBas.

DEFINE VARIABLE cTekst            AS CHAR      NO-UNDO.
DEFINE VARIABLE trgcReturn        AS CHARACTER NO-UNDO.
DEFINE VARIABLE trgbOk            AS LOG       NO-UNDO.
DEFINE VARIABLE bInitBildekode    AS LOG       NO-UNDO.
DEFINE VARIABLE bUndertrykk       AS LOG       NO-UNDO.
DEFINE VARIABLE bHKInstall        AS LOG       NO-UNDO.

DEFINE BUFFER trgEkstEDBSystem FOR EkstEDBSystem.
DEFINE BUFFER trgArtPris       FOR ArtPris.
DEFINE BUFFER trgStrekkode     FOR Strekkode.
DEFINE BUFFER trgVarGr         FOR VarGr.
DEFINE BUFFER trgArtBas        FOR ArtBas.
DEFINE BUFFER trgELogg         FOR ELogg.
DEFINE VARIABLE cStrl AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iTst  AS INTEGER    NO-UNDO.
DEFINE VARIABLE cKode AS CHARACTER  NO-UNDO.

/* Skal initiering av bildekode slås av? */
{syspara.i 2 4 24 cTekst}
IF CAN-DO('1,j,ja,y,yes,true',cTekst) 
  THEN bInitBildekode = TRUE.
  ELSE bInitBildekode = FALSE.

/* Er vi på HK? */
{syspara.i 1 1 18 cTekst}
IF CAN-DO('1,j,ja,y,yes,true',cTekst) 
  THEN bHKInstall = TRUE.
  ELSE bHKInstall = FALSE.

/* Skal utlegg til kasse av artikkelendringer gjørs? */
{syspara.i 2 4 43 cTekst}
IF CAN-DO('1,j,ja,y,yes,true',cTekst) 
  THEN bUndertrykk = TRUE.
  ELSE bUndertrykk = FALSE.

FIND trgVarGr NO-LOCK WHERE
  trgVarGr.Vg = ArtBas.Vg NO-ERROR.
IF AVAILABLE trgVarGr THEN 
  ASSIGN
    ArtBas.Hg = trgVarGr.Hg. 

IF  oldArtBas.Vg                     <> ArtBas.Vg OR 
    oldArtBas.LopNr                  <> ArtBas.LopNr OR 
    oldArtBas.SaSong                 <> ArtBas.SaSong OR 
    oldArtBas.Farg                   <> ArtBas.Farg OR 
    oldArtBas.Klack                  <> ArtBas.Klack OR 
    oldArtBas.MatKod                 <> ArtBas.MatKod OR 
    oldArtBas.BildNr                 <> ArtBas.BildNr OR 
    oldArtBas.Beskr                  <> ArtBas.Beskr OR 
    oldArtBas.LevNr                  <> ArtBas.LevNr OR 
    oldArtBas.LevKod                 <> ArtBas.LevKod OR 
    oldArtBas.ov-id                  <> ArtBas.ov-id OR 
    oldArtBas.last-id                <> ArtBas.last-id OR 
    oldArtBas.foder-id               <> ArtBas.foder-id OR 
    oldArtBas.inner-id               <> ArtBas.inner-id OR 
    oldArtBas.slit-id                <> ArtBas.slit-id OR 
    oldArtBas.anv-id                 <> ArtBas.anv-id OR 
    oldArtBas.valkod                 <> ArtBas.valkod OR 
    oldArtBas.VMId                   <> ArtBas.VMId OR 
    oldArtBas.LevFargKod             <> ArtBas.LevFargKod OR 
    oldArtBas.BongTekst              <> ArtBas.BongTekst OR
    oldArtBas.AnonseArtikkel         <> ArtBas.AnonseArtikkel OR 
    oldArtBas.VgKat                  <> ArtBas.VgKat OR 
    oldArtBas.ProdNr                 <> ArtBas.ProdNr OR 
    oldArtBas.SattPaKampanje         <> ArtBas.SattPaKampanje OR 
    oldArtBas.OPris                  <> ArtBas.OPris OR 
    oldArtBas.BildeIKasse            <> ArtBas.BildeIKasse OR 
    oldArtBas.Pakke                  <> ArtBas.Pakke OR 
    oldArtBas.Alder                  <> ArtBas.Alder OR 
    oldArtBas.IKasse                 <> ArtBas.IKasse OR 
    oldArtBas.BehKode                <> ArtBas.BehKode OR 
    oldArtBas.Pakkenr                <> ArtBas.Pakkenr OR 
    oldArtBas.AnbefaltPris           <> ArtBas.AnbefaltPris OR 
    oldArtBas.KundeRabatt            <> ArtBas.KundeRabatt OR 
    oldArtBas.Etikett                <> ArtBas.Etikett OR 
    oldArtBas.SalgsEnhet             <> ArtBas.SalgsEnhet OR 
    oldArtBas.Slasket                <> ArtBas.Slasket OR 
    oldArtBas.SlaskArtikkelNr        <> ArtBas.SlaskArtikkelNr OR 
    oldArtBas.ModellFarge            <> ArtBas.ModellFarge OR 
    oldArtBas.HovedModellFarge       <> ArtBas.HovedModellFarge OR 
    oldArtBas.Etikettekst1           <> ArtBas.Etikettekst1 OR 
    oldArtBas.EtiLayout              <> ArtBas.EtiLayout OR 
    oldArtBas.LinkVareNr             <> ArtBas.LinkVareNr OR 
    oldArtBas.Mengde                 <> ArtBas.Mengde OR 
    oldArtBas.ManRabIKas             <> ArtBas.ManRabIKas OR 
    oldArtBas.Pant                   <> ArtBas.Pant OR 
    oldArtBas.BestForslag            <> ArtBas.BestForslag OR 
    oldArtBas.GarantiKl              <> ArtBas.GarantiKl OR 
    oldArtBas.LevDato3               <> ArtBas.LevDato3 OR 
    oldArtBas.LevDato4               <> ArtBas.LevDato4 OR 
    oldArtBas.LinjeMerknad           <> ArtBas.LinjeMerknad OR 
    oldArtBas.KatalogPris            <> ArtBas.KatalogPris OR 
    oldArtBas.forhRab%               <> ArtBas.forhRab% OR 
    oldArtBas.supRab%                <> ArtBas.supRab% OR 
    oldArtBas.KjedeVare              <> ArtBas.KjedeVare OR 
    oldArtBas.VPIBildeKode           <> ArtBas.VPIBildeKode OR 
    oldArtBas.StrKode1               <> ArtBas.StrKode1 OR 
    oldArtBas.StrKode2               <> ArtBas.StrKode2 OR 
    oldArtBas.AntIPakn               <> ArtBas.AntIPakn OR 
    oldArtBas.VareFakta              <> ArtBas.VareFakta OR 
    oldArtBas.Lokasjon               <> ArtBas.Lokasjon OR 
    oldArtBas.Gjennomfaktureres      <> ArtBas.Gjennomfaktureres OR 
    oldArtBas.KjedeRab%              <> ArtBas.KjedeRab% OR 
    oldArtBas.KjedeInnkPris          <> ArtBas.KjedeInnkPris OR 
    oldArtBas.Depositum              <> ArtBas.Depositum OR 
    oldArtBas.Medlemsutbytte         <> ArtBas.Medlemsutbytte OR 
    oldArtBas.HoyLavMva              <> ArtBas.HoyLavMva OR 
    oldArtBas.Etikettekst2           <> ArtBas.Etikettekst2 OR 
    oldArtBas.WebButikkArtikkel      <> ArtBas.WebButikkArtikkel OR 
    oldArtBas.RAvdNr                 <> ArtBas.RAvdNr OR 
    oldArtBas.SanertDato             <> ArtBas.SanertDato OR 
    oldArtBas.Anbrekk                <> ArtBas.Anbrekk OR 
    oldArtBas.InkrAnbrekk            <> ArtBas.InkrAnbrekk OR 
    oldArtBas.KjedeValutaPris        <> ArtBas.KjedeValutaPris OR 
    oldArtBas.KjedeProdusent         <> ArtBas.KjedeProdusent OR 
    oldArtBas.PostVekt               <> ArtBas.PostVekt OR 
    oldArtBas.PostLengde             <> ArtBas.PostLengde OR 
    oldArtBas.PostHoyde              <> ArtBas.PostHoyde OR 
    oldArtBas.PostBredde             <> ArtBas.PostBredde OR 
    oldArtBas.WebMinLager            <> ArtBas.WebMinLager OR 
    oldArtBas.KampanjeKode           <> ArtBas.KampanjeKode OR 
    oldArtBas.WebLeveringstid        <> ArtBas.WebLeveringstid OR 
    oldArtBas.Leveringstid           <> ArtBas.Leveringstid OR 
    oldArtBas.SalgsEnhetsType        <> ArtBas.SalgsEnhetsType OR 
    oldArtBas.JamforEnhet            <> ArtBas.JamforEnhet OR 
    oldArtBas.TilgjengeligFraLev     <> ArtBas.TilgjengeligFraLev OR 
    oldArtBas.LevDatoStopp1          <> ArtBas.LevDatoStopp1 OR 
    oldArtBas.LevDatoStopp2          <> ArtBas.LevDatoStopp2 OR 
    oldArtBas.LevDatoStopp3          <> ArtBas.LevDatoStopp3 OR 
    oldArtBas.LevDatoStopp4          <> ArtBas.LevDatoStopp4 OR 
    oldArtBas.Utgatt                 <> ArtBas.Utgatt OR 
    oldArtBas.UtgattDato             <> ArtBas.UtgattDato OR 
    oldArtBas.KjedeSupRab%           <> ArtBas.KjedeSupRab% OR 
    oldArtBas.KjedeSupInnkPris       <> ArtBas.KjedeSupInnkPris OR 
    oldArtBas.NON_Sale               <> ArtBas.NON_Sale OR 
    oldArtBas.NegVare                <> ArtBas.NegVare OR 
    oldArtBas.Vekt                   <> ArtBas.Vekt OR 
    oldArtBas.Grunnsortiment         <> ArtBas.Grunnsortiment OR 
    oldArtBas.Bonus_Givende          <> ArtBas.Bonus_Givende OR 
    oldArtBas.PubliserINettbutikk    <> ArtBas.PubliserINettbutikk OR 
    oldArtBas.Link_til_Nettside      <> ArtBas.Link_til_Nettside OR 
    oldArtBas.Telefonkort            <> ArtBas.Telefonkort OR 
    oldArtBas.MengdeRabatt           <> ArtBas.MengdeRabatt OR 
    oldArtBas.HovedKatNr             <> ArtBas.HovedKatNr OR 
    oldArtBas.Kjokkenskriver         <> ArtBas.Kjokkenskriver OR 
    oldArtBas.LinkVareAnt            <> ArtBas.LinkVareAnt OR 
    oldArtBas.SalgsStopp             <> ArtBas.SalgsStopp OR 
    oldArtBas.AlfaKode2              <> ArtBas.AlfaKode2
THEN DO:
  ASSIGN
  ArtBas.EDato       = TODAY
  ArtBas.ETid        = TIME
  ArtBas.BrukerId    = USERID("skotex").
END.

ASSIGN
  ArtBas.Pant        = CAN-DO('8',STRING(ArtBas.ArtSlag))
  ArtBas.Pakke       = CAN-DO('7',STRING(ArtBas.ArtSlag))
  ArtBas.Vekt        = CAN-DO('1,2',STRING(ArtBas.ArtSlag))
  ArtBas.JamforEnhet = IF ArtBas.JamforEnhet = 'BLK' 
                         THEN ''
                       ELSE ArtBas.JamforEnhet
  ArtBas.LinkVareant = IF ArtBas.LinkVareant = 0 THEN 1 ELSE ArtBas.LinkVareant
  .
  
/* Inn/ut av grunnsortiment */
IF oldArtBas.Grunnsortiment <> ArtBas.Grunnsortiment THEN 
DO:
  IF ArtBas.Grunnsortiment THEN ArtBas.Notat = 'Lagt til grunnsortiment ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + ' ' + USERID("skotex") + CHR(10) + ArtBas.Notat.
  ELSE ArtBas.Notat = 'Tatt ut av grunnsortiment ' + STRING(TODAY) + ' ' + STRING(TIME,"HH:MM:SS") + ' ' + USERID("skotex") + CHR(10) + ArtBas.Notat.
END.
  
/* Initiering av bildekodefeltet. */
IF bInitBildekode = FALSE THEN DO:
  IF ArtBas.VPIBildeKode = '' THEN ArtBas.VPIBildeKode = TRIM(REPLACE(REPLACE(STRING(ArtBas.LevNr) + '_' + ArtBas.LevKod + '_' + ArtBas.LevFargKod,'/','_'),' ','_'),'_') + '.jpg'.
END.

/* Logger endring i WebButikkArtikkel */
IF oldArtBas.WebButikkArtikkel <> ArtBas.WebButikkArtikkel THEN DO:
  OUTPUT TO value(SESSION:TEMP-DIRECTORY + '\' + 'nettartikkel.txt') APPEND.
    IF ArtBas.WebButikkArtikkel = TRUE THEN 
      PUT UNFORMATTED STRING(TODAY) ";" STRING(TIME,"HH:MM:SS") ";" USERID('skotex') ";Aktivert i nettbutikk   ;"
                      STRING(ArtBas.ArtikkelNr) ";" ArtBas.LevKod ";" ArtBas.Beskr ";" ArtBas.LevFargKod ";" ArtBas.WebButikkArtikkel SKIP.
    ELSE DO: 
        PUT UNFORMATTED STRING(TODAY) ";" STRING(TIME,"HH:MM:SS") ";" USERID('skotex') ";Deaktivert i nettbutikk ;"
                        STRING(ArtBas.ArtikkelNr) ";" ArtBas.LevKod ";" ArtBas.Beskr ";" ArtBas.LevFargKod ";" ArtBas.WebButikkArtikkel SKIP.
        FOR EACH artbut WHERE artbut.artikkelnr = artbas.artikkelnr:
            artbut.deleted = TRUE.
        END.
    END.
  OUTPUT CLOSE.
END.
  
/* Utlegg til ferskvarevekt */
IF CAN-FIND (FIRST SysPara WHERE SysPara.SysHId = 23 AND SysPara.SysGr = 1 AND SysPara.Parameter1 > '0') AND 
  (ArtBas.ArtSlag = 1 OR oldArtBas.ArtSlag = 1) THEN 
DO FOR trgELogg:
  LOOPEN:
  FOR EACH trgStrekkode OF ArtBas NO-LOCK WHERE 
    LENGTH(trgStrekkode.Kode) = 13:
    FIND trgELogg WHERE 
         trgELogg.TabellNavn     = "ArtBas" AND
         trgELogg.EksterntSystem = "FVEKT"    AND
         trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr) + CHR(1) + STRING(trgStrekkode.Kode) NO-ERROR NO-WAIT.
    IF LOCKED trgELogg THEN 
        LEAVE LOOPEN.
    ELSE DO:
      IF NOT AVAIL trgELogg THEN DO:
          CREATE trgELogg.
          ASSIGN trgELogg.TabellNavn     = "ArtBas"
                 trgELogg.EksterntSystem = "FVEKT"   
                 trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr) + CHR(1) + STRING(trgStrekkode.Kode).
      END.
      ASSIGN trgELogg.EndringsType = IF ArtBas.ArtSlag = 1 THEN 1 ELSE 3 
             trgELogg.Behandlet    = FALSE.
      RELEASE trgELogg.
    END.
  END. /*LOOPEN */
END. /* trgELogg Scoope */

/* Artikkelendringer legges ut til kasse. Utlegg til kasse kan være undertrykket.                     */
/* Er utlegg undertrykket, vil alikevel endirnger av pris og/eller strekkode trigge utlegg til kasse. */
/* Denne undertrykkingen er gjort for Preem. Slik at anbudsfiler kan leses inn på hk profilen         */
/* uten at dette medfører utlegg til akke butikker/kasser.                                            */  
IF bUndertrykk = FALSE AND bHKInstall = FALSE THEN 
IKKE_UNDERTRYKKET:
DO FOR trgELogg:  
    /* Legger ut endringsposter til de det gjelder. */
    IF ArtBas.iKasse = TRUE OR NEW ArtBas THEN 
    DO:
      /* Kun hvis aktuelle felt for kassen er berørt, skal det skapes trgELogg poster. */
      IF   (oldArtBas.Vg                <> ArtBas.Vg               
         OR oldArtBas.Bongtekst         <> ArtBas.Bongtekst        
         OR oldArtBas.OPris             <> ArtBas.OPris            
         OR oldArtBas.LinkVareNr        <> ArtBas.LinkVareNr       
         OR oldArtBas.IKasse            <> ArtBas.IKasse           
         OR oldArtBas.Medlemsutbytte    <> ArtBas.Medlemsutbytte   
         OR oldArtBas.KundeRabatt       <> ArtBas.KundeRabatt      
         OR oldArtBas.Beskr             <> ArtBas.Beskr            
         OR oldArtBas.Pakkenr           <> ArtBas.Pakkenr          
         OR oldArtBas.Pant              <> ArtBas.Pant             
         OR oldArtBas.Alder             <> ArtBas.Alder            
         OR oldArtBas.GarantiKl         <> ArtBas.GarantiKl        
         OR oldArtbas.StrTypeID         <> Artbas.StrTypeID        
         OR oldArtBas.LopNr             <> ArtBas.LopNr            
         OR oldArtBas.HoyLavMva         <> ArtBas.HoyLavMva        
         OR oldArtBas.ArtikkelNr        <> ArtBas.ArtikkelNr       
         OR oldArtBas.LevKod            <> ArtBas.LevKod           
         OR oldArtBas.Farg              <> ArtBas.Farg             
         OR oldArtBas.ManRabIKas        <> ArtBas.ManRabIKas       
         OR oldArtBas.Telefonkort       <> ArtBas.Telefonkort       
         OR oldArtBas.Gjennomfaktureres <> ArtBas.Gjennomfaktureres)
         OR oldArtBas.SalgsStopp        <> ArtBas.SalgsStopp 
         THEN
         OPPRETT: 
         DO:
           FIND trgELogg WHERE 
                trgELogg.TabellNavn     = "ArtBas" AND
                trgELogg.EksterntSystem = "POS"    AND
                trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR NO-WAIT.
           IF LOCKED trgELogg THEN 
               LEAVE OPPRETT.
           ELSE DO:     
             IF NOT AVAIL trgELogg THEN DO:
                 CREATE trgELogg.
                 ASSIGN trgELogg.TabellNavn     = "ArtBas"
                        trgELogg.EksterntSystem = "POS"   
                        trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
             END.
             ASSIGN trgELogg.EndringsType = 1 
                    trgELogg.Behandlet    = FALSE.
           END.
         END. /* OPPRETT */
    END.
    /* I kasse flagget er slått av. */
    ELSE IF ArtBas.IKasse = FALSE AND ArtBas.IKasse <> oldArtBas.iKasse THEN
    ELOOPEN: 
    DO:
       FIND trgELogg WHERE 
            trgELogg.TabellNavn     = "ArtBas" AND
            trgELogg.EksterntSystem = "POS"    AND
            trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR NO-WAIT.
       IF LOCKED trgELogg THEN 
         LEAVE ELOOPEN.
       ELSE DO:
         IF NOT AVAIL trgELogg THEN DO:
             CREATE trgELogg.
             ASSIGN trgELogg.TabellNavn     = "ArtBas"
                    trgELogg.EksterntSystem = "POS"   
                    trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
         END.
         ASSIGN trgELogg.EndringsType = 3 
                trgELogg.Behandlet    = FALSE.
       END.
    END. /* ELOOPEN */
    IF AVAILABLE trgELogg THEN RELEASE trgELogg.
END. /* IKKE_UNDERTRYKKET */

/* Frisker opp 2av5 Interleave kodene på artikkelen. */
/* TN 7/8-13 Dette kan ikke gjøres her. Gir feilmelding fra trigger ' 'Cycle in procedurecals'
   Har flyttet koden til artikkelkortet. */
DEFINE VARIABLE cGenInterleave AS CHARACTER NO-UNDO.
{syspara.i 2 4 17 cGenInterleave}
IF (oldArtBas.Lopnr = ? AND ArtBas.LopNr <> ?) AND cGenInterleave = "1" THEN DO:
    IF ArtBas.Vg <= 999 AND ArtBas.LopNr <= 9999 THEN
    DO:
        /*RUN genInterleaf.p (ArtBas.Artikkelnr) NO-ERROR.*/
        STREKKODE:
        FOR EACH trgSTrekkode OF ArtBas EXCLUSIVE-LOCK:
          FIND StrKonv WHERE 
              StrKonv.StrKode = trgStrekkode.StrKode USE-INDEX StrKode NO-LOCK NO-ERROR.
          IF NOT AVAIL StrKonv THEN
              NEXT STREKKODE.
          ASSIGN cStrl = IF NUM-ENTRIES(StrKonv.Storl,".") = 2 
                           THEN TRIM(REPLACE(StrKonv.Storl,".","")) 
                           ELSE TRIM(StrKonv.Storl) + "0"
                 cStrl = FILL("0",4 - LENGTH(cStrl)) + cStrl
                 cKode = STRING(ArtBas.Vg,"999")     +
                         STRING(ArtBas.LopNr,"9999") +
                         "0" +
                         cStrl NO-ERROR.
          IF ERROR-STATUS:ERROR = FALSE THEN 
              trgStrekkode.Bestillingsnummer = cKode.
        END. /* STREKKODE */
    END.
END.

/* Utlegg til nettbutikk */
FIND FIRST trgEkstEDBSystem WHERE 
    trgEkstEDBSystem.DataType = "WEBBUT" AND 
    trgEkstEDBSystem.Aktiv = TRUE NO-LOCK NO-ERROR.
IF AVAILABLE trgEkstEDBSystem THEN
WEBBUTIKK:
DO FOR trgELogg:
    /* Endring som skal til Web */
    IF ArtBas.WebButikkArtikkel THEN 
    WEBBUT:
    DO:
        FIND trgELogg WHERE 
             trgELogg.TabellNavn     = "ArtBas" AND
             trgELogg.EksterntSystem = "WEBBUT"    AND
             trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR NO-WAIT.
        IF NOT LOCKED trgELogg THEN 
        DO:
          IF NOT AVAIL trgELogg THEN DO:
              CREATE trgELogg.
              ASSIGN trgELogg.TabellNavn     = "ArtBas"
                     trgELogg.EksterntSystem = "WEBBUT"   
                     trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
          END.
          ASSIGN trgELogg.EndringsType = 1 
                 trgELogg.Behandlet    = FALSE.
          RELEASE trgELogg.
        END.
        
        /* Når artikkel legges ut, skal alltid også lager legges ut. */
        FOR EACH Lager NO-LOCK WHERE
            Lager.ArtikkelNr = ArtBas.ArtikkelNr:
            FIND trgELogg WHERE 
                 trgELogg.TabellNavn     = "Lager" AND
                 trgELogg.EksterntSystem = "WEBBUT"    AND
                 trgELogg.Verdier        = STRING(Lager.ArtikkelNr)
                                        + chr(1) + string(Lager.butik) NO-ERROR NO-WAIT.
            IF LOCKED trgELogg THEN NEXT.
            ELSE DO:
              IF NOT AVAIL trgELogg THEN DO:
                  CREATE trgELogg.
                  ASSIGN trgELogg.TabellNavn     = "Lager"
                         trgELogg.EksterntSystem = "WEBBUT"   
                         trgELogg.Verdier        = STRING(Lager.ArtikkelNr)
                                          + chr(1) + string(Lager.butik).
              END.
              ASSIGN trgELogg.EndringsType = 1 
                     trgELogg.Behandlet    = FALSE.
              RELEASE trgELogg.
            END.
        END.
    END. /* WEBBUT */

    /* Artikkel skal slettes fra */
    IF oldArtBas.WebButikkArtikkel = TRUE AND
        ArtBas.WebButikkArtikkel   = FALSE THEN 
    WEBBEN:
    DO:
        FIND trgELogg WHERE 
             trgELogg.TabellNavn     = "ArtBas" AND
             trgELogg.EksterntSystem = "WEBBUT"    AND
             trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr) NO-ERROR NO-WAIT.
        IF LOCKED trgELogg THEN LEAVE WEBBEN.
        ELSE DO:
          IF NOT AVAIL trgELogg THEN DO:
              CREATE trgELogg.
              ASSIGN trgELogg.TabellNavn     = "ArtBas"
                     trgELogg.EksterntSystem = "WEBBUT"   
                     trgELogg.Verdier        = STRING(ArtBas.ArtikkelNr).
          END.
          ASSIGN trgELogg.EndringsType = 3 
                 trgELogg.Behandlet    = FALSE.
          RELEASE trgELogg.
        END.
    END. /* WEBBEN */
    IF AVAILABLE trgELogg THEN RELEASE trgELogg.
END. /* WEBBUTIKK */

/* Assigner feltet for utvidet søk. */
UTVIDETSOK:
DO FOR trgArtBas:
  FIND trgArtBas WHERE RECID(trgArtBas) = RECID(ArtBas).
  RUN init_utvidetsok.p (trgArtBas.ArtikkelNr).
  IF AVAILABLE trgArtBas THEN RELEASE trgArtBas.
END. /* UTVIDETSOK */



