/* Oppretter bestilling med input fra varehåndteringsbok
   Krever at data allerede er lagt inn i VarebehBestHode og VarebehBestLinje.
   Kalles fra: varebehbest_lagrebestlinje.p
   
   Opprettet: Høst 2004 av BHa
   Endret:    10.11.05  av BHa:
              - VarebehBestLinje benyttes kun som hjelpetabell ved registrering av BESTILLING
                Visning av bestilte størrelser i varehåndteringsboken henter data rett fra bestilling (BestStr)
              - Spesielt for innhold i VarebehBestLinje: 
                For inndelinger benyttes feltet Storl til å angi sortiment: "Sortiment|<SortId>"
                Henter sortimentets innhold via ArtSort
              30.03.07 av BHa:
              - Skal ikke lenger bytte leverandørnr for kjedeleverte artikler
                Endrer kriteriet for ordregenerering: 
                  Ordre skal ha samme status for kjedelevert/gjennomfakturert på alle artikler
              13.04.07 av BHa:
              - Bruker kalkyle fra varebehlinje uansett
              07.05.07 av BHa
              - Setter ordremottaker på ordre avhengig av varebehandlingslinjens leveringsflagg
                Ny ordre dersom vare ikke stemmer med ordremottaker
              02.08.07 av BHa
              - Dersom ikke HK og direktelevert skal det opprettes ny ordre for ny butikk
              15.11.07 av BHa
              - Direktelevert skal ikke lenger testes på ved opprettelse av ordre
---------------------------------------------------------------------------------------------------------------------------------------*/    

DEF INPUT PARAM ifVareBehNr   AS DEC NO-UNDO.
DEF INPUT PARAM iiCL          AS INT NO-UNDO.
DEF INPUT PARAM iiHodeLinjeId AS INT NO-UNDO.
DEF INPUT PARAM icKalkType    AS CHAR NO-UNDO. /* "Best": Kalkyle fra artbas. "Genbest": Kalkyle fra varebeh.linje */ 
DEF OUTPUT PARAM ocReturn     AS CHAR NO-UNDO.

DEF VAR dLevDato        AS DATE NO-UNDO.
DEF VAR iPrisIdx        AS INT  NO-UNDO.
DEF VAR fTotFriBut      AS INT  NO-UNDO.
DEF VAR cStrList        AS CHAR NO-UNDO.
DEF VAR cSortFordList   AS CHAR NO-UNDO.
DEF VAR iSumAntSort     AS INT  NO-UNDO.  
DEF VAR bCLfriBut       AS LOG  NO-UNDO.
DEF VAR iSuppType       AS INT  NO-UNDO.
DEF VAR iFriAntIx       AS INT  NO-UNDO.
DEF VAR iLevNr          AS INT  NO-UNDO.
DEF VAR cOrdremott      AS CHAR NO-UNDO.
DEF VAR bHKinst         AS LOG  NO-UNDO.
DEF VAR bOkButOrdre     AS LOG  NO-UNDO INIT YES.
DEF VAR bForhRab        AS LOG  NO-UNDO.
DEF VAR fEuro           AS DEC  NO-UNDO.

DEF BUFFER bVarebehLinje FOR VareBehLinje.

{syspara.i 5 5 2 iSuppType INT}
{syspara.i 1 1 18 bHKinst LOGICAL}
{syspara.i 2 1 1 fEuro DEC}

/* Finner poster for initiering: */
FIND VarebehHode 
     WHERE VarebehHode.VarebehNr = ifVarebehNr 
     NO-LOCK NO-ERROR.
IF NOT AVAIL VarebehHode THEN DO:
  ocReturn = "Varehåndteringsbok ikke tilgjengelig".
  RETURN.
END.
/* Dersom vareh.boken er knyttet til en varebok så skal kalkylen hentes varebeh.boken og ikke fra artpris uansett: */
IF CAN-FIND(FIRST VarebokHode WHERE VarebokHode.VarebokNr = VarebehHode.kilde) THEN
  icKalkType = "genbest".

FIND VarebehBestHode OF VarebehHode
     WHERE VarebehBestHode.CLbutikkNr  = iiCL
       AND VarebehBestHode.HodeLinjeId = iiHodeLinjeId
     EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
     
IF NOT AVAIL VarebehBestHode THEN DO:
  ocReturn = "Bestillingshode for varehåndteringsbok ikke tilgjengelig for oppdatering " + PROGRAM-NAME(1).
  RETURN.
END.

FIND VarebehLinje
     WHERE VarebehLinje.VarebehNr = ifVarebehNr
       AND VarebehLinje.ArtikkelNr = VarebehBestHode.Artikkelnr
     NO-LOCK NO-ERROR.
IF NOT AVAIL VarebehLinje THEN DO:
  ocReturn = "Varebehandlingslinje ikke tilgjengelig" + CHR(10) + 
             "VarebehNr: " + STRING(ifVarebehNr) + " ArtikkelNr: " + STRING(VarebehBestHode.Artikkelnr) + CHR(10) +
             "Program: " + PROGRAM-NAME(1)
             .
  RETURN.
END.

FIND FIRST messe NO-LOCK
     OF VarebehHode
     NO-ERROR.
IF AVAIL Messe THEN
  bForhRab = Messe.MesseType = 1.

FIND ArtBas 
     WHERE ArtBas.ArtikkelNr = VarebehBestHode.ArtikkelNr 
     NO-LOCK NO-ERROR.
IF NOT AVAIL ArtBas THEN DO:
  ocReturn = "Finner ikke artikkel " + STRING(VarebehLinje.artikkelnr) + " angitt på bestillingshode".
  RETURN.
END.

/*IF ArtBas.KjedeVare THEN DO:*/
/* IF VarebehLinje.KjedeVare THEN DO:                                                                                                                                */
/*   FIND FIRST SysPara NO-LOCK                                                                                                                                      */
/*        WHERE SysPara.SysHId = 1                                                                                                                                   */
/*          AND SysPara.SysGr  = 1                                                                                                                                   */
/*          AND SysPara.ParaNr = 53                                                                                                                                  */
/*       NO-ERROR.                                                                                                                                                   */
/*   IF AVAIL SysPara THEN DO:                                                                                                                                       */
/*     iLevNr = INT(SysPara.Parameter1) NO-ERROR.                                                                                                                    */
/*     IF ERROR-STATUS:ERROR OR NOT CAN-FIND(FIRST LevBas WHERE LevBas.LevNr = iLevNr) THEN DO:                                                                      */
/*       ocReturn = "Artikkel " + STRING(VarebehLinje.artikkelnr) + " er flagget for levering fra kjede men kjedens leverandørnr er ugyldig (sys.parameter 1,1,53)". */
/*       RETURN.                                                                                                                                                     */
/*     END.                                                                                                                                                          */
/*   END.                                                                                                                                                            */
/*   ELSE iLevNr = VarebehLinje.LevNr.                                                                                                                               */
/* END.                                                                                                                                                              */
/* ELSE                                                                                                                                                              */
  iLevNr = VarebehLinje.LevNr.

FIND FIRST ArtPris OF ArtBas
     WHERE ArtPris.ProfilNr = VarebehHode.ProfilNr
     NO-LOCK NO-ERROR.
IF NOT AVAIL ArtPris THEN DO:
  ocReturn = "Finner ikke pris for artikkel " + STRING(VarebehLinje.artikkelnr) + " med angitt prisprofil fra varehåndtering".
  RETURN.
END.

FIND FIRST LevBas 
     WHERE LevBas.LevNr = iLevNr
     NO-LOCK NO-ERROR.
IF NOT AVAIL LevBas THEN DO:
  ocReturn = "Finner ikke leverandør for artikkel " + STRING(VarebehLinje.artikkelnr).
  RETURN.
END.

IF VareBehBestHode.LevDato = ? THEN DO:
  RUN begweek.p (VareBehBestHode.Levuke,OUTPUT dLevDato).
  IF dLevDato = ? THEN DO:
    ocReturn = "Ugyldig leveringsdato (uke) for bestilling, artikkelnr " + STRING(VarebehLinje.artikkelnr).
    RETURN.
  END.
END.
ELSE dLevDato = VareBehBestHode.LevDato.

/* Herfra er alle data tilgjengelige for oppdatering: */
 
/* TN 12/3-08 Ref. møte med Sport1 11/3-08. */
cOrdreMott = IF (VarebehLinje.KjedeVare AND NOT VareBehLinje.Gjennomfaktureres )THEN "KJEDE"
             ELSE IF (NOT VarebehLinje.KjedeVare AND VareBehLinje.Gjennomfaktureres) THEN "GJENNOM"
             ELSE IF (VarebehLinje.KjedeVare AND VareBehLinje.Gjennomfaktureres) THEN "GJENNOM"
             ELSE "".

FIND FIRST Ordre 
     WHERE Ordre.LevNr          = iLevNr 
       AND Ordre.VareBehNr      = ifVarebehNr 
       AND Ordre.LeveringsDato  = dLevDato
       AND Ordre.OrdreStatus    = 1
       AND Ordre.CL             = iiCl
       AND Ordre.OrdreMottaker  = cOrdremott
     NO-LOCK NO-ERROR.
/* Dersom ikke HK og direktelevert skal det opprettes ny ordre for ny butikk: (BHa 02/08/07) */
/* Direktelevert skal ikke lenger testes på: (BHa 15/11/07) */

/* IF AVAIL Ordre AND VareBehBestHode.DirekteLev AND NOT bHKinst THEN DO: */
/*   bOkButOrdre = NO.                                                    */
/*   FOR EACH VarebehBestLinje NO-LOCK                                    */
/*       OF VarebehBestHode:                                              */
/*     FIND FIRST BestStr NO-LOCK                                         */
/*          WHERE BestStr.BestNr = VareBehBestHode.BestNr                 */
/*            AND BestStr.Butik  = VareBehBestLinje.BestiltButikkNr       */
/*          NO-ERROR.                                                     */
/*     IF AVAIL BestStr THEN DO:                                          */
/*       bOkButOrdre = YES.                                               */
/*       LEAVE.                                                           */
/*     END.                                                               */
/*   END.                                                                 */
/* END.                                                                   */

IF NOT AVAIL ordre OR NOT bOkButOrdre THEN DO:
  CREATE Ordre.
  ASSIGN Ordre.VarebehNr      = ifVarebehNr
         Ordre.LevNr          = iLevNr
         Ordre.LeveringsDato  = dLevDato
         Ordre.CL             = iiCl
         Ordre.OrdreMottaker  = cOrdremott
         .
END.

FIND BestHode
     WHERE BestHode.BestNr = VarebehBestHode.BestNr
     EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
IF NOT AVAIL BestHode THEN DO:
  IF LOCKED BestHode THEN DO:
    ocReturn = "Bestilling (bestillingshode) er ikke tilgjengelig for oppdatering".
    RETURN.
  END.
  ELSE 
    CREATE BestHode.
END.
ELSE IF BestHode.BestStat > 3 THEN DO:
  ocReturn = "Bestilling er sendt og kan ikke endres".
  RETURN.
END.
ELSE 
  ASSIGN BestHode.TotAntPar      = 0
         BestHode.TotDbKr        = 0
         BestHode.TotInnkjVerdi  = 0
         BestHode.TotSalgsVerdi  = 0
         .
                  
ASSIGN /* BestHode.AnonseArtikkel  =  */
       BestHode.ArtikkelNr      = VareBehBestHode.ArtikkelNr
       BestHode.Beskrivelse     = VarebehHode.VareBehBeskrivelse
       BestHode.BestillingsDato = TODAY
       BestHode.BestStat        = 3
/*        BestHode.BestType        = grunnbestilling / tillegg */
       BestHode.CL              = VareBehBestHode.CLButikkNr
       BestHode.DirekteLev      = VareBehBestHode.DirekteLev
       BestHode.LevDato         = dLevDato
       BestHode.LevFargKod      = ArtBas.LevFargKod
       BestHode.LevKod          = ArtBas.LevKod
       BestHode.LevNr           = iLevNr
       BestHode.LevTid          = STRING(VareBehBestHode.Levuke)
       BestHode.Merknad         = VarebehLinje.LinjeMerknad
       BestHode.OrdreNr         = Ordre.ordrenr
       BestHode.StrTypeID       = ArtBas.StrTypeId
       BestHode.VareBehNr       = VareBehBestHode.VareBehNr
       BestHode.BestType        = IF icKalkType = "best" THEN iSuppType ELSE BestHode.BestType
       .

/* 1 per butikik som har beställning */
FOR EACH BestLinje OF BestHode
    EXCLUSIVE-LOCK:
  DELETE BestLinje.
END.
FOR EACH VarebehBestLinje OF VarebehBestHode
    NO-LOCK
    BREAK BY VarebehBestLinje.BestiltButikkNr:
  IF FIRST-OF(VarebehBestLinje.BestiltButikkNr) THEN DO:
    CREATE BestLinje.
    ASSIGN BestLinje.BestNr  = BestHode.BestNr
           BestLinje.Butik   = VareBehBestLinje.BestiltButikkNr
           .
  END.
END.

FOR EACH BestPris OF BestHode
    EXCLUSIVE-LOCK:
  DELETE BestPris.
END.
iPrisIdx = 1. /* IF ArtPris.Tilbud THEN 2 ELSE 1. */
CREATE BestPris.
FIND FIRST Valuta OF LevBas NO-LOCK NO-ERROR.

/* Elementer som ikke er påvirket av rabatten */
ASSIGN 
    BestPris.ArtikkelNr    = BestHode.ArtikkelNr
    BestPris.BestNr        = BestHode.BestNr
    BestPris.BestStat      = BestHode.BestStat
    BestPris.ValPris       = IF AVAIL Valuta 
                               THEN VarebehLinje.InnkjopsPris / Valuta.ValKurs 
                               ELSE VarebehLinje.InnkjopsPris
    BestPris.InnkjopsPris  = VarebehLinje.InnkjopsPris 
    BestPris.MvaKr         = VarebehLinje.Pris * (VarebehLinje.Mva% / (100 + VarebehLinje.Mva%))
    BestPris.Mva%          = VarebehLinje.Mva%         
    BestPris.Pris          = VarebehLinje.Pris
    BestPris.EuroPris      = VareBehLinje.Pris * fEuro
    BestPris.ProfilNr      = VarebehHode.ProfilNr        
    .

IF icKalkType = "best" THEN 
DO:  /* Vanlig bestilling */
  /* Elementer som påvirkes av rabatt */
  IF bForhRab THEN
      ASSIGN
      BestPris.VareKost      = VarebehLinje.InnkjopsPris - ((VarebehLinje.InnkjopsPris * VarebehLinje.forhRab%) / 100)      
      BestPris.Rab1%         = VarebehLinje.forhRab%        
      BestPris.Rab1Kr        = (VarebehLinje.InnkjopsPris * VarebehLinje.forhRab%) / 100
      BestPris.DBKr          = VarebehLinje.Pris - BestPris.MvaKr - BestPris.VareKost 
      BestPris.DB%           = ROUND((BestPris.DBKr * 100)/ (VarebehLinje.Pris - BestPris.MvaKr),2)
      .
  ELSE
      ASSIGN
      BestPris.VareKost      = VarebehLinje.InnkjopsPris - ((VarebehLinje.InnkjopsPris * VarebehLinje.supRab%) / 100)      
      BestPris.Rab1%         = VarebehLinje.supRab%        
      BestPris.Rab1Kr        = (VarebehLinje.InnkjopsPris * VarebehLinje.supRab%) / 100
      BestPris.DBKr          = VarebehLinje.Pris - BestPris.MvaKr - BestPris.VareKost
      BestPris.DB%           = ROUND((BestPris.DBKr * 100)/ (VarebehLinje.Pris - BestPris.MvaKr),2)
      .
END.
ELSE DO:
  /* Elementer som ikke er påvirket av rabatten */
  ASSIGN 
      BestPris.VareKost      = VarebehLinje.InnkjopsPris - ((VarebehLinje.InnkjopsPris * VarebehLinje.forhRab%) / 100)      
      BestPris.Rab1%         = VarebehLinje.forhRab%        
      BestPris.Rab1Kr        = (VarebehLinje.InnkjopsPris * VarebehLinje.forhRab%) / 100
      BestPris.DBKr          = VarebehLinje.Pris - BestPris.MvaKr - BestPris.VareKost 
      BestPris.DB%           = ROUND((BestPris.DBKr * 100)/ (VarebehLinje.Pris - BestPris.MvaKr),2)
      .
END.

FOR EACH BestSort OF BestHode
    EXCLUSIVE-LOCK:
  DELETE BestSort.
END.
CREATE BestSort.
ASSIGN BestSort.Antall        = 0
       BestSort.AntSort       = 0
       BestSort.BestNr        = BestHode.BestNr
       BestSort.Fordeling     = ""
       BestSort.Fri           = TRUE
       BestSort.SortID        = "FRI"
       BestSort.Storrelser    = IF TRIM(VareBehBestHode.AlfaFordeling) NE "OS" THEN 
                                  TRIM(REPLACE(REPLACE(REPLACE(VareBehBestHode.AlfaFordeling,","," "),"  "," "),"   "," "))
                                ELSE "1"
       BestSort.StrInterval   = ""
       .

FOR EACH BestStr OF BestHode
    EXCLUSIVE-LOCK:
  DELETE BestStr.
END.
FOR EACH BestKasse OF BestHode
    EXCLUSIVE-LOCK:
  DELETE BestKasse.
END.

FOR EACH VarebehBestLinje NO-LOCK
    OF VarebehBestHode:
     
  IF NUM-ENTRIES(VareBehBestLinje.Storl,"|") = 1 AND NOT VareBehBestLinje.Storl BEGINS "Sortiment" THEN DO:
    CREATE BestStr.
    ASSIGN BestStr.Bestilt         = VareBehBestLinje.Bestilt
           BestStr.BestNr          = BestHode.BestNr
           BestStr.BestStat        = BestHode.BestStat
           BestStr.Butik           = VareBehBestLinje.BestiltButikkNr
           BestStr.Storl           = TRIM(VareBehBestLinje.Storl)
  
           BestHode.TotAntPar      = BestHode.TotAntPar     + VareBehBestLinje.Bestilt
           BestHode.TotDbKr        = BestHode.TotDbKr       + VareBehBestLinje.Bestilt * BestPris.DBKr
           BestHode.TotInnkjVerdi  = BestHode.TotInnkjVerdi + VareBehBestLinje.Bestilt * BestPris.Varekost /* BestPris.InnkjopsPris */
           BestHode.TotSalgsVerdi  = BestHode.TotSalgsVerdi + VareBehBestLinje.Bestilt * BestPris.Pris
           .
  END.
  ELSE IF VareBehBestLinje.Storl BEGINS "Sortiment" THEN DO:
    ASSIGN cStrList      = ""
           cSortFordList = ""
           iSumAntSort   = 0
           .

    FOR EACH LevSort NO-LOCK
        WHERE LevSort.LevNr  = BestHode.LevNr
          AND LevSort.SortId = ENTRY(2,VareBehBestLinje.Storl,"|"):

      FOR EACH LevSAnt OF LevSort NO-LOCK BY SeqNr:
        ASSIGN cStrList      = cStrList + TRIM(LevSAnt.SoStorl) + " "
               cSortFordList = cSortFordList + STRING(LevSAnt.SoAnt) + " "
               iSumAntSort   = iSumAntSort + LevSAnt.SoAnt
               .
      END.

      cStrList = TRIM(cStrList," ").

      FIND FIRST BestSort NO-LOCK
           WHERE BestSort.BestNr = BestHode.BestNr
             AND BestSort.SortId = ENTRY(2,VareBehBestLinje.Storl,"|")
           NO-ERROR.
      IF NOT AVAIL BestSort THEN DO:
        CREATE BestSort.
        ASSIGN BestSort.Antall        = iSumAntSort * VareBehBestLinje.Bestilt
               BestSort.AntSort       = VareBehBestLinje.Bestilt
               BestSort.BestNr        = BestHode.BestNr
               BestSort.Fordeling     = TRIM(cSortFordList," ")
               BestSort.Fri           = FALSE
               BestSort.SortID        = ENTRY(2,VareBehBestLinje.Storl,"|")
               BestSort.Storrelser    = cStrList
               BestSort.StrInterval   = ENTRY(1,cStrList," ") + " - " + ENTRY(NUM-ENTRIES(cStrList," "),cStrList," ")
               .
        CREATE BestKasse.
        ASSIGN BestKasse.BestNr     = BestHode.BestNr
               BestKasse.Antal      = VareBehBestLinje.Bestilt
               BestKasse.Butik      = VareBehBestLinje.BestiltButikkNr
               BestKasse.SortId     = ENTRY(2,VareBehBestLinje.Storl,"|")
               .
      END.
    END.
  END.
END.
    
FIND FIRST BestSort OF BestHode
     WHERE BestSort.Fri NO-LOCK NO-ERROR.
IF NOT AVAIL BestSort THEN DO:
  ocReturn = "Feil i kreering av bestilling. Fritt sortiment ble ikke opprettet. " + PROGRAM-NAME(1).
  RETURN.
END.

FOR EACH Fributik OF BestHode
    EXCLUSIVE-LOCK:
  DELETE Fributik.
END.

FOR EACH BestStr OF BestHode NO-LOCK
    BREAK BY BestStr.Butik:
  IF NOT bCLfriBut AND FIRST-OF(BestStr.Butik) THEN DO:
    CREATE Fributik.
    ASSIGN Fributik.BestNr   = BestHode.BestNr
           Fributik.BestStat = BestHode.BestStat
           .
    IF BestHode.DirekteLev THEN
      FriButik.Butik = BestStr.Butik.
    ELSE
      ASSIGN FriButik.Butik = iiCL
             bCLfriBut      = TRUE.
  END.
  IF AVAIL FriButik THEN DO:
    iFriAntIx = LOOKUP(BestStr.Storl,BestSort.Storrelser," ") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN iFriAntIx = 1.
    ASSIGN Fributik.FriAntal[iFriAntIx] = BestStr.Bestilt
           fTotFriBut = fTotFriBut + BestStr.Bestilt NO-ERROR.
    /*
    IF ERROR-STATUS:ERROR THEN DO:
      ocReturn = "Feil i oppretting av frie størrelser for artikkel " 
               + STRING(BestHode.ArtikkelNr) + CHR(10) 
               + "BestSort.Storrelser: " + BestSort.Storrelser + CHR(10)
               + "BestStr.Storl: " + BestStr.Storl + CHR(10)
               + PROGRAM-NAME(1).
      UNDO, LEAVE.
    END.
    */
    IF LAST-OF(BestStr.Butik) THEN 
      ASSIGN Fributik.TotAntal = fTotFriBut
             fTotFriBut        = IF BestHode.DirekteLev THEN 0 ELSE fTotFriBut
             . 
  END.    
END.


ASSIGN VareBehBestHode.BestNr     = BestHode.BestNr
       VareBehBestHode.godkjent   = TRUE
       VareBehBestHode.LevDato    = BestHode.LevDato
       VareBehBestHode.OrdreNr    = Ordre.Ordrenr
       VareBehBestHode.LevNr      = BestHode.LevNr
       .
