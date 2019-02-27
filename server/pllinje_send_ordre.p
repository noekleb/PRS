/* Nullstill bekreftet antall for plukkliste-linje
   Parametere: Buffer for query
   
   Opprettet: 21.08.09 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR lEuKurs       AS DEC  NO-UNDO.
DEF VAR iCL           AS INT  NO-UNDO.
DEF VAR hQuery AS HANDLE NO-UNDO.
DEFINE VARIABLE lplListeId AS DECIMAL NO-UNDO.
DEFINE VARIABLE dVarebehnr AS DECIMAL NO-UNDO.
DEFINE VARIABLE iAntLinjer AS INTEGER NO-UNDO. 
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.
DEFINE VARIABLE iLogPartner   AS INTEGER   NO-UNDO.
DEFINE VARIABLE bOverfOrdre   AS LOG NO-UNDO.
DEFINE VARIABLE wEDB-System   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE wTabell       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE TT_OurOrdre     NO-UNDO LIKE Ordre.
DEFINE TEMP-TABLE TT_OurBestHode  NO-UNDO LIKE Besthode.
DEFINE TEMP-TABLE TT_OurBestLinje NO-UNDO LIKE BestLinje.
DEFINE TEMP-TABLE TT_OurBestPris  NO-UNDO LIKE BestPris.
DEFINE TEMP-TABLE TT_OurBestSort  NO-UNDO LIKE BestSort.
DEFINE TEMP-TABLE TT_OurBestKasse NO-UNDO LIKE BestKasse.
DEFINE TEMP-TABLE TT_OurBestStr   NO-UNDO LIKE BestStr.
DEFINE TEMP-TABLE TT_OurFributik  NO-UNDO LIKE Fributik.

{ttOrdre.i}
DEFINE TEMP-TABLE tt_BestLst 
    FIELD BestNr LIKE BestHode.BestNr.

DEF TEMP-TABLE tt_Ordre NO-UNDO LIKE ttOrdre.    

DEFINE NEW SHARED TEMP-TABLE TT_OvBuffer NO-UNDO LIKE OvBuffer.

DEFINE BUFFER clButiker FOR Butiker.

/* Kode for låsing av artikkelnummer ved overføring. */
{syspara.i 1 2 3 wEDB-System}
IF wEDB-System = "" THEN
  wEDB-System = "OVERFOR-LOCK".

/* Sentrallager */
{syspara.i 5 1 1 iCL INT}
FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCl NO-ERROR.
{syspara.i 2 1 1 lEuKurs DEC}
IF lEuKurs = 0 THEN lEuKurs = 1.

/* Skal det legges opp overf.ordre istedenfor suppl.ordre på kjedelevrte varer? */
{syspara.i 5 24 6 cTekst}
IF CAN-DO('1,J,Ja,Y,Yes,True',cTekst) THEN 
  bOverfOrdre = TRUE.
ELSE
  bOverfOrdre = FALSE.    
    
/* Lev.nr på logistikkpartner eller kjedens sentrallager som suppleringsordre for kjedeleverte varer skal legges på. */
{syspara.i 5 24 3 iLogPartner INT}
IF iLogPartner = 0 THEN iLogPartner = 38.

ASSIGN 
    cLogg = 'pllinje_send_ordre' + REPLACE(STRING(TODAY),'/','')
    .

/* Henter aktiv varebok type supplering. Den skal opprettes hvis den ikke finnes. */
RUN bibl_AktivSupplering.p (2, TRUE, OUTPUT dVareBehNr).
IF dVareBehNr = 0 OR dVarebehNr = ? THEN DO: 
  ocReturn = 'Det er ingen aktiv/åpen suppleringsordre (Varehåndteringsbok).' + chr(10) +
             'Generering av ordre avbrutt.'.
  obOk = ocReturn = "".
  RETURN.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

LOOPEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND PlListeLinje EXCLUSIVE-LOCK WHERE 
      ROWID(PlListeLinje) = TO-ROWID(STRING(ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE))
      NO-WAIT NO-ERROR.
  IF AVAIL PlListeLinje THEN DO:
    FIND plListeHode NO-LOCK OF plListeLinje NO-ERROR.
    IF AVAILABLE PlListeHode THEN
    DO:
        lplListeId = plListeLinje.plListeId.
        LEAVE LOOPEN.
    END.
  END.
  LEAVE LOOPEN.
END. /* LOOPEN */

FIND plListeHode NO-LOCK WHERE
  plListeHode.PlListeId = lplListeId NO-ERROR. 

IF NOT AVAILABLE plListeHode THEN 
DO:
    ocReturn = 'Ukjent ordre.'.
    obOk = ocReturn = "".
    RETURN.
END.

IF plListeHode.plListeStatus >= 30 THEN 
DO:
    ocReturn = 'Ordren er allerede sendt.'.
    obOk = ocReturn = "".
    RETURN.
END.

FIND FIRST PlListeLinje NO-LOCK WHERE 
    PlListeLinje.plListeId = lPlListeId AND
    PlListeLinje.AntallPlukket > 0
     NO-WAIT NO-ERROR.
IF NOT AVAILABLE PlListeLinje THEN
DO:
    ocReturn = 'Det er ingen linjer i plukklisten hvor antall bestillt > 0.' + chr(10) +
               'Sending er avbrutt.'.
    obOk = ocReturn = "".
    RETURN.
END.

RUN ByggTempTabell.       /* Oppretter tt_Ordre postene som brukes for å opprette ordre. */
RUN leggTilVarerIVarebok. /* Mangler varer i suppleringsboken, legg dem inn automatisk.  */

/* Oppretter overføringsordre på grunnlag av suppl.ordre til kjedens logistikkpartner. */
IF iLogPartner = PlListeHode.LevNr AND bOverfOrdre = TRUE THEN 
DO:
  RUN opprettOvOrdre. /* Linjer hvor OrdreNr = 0, og hvor EkstId er utfyllt.         */
END.
/* Oppretter ordre og bestilling på grunnlag av forhåndsordren til kjedens logistikkpartner. */
ELSE DO:
  RUN opprettNyeOrdre.      /* Linjer hvor OrdreNr = 0, og hvor EkstId er utfyllt.         */
  RUN KalkulerBestHode.     /* Oppdaterer summene i ordrehodet.                            */
END.

DO TRANSACTION:
    FIND CURRENT plListeHode EXCLUSIVE-LOCK.
    ASSIGN
      PlListeHode.OverfortDato  = TODAY
      plListeHode.plListeStatus = 30
      lplListeId                = plListeLinje.plListeId.
    RELEASE plListeHode.
END. /* TRANSACTION */

obOk = ocReturn = "".

/* **********************  Internal Procedures  *********************** */


PROCEDURE ByggTempTabell:

/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/

  DEF VAR piLinjeNr     AS INT  NO-UNDO.
  DEF VAR piAntFeil     AS INT  NO-UNDO.
  DEF VAR pcBkuFil      AS CHAR NO-UNDO.
  DEF VAR piLoop        AS INT  NO-UNDO.
  DEF VAR pcStorl       AS CHAR NO-UNDO.
  DEF VAR cDato         AS CHAR NO-UNDO.
  DEF VAR cTid          AS CHAR NO-UNDO.
  DEF VAR cTekst        AS CHAR NO-UNDO.
  DEF VAR iRecType      AS INT  NO-UNDO.
  DEF VAR iSeqNr        AS INT  NO-UNDO.
  DEF VAR iDummy        AS INT  NO-UNDO.
  DEF VAR iButik        AS INT  NO-UNDO.
  DEF VAR cEAN          AS CHAR NO-UNDO.

  DEFINE VARIABLE dTest AS DECIMAL    NO-UNDO.

  /* Tømmer temp-tabeller. */
  FOR EACH tt_Ordre:
      DELETE tt_Ordre.
  END.
  IF AVAILABLE ttOrdre THEN
      DELETE ttOrdre.

  ASSIGN
      piLinjeNr     = 1
      iAntLinjer    = 0
      .

hQuery:GET-FIRST().

LOOPEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FIND PlListeLinje EXCLUSIVE-LOCK 
       WHERE ROWID(PlListeLinje) = TO-ROWID(STRING(ihBuffer:BUFFER-FIELD("RowIdent1"):BUFFER-VALUE))
       NO-WAIT NO-ERROR.
       
  IF AVAIL PlListeLinje THEN 
  BLOKKEN:
  DO:
    IF plListeLinje.AntallPlukket = 0 THEN 
      LEAVE BLOKKEN.
    
    FIND plListeHode OF plListeLinje NO-LOCK NO-ERROR.
    FIND Butiker NO-LOCK WHERE
      Butiker.Butik = plListeHode.FraButikk NO-ERROR.
    IF AVAILABLE StrKonv THEN RELEASE StrKonv.
    IF AVAILABLE ttOrdre THEN DELETE ttOrdre.
    FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = plListeLinje.ArtikkelNr NO-ERROR.
    FIND ArtPris OF ArtBas NO-LOCK WHERE
      ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
    FIND ArtPris OF ArtBas NO-LOCK WHERE
      ArtPris.ProfilNr = clButiker.ProfilNr NO-ERROR.
    FIND StrKonv NO-LOCK WHERE
      StrKonv.StrKode = plListeLinje.StrKode NO-ERROR.
    FIND LAST Strekkode NO-LOCK OF ArtBas WHERE
      Strekkode.StrKode = plListeLinje.StrKode NO-ERROR.
    ASSIGN
        iAntLinjer = iAntLinjer + 1
        iSeqNr = iSeqNr + 1.
          /* Record buffer å lese inn filen i */
    CREATE ttOrdre.
    ASSIGN
        iRecType              = 1
        ttOrdre.RecType       = iRecType
        ttOrdre.LevNr         = ArtBas.LevNr
        ttOrdre.EkstId        = STRING(plListeLinje.PlListeId)
        ttOrdre.SendtDato     = TODAY  
        ttOrdre.OrdreNr       = 0
        ttOrdre.BestNr        = 0
        ttOrdre.ArtikkelNr    = ArtBas.ArtikkelNr
        ttOrdre.StrKode       = plListeLinje.StrKode
        ttOrdre.Beskr         = ArtBas.Beskr
        ttOrdre.LevDato       = TODAY + 3
        ttOrdre.ValPris       = ArtPris.ValPris[1]
        ttOrdre.InnkjopsPris  = ArtPris.InnkjopsPris[1]
        ttOrdre.Rab1Kr        = ArtPris.Rab1Kr[1]
        ttOrdre.Rab1%         = ArtPris.Rab1%[1]
        ttOrdre.Pris          = ArtPris.Pris[1]
        ttOrdre.Butik         = plListeHode.FraButikk
        ttOrdre.Storl         = IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE ''
        ttOrdre.Bestilt       = plListeLinje.AntallPlukket
        ttOrdre.Strekkode     = IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE ''
        ttOrdre.Varekost      = ArtPris.VareKost[1]
        ttOrdre.plListeId     = plListeLinje.plListeId
        ttOrdre.ProfilNr      = ArtPris.ProfilNr
        .

    ASSIGN
        ttOrdre.DirekteLev = TRUE
        ttOrdre.Storl      = TRIM(ttOrdre.Storl).

    /* Tar vare på ordrelinje/størrelse i temptabell */
    CREATE tt_Ordre.
    BUFFER-COPY ttOrdre TO tt_Ordre.
    
    DELETE ttOrdre.    
    RELEASE tt_Ordre.
  
  END. /* BLOKKEN */
  hQuery:GET-NEXT().
END. /* LOOPEN */

END PROCEDURE.

PROCEDURE KalkulerBestHode:

/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
KALKULER:
FOR EACH TT_BestLst TRANSACTION:
    FIND BestHode EXCLUSIVE-LOCK WHERE
        BestHode.BestNr = TT_BestLst.BestNr NO-ERROR.
    FIND FIRST BestPris OF BestHode NO-LOCK NO-ERROR.
    IF AVAILABLE BestHode THEN
    DO:
        /* Vi trenger ikke bry oss med de andre sumfeltene, da ordren har en status som */
        /* tilsier at de feltene ikker er berørt ennå.                                  */
        ASSIGN
            BestHode.TotInnKjVerdi = 0
            BestHode.TotDbKr       = 0
            BestHode.TotSalgsVerdi = 0
            BestHode.TotAntPar     = 0
            .
        /* Sumerer opp alle bestillingene */
        FOR EACH BestStr OF BestHode NO-LOCK:
            ASSIGN
                BestHode.TotAntPar = BestHode.TotAntPar + BestStr.Bestilt
                .
        END.
        /* Nye sumverdier. */
        ASSIGN 
            BestHode.TotInnKjVerdi = BestHode.TotAntPar * BestPris.Varekost
            BestHode.TotDbKr       = BestHode.TotAntPar * BestPris.DbKr
            BestHode.TotSalgsVerdi = BestHode.TotAntPar * BestPris.Pris
            .
        RELEASE BestHode.

    END.
END. /* KALKULEr */


END PROCEDURE.

PROCEDURE leggTilVarerIVarebok:

/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
  DEF VAR cRowId   AS CHAR  NO-UNDO.

  IF dVarebehnr = 0 THEN
      RETURN.

  /* Leser pakkseddellinjer */                           
  LEGGTILVRE:
  FOR EACH tt_Ordre 
    BREAK BY tt_Ordre.ArtikkelNr:

    IF FIRST-OF(tt_Ordre.ArtikkelNr) THEN
    LEGGTIL:
    DO:
        /* Legges til hvis den ikke finnes i suppleringsboken fra før */
        IF NOT CAN-FIND(VareBehLinje WHERE
                        VareBehLinje.VareBehNr  = dVarebehnr AND
                        VareBehLinje.ArtikkelNr = tt_Ordre.ArtikkelNr) THEN
        VAREBOKLINJE:
        DO:
            IF DYNAMIC-FUNCTION("runProc","varebehlinje_new.p",STRING(tt_Ordre.ArtikkelNr) + "|" + STRING(dVarebehnr)
                                ,?) THEN
              cRowId = DYNAMIC-FUNCTION("getTransactionMessage").
            ELSE DO:
              /*
              DYNAMIC-FUNCTION("DoMessage",0,0,
                               DYNAMIC-FUNCTION("getTransactionMessage"),"","").
              */
              RUN bibl_logg.p (cLogg,DYNAMIC-FUNCTION("getTransactionMessage")).              
              RETURN.
            END.
        END. /*VAREBOKLINJE */
    END. /* LEGGTIL */

  END. /* LEGGTILVRE */
END PROCEDURE.

PROCEDURE opprettNyeOrdre:

/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
  DEF VAR iOrdreNr AS INT FORMAT ">>>>>>>9" NO-UNDO.
  DEF VAR iBestNr  AS INT FORMAT ">>>>>>>9" NO-UNDO.
  DEF VAR iButik        AS INT  NO-UNDO.
  DEF VAR pcOpphav AS CHAR INITIAL "ERP" NO-UNDO.
  DEF VAR piLoop   AS INT NO-UNDO.

  DEFINE VARIABLE iHodeLinjeId AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLevUke AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iLookUp AS INTEGER    NO-UNDO.

  DEF BUFFER bufOrdre     FOR Ordre.
  DEF BUFFER bufBestHode  FOR BestHode.
  DEF BUFFER bufBestLinje FOR BestLinje.
  DEF BUFFER bufBestPris  FOR BestPris.
  DEF BUFFER bufBestSort  FOR BestSort.
  DEF BUFFER bufBestStr   FOR BestStr.
  DEF BUFFER bufVarebehBestHode FOR VarebehBestHode.

DO FOR bufBestHode, bufBestLinje, bufBestPris, bufBestSort, bufBestStr:
  FOR EACH tt_Ordre WHERE
      tt_Ordre.OrdreNr = 0
      BREAK BY tt_Ordre.OrdreNr
            BY tt_Ordre.Funnet 
            BY tt_Ordre.EkstId
            BY tt_Ordre.Butik
            BY tt_Ordre.LevNr
            BY tt_Ordre.ArtikkelNr
            BY tt_Ordre.LevDato
            BY tt_Ordre.Storl:

      ASSIGN piLoop = piLoop + 1.
      IF iButik <> tt_Ordre.butik THEN DO:
          iButik = tt_Ordre.butik.
          FIND butiker WHERE butiker.butik = iButik NO-LOCK.
          iCL = IF Butiker.clButikkNr = 0 THEN iButik ELSE Butiker.clButikkNr.
      END.
      IF FIRST-OF(tt_Ordre.LevNr) THEN
      OPPRETT_ORDRE:
      DO FOR bufOrdre:
          CREATE bufOrdre.
          ASSIGN /* Ordrenr. tildeles i trigger. */
              bufOrdre.OrdreStatus    = 4 /* Nye ordre kommer inn som bekreftet. */ 
              bufOrdre.Leveringsdato  = tt_Ordre.LevDato
              bufOrdre.CL             = iCL /* tt_ordre.butik  */
              bufOrdre.SendtDato      = bufOrdre.Leveringsdato
              bufOrdre.LevNr          = tt_Ordre.LevNr
              bufOrdre.BekreftetOrdre = TRUE
              bufOrdre.BekreftetDato  = TODAY
              bufORdre.BekreftetAv    = "Supplering"
/*               bufOrdre.EkstId         = tt_Ordre.EkstId */
              bufOrdre.fraERP         = TRUE
              bufOrdre.Varebehnr      = dVarebehnr
              bufOrdre.Opphav         = pcOpphav
              bufOrdre.EkstId         = tt_Ordre.EkstId
              bufOrdre.plListeId      = tt_Ordre.plListeId
              iOrdreNr                = bufOrdre.OrdreNr
              .
              
          RELEASE bufOrdre.
      END. /* OPPRETT_ORDRE */
      IF FIRST-OF(tt_ordre.LevDato) THEN
      OPPRETT_BESTILLING:
      DO:
          FIND Butiker NO-LOCK WHERE
              Butiker.Butik = tt_Ordre.Butik NO-ERROR.
          FIND ArtBas NO-LOCK WHERE
              ArtBas.ArtikkelNr = tt_Ordre.ArtikkelNr NO-ERROR.
          IF NOT AVAILABLE ArtBas THEN
              RETURN.
          FIND FIRST ArtPris OF ArtBas WHERE
              ArtPris.ProfilNr = tt_Ordre.ProfilNr NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ArtPris THEN 
            FIND FIRST ArtPris OF ArtBas NO-ERROR.
          FIND StrType NO-LOCK WHERE
              StrType.StrTypeId = ArtBAs.StrTypeId NO-ERROR.
          IF NOT AVAILABLE StrType THEN
              RETURN.
          FIND VarGr NO-LOCK WHERE
              VarGr.Vg = ArtBas.Vg NO-ERROR.
          FIND Moms NO-LOCK WHERE
              Moms.MomsKod = VarGr.MomsKod NO-ERROR.
          FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
          IF NOT AVAILABLE LevBas THEN
              RETURN.

          DO:
              CREATE bufBestHode.
              
              ASSIGN
                  bufBestHode.ArtikkelNr      = tt_Ordre.ArtikkelNr
                  bufBestHode.Beskrivelse     = tt_Ordre.Beskrivelse
                  bufBestHode.BestillingsDato = TODAY
                  bufBestHode.BestStat        = 4
                  bufBestHode.BestType        = 2 /* Supplering */
                  bufBestHode.CL              = iCL /* IF Butiker.clButikkNr = 0 THEN Butiker.Butik ELSE Butiker.clButikkNr */
                  bufBestHode.DirekteLev      = tt_Ordre.DirekteLev
                  bufBestHode.LevDato         = tt_Ordre.LevDato
                  bufBestHode.LevFargKod      = ArtBas.LevFargKod
                  bufBestHode.LevKod          = ArtBas.LevKod
                  bufBestHode.LevNr           = ArtBas.LevNr
           /*     bufBestHode.LevTid          = STRING(VareBehBestHode.Levuke) */
                  bufBestHode.Merknad         = tt_Ordre.Merknad
                  bufBestHode.OrdreNr         = iOrdreNr
                  bufBestHode.StrTypeID       = ArtBas.StrTypeId
                  bufBestHode.VareBehNr       = dVarebehnr
                  bufBestHode.BekreftetOrdre  = TRUE
                  bufBestHode.BekreftetDato   = TODAY
                  bufBestHode.BekreftetAv     = USERID("SkoTex")
                  bufBestHode.EkstId          = tt_Ordre.EkstId
                  bufBestHode.Opphav          = pcOpphav
                  iBestNr                     = bufBestHode.BestNr
                  /* Setter på peker til ordre/bestilling */
                  tt_Ordre.OrdreNr       = IF tt_Ordre.OrdreNr = 0
                                             THEN bufBestHode.OrdreNr 
                                             ELSE tt_Ordre.OrdreNr
                  tt_Ordre.BestNr        = IF tt_Ordre.BestNr = 0
                                                  THEN bufBestHode.BestNr 
                                                  ELSE tt_Ordre.BestNr
                  .
          END.

          /* Logger bestilling */
          IF NOT CAN-FIND(tt_BestLst WHERE 
                          tt_BestLst.BestNr = bufBestHode.BestNr) THEN
          DO:
              CREATE tt_BestLst.
              ASSIGN tt_BestLst.BestNr = bufBestHode.BestNr.
          END.

          DO:
              CREATE bufBestPris.
              ASSIGN
                  bufBestPris.Pris          = tt_Ordre.Pris  
                  bufBestPris.EuroPris      = ROUND(tt_Ordre.Pris * lEuKurs,2)
                  /* bufBestPris.MvaKr         = ROUND(tt_Ordre.Pris * Moms.MomsProc / 100,2) */
                  bufBestPris.MvaKr         = tt_Ordre.Pris * (Moms.MomsProc / (100 + Moms.MomsProc))
                  bufBestPris.Mva%          = Moms.MomsProc         
                  bufBestPris.InnkjopsPris  = tt_Ordre.InnkjopsPris 
                  bufBestPris.ValPris       = tt_Ordre.ValPris
                /*          BestPris.Frakt         = VarebehLinje.Frakt  */
                /*          BestPris.Frakt%        = VarebehLinje.Frakt% */
                  bufBestPris.Rab1Kr        = tt_Ordre.Rab1Kr      
                  bufBestPris.Rab1%         = tt_Ordre.Rab1%        
                /*          BestPris.Rab2Kr        = VarebehLinje.InnkjopsPris * VarebehLinje.supRab% / 100 */
                /*          BestPris.Rab2%         = VarebehLinje.supRab%                                   */
                /*          BestPris.Rab3Kr        = VarebehLinje.Rab3Kr */
                /*          BestPris.Rab3%         = VarebehLinje.Rab3%  */
                  bufBestPris.DB%           = tt_Ordre.Db%          
                  bufBestPris.DBKr          = tt_Ordre.DbKr         
                /*          BestPris.DivKost%      = VarebehLinje.DivKost%  */
                /*          BestPris.DivKostKr     = VarebehLinje.DivKostKr */
                  bufBestPris.VareKost      = tt_Ordre.Innkjopspris - bufBestPris.Rab1Kr     
                  bufBestPris.ProfilNr      = tt_Ordre.ProfilNr        
                  bufBestPris.ArtikkelNr    = tt_Ordre.ArtikkelNr
                  bufBestPris.BestNr        = bufBestHode.BestNr
                  bufBestPris.BestStat      = bufBestHode.BestStat
                  bufBestPris.Varekost      = bufBestPris.InnkjopsPris - bufBestPris.Rab1Kr
                  .
              /* Setter inn varekost fra kalkylen hvis den ikke er oppgitt. */
              ASSIGN
                  bufBestPris.InnkjopsPris  = IF bufBestPris.InnkjopsPris = 0 THEN ArtPris.Innkjopspris[1] ELSE bufBestPris.Innkjopspris
                  bufBestPris.ValPris       = IF bufBestPris.ValPris      = 0 THEN ArtPris.ValPris[1] ELSE bufBestPris.ValPris
                  bufBestPris.VareKost      = IF bufBestPris.VareKost     = 0 THEN ArtPris.VareKost[1] ELSE bufBestPris.VareKost
                  bufBestPris.DBKr          = bufBestPris.Pris - bufBestPris.MvaKr - bufBestPris.Varekost
                  bufBestPris.Db%           = (bufBestPris.DBKr / (bufBestPris.Pris - bufBestPris.MvaKr)) * 100
                  bufBestPris.Db%           = IF bufBestPris.Db% = ? THEN 0 ELSE bufBestPris.Db%
                  .
          END.
          
          DO:
              CREATE bufBestSort.
              ASSIGN bufBestSort.Antall        = 0
                     bufBestSort.AntSort       = 0
                     bufBestSort.BestNr        = bufBestHode.BestNr
                     bufBestSort.Fordeling     = ""
                     bufBestSort.Fri           = TRUE
                     bufBestSort.SortID        = "FRI"
                     bufBestSort.Storrelser    = TRIM(REPLACE(REPLACE(REPLACE(StrType.AlfaFordeling,","," "),"  "," "),"   "," "))
                     bufBestSort.StrInterval   = ""
                     .
          END.

          RUN weeknum.p (INPUT bufBestHode.LevDato, OUTPUT iLevUke).

          FIND LAST bufVarebehBestHode WHERE bufVarebehBestHode.Varebehnr  = dVareBehnr AND
                                             bufVarebehBestHode.CLButikkNr = iCL NO-LOCK NO-ERROR.

          iHodeLinjeId = IF AVAIL bufVarebehBestHode THEN bufVarebehBestHode.HodeLinjeId + 1 ELSE 1.
          DO:
              CREATE VarebehBestHode.                                                      
              ASSIGN VarebehBestHode.VareBehNr         = dVareBehnr
                     VarebehBestHode.CLButikkNr        = iCL
                     VarebehBestHode.HodeLinjeId       = iHodeLinjeId
                     VarebehBestHode.ArtikkelNr        = bufBestHode.ArtikkelNr
                     VarebehBestHode.LevDato           = bufBestHode.LevDato
                     VarebehBestHode.Levuke            = iLevUke
                     VarebehBestHode.godkjent          = TRUE
                     VarebehBestHode.DirekteLev        = TRUE
                     VarebehBestHode.BestNr            = bufBestHode.BestNr
                     VarebehBestHode.levnr             = bufBestHode.LevNr
    /*                  Storrelser                      char               X(33) */
    /*                  Butikkliste       = IF */
                     VarebehBestHode.OrdreNr           = iOrdreNr
                     VarebehBestHode.AlfaFordeling     = StrType.Alfafordeling.
                     .
          END.
      END. /* OPPRETT_BESTILLING */
      IF NOT CAN-FIND(FIRST bufBestLinje WHERE bufBestLinje.BestNr = iBestNr AND
                                               bufBestLinje.Butik  = tt_Ordre.Butik) THEN 
      DO:
          CREATE bufBestLinje.
          ASSIGN
              bufBestLinje.BestNr = iBestNr
              bufBestLinje.Butik  = tt_Ordre.Butik
              .
          RELEASE bufBestLinje.
      END. /* OPPRETT_BESTLINJE */

      /* Tar vare på de nye nummerne */
      ASSIGN
          tt_Ordre.OrdreNr = iOrdreNr
          tt_Ordre.BestNr  = iBestNr
          .

      DO:
          FIND bufBestStr WHERE bufBestStr.BestNr          = bufBestHode.BestNr   AND
                                bufBestStr.BestStat        = bufBestHode.BestStat AND
                                bufBestStr.Butik           = tt_Ordre.Butik       AND
                                bufBestStr.Storl           = TRIM(tt_Ordre.Storl) NO-ERROR.
          IF NOT AVAIL bufBestStr THEN DO:
              CREATE bufBestStr.
              ASSIGN bufBestStr.BestNr          = bufBestHode.BestNr
                     bufBestStr.BestStat        = bufBestHode.BestStat
                     bufBestStr.Butik           = tt_Ordre.Butik
                     bufBestStr.Storl           = TRIM(tt_Ordre.Storl).
          END.
          ASSIGN bufBestStr.Bestilt         = bufBestStr.Bestilt + tt_Ordre.Bestilt

                 bufBestHode.TotAntPar      = bufBestHode.TotAntPar     + bufBestStr.Bestilt
                 bufBestHode.TotDbKr        = bufBestHode.TotDbKr       + bufBestStr.Bestilt * bufBestPris.DBKr
                 bufBestHode.TotInnkjVerdi  = bufBestHode.TotInnkjVerdi + bufBestStr.Bestilt * bufBestPris.Varekost /* BestPris.InnkjopsPris */
                 bufBestHode.TotSalgsVerdi  = bufBestHode.TotSalgsVerdi + bufBestStr.Bestilt * bufBestPris.Pris
                 .

           /* Finnes ikke størrelsen, skal den legges til i listen. */
           IF NOT CAN-DO(REPLACE(bufBestSort.Storrelser,' ',','),TRIM(bufBestStr.Storl)) THEN
               bufBestSort.Storrelser = bufBestSort.Storrelser + " " + TRIM(bufBestStr.Storl).
           IF LAST-OF(tt_ordre.LevDato) THEN DO:
               FOR EACH TT_OurFributik:
                   CREATE Fributik.
                   BUFFER-COPY TT_OurFributik TO Fributik NO-ERROR.
                   IF ERROR-STATUS:ERROR THEN
                       DELETE Fributik.
                   DELETE TT_OurFriButik.
               END.
           END.
      END. /* */

  END.
  IF AVAILABLE bufBestSort THEN RELEASE bufBestSort.
  IF AVAILABLE bufBestPris THEN RELEASE bufBestPris.
  IF AVAILABLE bufBestHode THEN RELEASE bufBestHode.
  IF AVAILABLE bufBestStr  THEN RELEASE bufBestStr.
  IF AVAILABLE bufBestSort THEN RELEASE bufBestSort.
END.


END PROCEDURE.

PROCEDURE opprettOvOrdre:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE pcTTId    AS CHAR NO-UNDO.
  DEFINE VARIABLE piLinjeNr AS INT  NO-UNDO.
  DEFINE VARIABLE iBuntNr   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iBatchNr  AS INT NO-UNDO.

  DEF BUFFER ovButiker FOR Butiker.
                 
  /* Tømmer */
  FOR EACH TT_OvBuffer:
      DELETE TT_OvBuffer. /* ev metod empty-temp-table */
  END.
  ASSIGN
      piLinjeNr = 1
      pcTTId    = "006"
      .

  /* Logger overføringer. */
  TRANSRAD:
  FOR EACH tt_Ordre WHERE
      BREAK BY tt_Ordre.OrdreNr
            BY tt_Ordre.Funnet 
            BY tt_Ordre.EkstId
            BY tt_Ordre.Butik
            BY tt_Ordre.LevNr
            BY tt_Ordre.ArtikkelNr
            BY tt_Ordre.LevDato
            BY tt_Ordre.Storl:
  
      /* Henter Artikkel */
      FIND ArtBas NO-LOCK WHERE
          ArtBas.ArtikkelNr = DECIMAL(tt_Ordre.ArtikkelNr) NO-ERROR.
      IF NOT AVAILABLE ArtBas THEN
          NEXT TRANSRAD.

      /* TN 28/10-01 Valutapris ikke omregnet legges inn her. Ref. Gøran hos JF. */
      IF AVAILABLE ArtBas THEN
        FIND ArtPris NO-LOCK WHERE
          Artpris.ArtikkelNr = ArtBas.ArtikkelNr AND
          ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.

      /* Henter butikk det overføres til. */
      FIND ovButiker NO-LOCK WHERE
          ovButiker.Butik = INT(PlListeHode.FraButikkNr) NO-ERROR.
  
      /* Logger overføringstransaksjonen */
      CREATE TT_OvBuffer.
      ASSIGN TT_OvBuffer.BuntNr      = 999 /* dummy, kan vara vad som helst */
             TT_OvBuffer.LinjeNr     = piLinjeNr
             TT_OvBuffer.ArtikkelNr  = DECIMAL(tt_Ordre.ArtikkelNr)
             TT_OvBuffer.Vg          = ArtBas.Vg   
             TT_OvBuffer.LopNr       = ArtBas.LopNr
             TT_OvBuffer.Antall      = tt_Ordre.Bestilt
             TT_OvBuffer.Merknad     = "Suppl.ordre: " + STRING(PlListeHode.PlListeId)
             TT_OvBuffer.Storl       = tt_Ordre.Storl
             TT_OvBuffer.TilStorl    = tt_Ordre.Storl
             TT_OvBuffer.Varekost    = tt_Ordre.Varekost
             piLinjeNr               = piLinjeNr + 1
             /* Setter datoinfo i registrert dato og tid. */
             TT_OvBuffer.RegistrertDato = TODAY 
             TT_OvBuffer.RegistrertTid  = TIME 
             TT_OvBuffer.RegistrertAv   = USERID("SkoTex")
             .
      ASSIGN
          TT_OvBuffer.ButikkNrFra = iCl
          TT_OvBuffer.ButikkNrTil = PlListeHode.FraButikkNr        
          .
  END. /* TRANSRAD. */

  IF CAN-FIND(FIRST TT_OvBuffer) THEN DO:
      ASSIGN iBuntNr = 0.
      RUN LagraOvBuffer.p (INPUT-OUTPUT iBuntNr,
                           0,
                           'J' + CHR(1) + 'Supl.ordre: ' + PlListeHode.PlNavn + ' ' + STRING(TODAY) + STRING(TIME,"HH:MM"),
                           wEDB-System,
                           wTabell,
                           6).
      EMPTY TEMP-TABLE TT_OvBuffer NO-ERROR.
  END.
END PROCEDURE.

/* ************************  Function Implementations ***************** */



