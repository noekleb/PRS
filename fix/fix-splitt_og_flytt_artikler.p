DEFINE    VARIABLE      iantRec       AS INTEGER       NO-UNDO.
DEFINE    VARIABLE      iAnt2Rec      AS INTEGER       NO-UNDO.
DEFINE    VARIABLE      iAntKode      AS INTEGER       NO-UNDO.
DEFINE    VARIABLE      cVmLst        AS CHARACTER      NO-UNDO.
DEFINE    VARIABLE      iLoop1        AS INTEGER       NO-UNDO.
DEFINE    VARIABLE      iLoop2        AS INTEGER       NO-UNDO.
DEFINE    VARIABLE      cHovedKatLst  AS CHARACTER      NO-UNDO.
DEFINE    VARIABLE      iVmId         AS INTEGER       NO-UNDO.
DEFINE    VARIABLE      iHovedKatNr   AS INTEGER       NO-UNDO.
DEFINE    VARIABLE      cStrKodeLst   AS CHARACTER      FORMAT "x(30)" NO-UNDO.
DEFINE    VARIABLE      cStrLst       AS CHARACTER      FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE cSjekkStrKode AS CHARACTER NO-UNDO.
DEFINE VARIABLE lArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
DEFINE VARIABLE bOk AS LOG NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRens AS CHARACTER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

FORM
  WITH FRAME A WIDTH 350 DOWN.

ASSIGN 
  cLogg         = 'fix-splitt_og_flytt_artikler' + REPLACE(STRING(TODAY),'/','')
  cVmLst        = '90,103'
  cHovedKatLst  = '1010'    
  iantRec       = 0
  cSjekkStrKode = '832,1089,1092' /* 50x70, 70x100, 80x80 */
  cRens         = '50x70,70x100,80x80'
  .

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.' 
    ).    


VAREMERKE:
DO iLoop1 = 1 TO NUM-ENTRIES(cVmLst):
  iVmId = INT(ENTRY(iLoop1,cVmLst)).
    
  HOVEDKATEGORI:
  DO iLoop2 = 1 TO NUM-ENTRIES(cHovedKatlst):
    iHovedKatNr = INT(ENTRY(iLoop2,cHovedKatLst)).

    ARTIKKEL:
    FOR EACH ArtBas NO-LOCK WHERE
      ArtBas.VmId = iVmId AND 
      ArtBas.HovedKatNr = iHovedKatNr:
            
      ASSIGN 
        iAntKode    = 0
        cStrKodeLst = ''
        cStrLst     = ''
        .
      STREKKODE:
      FOR EACH Strekkode OF ArtBas NO-LOCK WHERE 
        CAN-DO(cSjekkStrKode,STRING(Strekkode.StrKode)):
        FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.
                  
        ASSIGN 
          iAntKode    = iantKode + 1
          cStrKodeLst = cStrKodeLst + 
                                (IF cStrKodeLst = '' THEN '' ELSE ',') + 
                                STRING(Strekkode.StrKode)
          .

        IF AVAILABLE StrKonv THEN 
          cStrLst = cStrLst + 
            (IF cStrLst = '' THEN '' ELSE ',') + 
            (IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '').
      END. /* STREKKODE */
            
      ASSIGN 
        iAntRec = iAntRec + 1.
      .

      IF iAntKode > 1 THEN
      TARGET:
      DO:
        ASSIGN 
          iant2Rec = iAnt2Rec + (IF iAntKode > 0 THEN 1 ELSE 0)
          .
/*        /* TEST TEST */     */
/*        IF iAnt2Rec > 5 THEN*/
/*          LEAVE VAREMERKE.  */

        lArtikkelNr = 0.
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  splittArtikkel start.' 
            ).    
        RUN splittArtikkel (ArtBas.ArtikkelNr, OUTPUT lArtikkelNr, OUTPUT bOk).
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  splittArtikkel ferdig: ' + STRING(bOk) + '.' 
            ).    
        
        DISPLAY
          iVmId
          iHovedKatNr
          iAntRec
          iant2Rec
          ArtBas.ArtikkelNr
          ArtBas.Beskr
          ArtBas.LevKod
          ArtBas.LevFargKod
          cStrKodeLst
          cStrLst
          lArtikkelNr
          WITH FRAME A WIDTH 350 DOWN.
        DOWN WITH FRAME A.
        
      END. /* TARGET */
    END. /* ARTIKKEL */
  END. /* HOVEDKATEGORI */
END. /* VAREMERKE */

/* Dette gjøres på størrelses nivå, da bare de linjene som er lagt på nye artikler skal berøres.       */
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  fiksPkSdlLinjer start.' 
    ).    
RUN fiksPkSdlLinjer.
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  fiksPkSdlLinjer slutt.' 
    ).    

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Slutt.' 
    ).    


/* **********************  Internal Procedures  *********************** */

PROCEDURE splittArtikkel:
  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER plArtikkelNr   LIKE ArtBas.ArtikkelNr NO-UNDO.
  DEFINE OUTPUT PARAMETER plUtArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
  DEFINE OUTPUT PARAMETER pbOk           AS LOG                 NO-UNDO.

  DEFINE VARIABLE piAntKode AS INTEGER NO-UNDO.
  
  DEFINE BUFFER bufArtBas FOR ArtBas.
  
  FIND bufArtBas NO-LOCK WHERE 
    bufArtBas.ArtikkelNr = plArtikkelNr NO-ERROR.

  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    Artikkel: ' + STRING(bufArtBas.ArtikkelNr) + ' ' + 
                       bufArtBas.LevKod + ' ' + 
                       bufArtBas.LevFargKod
      ).    
    
  STREKKODELOOP:
  FOR EACH Strekkode OF bufArtBas EXCLUSIVE-LOCK WHERE 
    CAN-DO(cSjekkStrKode,STRING(Strekkode.StrKode)):
    FIND StrKonv OF Strekkode NO-LOCK NO-ERROR.

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '      Strekkode: ' + Strekkode.Kode + ' ' + StrKonv.Storl + '.'
        ).    
    
    /* Den første strekkoden skal bli liggende. De øvrige skal flyttes til nye artikler. */
    piAntKode = piAntKode + 1.
    IF piantKode = 1 THEN
    DO:
      /* Ser til at størrelsen ligger i varetekstn. */
      RUN sjekkBeskr (bufArtBas.ArtikkelNr, StrKonv.Storl, OUTPUT pbOk). 
      NEXT STREKKODELOOP.
    END.
    
    IF AVAILABLE bufArtBas THEN 
    DO TRANSACTION:
      FIND CURRENT bufArtBas EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF AVAILABLE bufArtBas AND NOT LOCKED bufArtBas THEN 
      DO:
        /* Flagger at posten er berørt ved å sette VPIDato på artikkelen. */
        ASSIGN 
          bufArtBas.VPIDato = TODAY
          .
        /* Henter et artikkelNr. */
        RUN forsteLedigeNr(OUTPUT plUtArtikkelNr).
        IF NOT CAN-FIND(FIRST ArtBas WHERE 
                        ArtBas.ArtikkelNr = plUtArtikkelNr) THEN 
        DO:
          RUN kopierArtikkel (bufArtBas.ArtikkelNr, plUtArtikkelNr, OUTPUT pbOk).          
          rStandardFunksjoner:SkrivTilLogg(cLogg,
              '      kopierArtikkel: ' + STRING(pbOk) + '.'
              ).    
          /* Flytter strekkoden og resten av artikkelens data. */
          IF pbOk THEN
          DO: 
            ASSIGN 
              Strekkode.ArtikkelNr = plUtArtikkelNr
              .
            /* Ser til at størrelsen ligger i varetekstn. */
            RUN sjekkBeskr (plUtArtikkelNr, StrKonv.Storl, OUTPUT pbOk). 
            
            /* Kopierer alle priser på artikkelen til den nye artikkelen. */  
            RUN kopierPriser (bufArtBas.ArtikkelNr, plUtArtikkelNr, OUTPUT pbOk).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '      kopierPriser: ' + STRING(pbOk) + '.'
                ).    
            /* Flytter alle transaksjoner som er gjort i den aktuelle størrelsen til ny artikkel. */
            RUN flyttTranslogg (bufArtBas.ArtikkelNr, plUtArtikkelNr, StrKonv.Storl, OUTPUT pbOk).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '      flyttTranslogg: ' + STRING(pbOk) + '.'
                ).    
            /* Kopierer de lagerposter og flytter de artlags poster som trengs på den nye artikkelen. */ 
            /* Sletter også lagerposter som ikke lenger har artlag under seg.                         */
            RUN flyttLager (bufArtBas.ArtikkelNr, plUtArtikkelNr, StrKonv.Storl, OUTPUT pbOk).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '      flyttLager: ' + STRING(pbOk) + '.'
                ).    
            /* Sletter lagerposter som ikke lenger har data under seg, og gjennoppbygger de som nå bare holder data for en støørrelse. */            
            RUN fiksLager(bufArtBas.ArtikkelNr, plUtArtikkelNr, OUTPUT pbOk).
            rStandardFunksjoner:SkrivTilLogg(cLogg,
                '      fiksLager: ' + STRING(pbOk) + '.'
                ).    
          END.
        END.
        RELEASE bufArtBas.
      END.
    END. /* TRANSACTION */
  END. /* STREKKODELOOP */
    
  RETURN.

END PROCEDURE.

PROCEDURE forsteLedigeNr:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER plArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
  
  DEFINE VARIABLE plLoop AS DECIMAL NO-UNDO.
  
  DEFINE BUFFER nextArtBas FOR ArtBas.

  EVIGHETEN:
  DO plLoop = 10000 TO 9999999999999:
    IF NOT CAN-FIND(nextArtBas WHERE 
                    nextArtBas.ArtikkelNr = plLoop) THEN 
    DO:
      plArtikkelNr = plLoop.
      LEAVE EVIGHETEN.
    END.
  END. /* EVIGHETEN */
  
END PROCEDURE.

PROCEDURE flyttLager:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pilFraArtikkelNr LIKE artbas.ArtikkelNr NO-UNDO.
  DEFINE INPUT PARAMETER pilTilArtikkelNr LIKE artbas.ArtikkelNr NO-UNDO.
  DEFINE INPUT PARAMETER pcStorl AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO. 

  DEFINE BUFFER bufLager FOR Lager.
  
  /* Flytter ArtLag postene på størrelsen. */
  FOR EACH ArtLag EXCLUSIVE-LOCK WHERE 
    ArtLag.ArtikkelNr = pilFraArtikkelNr AND 
    ArtLag.Storl      = pcStorl:
      
    ASSIGN
      ArtLag.ArtikkelNr = pilTilArtikkelNr 
      pbOk              = TRUE
      .   
  END.
  
  /* Kopierer lagerposter som mangler.                          */
  /* Tar bort lagerposter som ikke lenger har ArtLag under seg. */
  FOR EACH Lager EXCLUSIVE-LOCK WHERE 
    Lager.ArtikkelNr = pilFraArtikkelNr:
    IF NOT CAN-FIND(bufLager WHERE 
                    bufLager.ArtikkelNr = pilTilArtikkelNr AND 
                    bufLager.Butik      = Lager.Butik) THEN 
    DO:
      CREATE bufLager.
      BUFFER-COPY Lager
        EXCEPT ArtikkelNr 
        TO bufLager
        ASSIGN 
          bufLager.ArtikkelNr = pilTilArtikkelNr
          .
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '      Flytter lager: ' + STRING(bufLager.ArtikkelNr) + '  ' + 
                                    STRING(bufLager.Butik)+ '.' 
          ).    
    END.  
    IF NOT CAN-FIND (FIRST ArtLag WHERE 
                     ArtLag.ArtikkelNr = Lager.ArtikkelNr AND 
                     ArtLag.Butik      = Lager.butik) THEN
    DO: 
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '      Sletter lager: ' + STRING(Lager.ArtikkelNr) + '  ' + 
                                    STRING(Lager.Butik)+ '.' 
          ).    
      DELETE Lager.
    END.
  END.

  RETURN.

END PROCEDURE.

PROCEDURE fiksPkSdlLinjer:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  DEFINE BUFFER bufPkSdlLinje FOR PkSdlLinje.
  DEFINE BUFFER bufPkSdlPris FOR PkSdlLinje.
  
  DEFINE VARIABLE plArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
  
  PKSDLLOOP:
  FOR EACH PkSDlHode NO-LOCK WHERE 
    PkSdlHode.PkSdlStatus = 10, 
    EACH PkSdlLinje OF PksdlHode NO-LOCK:
      
      ASSIGN 
        plArtikkelNr = PkSdlLinje.ArtikkelNr
        .
      
      FIND Strekkode NO-LOCK WHERE 
        Strekkode.Kode = PkSdlLinje.Kode NO-ERROR.
      IF AVAILABLE Strekkode AND 
        Strekkode.ArtikkelNr <> PkSdlLinje.ArtikkelNr THEN
      STREKKODE: 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
            '    PkSdl: ' + PkSdlHode.PkSdlNr +  ' Linje: ' + STRING(PkSdlLinje.PkSdlLinje) + ' Endret artikkelnr fra ' +  STRING(PkSdlLinje.ArtikkelNr ) + ' til ' + STRING(Strekkode.ArtikkelNr) + '.'
            ).    
        
        FIND bufPkSdlLinje EXCLUSIVE-LOCK WHERE 
          RECID(bufPkSdlLinje) = RECID(PkSdlLinje) NO-ERROR.
        /* Flytter PkSdlLinjen */
        ASSIGN 
          bufPkSdlLinje.ArtikkelNr = Strekkode.ArtikkelNr
          .
        /* Kopierer pris hvis det ikke ligger pris på artikke på pakkseddelen. */
        IF NOT CAN-FIND(PkSdlPris WHERE 
                        PkSdlPris.PkSdlId    = PkSdlHode.PkSdlId AND 
                        PkSdlPris.ArtikkelNr = Strekkode.ArtikkelNr) THEN 
        PRISBLOKK:
        DO:
          FIND PkSdlPris NO-LOCK WHERE 
            PkSdlPris.PkSdlId    = PkSdlHode.PkSdlId AND 
            PkSdlPris.ArtikkelNr = plArtikkelNr NO-ERROR.
          IF AVAILABLE PkSdlPris THEN 
          DO:
            CREATE bufPkSdlPris.
            BUFFER-COPY PkSdlPris
              EXCEPT ArtikkelNr
              TO bufPkSdlPris
              ASSIGN 
                BufPkSdlPris.ArtikkelNr = Strekkode.ArtikkelNr
                .
          END.
        END. /* PRISBLOKK */
      END. /* STREKKODE */    
  END. /* PKSDLLOOP */

END PROCEDURE.

PROCEDURE kopierPriser:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pilFraArtikkelNr LIKE artbas.ArtikkelNr NO-UNDO.
  DEFINE INPUT PARAMETER pilTilArtikkelNr LIKE artbas.ArtikkelNr NO-UNDO.
  DEFINE OUTPUT PARAMETER pibOk AS LOG NO-UNDO. 
   
  DEFINE BUFFER bufArtPris FOR ArtPris.  
  DEFINE BUFFER buf2ArtPris FOR ArtPris.  
   
  FOR EACH bufArtPris EXCLUSIVE-LOCK WHERE 
    bufArtPris.ArtikkelNr = pilFraArtikkelNr:
      
    IF NOT CAN-FIND(buf2ArtPris WHERE 
                    buf2ArtPris.ArtikkelNr = pilTilArtikkelNr AND 
                    buf2ArtPris.ProfilNr   = bufArtPris.ProfilNr) THEN 
    DO:  
      CREATE buf2ArtPris.
      BUFFER-COPY 
        bufArtPris
        EXCEPT ArtikkelNr
        TO buf2ArtPris
        ASSIGN 
          buf2ArtPris.ArtikkelNr = pilTilArtikkelNr
          pibOk                  = TRUE
          .
      rStandardFunksjoner:SkrivTilLogg(cLogg,
          '      Ny prislinje: ' + STRING(buf2ArtPris.ArtikkelNr) + ' profilnr: ' + STRING(buf2ArtPris.ProfilNr) + '.'
          ).    
    END.  
  END.
  
  RETURN.
  
END PROCEDURE.

PROCEDURE kopierArtikkel:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pilFraArtikkelNr LIKE artbas.ArtikkelNr NO-UNDO.
  DEFINE INPUT PARAMETER pilTilArtikkelNr LIKE artbas.ArtikkelNr NO-UNDO.
  DEFINE OUTPUT PARAMETER pibOk AS LOG NO-UNDO. 

  DEFINE VARIABLE piLopNr AS INTEGER NO-UNDO.
  
  DEFINE BUFFER bufArtBas FOR ArtBas.
  DEFINE BUFFER buf2ArtBas FOR ArtBas.
  
  FIND bufArtBas NO-LOCK WHERE 
    bufArtBas.ArtikkelNr = pilFraArtikkelNr NO-ERROR.
  IF AVAILABLE bufArtBas THEN 
  DO TRANSACTION:
    CREATE buf2ArtBas.
    BUFFER-COPY bufArtBas 
      EXCEPT ArtikkelNr VPIDato LopNr
      TO buf2ArtBas
      ASSIGN 
        buf2ArtBas.ArtikkelNr = pilTilArtikkelNr
        buf2ArtBas.LopNr      = ?
        buf2ArtBas.VPIDato    = TODAY
        pibOk                 = TRUE
        .
    piLopNr = 0.
    RUN SettLopNr.p (buf2artbas.vg,"F",OUTPUT piLopNr).
    ASSIGN 
      buf2ArtBas.LopNr = piLopnr NO-ERROR.

    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '      Kopiert artikkel: ' + STRING(bufArtBas.ArtikkelNr) + ' til: ' + STRING(buf2ArtBas.ArtikkelNr) + '.'
        ).    
        
    RELEASE buf2ArtBas.
  END. /* TRANSACTION */
  
END PROCEDURE.

PROCEDURE flyttTranslogg:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pilFraArtikkelNr LIKE artbas.ArtikkelNr NO-UNDO.
  DEFINE INPUT PARAMETER pilTilArtikkelNr LIKE artbas.ArtikkelNr NO-UNDO.
  DEFINE INPUT PARAMETER pcStorl AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO.
  
    /* Flytter TransLogg */
  FOR EACH TransLogg EXCLUSIVE-LOCK WHERE 
    TransLogg.ArtikkelNr = pilFraArtikkelNr AND 
    TransLogg.Storl      = pcStorl USE-INDEX OppslagStr:
    ASSIGN 
      Translogg.ArtikkelNr = pilTilArtikkelNr
      pbOk = TRUE
      .  
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '      Flytter translogg: ' + STRING(Translogg.ArtikkelNr) + '  TransloggIdx: ' + 
                                      STRING(Translogg.Butik) + ' ' + 
                                      STRING(Translogg.TransNr) + ' ' + 
                                      STRING(Translogg.SeqNr) + '.' 
        ).    
  END.
  
  RETURN.
END PROCEDURE.

PROCEDURE fiksLager:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pilFraArtikkelNr LIKE artbas.ArtikkelNr NO-UNDO.
  DEFINE INPUT PARAMETER pilTilArtikkelNr LIKE artbas.ArtikkelNr NO-UNDO.
  DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO.

  RUN korrigerArtlag_fra_translogg.p (pilFraArtikkelNr).
  RUN korrigerArtlag_fra_translogg.p (pilTilArtikkelNr).
  
  pbOk = TRUE.
  RETURN.
  
END PROCEDURE.

PROCEDURE sjekkBeskr:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: cStrKodeLst inneholder en liste over denne artikkelens størrelser.
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pilArtikkelNr LIKE artbas.ArtikkelNr NO-UNDO.
  DEFINE INPUT PARAMETER pcStorl AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER pbOk AS LOG NO-UNDO.

  DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
  
  DEFINE BUFFER lokArtBas FOR artBas.
  
  FIND lokArtBas NO-LOCK WHERE 
    lokArtBas.ArtikkelNr = pilArtikkelNr NO-ERROR.
  /* Sjekker om noen av størrelsene allerede ligger i varetekstn */
  SJEKKEN:
  DO TRANSACTION:  
    FIND CURRENT lokArtBas EXCLUSIVE-LOCK.
    DO piLoop = 1 TO NUM-ENTRIES(cRens):
      lokArtBas.Beskr = REPLACE(lokArtBas.Beskr, ENTRY(piLoop,cRens),'').      
    END.
    ASSIGN 
      lokArtBas.Beskr = TRIM(lokArtBas.Beskr)
      lokArtBas.Beskr = lokArtBas.Beskr + ' ' + pcStorl
      pbOk = TRUE
      .
      
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '      Endret varetekst: ' + lokArtBas.Beskr + '.'
        ).    
      
    RELEASE lokArtBas.
  END. /* SJEKKEN TRANSACTION */
  
  RETURN.
  
END PROCEDURE.
