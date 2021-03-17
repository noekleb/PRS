/* Hent merkelapper for pakkseddel
   Parameter:  
   Opprettet:               
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cLogg         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lInnkjopsPris AS DECIMAL FORMAT "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE lPris         AS DECIMAL FORMAT "->,>>>,>>9.99" NO-UNDO.
DEFINE VARIABLE lRab%         AS DECIMAL FORMAT "->,>>9.99" NO-UNDO.
DEFINE VARIABLE lORab%        AS DECIMAL FORMAT "->,>>9.99" NO-UNDO.
DEFINE VARIABLE lURab%        AS DECIMAL FORMAT "->,>>9.99" NO-UNDO.
DEFINE VARIABLE lKampRab%     AS DECIMAL FORMAT "->,>>9.99" NO-UNDO.
DEFINE VARIABLE bOppdModell   AS LOG NO-UNDO. 
DEFINE VARIABLE lMva%         AS DECIMAL FORMAT "->,>>9.99" NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE bok AS LOG NO-UNDO.
DEFINE VARIABLE ohBuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE cparToADDRESS AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.

DEF VAR hQuery          AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttArtPris LIKE ArtPris.
DEFINE TEMP-TABLE tt2ArtPris LIKE ArtPris.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.

DEFINE BUFFER buf1ArtPris FOR ArtPris. /* eCom   */
DEFINE BUFFER buf2ArtPris FOR ArtPris. /* Outlet */
DEFINE BUFFER buf3ArtPris FOR ArtPris. /* Ved modellop */
DEFINE BUFFER buf4ArtPris FOR ArtPris. /* Ved modellop */
DEFINE BUFFER buf5ArtPris FOR ArtPris. /* Ved modellop */
DEFINE BUFFER comArtPris  FOR ArtPris. /* For Illums kommisjonsbutikker. */
DEFINE BUFFER com2ArtPris FOR ArtPris. /* For Illums kommisjonsbutikker. */
DEFINE BUFFER bufArtBas FOR ArtBas.

{syspara.i 50 50 36 cparToADDRESS}
/*cparToADDRESS = 'are@gant.no;christina@gant.no;tomn@nsoft.no;gant.stortingsgaten@gantretail.no;gant.akerbrygge@gantretail.no;gant.sandvika@gantretail.no;gant.tonsberg@gantretail.no;gant.strommen@gantretail.no;gant.moa@gantretail.no;gant.lagunen@gantretail.no;gant.bergen@gantretail.no;gant.tromso@gantretail.no;gant.rortunet@gantretail.no;grs@gantretail.no'.*/
IF SEARCH('tnc.txt') <> ? THEN 
  cparToADDRESS = 'tomn@nsoft.no'.

DEFINE STREAM Ut.

rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

/* Henter rabatt for ordinær butikk */
FIND FIRST ImpKonv NO-LOCK WHERE
     ImpKonv.EDB-System = 'Gant Global' AND
     ImpKonv.Tabell     = 'Def.Rab%' AND
     ImpKonv.EksterntId = '2' NO-ERROR.
IF AVAILABLE ImpKonv THEN
  ASSIGN lRab% = DEC(ImpKonv.Merknad).
ELSE 
  ASSIGN lRab% = 10.

/* Henter rabatt for outlet butikk */
FIND FIRST ImpKonv NO-LOCK WHERE
     ImpKonv.EDB-System = 'Gant Global' AND
     ImpKonv.Tabell     = 'Def.Rab%' AND
     ImpKonv.EksterntId = '10' NO-ERROR.
IF AVAILABLE ImpKonv THEN
  ASSIGN 
    lORab% = DEC(ImpKonv.Merknad)
    lURab% = DEC(ImpKonv.InterntId)
    .
ELSE 
  ASSIGN 
    lORab% = 50
    lURab% = 30
    .
     
ASSIGN 
    ocReturn  = ""
    lMva%     = 25
    cLogg = 'modell_prisoppdatering' + REPLACE(STRING(TODAY),'/','')
    .

IF icParam = '' /*OR NOT CAN-FIND(Butiker WHERE 
                                Butiker.butik = INT(icParam)
                                ) */ THEN 
  RETURN.

/* icParam: STRING(lArtikkelNr) + '|' + STRING(iProfilNr) + '|' + fiInnkjopsPris:SCREEN-VALUE + '|' + fiPris:SCREEN-VALUE + '|' + tgAktiverModell:SCREEN-VALUE */                                  
IF NUM-ENTRIES(icParam,'|') = 5 THEN 
DO:
  ASSIGN
    lInnkjopsPris  = DEC(ENTRY(3,icParam,'|'))
    lPris          = DEC(ENTRY(4,icParam,'|'))
    bOppdModell    = (IF ENTRY(5,icParam,'|') = 'yes' THEN 
                       TRUE 
                     ELSE 
                       FALSE) 
    .
END.
ELSE 
  RETURN. 

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Start.' 
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '  Parametre:' 
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    icParam:' + icParam 
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    lInnkjopsPris:' + STRING(lInnkjopsPris) 
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    lPris:' + STRING(lPris) 
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    bOppdModell:' + STRING(bOppdModell) 
  ).

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

/* Det er alltid eCom prisen vi får inn her. Profilnr 16. */
BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
  FIND ArtPris EXCLUSIVE-LOCK WHERE
    ArtPris.ArtikkelNr = DECIMAL(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) AND 
    ArtPris.ProfilNr   = INTEGER(ihBuffer:BUFFER-FIELD("ProfilNr"):BUFFER-VALUE) NO-ERROR.
    
  IF AVAILABLE ArtPris THEN
  ENDREPRIS: 
  DO:
    FIND ArtBas OF ArtPris NO-LOCK NO-ERROR.

    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Artikkel/Profil: '  + STRING(ArtBas.ArtikkelNr) + ' ' +
        ArtBAs.Beskr + ' ' + 
        ArtBas.LevKod + ' ' + 
        ArtBas.LevFargKod + ' Profil: ' + 
        STRING(ArtPris.ProfilNr)  
      ).
    
    /* TN 11/1-21.                                                                     */
    /* Er artikkelen aktiv på tilbud i denne profilen, skal også tilbudsprisen endres. */
    /* Tilbuds rabatten skal være lik på tilbudet etter endring av tilbudsprisen.      */
    /* Dette gjør at tilbudsprisen endres i nettbutikken, men tilbudsrabatten er lik.  */
    /* Her beregnes kampanjerabatten slik den var før normalprisendring.               */
    lKampRab% = 0.
    IF ArtPris.Tilbud THEN 
    DO:
      lKampRab% = ROUND(((ArtPris.Pris[1] - ArtPris.Pris[2]) * 100 / ArtPris.Pris[1]),0). 
    END. 
    
    /* eCom pris. */
    ASSIGN 
      ArtPris.InnkjopsPris[1] = lInnkjopsPris 
      ArtPris.ValPris[1]      = ArtPris.InnkjopsPris[1]
      ArtPris.Rab1%[1]        = lRab%
      ArtPris.Rab1Kr[1]       = ROUND((ArtPris.InnkjopsPris[1] * ArtPris.Rab1%[1]) / 100,2)
      ArtPris.VareKost[1]     = ArtPris.InnkjopsPris[1] - ROUND((ArtPris.InnkjopsPris[1] * ArtPris.Rab1%[1]) / 100,2) 

      ArtPris.Pris[1]         = lPris
      ArtPris.MvaKr[1]        = ArtPris.Pris[1] - ROUND((ArtPris.Pris[1] / (1 + (lMva% / 100))),2)

      ArtPris.DbKr[1]         = ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
      ArtPris.Db%[1]          = ROUND(
                                      (ArtPris.DbKr[1] * 100) / (ArtPris.Pris[1] - ArtPris.MvaKr[1])
                                      ,2) 
      ArtPris.Db%[1]          = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
      .
    /* 11/1-21 Regner om tilbudsprisen hvis den er aktiv på tilbud. */
    IF ArtPris.Tilbud AND lKampRab% > 0 THEN 
    DO:
      ASSIGN 
        ArtPris.Pris[2]  = ROUND(lPris - ((lPris * lKampRab%) / 100),0)
        ArtPris.MvaKr[2] = ArtPris.Pris[2] - ROUND((ArtPris.Pris[2] / (1 + (lMva% / 100))),2)
        ArtPris.DbKr[2]  = ArtPris.Pris[2] - ArtPris.MvaKr[2] - ArtPris.VareKost[2]
        ArtPris.Db%[2]   = ROUND(
                                 (ArtPris.DbKr[2] * 100) / (ArtPris.Pris[2] - ArtPris.MvaKr[2]),2) 
        ArtPris.Db%[2]   = IF ArtPris.Db%[2] = ? THEN 0 ELSE ArtPris.Db%[2]
        .
      FIND bufArtBas EXCLUSIVE-LOCK WHERE 
        bufArtBas.ArtikkelNr = ArtPris.ArtikkelNr NO-ERROR NO-WAIT.
      IF AVAILABLE bufArtBas AND NOT LOCKED bufArtBas THEN 
      DO:
        bufArtBas.ETid = TIME.
        RELEASE bufArtBas.
      END.
    END.
    rStandardFunksjoner:SkrivTilLogg(cLogg,
      '    ArtikkelNr/Profil/Pris: ' + STRING(ArtPris.ArtikkelNr) + '/' + STRING(ArtPris.ProfilNr) + '/' + STRING(ArtPris.Pris[1]) 
      ).

    EMPTY TEMP-TABLE ttArtPris.
    CREATE ttArtPris.
    BUFFER-COPY ArtPris
        TO ttArtPris.
    CREATE tt2ArtPris.
    BUFFER-COPY ArtPris
        TO tt2ArtPris.
    ohBuffer = BUFFER ttArtPris:HANDLE.  
    RUN opprettHPrisko.p ('',
                          ohBuffer,
                          '',
                          OUTPUT cTekst,
                          OUTPUT bOk 
                          ).  
      
    FIND CURRENT ArtPris NO-LOCK.

    
    /* Kjede pris skal settes lik eCom pris. */
    FIND buf1ArtPris EXCLUSIVE-LOCK WHERE 
      buf1ArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND 
      buf1ArtPris.ProfilNr   = 1 NO-ERROR.
    IF AVAILABLE buf1ArtPris THEN 
      DO:
        ASSIGN
          buf1ArtPris.InnkjopsPris[1] = ArtPris.InnkjopsPris[1]
          buf1ArtPris.ValPris[1]      = ArtPris.ValPris[1]
          buf1ArtPris.Rab1%[1]        = ArtPris.Rab1%[1]       
          buf1ArtPris.Rab1Kr[1]       = ArtPris.Rab1Kr[1]       
          buf1ArtPris.VareKost[1]     = ArtPris.VareKost[1]    
          buf1ArtPris.Pris[1]         = ArtPris.Pris[1]        
          buf1ArtPris.MvaKr[1]        = ArtPris.MvaKr[1]       
          buf1ArtPris.DbKr[1]         = ArtPris.DbKr[1]        
          buf1ArtPris.Db%[1]          = ArtPris.Db%[1]          
          .
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '    ArtikkelNr/Profil/Pris: ' + STRING(buf1ArtPris.ArtikkelNr) + '/' + STRING(buf1ArtPris.ProfilNr) + '/' + STRING(buf1ArtPris.Pris[1]) 
        ).
        EMPTY TEMP-TABLE ttArtPris.
        CREATE ttArtPris.
        BUFFER-COPY buf1ArtPris
            TO ttArtPris.
        CREATE tt2ArtPris.
        BUFFER-COPY buf1ArtPris
            TO tt2ArtPris.
        ohBuffer = BUFFER ttArtPris:HANDLE.  
        RUN opprettHPrisko.p ('',
                              ohBuffer,
                              '',
                              OUTPUT cTekst,
                              OUTPUT bOk 
                              ).  
      END.
    
    
    /* Outlet pris skal justeres på grunnlag av endring i eCom pris. */
    FIND buf2ArtPris EXCLUSIVE-LOCK WHERE 
      buf2ArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND 
      buf2ArtPris.ProfilNr   = 2 NO-ERROR.
    IF AVAILABLE buf2ArtPris THEN 
      DO:
        ASSIGN 
          /* Kostnadssiden */
          buf2ArtPris.InnkjopsPris[1] = ArtPris.InnkjopsPris[1] 
          buf2ArtPris.ValPris[1]      = ArtPris.ValPris[1]
          buf2ArtPris.Rab1%[1]        = lORab%
          buf2ArtPris.Rab1Kr[1]       = ROUND((buf2ArtPris.InnkjopsPris[1] * buf2ArtPris.Rab1%[1]) / 100,2)
          buf2ArtPris.VareKost[1]     = buf2ArtPris.InnkjopsPris[1] - ROUND((buf2ArtPris.InnkjopsPris[1] * buf2ArtPris.Rab1%[1]) / 100,2)
          /* Utpris siden */ 
          buf2ArtPris.Pris[1]         = ROUND(
                                              ArtPris.Pris[1] - 
                                              ((ArtPris.Pris[1] * lURab%) / 100)
                                             ,0)
          buf2ArtPris.MvaKr[1]        = buf2ArtPris.Pris[1] - ROUND((buf2ArtPris.Pris[1] / (1 + (lMva% / 100))),2)
          buf2ArtPris.DbKr[1]         = buf2ArtPris.Pris[1] - buf2ArtPris.MvaKr[1] - buf2ArtPris.VareKost[1]
          buf2ArtPris.Db%[1]          = ROUND((buf2ArtPris.DbKr[1] * 100) / (buf2ArtPris.Pris[1] - buf2ArtPris.MvaKr[1]),2)
          buf2ArtPris.Db%[1]          = IF buf2ArtPris.Db%[1] = ? THEN 0 ELSE buf2ArtPris.Db%[1]
          .

        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    ArtikkelNr/Profil/Pris: ' + STRING(buf2ArtPris.ArtikkelNr) + '/' + STRING(buf2ArtPris.ProfilNr) + '/' + STRING(buf2ArtPris.Pris[1]) 
          ).
        EMPTY TEMP-TABLE ttArtPris.
        CREATE ttArtPris.
        BUFFER-COPY buf2ArtPris
            TO ttArtPris.
        CREATE tt2ArtPris.
        BUFFER-COPY buf2ArtPris
            TO tt2ArtPris.
        ohBuffer = BUFFER ttArtPris:HANDLE.  
        RUN opprettHPrisko.p ('',
                              ohBuffer,
                              '',
                              OUTPUT cTekst,
                              OUTPUT bOk 
                              ).  
      END.

    /* Kommisjonsbutikkene skal ha oppdatering av utprisen. */
    FIND comArtPris EXCLUSIVE-LOCK WHERE 
      comArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND 
      comArtPris.ProfilNr   = 100 NO-ERROR.
    IF AVAILABLE comArtPris THEN 
      DO:
        lKampRab% = 0.
        IF comArtPris.Tilbud THEN 
        DO:
          lKampRab% = ROUND(((comArtPris.Pris[1] - comArtPris.Pris[2]) * 100 / comArtPris.Pris[1]),0). 
        END. 
        ASSIGN 
          /* Kostnadssiden. Innkjøpsprisen røres ikke. Rabatten står til 0 på denne kalkylen. */
          comArtPris.Pris[1]  = ArtPris.Pris[1]
          comArtPris.MvaKr[1] = ArtPris.MvaKr[1]
          comArtPris.DbKr[1]  = ArtPris.DbKr[1]
          comArtPris.Db%[1]   = ArtPris.Db%[1]
          .
        /* Står den på tilbud, skal den ha justert tilbudsprisen. */
        /* tilbud er samkjørt med ECom.                           */
        IF comArtPris.Tilbud AND lKampRab% > 0 THEN 
        DO:
          ASSIGN 
            comArtPris.Pris[2]  = ROUND(comArtPris.Pris[1] - ((comArtPris.Pris[1] * lKampRab%) / 100),0)
            comArtPris.MvaKr[2] = comArtPris.Pris[2] - ROUND((comArtPris.Pris[2] / (1 + (lMva% / 100))),2)
            comArtPris.DbKr[2]  = comArtPris.Pris[2] - comArtPris.MvaKr[2] - comArtPris.VareKost[2]
            comArtPris.Db%[2]   = ROUND(
                                     (comArtPris.DbKr[2] * 100) / (comArtPris.Pris[2] - comArtPris.MvaKr[2]),2) 
            comArtPris.Db%[2]   = IF comArtPris.Db%[2] = ? THEN 0 ELSE comArtPris.Db%[2]
            .
        END.
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '    ArtikkelNr/Profil/Pris: ' + STRING(comArtPris.ArtikkelNr) + '/' + STRING(comArtPris.ProfilNr) + '/' + STRING(comArtPris.Pris[1]) 
          ).

        EMPTY TEMP-TABLE ttArtPris.
        CREATE ttArtPris.
        BUFFER-COPY comArtPris
            TO ttArtPris.
        CREATE tt2ArtPris.
        BUFFER-COPY comArtPris
            TO tt2ArtPris.
        ohBuffer = BUFFER ttArtPris:HANDLE.  
        RUN opprettHPrisko.p ('',
                              ohBuffer,
                              '',
                              OUTPUT cTekst,
                              OUTPUT bOk 
                              ).
        RUN opprettArtPrisELogg.p (comArtPris.ArtikkelNr, comArtPris.ProfilNr, 'PRICAT_KOMMISJON').  
      END.
      
      /* Oppdaterer hele modellen med prisendringene. */
      IF bOppdModell AND AVAILABLE ArtBas AND ArtBas.LevKod > '' AND ArtBas.LevFargKod > '' THEN 
      MODELLBLOKK: 
      DO:
        ARTIKKELLOOP:
        FOR EACH BufArtBas NO-LOCK WHERE 
          bufArtBas.LevNr      = ArtBas.LevNr AND 
          bufArtBas.LevKod     = ArtBas.LevKod AND 
          bufArtBas.LevFargKod > '':
            
          /* Her har vi allerede gjort endringen. */
          IF bufArtBas.ArtikkelNr = ArtBas.ArtikkelNr THEN 
            NEXT.

          rStandardFunksjoner:SkrivTilLogg(cLogg,
            '  Artikkel/Profil: ' + STRING(bufArtBAs.ArtikkelNr) + ' ' + 
              bufArtBAs.Beskr + ' ' + 
              bufArtBas.LevKod + ' ' + 
              bufArtBas.LevFargKod + ' Profil: ' + 
              STRING(ArtPris.ProfilNr)  
            ).
            
          /* Korrigerer eCom prisen. */
          IF AVAILABLE ArtPris THEN
          DO: 
            FIND buf3ArtPris EXCLUSIVE-LOCK WHERE 
              buf3ArtPris.ArtikkelNr = bufArtBas.ArtikkelNr AND 
              buf3ArtPris.ProfilNr   = ArtPris.ProfilNr NO-ERROR.
            IF AVAILABLE buf3ArtPris THEN 
            DO:
              ASSIGN
                buf3ArtPris.InnkjopsPris[1] = ArtPris.InnkjopsPris[1]
                buf3ArtPris.ValPris[1]      = ArtPris.ValPris[1]
                buf3ArtPris.Rab1%[1]        = ArtPris.Rab1%[1]       
                buf3ArtPris.Rab1Kr[1]       = ArtPris.Rab1Kr[1]       
                buf3ArtPris.VareKost[1]     = ArtPris.VareKost[1]    
                buf3ArtPris.Pris[1]         = ArtPris.Pris[1]        
                buf3ArtPris.MvaKr[1]        = ArtPris.MvaKr[1]       
                buf3ArtPris.DbKr[1]         = ArtPris.DbKr[1]        
                buf3ArtPris.Db%[1]          = ArtPris.Db%[1]          
                .
            rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    ArtikkelNr/Profil/Pris: ' + STRING(buf3ArtPris.ArtikkelNr) + '/' + STRING(buf3ArtPris.ProfilNr) + '/' + STRING(buf3ArtPris.Pris[1]) 
              ).

            EMPTY TEMP-TABLE ttArtPris.
            CREATE ttArtPris.
            BUFFER-COPY buf3ArtPris
                TO ttArtPris.
            CREATE tt2ArtPris.
            BUFFER-COPY buf3ArtPris
                TO tt2ArtPris.
            ohBuffer = BUFFER ttArtPris:HANDLE.  
            RUN opprettHPrisko.p ('',
                                  ohBuffer,
                                  '',
                                  OUTPUT cTekst,
                                  OUTPUT bOk 
                                  ).  
            END.   
          END.
            
          /* Korrigerer kjede prisen. */
          IF AVAILABLE buf1ArtPris THEN
          DO:
            FIND buf4ArtPris EXCLUSIVE-LOCK WHERE
              buf4ArtPris.ArtikkelNr =  bufArtBas.ArtikkelNr AND
              buf4ArtPris.ProfilNr   = buf1ArtPris.ProfilNr NO-ERROR.
            IF AVAILABLE buf4ArtPris THEN
            DO:
              ASSIGN
                buf4ArtPris.InnkjopsPris[1] = buf1ArtPris.InnkjopsPris[1]
                buf4ArtPris.ValPris[1]      = buf1ArtPris.ValPris[1]
                buf4ArtPris.Rab1%[1]        = buf1ArtPris.Rab1%[1]
                buf4ArtPris.Rab1Kr[1]       = buf1ArtPris.Rab1Kr[1]
                buf4ArtPris.VareKost[1]     = buf1ArtPris.VareKost[1]
                buf4ArtPris.Pris[1]         = buf1ArtPris.Pris[1]
                buf4ArtPris.MvaKr[1]        = buf1ArtPris.MvaKr[1]
                buf4ArtPris.DbKr[1]         = buf1ArtPris.DbKr[1]
                buf4ArtPris.Db%[1]          = buf1ArtPris.Db%[1]
                .
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    ArtikkelNr/Profil/Pris: ' + STRING(buf4ArtPris.ArtikkelNr) + '/' + STRING(buf4ArtPris.ProfilNr) + '/' + STRING(buf4ArtPris.Pris[1])
                ).

              EMPTY TEMP-TABLE ttArtPris.
              CREATE ttArtPris.
              CREATE tt2ArtPris.
              BUFFER-COPY buf4ArtPris
                  TO ttArtPris.
              BUFFER-COPY buf4ArtPris
                  TO tt2ArtPris.
              ohBuffer = BUFFER ttArtPris:HANDLE.
              RUN opprettHPrisko.p ('',
                                    ohBuffer,
                                    '',
                                    OUTPUT cTekst,
                                    OUTPUT bOk
                                    ).
            END.
          END.
          
          /* Korrigerer Outlet prisen. */
          IF AVAILABLE buf2ArtPris THEN
          DO:
            FIND buf5ArtPris EXCLUSIVE-LOCK WHERE
              buf5ArtPris.ArtikkelNr =  bufArtBas.ArtikkelNr AND
              buf5ArtPris.ProfilNr   = buf2ArtPris.ProfilNr NO-ERROR.
            IF AVAILABLE buf5ArtPris THEN
            DO:
              ASSIGN
                buf5ArtPris.InnkjopsPris[1] = buf2ArtPris.InnkjopsPris[1]
                buf5ArtPris.ValPris[1]      = buf2ArtPris.ValPris[1]
                buf5ArtPris.Rab1%[1]        = buf2ArtPris.Rab1%[1]
                buf5ArtPris.Rab1Kr[1]       = buf2ArtPris.Rab1Kr[1]
                buf5ArtPris.VareKost[1]     = buf2ArtPris.VareKost[1]
                buf5ArtPris.Pris[1]         = buf2ArtPris.Pris[1]
                buf5ArtPris.MvaKr[1]        = buf2ArtPris.MvaKr[1]
                buf5ArtPris.DbKr[1]         = buf2ArtPris.DbKr[1]
                buf5ArtPris.Db%[1]          = buf2ArtPris.Db%[1]
                .

            rStandardFunksjoner:SkrivTilLogg(cLogg,
              '    ArtikkelNr/Profil/Pris: ' + STRING(buf5ArtPris.ArtikkelNr) + '/' + STRING(buf5ArtPris.ProfilNr) + '/' + STRING(buf5ArtPris.Pris[1])
              ).

              EMPTY TEMP-TABLE ttArtPris.
              CREATE ttArtPris.
              CREATE tt2ArtPris.
              BUFFER-COPY buf5ArtPris TO ttArtPris.
              BUFFER-COPY buf5ArtPris TO tt2ArtPris.
              ohBuffer = BUFFER ttArtPris:HANDLE.
              RUN opprettHPrisko.p ('',
                                    ohBuffer,
                                    '',
                                    OUTPUT cTekst,
                                    OUTPUT bOk
                                    ).

            END.
          END.
          
          /* Korrigerer kommisjons prisen. */
          IF AVAILABLE comArtPris THEN
          DO:
            FIND com2ArtPris EXCLUSIVE-LOCK WHERE
              com2ArtPris.ArtikkelNr = bufArtBas.ArtikkelNr AND
              com2ArtPris.ProfilNr   = comArtPris.ProfilNr NO-ERROR.
            IF AVAILABLE com2ArtPris THEN
            DO:
              BUFFER-COPY comArtPris
                EXCEPT ArtikkelNr ProfilNr
                TO com2ArtPris NO-ERROR.
                
              DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
                  cTekst = STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' '+ 
                           ERROR-STATUS:GET-MESSAGE(ix). 
                  rStandardFunksjoner:SkrivTilLogg(cLogg, 
                      '   ** Feil: ' + cTekst 
                  ). 
              END.
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                '    ArtikkelNr/Profil/Pris: ' + STRING(com2ArtPris.ArtikkelNr) + '/' + STRING(com2ArtPris.ProfilNr) + '/' + STRING(com2ArtPris.Pris[1])
                ).

              EMPTY TEMP-TABLE ttArtPris.
              CREATE ttArtPris.
              CREATE tt2ArtPris.
              BUFFER-COPY com2ArtPris TO ttArtPris.
              BUFFER-COPY com2ArtPris TO tt2ArtPris.
              ohBuffer = BUFFER ttArtPris:HANDLE.
              RUN opprettHPrisko.p ('',
                                    ohBuffer,
                                    '',
                                    OUTPUT cTekst,
                                    OUTPUT bOk
                                    ).
              RUN opprettArtPrisELogg.p (com2ArtPris.ArtikkelNr, com2ArtPris.ProfilNr, 'PRICAT_KOMMISJON').
            END.
          END.
        END. /* ARTIKKELLOOP */
      END. /* MODELLBLOKK*/
  END. /* ENDREPRIS */

  hQuery:GET-NEXT().
END. /* BLOKKEN */

DELETE OBJECT hQuery NO-ERROR.

IF CAN-FIND(FIRST tt2ArtPris) THEN 
  RUN sendEMail.

EMPTY TEMP-TABLE ttArtPris.
EMPTY TEMP-TABLE tt2ArtPris.

obOk = TRIM(ocReturn) = ''.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Slutt.' 
  ).

RETURN ocReturn.



/* **********************  Internal Procedures  *********************** */

PROCEDURE sendEMail:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

DEFINE VARIABLE cFil AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrefix AS CHARACTER NO-UNDO.

DEFINE BUFFER pbufPrisprofil FOR Prisprofil.

IF SEARCH('tnc.txt') <> ? THEN 
  cPrefix = 'TEST '.
ELSE 
  cPrefix = ''.
  
IF cparToADDRESS = '' THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg (cLogg,'      Adresseliste for mottager av prissjekk Syspara: 50 50 36, er ikke satt opp.').
  RETURN.
END.

IF CAN-FIND(FIRST tt2ArtPris) THEN 
DO:
  cFil = 'konv\PrisEndring' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.txt'.
  OUTPUT STREAM Ut TO VALUE(cFil).
  PUT STREAM Ut 
    'ArtikkelNr    ' /* 14*/
    'Varetekst                     ' /* 30 */
    'Lev.art.nr          ' /* 20 */
    'Lev.fargekode       ' /* 20 */
    'ProfilNr ' /* 9 */
    'Pris      ' /* 10 */
    SKIP.
    
  /* Klargjør og sender eMail */
  FOR EACH tt2ArtPris
    BREAK BY tt2ArtPris.ArtikkelNr
          BY tt2ArtPris.ProfilNr:

    FIND ArtBas NO-LOCK WHERE 
      ArtBas.ArtikkelNr = tt2ArtPris.ArtikkelNr NO-ERROR.
    PUT STREAM Ut 
      STRING(tt2ArtPris.ArtikkelNr) + fill(' ',14 - LENGTH(STRING(tt2ArtPris.ArtikkelNr))) FORMAT "x(14)"
      STRING(ArtBas.Beskr) + fill(' ',30 - LENGTH(STRING(ArtBas.Beskr))) FORMAT "x(30)"
      STRING(ArtBas.LevKod) + fill(' ',20 - LENGTH(STRING(ArtBas.LevKod))) FORMAT "x(20)"
      STRING(ArtBas.LevFargKod) + fill(' ',20 - LENGTH(STRING(ArtBas.LevFargKod))) FORMAT "x(20)"
      STRING(tt2ArtPris.ProfilNr) + fill(' ',9 - LENGTH(STRING(tt2ArtPris.ProfilNr))) FORMAT "x(9)"
      STRING(tt2ArtPris.Pris[1]) + fill(' ',10 - LENGTH(STRING(tt2ArtPris.Pris[1]))) FORMAT "x(10)"
      SKIP.
  END.
  OUTPUT STREAM Ut CLOSE.
END.

FILE-INFO:FILE-NAME = cFil.

rSendEMail:parToADDRESS = cparToADDRESS.
rSendEMail:parMailType = 'PRISMODELL'.
rSendEMail:parSUBJECT  = (IF SEARCH('tnc.txt') <> ? THEN cPrefix ELSE '') + 
                         'Prisendringer fra eCom på artikkel/modell' + ' (Dato/Tid: ' + STRING(NOW,"99/99/9999 HH:MM:SS") + ').'.
/*rSendEMail:parMESSAGE  = "Loggfil: " + cFil + '.' + CHR(10) +                         */
/*                         "Det har her skjedd endringer på en eller artikler/ farger.".*/
rSendEMail:parMESSAGE-FILE = FILE-INFO:FULL-PATHNAME.
rSendEMail:parFILE     = FILE-INFO:FULL-PATHNAME.  

rStandardFunksjoner:SkrivTilLogg (cLogg,'    eMail info:').
rStandardFunksjoner:SkrivTilLogg (cLogg,'        ' + rSendEMail:parMailType).
rStandardFunksjoner:SkrivTilLogg (cLogg,'        ' + rSendEMail:parSUBJECT).
rStandardFunksjoner:SkrivTilLogg (cLogg,'        ' + STRING(rSendEMail:parMESSAGE)).
rStandardFunksjoner:SkrivTilLogg (cLogg,'        ' + rSendEMail:parFILE).

bOk = rSendEMail:send( ).

rStandardFunksjoner:SkrivTilLogg (cLogg,'    eMail sende resultat: ' + STRING(bOk)).
                    
IF ERROR-STATUS:ERROR THEN 
    DO:
        rStandardFunksjoner:SkrivTilLogg (cLogg,'    **FEIL. eMail ikke sendt. Vedlegg ' + FILE-INFO:FULL-PATHNAME + '.').
        DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
            rStandardFunksjoner:SkrivTilLogg (cLogg, '          ' 
                + STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' ' + ERROR-STATUS:GET-MESSAGE(ix)    
                ).
        END.            
    END.

END PROCEDURE.

