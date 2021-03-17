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
DEFINE VARIABLE bOppdModell   AS LOG NO-UNDO. 
DEFINE VARIABLE lMva%         AS DECIMAL FORMAT "->,>>9.99" NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE bok AS LOG NO-UNDO.
DEFINE VARIABLE ohBuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE cparToADDRESS AS CHARACTER NO-UNDO.
DEFINE VARIABLE iX AS INTEGER NO-UNDO.
DEFINE VARIABLE bSendMail AS LOG NO-UNDO.

DEF VAR hQuery          AS HANDLE NO-UNDO.

DEFINE TEMP-TABLE ttArtPris LIKE ArtPris.
DEFINE TEMP-TABLE tt2ArtPris LIKE ArtPris.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rSendEMail AS cls.SendEMail.SendEMail NO-UNDO.

DEFINE BUFFER buf1ArtPris FOR ArtPris. /* eCom   */
DEFINE BUFFER buf2ArtPris FOR ArtPris. /* Outlet */
DEFINE BUFFER buf3ArtPris FOR ArtPris. /* Ved modellop */
DEFINE BUFFER bufArtBas FOR ArtBas.

/*{syspara.i 50 50 36 cparToADDRESS}*/
cparToADDRESS = 'are@gant.no;christina@gant.no;tomn@nsoft.no;gant.stortingsgaten@gantretail.no;gant.akerbrygge@gantretail.no;gant.sandvika@gantretail.no;gant.tonsberg@gantretail.no;gant.strommen@gantretail.no;gant.moa@gantretail.no;gant.lagunen@gantretail.no;gant.bergen@gantretail.no;gant.tromso@gantretail.no;gant.rortunet@gantretail.no;grs@gantretail.no'.

ASSIGN 
  cLogg = 'modell_prisoppdatering' + REPLACE(STRING(TODAY),'/','')
  .

IF SEARCH('tnc.txt') <> ? THEN 
  cparToADDRESS = 'tomn@nsoft.no'.

DEFINE STREAM Ut.

rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.
rSendEMail  = NEW cls.SendEMail.SendEMail( ) NO-ERROR.

rStandardFunksjoner:SkrivTilLogg(cLogg,
  'Start.' 
  ).

ASSIGN 
    lRab%     = 10
    lORab%    = 50
    lURab%    = 30
    ocReturn  = ""
    lMva%     = 25
    .

IF icParam = '' /*OR NOT CAN-FIND(Butiker WHERE 
                                Butiker.butik = INT(icParam)
                                ) */ THEN 
  RETURN.
                                  
IF NUM-ENTRIES(icParam,'|') >= 5 THEN 
DO:
  ASSIGN
    lInnkjopsPris  = DEC(ENTRY(3,icParam,'|'))
    lPris          = DEC(ENTRY(4,icParam,'|'))
    bOppdModell    = (IF CAN-DO('YES,TRUE,JA,1',ENTRY(5,icParam,'|')) THEN 
                       TRUE 
                     ELSE 
                       FALSE)
    .
  IF NUM-ENTRIES(icParam,'|') = 6 THEN
    ASSIGN  
      bSendMail =  IF CAN-DO('NO,FALSE,NEI,0',ENTRY(6,icParam,'|')) THEN 
                     FALSE
                   ELSE 
                     TRUE. 
   ELSE 
   ASSIGN  
      bSendMail = TRUE
      .
    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Parameteroppsett:' 
    ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    icParam: ' + icParam 
    ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    lInnkjopsPris: ' + STRING(lInnkjopsPris) 
    ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    lPris: ' + STRING(lPris) 
    ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    bOppdModell: ' + STRING(bOppdModell) 
    ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    bSendMail: ' + STRING(bSendMail) 
    ).

END.
ELSE 
  RETURN. 

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " WHERE TRUE").
hQuery:QUERY-OPEN().

hQuery:GET-FIRST().

BLOKKEN:
REPEAT WHILE NOT hQuery:QUERY-OFF-END TRANSACTION:
  FIND ArtPris NO-LOCK WHERE
    ArtPris.ArtikkelNr = DECIMAL(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE) AND 
    ArtPris.ProfilNr   = INTEGER(ihBuffer:BUFFER-FIELD("ProfilNr"):BUFFER-VALUE) NO-ERROR.
    
  IF AVAILABLE ArtPris THEN
  ENDREPRIS: 
  DO:
    FIND ArtBas OF ArtPris NO-LOCK NO-ERROR.
    /* Skal bare kjøres når det er prisendringsrutinen som anroper. */
    IF NUM-ENTRIES(icParam,'|') = 5 THEN 
    DO:
      rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  INNGANGSPRIS: Artikkelnr: ' + STRING(ArtPris.ArtikkelNr) + ' Prisprofil: ' + STRING(ArtPris.ProfilNr) 
        ).
      FIND CURRENT ArtPris EXCLUSIVE-LOCK.
  
      /* Oppdaterer prisen vi kommer inn fra. */
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
    END.
    
    /* Kjede pris skal settes lik eCom pris og motsatt. */
    FIND buf1ArtPris EXCLUSIVE-LOCK WHERE 
      buf1ArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND 
      buf1ArtPris.ProfilNr   = (IF ArtPris.ProfilNr = 1 THEN 16 ELSE 1) NO-ERROR.
    IF AVAILABLE buf1ArtPris THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          (IF buf1ArtPris.ProfilNr = 1 THEN '  KJEDEPRIS: ' ELSE '  ECOMPRIS: ') +  'Artikkelnr: ' + STRING(buf1ArtPris.ArtikkelNr) + ' Prisprofil: ' + STRING(buf1ArtPris.ProfilNr) 
          ).
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
    
    /* Outlet pris skal også oppdateres. */
    FIND buf2ArtPris EXCLUSIVE-LOCK WHERE 
      buf2ArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND 
      buf2ArtPris.ProfilNr   = 2 NO-ERROR.
    IF AVAILABLE buf2ArtPris  AND ArtPris.ProfilNr <> 2 THEN 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  OUTLETPRIS: Artikkelnr: ' + STRING(buf2ArtPris.ArtikkelNr) + ' Prisprofil: ' + STRING(buf2ArtPris.ProfilNr) 
          ).
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
      
    /* Oppdaterer hele modellen med prisendringene. */
    IF bOppdModell AND AVAILABLE ArtBas AND ArtBas.LevKod > '' AND ArtBas.LevFargKod > '' THEN 
      MODELLBLOKK: 
      DO:
        rStandardFunksjoner:SkrivTilLogg(cLogg,
          '  MODELLBLOKK: Kopierer til hele modellen.' 
          ).
        
        ARTIKKELLOOP:
        FOR EACH BufArtBas NO-LOCK WHERE 
          bufArtBas.LevNr      = ArtBas.LevNr AND 
          bufArtBas.LevKod     = ArtBas.LevKod AND 
          bufArtBas.LevFargKod > '':
            
          IF bufArtBas.ArtikkelNr = ArtBas.ArtikkelNr THEN 
            NEXT.
            
          /* Korrigerer eCom prisen. */
          IF AVAILABLE ArtPris THEN 
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
            
          /* Korrigerer kjede prisen. */
          IF AVAILABLE buf1ArtPris THEN 
            FIND buf3ArtPris EXCLUSIVE-LOCK WHERE 
              buf3ArtPris.ArtikkelNr =  bufArtBas.ArtikkelNr AND 
              buf3ArtPris.ProfilNr   = buf1ArtPris.ProfilNr NO-ERROR.
            IF AVAILABLE buf3ArtPris THEN 
            DO:
              ASSIGN
                buf3ArtPris.InnkjopsPris[1] = buf1ArtPris.InnkjopsPris[1]
                buf3ArtPris.ValPris[1]      = buf1ArtPris.ValPris[1]
                buf3ArtPris.Rab1%[1]        = buf1ArtPris.Rab1%[1]       
                buf3ArtPris.Rab1Kr[1]       = buf1ArtPris.Rab1Kr[1]       
                buf3ArtPris.VareKost[1]     = buf1ArtPris.VareKost[1]    
                buf3ArtPris.Pris[1]         = buf1ArtPris.Pris[1]        
                buf3ArtPris.MvaKr[1]        = buf1ArtPris.MvaKr[1]       
                buf3ArtPris.DbKr[1]         = buf1ArtPris.DbKr[1]        
                buf3ArtPris.Db%[1]          = buf1ArtPris.Db%[1]          
                .
              EMPTY TEMP-TABLE ttArtPris.
              CREATE ttArtPris.
              CREATE tt2ArtPris.
              BUFFER-COPY buf3ArtPris
                  TO ttArtPris.
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
            
          /* Korrigerer Outlet prisen. */
          IF AVAILABLE buf2ArtPris THEN 
            FIND buf3ArtPris EXCLUSIVE-LOCK WHERE 
              buf3ArtPris.ArtikkelNr =  bufArtBas.ArtikkelNr AND 
              buf3ArtPris.ProfilNr   = buf2ArtPris.ProfilNr NO-ERROR.
            IF AVAILABLE buf3ArtPris THEN 
            DO:
              ASSIGN
                buf3ArtPris.InnkjopsPris[1] = buf2ArtPris.InnkjopsPris[1]
                buf3ArtPris.ValPris[1]      = buf2ArtPris.ValPris[1]
                buf3ArtPris.Rab1%[1]        = buf2ArtPris.Rab1%[1]       
                buf3ArtPris.Rab1Kr[1]       = buf2ArtPris.Rab1Kr[1]       
                buf3ArtPris.VareKost[1]     = buf2ArtPris.VareKost[1]    
                buf3ArtPris.Pris[1]         = buf2ArtPris.Pris[1]        
                buf3ArtPris.MvaKr[1]        = buf2ArtPris.MvaKr[1]       
                buf3ArtPris.DbKr[1]         = buf2ArtPris.DbKr[1]        
                buf3ArtPris.Db%[1]          = buf2ArtPris.Db%[1]          
                .
                
              EMPTY TEMP-TABLE ttArtPris.
              CREATE ttArtPris.
              CREATE tt2ArtPris.
              BUFFER-COPY buf3ArtPris TO ttArtPris.
              BUFFER-COPY buf3ArtPris TO tt2ArtPris.
              ohBuffer = BUFFER ttArtPris:HANDLE.  
              RUN opprettHPrisko.p ('',
                                    ohBuffer,
                                    '',
                                    OUTPUT cTekst,
                                    OUTPUT bOk 
                                    ).  
                
            END.   
            
        END. /* ARTIKKELLOOP */
      END. /* MODELLBLOKK*/
  END. /* ENDREPRIS */

  hQuery:GET-NEXT().
END. /* BLOKKEN */

DELETE OBJECT hQuery NO-ERROR.

IF CAN-FIND(FIRST tt2ArtPris) AND bSendMail THEN 
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

DEFINE BUFFER pbufPrisprofil FOR Prisprofil.

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
rSendEMail:parSUBJECT  = (IF SEARCH('tnc.txt') <> ? THEN 'TEST ' ELSE '') + 
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

