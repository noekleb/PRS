/* lager_butikkstr.p
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE lArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>9" NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest AS LOG NO-UNDO.
DEFINE VARIABLE iAntall AS INTEGER NO-UNDO.
DEFINE VARIABLE cButLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cListeStr AS CHARACTER NO-UNDO.
DEFINE VARIABLE cListeAnt AS CHARACTER NO-UNDO.

DEF TEMP-TABLE Lager
    FIELD ArtikkelNr AS DECIMAL
    FIELD Butik AS INTEGER
    FIELD Lager_ButNr AS CHARACTER
    FIELD LagAnt AS DECIMAL
    FIELD VVarekost AS DECIMAL
    FIELD Str1 AS CHARACTER
    FIELD Str2 AS CHARACTER
    FIELD Str3 AS CHARACTER
    FIELD Str4 AS CHARACTER
    FIELD Str5 AS CHARACTER
    FIELD Str6 AS CHARACTER
    FIELD Str7 AS CHARACTER
    FIELD Str8 AS CHARACTER
    FIELD Str9 AS CHARACTER
    FIELD Str10 AS CHARACTER
    FIELD Str11 AS CHARACTER
    FIELD Str12 AS CHARACTER
    FIELD Str13 AS CHARACTER
    FIELD Str14 AS CHARACTER
    FIELD Str15 AS CHARACTER
    FIELD Str16 AS CHARACTER
    FIELD Str17 AS CHARACTER
    FIELD Str18 AS CHARACTER
    FIELD Str19 AS CHARACTER
    FIELD Str20 AS CHARACTER
    FIELD Str21 AS CHARACTER
    FIELD Str22 AS CHARACTER
    FIELD Str23 AS CHARACTER
    FIELD Str24 AS CHARACTER
    FIELD Str25 AS CHARACTER
    FIELD Str26 AS CHARACTER
    FIELD Str27 AS CHARACTER
    FIELD Str28 AS CHARACTER
    FIELD Str29 AS CHARACTER
    FIELD Str30 AS CHARACTER
    FIELD Str31 AS CHARACTER
    FIELD Str32 AS CHARACTER
    FIELD Str33 AS CHARACTER
    FIELD Str34 AS CHARACTER
    FIELD Str35 AS CHARACTER
    FIELD Str36 AS CHARACTER
    FIELD Str37 AS CHARACTER
    FIELD Str38 AS CHARACTER
    FIELD Str39 AS CHARACTER
    FIELD Str40 AS CHARACTER
    FIELD Str41 AS CHARACTER
    FIELD Str42 AS CHARACTER
    FIELD Str43 AS CHARACTER
    FIELD Str44 AS CHARACTER
    FIELD Str45 AS CHARACTER
    FIELD Str46 AS CHARACTER
    FIELD Str47 AS CHARACTER
    FIELD Str48 AS CHARACTER
    FIELD Str49 AS CHARACTER
    FIELD Str50 AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
    .
/* Trenger denne som en buffer å jobbe med for å opprette en sum record. */
DEFINE TEMP-TABLE ttLager 
    FIELD ArtikkelNr AS DECIMAL
    FIELD Butik AS INTEGER
    FIELD Lager_ButNr AS CHARACTER
    FIELD LagAnt AS DECIMAL
    FIELD VVarekost AS DECIMAL
    FIELD Str1 AS CHARACTER
    FIELD Str2 AS CHARACTER
    FIELD Str3 AS CHARACTER
    FIELD Str4 AS CHARACTER
    FIELD Str5 AS CHARACTER
    FIELD Str6 AS CHARACTER
    FIELD Str7 AS CHARACTER
    FIELD Str8 AS CHARACTER
    FIELD Str9 AS CHARACTER
    FIELD Str10 AS CHARACTER
    FIELD Str11 AS CHARACTER
    FIELD Str12 AS CHARACTER
    FIELD Str13 AS CHARACTER
    FIELD Str14 AS CHARACTER
    FIELD Str15 AS CHARACTER
    FIELD Str16 AS CHARACTER
    FIELD Str17 AS CHARACTER
    FIELD Str18 AS CHARACTER
    FIELD Str19 AS CHARACTER
    FIELD Str20 AS CHARACTER
    FIELD Str21 AS CHARACTER
    FIELD Str22 AS CHARACTER
    FIELD Str23 AS CHARACTER
    FIELD Str24 AS CHARACTER
    FIELD Str25 AS CHARACTER
    FIELD Str26 AS CHARACTER
    FIELD Str27 AS CHARACTER
    FIELD Str28 AS CHARACTER
    FIELD Str29 AS CHARACTER
    FIELD Str30 AS CHARACTER
    FIELD Str31 AS CHARACTER
    FIELD Str32 AS CHARACTER
    FIELD Str33 AS CHARACTER
    FIELD Str34 AS CHARACTER
    FIELD Str35 AS CHARACTER
    FIELD Str36 AS CHARACTER
    FIELD Str37 AS CHARACTER
    FIELD Str38 AS CHARACTER
    FIELD Str39 AS CHARACTER
    FIELD Str40 AS CHARACTER
    FIELD Str41 AS CHARACTER
    FIELD Str42 AS CHARACTER
    FIELD Str43 AS CHARACTER
    FIELD Str44 AS CHARACTER
    FIELD Str45 AS CHARACTER
    FIELD Str46 AS CHARACTER
    FIELD Str47 AS CHARACTER
    FIELD Str48 AS CHARACTER
    FIELD Str49 AS CHARACTER
    FIELD Str50 AS CHARACTER
    FIELD RowIdent1 AS CHARACTER 
    FIELD RowCount AS INTEGER
    FIELD jbCountDistinct AS INTEGER FORMAT '>>>,>>>,>>9' INIT 1
    FIELD jbAverage AS DECIMAL FORMAT '->>>,>>>,>>9.99'
    INDEX idxRowids  RowIdent1
  .  

DEFINE TEMP-TABLE ttStr NO-UNDO 
  FIELD Str AS CHARACTER
  FIELD StrKode AS INTEGER 
  FIELD Antall AS INTEGER 
  INDEX idxStr Str.
  
/*
• If you define a temp-table with the same name as a database table and 
  then you define a buffer for that name, the buffer will be associated 
  with the database table, not with the temp-table.
*/
DEFINE BUFFER bufLager FOR Lager. /* Dette bufferet går mot databasetabellen */ 

/* Standard funksjoner for logging */
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

{syspara.i 50 65 4 cButLst}
{syspar2.i 50 65 4 cTekst}
IF cTekst <> '' THEN 
  cButLst = cButLst + ',' + cTekst.

ASSIGN 
  bTest = TRUE 
  cLogg = 'lager_butikkstr' + REPLACE(STRING(TODAY),'/','')
  lArtikkelNr = DEC(ENTRY(1,icParam,'|'))
  .

IF bTest THEN 
DO:    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Start.' 
      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  lArtikkelNr: ' + STRING(lArtikkelNr) 
      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  cButLst: ' + cButLst 
      ).
END.    
    
RUN opprettLagerTbl.

ihBuffer:COPY-TEMP-TABLE (BUFFER Lager:HANDLE,NO,NO,YES).

IF bTest THEN 
DO:    
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  iAntall: ' + STRING(iAntall) 
      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  obOk: ' + STRING(obOk) 
      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  ocReturn: ' + ocReturn 
      ).
  rStandardFunksjoner:SkrivTilLogg(cLogg,
      'Slutt.' 
      ).
END.    

obOK = YES.
RETURN ocReturn.

/* **********************  Internal Procedures  *********************** */

PROCEDURE opprettLagerTbl:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VARIABLE lDec AS DECIMAL NO-UNDO.
  DEFINE VARIABLE cAlfaFordeling AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cStrBrukFordeling AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
  DEFINE VARIABLE iTotalt AS INTEGER NO-UNDO.
  DEFINE VARIABLE lVerdi AS DECIMAL FORMAT "->>>,>>>,>>9.99" NO-UNDO.
  DEFINE VARIABLE iOffSeth AS INTEGER NO-UNDO.
  DEFINE VARIABLE iPos AS INTEGER NO-UNDO.
      
  EMPTY TEMP-TABLE Lager.
  
  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ARtikkelNr = lArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN 
    RETURN.
  FIND StrType OF ArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE StrType THEN 
    RETURN.
  ASSIGN 
    cAlfaFordeling   = StrType.AlfaFordeling
    .

  STRLOOP:
  FOR EACH bufLager NO-LOCK WHERE
    bufLager.ArtikkelNr = lArtikkelNr:

    FOR EACH ArtLag NO-LOCK WHERE 
      ArtLag.ArtikkelNr = bufLager.ArtikkelNr AND 
      ArtLag.butik      = bufLAger.Butik:      
      IF CAN-DO(cButLst,STRING(bufLager.Butik)) THEN
      DO: 
        
        FIND FIRST ttStr WHERE 
                   ttStr .Str = ArtLag.Storl NO-ERROR.
        IF NOT AVAILABLE ttSTr THEN  
        DO:
          CREATE ttStr.
          ASSIGN 
            ttStr.Str     = ArtLag.Storl
            ttStr.StrKode = ArtLag.StrKode
            .
        END.
        ASSIGN 
          ttStr.Antall = ttStr.Antall + ArtLag.LagAnt
          .       
        IF NOT CAN-DO(TRIM(ArtLag.Storl),cAlfaFordeling) THEN 
          cAlfaFordeling = cAlfaFordeling + (IF cAlfaFordeling <> '' THEN ',' ELSE '') + ArtLag.Storl.    
      END.
    END.
  END. /* STRLOOP */
  /* Gjør dette her hvis det er oppdaget artlag som ikke ligger i størrelsestypen */
  ASSIGN 
    cStrBrukFordeling = FILL(',',NUM-ENTRIES(cAlfaFordeling) - 1)
    .

  /* Fyller inn StrKode der hvor det finnes en artlag post. */
  DO iLoop = 1 TO NUM-ENTRIES(cAlfaFordeling):
    FIND FIRST ttStr WHERE 
      ttStr.Str = ENTRY(iLoop,cAlfaFordeling) NO-ERROR.
    IF AVAILABLE ttStr THEN 
      ENTRY(iLoop,cStrBrukFordeling) = STRING(ttStr.StrKode).
  END.
  
  ASSIGN
    cListeStr = ''
    cListeAnt = ''
    .   
  /* Komprimerer listene. Tar bort tomme entries. */
  DO iLoop = 1 TO NUM-ENTRIES(cAlfaFordeling):
    IF INT(ENTRY(iLoop,cStrBrukFordeling)) <> 0 THEN 
      ASSIGN
        cListeStr = cListeStr + (IF cListeStr <> '' THEN ',' ELSE '') + ENTRY(iLoop,cAlfaFordeling)
        cListeAnt = cListeAnt + (IF cListeAnt <> '' THEN ',' ELSE '') + ENTRY(iLoop,cStrBrukFordeling)
        .   
  END.
  ASSIGN 
    cAlfaFordeling    = cListeStr
    cStrBrukFordeling = cListeAnt
    .
    
  /* Teller opp tomme kolonner (Offseth). Vi skal starte med første fyllte kolonne senere. */
  STRLOOP:
  DO iLoop = 1 TO NUM-ENTRIES(cStrBrukFordeling):
    IF INT(ENTRY(iLoop,cStrBrukFordeling)) > 0 THEN 
    DO:
      iOffSeth = iLoop - 1.
      LEAVE STRLOOP.
    END.
  END. /* STRLOOP */
    
  ASSIGN 
    ocReturn = cAlfaFordeling + '|' + cStrBrukFordeling + '|' + STRING(iOffSeth)
    .

  IF bTest THEN
  DO: 
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  StrType: ' + StrType.AlfaFordeling 
        ).
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  cStrBrukFordeling: ' + cStrBrukFordeling 
        ).        
    rStandardFunksjoner:SkrivTilLogg(cLogg,
        '  iOffseth: ' + STRING(iOffseth) 
        ).        
  END.
  
  LAGERLOOP:
  FOR EACH bufLager NO-LOCK WHERE
    bufLager.ArtikkelNr = lArtikkelNr , 
    FIRST ArtBas OF bufLager NO-LOCK:
      
    IF CAN-DO(cButLst,STRING(bufLager.Butik)) THEN
    DO: 
      CREATE Lager.
      ASSIGN 
        Lager.ArtikkelNr  = bufLager.ArtikkelNr
        Lager.Butik       = bufLager.Butik 
        Lager.LagAnt      = bufLager.LagAnt
        Lager.VVareKost   = bufLager.VVareKost
        Lager.RowIdent1   = STRING(ROWID(bufLager))
        Lager.Lager_ButNr = FILL(' ',8 - LENGTH(STRING(Lager.Butik))) + STRING(Lager.Butik)
        iAntall = iAntall + 1
        iTotalt           = iTotalt + bufLager.LagAnt
        lVerdi            = lVerdi + (bufLager.LagAnt * bufLager.VVareKost)
        .
        /* Leser */
        FOR EACH ArtLag NO-LOCK WHERE 
          ArtLag.ArtikkelNr = Lager.ArtikkelNr AND 
          ArtLag.Butik      = Lager.butik:
          iPos = LOOKUP(STRING(ArtLag.StrKode),cStrBrukFordeling) - iOffseth.
          IF iPos >= 1 THEN 
          CASE iPos:
            WHEN  1 THEN Lager.Str1  = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN  2 THEN Lager.Str2  = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN  3 THEN Lager.Str3  = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN  4 THEN Lager.Str4  = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN  5 THEN Lager.Str5  = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN  6 THEN Lager.Str6  = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN  7 THEN Lager.Str7  = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN  8 THEN Lager.Str8  = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN  9 THEN Lager.Str9  = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 10 THEN Lager.Str10 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
                                                           
            WHEN 11 THEN Lager.Str11 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 12 THEN Lager.Str12 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 13 THEN Lager.Str13 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 14 THEN Lager.Str14 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 15 THEN Lager.Str15 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 16 THEN Lager.Str16 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 17 THEN Lager.Str17 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 18 THEN Lager.Str18 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 19 THEN Lager.Str19 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 20 THEN Lager.Str20 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
                                                           
            WHEN 21 THEN Lager.Str21 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 22 THEN Lager.Str22 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 23 THEN Lager.Str23 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 24 THEN Lager.Str24 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 25 THEN Lager.Str25 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 26 THEN Lager.Str26 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 27 THEN Lager.Str27 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 28 THEN Lager.Str28 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 29 THEN Lager.Str29 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 30 THEN Lager.Str30 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            
            WHEN 31 THEN Lager.Str31 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 32 THEN Lager.Str32 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 33 THEN Lager.Str33 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 34 THEN Lager.Str34 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 35 THEN Lager.Str35 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 36 THEN Lager.Str36 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 37 THEN Lager.Str37 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 38 THEN Lager.Str38 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 39 THEN Lager.Str39 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 40 THEN Lager.Str40 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            
            WHEN 41 THEN Lager.Str41 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 42 THEN Lager.Str42 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 43 THEN Lager.Str43 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 44 THEN Lager.Str44 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 45 THEN Lager.Str45 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 46 THEN Lager.Str46 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 47 THEN Lager.Str47 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 48 THEN Lager.Str48 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 49 THEN Lager.Str49 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            WHEN 50 THEN Lager.Str50 = FILL(' ',8 - LENGTH(STRING(ArtLag.lagant))) + STRING(ArtLag.lagant).
            
          END CASE.
          ELSE DO:
            IF bTest THEN
              rStandardFunksjoner:SkrivTilLogg(cLogg,
                  '  iPos: ' + STRING(iPos) + ' OfSeth: ' + STRING(iOffSeth) +  ' StrKode: ' + STRING(ArtLag.StrKode) + ' liste: ' + cStrBrukFordeling
                  ).
          END.             
        END.
    END.
  END. /* LAGERLOOP */
  
  /* Totaler */
  CREATE ttLager.
  ASSIGN 
    ttLager.Butik       = 999999 
    ttLager.ArtikkelNr  = ArtBas.ArtikkelNr
    ttLager.LagAnt      = iTotalt
    ttLager.VVareKost   = ROUND(lVerdi / iTotalt,2)
    ttLager.VVareKost   = IF (ttLager.VVareKost < 0 OR ttLager.VVarekost = ?) THEN 0 ELSE ttLager.VVareKost 
    ttLager.RowIdent1   = ''
    ttLager.Lager_ButNr = '  Totalt'
    .
   
   /* Legger på plass summene i sumrecorden. */
   FOR EACH Lager:
     ASSIGN 
      ttLager.Str1  = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str1)  + INT(Lager.Str1))))  + STRING(INT(ttLager.Str1)  + INT(Lager.Str1))
      ttLager.Str2  = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str2)  + INT(Lager.Str2))))  + STRING(INT(ttLager.Str2)  + INT(Lager.Str2))
      ttLager.Str3  = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str3)  + INT(Lager.Str3))))  + STRING(INT(ttLager.Str3)  + INT(Lager.Str3))
      ttLager.Str4  = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str4)  + INT(Lager.Str4))))  + STRING(INT(ttLager.Str4)  + INT(Lager.Str4))
      ttLager.Str5  = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str5)  + INT(Lager.Str5))))  + STRING(INT(ttLager.Str5)  + INT(Lager.Str5))
      ttLager.Str6  = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str6)  + INT(Lager.Str6))))  + STRING(INT(ttLager.Str6)  + INT(Lager.Str6))
      ttLager.Str7  = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str7)  + INT(Lager.Str7))))  + STRING(INT(ttLager.Str7)  + INT(Lager.Str7))
      ttLager.Str8  = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str8)  + INT(Lager.Str8))))  + STRING(INT(ttLager.Str8)  + INT(Lager.Str8))
      ttLager.Str9  = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str9)  + INT(Lager.Str9))))  + STRING(INT(ttLager.Str9)  + INT(Lager.Str9))
      ttLager.Str10 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str10) + INT(Lager.Str10)))) + STRING(INT(ttLager.Str10) + INT(Lager.Str10))

      ttLager.Str11 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str11) + INT(Lager.Str11)))) + STRING(INT(ttLager.Str11) + INT(Lager.Str11))
      ttLager.Str12 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str12) + INT(Lager.Str12)))) + STRING(INT(ttLager.Str12) + INT(Lager.Str12))
      ttLager.Str13 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str13) + INT(Lager.Str13)))) + STRING(INT(ttLager.Str13) + INT(Lager.Str13))
      ttLager.Str14 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str14) + INT(Lager.Str14)))) + STRING(INT(ttLager.Str14) + INT(Lager.Str14))
      ttLager.Str15 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str15) + INT(Lager.Str15)))) + STRING(INT(ttLager.Str15) + INT(Lager.Str15))
      ttLager.Str16 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str16) + INT(Lager.Str16)))) + STRING(INT(ttLager.Str16) + INT(Lager.Str16))
      ttLager.Str17 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str17) + INT(Lager.Str17)))) + STRING(INT(ttLager.Str17) + INT(Lager.Str17))
      ttLager.Str18 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str18) + INT(Lager.Str18)))) + STRING(INT(ttLager.Str18) + INT(Lager.Str18))
      ttLager.Str19 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str19) + INT(Lager.Str19)))) + STRING(INT(ttLager.Str19) + INT(Lager.Str19))
      ttLager.Str20 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str20) + INT(Lager.Str20)))) + STRING(INT(ttLager.Str20) + INT(Lager.Str20))

      ttLager.Str21 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str21) + INT(Lager.Str21)))) + STRING(INT(ttLager.Str21) + INT(Lager.Str21))
      ttLager.Str22 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str22) + INT(Lager.Str22)))) + STRING(INT(ttLager.Str22) + INT(Lager.Str22))
      ttLager.Str23 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str23) + INT(Lager.Str23)))) + STRING(INT(ttLager.Str23) + INT(Lager.Str23))
      ttLager.Str24 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str24) + INT(Lager.Str24)))) + STRING(INT(ttLager.Str24) + INT(Lager.Str24))
      ttLager.Str25 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str25) + INT(Lager.Str25)))) + STRING(INT(ttLager.Str25) + INT(Lager.Str25))
      ttLager.Str26 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str26) + INT(Lager.Str26)))) + STRING(INT(ttLager.Str26) + INT(Lager.Str26))
      ttLager.Str27 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str27) + INT(Lager.Str27)))) + STRING(INT(ttLager.Str27) + INT(Lager.Str27))
      ttLager.Str28 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str28) + INT(Lager.Str28)))) + STRING(INT(ttLager.Str28) + INT(Lager.Str28))
      ttLager.Str29 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str29) + INT(Lager.Str29)))) + STRING(INT(ttLager.Str29) + INT(Lager.Str29))
      ttLager.Str30 = FILL(' ',8 - LENGTH(STRING(INT(ttLager.Str30) + INT(Lager.Str30)))) + STRING(INT(ttLager.Str30) + INT(Lager.Str30))
      .
   END.

   ASSIGN
    ttLager.Str1  = IF INT(ttLager.Str1 ) = 0 THEN '' ELSE ttLager.Str1 
    ttLager.Str2  = IF INT(ttLager.Str2 ) = 0 THEN '' ELSE ttLager.Str2
    ttLager.Str3  = IF INT(ttLager.Str3 ) = 0 THEN '' ELSE ttLager.Str3
    ttLager.Str4  = IF INT(ttLager.Str4 ) = 0 THEN '' ELSE ttLager.Str4
    ttLager.Str5  = IF INT(ttLager.Str5 ) = 0 THEN '' ELSE ttLager.Str5
    ttLager.Str6  = IF INT(ttLager.Str6 ) = 0 THEN '' ELSE ttLager.Str6
    ttLager.Str7  = IF INT(ttLager.Str7 ) = 0 THEN '' ELSE ttLager.Str7
    ttLager.Str8  = IF INT(ttLager.Str8 ) = 0 THEN '' ELSE ttLager.Str8
    ttLager.Str9  = IF INT(ttLager.Str9 ) = 0 THEN '' ELSE ttLager.Str9
    ttLager.Str10 = IF INT(ttLager.Str10) = 0 THEN '' ELSE ttLager.Str10

    ttLager.Str11 = IF INT(ttLager.Str11) = 0 THEN '' ELSE ttLager.Str11
    ttLager.Str12 = IF INT(ttLager.Str12) = 0 THEN '' ELSE ttLager.Str12
    ttLager.Str13 = IF INT(ttLager.Str13) = 0 THEN '' ELSE ttLager.Str13
    ttLager.Str14 = IF INT(ttLager.Str14) = 0 THEN '' ELSE ttLager.Str14
    ttLager.Str15 = IF INT(ttLager.Str15) = 0 THEN '' ELSE ttLager.Str15
    ttLager.Str16 = IF INT(ttLager.Str16) = 0 THEN '' ELSE ttLager.Str16
    ttLager.Str17 = IF INT(ttLager.Str17) = 0 THEN '' ELSE ttLager.Str17
    ttLager.Str18 = IF INT(ttLager.Str18) = 0 THEN '' ELSE ttLager.Str18
    ttLager.Str19 = IF INT(ttLager.Str19) = 0 THEN '' ELSE ttLager.Str19
    ttLager.Str20 = IF INT(ttLager.Str20) = 0 THEN '' ELSE ttLager.Str20

    ttLager.Str21 = IF INT(ttLager.Str21) = 0 THEN '' ELSE ttLager.Str21
    ttLager.Str22 = IF INT(ttLager.Str22) = 0 THEN '' ELSE ttLager.Str22
    ttLager.Str23 = IF INT(ttLager.Str23) = 0 THEN '' ELSE ttLager.Str23
    ttLager.Str24 = IF INT(ttLager.Str24) = 0 THEN '' ELSE ttLager.Str24
    ttLager.Str25 = IF INT(ttLager.Str25) = 0 THEN '' ELSE ttLager.Str25
    ttLager.Str26 = IF INT(ttLager.Str26) = 0 THEN '' ELSE ttLager.Str26
    ttLager.Str27 = IF INT(ttLager.Str27) = 0 THEN '' ELSE ttLager.Str27
    ttLager.Str28 = IF INT(ttLager.Str28) = 0 THEN '' ELSE ttLager.Str28
    ttLager.Str29 = IF INT(ttLager.Str29) = 0 THEN '' ELSE ttLager.Str29
    ttLager.Str30 = IF INT(ttLager.Str30) = 0 THEN '' ELSE ttLager.Str30
    .
   
  /* Oppretter sumrecord i temp tabellen. */
  CREATE Lager.
  BUFFER-COPY ttLager
    TO Lager.  
    
END PROCEDURE.
