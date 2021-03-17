
/*------------------------------------------------------------------------
    File        : artprisPRICAT.p
    Purpose     : Generering av de EDI filer som skal sendes Illums ved manuell prisendring

    Syntax      :

    Description : 

    Author(s)   : Tom Nøkleby
    Created     : 14/1-21
    Notes       :
      
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{cls\kommisjon\ttArtPris.i}  

DEFINE INPUT PARAMETER cLogg AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iType AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR ttArtPris.

DEFINE VARIABLE cUNA AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUNB AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgUNB AS CHARACTER NO-UNDO.
DEFINE VARIABLE cGLNSender AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQuilifierSender AS CHARACTER NO-UNDO.
DEFINE VARIABLE cGLNRecipient AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQuilifierRecipient AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIllumButGLN AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDato AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDato1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDato2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDato3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTid AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMSGSeqNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cProd AS CHARACTER NO-UNDO.
DEFINE VARIABLE iPRICATLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cKontaktPerson AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE cImportKatalog AS CHARACTER NO-UNDO.

DEFINE VARIABLE cUNH AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgUNH AS CHARACTER NO-UNDO.
DEFINE VARIABLE cBGM AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgBGM AS CHARACTER NO-UNDO.

DEFINE VARIABLE cDTM1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDTM2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDTM3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgDTM1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgDTM2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgDTM3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDTM4 AS CHARACTER NO-UNDO.

DEFINE VARIABLE cNADSu AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgNADSu AS CHARACTER NO-UNDO.
DEFINE VARIABLE cNADBy AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgNADBy AS CHARACTER NO-UNDO.

DEFINE VARIABLE cUNS AS CHARACTER NO-UNDO.

DEFINE VARIABLE cCNT AS CHARACTER NO-UNDO. 
DEFINE VARIABLE cOrgCNT AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNbLines AS INTEGER NO-UNDO.
 
DEFINE VARIABLE cCUX AS CHARACTER NO-UNDO.

DEFINE VARIABLE cUNT AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgUNT AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNbSegments AS INTEGER NO-UNDO.

DEFINE VARIABLE cUNZ AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgUNZ AS CHARACTER NO-UNDO.

DEFINE VARIABLE cLIN AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgLIN AS CHARACTER NO-UNDO.

DEFINE VARIABLE cPIA AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgPIA AS CHARACTER NO-UNDO.

DEFINE VARIABLE cIMD_F AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgIMD_F AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIMD_F_BRN AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgIMD_F_BRN AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIMD_C AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgIMD_C AS CHARACTER NO-UNDO.
DEFINE VARIABLE cIMD_F_35 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgIMD_F_35 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cQTY AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgQTY AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFTX AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgFTX AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPRI AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgPRI AS CHARACTER NO-UNDO.

DEFINE VARIABLE cCTA AS CHARACTER NO-UNDO.
DEFINE VARIABLE cOrgCTA AS CHARACTER NO-UNDO.

DEFINE VARIABLE cPRICATFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDESADVFilNavn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRECADVFilNavn AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE ttPRICAT NO-UNDO
  FIELD LinjeNr AS INTEGER FORMAT ">>>>>9"
  FIELD Segments AS CHARACTER FORMAT "x(60)"
  .

DEFINE STREAM Ut.

DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */



rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ) NO-ERROR.

{syspara.i 5 40 1 cGLNSender}
{syspar2.i 5 40 1 cQuilifierSender}
{syspara.i 5 40 2 cGLNRecipient}
{syspar2.i 5 40 2 cQuilifierRecipient}
{syspara.i 5 40 3 iMSGSeqNr INT}
{syspara.i 5 40 4 cProd}
{syspara.i 5 40 4 cKontaktPerson}
{syspara.i 5 40 10 cEksportKatalog}

ASSIGN 
  iPRICATLinjeNr = 0
  cOrgUNB   = "UNB+UNOC:3+&GLNSender:&QuilifierSender+&GLNRecipient:&QuilifierRecipient+&Dato:&Tid+&PkSdlNr+++++0+&Prod'"
  cOrgUNH   = "UNH+&MSGSeqNr+&Type:D:01B:UN:EAN010'"
  cOrgBGM   = "BGM+&DocId+&MSGSeqNr+9'"
/*  cOrgDTM1  = "DTM+137:&Dato:203'"       */
/*  corgDTM2  = "DTM+194:&Dato:203'"       */
/*  cOrgDTM3  = "DTM+206:&Dato:203'"       */
/*  cDTM4     = "DTM+203:CCYYMMDDHHMM:203'"*/
  cOrgDTM1  = "DTM+137:&Dato:102'"
  corgDTM2  = "DTM+194:&Dato:102'" 
  cOrgDTM3  = "DTM+206:&Dato:102'"
  cDTM4     = "DTM+102:CCYYMMDD:102'"
  cOrgNADSu = "NAD+SU+&GLNSender::9'" 
  cOrgNADBy = "NAD+BY+&GLNRecipient::9'"
  cUNS      = "UNS+S'"
  cOrgCNT   = "CNT+2:&NbLines'"
  cOrgUNT   = "UNT+&NbSegments+&MSGSeqNr'"
  cOrgUNZ   = "UNZ+1+&MSGSeqNr'"
  cCUX      = "CUX+2:NOK:4'"
  cOrgLIN   = "LIN+1++&EAN:EN'"
  cOrgPIA   = "PIA+5+&LevKod:SA'"
  cOrgIMD_F = "IMD+F++:::&Varetekst:91'"
  cOrgIMD_F_BRN = "IMD+F+BRN+:::&Varemerke'"
  cOrgIMD_C = "IMD+C+98+&Storr::91'"
  cOrgIMD_F_35 = "IMD+F+35+:::&Farge'"
  cOrgQTY   = "QTY+&QtyType:&Enheter:PCE'"
  cOrgFTX   = "FTX+ZZZ+++&Tekst'"
  cOrgPRI   = "PRI+AAE:&Pris'"
  cOrgCTA   = "CTA+OC+:&KontaktPerson'"
  .

rStandardFunksjoner:SkrivTilLogg(cLogg,
  '  Start artprisPRICAT.p (Type ' + STRING(iType) + ')'
  ).

FIND FIRST ttArtPris NO-LOCK NO-ERROR.
IF NOT AVAILABLE ttArtPris THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Ingen prisendringer mottatt. Avslutter.'
    ).
  RETURN.
END.
/* Tidsperiode for normalpris. */
IF iType = 1 THEN /* Dagens dato og 999 dager. */ 
  DO:
    ASSIGN 
      /* Normalpris som skal aktiveres umiddelbart og vare i 3 år. */
      /* Dokumentets dato */
      cDato1   = STRING(YEAR(TODAY),"9999") + 
                 STRING(MONTH(TODAY),"99") +
                 STRING(DAY(TODAY),"99") /*+ 
                 REPLACE(STRING(TIME,"HH:MM"),':','')*/
      /* Aktiverings dato/tid for normalpris som skal aktiveres umiddelbart. */
      cDato2    = STRING(YEAR(TODAY),"9999") + 
                  STRING(MONTH(TODAY),"99") +
                  STRING(DAY(TODAY),"99") /*+ 
                  REPLACE(STRING(TIME,"HH:MM"),':','')*/
      /* Pris gyldig til dato 3 år frem i tid. */
      cDato3    = STRING(YEAR(TODAY) + 3,"9999") + 
                  STRING(MONTH(TODAY),"99") +
                  STRING(DAY(TODAY),"99") /*+
                  '2359' */
      .
  END.
ELSE IF iType = 2 THEN 
  DO:
    ASSIGN 
      /* Flagger at det er en tilbudspris som skal gjelde i en periode. */
      cOrgPRI   = "PRI+AAE:&Pris+CT+DPR'"
      /* Dokumentets dato */
      cDato1   = STRING(YEAR(TODAY),"9999") + 
                 STRING(MONTH(TODAY),"99") +
                 STRING(DAY(TODAY),"99") /*+ 
                 REPLACE(STRING(TIME,"HH:MM"),':','')*/
      /* Start på kampanjeperiode. */
      cDato2 = STRING(YEAR(ttArtPris.TilbudFraDato),"9999") + 
              STRING(MONTH(ttArtPris.TilbudFraDato),"99") +
              STRING(DAY(ttArtPris.TilbudFraDato),"99") /*+ 
              REPLACE(STRING(ttArtPris.TilbudFraTid,"HH:MM"),':','')*/
              
      /* Slutt på kampanjeperiode. */
      cDato3 = STRING(YEAR(ttArtPris.TilbudTilDato),"9999") + 
              STRING(MONTH(ttArtPris.TilbudTilDato),"99") +
              STRING(DAY(ttArtPris.TilbudTilDato),"99") /*+ 
              REPLACE(STRING(ttArtPris.TilbudTilTid,"HH:MM"),':','')*/
      .
  END.
ELSE IF itype = 3 THEN 
  DO:
    ASSIGN 
      /* Normalpris som skal aktiveres når tilbudsperioden avsluttes, og vare i 3 år. */
      /* Dokumentets dato */
      cDato1   = STRING(YEAR(TODAY),"9999") + 
                 STRING(MONTH(TODAY),"99") +
                 STRING(DAY(TODAY),"99") /*+ 
                 REPLACE(STRING(TIME,"HH:MM"),':','')*/
      .
      /* Start på aktiv periode. Direkte etter at tilbud er avsluttet. */
      /* Hvis tilbud avsluttes før døgnskifte. */
      IF INT(REPLACE(STRING(ttArtPris.TilbudTilTid,"HH:MM"),':','')) < 2359 THEN 
        cDato2 = STRING(YEAR(ttArtPris.TilbudTilDato),"9999") + 
                 STRING(MONTH(ttArtPris.TilbudTilDato),"99") +
                 STRING(DAY(ttArtPris.TilbudTilDato),"99") /*+ 
                 REPLACE(STRING(ttArtPris.TilbudTilTid,"HH:MM"),':','') */ .
      /* Hvis tilbud avsluttes ved døgnskifte. Da skal normalpris aktiveres etter døgnskiftet. */
      ELSE 
        cDato2 = STRING(YEAR(ttArtPris.TilbudTilDato + 1),"9999") + 
                 STRING(MONTH(ttArtPris.TilbudTilDato + 1),"99") +
                 STRING(DAY(ttArtPris.TilbudTilDato + 1),"99") /*+ 
                 '00:00' */ . 
              
      /* Prisen er gyldig til 3 år frem i tid. */
    ASSIGN 
      cDato3    = STRING(YEAR(TODAY) + 3,"9999") + 
                  STRING(MONTH(TODAY),"99") +
                  STRING(DAY(TODAY),"99") /*+
                  '2359' */ 
      .
  END.

ASSIGN 
  cDato = SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2) + 
          STRING(MONTH(TODAY),"99") +
          STRING(DAY(TODAY),"99")
  cTid  = REPLACE(STRING(TIME,'HH:MM'),':','')
  iMSGSeqNr = iMSGSeqNr + 1
  cUNA   = "UNA:+.? '" 
  cUNB   = cOrgUNB
  cUNH   = cOrgUNH
  cBGM   = cOrgBGM
  cNADSu = cOrgNADSu
  cNADBy = cOrgNADBy
  cCNT   = cOrgCNT
  cUNT   = cOrgUNT
  cUNZ   = cOrgUNZ
  NO-ERROR.
  
/* Legger tilbake oppdatert sekvensnr. */
{setsyspara.i 5 40 3 STRING(iMSGSeqNr)}

cUNB = REPLACE(cUNB,'&GLNSender',cGLNSender).  
cUNB = REPLACE(cUNB,'&QuilifierSender',cQuilifierSender).  
cUNB = REPLACE(cUNB,'&GLNRecipient',cGLNRecipient).  
cUNB = REPLACE(cUNB,'&QuilifierRecipient',cQuilifierRecipient).  
cUNB = REPLACE(cUNB,'&Dato',cDato).  
cUNB = REPLACE(cUNB,'&Tid',cTid).  
cUNB = REPLACE(cUNB,'&PkSdlNr',cDato).  
cUNB = REPLACE(cUNB,'&Prod',cProd).  
cUNH = REPLACE(cUNH,'&MSGSeqNr',STRING(iMSGSeqNr)). /* OK */ 
cUNH = REPLACE(cUNH,'&Type','PRICAT').  
cBGM = REPLACE(cBGM,'&MSGSeqNr',STRING(iMSGSeqNr)).
cBGM = REPLACE(cBGM,'&DocId','9').  
cDTM1  = REPLACE(cOrgDTM1,'&Dato',cDato1).
cDTM2  = REPLACE(cOrgDTM2,'&Datotype','194').
cDTM2  = REPLACE(cDTM2,'&Dato',cDato2).
cDTM3  = REPLACE(cOrgDTM3,'&Dato',cDato3).
cNADSu = REPLACE(cNADSu,'&GLNSender',cGLNSender).  
cNADBy = REPLACE(cNADBy,'&GLNRecipient',cGLNRecipient).  

/* Teller opp antall linjer. */
iNbLines = 0.
FOR EACH ttArtPris:
  iNbLines = iNbLines + 1.
END.
cCNT = REPLACE(cCNT,'&NbLines',STRING(iNbLines)).
cUNZ = REPLACE(cUNZ,'&MSGSeqNr',STRING(iMSGSeqNr)).

RUN genPRICAT.
RUN eksportPRICAT.

EMPTY TEMP-TABLE ttPRICAT.  
  
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '  Slutt artprisPRICAT.p'
  ).

/* **********************  Internal Procedures  *********************** */

PROCEDURE PRICATLinje:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcRow AS CHARACTER NO-UNDO.
  
  iPRICATLinjeNr = iPRICATLinjeNr + 1.
  CREATE ttPRICAT.
  ASSIGN 
    ttPRICAT.LinjeNr  = iPRICATLinjeNr
    ttPRICAT.Segments = pcRow
    . 

END PROCEDURE.

PROCEDURE skrivTilLogg:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '  Parametre:'
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    UNA: ' + cUNA
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    UNB: ' + cUNB
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    UNH: ' + cUNH
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    BGM: ' + cBGM
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    DTM: ' + cDTM1
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    DTM: ' + cDTM2
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    DTM: ' + cDTM3
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    DTM: ' + cDTM4
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    NAD: ' + cNADSu
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    NAD: ' + cNADBy
  ).

/* Her skal linjene inn. */  
  
/* Sluttblokk */  
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    UNS: ' + cUNS
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    CNT: ' + cCNT
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    UNT: ' + cUNT
  ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '    UNZ: ' + cUNZ
  ). 

END PROCEDURE.

PROCEDURE genPRICAT:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

RUN PRICATLinje (cUNA).
RUN PRICATLinje (cUNB).
RUN PRICATLinje (cUNH).
RUN PRICATLinje (cBGM).
RUN PRICATLinje (cDTM1).
RUN PRICATLinje (cDTM2).
RUN PRICATLinje (cDTM3).
RUN PRICATLinje (cDTM4).
RUN PRICATLinje (cNADSu).
RUN PRICATLinje (cNADBy).
RUN PRICATLinje (cCUX).

/* --------- Legg inn linjene her --------- */
LOOPEN:
FOR EACH ttArtPris:
  FIND ArtBas NO-LOCK WHERE 
    ArtBAs.ArtikkelNr = ttArtPris.ArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO: 
    FIND ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
      ArtPris.ProfilNr = ttArtPris.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
  END.
  ELSE RELEASE ArtPris.
  
  FIND HovedKategori NO-LOCK WHERE 
    HovedKategori.HovedKatNr = ArtBas.HovedKatNr NO-ERROR.
  FOR EACH Strekkode OF ArtBas NO-LOCK:        
    FIND StrKonv NO-LOCK WHERE 
      StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
    cLIN = REPLACE(cOrgLIN,'&EAN',Strekkode.Kode).
    RUN PRICATLinje (cLIN).
  
    cPIA = REPLACE(cOrgPIA,'&LevKod',IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE '').
    RUN PRICATLinje (cPIA).
    
    cIMD_F = REPLACE(cOrgIMD_F,'&Varetekst',IF AVAILABLE ArtBas THEN ArtBas.Beskr ELSE '').
    RUN PRICATLinje (cIMD_F).
  
    cIMD_F_BRN = REPLACE(cOrgIMD_F_BRN,'&Varemerke',IF AVAILABLE HovedKategori THEN HovedKategori.HovedKatTekst ELSE '').
    RUN PRICATLinje (cIMD_F_BRN).
  
    cIMD_C = REPLACE(cOrgIMD_C,'&Storr',IF AVAILABLE StrKonv THEN StrKonv.Storl ELSE '').
    RUN PRICATLinje (cIMD_C).
  
    cIMD_F_35 = REPLACE(cOrgIMD_F_35,'&Farge',IF AVAILABLE ArtBAs THEN ArtBas.LevFargKod ELSE '').
    RUN PRICATLinje (cIMD_F_35).
  
    cQTY = REPLACE(cOrgQTY,'&Enheter','1').
    cQTY = REPLACE(cQTY,'&QtyType','53').
    RUN PRICATLinje (cQTY).
  
    cFTX = REPLACE(cOrgFTX,'&Tekst','Prisendring ' + IF AVAILABLE ArtPris THEN TRIM(STRING(ArtPris.ArtikkelNr,">>>>>>>>>>>>9")) ELSE '').
    RUN PRICATLinje (cFTX).
  
    cPRI = REPLACE(cOrgPRI,'&Pris',IF AVAILABLE ArtPRIS THEN TRIM(REPLACE(STRING(ArtPris.Pris[1],">>>>>>>>9.99"),',','.')) ELSE '0.0').
    RUN PRICATLinje (cPRI).
  END.
END. /* LOOPEN */

/* ------------- Slutt linjer ------------- */
RUN PRICATLinje (cUNS).
RUN PRICATLinje (cCNT).
/* Teller opp antall segmenter. */
iNbSegments = 0.
FOR EACH ttPRICAT
  BY ttPRICAT.LinjeNr:
  IF ttPRICAT.Segments <> '' THEN 
    iNbSegments = iNbSegments + NUM-ENTRIES(ttPRICAT.Segments,'+').  
END.
cUNT = REPLACE(cUNT,'&NbSegments',STRING(iNbSegments)).
cUNT = REPLACE(cUNT,'&MSGSeqNr',STRING(iMSGSeqNr)).
RUN PRICATLinje (cUNT).
RUN PRICATLinje (cUNZ).

END PROCEDURE.

PROCEDURE eksportPRICAT:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  FIND FIRST ttPRICAT.
  IF NOT AVAILABLE ttPRICAT THEN 
    RETURN.
    
  ASSIGN 
/*    cPRICATFilNavn = cEksportKatalog + '\PRICAT' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.edi'*/
    cPRICATFilNavn = cEksportKatalog + '\PRICATNb' + STRING(iMSGSeqNr) + '.edi'
    .
  OUTPUT STREAM Ut TO VALUE(cPRICATFilNavn).
  FOR EACH ttPRICAT 
    BREAK BY ttPRICAT.LinjeNr:
      
    PUT STREAM Ut UNFORMATTED 
      ttPRICAT.Segments   
      SKIP.
  END.    
  OUTPUT STREAM Ut CLOSE.
  
END PROCEDURE.

  