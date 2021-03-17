
/*------------------------------------------------------------------------
    File        : EDIKommisjon.p
    Purpose     : Generering av de EDI filer som skal sendes Illums ved import av pakkseddel

    Syntax      :

    Description : Generering av PRICAT (Price Catalog) og DESADV (Despatch Advice)

    Author(s)   : Tom Nøkleby
    Created     : Sat Nov 28 10:43:56 CET 2020
    Notes       :
      
    PRICAT fil eksempel:
                    - UNA Default UNA:+.?*'. Dvs. at det her benyttes SPACE istedenfor STJERNE for repetition separation (value: "*" ). 
                  UNA:+.? '
                    UNB Interchange Header
                  UNB+UNOC:3+5702533000001:14+5790002192470:14+201110:1510+541+++++0+0'
                    UNB+UNOC:3+&GANTGLN:14+&IllumButGLN:14+&Dato:&Tid+&MSGSeqNr+++++0+0'
                  UNH+1+PRICAT:D:01B:UN:EAN010'
                    UNH+&MSGSeqNr+PRICAT:D:01B:UN:EAN010'
                  BGM+9+3184395+9'
                    BGM+9+&MSGSeqNr+9'
                  DTM+137:20201110:102'
                  DTM+194:20201110:102'
                  DTM+206:20210509:102'
                  NAD+SU+5702533000001::9'
                  NAD+BY+5790002192470::9'
                  CUX+2:DKK:8'
                    /* CUX anbefales ikke brukt ved innenlands handel */
                  LIN+1+1+5715021128903:EN'
                  PIA+5+041Y6258N_180_7:SA'
                  PIA+1+62033100:HS'
                  IMD+F++:::6269 - Star Napoli Normal:91'
                  IMD+F+BRN+:::GANT'
                  IMD+C+98+56::91'
                  IMD+F+35+:::Grey'
                  QTY+53:1:PCE'
                  FTX+PRD+1+SEA::91+04'
                  PRI+AAE:0.00'
                  CUX+2:DKK:4'
                  PRI+AAE:0.00'
                  CUX+2:NOK:4'
                  PRI+AAE:0.00'
                  CUX+2:SEK:4'
                  PRI+AAE:0.00'
                  CUX+2:EUR:4'
                  UNS+S'
                  CNT+2:2'
                  UNT+45+1'
                  UNZ+1+541'      
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER lPkSdlId AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER cLogg AS CHARACTER NO-UNDO.

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
DEFINE VARIABLE iDESADVLinjeNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cKontaktPerson AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEksportKatalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE cImportKatalog AS CHARACTER NO-UNDO.
DEFINE VARIABLE iGantAktiv AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lLC% AS DECIMAL NO-UNDO.
DEF VAR bStdPrisOverf AS LOG  NO-UNDO.

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

DEFINE TEMP-TABLE ttDESADV NO-UNDO
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
{syspara.i 210 100 8 iGantAktiv INT}
/* Er normalt satt til 45%. */
{syspara.i 210 100 10 lLC% DEC}
{syspara.i 5 26 1 bStdPrisOverf LOGICAL}

ASSIGN 
  iPRICATLinjeNr = 0
  iDESADVLinjeNr = 0
  cOrgUNB   = "UNB+UNOC:3+&GLNSender:&QuilifierSender+&GLNRecipient:&QuilifierRecipient+&Dato:&Tid+&PkSdlNr+++++0+&Prod'"
  cOrgUNH   = "UNH+&MSGSeqNr+&Type:D:01B:UN:EAN010'"
  cOrgBGM   = "BGM+&DocId+&MSGSeqNr+9'"
/*  cOrgDTM1  = "DTM+137:&Dato:203'"      */
/*  corgDTM2  = "DTM+&Datotype:&Dato:203'"*/
/*  cOrgDTM3  = "DTM+206:&Dato:203'"      */
/*  cDTM4     = "DTM+203:CCYYMMDDHHMM:203'"*/
  cOrgDTM1  = "DTM+137:&Dato:102'"
  corgDTM2  = "DTM+&Datotype:&Dato:102'" 
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
  '  Start EDIKommisjon.p'
  ).

FIND PkSdlHode NO-LOCK WHERE 
  PkSdlHode.PkSdlId = lPkSdlId NO-ERROR.
IF NOT AVAILABLE PkSdlHode THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Ukjent pakkseddel PkSdlId: '  + STRING(lPkSdlId) + '. Avbryter.'
    ).
  RETURN.
END.

FIND Butiker NO-LOCK WHERE 
  Butiker.butik = PkSdlHode.ButikkNr NO-ERROR.
IF NOT AVAILABLE Butiker THEN 
DO:
  rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    Ukjent butikk på pakkseddel PkSdlId: '  + STRING(lPkSdlId) + '. Avbryter.'
    ).
  RETURN.
END.

ASSIGN 
  cDato = SUBSTRING(STRING(YEAR(PkSdlHode.RegistrertDato),"9999"),3,2) + 
          STRING(MONTH(PkSdlHode.RegistrertDato),"99") +
          STRING(DAY(PkSdlHode.RegistrertDato),"99")
  cTid  = REPLACE(STRING(PkSdlHode.RegistrertTid,'HH:MM'),':','')
  iMSGSeqNr = iMSGSeqNr + 1
  cUNA   = "UNA:+.? '" 
  cUNB   = cOrgUNB
  cUNH   = cOrgUNH
  cBGM   = cOrgBGM
  cDato1 = STRING(YEAR(TODAY),"9999") + 
          STRING(MONTH(TODAY),"99") +
          STRING(DAY(TODAY),"99") /*+ 
          REPLACE(STRING(TIME,"HH:MM"),':','')*/
  cDato2 = STRING(YEAR(TODAY),"9999") + 
          STRING(MONTH(TODAY),"99") +
          STRING(DAY(TODAY),"99") /*+ 
          REPLACE(STRING(TIME,"HH:MM"),':','')*/
  cDato3 = STRING(YEAR(TODAY) + 3,"9999") + 
          STRING(MONTH(TODAY),"99") +
          STRING(DAY(TODAY),"99") /*+
          REPLACE(STRING(TIME,"HH:MM"),':','')*/
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
cUNB = REPLACE(cUNB,'&PkSdlNr',PkSdlHode.PkSdlNr).  
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
/* Teller opp antall linjer i pakkseddelen. */
iNbLines = 0.
FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
  iNbLines = iNbLines + 1.
END.
cCNT = REPLACE(cCNT,'&NbLines',STRING(iNbLines)).
cUNZ = REPLACE(cUNZ,'&MSGSeqNr',STRING(iMSGSeqNr)).

/* Sjekker at artikkelens kalkyle er satt med LC pris på kommisjonsbutikkenes prisprofil.      */
/* Regner også om pakkseddelen, slik at denne ligger med ritige priser for kommisjonsbutikken. */
RUN sjekkLC. 

/* Bare forward ordre skal legge ut PRICAT. */
IF CAN-DO('1,12',PksdlHode.OrdreType) THEN 
DO:
  RUN genPRICAT.
  RUN eksportPRICAT.
END.

/* Utlegg av pakkseddel til kommisjonsbutikk. */
RUN genDESADV.
RUN eksportDESADV.

EMPTY TEMP-TABLE ttPRICAT.  
EMPTY TEMP-TABLE ttDESADV.  
  
rStandardFunksjoner:SkrivTilLogg(cLogg,
  '  Slutt EDIKommisjon.p'
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
FOR EACH PkSdlPris OF PkSdlHode NO-LOCK:
  FIND ArtBas NO-LOCK WHERE 
    ArtBAs.ArtikkelNr = PkSdlPris.ArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO: 
    FIND ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
      ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
  END.
  ELSE RELEASE ArtPris.
  FIND StrKonv NO-LOCK WHERE 
    StrKonv.StrKode = PkSdlLinje.StrKode NO-ERROR.
  FIND HovedKategori NO-LOCK WHERE 
    HovedKategori.HovedKatNr = ArtBas.HovedKatNr NO-ERROR.
         
  cLIN = REPLACE(cOrgLIN,'&EAN',IF AVAILABLE PkSdlLinje THEN PkSdlLinje.Kode ELSE '').
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

  cFTX = REPLACE(cOrgFTX,'&Tekst','Pakkseddel ' + STRING(INT(PkSdlHode.PkSdlNr))).
  RUN PRICATLinje (cFTX).

  cPRI = REPLACE(cOrgPRI,'&Pris',IF AVAILABLE ArtPRIS THEN TRIM(REPLACE(STRING(ArtPris.Pris[1],">>>>>>>>9.99"),',','.')) ELSE '0.0').
  RUN PRICATLinje (cPRI).
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
/*    cPRICATFilNavn = 'konv\PRICAT' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.edi'*/
/*    cPRICATFilNavn = cEksportKatalog + '\PRICATNb' + PkSdlHode.PkSdlNr + '.edi'*/
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

PROCEDURE genDESADV:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
                  UNA:+.? '
                  UNB+UNOC:3+5702533000001:14+5790002192470:14+201110:1510+541+++++0+0'
                  UNH+1+PRICAT:D:01B:UN:EAN010'
                  BGM+9+3184395+9'
                  DTM+137:20201110:102'
                  DTM+194:20201110:102'
                  DTM+206:20210509:102'
                  NAD+SU+5702533000001::9'
                  NAD+BY+5790002192470::9'
                  CUX+2:DKK:8'
                  LIN+1+1+5715021128903:EN'
                  PIA+5+041Y6258N_180_7:SA'
                  PIA+1+62033100:HS'
                  IMD+F++:::6269 - Star Napoli Normal:91'
                  IMD+F+BRN+:::GANT'
                  IMD+C+98+56::91'
                  IMD+F+35+:::Grey'
                  QTY+53:1:PCE'
                  FTX+PRD+1+SEA::91+04'
                  PRI+AAE:0.00::SRP'
                  CUX+2:DKK:4'
                  PRI+AAE:0.00::SRP'
                  CUX+2:NOK:4'
                  PRI+AAE:0.00::SRP'
                  CUX+2:SEK:4'
                  PRI+AAE:0.00::SRP'
                  CUX+2:EUR:4'
                  UNS+S'
                  CNT+2:2'
                  UNT+45+1'
                  UNZ+1+541'      
   
------------------------------------------------------------------------------*/
RUN DESADVLinje (cUNA).
RUN DESADVLinje (cUNB).
cUNH = cOrgUNH.
cUNH = REPLACE(cUNH,'&MSGSeqNr',STRING(iMSGSeqNr)).  
cUNH = REPLACE(cUNH,'&Type','DESADV').  
RUN DESADVLinje (cUNH).
cBGM = cOrgBGM.
cBGM = REPLACE(cBGM,'&MSGSeqNr',STRING(iMSGSeqNr)).
cBGM = REPLACE(cBGM,'&DocId','351').  
RUN DESADVLinje (cBGM).
RUN DESADVLinje (cDTM1).
cDTM2 = cOrgDTM2.
cDTM2  = REPLACE(cDTM2,'&Datotype','35').
cDTM2  = REPLACE(cDTM2,'&Dato',cDato2).
RUN DESADVLinje (cDTM2).
RUN DESADVLinje (cDTM4).
RUN DESADVLinje (cNADBy).
cCTA = cOrgCTA.
cCTA  = REPLACE(cCTA,'&KontaktPerson',cKontaktPerson).
RUN DESADVLinje (cCTA).
RUN DESADVLinje (cNADSu).

/* --------- Legg inn linjene her --------- */
LOOPEN:
FOR EACH PkSdlLinje OF PkSdlHode NO-LOCK:
  FIND ArtBas NO-LOCK WHERE 
    ArtBAs.ArtikkelNr = PkSdlLinje.ArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
  DO: 
    FIND ArtPris NO-LOCK WHERE 
      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
      ArtPris.ProfilNr = Butiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE ArtPris THEN 
      FIND FIRST ArtPris OF ArtBas NO-LOCK NO-ERROR.
  END.
  ELSE RELEASE ArtPris.
  FIND StrKonv NO-LOCK WHERE 
    StrKonv.StrKode = PkSdlLinje.StrKode NO-ERROR.
  FIND HovedKategori NO-LOCK WHERE 
    HovedKategori.HovedKatNr = ArtBas.HovedKatNr NO-ERROR.
         
  cLIN = REPLACE(cOrgLIN,'&EAN',IF AVAILABLE PkSdlLinje THEN PkSdlLinje.Kode ELSE '').
  RUN DESADVLinje (cLIN).

  cPIA = REPLACE(cOrgPIA,'&LevKod',IF AVAILABLE ArtBas THEN ArtBas.LevKod ELSE '').
  RUN DESADVLinje (cPIA).
  
  cQTY = REPLACE(cOrgQTY,'&Enheter',STRING(INT(PkSdlLinje.Antall))).
  cQTY = REPLACE(cQTY,'&QtyType','12').
  RUN DESADVLinje (cQTY).
  
END. /* LOOPEN */

/* ------------- Slutt linjer ------------- */
RUN DESADVLinje (cUNS).
RUN DESADVLinje (cCNT).
/* Teller opp antall segmenter. */
iNbSegments = 0.
FOR EACH ttDESADV
  BY ttDESADV.LinjeNr:
  IF ttDESADV.Segments <> '' THEN 
    iNbSegments = iNbSegments + NUM-ENTRIES(ttDESADV.Segments,'+').  
END.
cUNT = REPLACE(cUNT,'&NbSegments',STRING(iNbSegments)).
cUNT = REPLACE(cUNT,'&MSGSeqNr',STRING(iMSGSeqNr)).
RUN DESADVLinje (cUNT).
RUN DESADVLinje (cUNZ).

END PROCEDURE.

PROCEDURE eksportDESADV:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  FIND FIRST ttDESADV.
  IF NOT AVAILABLE ttDESADV THEN 
    RETURN.
    
  ASSIGN 
    cDESADVFilNavn = cEksportKatalog + '\DESADV' + PkSdlHode.PkSdlNr + '.edi'
    .
  OUTPUT STREAM Ut TO VALUE(cDESADVFilNavn).
  FOR EACH ttDESADV 
    BREAK BY ttDESADV.LinjeNr:
      
    PUT STREAM Ut UNFORMATTED 
      ttDESADV.Segments   
      SKIP.
  END.    
  OUTPUT STREAM Ut CLOSE.


END PROCEDURE.

PROCEDURE DESADVLinje:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER pcRow AS CHARACTER NO-UNDO.
  
  iDESADVLinjeNr = iDESADVLinjeNr + 1.
  CREATE ttDESADV.
  ASSIGN 
    ttDESADV.LinjeNr  = iDESADVLinjeNr
    ttDESADV.Segments = pcRow
    . 


END PROCEDURE.

PROCEDURE sjekkLC:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE VARIABLE plLC AS DECIMAL NO-UNDO.
DEFINE VARIABLE plMva% AS DECIMAL NO-UNDO.
DEFINE VARIABLE pfMvaKr AS DECIMAL NO-UNDO.
DEFINE VARIABLE pfDbKr  AS DECIMAL NO-UNDO.

DEFINE BUFFER bufPkSdlPris FOR PkSdlPris.

IF iGantAktiv = 1 AND 
  PkSdlHode.butikkNr >= 10100 AND 
  PkSdlHode.ButikkNr <= 10999 THEN . /* Gjør ingenting her. Rutinen skal kjøres. */
ELSE /* Avbryter. rutinen skal ikke kjrøes. */
  RETURN.

LINJE:  
FOR EACH PkSdlPris OF PkSdlHode NO-LOCK:
  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = PkSdlPris.ArtikkelNr NO-ERROR.
  IF AVAILABLE ArtBas THEN
  ARTIKKEL:
  DO TRANSACTION:
    FIND ArtPris EXCLUSIVE-LOCK WHERE 
      ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND 
      ArtPris.ProfilNr = Butiker.ProfilNr NO-WAIT NO-ERROR.
    IF AVAILABLE ArtPris AND NOT LOCKED ArtPris THEN
    KORRIGERING_ARTPRIS: 
    DO:
      IF ArtBas.KjedeInnkPris > 0 THEN 
        plLC = ROUND(ArtBas.KjedeInnkPris,0).
      ELSE 
        plLC = ROUND((ArtPris.InnkjopsPris[1] * lLC%) / 100,0). 
      plMva% = ArtPris.Mva%[1].
      
      ASSIGN 
        ArtPris.InnkjopsPris[1] = plLC 
        ArtPris.ValPris[1]      = plLC
        ArtPris.Rab1%[1]        = 0
        ArtPris.Rab1Kr[1]       = 0
        ArtPris.VareKost[1]     = plLC 
        ArtPris.MvaKr[1]        = ArtPris.Pris[1] - ROUND((ArtPris.Pris[1] / (1 + (plMva% / 100))),2)
        ArtPris.DbKr[1]         = ArtPris.Pris[1] - ArtPris.MvaKr[1] - ArtPris.VareKost[1]
        ArtPris.Db%[1]          = ROUND(
                                     (ArtPris.DbKr[1] * 100) / (ArtPris.Pris[1] - ArtPris.MvaKr[1])
                                     ,2) 
        ArtPris.Db%[1]          = IF ArtPris.Db%[1] = ? THEN 0 ELSE ArtPris.Db%[1]
        .
      
      FIND bufPkSdlPris EXCLUSIVE-LOCK WHERE 
        ROWID(bufPkSdlPris) = ROWID(PkSdlPris) NO-ERROR.
      IF AVAILABLE bufPkSdlPris THEN
      KORRPKSDLPRIS: 
      DO:
        ASSIGN 
          bufPkSdlPris.VareKost       = ArtPris.VareKost[1]
          bufPkSdlPris.Rab1%          = ArtPris.Rab1%[1]
          bufPkSdlPris.Pris           = ArtPris.Pris[1]
          bufPkSdlPris.Frakt          = ArtPris.Frakt[1]
          bufPkSdlPris.Db%            = ArtPris.Db%[1]
          bufPkSdlPris.InnkjopsPris   = ArtPris.InnkjopsPris[1]
          /* Ny varekost som skal gjelde i butikken */
          bufPkSdlPris.NyVarekost     = plLC 
          bufPkSdlPris.NyRab1%        = 0
          bufPkSdlPris.NyInnkjopsPris = plLC
          bufPkSdlPris.NyFrakt        = 0
          pfMvaKr                     = bufPkSdlPris.NyPris - (bufPkSdlPris.NyPris / (1 + (plMva% / 100)))
          pfDbKr                      = bufPkSdlPris.NyPris - pfMvaKr - bufPkSdlPris.NyVarekost                   
          bufPkSdlPris.NyDB%          = ROUND((pfDbKr * 100) / (bufPkSdlPris.NyPris - pfMvaKr),2)
          bufPkSdlPris.NyDB%          = IF bufPkSdlPris.NyDB% = ? THEN 0 ELSE bufPkSdlPris.NyDB%
          bufPkSdlPris.OverstyrPris   = YES
          .
        RELEASE bufPkSdlPris. 
      END.  
      RELEASE ArtPris.
    END. /* KORRIGERING_ARTPRIS */
  END. /* ARTIKKEL TRANSACTION */  
END. /* KORRPKSDLPRIS */

END PROCEDURE.

  