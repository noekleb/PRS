/* Aktiver VPI for varebok 
   Overfører varebok direkte til HK's VPI register. Slik at det blir unødvendig 
   å gjøre dette via lokalt artikkelregsiter på HK.
   Parametere:  Vareboknr;evt liste over artikler
   
   Opprettet: 18.06.07 av TN 
   Endret:    
            
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery            AS HANDLE NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR bOK               AS LOG    NO-UNDO.
DEF VAR hPrisKo           AS HANDLE NO-UNDO.
DEF VAR iAntSlett         AS INT    NO-UNDO.
DEF VAR iAntNyEndre       AS INT    NO-UNDO.

DEF VAR iAntall           AS INT    NO-UNDO.
DEF VAR i2Antall          AS INT    NO-UNDO.
DEF VAR iUAntall          AS INT    NO-UNDO.

DEF VAR iEkstVPILevNr     AS INT    NO-UNDO.
DEF VAR iSend             AS INT    NO-UNDO.
DEF VAR cButList          AS CHAR   NO-UNDO.
DEF VAR cEDBSystem        AS CHAR   NO-UNDO.

DEF BUFFER bArtBas FOR ArtBas.

ASSIGN
    iEkstVPILevNr = 1
    iSend         = INT(ENTRY(3,icParam,";")) 
    cButList      = replace(ENTRY(4,icParam,";"),"|",",")
    cEDBSystem    = "POS" + STRING(TIME).

FIND FIRST VarebokHode WHERE VarebokHode.VarebokNr = DEC(ENTRY(1,icParam,";")) NO-LOCK NO-ERROR.
IF NOT AVAIL VarebokHode THEN DO:
  ocReturn = "Finner ikke varebok: " + ENTRY(1,icParam,";").
  RETURN.
END.

IF SEARCH("prisko.p") NE ? OR SEARCH("prisko.r") NE ? THEN
  RUN prisko.p PERSIST SET hPrisKo.
ELSE DO:
  ocReturn = "Finner ikke program prisko.p".
  RETURN.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(BUFFER VarebokLinje:HANDLE).
obOk = hQuery:QUERY-PREPARE("FOR EACH VarebokLinje NO-LOCK WHERE VarebokNr = " 
                           + ENTRY(1,icParam,";")  /* Vareboknr */
                           + (IF ENTRY(2,icParam,";") <> '' THEN ENTRY(2,icParam,";") ELSE "")  /* Evt. liste over artikler (can-do) */
                           ) NO-ERROR.
IF NOT obOk THEN DO:
  ocReturn = "Denne spørringen blir ikke godtatt: " + CHR(10) + "FOR EACH VarebokLinje NO-LOCK WHERE VarebokNr = " + ENTRY(1,icParam,";") + " BY " + ENTRY(3,icParam,";").
  RETURN.
END.

hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    
  FIND FIRST ArtBas WHERE ArtBas.ArtikkelNr = VarebokLinje.ArtikkelNr NO-LOCK NO-ERROR.

  IF AVAIL ArtBas THEN 
      RUN OverforTilVPI.
           .
  IF NOT obOK THEN
    UNDO, LEAVE.

  hQuery:GET-NEXT().
END.

DELETE OBJECT hQuery.

IF iAntall > 0 AND iSend = 1 THEN
DO:
    RUN vpieksport.w (cEDBSystem,
                      cButList, /* Ekstent skal være butikk som skal ha filen. */
                      1, /* Bildedata skal sendes med */
                      OUTPUT iAntSlett,
                      OUTPUT iAntNyEndre).
END.

IF ocReturn = "" THEN obOk = TRUE.
IF obOk THEN
DO:
    ocReturn = "Antall artikler overført til HK's VPI register er " + STRING(iAntall) + ".".
    IF i2Antall > 0 THEN
        ocReturn = ocReturn + CHR(13) + "Antall ikke overført " + STRING(i2Antall) + " (Ikke HK varer).". 
    IF iUAntall > 0 THEN
        ocReturn = ocReturn + CHR(13) + "Antall uten utpris eller innpris - ikke overført " + STRING(iUAntall) + ".". 
END.

IF VALID-HANDLE(hPrisKo) THEN
    DELETE PROCEDURE hPrisKo.

PROCEDURE OverforTilVPI:
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: Tilpasset fra prosedyre LagreKalkyle i w-kalkyle.w 
------------------------------------------------------------------------------*/

  def var wSkjerm      AS CHAR  NO-UNDO.
  
  DEF VAR pcError      AS CHAR  NO-UNDO.
  DEF VAR wArtBasRecid AS RECID NO-UNDO.
  
  /* Bare artikler som er opprettet på HK skal overføres til VPI register.*/
/*   IF ArtBas.ArtikkelNr < 8500000 THEN */
/*   DO:                                 */
/*       i2Antall = i2Antall + 1.        */
/*       RETURN.                         */
/*   END.                                */

  /* Artikler uten innkjøps og/eller salgspris til butikk, skal ikke kunne legges over. */
  IF (VarebokLinje.Pris <= 0 OR VarebokLinje.InnkjopsPris <= 0) THEN
  DO:
      iUAntall = iUAntall + 1.
      RETURN.
  END.

  ASSIGN 
      wArtBasRecid = RECID(ArtBas)
      iAntall      = iAntall + 1
      .

  FIND VPIArtBas WHERE VPIArtBas.EkstVPILevNr = iEkstVPILevNr AND
                       VPIArtBas.VareNr       = string(ArtBas.ArtikkelNr) NO-ERROR.

  /* Renser bort VPI hvis den finnes fra før */
  IF AVAIL VPIArtBas THEN DO:
      FOR EACH VPIArtPris OF VPIArtBas:
          DELETE VPIArtPris.
      END.
      FIND VPIBilderegister WHERE VPIBilderegister.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
                                  VPIBilderegister.VareNr       = VPIArtBas.VareNr       AND
                                  VPIBilderegister.BildNr       = VPIArtBas.BildNr NO-ERROR.
      IF AVAIL VPIBilderegister THEN DO:
          FOR EACH VPIBildeData OF VPIBilderegister:
              DELETE VPIBildeData.
          END.
          DELETE VPIBilderegister.
      END.
      FOR EACH VPIStrekKode OF VPIArtBas:
          DELETE VPIStrekKode.
      END.
      FOR EACH VPIAltLevBas where
          VPIAltLevBas.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
          VPIAltLevBas.VareNr       = VPIArtBas.VareNr:
          DELETE VPIAltLevBas.
      END.
      FOR EACH VPIArtBestPkt OF VPIArtBas:
          DELETE VPIArtBestPkt.
      END.
      DELETE VPIArtBas.
  END.

  IF AVAIL ArtBas THEN DO:
      CREATE VPIArtBas.
      BUFFER-COPY ArtBas 
          EXCEPT KatalogPris ForhRab%
          TO VPIArtBas
          ASSIGN VPIArtBas.EkstVPILevNr   = iEkstVPILevNr
                 VPIArtBas.VareNr         = string(ArtBas.ArtikkelNr)
                 VPIArtBas.KatalogPris[1] = ArtBas.KatalogPris
                 VPIArtBas.forhRab%[1]    = ArtBas.forhRab%
                 VPIArtBas.suppRab%[1]    = ArtBas.supRab%
                 VPIArtBas.LopNr          = ?
                 .
      /* Informasjonen overstyres med det som hentes fra Vareboken. */
      ASSIGN
          VPIArtBas.LinjeMerknad  = VareBokLinje.LinjeMerknad
          VPIArtBas.Katalogpris   = VarebokLinje.Katalogpris 
          VPIArtBas.Beskr         = VarebokLinje.Beskr       
          VPIArtBas.LevFargKod    = VarebokLinje.LevFargKod  
          VPIArtBas.AnbefaltPris  = VarebokLinje.AnbefaltPris
          VPIArtBas.ForhRab%      = VarebokLinje.ForhRab%    
          VPIArtBas.SuppRab%      = VarebokLinje.SupRab%     
          /*VPIArtBas.KjedeInnkPris = 0*/
          /*VPIArtBas.KjedeRab%     = 0*/
          VPIArtBas.LevDato1      = VarebokLinje.LevDato1
          VPIArtBas.LevDato2      = VarebokLinje.LevDato2
          VPIArtBas.LevDato3      = VarebokLinje.LevDato3
          VPIArtBas.LevDato4      = VarebokLinje.LevDato4
          .

      STREKKODE:
      FOR EACH StrekKode OF ArtBas NO-LOCK:
          /* 02 koder skal ikke sendes ut. */
          IF Strekkode.Kode BEGINS "02" AND
              LENGTH(Strekkode.Kode) = 13 THEN
              NEXT STREKKODE.

          CREATE VPIStrekKode.
          BUFFER-COPY StrekKode TO VPIStrekKode
              ASSIGN VPIStrekKode.EkstVPILevNr = iEkstVPILevNr  
                     VPIStrekKode.VareNr       = string(ArtBas.ArtikkelNr).
      END. /* STREKKODE */
      FOR EACH ArtPris OF ArtBas NO-LOCK.
          CREATE VPIArtPris.
          /* Priser skal inn fra varebok, ikke kopieres fra artikkelkortet. 
          BUFFER-COPY ArtPris TO VPIArtPris
              ASSIGN VPIArtPris.EkstVPILevNr = iEkstVPILevNr 
                     VPIArtPris.VareNr       = string(ArtBas.ArtikkelNr)
                     VPIArtPris.Tilbud       = FALSE.
          */
          ASSIGN
              VPIArtPris.EkstVPILevNr = iEkstVPILevNr
              VPIArtPris.VareNr       = string(ArtBas.ArtikkelNr)
              VPIArtPris.ProfilNr     = ArtPris.ProfilNr
              VPIArtPris.Tilbud       = FALSE.
                     
          /* Oppdaterer kalkyle fra Varebok */
          ASSIGN
              VPIArtPris.ValPris[1]      = VarebokLinje.InnkjopsPris 
              VPIArtPris.InnkjopsPris[1] = VarebokLinje.InnkjopsPris 
              VPIArtPris.Rab1Kr[1]       = (VarebokLinje.InnkjopsPris * VarebokLinje.supRab%) / 100
              VPIArtPris.Rab1%[1]        = VarebokLinje.supRab%     
              VPIArtPris.Varekost[1]     = VarebokLinje.supVareKost 
              VPIArtPris.MvaKr[1]        = VarebokLinje.Mva    
              VPIArtPris.Mva%[1]         = VarebokLinje.Mva%   
              VPIArtPris.DbKr[1]         = VarebokLinje.supDBkr
              VPIArtPris.Db%             = VarebokLinje.supDB% 
              VPIArtPris.Pris[1]         = VarebokLinje.Pris   
              VPIArtPris.AktivFraDato    = TODAY 
              VPIArtPris.AktivFraTid     = TIME - 10
              .
      END.
      FOR EACH AltLevBas NO-LOCK WHERE
          AltLevBas.ArtikkelNr = ArtBas.ArtikkelNr AND 
          AltLevBas.LevNr      > 0:
          CREATE VPIAltLevBas.
          BUFFER-COPY AltLevBas TO VPIAltLevBas
              ASSIGN VPIAltLevBas.EkstVPILevNr = iEkstVPILevNr 
                     VPIAltLevBas.VareNr       = string(ArtBas.ArtikkelNr)
              NO-ERROR.
      END.
      FOR EACH ArtBestPk OF ArtBas NO-LOCK:
          CREATE VPIArtBestPk.
          BUFFER-COPY ArtBestPk TO VPIArtBestPk
              ASSIGN VPIArtBestPk.EkstVPILevNr = iEkstVPILevNr 
                     VPIArtBestPk.VareNr       = string(ArtBas.ArtikkelNr)
              NO-ERROR.
      END.
      IF ArtBas.BildNr > 0 THEN
          FIND BildeRegister OF ArtBas NO-LOCK NO-ERROR.
      ELSE DO:
          IF AVAILABLE BildeRegister THEN
              RELEASE BildeRegister.
      END.
      
      IF AVAIL BildeRegister  AND 
          (NOT CAN-find(VPIBilderegister WHERE VPIBilderegister.EkstVPILevNr = VPIArtBas.EkstVPILevNr AND
                                      VPIBilderegister.VareNr       = VPIArtBas.VareNr AND
                                      VPIBilderegister.BildNr       = VPIArtBas.BildNr))
          THEN 
      DO:
          CREATE VPIBildeRegister.
          BUFFER-COPY BildeRegister TO VPIBilderegister
              ASSIGN VPIBilderegister.EkstVPILevNr = iEkstVPILevNr  
                     VPIBilderegister.VareNr       = string(ArtBas.ArtikkelNr).

          FOR EACH BildeData OF BildeRegister NO-LOCK:
              CREATE VPIBildeData.
              BUFFER-COPY BildeData TO VPIBildeData
              ASSIGN VPIBildeData.EkstVPILevNr = iEkstVPILevNr  
                     VPIBildeData.VareNr       = string(ArtBas.ArtikkelNr).
          END.
      END.
      ELSE
          VPIArtBas.BildNr = 0.
      /* Logger for sending til butikk */
      IF iSend = 1 THEN
           RUN create_elogg.p ('VPIArtBas',cEDBSystem,string(VPIArtBas.EkstVPILevNr) + CHR(1) + VPIArtBas.VareNr).

  END.
END PROCEDURE.


