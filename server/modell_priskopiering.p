/* Kopierer en prisprofil til en liste med profiler.
   Parameter:  
   Opprettet:               
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cLogg         AS CHARACTER NO-UNDO.
DEFINE VARIABLE bok AS LOG NO-UNDO.
DEFINE VARIABLE iProfilNr AS INT NO-UNDO.
DEFINE VARIABLE iInt AS INTEGER NO-UNDO.
DEFINE VARIABLE iLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE cProfilLst AS CHARACTER NO-UNDO.
DEFINE VARIABLE lArtikkelNr AS DECIMAL NO-UNDO.
DEFINE VARIABLE iAnt AS INTEGER NO-UNDO.
DEFINE VARIABLE lRab%         AS DECIMAL FORMAT "->,>>9.99" NO-UNDO.
DEFINE VARIABLE lORab%        AS DECIMAL FORMAT "->,>>9.99" NO-UNDO.
DEFINE VARIABLE lURab%        AS DECIMAL FORMAT "->,>>9.99" NO-UNDO.
DEFINE VARIABLE lMva%         AS DECIMAL NO-UNDO.
DEFINE VARIABLE ohBuffer AS HANDLE NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKomisjonsProfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE lLC% AS DECIMAL NO-UNDO.

DEFINE TEMP-TABLE ttArtPris LIKE ArtPris.

DEFINE BUFFER bufArtPris FOR ArtPris.

/* Er normalt satt til 45%. */
{syspara.i 210 100 10 lLC% DEC}

ASSIGN 
    cLogg = 'modell_priskopiering' + REPLACE(STRING(TODAY),'/','')
    cKomisjonsProfil = '100'
    .
DEFINE VARIABLE rStandardFunksjoner AS cls.StdFunk.StandardFunksjoner NO-UNDO.
rStandardFunksjoner  = NEW cls.StdFunk.StandardFunksjoner( cLogg ) NO-ERROR.

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

rStandardFunksjoner:SkrivTilLogg(cLogg,
    'Start.' 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  Parameter:' 
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    icParam: ' + icParam
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    lRab%: ' + STRING(lRab%)
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    lORab%: ' + STRING(lORab%)
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    lURab%: ' + STRING(lURab%)
    ).

IF icParam = '' THEN 
  RETURN 'Mangelfull parameteroppsett!'.
                                  
ASSIGN
  lArtikkelNr = DEC(ENTRY(1,icParam,'|'))
  iProfilNr  = INT(ENTRY(2,icParam,'|'))
  cProfilLst = ENTRY(3,icParam,'|')
  NO-ERROR.
IF ERROR-STATUS:ERROR THEN 
  RETURN 'Feil i parameteroppsett (' + icParam + ').'.  

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    lArtikkelNr: ' + STRING(lArtikkelNr)
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    iProfilNr: ' + STRING(iProfilNr)
    ).
rStandardFunksjoner:SkrivTilLogg(cLogg,
    '    cProfilLst: ' + cProfilLst
    ).

FIND ArtPris NO-LOCK WHERE
  ArtPris.ArtikkelNr = lArtikkelNr AND  
  ArtPris.ProfilNr   = iProfilNr NO-ERROR.
IF NOT AVAILABLE ArtPris THEN 
  RETURN 'Ukjent pris valgt (' + STRING(iProfilNr) + ').'.  
  
ocReturn = ''.
DO iLoop = 1 TO NUM-ENTRIES(cProfilLst):
  iInt = INT(ENTRY(iLoop,cProfilLst)).

  rStandardFunksjoner:SkrivTilLogg(cLogg,
      '  Kopierer fra ' + STRING(ArtPris.ProfilNr) + ' til ' + STRING(iInt) + '.'
      ).
  
  FIND bufArtPris NO-LOCK WHERE
    bufArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND  
    bufArtPris.ProfilNr   = iInt NO-ERROR.
  IF NOT AVAILABLE bufArtPris THEN
  BUFFERCOPY: 
  DO:
    /* Kopierer kalkyle. NB: Slår alltid av eventuelt tilbud. */
    CREATE bufArtPris.
    BUFFER-COPY ArtPris
      EXCEPT ProfilNr Tilbud
      TO bufArtPris
      ASSIGN 
        bufArtPris.ProfilNr = iInt
        bufArtPris.Tilbud   = FALSE 
        .
    iAnt = iAnt + 1.
    
    /* Mottagende profil skal ha kopi av outlet pris, omregnet med normal rabatt og utpris. */
    IF ArtPris.ProfilNr = 2 AND bufArtPris.ProfilNr <> 2 THEN 
      RUN setPrisFraOutlet.
    /* Kopierer fra butikk til Outlet. Her skal Outlet raabatt og pris settes. */  
    ELSE IF bufArtPris.ProfilNr = 2 THEN 
      RUN setOutletPris.
    /* Kopiere til komisjonsbutikkenes profil. */  
    ELSE IF CAN-DO(cProfilLst,STRING(bufArtPris.ProfilNr)) THEN 
      RUN setKomisjonsPris.
    /* Fra til prisprofil med samme rabatt og pris */
    ELSE. /* Gjør ingenting. Mottagende butikk skal ha samme pris. */ 
    
  END. /* BUFFERCOPY*/
END.

rStandardFunksjoner:SkrivTilLogg(cLogg,
    '  iAnt: ' + STRING(iAnt)
    ).

obOk = iAnt > 0.
IF obOk = FALSE THEN 
  RETURN 'Ingen kopiering utført.'.
ELSE 
  RETURN 'Valgt prisprofil er kopiert til ' + STRING(iAnt) + ' profiler.'.

/* **********************  Internal Procedures  *********************** */

PROCEDURE setPrisFraOutlet:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: Her kommer Outlet kalkylen i ArtPris. Normal kalkyle skal inn i bufArtPris.
        He rmå prisen korrigeres slik at den ikke lenger har outlet rabatt.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE plPris AS DECIMAL NO-UNDO.
    
  plPris = ArtPris.Pris[1].
  plPris = ROUND(plPris / (1 - (lURab% / 100)),0).  
    
  ASSIGN 
    bufArtPris.InnkjopsPris[1] = ArtPris.InnkjopsPris[1] 
    bufArtPris.ValPris[1]      = ArtPris.ValPris[1]
    bufArtPris.Rab1%[1]        = lRab%
    bufArtPris.Rab1Kr[1]       = ROUND((bufArtPris.InnkjopsPris[1] * bufArtPris.Rab1%[1]) / 100,2)
    bufArtPris.VareKost[1]     = bufArtPris.InnkjopsPris[1] - ROUND((bufArtPris.InnkjopsPris[1] * bufArtPris.Rab1%[1]) / 100,2) 

    bufArtPris.Pris[1]         = plPris
    bufArtPris.MvaKr[1]        = bufArtPris.Pris[1] - ROUND((bufArtPris.Pris[1] / (1 + (lMva% / 100))),2)

    bufArtPris.DbKr[1]         = bufArtPris.Pris[1] - bufArtPris.MvaKr[1] - bufArtPris.VareKost[1]
    bufArtPris.Db%[1]          = ROUND(
                                    (bufArtPris.DbKr[1] * 100) / (bufArtPris.Pris[1] - bufArtPris.MvaKr[1])
                                    ,2) 
    bufArtPris.Db%[1]          = IF bufArtPris.Db%[1] = ? THEN 0 ELSE bufArtPris.Db%[1]
    .

  EMPTY TEMP-TABLE ttArtPris.
  CREATE ttArtPris.
  BUFFER-COPY bufArtPris
      TO ttArtPris.
  ohBuffer = BUFFER ttArtPris:HANDLE.  
  RUN opprettHPrisko.p ('',
                        ohBuffer,
                        '',
                        OUTPUT cTekst,
                        OUTPUT bOk 
                        ).  

END PROCEDURE.

PROCEDURE setOutletPris:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: Outlet kalkyle ligger i bufArtPris, og opphav ligger i ArtPris.
        Her skal outlet rabatt og outlet rabattert utpris settes. 
------------------------------------------------------------------------------*/

    ASSIGN 
      /* Kostnadssiden */
      bufArtPris.InnkjopsPris[1] = ArtPris.InnkjopsPris[1] 
      bufArtPris.ValPris[1]      = ArtPris.ValPris[1]
      bufArtPris.Rab1%[1]        = lORab%
      bufArtPris.Rab1Kr[1]       = ROUND((bufArtPris.InnkjopsPris[1] * bufArtPris.Rab1%[1]) / 100,2)
      bufArtPris.VareKost[1]     = bufArtPris.InnkjopsPris[1] - ROUND((bufArtPris.InnkjopsPris[1] * bufArtPris.Rab1%[1]) / 100,2)
      /* Utpris siden */ 
      bufArtPris.Pris[1]         = ROUND(
                                          ArtPris.Pris[1] - 
                                          ((ArtPris.Pris[1] * lURab%) / 100)
                                         ,0)
      bufArtPris.MvaKr[1]        = bufArtPris.Pris[1] - ROUND((bufArtPris.Pris[1] / (1 + (lMva% / 100))),2)
      bufArtPris.DbKr[1]         = bufArtPris.Pris[1] - bufArtPris.MvaKr[1] - bufArtPris.VareKost[1]
      bufArtPris.Db%[1]          = ROUND((bufArtPris.DbKr[1] * 100) / (bufArtPris.Pris[1] - bufArtPris.MvaKr[1]),2)
      bufArtPris.Db%[1]          = IF bufArtPris.Db%[1] = ? THEN 0 ELSE bufArtPris.Db%[1]
      .

    EMPTY TEMP-TABLE ttArtPris.
    CREATE ttArtPris.
    BUFFER-COPY bufArtPris
        TO ttArtPris.
    ohBuffer = BUFFER ttArtPris:HANDLE.  
    RUN opprettHPrisko.p ('',
                          ohBuffer,
                          '',
                          OUTPUT cTekst,
                          OUTPUT bOk 
                          ).  

END PROCEDURE.

PROCEDURE setKomisjonsPris:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: Det kopieres normalt fra eCom (16) til kommisjonsprofil.
        Komisjonsprofilen skal ha LC som innkjøpspris, og ingen rabatt.
------------------------------------------------------------------------------*/
  DEFINE VARIABLE plLCPris AS DECIMAL NO-UNDO.
    
  FIND ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr = ArtPris.ArtikkelNr NO-ERROR.
      
  plLCPris = ROUND(ArtBas.KjedeInnkPris,0).
  plLCPris = IF plLCPris = 0 
              THEN ROUND(((ArtPris.InnkjopsPris[1] * lLC%) / 100),0) 
              ELSE plLCPris.
    
  ASSIGN 
    bufArtPris.InnkjopsPris[1] = plLCPris 
    bufArtPris.ValPris[1]      = plLCPris
    bufArtPris.Rab1%[1]        = 0
    bufArtPris.Rab1Kr[1]       = 0
    bufArtPris.VareKost[1]     = plLCPris 

    bufArtPris.Pris[1]         = ArtPris.Pris[1] 
    bufArtPris.MvaKr[1]        = bufArtPris.Pris[1] - ROUND((bufArtPris.Pris[1] / (1 + (lMva% / 100))),2)

    bufArtPris.DbKr[1]         = bufArtPris.Pris[1] - bufArtPris.MvaKr[1] - bufArtPris.VareKost[1]
    bufArtPris.Db%[1]          = ROUND(
                                    (bufArtPris.DbKr[1] * 100) / (bufArtPris.Pris[1] - bufArtPris.MvaKr[1])
                                    ,2) 
    bufArtPris.Db%[1]          = IF bufArtPris.Db%[1] = ? THEN 0 ELSE bufArtPris.Db%[1]
    .

  EMPTY TEMP-TABLE ttArtPris.
  CREATE ttArtPris.
  BUFFER-COPY bufArtPris
      TO ttArtPris.
  ohBuffer = BUFFER ttArtPris:HANDLE.  
  RUN opprettHPrisko.p ('',
                        ohBuffer,
                        '',
                        OUTPUT cTekst,
                        OUTPUT bOk 
                        ).  

END PROCEDURE.

