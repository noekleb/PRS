DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM hDummy      AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOk        AS LOG NO-UNDO.

DEF VAR iEkstVPILevNr     AS INT    NO-UNDO.
DEF VAR fArtikkelnr       AS DEC    NO-UNDO.
DEF VAR fVareboknr        AS DEC    NO-UNDO.
DEF VAR fMesseNr          AS DEC    NO-UNDO.
DEF VAR bOk               AS LOG    NO-UNDO.
DEF VAR ix                AS INT    NO-UNDO.
DEF VAR iCL               AS INT    NO-UNDO.
DEF VAR cFieldList        AS CHAR   NO-UNDO.
DEF VAR cTable            AS CHAR   NO-UNDO.
DEF VAR cExceptFieldList  AS CHAR   NO-UNDO.

DEF BUFFER bVarebokLinje FOR VarebokLinje.
DEF BUFFER clButiker     FOR Butiker.
DEF BUFFER bStrekkode    FOR Strekkode.
DEF BUFFER bufArtBas     FOR ArtBas.

DEF VAR bhArtBas         AS HANDLE NO-UNDO.
DEF VAR bhArtPris        AS HANDLE NO-UNDO.
DEF VAR bhVareboklinje   AS HANDLE NO-UNDO.
DEF VAR bhVareboklinje2  AS HANDLE NO-UNDO.

ASSIGN 
  iEkstVPILevNr = INT(ENTRY(1,icParam))
  fArtikkelnr   = DEC(ENTRY(2,icParam))
  fVareboknr    = DEC(ENTRY(3,icParam))
.

cTable = IF CAN-DO('?,0',STRING(iEkstVPILevNr)) THEN 'ArtBas' ELSE 'VPIArtBas'.

IF NUM-ENTRIES(icParam) > 3 THEN
  cFieldList = REPLACE(ENTRY(4,icParam),"|",",").

cExceptFieldList = "LevKod,Beskr,LevFargKod,LevNr,ProdNr,LevDato1,LevDato2,LevDato3,LevDato4,Sasong,Vg,VPIBildekode,LinjeMerknad"
                 + ",Gjennomfaktureres,KjedeVare,KjedeValutaPris,KjedeProdusent,AntIPkn"
                 + ",forhRab%,supRab%,AnbefaltPris,Katalogpris" 
                 + ",KjedeInnkPris,KjedeRab%"
                 + ",KjedeSupInnkPris,KjedeSupRab%" 
                 + ",Sortimentkoder,Lagerkoder,Kampanjeuker,Kampanjestotte".

/* MESSAGE program-name(1) cTable SKIP iEkstVPILevNr SKIP fArtikkelnr SKIP fvareboknr SKIP cFieldList */
/*   VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                               */

FIND FIRST VarebokHode NO-LOCK
     WHERE VarebokHode.VarebokNr = fVarebokNr
     NO-ERROR.
IF NOT AVAIL VarebokHode THEN 
DO:
  ASSIGN
    obOk = FALSE
    ocReturn = "Feil i oppfrisking. Finner ikke varebok"
  .
  RETURN.
END.

{syspara.i 5 1 1 iCL INT}
FIND clButiker WHERE clButiker.Butik = iCL NO-LOCK NO-ERROR.
IF NOT AVAIL clButiker THEN 
DO:
  ASSIGN
    obOk = FALSE
    ocReturn = "Finner ikke sentral-lager: " + STRING(iCL).
  .
  RETURN.
END.

CASE cTable:
  WHEN 'VPIArtBas' THEN
  DO:
    CREATE BUFFER bhArtBas  FOR TABLE cTable.
    CREATE BUFFER bhArtPris FOR TABLE 'VPIArtPris'.
  END.
  WHEN 'ArtBas' THEN
  DO:
    CREATE BUFFER bhArtBas  FOR TABLE cTable.
    CREATE BUFFER bhArtPris FOR TABLE 'ArtPris'.
  END.
END CASE.

/* IF NOT VALID-HANDLE(bhArtBas) THEN              */
/* DO:                                             */
/*   ASSIGN                                        */
/*     obOk = FALSE                                */
/*     ocReturn = "Fant ikke tabellen: " + cTable. */
/*   .                                             */
/*   RETURN.                                       */
/* END.                                            */

IF cTable = 'VPIArtBas' THEN
  bhArtBas:FIND-UNIQUE('WHERE EkstVPILevNr = ' + STRING(iEkstVPILevNr) 
                     + '  AND ArtikkelNr = DEC(' + QUOTER(STRING(fArtikkelNr)) + ')') NO-ERROR.
ELSE 
  bhArtBas:FIND-UNIQUE('WHERE ArtikkelNr = DEC(' + QUOTER(STRING(fArtikkelNr)) + ')') NO-ERROR.

IF bhArtBas:AVAIL THEN
DO:
  FIND VarGr  WHERE VarGr.Vg     = INT(bhArtBas:BUFFER-FIELD('Vg'):BUFFER-VALUE) NO-LOCK NO-ERROR.
  FIND HuvGr  WHERE HuvGr.Hg     = INT(bhArtBas:BUFFER-FIELD('Hg'):BUFFER-VALUE) NO-LOCK NO-ERROR.
  FIND LevBas WHERE LevBas.LevNr = INT(bhArtBas:BUFFER-FIELD('LevNr'):BUFFER-VALUE) NO-LOCK NO-ERROR.

  IF cTable = 'VPIArtBas' THEN
    bhArtPris:FIND-FIRST('WHERE EkstVPILevNr = ' + STRING(iEkstVPILevnr) 
                        + ' AND VareNr = ' + QUOTER(STRING(bhArtBas:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE)) + ''
                        + ' AND ProfilNr  = ' + STRING(varebokhode.profilnr)
                         ,NO-LOCK
                         ).
  ELSE /*ArtBas*/
    bhArtPris:FIND-FIRST('WHERE Artikkelnr = DEC(' + QUOTER(STRING(bhArtBas:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE)) + ')'
                        + ' AND ProfilNr  = ' + STRING(varebokhode.profilnr)
                         ,NO-LOCK
                         ).

  /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
  IF NOT bhArtPris:AVAIL THEN
  DO:
    IF cTable = 'VPIArtBas' THEN
      bhArtPris:FIND-FIRST('WHERE EkstVPILevNr = ' + STRING(iEkstVPILevnr) 
                          + ' AND VareNr = (' + QUOTER(STRING(bhArtBas:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE)) + ')'
                          + ' AND ProfilNr  = ' + STRING(clButiker.profilnr)
                           ,NO-LOCK
                           ).
    ELSE /*ArtBas*/
      bhArtPris:FIND-FIRST('WHERE Artikkelnr = DEC(' + QUOTER(STRING(bhArtBas:BUFFER-FIELD('artikkelnr'):BUFFER-VALUE)) + ')'
                          + ' AND ProfilNr  = ' + STRING(clButiker.profilnr)
                           ,NO-LOCK
                           ).

  END.

  IF NOT bhArtPris:AVAIL THEN
  DO:
    ASSIGN 
      obOk     = FALSE
      ocReturn = "Ingen pris på artikkel " + STRING(fArtikkelNr).
    RETURN.  
  END.

  IF AVAIL VarGr AND AVAIL HuvGr AND AVAIL LevBas AND bhArtPris:AVAIL THEN 
  DO:
    FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
    IF AVAIL Avdeling THEN 
    DO:
      FIND VareBokLinje
           WHERE VareBokLinje.VareBokNr = fVarebokNr
             AND VareBokLinje.ArtikkelNr = DEC(bhArtBas:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE)
          EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
      IF AVAIL VareBokLinje THEN 
      DO:
        bhVareboklinje = BUFFER Vareboklinje:HANDLE.

        /* AntIPkn feltet oppdateres rett i ArtBas. */                                                        
        bhVareboklinje:BUFFER-COPY(bhArtBas,cExceptFieldList).        
        IF VarebokLinje.KatalogPris = 0 THEN VareBokLinje.KatalogPris = IF cTable = 'VPIArtBas' THEN DEC(bhArtBas:BUFFER-FIELD('KatalogPris'):BUFFER-VALUE(1))
                                                                        ELSE DEC(bhArtBas:BUFFER-FIELD('KatalogPris'):BUFFER-VALUE).
        bhArtPris = BUFFER ArtPris:HANDLE.


        DO ix = 1 TO NUM-ENTRIES(cFieldList):          
          CASE ENTRY(ix,cFieldList):
            WHEN "MerknadsKoder" THEN DO:
              ASSIGN
                VarebokLinje.Sortimentkoder  = bhArtBas:BUFFER-FIELD('Sortimentkoder'):BUFFER-VALUE
                VarebokLinje.Lagerkoder      = bhArtBas:BUFFER-FIELD('Lagerkoder'):BUFFER-VALUE        
                VarebokLinje.Kampanjeuker    = bhArtBas:BUFFER-FIELD('Kampanjeuker'):BUFFER-VALUE                     
                VarebokLinje.Kampanjestotte  = bhArtBas:BUFFER-FIELD('Kampanjestotte'):BUFFER-VALUE           
                .
            END.
            /* AntIPkn feltet oppdateres bare her. Ikke på koblede varebøker. */
            WHEN "AntIPkn"      THEN DO:
                                        FIND bufArtBas EXCLUSIVE-LOCK WHERE
                                          bufArtBas.ArtikkelNr = VareBokLinje.ArtikkelNr NO-ERROR.
                                        IF AVAILABLE bufArtBas THEN
                                          bufArtBas.AntIPakn = IF cTable = 'VPIArtBas' 
                                                                 THEN INTEGER(bhArtBas:BUFFER-FIELD('AntIPkn'):BUFFER-VALUE)
                                                                 ELSE INTEGER(bhArtBas:BUFFER-FIELD('AntIPakn'):BUFFER-VALUE).
                                      END.
                                      
            WHEN "Levkod"        THEN Vareboklinje.levKod     = bhArtBas:BUFFER-FIELD('LevKod'):BUFFER-VALUE.
            WHEN "levnr"         THEN 
                DO:
                  BUFFER-COPY LevBas TO VareBokLinje.
                  Vareboklinje.levnr  = INT(bhArtBas:BUFFER-FIELD('Levnr'):BUFFER-VALUE).
                  FIND bufArtBas EXCLUSIVE-LOCK WHERE
                      bufArtBas.ArtikkelNr = VareBokLinje.ArtikkelNr NO-ERROR.
                  IF AVAILABLE bufArtBas THEN
                      bufArtBas.LevNr = INTEGER(bhArtBas:BUFFER-FIELD('Levnr'):BUFFER-VALUE).
                END.
            WHEN "prodnr"        THEN Vareboklinje.ProdNr     = bhArtBas:BUFFER-FIELD('ProdNr'):BUFFER-VALUE.
            WHEN "Vg"            THEN 
                DO:
                  BUFFER-COPY Avdeling TO VareBokLinje.
                  BUFFER-COPY HuvGr    TO VareBokLinje.
                  BUFFER-COPY VarGr    TO VareBokLinje.
                  ASSIGN 
                    Vareboklinje.Vg         = INT(bhArtBas:BUFFER-FIELD('Vg'):BUFFER-VALUE)
                    VareBokLinje.Hg         = INT(bhArtBas:BUFFER-FIELD('Hg'):BUFFER-VALUE)
                    VareBokLinje.AvdelingNr = Huvgr.AvdelingNr.
                  FIND bufArtBas EXCLUSIVE-LOCK WHERE
                      bufArtBas.ArtikkelNr = VareBokLinje.ArtikkelNr NO-ERROR.
                  IF AVAILABLE bufArtBas THEN DO:
                      IF bufArtBas.Vg <> bhArtBas:BUFFER-FIELD('Vg'):BUFFER-VALUE THEN
                      DO:
                          ASSIGN
                            bufArtBas.LopNr = ?.
                          ASSIGN
                            bufArtBas.Vg = bhArtBas:BUFFER-FIELD('Vg'):BUFFER-VALUE
                            bufArtBas.Hg = bhArtBas:BUFFER-FIELD('Hg'):BUFFER-VALUE.
                                    /* Tildeler løpenummer til ny artikkel */
                          RUN settlopnr.p (INPUT bufArtBas.Vg, INPUT 'N', OUTPUT bufArtBas.LopNr).

                      END.
                  END.
                END.
            WHEN "NyStrekkode"   THEN 
            DO:

              FOR EACH VPIStrekkode WHERE VPIStrekkode.EkstVPILevNr = INT(bhArtBas:BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE) 
                                      AND VPIStrekkode.varenr       = STRING(bhArtBas:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE) NO-LOCK:
                  IF NOT CAN-FIND(FIRST Strekkode 
                                      WHERE Strekkode.artikkelnr = DEC(VPIStrekkode.varenr) 
                                        AND Strekkode.strkode    = VPIStrekkode.strkode
                                        AND Strekkode.kode       = VPIStrekkode.kode) 
                         AND NOT CAN-FIND(FIRST Strekkode 
                                           WHERE Strekkode.kode = VPIStrekkode.kode) THEN
                  DO:
                    CREATE strekkode.
                    BUFFER-COPY VPIStrekkode TO Strekkode.
                    IF AVAILABLE Strekkode THEN
                        ASSIGN
                        Strekkode.ArtikkelNr = dec(vpiStrekkode.Varenr)   
                        Strekkode.StrKode    = vpiStrekkode.StrKode  
                        Strekkode.Kode       = vpiStrekkode.Kode 
                        Strekkode.Bestillingsnummer = VPIStrekkode.Bestillingsnummer
                        NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN 
                    DO:
                      ocReturn = ERROR-STATUS:GET-MESSAGE(1).
                      obOk     = FALSE.
                      UNDO,LEAVE.
                    END.
                    ASSIGN 
                      Strekkode.iKasse = TRUE.
                    .
                  END.
              END. /*FOR EACH*/
            END.
            WHEN "KorrStrekkode" THEN
            DO:
              FOR EACH VPIStrekkode WHERE VPIStrekkode.EkstVPILevNr = INT(bhArtBas:BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE) 
                                      AND VPIStrekkode.varenr       = STRING(bhArtBas:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE) NO-LOCK:
                IF CAN-FIND(FIRST Strekkode 
                                  WHERE Strekkode.kode = VPIStrekkode.kode)
                       AND NOT CAN-FIND(FIRST Strekkode 
                                      WHERE Strekkode.artikkelnr = DEC(VPIStrekkode.varenr) 
                                        AND Strekkode.strkode    = VPIStrekkode.strkode
                                        AND Strekkode.kode       = VPIStrekkode.kode) THEN
                DO:
                  FIND FIRST Strekkode WHERE Strekkode.kode = VPIStrekkode.kode EXCLUSIVE-LOCK NO-ERROR.                  
                  IF ERROR-STATUS:ERROR THEN 
                  DO:
                    ocReturn = ERROR-STATUS:GET-MESSAGE(1).
                    obOk     = FALSE.
                    UNDO,LEAVE.
                  END.
                  IF AVAILABLE Strekkode THEN
                  DO:
                      BUFFER-COPY VPIStrekkode TO Strekkode NO-ERROR.
                      ASSIGN
                        Strekkode.ArtikkelNr = dec(vpiStrekkode.Varenr)   
                        Strekkode.StrKode    = vpiStrekkode.StrKode  
                        Strekkode.Kode       = vpiStrekkode.Kode 
                        Strekkode.Bestillingsnummer = VPIStrekkode.Bestillingsnummer
                      NO-ERROR.
                  END.
                  IF ERROR-STATUS:ERROR THEN 
                  DO:
                    ocReturn = ERROR-STATUS:GET-MESSAGE(1).
                    obOk     = FALSE.
                    UNDO,LEAVE.
                  END.
                END.
              END. /*FOR EACH*/
            END.

            WHEN "Mva%"          THEN IF cTable = 'ArtBas' THEN VarebokLinje.Mva%             = DEC(bhArtPris:BUFFER-FIELD('Mva%'):BUFFER-VALUE(1)).
            WHEN "KjedeRab%"     THEN IF cTable = 'VPIArtBas' THEN DO:
                                                                ASSIGN 
                                                                VarebokLinje.KjedeInnkPris    = DEC(bhArtBas:BUFFER-FIELD('KjedeInnkPris'):BUFFER-VALUE)
                                                                VarebokLinje.KjedeRab%        = DEC(bhArtBas:BUFFER-FIELD('KjedeRab%'):BUFFER-VALUE).
                                                                END.
            WHEN "KjedeSupRab%"  THEN IF cTable = 'VPIArtBas' THEN DO:
                                                                ASSIGN 
                                                                VarebokLinje.KjedeSupInnkPris = DEC(bhArtBas:BUFFER-FIELD('KjedeSupInnkPris'):BUFFER-VALUE)
                                                                VarebokLinje.KjedeSupRab%     = DEC(bhArtBas:BUFFER-FIELD('KjedeSupRab%'):BUFFER-VALUE).
                                                                END.

            WHEN "Beskr"         THEN VarebokLinje.Beskr         = bhArtBas:BUFFER-FIELD('Beskr'):BUFFER-VALUE.
            WHEN "LevFargKod"    THEN VarebokLinje.LevFargKod    = bhArtBas:BUFFER-FIELD('LevFargKod'):BUFFER-VALUE.

            WHEN "AnbefaltPris"  THEN VarebokLinje.AnbefaltPris  = DEC(bhArtBas:BUFFER-FIELD('AnbefaltPris'):BUFFER-VALUE).
            WHEN "Pris"          THEN VarebokLinje.Pris          = DEC(bhArtBas:BUFFER-FIELD('AnbefaltPris'):BUFFER-VALUE).

            WHEN "InnkjopsPris"  THEN 
                IF cTable = 'VPIArtBas' THEN VarebokLinje.InnkjopsPris  = DEC(bhArtBas:BUFFER-FIELD('Katalogpris'):BUFFER-VALUE(1)).
                                      ELSE VarebokLinje.InnkjopsPris  = DEC(bhArtBas:BUFFER-FIELD('Katalogpris'):BUFFER-VALUE).

            WHEN "forhRab%"      THEN IF cTable = 'VPIArtBas' THEN VarebokLinje.forhRab%      = DEC(bhArtBas:BUFFER-FIELD('forhRab%'):BUFFER-VALUE(1)).
                                      ELSE VarebokLinje.forhRab%      = DEC(bhArtBas:BUFFER-FIELD('forhRab%'):BUFFER-VALUE).

            WHEN "supRab%"       THEN IF cTable = 'VPIArtBas' THEN VarebokLinje.supRab%       = DEC(bhArtBas:BUFFER-FIELD('suppRab%'):BUFFER-VALUE(1)).
                                      ELSE VarebokLinje.supRab%       = DEC(bhArtBas:BUFFER-FIELD('supRab%'):BUFFER-VALUE).

            WHEN "LevDato1"      THEN ASSIGN VarebokLinje.LevDato1      = bhArtBas:BUFFER-FIELD('LevDato1'):BUFFER-VALUE
                                             VarebokLinje.LevDato2      = bhArtBas:BUFFER-FIELD('LevDato2'):BUFFER-VALUE
                                             VarebokLinje.LevDato3      = bhArtBas:BUFFER-FIELD('LevDato3'):BUFFER-VALUE
                                             VarebokLinje.LevDato4      = bhArtBas:BUFFER-FIELD('LevDato4'):BUFFER-VALUE
                                      .
            
            WHEN "Gjennomfaktureres" THEN VarebokLinje.Gjennomfaktureres = bhArtBas:BUFFER-FIELD('Gjennomfaktureres'):BUFFER-VALUE.
            WHEN "KjedeVare"     THEN VarebokLinje.Kjedevare      = bhArtBas:BUFFER-FIELD('KjedeVare'):BUFFER-VALUE. 
            WHEN "SaSong"        THEN VarebokLinje.Sasong         = bhArtBas:BUFFER-FIELD('SaSong'):BUFFER-VALUE.
            WHEN "KjedeValutaPris" THEN VareBokLinje.KjedeValutaPris  = bhArtBas:BUFFER-FIELD('KjedeValutaPris'):BUFFER-VALUE. 
            WHEN "KjedeProdusent"  THEN VareBokLinje.KjedeProdusent   = bhArtBas:BUFFER-FIELD('KjedeProdusent'):BUFFER-VALUE.
            WHEN "VPIBildekode" THEN
              DO:
                FIND bufArtBas EXCLUSIVE-LOCK WHERE
                    bufArtBas.ArtikkelNr = VareBokLinje.ArtikkelNr NO-ERROR.
                IF AVAILABLE bufArtBas THEN
                    bufArtBas.VPIBildekode = bhArtBas:BUFFER-FIELD('VPIBildekode'):BUFFER-VALUE.
              END.
            WHEN "RAvdNr" THEN
              DO:
                FIND bufArtBas EXCLUSIVE-LOCK WHERE
                    bufArtBas.ArtikkelNr = VareBokLinje.ArtikkelNr NO-ERROR.
                IF AVAILABLE bufArtBas THEN
                    bufArtBas.RAvdNr = bhArtBas:BUFFER-FIELD('RAvdNr'):BUFFER-VALUE.
              END.
          END CASE.
        END.

        IF VarebokLinje.InnkjopsPris NE 0 THEN
          RUN vareboklinje_kalkuler.p (bhVareboklinje,"InnkjopsPris").

        obOk = TRUE.

        FIND FIRST VarebokHode OF VarebokLinje NO-LOCK.
        fMesseNr = VarebokHode.MesseNr.

        /* Oppdaterer koblede varebøker. */
        FOR EACH VarebokHode  
            WHERE VarebokHode.MesseNr   = fMesseNr
              AND VarebokHode.VarebokNr NE VarebokLinje.VarebokNr,
            FIRST bVarebokLinje OF VarebokHode
                  WHERE bVarebokLinje.ArtikkelNr = VarebokLinje.ArtikkelNr
                  EXCLUSIVE-LOCK:
          
          bhVareboklinje2 = BUFFER bVareboklinje:HANDLE.
          bhVarebokLinje2:BUFFER-COPY(bhArtBas,cExceptFieldList).
          
          IF VarebokLinje.KatalogPris = 0 THEN VareBokLinje.KatalogPris = IF cTable = 'VPIArtBas' THEN DEC(bhArtBas:BUFFER-FIELD('KatalogPris'):BUFFER-VALUE(1))
                                                                          ELSE DEC(bhArtBas:BUFFER-FIELD('KatalogPris'):BUFFER-VALUE).
                                                                          
        DO ix = 1 TO NUM-ENTRIES(cFieldList):          
          CASE ENTRY(ix,cFieldList):
            WHEN "MerknadsKoder" THEN DO:
              ASSIGN
                VarebokLinje.Sortimentkoder  = bhArtBas:BUFFER-FIELD('Sortimentkoder'):BUFFER-VALUE
                VarebokLinje.Lagerkoder      = bhArtBas:BUFFER-FIELD('Lagerkoder'):BUFFER-VALUE        
                VarebokLinje.Kampanjeuker    = bhArtBas:BUFFER-FIELD('Kampanjeuker'):BUFFER-VALUE                     
                VarebokLinje.Kampanjestotte  = bhArtBas:BUFFER-FIELD('Kampanjestotte'):BUFFER-VALUE           
                .                                                           
            END.
            WHEN "Levkod"        THEN bVareboklinje.levKod     = bhArtBas:BUFFER-FIELD('LevKod'):BUFFER-VALUE.
            WHEN "levnr"         THEN 
                DO:
                   BUFFER-COPY LevBas TO bVareBokLinje.
                   bVareboklinje.levnr  = INT(bhArtBas:BUFFER-FIELD('Levnr'):BUFFER-VALUE).
                END.
            WHEN "prodnr"        THEN bVareboklinje.ProdNr     = bhArtBas:BUFFER-FIELD('ProdNr'):BUFFER-VALUE.
            WHEN "Vg"            THEN 
                DO:
                    BUFFER-COPY Avdeling TO bVareBokLinje.
                    BUFFER-COPY HuvGr    TO bVareBokLinje.
                    BUFFER-COPY VarGr    TO bVareBokLinje.
                    ASSIGN 
                      bVareboklinje.Vg         = INT(bhArtBas:BUFFER-FIELD('Vg'):BUFFER-VALUE)
                      bVareBokLinje.Hg         = INT(bhArtBas:BUFFER-FIELD('Hg'):BUFFER-VALUE)
                      bVareBokLinje.AvdelingNr = Huvgr.AvdelingNr.
                END.
            WHEN "NyStrekkode"   THEN 
            DO: 
              FOR EACH VPIStrekkode WHERE VPIStrekkode.EkstVPILevNr = INT(bhArtBas:BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE) 
                                      AND VPIStrekkode.varenr       = STRING(bhArtBas:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE) NO-LOCK:
                  IF NOT CAN-FIND(FIRST Strekkode 
                                      WHERE Strekkode.artikkelnr = DEC(VPIStrekkode.varenr) 
                                        AND Strekkode.strkode    = VPIStrekkode.strkode
                                        AND Strekkode.kode       = VPIStrekkode.kode) 
                         AND NOT CAN-FIND(FIRST Strekkode 
                                           WHERE Strekkode.kode = VPIStrekkode.kode) THEN
                  DO:
                    CREATE strekkode.
                    BUFFER-COPY VPIStrekkode EXCEPT hovednr TO Strekkode NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN 
                    DO:
                      ocReturn = ERROR-STATUS:GET-MESSAGE(1).
                      obOk     = FALSE.
                      UNDO,LEAVE.
                    END.
                    ASSIGN 
                      Strekkode.iKasse = TRUE.
                    .
                  END.
              END. /*FOR EACH*/
            END.
            WHEN "KorrStrekkode" THEN
            DO:
              FOR EACH VPIStrekkode WHERE VPIStrekkode.EkstVPILevNr = INT(bhArtBas:BUFFER-FIELD('EkstVPILevNr'):BUFFER-VALUE) 
                                      AND VPIStrekkode.varenr       = STRING(bhArtBas:BUFFER-FIELD('ArtikkelNr'):BUFFER-VALUE) NO-LOCK:
                IF CAN-FIND(FIRST Strekkode 
                                  WHERE Strekkode.kode = VPIStrekkode.kode)
                       AND NOT CAN-FIND(FIRST Strekkode 
                                      WHERE Strekkode.artikkelnr = DEC(VPIStrekkode.varenr) 
                                        AND Strekkode.strkode    = VPIStrekkode.strkode
                                        AND Strekkode.kode       = VPIStrekkode.kode) THEN
                DO:
                  FIND FIRST Strekkode WHERE Strekkode.kode = VPIStrekkode.kode EXCLUSIVE-LOCK NO-ERROR.
                  IF ERROR-STATUS:ERROR THEN 
                  DO:
                    ocReturn = ERROR-STATUS:GET-MESSAGE(1).
                    obOk     = FALSE.
                    UNDO,LEAVE.
                  END.

                  IF AVAILABLE Strekkode THEN
                  DO:
                      BUFFER-COPY VPIStrekkode TO Strekkode NO-ERROR.
                      ASSIGN
                        Strekkode.ArtikkelNr        = dec(vpiStrekkode.Varenr)   
                        Strekkode.StrKode           = vpiStrekkode.StrKode  
                        Strekkode.Kode              = vpiStrekkode.Kode 
                        Strekkode.Bestillingsnummer = VPIStrekkode.Bestillingsnummer
                      NO-ERROR.
                  END.
                  IF ERROR-STATUS:ERROR THEN 
                  DO:
                    ocReturn = ERROR-STATUS:GET-MESSAGE(1).
                    obOk     = FALSE.
                    UNDO,LEAVE.
                  END.
                END.
              END. /*FOR EACH*/
            END.

            WHEN "Mva%"          THEN IF cTable = 'ArtBas' THEN bVarebokLinje.Mva%          = DEC(bhArtPris:BUFFER-FIELD('Mva%'):BUFFER-VALUE(1)).
            WHEN "KjedeRab%"     THEN IF cTable = 'ArtBas' THEN ASSIGN 
                                                                bVarebokLinje.KjedeInnkPris    = DEC(bhArtBas:BUFFER-FIELD('KjedeInnkPris'):BUFFER-VALUE)
                                                                bVarebokLinje.KjedeRab%        = DEC(bhArtBas:BUFFER-FIELD('KjedeRab%'):BUFFER-VALUE).
            WHEN "KjedeSupRab%"  THEN IF cTable = 'ArtBas' THEN ASSIGN 
                                                                bVarebokLinje.KjedeSupInnkPris = DEC(bhArtBas:BUFFER-FIELD('KjedeSupInnkPris'):BUFFER-VALUE)
                                                                bVarebokLinje.KjedeSupRab%     = DEC(bhArtBas:BUFFER-FIELD('KjedeSupRab%'):BUFFER-VALUE).

            WHEN "Beskr"         THEN bVarebokLinje.Beskr         = bhArtBas:BUFFER-FIELD('Beskr'):BUFFER-VALUE.
            WHEN "LevFargKod"    THEN bVarebokLinje.LevFargKod    = bhArtBas:BUFFER-FIELD('LevFargKod'):BUFFER-VALUE.

            WHEN "AnbefaltPris"  THEN bVarebokLinje.AnbefaltPris  = DEC(bhArtBas:BUFFER-FIELD('AnbefaltPris'):BUFFER-VALUE).
            WHEN "Pris"          THEN bVarebokLinje.Pris          = DEC(bhArtBas:BUFFER-FIELD('AnbefaltPris'):BUFFER-VALUE).

            WHEN "InnkjopsPris"  THEN IF cTable = 'VPIArtBas' THEN bVarebokLinje.InnkjopsPris  = DEC(bhArtBas:BUFFER-FIELD('Katalogpris'):BUFFER-VALUE(1)).
                                      ELSE bVarebokLinje.InnkjopsPris  = DEC(bhArtBas:BUFFER-FIELD('Katalogpris'):BUFFER-VALUE).

            WHEN "forhRab%"      THEN IF cTable = 'VPIArtBas' THEN bVarebokLinje.forhRab%      = DEC(bhArtBas:BUFFER-FIELD('forhRab%'):BUFFER-VALUE(1)).
                                      ELSE bVarebokLinje.forhRab%      = DEC(bhArtBas:BUFFER-FIELD('forhRab%'):BUFFER-VALUE).

            WHEN "supRab%"       THEN IF cTable = 'VPIArtBas' THEN bVarebokLinje.supRab%       = DEC(bhArtBas:BUFFER-FIELD('suppRab%'):BUFFER-VALUE(1)).
                                      ELSE bVarebokLinje.supRab%       = DEC(bhArtBas:BUFFER-FIELD('supRab%'):BUFFER-VALUE).

            WHEN "LevDato1"      THEN ASSIGN bVarebokLinje.LevDato1      = bhArtBas:BUFFER-FIELD('LevDato1'):BUFFER-VALUE
                                             bVarebokLinje.LevDato2      = bhArtBas:BUFFER-FIELD('LevDato2'):BUFFER-VALUE
                                             bVarebokLinje.LevDato3      = bhArtBas:BUFFER-FIELD('LevDato3'):BUFFER-VALUE
                                             bVarebokLinje.LevDato4      = bhArtBas:BUFFER-FIELD('LevDato4'):BUFFER-VALUE
                                      .
            WHEN "Gjennomfaktureres" THEN bVarebokLinje.Gjennomfaktureres = bhArtBas:BUFFER-FIELD('Gjennomfaktureres'):BUFFER-VALUE.
            WHEN "KjedeVare"     THEN bVarebokLinje.Kjedevare      = bhArtBas:BUFFER-FIELD('KjedeVare'):BUFFER-VALUE. 
            WHEN "SaSong"        THEN bVarebokLinje.Sasong         = bhArtBas:BUFFER-FIELD('SaSong'):BUFFER-VALUE.
            WHEN "KjedeValutaPris" THEN VareBokLinje.KjedeValutaPris  = bhArtBas:BUFFER-FIELD('KjedeValutaPris'):BUFFER-VALUE. 
            WHEN "KjedeProdusent"  THEN VareBokLinje.KjedeProdusent   = bhArtBas:BUFFER-FIELD('KjedeProdusent'):BUFFER-VALUE.
          END CASE.
        END. /*For Each */

        IF bVarebokLinje.InnkjopsPris NE 0 THEN
          RUN vareboklinje_kalkuler.p (bhVareboklinje2,"InnkjopsPris").
        END.
      END.
      ELSE ocReturn = "Oppdatering av varebok: Vareboklinje ikke tilgjengelig for oppdatering".
    END.
    ELSE ocReturn = "Oppdatering av varebok: Finner ikke avdeling for artikkel".
  END.
  ELSE IF NOT AVAIL VarGr THEN
    ocReturn = "Oppdatering av varebok: Finner ikke varegruppe for artikkel".
  ELSE IF NOT AVAIL HuvGr THEN
    ocReturn = "Oppdatering av varebok: Finner ikke hovedgruppe for artikkel".
  ELSE IF NOT AVAIL LevBas THEN
    ocReturn = "Oppdatering av varebok: Finner ikke leverandør for artikkel".
END.
ELSE ocReturn = "Oppdatering av varebok: Finner ikke artikkel: " + STRING(fArtikkelNr).

bhArtbas:BUFFER-RELEASE() NO-ERROR.
DELETE OBJECT bhArtbas NO-ERROR.
bhArtPris:BUFFER-RELEASE() NO-ERROR.
DELETE OBJECT bhArtPris NO-ERROR.
  
