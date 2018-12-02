/* Bibliotek for kalkulerte felter, artpris i forbindelse med priskø 
  Opprettet: 09.11.09 av BHa
------------------------------------------------------------------------------*/  
DEF VAR iCL           AS INT NO-UNDO.
DEF VAR iClPrisProfil AS INT NO-UNDO.
DEF VAR fAktivPris   AS DEC NO-UNDO.
DEFINE VARIABLE cOptProfilbutik     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iCLOpt AS INTEGER     NO-UNDO.
{syspara.i 5 1 1 iCl INT}.
{syspar2.i 5 1 1 cOptProfilbutik}
 cOptProfilbutik = TRIM(cOptProfilbutik). 

FIND FIRST Butiker NO-LOCK
     WHERE Butiker.Butik = iCL 
     NO-ERROR.
IF AVAIL Butiker THEN
  iClPrisProfil = Butiker.ProfilNr.

DEF BUFFER bArtPris FOR ArtPris.
DEFINE BUFFER clOptButiker FOR butiker.
PROCEDURE aktivpris:
  DEF INPUT  PARAM irArtPris    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icProfilnr   AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  fAktivPris = 0.

  FIND ArtPris WHERE ROWID(ArtPris) = irArtPris NO-LOCK NO-ERROR.
  IF AVAIL ArtPris THEN DO:
    FIND FIRST bArtPris NO-LOCK
         WHERE bArtPris.ArtikkelNr    = ArtPris.ArtikkelNr
           AND bArtPris.ProfilNr      = INTEGER(icProfilnr)
         NO-ERROR.
    IF NOT AVAIL bArtPris AND cOptProfilbutik <> "" THEN DO:
        /* se efter vilket HK dom hör till */
        FIND FIRST clOptButiker WHERE clOptButiker.profilnr = INTEGER(icProfilnr) NO-LOCK NO-ERROR.
        IF AVAIL clOptButiker AND clOptButiker.sentrallager = FALSE THEN DO:
            /*  */
            iCLOpt = clOptButiker.clButikknr.
            FIND FIRST clOptButiker WHERE clOptButiker.butik = iCLOpt NO-LOCK NO-ERROR.
            IF AVAIL clOptButiker THEN DO:
                FIND FIRST bArtPris WHERE bArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND 
                                          bArtPris.ProfilNr   = clOptButiker.ProfilNr NO-LOCK NO-ERROR.
            END.
        END.

    END.
    IF AVAIL bArtPris THEN
      ASSIGN ocValue = STRING(bArtPris.Pris[1])
             fAktivPris = bArtPris.Pris[1].
    ELSE 
      ASSIGN ocValue = STRING(ArtPris.Pris[1])
             fAktivPris = ArtPris.Pris[1].
  END.
END.

PROCEDURE aktivdb%:
  DEF INPUT  PARAM irArtPris    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icProfilnr   AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irArtPris NO-LOCK NO-ERROR.
  IF AVAIL ArtPris THEN DO:
    FIND FIRST bArtPris NO-LOCK
         WHERE bArtPris.ArtikkelNr    = ArtPris.ArtikkelNr
           AND bArtPris.ProfilNr      = INTEGER(icProfilnr)
         NO-ERROR.
    IF NOT AVAIL bArtPris AND cOptProfilbutik <> "" THEN DO:
        /* se efter vilket HK dom hör till */
        FIND FIRST clOptButiker WHERE clOptButiker.profilnr = INTEGER(icProfilnr) NO-LOCK NO-ERROR.
        IF AVAIL clOptButiker AND clOptButiker.sentrallager = FALSE THEN DO:
            /*  */
            iCLOpt = clOptButiker.clButikknr.
            FIND FIRST clOptButiker WHERE clOptButiker.butik = iCLOpt NO-LOCK NO-ERROR.
            IF AVAIL clOptButiker THEN DO:
                FIND FIRST bArtPris WHERE bArtPris.ArtikkelNr = ArtPris.ArtikkelNr AND 
                                          bArtPris.ProfilNr   = clOptButiker.ProfilNr NO-LOCK NO-ERROR.
            END.
        END.

    END.
    IF AVAIL bArtPris THEN 
    DO:
      ocValue = STRING(bArtPris.DB%[1]).
    END.
    ELSE 
      ocValue = STRING(ArtPris.DB%[1]).
  END.
END.

PROCEDURE priskopris:
  DEF INPUT  PARAM irArtPris    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icProfilnr   AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irArtPris NO-LOCK NO-ERROR.
  IF AVAIL ArtPris THEN DO:
    FIND FIRST PrisKo NO-LOCK
         WHERE PrisKo.ArtikkelNr     = ArtPris.ArtikkelNr
           AND PrisKo.ProfilNr       = INTEGER(icProfilnr)
           AND PrisKo.Klargjorstatus = 0
           USE-INDEX Aktiveres
         NO-ERROR.
    IF AVAIL PrisKo THEN
      ocValue = STRING(PrisKo.Pris).
/*     ELSE                                 */
/*       ocValue = STRING(ArtPris.Pris[1]). */
  END.
END.

PROCEDURE priskovarekost:
  DEF INPUT  PARAM irArtPris    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icProfilnr   AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irArtPris NO-LOCK NO-ERROR.
  IF AVAIL ArtPris THEN DO:
    FIND FIRST PrisKo NO-LOCK
         WHERE PrisKo.ArtikkelNr     = ArtPris.ArtikkelNr
           AND PrisKo.ProfilNr       = INTEGER(icProfilnr)
           AND PrisKo.Klargjorstatus = 0
           USE-INDEX Aktiveres
         NO-ERROR.
    IF AVAIL PrisKo THEN
      ocValue = STRING(PrisKo.Varekost).
/*     ELSE                                 */
/*       ocValue = STRING(ArtPris.Pris[1]). */
  END.
END.

PROCEDURE priskoinnpris:
  DEF INPUT  PARAM irArtPris    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icProfilnr   AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  FIND ArtPris WHERE ROWID(ArtPris) = irArtPris NO-LOCK NO-ERROR.
  IF AVAIL ArtPris THEN DO:
    FIND FIRST PrisKo NO-LOCK
         WHERE PrisKo.ArtikkelNr     = ArtPris.ArtikkelNr
           AND PrisKo.ProfilNr       = INTEGER(icProfilnr)
           AND PrisKo.Klargjorstatus = 0
           USE-INDEX Aktiveres
         NO-ERROR.
    IF AVAIL PrisKo THEN
      ocValue = STRING(PrisKo.InnkjopsPris).
/*     ELSE                                 */
/*       ocValue = STRING(ArtPris.Pris[1]). */
  END.
END.

PROCEDURE datosistesalg:    
  DEF INPUT  PARAM irArtPris    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.
  
  DEF VAR iButnr        AS INT NO-UNDO.
  DEF VAR bVisKunSalg   AS LOG NO-UNDO.
  DEF VAR isokaarperl  AS INTE NO-UNDO.
  iButnr = INTEGER(ENTRY(1,icParam,"¤")).

  IF NUM-ENTRIES(icParam,"¤") > 1 THEN
    bVisKunSalg = LOGICAL(ENTRY(2,icParam,"¤")).

  FIND ArtPris WHERE ROWID(ArtPris) = irArtPris NO-LOCK NO-ERROR.
  IF AVAIL ArtPris THEN DO:
    /*** GML kode ****
    IF iButnr NE iCl AND 
       CAN-FIND(FIRST TransLogg 
                WHERE TransLogg.ArtikkelNr = ArtPris.ArtikkelNr
                  AND TransLogg.Butik = iButNr)
       THEN
      FIND LAST TransLogg NO-LOCK
           WHERE TransLogg.ArtikkelNr = ArtPris.ArtikkelNr
             AND TransLogg.Dato       > 01/01/1900
             AND TransLogg.Tid        > 0
             AND TransLogg.Butik      = iButnr
             AND TransLogg.TTId       = 1
           USE-INDEX OppslagDatoTid 
           NO-ERROR.
    ELSE IF iButnr = iCl AND 
       CAN-FIND(FIRST TransLogg 
                WHERE TransLogg.ArtikkelNr = ArtPris.ArtikkelNr)
       THEN
      FIND LAST TransLogg NO-LOCK
           WHERE TransLogg.ArtikkelNr = ArtPris.ArtikkelNr
             AND TransLogg.Dato       > 01/01/1900
             AND TransLogg.Tid        > 0
             AND TransLogg.TTId       = 1
           USE-INDEX OppslagDatoTid 
           NO-ERROR.
    IF AVAIL TransLogg THEN
      ocValue = STRING(TransLogg.Dato).
    ELSE IF bVisKunSalg THEN
      ocValue = "skiprow".
    **** Gml kode ****/
      IF iButnr NE iCl AND 
         CAN-FIND(FIRST StLinje WHERE
                  StLinje.DataObjekt = STRING(ArtPris.ArtikkelNr,"9999999999999") AND
                  StLinje.StTypeId   = 'ARTIKKEL' AND
                  StLinje.Butik      = iButNr) THEN DO:
          iSokaarperl = (YEAR(TODAY)  - 1) * 1000 + (DATE(12,31,YEAR(TODAY) - 1) - DATE(12,31,YEAR(TODAY) - 2)).
          FIND LAST StLinje NO-LOCK WHERE
           StLinje.StTypeId   = 'ARTIKKEL' AND
           StLinje.Butik      = iButNr AND
           StLinje.PerId      = 'DAG' AND
           StLinje.AarPerLinNr > iSokaarperl AND
           StLinje.DataObjekt = STRING(ArtPris.ArtikkelNr,"9999999999999") NO-ERROR.
      IF NOT AVAIL StLinje THEN
      FIND LAST StLinje NO-LOCK WHERE
       StLinje.StTypeId   = 'ARTIKKEL' AND
       StLinje.Butik      = iButNr AND
       StLinje.PerId      = 'DAG' AND
       StLinje.AarPerLinNr > 0 AND
       StLinje.DataObjekt = STRING(ArtPris.ArtikkelNr,"9999999999999") NO-ERROR.
      END.
      ELSE IF iButnr = iCl AND 
          CAN-FIND(FIRST StLinje WHERE
                   StLinje.DataObjekt = STRING(ArtPris.ArtikkelNr,"9999999999999") AND
                   StLinje.StTypeId   = 'ARTIKKEL')
         THEN
         FIND LAST StLinje NO-LOCK WHERE
           StLinje.StTypeId    = 'ARTIKKEL' AND
           StLinje.Butik       > 0 AND
           StLinje.PerId       = 'DAG' AND
           StLinje.AarPerLinNr > 0 AND
           StLinje.DataObjekt  = STRING(ArtPris.ArtikkelNr,"9999999999999") NO-ERROR.
      IF AVAIL StLinje THEN
        ocValue = STRING(DATE(12,31,StLinje.Aar - 1) + StLinje.PerLinNr).
      ELSE IF bVisKunSalg THEN
        ocValue = "skiprow".
  END.
END PROCEDURE.

PROCEDURE OprisFilter:    
  DEF INPUT  PARAM irArtBas     AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  IF icParam NE "" THEN DO:
    FIND ArtBas NO-LOCK
         WHERE ROWID(ArtBas) = irArtBas
         NO-ERROR.
    IF AVAIL ArtBas AND ArtBas.OPris THEN 
      ocValue = "skiprow".
  END.
END PROCEDURE.

PROCEDURE aktiv_pris_filter:    
  DEF INPUT  PARAM irArtPris    AS ROWID NO-UNDO.
  DEF INPUT  PARAM icParam      AS CHAR  NO-UNDO.
  DEF INPUT  PARAM icSessionId  AS CHAR  NO-UNDO.
  DEF OUTPUT PARAM ocValue      AS CHAR  NO-UNDO.

  DEF VAR iProfilNr AS INT NO-UNDO.

  IF icParam NE "" THEN DO:
    iProfilNr = INT(icParam) NO-ERROR.
    IF iProfilNr = iClPrisProfil THEN RETURN.

    FIND ArtPris NO-LOCK
         WHERE ROWID(ArtPris) = irArtPris
         NO-ERROR.
    IF AVAIL ArtPris THEN DO:        
      FIND FIRST bArtPris NO-LOCK
           WHERE bArtPris.ArtikkelNr    = ArtPris.ArtikkelNr
             AND bArtPris.ProfilNr      = iProfilNr
           NO-ERROR.
      IF NOT AVAIL bArtPris THEN
        ocValue = "skiprow".
    END.
  END.

END PROCEDURE.
