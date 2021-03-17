/* Kopier artikkel 
   Parametere: Aksjon (new|edit|copy),artikkelnr,prisprofil
               temp-tabell med overstyrte verdier for ny artikkel
   
   Opprettet: 17.11.04 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery        AS HANDLE NO-UNDO.
DEF VAR hBuffArtBas   AS HANDLE NO-UNDO.
DEF VAR hBuffArtPris  AS HANDLE NO-UNDO.
DEF VAR hField        AS HANDLE NO-UNDO.
DEF VAR iLopNr        AS INT    NO-UNDO.
DEF VAR cAction       AS CHAR   NO-UNDO.
DEF VAR fArtikkelNr   AS DEC    NO-UNDO.
DEF VAR iProfilNr     AS INT    NO-UNDO.
DEF VAR fMva%         AS DEC    NO-UNDO INIT 25.
DEF VAR bVarekostUpd  AS LOG    NO-UNDO.
DEF VAR fPrisExMva    AS DEC    NO-UNDO.
DEF VAR fEuroKurs     AS DEC    NO-UNDO.
DEF VAR fVarebehNr    AS DEC    NO-UNDO.
DEF VAR bForhRab      AS LOG    NO-UNDO.
DEFINE VARIABLE cModus AS CHAR NO-UNDO.

FUNCTION ValidateInput RETURNS CHARACTER 
        (INPUT icField AS CHAR,
         INPUT icValue AS CHAR) FORWARD.

FIND FIRST Moms NO-LOCK
     WHERE Moms.MomsKod = 1
     NO-ERROR.
IF AVAIL Moms THEN fMva% = Moms.MomsProc.
ELSE DO:
  ocReturn = "MVA register er mangler kode 1".
  RETURN.
END.

{syspara.i 2 1 1 fEuroKurs DECIMAL}
IF fEuroKurs = ? OR fEuroKurs = 0 THEN
  fEuroKurs = 0.5.

DEF BUFFER clButiker    FOR Butiker.
DEF BUFFER bFromArtBas  FOR ArtBas.
DEF BUFFER bFromArtPris FOR ArtPris.
CREATE BUFFER hBuffArtBas  FOR TABLE "ArtBas".
CREATE BUFFER hBuffArtPris FOR TABLE "ArtPris".

ASSIGN cAction     = ENTRY(1,icParam)
       fArtikkelNr = DEC(ENTRY(2,icParam))
       iProfilNr   = INT(ENTRY(3,icParam))
       .
/* Modus = '1'-Supplering, '2'=Forhånd. */       
IF NUM-ENTRIES(icParam) > 4 THEN 
  cModus = TRIM(ENTRY(5,icParam)).

IF NUM-ENTRIES(icParam) > 3 THEN 
HODE:
DO:
  fVarebehNr = DEC(ENTRY(4,icParam)).
  IF fVarebehNr = 0 THEN LEAVE HODE.
  
  FIND FIRST VareBehHode NO-LOCK
       WHERE VareBehHode.VareBehNr = fVarebehNr
       NO-ERROR.
  IF NOT AVAIL VareBehHode THEN DO:
    ocReturn = "Finner ikke varehåndteringsbok. Programfeil: " + STRING(fVarebehNr) + '  ' + PROGRAM-NAME(1).
    RETURN.
  END.
  FIND FIRST Messe OF VareBehHode
       NO-LOCK NO-ERROR.
  IF AVAIL Messe AND Messe.MesseType = 1 THEN
    bForhRab = YES.
END. /* HODE */

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).
hQuery:QUERY-OPEN().

IF cAction NE "new" THEN 
DO:
  FIND bFromArtBas  
       WHERE bFromArtBas.ArtikkelNr  = fArtikkelNr 
       NO-LOCK NO-ERROR.
  FIND bFromArtPris 
       WHERE bFromArtPris.ArtikkelNr = fArtikkelNr 
         AND bFromArtPris.ProfilNr = iProfilNr 
       NO-LOCK NO-ERROR.
END.

/* Er det registrering av forhåndsordre, skal ikke endring oppdateres i artikkelregisteret, Bare i vareh.bok. */
IF cModus = '1' AND CAN-DO("edit",cAction) THEN 
    RUN oppdaterVarebehLinje.
ELSE RUN oppdaterArtBas.

DELETE OBJECT hQuery.
DELETE OBJECT hBuffArtBas.
DELETE OBJECT hBuffArtPris.

/* **********************  Internal Procedures  *********************** */


PROCEDURE oppdaterVarebehLinje:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
  IF hQuery:IS-OPEN THEN
  BLOKKEN: 
  DO:
    FIND VareBehLinje EXCLUSIVE-LOCK WHERE 
        VareBehLinje.VareBeHNr  = VareBehHode.VareBehNr AND
        VareBehLinje.ArtikkelNr = fArtikkelNr NO-ERROR.
    IF AVAILABLE VareBehLinje THEN
    LINJE_OPPDATERING:
    DO:

      hQuery:GET-FIRST().
      QUERY_LOOP:
      REPEAT WHILE NOT hQuery:QUERY-OFF-END:
    
        ocReturn = ocReturn + ValidateInput(ihBuffer:BUFFER-FIELD("cNavn"):BUFFER-VALUE,
                                            ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
        IF ocReturn NE "" THEN ocReturn = ocReturn + CHR(10).

        CASE STRING(ihBuffer:BUFFER-FIELD("cNavn"):BUFFER-VALUE):
          WHEN 'Beskr'      THEN VareBehLinje.Beskr    = STRING(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
          WHEN 'Vg'         THEN DO:
                               VareBehLinje.Vg = INT(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
                               FIND FIRST VarGr NO-LOCK WHERE 
                                 VarGr.Vg = VareBehLinje.Vg NO-ERROR.
                               IF AVAILABLE VarGr THEN 
                                 VareBehLinje.Hg = VarGr.Hg.
                            END.
          WHEN 'LevNr'      THEN DO:
                               VareBehLinje.LevNr = INT(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
                               FIND LevBas NO-LOCK WHERE 
                                   LevBas.LEvNr = VareBehLinje.LevNr NO-ERROR.
                               IF AVAILABLE LevBas THEN 
                                   VareBehLinje.LevNamn = LevBas.LevNamn. 
                            END.
          WHEN 'LevKod'     THEN VareBehLinje.LevKod = STRING(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
          WHEN 'LevFargKod' THEN VareBehLinje.LevFargKod = STRING(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
          WHEN 'ProdNr'     THEN VareBehLinje.ProdNr = INT(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
          WHEN 'Varekost'   THEN DO:
                                ASSIGN 
                                    VareBehLinje.VareKost = DEC(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
                                    bVarekostUpd          = YES.
                            END.
          WHEN 'Pris'       THEN VareBehLinje.Pris = DEC(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
          WHEN 'DB%'        THEN VareBehLinje.DB%  = DEC(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
          WHEN 'DBKr'       THEN VareBehLinje.DBKr = DEC(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
          WHEN 'forhRab%'   THEN VareBehLinje.forhRab% = DEC(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
          WHEN 'InnkjopsPris' THEN VareBehLinje.InnkjopsPris = DEC(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
          WHEN 'Mva%'       THEN VareBehLinje.Mva% = DEC(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
          WHEN 'Rab1Kr'     THEN VareBehLinje.Rab1Kr = DEC(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
        END CASE.    
        hQuery:GET-NEXT().
      END. /* QUERY_LOOP */
      
    END. /* LINJE_OPPDATERING */
  END. /* BLOKKEN */

IF ocReturn = "" THEN 
DO:
  ASSIGN obOk = TRUE
         ocReturn = STRING(VareBehLinje.ArtikkelNr).
  IF AVAILABLE VareBehLinje THEN 
    RELEASE VareBehLinje.
END.
ELSE ocReturn = TRIM(ocReturn,CHR(10)).

END PROCEDURE.

PROCEDURE oppdaterArtBas:
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/
IF ((cAction NE "new" AND AVAIL bFromArtBas AND AVAIL bFromArtPris) OR cAction = "new")
    AND hQuery:IS-OPEN THEN 
DO TRANSACTION ON ERROR UNDO, LEAVE:

  IF CAN-DO("new,copy",cAction) THEN 
  DO:
    hBuffArtBas:BUFFER-CREATE().
    hBuffArtPris:BUFFER-CREATE().
    IF cAction = "new" THEN
      ASSIGN hBuffArtPris:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = hBuffArtBas:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE
             hBuffArtPris:BUFFER-FIELD("ProfilNr"):BUFFER-VALUE   = iProfilNr
             .
  END.
  ELSE DO:
    hBuffArtBas:FIND-BY-ROWID(ROWID(bFromArtBas),EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
    hBuffArtPris:FIND-BY-ROWID(ROWID(bFromArtPris),EXCLUSIVE-LOCK,NO-WAIT) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
      ocReturn = "Artikkel holdes av en annen bruker".
      RETURN.
    END.
  END.
  IF cAction = "copy" THEN 
  DO:
    hBuffArtBas:BUFFER-COPY(BUFFER bFromArtBas:HANDLE,"ArtikkelNr,lopnr,RegistrertDato,RegistrertTid,RegistrertAV").
    hBuffArtPris:BUFFER-COPY(BUFFER bFromArtPris:HANDLE,"ArtikkelNr").
    hBuffArtPris:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE = hBuffArtBas:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE.
  END.

  hQuery:GET-FIRST().
  QUERY_LOOP:
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    ocReturn = ocReturn + ValidateInput(ihBuffer:BUFFER-FIELD("cNavn"):BUFFER-VALUE,
                                        ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
    IF ocReturn NE "" THEN ocReturn = ocReturn + CHR(10).

    hField = hBuffArtBas:BUFFER-FIELD(ihBuffer:BUFFER-FIELD("cNavn"):BUFFER-VALUE) NO-ERROR.
    IF NOT VALID-HANDLE(hField) THEN
      hField = hBuffArtPris:BUFFER-FIELD(ihBuffer:BUFFER-FIELD("cNavn"):BUFFER-VALUE) NO-ERROR.

    IF VALID-HANDLE(hField) THEN 
    VALID-HANDLE-FELT:
    DO:
      
      IF hField:NAME = "Varekost" THEN bVarekostUpd = YES.

      CASE hField:DATA-TYPE:
        WHEN "DECIMAL" THEN 
          DO:
            IF hField:EXTENT > 1 THEN
              hField:BUFFER-VALUE[1] = DEC(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
            ELSE
              hField:BUFFER-VALUE = DEC(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
            IF hField:NAME = "supRab%" AND NOT bForhRab THEN 
              hBuffArtPris:BUFFER-FIELD("Rab1%"):BUFFER-VALUE[1] = DEC(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
            IF hField:NAME = "forhRab%" AND bForhRab THEN 
              hBuffArtPris:BUFFER-FIELD("Rab1%"):BUFFER-VALUE[1] = DEC(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
          END.
        WHEN "DATE"    THEN hField:BUFFER-VALUE = DATE(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
        WHEN "INTEGER" THEN hField:BUFFER-VALUE = INT(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
        WHEN "LOGICAL" THEN hField:BUFFER-VALUE = LOGICAL(ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE).
        OTHERWISE hField:BUFFER-VALUE = ihBuffer:BUFFER-FIELD("cVerdi"):BUFFER-VALUE.
      END CASE.
      
      IF hField:NAME = "Vg" AND CAN-DO("new,copy",cAction) THEN 
      DO:
        RUN SettLopNr.p (hField:BUFFER-VALUE,"N",OUTPUT iLopNr).
        hBuffArtBas:BUFFER-FIELD("lopnr"):BUFFER-VALUE = iLopNr.
        IF cAction = "new" THEN 
        DO:
          FIND FIRST VarGr WHERE VarGr.Vg = hField:BUFFER-VALUE NO-LOCK.
          hBuffArtBas:BUFFER-FIELD("Hg"):BUFFER-VALUE = VarGr.Hg.
        END.
      END.
      ELSE IF hField:NAME = "Beskr" THEN
        hBuffArtBas:BUFFER-FIELD("BongTekst"):BUFFER-VALUE = SUBSTR(hField:BUFFER-VALUE,1,20).
      /* Manuelt opprettet */
      hBuffArtBas:BUFFER-FIELD("ManueltOpprettet"):BUFFER-VALUE = TRUE.   
    END. /* VALID-HANDLE-FELT */
    hQuery:GET-NEXT().
  END. /* QUERY_LOOP */

  IF cAction NE "new" THEN
    fMva% = hBuffArtPris:BUFFER-FIELD("Mva%"):BUFFER-VALUE[1].
  ELSE hBuffArtPris:BUFFER-FIELD("Mva%"):BUFFER-VALUE[1] = fMva%.
 
  ASSIGN hBuffArtPris:BUFFER-FIELD("Varekost"):BUFFER-VALUE[1] = 
                hBuffArtPris:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE[1] 
              - hBuffArtPris:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE[1] * hBuffArtPris:BUFFER-FIELD("Rab1%"):BUFFER-VALUE[1] / 100
              + hBuffArtPris:BUFFER-FIELD("Frakt"):BUFFER-VALUE[1]
           fPrisExMva = hBuffArtPris:BUFFER-FIELD("Pris"):BUFFER-VALUE[1] / (1 + fMva% / 100)
           hBuffArtPris:BUFFER-FIELD("Db%"):BUFFER-VALUE[1] = 
                (fPrisExMva - hBuffArtPris:BUFFER-FIELD("Varekost"):BUFFER-VALUE[1]) / fPrisExMva * 100

          hBuffArtPris:BUFFER-FIELD("MvaKr"):BUFFER-VALUE[1]    = hBuffArtPris:BUFFER-FIELD("Pris"):BUFFER-VALUE[1] - hBuffArtPris:BUFFER-FIELD("Pris"):BUFFER-VALUE[1] / (1 + fMva% / 100)
          hBuffArtPris:BUFFER-FIELD("DbKr"):BUFFER-VALUE[1]     = hBuffArtPris:BUFFER-FIELD("Pris"):BUFFER-VALUE[1] - hBuffArtPris:BUFFER-FIELD("MvaKr"):BUFFER-VALUE[1] - hBuffArtPris:BUFFER-FIELD("Varekost"):BUFFER-VALUE[1]
          hBuffArtPris:BUFFER-FIELD("Rab1Kr"):BUFFER-VALUE[1]   = hBuffArtPris:BUFFER-FIELD("Rab1%"):BUFFER-VALUE[1] * hBuffArtPris:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE[1] / 100
          hBuffArtPris:BUFFER-FIELD("Frakt%"):BUFFER-VALUE[1]   = hBuffArtPris:BUFFER-FIELD("Frakt"):BUFFER-VALUE[1] / (hBuffArtPris:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE[1] - hBuffArtPris:BUFFER-FIELD("Rab1Kr"):BUFFER-VALUE[1]) * 100
          hBuffArtPris:BUFFER-FIELD("ValPris"):BUFFER-VALUE[1]  = hBuffArtPris:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE[1]
          hBuffArtPris:BUFFER-FIELD("EuroPris"):BUFFER-VALUE[1] = hBuffArtPris:BUFFER-FIELD("InnkjopsPris"):BUFFER-VALUE[1] * fEuroKurs
          .
          
  IF ocReturn NE "" THEN UNDO, LEAVE.
END.

IF ocReturn = "" THEN 
DO:
  hBuffArtBas:FIND-CURRENT(NO-LOCK).
  ASSIGN obOk = TRUE
         ocReturn = STRING(hBuffArtBas:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE).
         .
END.
ELSE ocReturn = TRIM(ocReturn,CHR(10)).

END PROCEDURE.

FUNCTION ValidateInput RETURNS CHARACTER
         (INPUT icField AS CHAR,
          INPUT icValue AS CHAR):

  CASE icField:
    WHEN "LevNr"     THEN RETURN IF NOT CAN-FIND(FIRST LevBas WHERE LevBas.LevNr = INT(icValue)) THEN "Ukjent leverandør" ELSE "".
    WHEN "VarGr"     THEN RETURN IF NOT CAN-FIND(FIRST VarGr WHERE VarGr.Vg = INT(icValue)) THEN "Ukjent varegruppe" ELSE "".
    WHEN "StrTypeId" THEN RETURN IF NOT CAN-FIND(FIRST StrType WHERE StrType.StrTypeId = INT(icValue)) THEN "Ukjent størrelsestype" 
                                 ELSE IF INT(icValue) = 1 THEN "Pakkevarer kan ikke registreres her"
                                 ELSE "".
    WHEN "Farg"      THEN RETURN IF NOT CAN-FIND(FIRST Farg WHERE Farg.Farg = INT(icValue)) THEN "Ukjent fargekode" ELSE "".
    WHEN "ProfilNr"  THEN RETURN IF NOT CAN-FIND(FIRST Prisprofil WHERE Prisprofil.Profilnr = INT(icValue)) THEN "Ukjent prisprofil" ELSE "".
    WHEN "MatKod"    THEN RETURN IF NOT CAN-FIND(FIRST Material WHERE Material.MatKod = INT(icValue)) THEN "Ukjent materiale" ELSE "".
    WHEN "Sasong"    THEN RETURN IF NOT CAN-FIND(FIRST Sasong WHERE Sasong.Sasong = INT(icValue)) THEN "Ukjent sesong" ELSE "".
    WHEN "ProdNr"    THEN RETURN IF NOT CAN-FIND(FIRST Produsent WHERE Produsent.ProdNr = INT(icValue)) THEN "Ukjent produsent" ELSE "".
  END CASE.

  RETURN "".
END FUNCTION.

