/* Sett verdi for mange forekomster i VarebehLinjeTransTrans 
   Parametere:  <Feltnavn>,<Verdi> i parameterstreng
           
                temp-tabell med feltene Varebehnr, Artikkelnr og strekkode (kode) 
                
              eller
                 Tilleggsliste over rowid's med VarebehLinjeTransr i parameterstreng:
                   Fom entry(3): <"ROWID">,<Rowid1,Rowid2..>
              eller
                   Tilleggsiste over artikkelnr i parameterstreng:
                   Fom entry(3): <"ARTNR">,<Varebehnr>,<Artnr1,Artnr2..>
   Kommentar: Benytter eksisterende rutiner, varetilvpielogg.p og (evt) eloggtilvpivare.p
   
   Opprettet: 06.09.04 av BHa          
   Endret:    19.09.05 av BHa
            - Tar med leveringsuke som parameter 
            - Programmet brukes ikke lenger til å endre leveringsuker       
            - Ny parameterstreng: <Feltnavn, best.kvt>|<Verdi>|Lev.uke|
   Endret:    17.01.06 av BHa
            - Tar med parameter om bruker er en leverandør. I så fall skal bestillingen settes som forslag
            - På rader med verdi settes RegistrertBestilling
            - Ny parameterstreng: <Feltnavn, best.kvt>|<Verdi>|Lev.uke|LevNrListe|(evt rowids.) 
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery      AS HANDLE NO-UNDO.
DEF VAR ix          AS INT    NO-UNDO.
DEF VAR httTable    AS HANDLE NO-UNDO.
DEF VAR hVBLbuffer  AS HANDLE NO-UNDO.
DEF VAR fVarebehNr  AS DEC    NO-UNDO.
DEF VAR cLevUke     AS CHAR   NO-UNDO.
DEF VAR fArtikkelNr AS DEC    NO-UNDO.
DEF VAR iButikkNr   AS INT    NO-UNDO.

hVBLbuffer = BUFFER VarebehLinjeTrans:HANDLE.

IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam,"|") > 4 THEN DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("VarebehNr","VarebehLinjeTrans.VarebehNr").
  httTable:ADD-LIKE-FIELD("ArtikkelNr","VarebehLinjeTrans.ArtikkelNr").
  httTable:ADD-LIKE-FIELD("Kode","VarebehLinjeTrans.Kode").
  httTable:ADD-LIKE-FIELD("ButikkNr","VarebehLinjeTrans.ButikkNr").
  httTable:TEMP-TABLE-PREPARE("ttVarebehLinjeTrans").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  IF ENTRY(5,icParam,"|") = "ROWID" THEN
    DO ix = 6 TO NUM-ENTRIES(icParam,"|"):
      FIND VarebehLinjeTrans WHERE ROWID(VarebehLinjeTrans) = TO-ROWID(ENTRY(ix,icParam,"|")) NO-LOCK NO-ERROR.
      IF AVAIL VarebehLinjeTrans THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER VarebehLinjeTrans:HANDLE).
      END.
    END.
  ELSE DO:
    ocReturn = "Ugyldig angivelse av parameter til program: " + icParam + CHR(10) + PROGRAM-NAME(1).
    obOk = FALSE.
    RETURN.

    /*     fVarebehNr = DEC(ENTRY(5,icParam,"|")).                                                  */
/*     DO ix = 6 TO NUM-ENTRIES(icParam,"|"):                                                   */
/*       FIND VarebehLinjeTrans                                                                 */
/*            WHERE VarebehLinjeTrans.VarebehNr = fVarebehnr                                    */
/*              AND VarebehLinjeTrans.ArtikkelNr = DEC(ENTRY(ix,icParam,"|")) NO-LOCK NO-ERROR. */
/*       IF AVAIL VarebehLinjeTrans THEN DO:                                                    */
/*         ihBuffer:BUFFER-CREATE().                                                            */
/*         ihBuffer:BUFFER-COPY(BUFFER VarebehLinjeTrans:HANDLE).                               */
/*       END.                                                                                   */
/*     END.                                                                                     */
  END.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

DO TRANSACTION:
  hQuery:GET-FIRST().
  IF ihBuffer:AVAIL THEN
    ASSIGN fVarebehNr  = ihBuffer:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE
           fArtikkelNr = ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE
           iButikkNr   = ihBuffer:BUFFER-FIELD("ButikkNr"):BUFFER-VALUE.
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:

    obOK = hVBLbuffer:FIND-FIRST("WHERE Varebehnr   = " + STRING(ihBuffer:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE) 
                                 + " AND Artikkelnr = " + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)
                                 + " AND Butikknr   = " + STRING(ihBuffer:BUFFER-FIELD("Butikknr"):BUFFER-VALUE)
                                 + " AND Kode       = '" + ihBuffer:BUFFER-FIELD("Kode"):BUFFER-VALUE + "'"
                                 ,
                                 EXCLUSIVE-LOCK) NO-ERROR.
    IF obOk THEN DO:
      CASE ENTRY(1,icParam,"|"):
        WHEN "Bestilt1" THEN
          ASSIGN hVBLbuffer:BUFFER-FIELD("Bestilt1"):BUFFER-VALUE = DEC(ENTRY(2,icParam,"|"))
                 hVBLbuffer:BUFFER-FIELD("LevUke1"):BUFFER-VALUE  = INT("20" + SUBSTR(ENTRY(3,icParam,"|"),3) + SUBSTR(ENTRY(3,icParam,"|"),1,2))
                 .
        WHEN "Bestilt2" THEN
          ASSIGN hVBLbuffer:BUFFER-FIELD("Bestilt2"):BUFFER-VALUE = DEC(ENTRY(2,icParam,"|"))
                 hVBLbuffer:BUFFER-FIELD("LevUke2"):BUFFER-VALUE  = INT("20" + SUBSTR(ENTRY(3,icParam,"|"),3) + SUBSTR(ENTRY(3,icParam,"|"),1,2))
                 .
        WHEN "Bestilt3" THEN
          ASSIGN hVBLbuffer:BUFFER-FIELD("Bestilt3"):BUFFER-VALUE = DEC(ENTRY(2,icParam,"|"))
                 hVBLbuffer:BUFFER-FIELD("LevUke3"):BUFFER-VALUE  = INT("20" + SUBSTR(ENTRY(3,icParam,"|"),3) + SUBSTR(ENTRY(3,icParam,"|"),1,2))
                 .
        WHEN "Bestilt4" THEN
          ASSIGN hVBLbuffer:BUFFER-FIELD("Bestilt4"):BUFFER-VALUE = DEC(ENTRY(2,icParam,"|"))
                 hVBLbuffer:BUFFER-FIELD("LevUke4"):BUFFER-VALUE  = INT("20" + SUBSTR(ENTRY(3,icParam,"|"),3) + SUBSTR(ENTRY(3,icParam,"|"),1,2))
                 .
        WHEN "LevUke1" THEN
          hVBLbuffer:BUFFER-FIELD("LevUke1"):BUFFER-VALUE  = INT("20" + SUBSTR(ENTRY(2,icParam,"|"),3) + SUBSTR(ENTRY(2,icParam,"|"),1,2)).
        WHEN "LevUke2" THEN
          hVBLbuffer:BUFFER-FIELD("LevUke2"):BUFFER-VALUE  = INT("20" + SUBSTR(ENTRY(2,icParam,"|"),3) + SUBSTR(ENTRY(2,icParam,"|"),1,2)).
        WHEN "LevUke3" THEN
          hVBLbuffer:BUFFER-FIELD("LevUke3"):BUFFER-VALUE  = INT("20" + SUBSTR(ENTRY(2,icParam,"|"),3) + SUBSTR(ENTRY(2,icParam,"|"),1,2)).
        WHEN "LevUke4" THEN
          hVBLbuffer:BUFFER-FIELD("LevUke4"):BUFFER-VALUE  = INT("20" + SUBSTR(ENTRY(2,icParam,"|"),3) + SUBSTR(ENTRY(2,icParam,"|"),1,2)).
      END CASE.
    END.
    ELSE DO:
      ocReturn = "Varebeh / Art "
                 + STRING(ihBuffer:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
                 + " / " + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE) +
                 " ikke tilgj. for oppdatering".
      LEAVE.
    END.
    hQuery:GET-NEXT().
  END.

  IF NOT icParam BEGINS "LevUke" THEN DO:
    FOR EACH VarebehLinjeTrans EXCLUSIVE-LOCK
        WHERE VarebehLinjeTrans.VarebehNr  = fVarebehNr
          AND VarebehLinjeTrans.ArtikkelNr = fArtikkelNr
          AND VarebehLinjeTrans.ButikkNr   = iButikkNr
          AND (Bestilt1 > 0 OR Bestilt2 > 0 OR Bestilt3 > 0 OR Bestilt4 > 0):
      IF ENTRY(4,icParam,"|") NE "*" THEN  /* Leverandørbruker */
         VarebehLinjeTrans.GodkjentBestilling = FALSE.
      ELSE
        VarebehLinjeTrans.GodkjentBestilling = TRUE.
      VarebehLinjeTrans.RegistrertBestilling = TRUE.
    END.
      
    FOR EACH VarebehLinjeTrans EXCLUSIVE-LOCK
        WHERE VarebehLinjeTrans.VarebehNr  = fVarebehNr
          AND VarebehLinjeTrans.ArtikkelNr = fArtikkelNr
          AND VarebehLinjeTrans.ButikkNr   = iButikkNr
          AND Bestilt1 = 0 AND Bestilt2 = 0 AND Bestilt3 = 0 AND Bestilt4 = 0:
      VarebehLinjeTrans.GodkjentBestilling   = FALSE.
      VarebehLinjeTrans.RegistrertBestilling = FALSE.
    END.
  END.
END.

DELETE OBJECT hQuery.

IF ocReturn = "" THEN obOk = TRUE.

