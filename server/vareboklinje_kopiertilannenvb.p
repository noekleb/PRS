/* Kopier artikler i en varebok til en annen 
   Parametere:  <TilVareboknr1|TilVareboknr2..>,ROWID eller FraVarebokNr,<Rowid1|Rowid2..>
   
                temp-tabell med feltene Vareboknr og Artikkelnr 
                
              eller
                 Tilleggsliste over rowid's med vareboklinjer i parameterstreng:
                   Fom entry(3): <"ROWID">,<Rowid1,Rowid2..>
              eller
                   Tilleggsiste over artikkelnr i parameterstreng:
                   Fom entry(3): <"ARTNR">,<Vareboknr>,<Artnr1,Artnr2..>
   Kommentar: Benytter eksisterende rutiner, varetilvpielogg.p og (evt) eloggtilvpivare.p
   
   Opprettet: 02.09.04 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR httTable     AS HANDLE NO-UNDO.
DEF VAR hFromVBLbuff AS HANDLE NO-UNDO.
DEF VAR hToVBLbuff   AS HANDLE NO-UNDO.
DEF VAR fVarebokNr   AS DEC    NO-UNDO.

DEF BUFFER bVBL  FOR VarebokLinje.

hFromVBLbuff = BUFFER VarebokLinje:HANDLE.
CREATE BUFFER hToVBLbuff FOR TABLE "VarebokLinje".

IF NOT VALID-HANDLE(ihBuffer) AND NUM-ENTRIES(icParam) > 1 THEN DO:
  CREATE TEMP-TABLE httTable.
  httTable:ADD-LIKE-FIELD("VarebokNr","VarebokLinje.VarebokNr").
  httTable:ADD-LIKE-FIELD("ArtikkelNr","VarebokLinje.ArtikkelNr").
  httTable:TEMP-TABLE-PREPARE("ttVarebokLinje").
  ihBuffer = httTable:DEFAULT-BUFFER-HANDLE.
  IF ENTRY(2,icParam) = "ROWID" THEN
    DO ix = 1 TO NUM-ENTRIES(ENTRY(3,icParam),"|"):
      FIND VarebokLinje WHERE ROWID(VarebokLinje) = TO-ROWID(ENTRY(ix,ENTRY(3,icParam),"|")) NO-LOCK NO-ERROR.
      IF AVAIL VarebokLinje THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER VarebokLinje:HANDLE).
      END.
    END.
  ELSE DO:
    fVarebokNr = DEC(ENTRY(2,icParam)).
    DO ix = 1 TO NUM-ENTRIES(ENTRY(3,icParam),"|"):
      FIND VarebokLinje 
           WHERE VarebokLinje.VarebokNr = fVareboknr
             AND VarebokLinje.ArtikkelNr = DEC(ENTRY(ix,ENTRY(3,icParam),"|")) NO-LOCK NO-ERROR.
      IF AVAIL VarebokLinje THEN DO:
        ihBuffer:BUFFER-CREATE().
        ihBuffer:BUFFER-COPY(BUFFER VarebokLinje:HANDLE).
      END.
    END.
  END.
END.

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").
hQuery:QUERY-OPEN().

DO TRANSACTION:
  hQuery:GET-FIRST().
  REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  
    obOK = hFromVBLbuff:FIND-FIRST("WHERE Vareboknr = " + STRING(ihBuffer:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE) 
                                  + " AND Artikkelnr = " + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE),
                                   NO-LOCK) NO-ERROR.
          
    IF obOk THEN DO ix = 1 TO NUM-ENTRIES(ENTRY(1,icParam),"|"):
/*       obOK = hToVBLbuff:FIND-FIRST("WHERE VarebokNr = " + ENTRY(ix,ENTRY(1,icParam),"|")                             */
/*                                   + " AND ArtikkelNr = " + STRING(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE), */
/*                                     NO-LOCK) NO-ERROR.                                                               */
      FIND FIRST bVBL 
           WHERE bVBL.VarebokNr  = DEC(ENTRY(ix,ENTRY(1,icParam),"|"))
             AND bVBL.ArtikkelNr = DEC(ihBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE)
           NO-LOCK NO-ERROR.
      IF NOT AVAIL bVBL THEN DO:
        hToVBLbuff:BUFFER-CREATE().
        hToVBLbuff:BUFFER-COPY(hFromVBLbuff,"VarebokNr").
        hToVBLbuff:BUFFER-FIELD("VarebokNr"):BUFFER-VALUE = DEC(ENTRY(ix,ENTRY(1,icParam),"|")).
      END.
      ELSE DO:
        IF hToVBLbuff:FIND-BY-ROWID(ROWID(bVBL),EXCLUSIVE-LOCK) THEN
          hToVBLbuff:BUFFER-COPY(hFromVBLbuff,"VarebokNr,ArtikkelNr").
      END.
    END.
    hQuery:GET-NEXT().
  END.
END.

DELETE OBJECT hQuery.
IF VALID-HANDLE(httTable) THEN DELETE OBJECT httTable.
DELETE OBJECT hToVBLbuff.


IF ocReturn = "" THEN obOk = TRUE.

