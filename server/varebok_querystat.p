/* Henter statistikk for utvalgte felter
   Parametere: Input: Query|stat-felter
               Output (ocReturn): stat-felter med tilh.verdier
   
   Opprettet: 10.12.04 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR ix                  AS INT NO-UNDO.
DEF VAR iy                  AS INT NO-UNDO.
DEF VAR cStatFields         AS CHAR NO-UNDO.
DEF VAR cPrefixedStatFields AS CHAR NO-UNDO.
DEF VAR iCount              AS INT NO-UNDO.
DEF VAR hQuery              AS HANDLE NO-UNDO.
DEF VAR hBuffer             AS HANDLE NO-UNDO.
DEF VAR fTot                AS DEC NO-UNDO EXTENT 20.
DEF VAR fAvg                AS DEC NO-UNDO EXTENT 20.
DEF VAR cCriteria           AS CHAR NO-UNDO.
DEF VAR fTotInnkjopspris    AS DEC NO-UNDO.
DEF VAR fTotSupInnkjopspris AS DEC NO-UNDO.
DEF VAR fTotVarekost        AS DEC NO-UNDO.
DEF VAR fTotSupVarekost     AS DEC NO-UNDO.
DEF VAR fTotDBkr            AS DEC NO-UNDO.
DEF VAR fTotSupDBkr         AS DEC NO-UNDO.
DEF VAR fTotPrisExMVA       AS DEC NO-UNDO.
DEF VAR fTotSupPrisExMVA    AS DEC NO-UNDO.
DEF VAR fTotPris            AS DEC NO-UNDO.
DEF VAR fTotSupPris         AS DEC NO-UNDO.
DEF VAR fTotAntall          AS DEC NO-UNDO.
DEF VAR fTotSupAntall       AS DEC NO-UNDO.

ASSIGN cStatFields         = TRIM(ENTRY(2,icParam,"|"),",")
       cPrefixedStatFields = TRIM(ENTRY(3,icParam,"|"),",")
       icParam = REPLACE(icParam,"and false","")
       cCriteria           = ENTRY(1,icParam,"|")
       .
CREATE BUFFER hBuffer FOR TABLE "VarebokLinje".
CREATE QUERY hQuery.
hQuery:SET-BUFFERS(hBuffer).
hQuery:QUERY-PREPARE("FOR EACH VarebokLinje NO-LOCK " + cCriteria).
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().

REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ASSIGN iCount = iCount + 1
         fTotInnkjopspris    = fTotInnkjopspris    + hBuffer:BUFFER-FIELD("Innkjopspris"):BUFFER-VALUE * hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE
         fTotSupInnkjopspris = fTotSupInnkjopspris + hBuffer:BUFFER-FIELD("Innkjopspris"):BUFFER-VALUE * hBuffer:BUFFER-FIELD("supAntall"):BUFFER-VALUE
         fTotVarekost        = fTotVarekost        + hBuffer:BUFFER-FIELD("Varekost"):BUFFER-VALUE * hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE
         fTotSupVarekost     = fTotSupVarekost     + hBuffer:BUFFER-FIELD("SupVarekost"):BUFFER-VALUE * hBuffer:BUFFER-FIELD("supAntall"):BUFFER-VALUE
         fTotDBkr            = fTotDBkr            + hBuffer:BUFFER-FIELD("DBkr"):BUFFER-VALUE * hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE
         fTotSupDBkr         = fTotSupDBkr         + hBuffer:BUFFER-FIELD("SupDBkr"):BUFFER-VALUE * hBuffer:BUFFER-FIELD("supAntall"):BUFFER-VALUE
         fTotPrisExMVA       = fTotPrisExMVA       + hBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE / (1 + hBuffer:BUFFER-FIELD("MVA%"):BUFFER-VALUE / 100) * hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE
         fTotSupPrisExMVA    = fTotSupPrisExMVA    + hBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE / (1 + hBuffer:BUFFER-FIELD("MVA%"):BUFFER-VALUE / 100) * hBuffer:BUFFER-FIELD("supAntall"):BUFFER-VALUE
         fTotPris            = fTotPris            + hBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE * hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE
         fTotSupPris         = fTotSupPris         + hBuffer:BUFFER-FIELD("Pris"):BUFFER-VALUE * hBuffer:BUFFER-FIELD("supAntall"):BUFFER-VALUE
         fTotAntall          = fTotAntall          + hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE
         fTotSupAntall       = fTotSupAntall       + hBuffer:BUFFER-FIELD("supAntall"):BUFFER-VALUE
         .

  DO ix = 1 TO NUM-ENTRIES(cStatFields):
    IF CAN-DO(cPrefixedStatFields,"tot_" + ENTRY(ix,cStatFields)) THEN DO:
      IF ENTRY(ix,cStatFields) MATCHES "*sup*" AND NOT ENTRY(ix,cStatFields) MATCHES "*Antall*" THEN
        fTot[ix] = fTot[ix] + hBuffer:BUFFER-FIELD("supAntall"):BUFFER-VALUE * hBuffer:BUFFER-FIELD(ENTRY(ix,cStatFields)):BUFFER-VALUE.
      ELSE IF NOT ENTRY(ix,cStatFields) MATCHES "*Antall*" THEN
        fTot[ix] = fTot[ix] + hBuffer:BUFFER-FIELD("Antall"):BUFFER-VALUE * hBuffer:BUFFER-FIELD(ENTRY(ix,cStatFields)):BUFFER-VALUE.
      ELSE IF ENTRY(ix,cStatFields) MATCHES "*Antall*" THEN
        fTot[ix] = fTot[ix] + hBuffer:BUFFER-FIELD(ENTRY(ix,cStatFields)):BUFFER-VALUE.
    END.
  END.

  hQuery:GET-NEXT().
END.

DO ix = 1 TO NUM-ENTRIES(cStatFields):
  IF CAN-DO(cPrefixedStatFields,"tot_" + ENTRY(ix,cStatFields)) THEN 
    ocReturn = ocReturn + ENTRY(ix,cStatFields) + "|" + STRING(fTot[ix]) + ";".
  ELSE DO:
    CASE ENTRY(ix,cStatFields):
      WHEN "forhKalkyle" THEN ocReturn = ocReturn + ENTRY(ix,cStatFields) + "|" + STRING(fTotPris / fTotVarekost) + ";".
      WHEN "supKalkyle"  THEN ocReturn = ocReturn + ENTRY(ix,cStatFields) + "|" + STRING(fTotSupPris / fTotSupVarekost) + ";".
      WHEN "forhRab%"    THEN ocReturn = ocReturn + ENTRY(ix,cStatFields) + "|" + STRING((fTotInnkjopsPris - fTotVarekost) / fTotInnkjopsPris * 100) + ";".
      WHEN "supRab%"     THEN ocReturn = ocReturn + ENTRY(ix,cStatFields) + "|" + STRING((fTotSupInnkjopsPris - fTotSupVarekost) / fTotSupInnkjopsPris * 100) + ";".
      WHEN "DB%"         THEN ocReturn = ocReturn + ENTRY(ix,cStatFields) + "|" + STRING(fTotDBkr / fTotPrisExMVA * 100) + ";".
      WHEN "supDB%"      THEN ocReturn = ocReturn + ENTRY(ix,cStatFields) + "|" + STRING(fTotSupDBkr / fTotSupPrisExMVA * 100) + ";".
    END CASE.
  END.
END.

ocReturn = ocReturn + "rowcount|" + STRING(iCount).
DELETE OBJECT hQuery.
DELETE OBJECT hBuffer.

IF iCount = 0 THEN
  ocReturn = "Vareboken inneholder ingen artikler".
ELSE 
  obOk     = TRUE.



