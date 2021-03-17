/* Beregner totalsummer for en messebok, alle tilgj butikker, enten best.forslag eller bestilt 
   Parametere:  <butikkliste>|"bestilt" eller "forslag"
                Buffer-parameter inneholder gjeldende artikler i messebok
   Opprettet: 01.02.07 av BHa                  
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR hQuery       AS HANDLE NO-UNDO.
DEF VAR ix           AS INT    NO-UNDO.
DEF VAR fTotal       AS DEC    NO-UNDO.
DEF VAR iAntFord     AS INT    NO-UNDO.
DEF VAR cButikkListe AS CHAR   NO-UNDO.
DEF VAR cBestType    AS CHAR   NO-UNDO.

ASSIGN cButikkListe = ENTRY(1,icParam,"|")
       cBestType    = ENTRY(2,icParam,"|")
       .

DEF TEMP-TABLE ttButTotal
    FIELD iButikkNr  AS INT
    FIELD cButNavn   AS CHAR
    FIELD fButTotal  AS DEC
    .

CREATE QUERY hQuery.
hQuery:SET-BUFFERS(ihBuffer).
hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME).

hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  FOR EACH varebehlinjetrans NO-LOCK
      WHERE varebehlinjetrans.VareBehNr  = DECIMAL(ihBuffer:BUFFER-FIELD("VarebehNr"):BUFFER-VALUE)
        AND varebehlinjetrans.ArtikkelNr = DECIMAL(ihBuffer:BUFFER-FIELD("ArtikkelNr"):BUFFER-VALUE)
        AND CAN-DO(cButikkListe,STRING(varebehlinjetrans.ButikkNr))
        AND varebehlinjetrans.RegistrertBestilling
        AND (IF cBestType = "forslag" THEN 
               varebehlinjetrans.GodkjentBestilling = NO
            ELSE varebehlinjetrans.GodkjentBestilling = YES)
      :


    iAntFord = 0.
    FOR EACH ArtSort NO-LOCK
        WHERE ArtSort.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
          AND ArtSort.SortId     = VarebehLinjeTrans.Kode
       ,FIRST LevSort OF ArtSort NO-LOCK:
  
      FOR EACH LevSAnt OF LevSort NO-LOCK:
        iAntFord      = iAntFord + LevSAnt.SoAnt.
      END.
    END.

    FIND FIRST ttButTotal
         WHERE ttButTotal.iButikkNr = varebehlinjetrans.butikknr
         NO-ERROR.
    IF NOT AVAIL ttButTotal THEN DO:
      FIND FIRST butiker NO-LOCK
           WHERE butiker.butik = varebehlinjetrans.butikknr
           NO-ERROR.
      IF AVAIL butiker THEN DO:
        CREATE ttButTotal.
        ASSIGN ttButTotal.iButikkNr = butiker.butik
               ttButTotal.cButNavn  = butiker.butnamn.
      END.
    END.
    IF AVAIL ttButTotal THEN
      ttButTotal.fButTotal = ttButTotal.fButTotal + (VareBehLinjeTrans.Bestilt1 + VareBehLinjeTrans.Bestilt2 + VareBehLinjeTrans.Bestilt3 + VareBehLinjeTrans.Bestilt4) * DECIMAL(ihBuffer:BUFFER-FIELD("Varekost"):BUFFER-VALUE) * MAX(1,iAntFord).
  
    fTotal = fTotal + (VareBehLinjeTrans.Bestilt1 + VareBehLinjeTrans.Bestilt2 + VareBehLinjeTrans.Bestilt3 + VareBehLinjeTrans.Bestilt4) * DECIMAL(ihBuffer:BUFFER-FIELD("Varekost"):BUFFER-VALUE) * MAX(1,iAntFord).
  END.
  hQuery:GET-NEXT().
END.

FOR EACH ttButTotal 
    BY ttButTotal.iButikkNr:
  ocReturn = ocReturn + STRING(ttButTotal.iButikkNr) + "~t" + ttButTotal.cButNavn + FILL(" ",40 - LENGTH(ttButTotal.cButNavn)) + "~t~t" + STRING(ttButTotal.fButTotal) + CHR(10).
END.

ASSIGN ocReturn = ocReturn + CHR(10) + "~tTotalt:" + "~t" + STRING(fTotal,"->>,>>>,>>9.99")
       obOk     = TRUE.

DELETE OBJECT hQuery.
