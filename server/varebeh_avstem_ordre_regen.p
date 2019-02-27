/* Avstemmingsrapport, messeordre mot ordre
   Parametere: Input: Varebehnr
               Output (ocReturn): Filnavn
   
   Opprettet: 01.09.06 av BHa               
-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEF VAR fVarebehNr      AS DEC  NO-UNDO.
DEF VAR cFileName       AS CHAR NO-UNDO.
def var isumforh        as int  no-undo.
def var isumbest        as int  no-undo.
def var edatosort       as date no-undo.
def var iantsort        as int  no-undo.
DEF VAR cOrdreList      AS CHAR NO-UNDO.
def var iBestStat       as int  no-undo.
DEF VAR dForsteLev      AS DATE NO-UNDO.
DEF VAR iForhUke1       AS INT  NO-UNDO.
DEF VAR iForhUke2       AS INT  NO-UNDO.
DEF VAR iForhUke3       AS INT  NO-UNDO.
DEF VAR iForhUke4       AS INT  NO-UNDO.
DEF VAR iForhBestilt1   AS INT  NO-UNDO.
DEF VAR iForhBestilt2   AS INT  NO-UNDO.
DEF VAR iForhBestilt3   AS INT  NO-UNDO.
DEF VAR iForhBestilt4   AS INT  NO-UNDO.
DEF VAR iBestilt1       AS INT  NO-UNDO.
DEF VAR iBestilt2       AS INT  NO-UNDO.
DEF VAR iBestilt3       AS INT  NO-UNDO.
DEF VAR iBestilt4       AS INT  NO-UNDO.
DEF VAR iBestUke1       AS INT  NO-UNDO.
DEF VAR iBestUke2       AS INT  NO-UNDO.
DEF VAR iBestUke3       AS INT  NO-UNDO.
DEF VAR iBestUke4       AS INT  NO-UNDO.
DEF VAR iWeek           AS INT  NO-UNDO.

DEF BUFFER bBeststr FOR BestStr.

fVarebehNr = DEC(icParam) NO-ERROR.

FIND VarebehHode NO-LOCK
     WHERE VarebehHode.VarebehNr = fVarebehNr
     NO-ERROR.
IF NOT AVAIL VarebehHode THEN DO:
  ocReturn = "Finner ikke messebok " + icParam.
  RETURN.
END.

cFileName       = SESSION:TEMP-DIR + "Avstem_" + STRING(VarebehHode.VarebehNr) + "_" + STRING(YEAR(TODAY)) + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY)) + STRING(TIME) + ".txt".

OUTPUT TO VALUE(cFileName).

PUT UNFORMATTED VarebehHode.VareBehBeskrivelse + "~t~t~tAvstemmingsrapport mot ordre"
                SKIP.
PUT UNFORMATTED 
                "Artikkelnr"             + "~t" 
                "Lev.nr"                 + "~t"
                "Kjedevare"              + "~t"
                "Butikknr"               + "~t"
                "Ant forh"               + "~t"
                "Ant best"               + "~t"
                "Best.stat"              + "~t"
                "Forh oppr"              + "~t"
                "Forh endr"              + "~t" 
                "Endr av"                + "~t" 
                "Første lev"             + "~t"
                "Ordrenr"                        
                SKIP.

for each varebehlinjetrans no-lock
    where varebehlinjetrans.butikknr > 0
      and varebehlinjetrans.varebehnr = fVarebehNr
      and varebehlinjetrans.godkjent  = yes
      and (bestilt1 > 0 or bestilt2 > 0 or bestilt3 > 0 or bestilt4 > 0)
    ,first varebehlinje of varebehlinjetrans no-lock
    ,FIRST varebehlinjeThode OF varebehlinjeTRans NO-LOCK
           WHERE VarebehLinjeTHode.Godkjent = YES
    ,first artbas of varebehlinje no-lock
     break by varebehlinjetrans.artikkelnr
           by varebehlinjetrans.butikknr
      :
   
  assign iantsort = 0
         edatosort = 01/01/1900.
  
  FOR EACH ArtSort NO-LOCK
      WHERE ArtSort.ArtikkelNr = VarebehLinjeTrans.ArtikkelNr
        AND ArtSort.SortId     = VarebehLinjeTrans.Kode
      ,FIRST LevSort OF ArtSort NO-LOCK:
  
    FOR EACH LevSAnt OF LevSort NO-LOCK BY SeqNr:
      iantsort = iantsort + levsant.soant.
      if levsant.edato > edatosort then
        edatosort = levsant.edato.
    END.
  END.
  
  ASSIGN isumforh = isumforh + (bestilt1 + bestilt2 + bestilt3 + bestilt4) * max(1,iantsort)
         iForhBestilt1 = iForhBestilt1 + bestilt1 * max(1,iantsort)
         iForhBestilt2 = iForhBestilt2 + bestilt2 * max(1,iantsort)
         iForhBestilt3 = iForhBestilt3 + bestilt3 * max(1,iantsort)
         iForhBestilt4 = iForhBestilt4 + bestilt4 * max(1,iantsort)
         iForhUke1     = IF levuke1 NE 0 THEN levuke1 ELSE iForhUke1
         iForhUke2     = IF levuke2 NE 0 THEN levuke2 ELSE iForhUke2
         iForhUke3     = IF levuke3 NE 0 THEN levuke3 ELSE iForhUke3
         iForhUke4     = IF levuke4 NE 0 THEN levuke4 ELSE iForhUke4
         .
      
  if last-of(varebehlinjetrans.butikknr) then do:
    ASSIGN isumbest     = 0
           cOrdreList   = ""
           iBestStat    = 0
           dForsteLev   = 01/01/2100
           .
  
    FOR EACH besthode NO-LOCK
         WHERE besthode.varebehnr  = fvarebehnr
           AND besthode.artikkelnr = varebehlinjetrans.artikkelnr
         ,each beststr no-lock
               OF besthode
               where beststr.butik = varebehlinjetrans.butikknr
              :
    
   
      if not can-find(first bbeststr no-lock
                      where bbeststr.bestnr = beststr.bestnr
                        and bbeststr.butik  = beststr.butik
                        and bbeststr.storl  = beststr.storl
                        and bbeststr.beststat > beststr.beststat) then DO:
        isumbest = isumbest + beststr.bestilt.

        RUN weeknum.p(besthode.levdato,OUTPUT iWeek).
        IF iWeek = iForhUke1 THEN 
          ASSIGN iBestUke1 = iForhUke1
                 iBestilt1 = iBestilt1 + beststr.bestilt.
        ELSE IF iWeek = iForhUke2 THEN 
          ASSIGN iBestUke2 = iForhUke2
                 iBestilt2 = iBestilt2 + beststr.bestilt.
        ELSE IF iWeek = iForhUke3 THEN 
          ASSIGN iBestUke3 = iForhUke3
                 iBestilt3 = iBestilt3 + beststr.bestilt.
        ELSE IF iWeek = iForhUke4 THEN 
          ASSIGN iBestUke4 = iForhUke1
                 iBestilt4 = iBestilt4 + beststr.bestilt.
      END.
   
  
      IF BestHode.BestStat > iBestStat THEN iBestStat = BestHode.BestStat.
  
      IF BestHode.LevDato < dForsteLev THEN dForsteLev = BestHode.LevDato.
       
      IF NOT CAN-DO(cOrdreList,STRING(besthode.ordrenr)) THEN
        cOrdreList = cOrdreList + STRING(besthode.ordrenr) + ",".
    END.
    if isumforh ne isumbest then DO: 
      export delimiter "~t" 
             varebehlinjetrans.artikkelnr
             varebehlinje.levnr
             artbas.kjedevare
             varebehlinjetrans.butikknr
             isumforh
             isumbest
             iBestStat
             varebehlinjetrans.registrertdato
             varebehlinjetrans.edato
             varebehlinjetrans.brukerid
             dForsteLev
             cOrdreList 
            .
      IF iForhBestilt1 NE 0 AND iBestilt1 = 0 THEN
        RUN bha\genvarebehmesse_EN_best.p(fVarebehNr,STRING(varebehlinjetrans.butikknr),STRING(varebehlinjetrans.artikkelnr),1).
      ELSE IF iForhBestilt2 NE 0 AND iBestilt2 = 0 THEN
        RUN bha\genvarebehmesse_EN_best.p(fVarebehNr,STRING(varebehlinjetrans.butikknr),STRING(varebehlinjetrans.artikkelnr),2).
      ELSE IF iForhBestilt3 NE 0 AND iBestilt3 = 0 THEN
        RUN bha\genvarebehmesse_EN_best.p(fVarebehNr,STRING(varebehlinjetrans.butikknr),STRING(varebehlinjetrans.artikkelnr),3).
      ELSE IF iForhBestilt4 NE 0 AND iBestilt4 = 0 THEN
        RUN bha\genvarebehmesse_EN_best.p(fVarebehNr,STRING(varebehlinjetrans.butikknr),STRING(varebehlinjetrans.artikkelnr),4).
    END.
  
    ASSIGN isumforh = 0
           iForhBestilt1 = 0
           iForhBestilt2 = 0
           iForhBestilt3 = 0
           iForhBestilt4 = 0
           iForhUke1     = 0
           iForhUke2     = 0
           iForhUke3     = 0
           iForhUke4     = 0
           iBestUke1     = 0
           iBestUke2     = 0
           iBestUke3     = 0
           iBestUke4     = 0
           iBestilt1     = 0
           iBestilt2     = 0
           iBestilt3     = 0
           iBestilt4     = 0
           .
  end.   
end.   

OUTPUT CLOSE.
         
IF ocReturn = "" THEN
  ASSIGN obOk = YES
         ocReturn = cFileName.
