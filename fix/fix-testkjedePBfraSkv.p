CURRENT-WINDOW:WIDTH = 200.

DEFINE VARIABLE cStr AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cc AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBeligg AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cBelStr AS CHARACTER  NO-UNDO.

ASSIGN cBelStr = "T-ban,Övr-förort-landsort,Sjukhus,Jvg-Lokal,Övr Trafik,Flygplatser,Köpcentrum,Övr Innerstad,Jvg-Riks,Arb-Mässa-Högskola,NEDLAGD".
INPUT FROM "C:\DB\storepointtst\dfiler\PB.skv".
DEFINE TEMP-TABLE TT_Ex
    FIELD kjedenr AS INTE
    FIELD regionnr AS INTE
    FIELD distriktnr AS INTE
    FIELD butikknr AS INTE
    FIELD Butikknavn AS CHAR
    FIELD beliggenhet AS CHAR
    FIELD driftsform AS CHAR
    FIELD driftstype AS CHAR
    FIELD DriftsTypeId  LIKE kjedensbutikker.DriftsTypeId 
    FIELD DriftsFormId  LIKE kjedensbutikker.DriftsFormId 
    FIELD BeliggenhetId LIKE kjedensbutikker.BeliggenhetId
    INDEX but kjedenr regionnr distriktnr butikknr.
REPEAT:
    IMPORT UNFORMATTED cStr.
/*     FIND KjedensButikker WHERE kjedenr = 1    AND               */
/*          regionnr    = int(ENTRY(2,cStr,";")) AND               */
/*          distriktnr  = int(ENTRY(3,cStr,";")) AND               */
/*          butikknr    = int(ENTRY(4,cStr,";")) NO-LOCK NO-ERROR. */
/*     IF NOT AVAIL kjedensbutikker THEN DO:                       */
/*         CREATE kjedensbutikker.                                 */
    CREATE TT_Ex.
     ASSIGN 
            TT_Ex.KjedeNr      = 1
            TT_Ex.RegionNr     = LOOKUP(ENTRY(2,cStr,";"),"Nord,Mellan,Syd")
            TT_Ex.DistriktNr   = IF ENTRY(3,cStr,";") = "PS" THEN 900 ELSE int(ENTRY(3,cStr,";"))
            TT_Ex.ButikkNr     = int(ENTRY(4,cStr,";"))
/*             Firmanavn    = ENTRY(5,cStr,";") */
            TT_Ex.ButikkNavn   = ENTRY(5,cStr,";")
            TT_Ex.Beliggenhet = ENTRY(6,cStr,";")
            TT_Ex.DriftsForm = ENTRY(7,cStr,";")
            TT_Ex.DriftsType = ENTRY(8,cStr,";").
     ASSIGN TT_Ex.BeliggenhetId = LOOKUP(TT_Ex.beliggenhet,cBelStr)
            TT_Ex.DriftsFormId  = LOOKUP(TT_Ex.DriftsForm,"Egen Regi,Franchise")
            TT_Ex.DriftsTypeId  = 0.
END.
INPUT CLOSE.
/* OUTPUT TO "CLIPBOARD".   */
/* PUT UNFORMATTED cc SKIP. */
/* OUTPUT CLOSE.            */
/* FOR EACH TT_Ex.                                                                                 */
/*     FIND kjedensbutikker OF TT_Ex NO-LOCK NO-ERROR.                                             */
/*     IF NOT AVAIL kjedensbutikker THEN DO:                                                       */
/*         DISP TT_Ex WITH FRAME a DOWN WIDTH 200.                                                 */
/*         MESSAGE CAN-FIND(FIRST kjedensbutikker WHERE kjedensbutikker.butikknr = TT_Ex.butikknr) */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                  */
/*     END.                                                                                        */
/* END.                                                                                            */
/* 5104,5115,5148,5160,5305,5317,5502,25102,25145,25160,25165,25378,25392,65285, */
/* FOR EACH TT_Ex:                                                                  */
/*     FIND kjededistrikt WHERE kjededistrikt.kjedenr = 1 AND                       */
/*                            kjededistrikt.regionnr = TT_Ex.regionnr AND           */
/*                            kjededistrikt.distriktnr = TT_Ex.distriktnr NO-ERROR. */
/*     IF NOT AVAIL kjededistrikt THEN                                              */
/*         MESSAGE TT_Ex.butikknr                                                   */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                   */
/* END.                                                                             */
FOR EACH TT_Ex.
 CREATE kjedensbutikker.
 ASSIGN 
        kjedensbutikker.KjedeNr      = 1
        kjedensbutikker.RegionNr     = TT_Ex.RegionNr  
        kjedensbutikker.DistriktNr   = TT_Ex.DistriktNr
        kjedensbutikker.ButikkNr     = TT_Ex.ButikkNr  
        kjedensbutikker.Firmanavn    = TT_Ex.ButikkNavn
        kjedensbutikker.ButikkNavn   = TT_Ex.ButikkNavn
        kjedensbutikker.BeliggenhetId = TT_Ex.BeliggenhetId
        kjedensbutikker.DriftsFormId  = TT_Ex.DriftsFormId 
        kjedensbutikker.DriftsTypeId  = TT_Ex.DriftsTypeId 
        kjedensbutikker.Medlemsstatus = 1.
END.

