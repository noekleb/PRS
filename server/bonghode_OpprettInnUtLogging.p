/* bonghode_OpprettInnUtLogging.p

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE iBongNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iBongLinje AS INTEGER NO-UNDO.
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iKasseNr AS INTEGER NO-UNDO.
DEFINE VARIABLE cAnsattNr AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMerknad AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTTId AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSelgerNr AS CHARACTER NO-UNDO.

{syspara.i 150 1 3 iButNr INT}

ASSIGN 
  iBongNr   = TIME
  iKasseNr  = 99
  cSelgerNr = ENTRY(1,icParam,'|')
  cMerknad  = ENTRY(2,icParam,'|')
  cTTId     = ENTRY(3,icParam,'|')
  .

FIND LAST BongHode NO-LOCK WHERE
      BongHode.ButikkNr = iButNr AND
      BongHode.GruppeNr = 1 AND
      BongHode.KasseNr  = iKasseNr  AND
      BongHode.Dato     = TODAY NO-ERROR.
  IF AVAILABLE BongHode THEN
    iBongNr = BongHode.BongNr + 1.
  ELSE 
    iBongNr = 1. 

FIND FIRST Selger NO-LOCK WHERE 
  Selger.SelgerNr = DEC(cSelgerNr) NO-ERROR.
  
DO TRANSACTION:
  CREATE BongHode.
  ASSIGN
    BongHode.ButikkNr      = ibutNr 
    BongHode.GruppeNr      = 1 
    BongHode.KasseNr       = iKasseNr  
    BongHode.Dato          = TODAY 
    BongHode.Tid           = TIME
    BongHode.BongNr        = iBongNr
    BongHode.BongStatus    = 0 /* Under klargjøring */
    BongHode.OpdKvit       = TRUE
    Bonghode.DataSettId    = 0
    BongHode.Utskriftskopi = "Innlogging " + 
                             STRING(iBongNr) + "."
    BongHode.KassererNr    = 1
    BongHode.SelgerNr      = IF AVAILABLE Selger THEN Selger.SelgerNr ELSE 0
    BongHode.KassererNavn  = IF AVAILABLE Selger THEN Selger.ForNavn + ' ' + Selger.Navn ELSE ''
    BongHode.SelgerNavn    = IF AVAILABLE Selger THEN Selger.ForNavn + ' ' + Selger.Navn ELSE ''
    BongHode.KOrdre_Id     = 0
    BongHode.Konvertert    = TRUE
    BongHode.Belop         = 0
    BongHode.BongStatus    = 5 /* Oppdatert */
    BongHode.Logg          = cMerknad
    .
  FIND CURRENT BongHode NO-LOCK.
  
  CREATE BongLinje. /* */
  ASSIGN
      BongLinje.B_Id         = BongHode.B_Id
      BongLinje.ButikkNr     = BongHode.ButikkNr 
      BongLinje.GruppeNr     = BongHode.GruppeNr 
      BongLinje.KasseNr      = BongHode.KasseNr  
      BongLinje.Dato         = BongHode.Dato /*pBongDato*/     
      BongLinje.BongNr       = BongHode.BongNr   
      BongLinje.TTId         = INT(cTTId)
      BongLinje.TBId         = 1
      BongLinje.LinjeNr      = iBongLinje + 1 /*BongLinje*/
      BongLinje.TransDato    = BongHode.Dato
      BongLinje.TransTid     = BongHode.Tid
      BongLinje.RefNr        = 1
      BongLinje.Reftekst     = cMerknad
      BongLinje.BongTekst  = "INNLOGGING"
      BongLinje.Antall     = 0
      BongLinje.LinjeSum   = 0
      BongLinje.BongPris   = 0
      .
  RELEASE BongLinje.
  
END. /* TRANSACTION */

ASSIGN 
    obOk     = TRUE 
    .


/*cNettButikkType = (DYNAMIC-FUNCTION("getFieldValues","SysPara",                                       */
/*                        "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 20","Parameter1")).            */
/*                                                                                                      */
/*CREATE QUERY hQuery.                                                                                  */
/*hQuery:SET-BUFFERS(ihBuffer).                                                                         */
/*hQuery:QUERY-PREPARE("FOR EACH " + ihBuffer:NAME + " NO-LOCK").                                       */
/*hQuery:QUERY-OPEN().                                                                                  */
/*                                                                                                      */
/*hQuery:GET-FIRST().                                                                                   */
/*REPEAT WHILE NOT hQuery:QUERY-OFF-END:                                                                */
/*  ASSIGN                                                                                              */
/*    lNekad   = FALSE                                                                                  */
/*    lPs12    = FALSE                                                                                  */
/*    ocReturn = ''                                                                                     */
/*    .                                                                                                 */
/*                                                                                                      */
/*  FIND FIRST KordreHode WHERE                                                                         */
/*      KordreHode.KOrdre_Id = DEC(ihBuffer:BUFFER-FIELD('KOrdre_Id'):BUFFER-VALUE)                     */
/*      NO-LOCK NO-ERROR.                                                                               */
/*  IF AVAIL KOrdreHode THEN                                                                            */
/*  DO:                                                                                                 */
/*    /* For JF */                                                                                      */
/*    IF KOrdreHode.Opphav = 10 AND cNettButikkType = "2" /* PRS nettbutikk */ THEN                     */
/*    DO:                                                                                               */
/*        IF CAN-FIND(FIRST kordrelinje WHERE                                                           */
/*                    kordrelinje.kordre_id = KOrdreHode.Kordre_id AND KOrdrelinje.plockstatus > 2) THEN*/
/*            lNekad = TRUE.                                                                            */
/*        IF CAN-FIND(FIRST kordrelinje WHERE                                                           */
/*                    kordrelinje.kordre_id = KOrdreHode.Kordre_id AND KOrdrelinje.plockstatus > 0 AND  */
/*                    KOrdrelinje.plockstatus < 3) THEN                                                 */
/*            lPs12 = TRUE.                                                                             */
/*        IF lNekad AND lPs12 THEN                                                                      */
/*        DO:                                                                                           */
/*            ASSIGN                                                                                    */
/*                obOk = FALSE                                                                          */
/*                ocReturn = "Behandling av order påbörjad. >> Manuell handtering krävs"                */
/*                .                                                                                     */
/*            LEAVE.                                                                                    */
/*        END.                                                                                          */
/*    END.                                                                                              */
/*  END.                                                                                                */
/*  IF obOk THEN                                                                                        */
/*    LEAVE.                                                                                            */
/*                                                                                                      */
/*  IF AVAIL KOrdreHode THEN                                                                            */
/*    RELEASE KOrdreHode.                                                                               */
/*  hQuery:GET-NEXT().                                                                                  */
/*END.                                                                                                  */

