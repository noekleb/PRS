DEF VAR hQuery AS HANDLE.
DEF VAR ix AS INT.
/* DEF VAR cBuffers AS CHAR INIT "VarGr". */
DEF VAR cBuffers AS CHAR INIT "ttprescanjoin,Stilling,stillingstype,stillingskategori,orgenhet,orgenhettype,subjekt".
/* DEF VAR cBuffers AS CHAR INIT "ttprescanjoin,Stilling". */
DEF VAR hBuffer  AS HANDLE.
DEF VAR buf2_orgenhet AS HANDLE.
DEF VAR buf3_orgenhet AS HANDLE.


DEF TEMP-TABLE ttPrescanjoin
    FIELD rPrescanjoin AS ROWID.

CREATE BUFFER buf2_OrgEnhet FOR TABLE "orgenhet" BUFFER-NAME "buf2_orgenhet".
CREATE BUFFER buf3_OrgEnhet FOR TABLE "orgenhet" BUFFER-NAME "buf3_orgenhet".

FOR EACH Subjekt WHERE cPostNr = "9600" AND cPostSted = "HAMMERFEST" NO-LOCK
   ,EACH Stilling  OF Subjekt WHERE CAN-DO('naavaerende,permisjon',cStatus)  NO-LOCK
  : 
/*    DISP cetternavn. */
   CREATE ttPrescanjoin.
   rprescan = ROWID(stilling).
   ix = ix + 1.
END.

ETIME(TRUE).

CREATE QUERY hQuery.
DO ix = 1 TO NUM-ENTRIES(cBuffers):
  CREATE BUFFER hBuffer FOR TABLE ENTRY(ix,cBuffers).
  hQuery:ADD-BUFFER (hBuffer).
END.
  hQuery:ADD-BUFFER (buf2_orgenhet).
  hQuery:ADD-BUFFER (buf3_orgenhet).

hQuery:QUERY-PREPARE("FOR EACH ttPreScanJoin,EACH Stilling WHERE ROWID(Stilling) = ttPreScanJoin.rPreScanJoin AND CAN-DO('naavaerende,permisjon',cStatus) AND ( cPostSted BEGINS 'hamm') NO-LOCK,FIRST StillingsType OF Stilling    NO-LOCK OUTER-JOIN,FIRST StillingsKategori OF StillingsType    NO-LOCK OUTER-JOIN,FIRST OrgEnhet WHERE OrgEnhet.iOrgEnhetId = Stilling.iArbeidsgiver    NO-LOCK OUTER-JOIN,FIRST OrgEnhetType OF OrgEnhet    NO-LOCK OUTER-JOIN,FIRST Subjekt OF Stilling  WHERE NOT CAN-DO('test1,test2,gh,bha',Subjekt.PFuserid)  AND ( Subjekt.cPostNr = '9600' AND cPostSted = 'HAMMERFEST') NO-LOCK,FIRST buf2_OrgEnhet WHERE buf2_OrgEnhet.iOrgEnhetId = Subjekt.iBostedsBispedID AND buf2_OrgEnhet.iOrgEnhetId NE 0    NO-LOCK OUTER-JOIN,FIRST buf3_OrgEnhet WHERE buf3_OrgEnhet.iOrgEnhetId = Subjekt.iBostedsProstiID AND buf3_OrgEnhet.iOrgEnhetId NE 0    NO-LOCK OUTER-JOIN").
/* hQuery:QUERY-PREPARE("FOR EACH ttPreScanJoin,EACH Stilling WHERE ROWID(Stilling) = ttPreScanJoin.rPreScanJoin AND CAN-DO('naavaerende,permisjon',cStatus) NO-LOCK,FIRST StillingsType OF Stilling    NO-LOCK OUTER-JOIN,FIRST StillingsKategori OF StillingsType    NO-LOCK OUTER-JOIN,FIRST OrgEnhet WHERE OrgEnhet.iOrgEnhetId = Stilling.iArbeidsgiver    NO-LOCK OUTER-JOIN,FIRST OrgEnhetType OF OrgEnhet    NO-LOCK OUTER-JOIN,FIRST Subjekt OF Stilling  WHERE NOT CAN-DO('test1,test2,gh,bha',Subjekt.PFuserid)  AND ( subjekt.cPostNr = '9600' AND subjekt.cPostSted = 'HAMMERFEST') NO-LOCK"). */

/* hQuery:QUERY-PREPARE("FOR EACH VarGr " + "WHERE RECID(VarGr) = 322862"). */
hQuery:QUERY-OPEN().
hQuery:GET-FIRST().
ix = 0.
REPEAT WHILE NOT hQuery:QUERY-OFF-END:
  ix = ix + 1.
  hQuery:GET-NEXT().
END.
MESSAGE PROGRAM-NAME(1) SKIP
        ix  SKIP
        ETIME / 1000
        VIEW-AS ALERT-BOX.

/* MESSAGE PROGRAM-NAME(1) SKIP                                             */
/*         hBuffer:BUFFER-FIELD("vg"):BUFFER-VALUE                          */
/*         VIEW-AS ALERT-BOX.                                               */

/* FOR EACH VarGr WHERE RECID(VarGr) = 322862: */
/*   MESSAGE PROGRAM-NAME(1) SKIP              */
/*           vg                                */
/*           VIEW-AS ALERT-BOX.                */
/* END.                                        */


/* hBuffer:FIND-FIRST("WHERE RECID(VarGr) = 322862",NO-LOCK) */
