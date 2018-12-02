

DEFINE VARIABLE Persondetaljer_Fornamn AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Efternamn AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Fodelsetid AS CHAR NO-UNDO. 
DEFINE VARIABLE Persondetaljer_Kon AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_Utdelningsadress2 AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_PostNr AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_Postort AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordLanKod AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordKommunKod AS CHAR NO-UNDO. 
DEFINE VARIABLE Folkbokforingsadress_FolkbokfordForsamlingKod AS CHAR NO-UNDO. 
DEFINE VARIABLE cErrorMessage AS CHAR NO-UNDO. 
DEFINE VARIABLE lPersonFunnet AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO. 

ETIME(TRUE).

RUN SPAR_Personsokninfraga_tst.p ("193701308888", /* personnr */
/*RUN SPAR_Personsokninfraga.p ("195402126238", /* personnr Kenneth*/ */
        OUTPUT Persondetaljer_Fornamn,
        OUTPUT Persondetaljer_Efternamn,
        OUTPUT Persondetaljer_Fodelsetid,
        OUTPUT Persondetaljer_Kon,
        OUTPUT Folkbokforingsadress_Utdelningsadress2,
        OUTPUT Folkbokforingsadress_PostNr,
        OUTPUT Folkbokforingsadress_Postort,
        OUTPUT Folkbokforingsadress_FolkbokfordLanKod,
        OUTPUT Folkbokforingsadress_FolkbokfordKommunKod,
        OUTPUT Folkbokforingsadress_FolkbokfordForsamlingKod,
        OUTPUT cErrorMessage,
        OUTPUT lPersonFunnet,
        OUTPUT lSuccess). 

MESSAGE "Tid  ms.                " ETIME / 1000 skip                 
        "lSuccess                " lSuccess                 skip
        "lPersonFunnet           " lPersonFunnet            SKIP  
        "cErrormessage           " cErrormessage            skip
        "Persondetaljer_Fornamn  " Persondetaljer_Fornamn   SKIP
        "Persondetaljer_Efternamn" Persondetaljer_Efternamn skip
        VIEW-AS ALERT-BOX.
/* OUTPUT TO "CLIPBOARD".               */
/* PUT UNFORMATTED cErrormessage SKIP.  */
/* OUTPUT CLOSE.                        */
