
DEFINE VARIABLE hServer AS HANDLE NO-UNDO. 
DEFINE VARIABLE cLayout AS CHAR NO-UNDO. 
DEFINE VARIABLE cBrukerId AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPingHost AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iSvar AS INTEGER NO-UNDO.
DEFINE VARIABLE cConnectionString AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lConnected AS LOGICAL     NO-UNDO.
cConnectionString = "-H 192.168.100.68 -AppService asbroker1".

FIND FakturaHode NO-LOCK WHERE 
    FakturaHode.FakturaNr = 2900001.
FIND Butiker NO-LOCK WHERE 
    Butiker.butik = FakturaHode.butikkNr.

    RUN skrivfaktura.p (STRING(FakturaHode.Faktura_Id) + "|",TRUE,Butiker.RapPrinter,1,"",1).

/*     DO:                                                                                                        */
/*         cPingHost = ENTRY(2,cConnectionString," ").                                                            */
/*         DO:                                                                                                    */
/*             CREATE SERVER hServer.                                                                             */
/*             lConnected = hServer:CONNECT(cConnectionString) .                                                  */
/*             IF lConnected THEN                                                                                 */
/*             DO:                                                                                                */
/*                 RUN skrivfaktura.p ON hServer (STRING(FakturaHode.Faktura_Id),TRUE,Butiker.RapPrinter,1,"",1). */
/*                                                                                                                */
/*                 hServer:DISCONNECT().                                                                          */
/*             END.                                                                                               */
/*             DELETE OBJECT hServer.                                                                             */
/*         END.                                                                                                   */
/*     END.                                                                                                       */
/*                                                                                                                */

