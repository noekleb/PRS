TRIGGER PROCEDURE FOR CREATE OF KupongTransLogg.
DEFINE BUFFER bufKupongtrans FOR KupongTransLogg.
/* def var trgDec as DEC no-undo. */

{trg\c_w_trg.i &Fil=SkoTex.KupongTransLogg &Type="C"}

    FIND LAST bufKupongtrans USE-INDEX KupongTransLogg NO-LOCK NO-ERROR.
    IF NOT AVAIL bufKupongtrans THEN DO:
        KupongTransLogg.KTLoggId = 1.
    END.
    ELSE DO:
        KupongTransLogg.KTLoggId = bufKupongtrans.KTLoggId + 1.
    END.
/*                                                              */
/* LOOPEN:                                                      */
/* DO WHILE TRUE:                                               */
/*     RUN trg/genktloggid (OUTPUT trgdec).                     */
/* FIND LAST                                                    */
/*     IF CAN-FIND(KupongTransLogg WHERE                        */
/*                 KupongTransLogg.KTLoggId = int(trgDec)) THEN */
/*         NEXT loopen.                                         */
/*     ELSE DO:                                                 */
/*         assign                                               */
/*           KupongTransLogg.KTLoggId = trgDec                  */
/*           NO-ERROR.                                          */
/*         LEAVE LOOPEN.                                        */
/*     END.                                                     */
/* END.                                                         */



