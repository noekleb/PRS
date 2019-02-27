DEFINE VARIABLE icount AS INTEGER    NO-UNDO.
DEFINE VARIABLE icount2 AS INTEGER    NO-UNDO.
MESSAGE "denna skall inte köras"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
/* OUTPUT TO SESSION:TEMP-DIRECTORY + "fixbonglinjeMD.txt".                                  */
/* FOR EACH butiker NO-LOCK WHERE can-find(FIRST kasse WHERE kasse.butik = butiker.butik AND */
/*                  Kasse.ModellNr = 30):                                                    */
/*     DO TRANSACTION:                                                                       */
/*     For each bonglinje where butikk = butiker.butik AND TTId = 10 OR TTId = 1:            */
/*          If hovedgr = 1905 then do:                                                       */
/*              EXPORT bonglinje.                                                            */
/*               Assign Linjesum  = linjesum                                                 */
/*                      Bongpris   = ABS(antall) * bongpris                                  */
/*                      VVarekost = 0                                                        */
/*                      Ttid      = 62                                                       */
/*                      Antall    = 0.                                                       */
/*         End.                                                                              */
/*         Else if abs(antall) > 1 then do:                                                  */
/*             EXPORT bonglinje.                                                             */
/*             Assign Bongpris   = ABS(antall) * vvarekost                                   */
/*                    VVarekost  =   Bongpris.                                               */
/*         End.                                                                              */
/*     END.                                                                                  */
/*     END.                                                                                  */
/* END.                                                                                      */
/* OUTPUT CLOSE.                                                                             */
