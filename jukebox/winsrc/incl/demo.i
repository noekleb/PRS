DEF VAR bJukeBoxDemo AS LOG NO-UNDO.

/* IF CONNECTED("sports2000") THEN DO:                                                */
/*   FIND FIRST _file WHERE _file-name = "SupplierItemXref" NO-LOCK NO-ERROR.         */
/*   IF NOT AVAIL _file THEN DO:                                                      */
/*     MESSAGE "Demo version of JukeBox can only be used on the Sports2000 database"  */
/*             VIEW-AS ALERT-BOX ERROR.                                               */
/*     QUIT.                                                                          */
/*   END.                                                                             */
/*   ELSE bJukeBoxDemo = TRUE.                                                        */
/* END.                                                                               */
/* ELSE DO:                                                                           */
/*   MESSAGE "Demo version of JukeBox can only be used on the Sports2000 database!"   */
/*           VIEW-AS ALERT-BOX ERROR.                                                 */
/*   QUIT.                                                                            */
/* END.                                                                               */
  
