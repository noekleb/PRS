
/* Special text translation (sample from InfoPos SE, win\w-gridord.w) */

PROCEDURE InitButtons:            
  
  /* ... */

  /* If no translation is found the text itself ("&Godkjenn") will be returned.
     For Progress widgets with special translation you should use the widget name as type to automatically remove the 
     widget itself from the translation list. 
  */                                                                                                            
 
  CASE BestHode.BestStat:
    WHEN 1 THEN 
      BUTTON-Godkann:LABEL = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-Godkann","&Godkjenn").
    WHEN 2 THEN 
      BUTTON-Godkann:LABEL = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-Godkann","Til &ordre...").
    WHEN 3 THEN 
      BUTTON-Godkann:LABEL = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"BUTTON-Godkann","Fra &ordre").
  END CASE.
  
  /* ... */

END PROCEDURE.

PROCEDURE InitStringTranslation:


  DEF INPUT PARAM        ihWindow    AS HANDLE NO-UNDO.
  DEF INPUT-OUTPUT PARAM iocTypeList AS CHAR   NO-UNDO.

  IF ihWindow NE THIS-PROCEDURE:CURRENT-WINDOW THEN RETURN.

  /* For translation: Init special translation type and corresponding values: */
  IF NOT CAN-DO(iocTypeList,"BUTTON-Godkann") THEN
    iocTypeList = iocTypeList + ",BUTTON-Godkann".
  IF NOT CAN-DO(iocTypeList,"WINDOWTITLE") THEN
    iocTypeList = iocTypeList + ",WINDOWTITLE".
  IF NOT CAN-DO(iocTypeList,"GRID") THEN
    iocTypeList = iocTypeList + ",GRID".

  /* The following initialization is only needed for the button since the InitButtons procedure only requests individual texts */
  DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"btnGodkjenn","&Godkjenn|Til &ordre...|Fra &ordre").

  ASSIGN ch_Grid:TextMatrix(0,ch_Grid:FixedCols - 2) = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"GRID","Kasser ")
         ch_Grid:TextMatrix(2,0) = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"GRID","Fri ")
         ch_Grid:TextMatrix(1,0) = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"GRID","Kasser ")
         ch_Grid:TextMatrix(3,0) = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"GRID","Bestilt")
         .

  RUN InitButtons.
  RUN SetWindowTitle.

END PROCEDURE

PROCEDURE SetWindowTitle:

  DEF VAR cTranslation AS CHAR NO-UNDO.
  
  /* When the translation program is running this call also initiates the values for translation */
  cTranslation = DYNAMIC-FUNCTION("getStringTranslation",THIS-PROCEDURE:FILE-NAME,"WINDOWTITLE","Bestilling artikkel: | Best.nr: |NY| Status: | Ordre: | (Sendt ").

  ASSIGN CURRENT-WINDOW:HIDDEN = NO
         CURRENT-WINDOW:TITLE = ENTRY(1,cTranslation,"|") + 
           (IF AVAILABLE ArtBas
              THEN STRING(ArtBas.ArtikkelNr)
              ELSE "***")  + " " + ENTRY(2,cTranslation,"|") +
           IF NOT AVAIL BestHode THEN  ENTRY(3,cTranslation,"|") ELSE
           STRING(BestHode.BestNr) + " " + ENTRY(4,cTranslation,"|") + ENTRY(BestHode.BestStat,wStatus) + 
           (IF AVAIL BestHode AND BestHode.OrdreNr <> 0 THEN ENTRY(5,cTranslation,"|") + 
           STRING(BestHode.OrdreNr) ELSE "") +
           (IF BestHode.SendtDato <> ? THEN ENTRY(6,cTranslation,"|") + STRING(BestHode.SendtDato) + " " +
                       STRING(BestHode.SendtTid,"HH:MM") + " " +
                       BestHode.SendtAv + ")" ELSE "").

END PROCEDURE.

/* main (before incl/wintrigg.i (which is part of lng.i in w-gridord.w)): */

  SUBSCRIBE TO "InitStringTranslation" ANYWHERE.
