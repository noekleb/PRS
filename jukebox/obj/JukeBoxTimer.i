  /*===============================================================
    Timer Object
    ===============================================================*/

&IF DEFINED(WND_PROCEDURES-PLEASE) = 0 &THEN
&SCOPED-DEFINE WND_PROCEDURES-PLEASE YES
&ENDIF

&IF DEFINED(WND_PROCEDURES-PLEASE) <> 0 AND {&WND_PROCEDURES-PLEASE} = YES &THEN

  PROCEDURE TimerCreate EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER TMR_hFrameHWND              AS LONG.
      DEFINE INPUT  PARAMETER TMR_iIntervalInMilliSeconds AS LONG.
      DEFINE RETURN PARAMETER TMR_hTimer                  AS LONG.
  END PROCEDURE.
  
  PROCEDURE TimerSetTimeInterval EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER TMR_hTimer                  AS LONG.
      DEFINE INPUT  PARAMETER TMR_iIntervalInMilliSeconds AS LONG.
      DEFINE RETURN PARAMETER TMR_iSuccess                AS LONG.
  END PROCEDURE.

  PROCEDURE TimerEnable EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER TMR_hTimer                  AS LONG.
      DEFINE RETURN PARAMETER TMR_iSuccess                AS LONG.
  END PROCEDURE.

  PROCEDURE TimerDisable EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER TMR_hTimer                  AS LONG.
      DEFINE RETURN PARAMETER TMR_iSuccess                AS LONG.
  END PROCEDURE.

&ENDIF
