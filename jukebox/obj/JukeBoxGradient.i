  
&IF DEFINED(WND_PROCEDURES-PLEASE) = 0 &THEN
&SCOPED-DEFINE WND_PROCEDURES-PLEASE YES
&ENDIF

&IF DEFINED(WND_PROCEDURES-PLEASE) <> 0 AND {&WND_PROCEDURES-PLEASE} = YES &THEN 
  PROCEDURE GradientFrameCreate EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT  PARAMETER GF_hParent             AS LONG.
      DEFINE INPUT  PARAMETER GF_sImage              AS CHARACTER.
      DEFINE INPUT  PARAMETER GF_iMode               AS LONG.
      DEFINE INPUT  PARAMETER GF_iBeginColor         AS LONG.
      DEFINE INPUT  PARAMETER GF_iEndColor           AS LONG.
      DEFINE INPUT  PARAMETER GF_iGradientMode       AS LONG.
      DEFINE RETURN PARAMETER GF_hGradientFrame      AS LONG.
  END PROCEDURE.
  
  PROCEDURE GradientFrameDestroy EXTERNAL "CONTROLS.DLL":U:
      DEFINE INPUT PARAMETER GF_hParent              AS LONG.
      DEFINE RETURN PARAMETER GF_bStatus             AS LONG.
  END PROCEDURE.

&ENDIF
/*================================================================
  Gradient Frame Mode. 0 = Gradient Mode. 1 = Picture Mode.
==================================================================*/
&GLOBAL-DEFINE GRAD_FRAME-GRADIENT    0
&GLOBAL-DEFINE GRAD_FRAME-PICTURE     1

/*================================================================
  GradientMode types of the Gradient Frame control.
==================================================================*/
&GLOBAL-DEFINE GRAD_FRAME_GRADIENT-HORIZONTAL       0
&GLOBAL-DEFINE GRAD_FRAME_GRADIENT-VERTICAL         1
&GLOBAL-DEFINE GRAD_FRAME_GRADIENT-FORWARDDIAGONAL  2
&GLOBAL-DEFINE GRAD_FRAME_GRADIENT-BACKWARDDIAGONAL 3
