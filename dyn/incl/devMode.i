&IF DEFINED(UIB_is_Running) NE 0 &THEN
/*   {incl/ttDataDict.i NEW} */

  RUN jboxloadlib.p ("resizelib.p,jboxuilib.p,jboxaslib.p,controls.p").
  
/*   DEF VAR hResizeLib  AS HANDLE NO-UNDO.  */
/*   RUN ResizeLib.p PERSIST SET hResizeLib. */
/*   SESSION:ADD-SUPER-PROC(hResizeLib).     */
/*                                           */
/*   DEF VAR hJBoxASlib AS HANDLE NO-UNDO.   */
/*   RUN JBoxASlib.p PERSIST SET hJBoxASlib. */
/*   SESSION:ADD-SUPER-PROC(hJBoxASlib).     */
/*                                           */
/*   DEF VAR hJBoxUIlib AS HANDLE NO-UNDO.   */
/*   RUN JBoxUIlib.p PERSIST SET hJBoxUIlib. */
/*   SESSION:ADD-SUPER-PROC(hJBoxUIlib).     */


/*   DEF VAR hChemistryFU_lib AS HANDLE NO-UNDO.          */
/*   RUN ChemistryFU_lib.p PERSIST SET hChemistryFU_lib.  */
/*   SESSION:ADD-SUPER-PROC(hChemistryFU_lib).            */
&ENDIF
