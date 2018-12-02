DEF VAR iant AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.
FOR EACH StrType EXCLUSIVE-LOCK WHERE 
    StrType.StrTypeId > 2 AND 
    NOT CAN-FIND(FIRST ArtBas WHERE 
                 ArtBas.StrTypeId = StrType.StrTypeId)
    BREAK BY StrType.Intervall DESCENDING
          BY StrType.fordeling:
    iAnt = iant + 1.

/*    DISPLAY                   */
/*        StrType.StrTypeId     */
/*        Strtype.Beskrivelse   */
/*        StrType.KortNavn      */
/*        StrType.Intervall     */
/*        StrType.fordeling     */
/*        StrType.RegistrertDato*/
/*        StrType.EDato         */
/*    WITH WIDTH 350.           */
    
    FOR EACH StrTStr OF StrType:
        DELETE StrTStr.
    END.
    DELETE StrType.

    
END.
MESSAGE iAnt
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
