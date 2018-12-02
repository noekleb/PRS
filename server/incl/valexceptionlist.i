
/* Don't bother check these columns for FK existence */
DEF VAR cValExceptionList AS CHAR NO-UNDO 
    INIT "dCreated,cCreatedBy,dModified,cModifiedBy,cDescription,cName". 
