 
/*------------------------------------------------------------------------
   File        : JBoxConvert
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : goo_000
   Created     : Tue Mar 29 11:24:53 CEST 2016
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

&IF "{&OPSYS}" = "WIN32" &THEN USING System.TimeZone FROM ASSEMBLY. &ENDIF

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS JBoxConvert: 
  DEFINE PUBLIC STATIC PROPERTY Instance AS CLASS JBoxConvert NO-UNDO
    GET():
      IF Instance = ? THEN Instance = NEW JBoxConvert().
      RETURN Instance.
    END GET.
    PRIVATE SET.

  /*------------------------------------------------------------------------------
   Purpose:
   Notes:
  ------------------------------------------------------------------------------*/

  METHOD PUBLIC datetime-tz convertDateTime2localTimeZone(INPUT idtDateTime AS DATETIME):
	  def var cOffset as char no-undo.
	  &IF "{&OPSYS}" = "WIN32" &THEN cOffSet = TimeZone:CurrentTimeZone:GetUtcOffset(System.DateTime:Now):ToString(). &ENDIF
    return datetime-tz(string(idtDateTime) + '+' + substring(cOffSet,1,5) ).
  END METHOD.

  METHOD PUBLIC INTEGER convertTime(INPUT icTime AS CHARACTER):
    CASE NUM-ENTRIES(icTime,':'):
      WHEN 1 THEN /*HH*/
        DO:
          RETURN int(ENTRY(1,icTime,':')) * 3600.  
        END.
      WHEN 2 THEN /*HH:MM:SS*/
        DO:
          RETURN int(ENTRY(1,icTime,':')) * 3600 + int(ENTRY(2,icTime,':')) * 60.  
        END.
      WHEN 3 THEN /*HH:MM:SS*/
        DO:
          RETURN int(ENTRY(1,icTime,':')) * 3600 + int(ENTRY(2,icTime,':')) * 60 + int(ENTRY(3,icTime,':')).  
        END.
      OTHERWISE /**/
      DO:
        MESSAGE 'Something wrong:' icTime
          VIEW-AS ALERT-BOX.  
      END.
    END CASE.

  END METHOD.

  METHOD PUBLIC CHARACTER convertTime(INPUT iiTime AS INTEGER, input cFormat as char):
    RETURN STRING(iiTime,cFormat).
  END METHOD.

  METHOD PUBLIC CHARACTER getTimePart(INPUT idtDateTime AS DATETIME-TZ):
    DEFINE VARIABLE cDateTime     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTimePart     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTimeZonePart AS CHARACTER NO-UNDO.
    
    cDateTime = STRING(idtDateTime).
    cTimePart = ENTRY(2,cDateTime,' ').

    IF INDEX(cTimePart,'+')      GT 0 THEN ASSIGN 
        cTimeZonePart = '+ ' + entry(2,cTimePart,'+')
        cTimePart     = ENTRY(1,cTimePart,'+').
    ELSE IF INDEX(cTimePart,'-') GT 0 THEN ASSIGN 
          cTimeZonePart = '- ' + entry(2,cTimePart,'-')
          cTimePart     = ENTRY(1,cTimePart,'-').
    
    RETURN SUBSTRING(cTimePart,1,INDEX(cTimePart,SESSION:NUMERIC-DECIMAL-POINT) - 1).

  END METHOD.
  METHOD PUBLIC CHARACTER getTimeZonePart(INPUT idtDateTime AS DATETIME-TZ):
    DEFINE VARIABLE cDateTime     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTimePart     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTimeZonePart AS CHARACTER NO-UNDO.
    
    cDateTime = STRING(idtDateTime).
    cTimePart = ENTRY(2,cDateTime,' ').
    
    IF INDEX(cTimePart,'+')      GT 0 THEN ASSIGN 
        cTimeZonePart = '+ ' + entry(2,cTimePart,'+')
        cTimePart     = ENTRY(1,cTimePart,'+').
    ELSE IF INDEX(cTimePart,'-') GT 0 THEN ASSIGN 
          cTimeZonePart = '- ' + entry(2,cTimePart,'-')
          cTimePart     = ENTRY(1,cTimePart,'-').
    
    RETURN cTimeZonePart.
    

  END METHOD.
  METHOD PUBLIC INTEGER getMilisecondPart(INPUT idtDateTime AS DATETIME-TZ):
    DEFINE VARIABLE cDateTime     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTimePart     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTimeZonePart AS CHARACTER NO-UNDO.
    
    cDateTime = STRING(idtDateTime).
    cTimePart = ENTRY(2,cDateTime,' ').
    
    IF INDEX(cTimePart,'+')      GT 0 THEN ASSIGN 
        cTimePart = ENTRY(1,cTimePart,'+').
    ELSE IF INDEX(cTimePart,'-') GT 0 THEN ASSIGN 
          cTimePart = ENTRY(1,cTimePart,'-').
    
    RETURN int(ENTRY(2,ENTRY(3,cTimePart,':'),SESSION:NUMERIC-DECIMAL-POINT)).
    
  END METHOD.

END CLASS.
