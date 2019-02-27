  DEF VAR cRowIdList  AS CHAR NO-UNDO.

  cRowIdList = JBoxServerAPI:Instance:getRowIdList("SalesRep","WHERE RepName BEGINS 'B'").
/*  cRowIdList = JBoxServerAPI:Instance:getRowIdList("<TableList>","<table for rowid>","<query>"). */
  
  JBoxServerAPI:Instance:SelectorDialog("SalesRep" 
                      + ";SalesRep"  
                      + ";RepName"
                      + ";MonthQuota[1]"
                      + ";MonthQuota[2]"
                      ,"where true",
                      cRowIdList).
  IF JBoxServerAPI:Instance:SelectorOk THEN
    MESSAGE JBoxServerAPI:Instance:SelectorRowidList skip(1)
            JBoxServerAPI:Instance:SelectorDeselectRowidList
    VIEW-AS ALERT-BOX.  
