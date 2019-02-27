&IF DEFINED(UIB_is_Running) NE 0 &THEN
  DYNAMIC-FUNCTION("setAppserviceId","skotex").
  DYNAMIC-FUNCTION("setSessionId","validsession").
  DYNAMIC-FUNCTION("setASUserId","tomn","Tom Nøkleby").
  DYNAMIC-FUNCTION("setAppTitle","PRS").
  DYNAMIC-FUNCTION("setCompanyId","1").
  DYNAMIC-FUNCTION("setBehaviour",
                    "DefaultSortFont|6," +   
                    "DefaultSortColor|15," + 
                    "BrowseSearchDefault|goto," +
                    "TabOnReturn|yes," +       
                    "SetSortLabel|yes," +
                    "DefaultImageList|" +
                       "bmp/accep16e.bmp;icon/add.bmp;icon/e-detail.bmp" 
                     + ";bmp/e-exit.bmp;bmp/e-help.bmp;icon/copyrec.bmp" 
                     + ";icon/reset.bmp;icon/deleterec.bmp;gif/saverec.gif" 
                     + ";gif/afexcel.gif;gif/afword.gif;bmp/print16e.bmp" 
                     + ";gif/filter.gif;gif/afinternet.gif;gif/msngrWindow.gif" 
                     + ";icon/next.bmp;icon/prev.bmp;icon/first.bmp;icon/last.bmp"
                     + ";bmp/commit.bmp;bmp/rollback.bmp;gif/active.gif;gif/sdogen16.gif"
                    ).      

  DYNAMIC-FUNCTION("setEnableColor",FALSE).

  DYNAMIC-FUNCTION("setASlibBehaviour",
                    "QueryLogFile|serverquery.log," +   
                    "TransLogFile|servertrans.log"
                    ).      
&ENDIF
