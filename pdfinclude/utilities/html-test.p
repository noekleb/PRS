<table class="voorbeeld"><tr><td><pre>
 
<span class="src-comment">/******************************************************************************
  Test Comment 
******************************************************************************/</span src-comment>
 
<span class="src-prep">&GLOBAL-DEFINE</span src-prep> PDFDIR 
 
<span class="src-prep">&IF</span src-prep> <span class="src-keyword">OPSYS</span src-keyword> = <span class="src-char">"UNIX"</span src-char> <span class="src-prep">&THEN</span src-prep>
  <span class="src-prep">&GLOBAL-DEFINE</span src-prep> zlib          /lib/libz.so.<span class="src-number">1</span src-number>
<span class="src-prep">&ENDIF</span src-prep>
 
<span class="src-prep">{ inc.i }</span src-prep<span class="src-keyword">></span src-keyword>
 
<span class="src-keyword">FOR EACH</span src-keyword> Customer <span class="src-keyword">NO-LOCK</span src-keyword>:
<span class="src-keyword">END</span src-keyword>.
 
<span class="src-comment">/* end of pdf_pre.i */</span src-comment>
 
</pre></td></tr></table>
 
