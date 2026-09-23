<script>
Reveal.on('ready', function () {
  Reveal.slide(1,0,0);
  var el=document.querySelector('.myindent'), out=[];
  for(var i=0;i<document.styleSheets.length;i++){
    var rs; try{rs=document.styleSheets[i].cssRules}catch(e){continue}
    for(var j=0;j<rs.length;j++){
      var r=rs[j];
      if(!r.selectorText) continue;
      var ml=r.style && (r.style.marginLeft||r.style.margin);
      if(!ml) continue;
      var matches=false;
      try{matches=el.matches(r.selectorText)}catch(e){}
      if(matches) out.push(r.selectorText+' => margin-left:'+(r.style.marginLeft||'-')+' margin:'+(r.style.margin||'-'));
    }
  }
  var pre=document.createElement('pre');
  pre.style.cssText='position:fixed;top:0;left:0;z-index:99999;background:#fff;font-size:15px;padding:8px';
  pre.textContent='rules matching .myindent that set a left margin:\n'+out.join('\n');
  document.body.appendChild(pre);
});
</script>
