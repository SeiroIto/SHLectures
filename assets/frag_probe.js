<script>
Reveal.on('ready', function () {
  Reveal.slide(1, 0, 0);              // go to the content slide, step 0
  var slide = Reveal.getCurrentSlide();
  var frags = slide.querySelectorAll('.fragment');
  var out = ['slide: ' + (slide.querySelector('h2') ? slide.querySelector('h2').textContent : '(none)'),
             'fragments: ' + frags.length];
  for (var step = 0; step <= frags.length; step++) {
    var vis = [];
    frags.forEach(function (f) {
      if (f.classList.contains('visible')) vis.push(f.textContent.trim().slice(0, 16));
    });
    out.push('step ' + step + ': ' + (vis.length ? vis.join(' / ') : '(none)'));
    Reveal.next();
  }
  var pre = document.createElement('pre');
  pre.style.cssText = 'position:fixed;top:0;left:0;z-index:99999;background:#fff;font-size:16px;padding:8px';
  pre.textContent = out.join('\n');
  document.body.appendChild(pre);
});
</script>
