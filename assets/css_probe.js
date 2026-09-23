<script>
Reveal.on('ready', function () {
  function cs(el, prop) { return el ? getComputedStyle(el)[prop] : 'NO ELEMENT'; }
  var out = [];
  function row(label, sel, prop) {
    var el = document.querySelector(sel);
    out.push((el ? '' : '[missing] ') + label + ' -> ' + prop + ': ' + cs(el, prop));
  }
  row('Red 要素',        'Red',                     'color');
  row('span.Red',        'span.Red',                'color');
  row('Orange 要素',     'Orange',                  'color');
  row('span.Orange',     'span.Orange',             'color');
  row('Blue 要素',       'Blue',                    'color');
  row('span.Blue',       'span.Blue',               'color');
  row('Gray 要素',       'Gray',                    'color');
  row('span.Gray',       'span.Gray',               'color');
  row('description-list','.description-list',       'fontSize');
  row('  dt',            '.description-list dt',    'display');
  row('  dd',            '.description-list dd',    'display');
  row('description-lb',  '.description-lb',         'fontSize');
  row('  dt',            '.description-lb dt',      'display');
  row('  dd margin-bot', '.description-lb dd',      'marginBottom');
  row('ListArrow row',   '.ListArrow .arrow-row',   'display');
  row('  arrow-left',    '.ListArrow .arrow-left',  'flexBasis');
  row('  arrow',         '.ListArrow .arrow',       'textAlign');
  row('  arrow-right',   '.ListArrow .arrow-right', 'flexBasis');
  row('myindent',        '.myindent',               'marginLeft');
  var pre = document.createElement('pre');
  pre.style.cssText = 'position:fixed;top:0;left:0;z-index:99999;background:#fff;font-size:15px;padding:8px;line-height:1.35';
  pre.textContent = out.join('\n');
  document.body.appendChild(pre);
});
</script>
