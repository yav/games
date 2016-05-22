function drawMaterial(x) {
  var colors = [ '#f00', '#0f0', '#00f', '#ff0', '#f0f', '#0ff' ]

  var m = $('<div/>')
          .css('width',            '15px')
          .css('height',           '16px')
          .css('margin',           '2px')
          .css('line-height',      '0px')
          .css('display',          'inline-block')
          .css('vertical-align',   'middle')
          .css('border',           '2px solid black')
          .attr('title',            x)

  switch(x) {
    case 'A': m.css('background-color',colors[0]); break
    case 'B': m.css('background-color',colors[1]); break
    case 'C': m.css('background-color',colors[2]); break
    case 'D': m.css('background-color',colors[3]); break
    case 'E': m.css('background-color',colors[4]); break
    case 'F': m.css('background-color',colors[5]); break
    case 'X': m.css('background-color','#ccc');    break
    case '?':
      m.css('background-color', 'rgba(0,0,0,0.2)')
       .css('position',         'relative')
      var smallW = 15/3
      var smallH = 16/2
      var i, j, col = 0
      for (i = 0; i < 3; i++)
        for (j = 0; j < 2; j++)
          m.append($('<div/>')
                   .css('position', 'absolute')
                   .css('width',    smallW + 'px')
                   .css('height',   smallH + 'px')
                   .css('line-height', '0px')
                   .css('left', i * smallW + 'px')
                   .css('top',  j * smallH + 'px')
                   .css('background-color', colors[col++])
                   )
      break
  }

  return m
}


