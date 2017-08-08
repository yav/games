function drawGame(g) {
  console.log(g)
  var dom = $('<div/>')

  var tiles = g.board.tiles
  var boardDom = $('<table/>')
  jQuery.each(tiles, function(ixY,row) {
    var tr = $('<tr/>')
    var y = tiles.length - ixY - 1
    jQuery.each(row, function(x, t) {
      tr.append(drawTile(x,y,t))
    })
    boardDom.append(tr)
  })

  dom.append(boardDom)

  return dom
}

function drawTile(x,y,lab) {

  var dom = $('<td/>').css('text-align','center')
  if (lab === null) dom

  var sz = 200

  var div = $('<div/>')
            .css('width', sz + 'px')
            .css('height', sz + 'px')
            .css('background-color','#090')
            .css('position','relative')
            .css('overflow', 'hidden')

  var txt = $('<div/>').text(lab)
            .css('position','absolute')
            .css('top',  1.2 * (sz/6) + 'px')
            .css('left', 1.2 * (sz/6) + 'px')
            .css('font-weight','bold')
  div.append(txt)

  for (var r = 0; r < 3; ++r) {
    for (var c = 0; c < 3; ++c) {
      div.append(drawBasicPawnLoc(sz,x,y,r,c))
    }
  }

  return dom.append(div)
}

function drawBasicPawnLoc(sz,x,y,r,c) {
  var dom = $('<dom/>')

  var half = sz / 6

  var xx = 2*x + c
  var yy = 2*y + r

  var nm = xx + '_' + yy

  if (r < 2 && c < 2) { dom.attr('id',nm) }

  dom.addClass(nm)
     .css('background-color', 'rgba(255,255,255,0.5)')
     .css('width', (2 * half) + 'px')
     .css('height', (2 * half) + 'px')
     .css('border-radius',half + 'px')
     .css('position','absolute')
     .css('left', ((3*c - 1) * half) + 'px')
     .css('bottom', ((3*r - 1) * half) + 'px')
     .click(function() { console.log(xx,yy) })

  return dom
}


