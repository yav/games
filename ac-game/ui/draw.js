function drawPawnLoc(tiles,x,y) {
  var sz = 64
  var sm = sz / 2
  var smaller = 4
  var sm1 = sm - smaller

  var evx = x % 2 === 0
  var evy = y % 2 === 0

  function color(a,b) {
    return (a >= 0 && b >= 0 && tiles[a] && tiles[a][b]) ?
                      '#090' : 'rgba(0,0,0,0)'
  }


  var qw = evx ? sm1 : sm
  var qh = evy ? sm1 : sm

  var tx = Math.floor(x / 2)
  var ty = Math.floor(y / 2)

  var bl
  var br
  var tl
  var tr

  if (evx) {
    if (evy) {
      bl = color(tx - 1, ty - 1)
      br = color(tx, ty - 1)
      tl = color(tx - 1, ty)
      tr = color(tx, ty)
    } else {
      bl = tl = color(tx - 1, ty)
      br = tr = color(tx,ty)
    }
  } else {
    if (evy) {
      bl = br = color(tx, ty - 1)
      tl = tr = color(tx, ty)
    } else {
      bl = br = tl = tr = color(tx,ty)
    }

  }


  function quad(c,hor,ver) { return $('<div/>')
                           .css('position','absolute')
                           .css('width', qw + 'px')
                           .css('height', qh + 'px')
                           .css('background-color', c)
                           .css(hor,0)
                           .css(ver,0)

  }



  return $('<td/>')
          .append($('<div/>')
                  .attr('id', x + '_' + y)
                  .css('position','relative')
                  .css('width', sz + 'px')
                  .css('height', sz + 'px')
                  .append( quad(bl,'left','bottom')
                         , quad(br,'right','bottom')
                         , quad(tl,'left','top')
                         , quad(tr,'right','top')
                         ))
          .click(function() { console.log(x,y) })
}



function drawBoard(tiles) {

  tiles[1][1] = null
  var w = 3
  var h = 3

  var ph = 2 * h
  var pw = 2 * w

  var table = $('<table/>')
              .css('border-spacing', '0px')
              .css('border-collapse', 'true')
  for (var y = ph; y >= 0; --y) {
    var tr = $('<tr>')
    for (var x = 0; x <= pw; ++x) {
      tr.append(drawPawnLoc(tiles,x,y))
    }
    table.append(tr)
  }

  return table
}

function drawPawn(x,y) {
  var dom = $('<div/>')
            .css('width','46px')
            .css('height','46px')
            .css('border-radius','24px')
            .css('border', '2px solid black')
            .css('background-color','#fc6')
            .css('position','absolute')
            .css('left','8px')
            .css('top','8px')
            .css('z-index','10')
  $('#' + x + '_' + y).append(dom)
}


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
  if (lab === null || x == 1 && y == 1) return dom


  var sz = 200

  var div = $('<div/>')
            .css('width', sz + 'px')
            .css('height', sz + 'px')
            .css('background-color','#090')
            .css('position','relative')
            .css('overflow', 'hidden')
            .css('border', '3px solid black')

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
     .css('background-color', 'rgba(255,255,255,0.25)')
     .css('width', (2 * half) + 'px')
     .css('height', (2 * half) + 'px')
    // .css('border-radius',half + 'px')
     .css('position','absolute')
     .css('left', ((3*c - 1) * half) + 'px')
     .css('bottom', ((3*r - 1) * half) + 'px')
     .click(function() { console.log(xx,yy) })

  return dom
}

