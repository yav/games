var pawn_spot_sz = 64
var tile_size = 3 * pawn_spot_sz
var action = null

function pawnCoordToTiles(x,y) {
  var tx = Math.floor(x/2)
  var ty = Math.floor(y/2)
  var ex = x % 2 == 0
  var ey = y % 2 == 0

  var t = [tx, ty]
  var l = [tx - 1, ty]
  var b = [tx, ty - 1]
  var bl = [ tx - 1, ty - 1 ]

  return ex? (ey? [ t, b, bl, l ] : [ t, l ] )
           : (ey? [ t, b ]        : [ t ])
}

function getTile(tiles,a) {
  var row = tiles[a[1]]
  return row && row[a[0]]
}



function addPawnLoc(board, tiles, x, y) {
  var ts = pawnCoordToTiles(x,y)
  var have = false
  jQuery.each(ts, function(ix,t) {
    have = getTile(tiles,t)
    if (have) return false
  })
  if (!have) return


  var sz = pawn_spot_sz
  var back = $('<div/>')
             .css('width', sz + 'px')
             .css('height', sz + 'px')
             .css('background-color', 'rgba(0,0,0,0.25)')
             .css('border-radius', sz/6)
             .css('position', 'absolute')
             .css('left', realPawnLoc(x) + 'px')
             .css('bottom', realPawnLoc(y) + 'px')
             .css('z-index', '20')

  var front = back
            .clone()
            .css('background-color', 'rgba(0,0,0,0)')
            .css('z-index','100')
            .click(function() {
                  if (action) { toServer({x:x,y:y}) }
               })

  board.append(back,front)
}

function realPawnLoc(d) { return (1.5 * d - 0.5) * pawn_spot_sz }

function addTile(board,tiles,x,y) {
  var lab = getTile(tiles,[x,y])
  if (!lab) return

  var border = 2

  var sz = tile_size - 2 * border

  var left = x * tile_size
  var bot  = y * tile_size

  var l = $('<div/>')
          .css('background-color','rgba(255,255,255,0.5)')
          .css('border-radius','3px')
          .css('position','absolute')
          .css('left', (left + 0.5 * pawn_spot_sz) + 'px')
          .css('bottom',  (bot + 2.1 * pawn_spot_sz) + 'px')
          .css('padding','2px')
          .text(lab)

  board.append(
      $('<div/>')
      .css('width',  sz + 'px')
      .css('height', sz + 'px')
      .css('position','absolute')
      .css('left',    left + 'px')
      .css('bottom',  bot + 'px')
      .css('border',border + 'px ' + 'solid black')
      .css('background-color','#090')
  , l
  )
}


function addPawn(b,p,x,y,ix) {
  var sz = 0.75 * pawn_spot_sz
  var off = (pawn_spot_sz - sz) / 2
  var ssh = 0.80 * sz
  var ssw = ssh/2

  var xx = realPawnLoc(x)
  var yy = realPawnLoc(y)

  var pow = $('<div/>')
            .css('width', ssw + 'px')
            .css('height', ssh + 'px')
            .css('color','white')
            .css('background-color','rgba(0,0,0,0.5)')
            .css('display', 'inline-block')
            .css('border-top-left-radius',ssh + 'px')
            .css('border-bottom-left-radius',ssh + 'px')
            .css('position','relative')
            .css('top','50%')
            .css('transform','translateY(-50%)')
            .css('line-height',ssh + 'px')
            .text(p.power)

  var mov = $('<div/>')
            .css('width', ssw + 'px')
            .css('height', ssh + 'px')
            .css('color','black')
            .css('background-color','rgba(255,255,255,0.5)')
            .css('display', 'inline-block')
            .css('border-top-right-radius',ssh + 'px')
            .css('border-bottom-right-radius',ssh + 'px')
            .css('position','relative')
            .css('top','50%')
            .css('transform','translateY(-50%)')
            .css('line-height',ssh + 'px')
            .text(p.speed)


  var d = $('<div/>')
          .css('width',sz + 'px')
          .css('height',sz + 'px')
          .css('border-radius', sz + 'px')
          .css('background-color', playerColor(p.player))
          .css('color', playerFgColor(p.player))
          .css('position','absolute')
          .css('border', '1px solid black')
          .css('left', (xx+off) + 'px')
          .css('bottom', (yy+off - (ix * sz/6)) + 'px')
          .css('text-align','center')
          .css('z-index', 40 - ix)

  b.append(d.append(pow, mov))
}


function drawBoard(b) {
  var ts = b.tiles
  var h = ts.length
  var w = ts[0].length  // assumes non-empty rows

  var board = $('<div/>')
              .css('position','relative')
              .css('display','inline-block')
              .css('vertical-align','top')
              .css('width',  tile_size * w + 'px')
              .css('height', tile_size * h + 'px')
              .css('margin', pawn_spot_sz + 'px')
              .css('border', '1px solid black')

  for (var x = 0; x < w; ++ x)
    for (var y = 0; y < h; ++ y)
      addTile(board,ts,x,y)


  for (var x = 0; x <= 2 * w; ++ x)
    for (var y = 0; y <= 2 * h; ++ y)
      addPawnLoc(board,ts,x,y)

  jQuery.each(b.pawns, function(ix,p) {
    jQuery.each(p.pawns, function(pix,pa) {
      addPawn(board,pa,p.loc.x,p.loc.y,pix)
    })
  })

  return board

}

function playerColor(x) {
  if (x == 0) return 'orange'
  if (x == 1) return 'purple'
  if (x == 2) return 'yellow'
  if (x == 3) return 'green'
}

function playerFgColor(x) {
  if (x == 1) return 'white'
  return 'black'

}

function drawPlayerShell(p) {
  var s = $('<div/>')
          .css('display','inline-block')
          .css('color',playerFgColor(p.id))
          .css('background-color',playerColor(p.id))
          .css('border-radius','5px')
          .css('margin','1em')
          .css('padding','0.3em')
  return s
}

function drawPlayerStat(l,a) {
    return $('<tr/>')
           .append( $('<td/>').css('font-weight','bold').text(l)
                  , $('<td/>').text(a)
                  )
}

function toServer(i) {
  jQuery.post(action,i,drawGame)
        .fail(function(x) {
            $('#message').empty().text(x.statusText)
        })
}

function clickable(x,f) {
  x.css('cursor','pointer')
   .click(f)
}

function drawPlayer(p) {
  var sh = drawPlayerShell(p)
  var t = $('<table>')
  sh.append(t)
  t.append( drawPlayerStat('workers',p.workers)
          , drawPlayerStat('power',p.powerBoost)
          , drawPlayerStat('move',p.speedBoost)
          , drawPlayerStat('share',p.shareSpace)
          )

  return sh
}

function drawCurPawn(board,ix,cp) {
  var x = cp.loc.x
  var y = cp.loc.y
  addPawn(board,cp.pawn,x,y,ix)
  var xx = realPawnLoc(x)
  var yy = realPawnLoc(y)
  var d = $('<div/>')
          .css('position','absolute')
          .css('left',xx)
          .css('bottom',yy)
          .css('font-weight','bold')
          .text(cp.boost + ',' + cp.move)
  board.append(d)
}

function drawCurPlayer(b,cp) {
  action = null
  var p = cp.player
  var sh = drawPlayerShell(p)
  var t = $('<table>')
  sh.append(t)
  var ws = drawPlayerStat('workers',p.workers)

  if (p.workers > 0 && !cp.pawn) clickable(ws,function() {
    action = '/newWorker'
  })

  var pow = drawPlayerStat('power',p.powerBoost)

  if (p.power > 0 && cp.pawn) clickable(pow,function() {
    console.log('boost')
    action = '/boost'
  })

  if (cp.pawn) {
    action = '/move'
    drawCurPawn(b,0,cp.pawn) // XXX: ix
  } else {
    action = '/select'
  }


  t.append( ws
          , pow
          , drawPlayerStat('move',p.speedBoost)
          , drawPlayerStat('share',p.shareSpace)
          )

  if (cp.pawn) {
    var btn = $('<div/>')
              .css('width','96px')
              .css('height','20px')
              .css('border-radius','5px')
              .css('background-color', playerFgColor(cp.player.id))
              .css('color', playerColor(cp.player.id))
              .css('padding','5px')
              .css('font-weight','bold')
              .css('text-align','center')
              .text('End turn')

   clickable(btn, function() {
    action = 'doAction'
    toServer({})
   })

    t.append(btn)
  }

  return sh
}

function drawGame(g) {
  var d = $('<div/>')
  var b = drawBoard(g.board)

  d.append( b
          , drawCurPlayer(b,g.player)
          )

  jQuery.each(g.players, function(ix,p) {
    d.append(drawPlayer(p))
  })


  $('#game').empty().append(d)
}
