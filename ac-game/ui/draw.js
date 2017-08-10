var pawn_spot_sz = 64
var tile_size = 3 * pawn_spot_sz
var action = null
var selected = false

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
  board.append($('<div/>')
               .css('width', sz + 'px')
               .css('height', sz + 'px')
               .css('background-color', 'rgba(0,0,0,0.25)')
               .css('border-radius', sz/6)
               .css('position', 'absolute')
               .css('left', realPawnLoc(x) + 'px')
               .css('bottom', realPawnLoc(y) + 'px')
               .css('z-index', '2')
               .click(function() {
                console.log(x,y)
                  if (action) {
                    toServer({x:x,y:y})
                  }
               })
              )
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
  console.log(p)
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
          .css('bottom', (yy+off - (ix * 5)) + 'px')
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
            console.log(x)
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

function drawCurPlayer(b,cp) {
  console.log(cp)
  var p = cp.player
  var sh = drawPlayerShell(p)
  var t = $('<table>')
  sh.append(t)
  var ws = drawPlayerStat('workers',p.workers)

  if (p.workers > 0) clickable(ws,function() {
    console.log('new worker')
    action = '/newWorker'
  })

  var pow = drawPlayerStat('power',p.powerBoost)

  if (p.power > 0 && cp.pawn) {
    console.log('boost')
  }

  if (cp.pawn) {
    action = '/move'
    selected = true
  }


  t.append( ws
          , pow
          , drawPlayerStat('move',p.speedBoost)
          , drawPlayerStat('share',p.shareSpace)
          )

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
