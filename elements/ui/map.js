// For testing
function randomTile() {
  var x = []
  var opts = ['forest','sand','grass','hills','grass','grass','bog','sea',
                  'mountains', 'lava']
  jQuery.each(['NW','NE','E','C','SE','SW','W'], function(ix,d) {
  var t = opts[Math.floor(Math.random() * opts.length)]
  var c = Math.floor(Math.random() * 5) === 0 ? 'shop' : null
  x.push({ addr: d, content: { terrain: t, character: c } })
  })
  return x
}



function drawHex(hex) {

  var tUrl = '/img/terrain/' + hex.terrain + '.svg'

  var me = $('<div/>')
        .addClass('hex')
        .attr('title',hex.terrain)

  var bg = $('<div/>').addClass('bg').addClass(hex.terrain)

  me.append(bg)

  jQuery.each(hex.features, function(ix,feature) {
    var fea = $('<div/>')
            .addClass('feature')
            .addClass(feature)
            .attr('title',feature)
    me.append(fea)

  })

  return me
}

function drawTile(coord_tile) {

  var tile = coord_tile.tile

  var me = $('<div/>').addClass('tile')
  function px(x) { return x + 'px' }
  var width = 0
  var height = 0
  var hexes = {}

  jQuery.each(tile.static, function (ix,hex) {
    var dom = drawHex(hex.content)
    dom.addClass(hex.addr)

    var bg = dom.find('.bg')
    bg.click(function() {
      jQuery.post('/move', { tile_x: coord_tile.x
                           , tile_y: coord_tile.y
                           , hex: hex.addr
                           }, redrawGame)
    })

    hexes[hex.addr] = dom
    me.append(dom)

  })


  jQuery.each(tile.dynamic, function (ix,hex) {
    var c = hex.content

    jQuery.each(c.enemies, function(ix,e) {
      console.log('XXX: drawEnemy', e)
    })

    jQuery.each(c.players, function(ix,p) {
      hexes[hex.addr].append(drawPlayer(p))
    })

  })

  return me
}

function drawPlayer(p) {
  return $('<div/>')
         .addClass('player icon')
}


// upside down
function drawMap(land) {

  var map_container = $('<div/>').addClass('map_container')

  var map = $('<div/>').addClass('map')


  var mini = $('<div/>').addClass('mini_btn clickable').text('+')
  map_container.append([mini,map])

  var tiles = land.map
  var size  = 1


  function redraw() {
    // map dimension in units of tiles

    var w = 1.5 * (1 + size)
    var h = 2   * (1 + size)

    var xdx = 2/3    / w
    var xdy = -0.6   / h
    var ydx = (-1/6) / w
    var ydy = -0.9   / h

    var locs = []
    var minX = 0
    var minY = 0

    jQuery.each(tiles,function(ix,t) {
      var x = 100 * (t.x * xdx + t.y * ydx)
      var y = 100 * (t.x * xdy + t.y * ydy)
      if (x < minX) minX = x
      if (y < minY) minY = y
      locs.push({left: x, top: y, dom: drawTile(t)})
    })

    jQuery.each(locs, function(ix,l) {
      var x = l.left - minX
      var y = l.top  - minY
      l.dom.addClass('size' + size)
      map.append(l.dom.css('left', x + '%').css('top',  y + '%'))
    })
  }

  redraw()

  mini.click( function() {
    map.find('.tile').remove()
    size = (size + 1) % 3
    redraw()
  })

  return map_container
}

