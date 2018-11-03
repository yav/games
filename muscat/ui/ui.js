function tileCalss(tile) {
  return [ 'tl', 'tr', 'br', 'bl' ] [tile]
}

function ownerClass(owner) {
  return (owner === undefined || owner === null) ? 'blank' : 'p' + owner;
}

function drawTile(tile, owner) {
  var style = ownerClass(owner)
  var dom = $('<img/>').attr('src','img/tile.svg')
                       .addClass('tile')
                       .addClass(tileCalss(tile))
                       .addClass(style)
                       .data('owner',owner)
  return dom
}

function setOwner(dom,owner) {
  var cur = dom.data('owner')
  dom.removeClass(ownerClass(cur))
     .addClass(ownerClass(owner))
     .data('owner',owner)
}



function marketStallName(areaid,mid,tile) {
  return 'market_' + areaid + '_' + mid + '_' + tile
}

function streeName(aid) {
  return 'street_' + aid
}


function drawMarket(areaid,mid,owner) {

  function stall(tile) {
    return dom = $('<td/>')
                 .attr('id',marketStallName(areaid,mid,tile))
                 .append(drawTile(tile,owner[tile]))
                 .click(marketClicked(areaid,mid))
  }

  return $('<table/>')
         .addClass('market')
         .append( $('<tr/>').append(stall(0),stall(1))
                , $('<tr/>').append(stall(3),stall(2))
                )
}


function drawValue(x) { return $('<td/>').addClass('value').text(x) }

function drawArea(areaid,area) {

  var areaVal = 1 + areaid


  var marketRow = $('<tr/>')
  jQuery.each(area.markets,function(mid,m) {
    marketRow.append($('<td/>').append(drawMarket(areaid,mid,m)))
  })
  marketRow.append(drawValue(2 * areaVal))

  var streetRow = $('<tr/>')
  var street = $('<td/>')
               .attr('id',streeName(areaid))
               .addClass('street')
               .attr('colspan','4')
  jQuery.each(area.vagrants,function(unused,s) {
    var t = drawTile(s.tile,s.owner)
            .click(vagrantClicked(area,s.tile,s.owner))
    street.append(t)
  })
  streetRow.append(street,drawValue(-1 * areaVal))

  return [marketRow,streetRow]
}

function drawPlayer(pid,player) {
  // Player = { name :: String
  //          , visible :: Maybe Tile, hiddenNum :: Int
  //          , discarded :: [Tile] }



  var lab = $('<div/>')
            .addClass('label')
            .addClass(ownerClass(pid))
            .text(player.name)

  var vis = player.visible ? [ drawTile(player.visible, pid) ]
                           : [ ]

  var stack = $('<div/>')
              .addClass('stack')
              .text(player.hiddenNum)

  var discards = $('<div/>')
                 .addClass('discards')

  jQuery.each(player.discarded, function(ix,t) {
    discards.append(drawTile(t,pid).addClass('discarded_tile')
                                   .removeClass('tile'))
  })


  return $('<div/>')
         .addClass('player')
         .append(lab,vis,stack,discards)
}


function drawBoard(board) {
  var dom = $('<table/>').addClass('board')

  jQuery.each(board.areas,function(areaid,area) {
    dom.prepend(drawArea(areaid,area))
  })

  var palaceRow = $('<tr/>')
  var palace = $('<td/>')
               .attr('id','palace')
               .addClass('palace').attr('colspan','4')
  jQuery.each(board.palace, function(unused,s) {
    palace.append(drawTile(s.tile,s.owner))
  })
  palaceRow.append(palace,drawValue(2 * (1 + board.areas.length)))
  dom.prepend(palaceRow)

  return dom
}

function drawState(state) {
  // state : { board: Board, players : [Player] }

  var dom = $('<table/>').addClass('game')
  jQuery.each(state.players,function(ix,p) {
    var r = $('<tr/>')
    if (ix === 0) {
      r.append($('<td/>')
               .attr('rowspan',state.players.length)
               .append(drawBoard(state.board)))
    }

    r.append($('<td/>').append(drawPlayer(ix,p)))
    dom.append(r)
  })

  return dom
}

// -----------------------------------------------------------------------------
// Moving tiles

function movingTile(obj,end,done) {
  var cl = obj.data('owner')

  var pos = obj.offset()
  var t = obj.clone()
             .css('position','absolute')
             .css('visibility','visible')
             .css('z-index','10')
             .css('left',pos.left)
             .css('top',pos.top)

  setOwner(obj,null)
  $('body').append(t)
  t.animate( {left:end.left, top:end.top}
           , 'slow'
           , 'swing'
           , function() { t.remove(); done(cl) })

}

function getMarketTile(aid,mid,tile) {
  return $('#' + marketStallName(aid,mid,tile) + ' img')
}

function getStreet(aid) {
  return $('#' + streeName(aid))
}

function getPalace() {
  return $('#palace')
}

function getVagrantFrom(str,owner,tile) {
  var query = '.' + ownerClass(owner) + '.' + tileCalss(tile)
  var res = null
  jQuery.each(str.find(query).first(), function(ix,a) {
    res = $(a)
  })
  return res
}

function moveBetweenMarkets(aid1,mid1,aid2,mid2,tile) {
  var src = getMarketTile(aid1,mid1,tile)
  var tgt = getMarketTile(aid2,mid2,tile)
  movingTile(src,tgt.offset(),function(cl) { setOwner(tgt,cl) })
}

function swapBetweenMarket(aid,mid1,mid2,tile) {
  var src = getMarketTile(aid,mid1,tile)
  var tgt = getMarketTile(aid,mid2,tile)

  movingTile(src,tgt.offset(), function(cl) { setOwner(tgt,cl) })
  movingTile(tgt,src.offset(), function(cl) { setOwner(src,cl) })
}

function moveMarketStreet(aid,mid,tile) {
  var src  = getMarketTile(aid,mid,tile)
  var str  = getStreet(aid)
  var tgt  = drawTile(tile,null).css('visibility','hidden')
  str.append(tgt)
  movingTile(src,tgt.offset(), function(cl) {
    setOwner(tgt,cl)
    tgt.css('visibility','visible')
  })
}

function moveStreeMarket(aid1,aid2,mid2,owner,tile) {
  var str = getStreet(aid1)
  var src = getVagrantFrom(str,owner,tile)
  if (src === null) return
  var tgt = getMarketTile(aid2,mid2,tile)

  src.css('visibility','hidden')
  movingTile(src,tgt.offset(),function(newOwner) {
    setOwner(tgt,newOwner)
    src.remove()
  })
}

function moveMarketPalace(aid,mid,tile) {
  var src = getMarketTile(aid,mid,tile)
  var pal = getPalace()
  var tgt = drawTile(tile,null).css('visibility','hidden')
  pal.append(tgt)
  movingTile(src,tgt.offset(), function(cl) {
    setOwner(tgt,cl)
    tgt.css('visibility','visible')
  })
}



/* Events */

function marketClicked(areaid,mid) {
  return function() {
   console.log('Clicked on area ' + areaid + ", market " + mid)
  }
}

function vagrantClicked(areaid,pid,tile) {
  return function() {
   console.log('Clicked on vagrant in area ' + areaid +
               ', tile ' + tile +  ", owned by " + pid)
  }
}


