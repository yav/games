function tileCalss(tile) {
  return [ 'tl', 'tr', 'br', 'bl' ] [tile]
}

function ownerClass(owner) {
  return (owner === undefined || owner === null) ? 'blank' : 'p' + owner;
}

function marketStallName(areaid,mid,tile) {
  return 'market_' + areaid + '_' + mid + '_' + tile
}

function streeName(aid) {
  return 'street_' + aid
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

function getPlayer(p) {
  return $('.player:has(.' + ownerClass(p) + ')')
}


/* ----------------------------------------- */


function setOwner(dom,owner) {
  var cur = dom.data('owner')
  dom.removeClass(ownerClass(cur))
     .addClass(ownerClass(owner))
     .data('owner',owner)
}

function getOwner(dom) { return dom.data('owner') }

/* ----------------------------------------- */





function drawTile(t) {
  // t : { owner: int; tile: int }

  var style = ownerClass(t.owner)
  var dom = $('<img/>').attr('src','img/tile.svg')
                       .addClass('tile')
                       .addClass(tileCalss(t.tile))
                       .addClass(style)
                       .data('owner',t.owner)
  return dom
}

function drawDiscardedTile(t) {
  return drawTile(t).addClass('discarded_tile').removeClass('tile')
}




function drawMarket(areaid,mid,owner) {

  function stall(tile) {
    return dom = $('<td/>')
                 .attr('id',marketStallName(areaid,mid,tile))
                 .append(drawTile( { tile:tile, owner: owner[tile] }))
                 .addClass('clickable')
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
  // Area = { markets: [ Market ], vagrants: [ OwnedTile ] }

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
    var t = drawTile(s)
            .addClass('clickable')
            .click(vagrantClicked(area,s))
    street.append(t)
  })
  streetRow.append(street,drawValue(-1 * areaVal))

  return [marketRow,streetRow]
}


function drawPlayer(pid,player,isCur) {
  // Player = { name :: String
  //          , score :: Int
  //          , visible :: Maybe Tile, hiddenNum :: Int
  //          , discarded :: [Tile] }

  var dom = $('<div/>').addClass('player')

  if (isCur) dom.addClass('current')

  var name  = $('<span/>').addClass('name').text(player.name)
  var score = $('<span/>').addClass('score').text(player.score)

  var lab = $('<div/>')
            .addClass('label')
            .addClass(ownerClass(pid))
            .append(score,name)
  dom.append(lab)

  if (player.visible) {
     var ti = drawTile( { tile: player.visible, owner: pid }).addClass('vis')
     if (isCur) ti.addClass('clickable').click(visClicked)
     dom.append(ti)
  }

  var stack = $('<div/>')
              .addClass('stack')
              .addClass('label')
              .text(player.hiddenNum)

  if (isCur && player.hiddenNum > 0) {
    stack.addClass('clickable').click(stackClicked)
  }

  dom.append(stack)
  jQuery.each(player.discarded, function(ix,t) {
    dom.append(drawDiscardedTile({tile:t,owner:pid}))
  })

  return dom
}



function drawBoard(board) {
  // board : { areas: [Area], palace: [ OwnedTile ] }

  var dom = $('<table/>').addClass('board')

  jQuery.each(board.areas,function(areaid,area) {
    dom.prepend(drawArea(areaid,area))
  })

  var palaceRow = $('<tr/>')
  var palace = $('<td/>')
               .attr('id','palace')
               .addClass('palace').attr('colspan','4')
  jQuery.each(board.palace, function(unused,s) {
    palace.append(drawTile(s))
  })
  palaceRow.append(palace,drawValue(2 * (1 + board.areas.length)))
  dom.prepend(palaceRow)

  return dom
}



function drawState(state) {
  // state : { board: Board, players : [Player], curPlayer : int }

  var dom   = $('<table/>').addClass('game')
  var row   = $('<tr/>')
  var board = $('<td/>').append(drawBoard(state.board))
  var play  = $('<td/>').addClass('players')

  jQuery.each(state.players,function(ix,p) {
    play.append(drawPlayer(ix,p,state.curPlayer===ix))
  })

  return dom.append(row.append(board,play))
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
  var tgt  = drawTile( {tile: tile, owner: null }).css('visibility','hidden')
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
  var tgt = drawTile( {tile: tile, owner: null} ).css('visibility','hidden')
  pal.append(tgt)
  movingTile(src,tgt.offset(), function(cl) {
    setOwner(tgt,cl)
    tgt.css('visibility','visible')
  })
}

function moveStreeMarketDiscard(aid,owner,tile) {
  var str = getStreet(aid)
  var src = getVagrantFrom(str,owner,tile)
  if (src === null) return
  var box = getPlayer(owner)
  var tgt = drawDiscardedTile(tile,owner).css('visibility','hidden')
  box.append(tgt)
  src.css('visibility','hidden')
  movingTile(src,tgt.offset(), function(cl) {
    src.remove()
    tgt.css('visibility','visible')
  })
}

function moveMarketDiscard(aid,mid,tile) {
  var src   = getMarketTile(aid,mid,tile)
  var owner = getOwner(src)
  var box   = getPlayer(owner)
  console.log(box)
  var tgt   = drawDiscardedTile(tile,owner).css('visibility','hidden')
  box.append(tgt)
  movingTile(src,tgt.offset(),function(cl) {
    setOwner(src,null)
    tgt.css('visibility','visible')
  })
}

/* Events */

function marketClicked(areaid,mid) {
  return function() {
   console.log('Clicked on area ' + areaid + ", market " + mid)
  }
}

function vagrantClicked(areaid,t) {
  return function() {
   console.log('Clicked on vagrant in area ' + areaid +
               ', tile ' + t.tile +  ", owned by " + t.owner)
  }
}

function visClicked() {
  console.log('Vis clicked')
}

function stackClicked() {
  console.log('Stack clicked')
}

// -----------

function select(x) {
  x.addClass('selected')
}



