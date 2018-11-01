function tileCalss(tile) {
  return [ 'tl', 'tr', 'br', 'bl' ] [tile]
}

function drawTile(tile, owner) {
  var style = (owner === undefined || owner === null) ? 'blank' : 'p' + owner;
  return $('<img/>').attr('src','img/tile.svg')
                    .addClass('tile')
                    .addClass(tileCalss(tile))
                    .addClass(style)
}

function marketStallName(areaid,mid,tile) {
  return 'market_' + areaid + '_' + mid + '_' + tile
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
  var street = $('<td/>').addClass('street').attr('colspan','4')
  jQuery.each(area.vagrants,function(unused,s) {
    var t = drawTile(s.tile,s.owner)
            .click(vagrantClicked(area,s.tile,s.owner))
    street.append(t)
  })
  streetRow.append(street,drawValue(-1 * areaVal))

  return [marketRow,streetRow]
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

function moveBetweenMarkets(aid1,mid1,aid2,mid2,tile) {
  var body = $('body')
  var src = $('#' + marketStallName(aid1,mid1,tile) + ' img')
  var tgt = $('#' + marketStallName(aid2,mid2,tile) + ' img')

  var cl = 'blank'
  for (var c = 0; c < 5; ++c) {
    if (src.hasClass('p' + c)) cl = 'p' + c;
  }
  if (cl === 'blank') return


  var start = src.offset()
  var end   = tgt.offset()
  var tile = src.clone()
                .css('position','absolute')
                .css('z-index','10')
                .css('left',start.left)
                .css('top',start.top)

  src.removeClass(cl).addClass('blank')
  body.append(tile)
                tile
                .animate({left:end.left, top:end.top},'slow','swing',function(){
                  tile.remove()
                  tgt.removeClass('blank').addClass(cl)
                })
}

function swapBetweenMarket(aid,mid1,mid2,tile) {
  var body = $('body')
  var src = $('#' + marketStallName(aid,mid1,tile) + ' img')
  var tgt = $('#' + marketStallName(aid,mid2,tile) + ' img')

  var cl1 = 'blank'
  var cl2 = 'blank'
  for (var c = 0; c < 5; ++c) {
    if (src.hasClass('p' + c)) cl1 = 'p' + c;
    if (tgt.hasClass('p' + c)) cl2 = 'p' + c;
  }
  if      (cl1 === 'blank') {
      moveBetweenMarkets(aid,mid2,aid,mid1,tile)
      return
  }
  else if (cl2 === 'blank') {
    moveBetweenMarkets(aid,mid1,aid,mid2,tile)
    return
  }

  var start = src.offset()
  var end   = tgt.offset()
  var tile1 = src.clone()
                 .css('position','absolute')
                 .css('z-index','10')
                 .css('left',start.left)
                 .css('top',start.top)

  var tile2 = tgt.clone()
                 .css('position','absolute')
                 .css('z-index','10')
                 .css('left',end.left)
                 .css('top',end.top)

  body.append(tile1)
  body.append(tile2)

  src.removeClass(cl1).addClass('blank')
  tgt.removeClass(cl2).addClass('blank')

  tile1.animate({left:end.left, top:end.top},'slow','swing',function(){
     tile1.remove()
     tgt.removeClass('blank').addClass(cl1)
   })

  tile2.animate({left:start.left, top:start.top},'slow','swing',function(){
    tile2.remove()
    src.removeClass('blank').addClass(cl2)
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


