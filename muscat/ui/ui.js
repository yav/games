function tileCalss(tile) {
  return [ 'tl', 'tr', 'br', 'bl' ] [tile]
}

function drawTile(tile, owner) {
 return $('<img/>').attr('src','img/tile.svg')
                   .addClass('tile')
                   .addClass(tileCalss(tile))
                   .addClass(owner ? ('p' + owner) : 'blank')
}


function drawMarket(areaid,mid,owner) {

  function stall(tile) {
    return dom = $('<td/>').append(drawTile(tile,owner[tile]))
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
    street.append(drawTile(s.tile, s.owner))
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
  var palace = $('<td/>').addClass('palace').attr('colspan','4')
  jQuery.each(board.palace, function(unused,s) {
    palace.append(drawTile(s.tile,s.owner))
  })
  palaceRow.append(palace,drawValue(2 * (1 + board.areas.length)))
  dom.prepend(palaceRow)



  return dom
}

