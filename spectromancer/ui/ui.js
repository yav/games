function img(x) {
  return $('<img/>')
         .attr('src','img/' + x)
         .css('position','absolute')
         .css('left', '0')
         .css('top', '0')
         .css('width', '96px')
         .css('height','96px')
}

function stat(x,t,l,c) {
  return $('<div/>')
         .css('position','absolute')
         .css('color',c)
         .css('font-size','10px')
         .css('font-weight','bold')
         .css('top',t + 'px')
         .css('left', l + 'px')
         .css('z-index','4')
         .css('width','10px')
         .css('height','16px')
         .css('text-align','center')
         .text(x == null ? '?' : x)

}

function drawCard(c) {
  var grp = $('<div/>')
            .css('position','relative')
            .css('overflow','hidden')
            .css('display','inline-block')
            .css('width', '96px')
            .css('height','96px')
            .attr('title',c.name + '\n' + c.description)

  if (c.type == 'spell') {
    grp.append(img(c.image)
               .css('height','112')
               .css('top','-16px')
              )
    grp.append(img('ramka2.png')
              .css('z-index','2'))
    grp.append(stat(c.cost,4,80,'black'))



  } else {
    grp.append(img(c.image))
    grp.append(img('ramka1.png')
              .css('z-index','2'))
    grp.append(stat(c.cost,7,76,'black'))
    grp.append(stat(c.life,77,76,'green'))
    grp.append(stat(c.attack,76,10,'red'))
  }

  return $('<div/>')
         .css('display','inline-block')
         .append($('<span/>').text(c.name),$('<br/>'),grp)
}

function drawDeckRow(p,row) {
  var dom = $('<tr/>')
  var pow = $('<td/>').text(row + ': ' + p.power[row])
  dom.append(pow)
  jQuery.each(p.deck[row], function(ix,card) {
    dom.append($('<td/>').append(drawCard(card)))
  })
  return dom
}

function drawPlayer(p) {
  var dom = $('<div/>')
  var name = $('<div/>').text(p.name + ' (' + p.life + ')')
  dom.append(name)

  var active = $('<div/>')
  jQuery.each(p.active, function(ix,c) {
    // active.append(drawCard(c))
  })
  dom.append(active)

  var deckTable = $('<table/>')
  jQuery.each(["Fire","Water","Air","Earth","Special"], function(ix,w) {
    deckTable.append(drawDeckRow(p,w))
  })

  dom.append(deckTable)

  return dom
}

function drawGame(g) {
  return $('<div/>')
         .append(drawPlayer(g.current),drawPlayer(g.other))
}


