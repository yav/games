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

  if (c == null) {
    grp.css('border','1px solid black')
    return grp
  }

  grp.attr('title', c.name + '\n' + c.description)
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

  return grp
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
            .css('display','inline-block')
            .css('margin', '20px')
  var name = $('<h3/>')
             .css('text-align','center')
             .text(p.name + ' (' + p.life + ')')
  dom.append(name)

  var deckTable = $('<table/>')
  jQuery.each(["Fire","Water","Air","Earth","Special"], function(ix,w) {
    deckTable.append(drawDeckRow(p,w))
  })

  dom.append(deckTable)

  return dom
}

function drawArena(p1,p2) {
  var dom = $('<table/>').css('display','inline-block')
                         .css('valign','bottom')
  var act1 = p1.active
  var act2 = p2.active
  for (var i = 0; i < 6; ++i) {
    var row = $('<tr/>')
    row.append($('<td/>').append(drawCard(act1[i]))
              ,$('<td/>').append(drawCard(act2[i]))
              )
    dom.append(row)
  }
  return dom
}

function drawGame(g) {
  return $('<table/>')
         .append($('<tr/>')
                .append( $('<td/>').append(drawPlayer(g.current))
                       , $('<td/>').append(drawArena(g.current,g.other))
                       , $('<td/>').append(drawPlayer(g.other))
                       ))
}


